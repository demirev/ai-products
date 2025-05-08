import os
import spacy
import argparse
import numpy as np
from find_relevant_releases import read_list_from_csv, save_list_to_csv
from datetime import datetime
from transformers import AutoTokenizer, AutoModel
import torch
from sentence_transformers import SentenceTransformer
import pandas as pd
import json

# Initialize BART model and tokenizer
model_name = "facebook/bart-large"
sentence_model = "sentence-transformers/all-mpnet-base-v2"
tokenizer = AutoTokenizer.from_pretrained(model_name)
bart_model = AutoModel.from_pretrained(model_name)
sentence_model = SentenceTransformer(sentence_model)

def get_bart_embeddings(text, max_length=512):
	# Prepare inputs
	inputs = tokenizer(
		text,
		max_length=max_length,
		padding=True,
		truncation=True,
		return_tensors="pt"
	)
	
	# Get model output
	with torch.no_grad():
		outputs = bart_model(**inputs)
	
	# Use [CLS] token embedding as sentence representation
	embeddings = outputs.last_hidden_state[:, 0, :]
	return embeddings.squeeze().numpy() # to array


def get_sentence_embeddings(text, sentence_model=sentence_model):
  return sentence_model.encode(text)


def pull_all_capabilities(scored_releases, intent=None, type=None):
	all_capabilities = []
	all_release_idxs = []
	if intent is not None:
		if intent == 'automation':
			relevant_intents = [
        'End-to-End Processing', 
        'Replacement Messaging', 
        'Self-Correction Mechanisms',
        'Volume/Scale Emphasis' 
      ]
		elif intent == 'augmentation':
			relevant_intents = [
        'Expert Amplification', 
        'Human-in-the-Loop Design', 
        'Insight Generation'
      ]
		else:
			raise ValueError(f"Invalid intent: {intent}")
	else:
		relevant_intents = None
        
	if type is not None:
		if type == 'announcement':
			relevant_types = [
        'AI Product Launch Announcement',
        'AI Product Adoption Announcement'
      ]
		elif type == 'report':
			relevant_types = [
        'AI Product News',
        'AI Product Market Report'
      ]
		else:
			raise ValueError(f"Invalid type: {type}")
	else:
		relevant_types = None


	i = 0
	for release in scored_releases:
		capability = release.get('capability_string', '')
		release_type = release.get('document_type', '')
		intent_type = release.get('intent_type', '')
		string_not_empty = capability != '' and capability != '""'
		release_is_relevant = release_type != 'Not Relevant'
		intent_matches = relevant_intents is None or intent_type in relevant_intents
		type_matches = relevant_types is None or release_type in relevant_types
		if string_not_empty and release_is_relevant and intent_matches and type_matches:
			# Parse the string as a JSON array
			try:  
				capability_list = json.loads(capability.replace("'", '"'))
				all_capabilities.extend(capability_list)
				all_release_idxs.extend([i] * len(capability_list))
			except:
				print(f"Error parsing capability: {capability}")
		i += 1
	return all_capabilities, all_release_idxs


def cosine_similarity(v1, v2):
  dot_product = sum([a * b for a, b in zip(v1, v2)])
  norm_v1 = sum([a**2 for a in v1])**0.5
  norm_v2 = sum([b**2 for b in v2])**0.5
  return dot_product / (norm_v1 * norm_v2)


def cosine_similarity_np(v1, v2):
  # Ensure input arrays are numpy arrays
  if not isinstance(v1, np.ndarray):
    vectors1 = np.array(v1)
  else:
    vectors1 = v1

  if not isinstance(v2, np.ndarray):
    vectors2 = np.array(v2)
  else:
    vectors2 = v2

  # Calculate dot products. For 2D arrays, np.dot does matrix multiplication,
  # so we use np.einsum to compute dot products along the last axis.
  dot_products = np.einsum('ij,ij->i', vectors1, vectors2)
  
  # Calculate norms of each vector
  norms_v1 = np.linalg.norm(vectors1, axis=1)
  norms_v2 = np.linalg.norm(vectors2, axis=1)
  norms_product = norms_v1 * norms_v2
  
  # avoid division by zero
  valid_indices = norms_product != 0
  cosine_similarities = np.zeros_like(dot_products, dtype=float)
  cosine_similarities[valid_indices] = dot_products[valid_indices] / norms_product[valid_indices]
  
  return cosine_similarities


def group_skills(skills, skill_hierarchy):
  def find_broader_skill(skill, skill_hierarchy):
    for sk in skill_hierarchy:
      if sk['conceptUri'] == skill and sk['broaderType'] == "SkillGroup":
        return sk['broaderUri']
    return None

  skill_groups = []
  for skill in skills:
    broader_skill_1 = find_broader_skill(skill, skill_hierarchy)
    broader_skill_2 = find_broader_skill(broader_skill_1, skill_hierarchy)
    broader_skill_3 = find_broader_skill(broader_skill_2, skill_hierarchy)
    skill_groups.append({
      'conceptUri': skill,
      'group_level_3': broader_skill_1,
      'group_level_2': broader_skill_2,
      'group_level_1': broader_skill_3
    })
  return skill_groups

def log_sum_exp(scores, gamma=1.0):
  """
  Compute the log-sum-exp of a list or array of scores with scaling parameter gamma.
  """
  if gamma == 0:
    raise ValueError("gamma must be non-zero.")

  scores = np.array(scores)
  # Multiply scores by gamma
  scaled_scores = gamma * scores
  # For numerical stability, subtract the maximum scaled score
  max_score = np.max(scaled_scores)
  lse = max_score + np.log(np.sum(np.exp(scaled_scores - max_score)))
  return lse / gamma - np.log(len(scores)) / gamma # normalize by number of scores


if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Match product capabilities to skills")
  parser.add_argument(
    "--capabilities-input-file", 
    type=str, 
    default="results/press_releases/processed_press_releases.csv",
    help="Path to the file containing the product capabilities"
  )
  parser.add_argument(
    "--skills-input-file", 
    type=str, 
    default="data/esco/skills_en.csv",
    help="Path to the file containing the skills"
  )
  parser.add_argument(
    "--skills-output-file", type=str, help="Path to the output file",
    default="results/scored_esco_skills/scored_esco_skills.csv"
  )
  parser.add_argument(
    "--no-embedding", 
    action="store_true", 
    help="Do not calculate capability and skill embeddings",
    default=False
  )
  parser.add_argument(
    "--no-within-skill-similarity", 
    action="store_true", 
    help="Do not calculate within-skill similarity",
    default=False
  )
  parser.add_argument(
    "--no-capability-skill-similarity", 
    action="store_true", 
    help="Do not calculate capability-skill similarity",
    default=False
  )

  args = parser.parse_args()

  print("Starting")
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

  # read input data
  scored_releases_file = args.capabilities_input_file
  esco_skills_file = args.skills_input_file
  capability_vector_file = "checkpoints/capability_vectors.csv"
  skill_vector_file = "checkpoints/skill_vectors.csv"
  within_skill_similarity_file = "results/scored_esco_skills/within_skill_similarity.csv"
  skill_hierarchy_file = "data/esco/broaderRelationsSkillPillar_en.csv"
  skills_output_file = args.skills_output_file

  scored_releases = read_list_from_csv(scored_releases_file)
  esco_skills = read_list_from_csv(esco_skills_file)
  skill_hierarchy = read_list_from_csv(skill_hierarchy_file)

  # filter only relevant esco_skills
  esco_skills = [skill for skill in esco_skills if skill.get('status', '') == 'released' and skill.get('skillType', '') == 'skill/competence']
  
  # tabulate releases by intent
  print(f"Total releases: {len([sr for sr in scored_releases if sr['document_type'] != 'Not Relevant' and sr['capability_string'] != '[]'])}")
  # 8257
  automation_intents = [
    "End-to-End Processing", 
    "Replacement Messaging", 
    "Self-Correction Mechanisms",
    "Volume/Scale Emphasis"
  ]
  automation_releases = [sr for sr in scored_releases if sr["document_type"] != "Not Relevant" and sr["intent_type"] in automation_intents]
  print(f"Automation releases: {len(automation_releases)}") 
  # 3975
  augmentation_intents = [
    "Expert Amplification", 
    "Human-in-the-Loop Design", 
    "Insight Generation"
  ]
  augmentation_releases = [sr for sr in scored_releases if sr["document_type"] != "Not Relevant" and sr["intent_type"] in augmentation_intents]
  print(f"Augmentation releases: {len(augmentation_releases)}") 
  # 3673
  company_releases = [sr for sr in scored_releases if sr["document_type"] == "AI Product Launch Announcement" or sr["document_type"] == "AI Product Adoption Announcement"]
  print(f"Company releases: {len(company_releases)}")
  # 5973
  report_releases = [sr for sr in scored_releases if sr["document_type"] == "AI Product Market Report" or sr["document_type"] == "AI Product News"]
  print(f"Report releases: {len(report_releases)}")
  # 2419
  

  # only populated capabilities
  total_capabilities, all_release_idxs = pull_all_capabilities(scored_releases)
  print(f"Total non-unique capabilities: {len(total_capabilities)}")
  # 27414
  print(f"Total unique capabilities: {len(set(total_capabilities))}")
  # 25187
  #all_capabilities = list(set(total_capabilities))
  all_capabilities = total_capabilities # keep duplicates so that multiple releases for the same capability are not lost
 
  all_automation_capabilities, _ = pull_all_capabilities(scored_releases, intent='automation')
  all_augmentation_capabilities, _ = pull_all_capabilities(scored_releases, intent='augmentation')
  all_company_capabilities, _ = pull_all_capabilities(scored_releases, type='announcement')
  all_report_capabilities, _ = pull_all_capabilities(scored_releases, type='report')

  automation_capabilities = [ac for ac in all_automation_capabilities if ac not in all_augmentation_capabilities] # only unambiguously automation capabilities
  augmentation_capabilities = [ac for ac in all_augmentation_capabilities if ac not in all_automation_capabilities] # only unambiguously augmentation capabilities
  company_capabilities = [ac for ac in all_company_capabilities if ac not in all_report_capabilities] # only unambiguously company capabilities
  report_capabilities = [ac for ac in all_report_capabilities if ac not in all_company_capabilities] # only unambiguously report capabilities

  print(f"Automation capabilities: {len(automation_capabilities)}")
  # 12826
  print(f"Augmentation capabilities: {len(augmentation_capabilities)}")
  # 11084
  print(f"Company capabilities: {len(company_capabilities)}")
  # 19024
  print(f"Report capabilities: {len(report_capabilities)}")
  # 6637

  if not args.no_embedding:
    # embed capabilities
    capability_vectors = np.array([get_sentence_embeddings(capability) for capability in all_capabilities])
    
    # Save capabilities with vectors using pandas
    capability_df = pd.DataFrame({
      'capability': all_capabilities,
      'vector': [json.dumps(vector.tolist()) for vector in capability_vectors],
      'release_idx': all_release_idxs
    })
    capability_df.to_csv(capability_vector_file, index=False)
    print(f"Saved capability vectors to {capability_vector_file}")
    
    # embed skills
    skill_vectors = np.array([get_sentence_embeddings(skill.get('preferredLabel', '') + " " + skill.get('description', '')) for skill in esco_skills])
    
    # Save skills with vectors using pandas
    skill_df = pd.DataFrame({
      'skill': [json.dumps(skill) for skill in esco_skills],
      'vector': [json.dumps(vector.tolist()) for vector in skill_vectors]
    })
    skill_df.to_csv(skill_vector_file, index=False)
    print(f"Saved skill vectors to {skill_vector_file}")
  
  # Read capability vectors using pandas
  capability_df = pd.read_csv(capability_vector_file)
  if not capability_df.empty:
    capability_vectors = np.array([np.array(json.loads(vector)) for vector in capability_df['vector']])
    release_idxs = capability_df['release_idx'].tolist()
  else:
    raise ValueError("Capability vectors not found")
  
  # Read skill vectors using pandas
  skill_df = pd.read_csv(skill_vector_file)
  if not skill_df.empty:
    skill_vectors = np.array([np.array(json.loads(vector)) for vector in skill_df['vector']])
  else:
    raise ValueError("Skill vectors not found")

  if not args.no_within_skill_similarity:
    # calculate within-skill similarity
    skill_groups = group_skills(
      [skill['conceptUri'] for skill in esco_skills if skill.get('status', '') == 'released' and skill.get('skillType', '') == 'skill/competence'], 
      skill_hierarchy
    )
    
    within_skill_similarity = []
    i = 0
    for idx, skill in enumerate(esco_skills):
      i += 1
      print(f"Processing skill {i} of {len(esco_skills)}")

      # calculate similarity scores with all skills
      similarity_scores = cosine_similarity_np(
        np.tile(skill_vectors[idx], (len(skill_vectors), 1)), 
        skill_vectors
      )
      
      # get the ids iand idxs of skills in the same skill groups
      this_group = [
        sg for sg in skill_groups if sg['conceptUri'] == skill['conceptUri']
      ][0]

      if this_group['group_level_3'] is None:
        # this is a sub-skill, so we skip it
        continue

      grps = {
        "level_3": [sg['conceptUri'] for sg in skill_groups if sg['group_level_3'] == this_group['group_level_3']],
        "level_3_idx": [idx for idx, sg in enumerate(skill_groups) if sg['group_level_3'] == this_group['group_level_3']],
        "level_2": [sg['conceptUri'] for sg in skill_groups if sg['group_level_2'] == this_group['group_level_2']],
        "level_2_idx": [idx for idx, sg in enumerate(skill_groups) if sg['group_level_2'] == this_group['group_level_2']],
        "level_1": [sg['conceptUri'] for sg in skill_groups if sg['group_level_1'] == this_group['group_level_1']],
        "level_1_idx": [idx for idx, sg in enumerate(skill_groups) if sg['group_level_1'] == this_group['group_level_1']],
      }
      
      # Add exclusive index lists (only sibling groups, without self group)
      grps["level_3_idx_excl"] = [idxn for idxn in grps["level_3_idx"] if idxn != idx]
      grps["level_2_idx_excl"] = [idxn for idxn in grps["level_2_idx"] if idxn not in grps["level_3_idx"]]
      grps["level_1_idx_excl"] = [idxn for idxn in grps["level_1_idx"] if idxn not in grps["level_2_idx"]]

      # if there are no sibling groups, use the group itself
      if len(grps['level_3_idx_excl']) == 0:
        grps['level_3_idx_excl'] = grps['level_3_idx']
      if len(grps['level_2_idx_excl']) == 0:
        grps['level_2_idx_excl'] = grps['level_2_idx']
      if len(grps['level_1_idx_excl']) == 0:
        grps['level_1_idx_excl'] = grps['level_1_idx']

      # calculate group-level similarity scores
      within_skill_similarity.append({
        'skill': skill.get('preferredLabel', ''),
        'skill_uri': skill.get('conceptUri', ''),
        'group_level_3_avg': np.nanmean(similarity_scores[grps['level_3_idx']]),
        'group_level_3_excl_max': np.nanmax(similarity_scores[grps['level_3_idx_excl']]),
        'group_level_2_avg': np.nanmean(similarity_scores[grps['level_2_idx']]),
        'group_level_2_excl_max': np.nanmax(similarity_scores[grps['level_2_idx_excl']]),
        'group_level_1_avg': np.nanmean(similarity_scores[grps['level_1_idx']]),
        'group_level_1_excl_max': np.nanmax(similarity_scores[grps['level_1_idx_excl']]),
      })
      
    save_list_to_csv(
      within_skill_similarity, 
      within_skill_similarity_file, 
      append=False
    )


  if not args.no_capability_skill_similarity:
    # load within-skill similarity
    within_skill_similarity = read_list_from_csv(within_skill_similarity_file)
    if not within_skill_similarity:
      raise ValueError("Within-skill similarity not found")

    # Find indices of automation and augmentation capabilities in all_capabilities
    automation_indices = [i for i, cap in enumerate(capability_df['capability']) if cap in automation_capabilities] # indices are relative to the capability_df not all_capabilities
    augmentation_indices = [i for i, cap in enumerate(capability_df['capability']) if cap in augmentation_capabilities]
    company_indices = [i for i, cap in enumerate(capability_df['capability']) if cap in company_capabilities]
    report_indices = [i for i, cap in enumerate(capability_df['capability']) if cap in report_capabilities]
    
    # calculate summary scores
    summary_scores = []
    
    overall_cut_off_values = {
      'level_3': np.nanmean([float(ws['group_level_3_excl_max']) for ws in within_skill_similarity]),
      'level_2': np.nanmean([float(ws['group_level_2_excl_max']) for ws in within_skill_similarity]),
      'level_1': np.nanmean([float(ws['group_level_1_excl_max']) for ws in within_skill_similarity]),
    }
    print(f"Overall cut-off values: {overall_cut_off_values}") # {'level_3': 0.6419223837660718, 'level_2': 0.5994165161048206, 'level_1': 0.6198785284251925}

    i = 0
    for idx, skill in enumerate(esco_skills):
      # get cut-off values for skill groups
      # this_skill_group = [ws for ws in within_skill_similarity if ws['skill_uri'] == skill['conceptUri']]
      # if len(this_skill_group) == 0:
      #   no_skill_group = True
      #   cut_off_values = overall_cut_off_values
      # else:
      #   no_skill_group = False
      #   cut_off_values = {
      #     'level_3': np.nanmax([float(this_skill_group[0]['group_level_3_excl_max']), overall_cut_off_values['level_3']]), # but no lower than overall cut-off
      #     'level_2': np.nanmax([float(this_skill_group[0]['group_level_2_excl_max']), overall_cut_off_values['level_2']]),
      #     'level_1': np.nanmax([float(this_skill_group[0]['group_level_1_excl_max']), overall_cut_off_values['level_1']]),
      #   }
      no_skill_group = False
      cut_off_values = overall_cut_off_values

      i += 1
      print(f"Processing skill {i} of {len(esco_skills)}")
      # calculate summary scores
      cosine_similarities = cosine_similarity_np(
        np.tile(skill_vectors[idx], (len(capability_vectors), 1)), 
        capability_vectors
      )
      
      # Create subset arrays using the indices
      cosine_similarities_automation = cosine_similarities[automation_indices] if automation_indices else np.array([])
      cosine_similarities_augmentation = cosine_similarities[augmentation_indices] if augmentation_indices else np.array([])
      cosine_similarities_company = cosine_similarities[company_indices] if company_indices else np.array([])
      cosine_similarities_report = cosine_similarities[report_indices] if report_indices else np.array([])

      summary_scores.append({
        'esco_skill_label': skill.get('preferredLabel', ''),
        'esco_skill_uri': skill.get('conceptUri', ''),
        'max_similarity': np.nanmax(cosine_similarities), 
        'max_similarity_automation_intent': np.nanmax(cosine_similarities_automation) if len(cosine_similarities_automation) > 0 else None,
        'max_similarity_augmentation_intent': np.nanmax(cosine_similarities_augmentation) if len(cosine_similarities_augmentation) > 0 else None,
        'top5_similarity': np.nanmean(np.sort(cosine_similarities)[-5:]),
        'logsumexp_similarity_5': log_sum_exp(cosine_similarities, gamma=5),
        'logsumexp_similarity_10': log_sum_exp(cosine_similarities, gamma=10),
        'logsumexp_similarity_100': log_sum_exp(cosine_similarities, gamma=100),
        'n_similar_l3': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_3'])) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l3_automation_intent': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_3'] and i in automation_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l3_augmentation_intent': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_3'] and i in augmentation_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l3_company_source': [  
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_3'] and i in company_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l3_report_source': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_3'] and i in report_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'mean_similarity_l3': [np.nanmean(cosine_similarities[cosine_similarities >= cut_off_values['level_3']]) if not no_skill_group else None][0],
        'mean_similarity_l3_automation_intent': [np.nanmean(cosine_similarities_automation[cosine_similarities_automation >= cut_off_values['level_3']]) if not no_skill_group else None][0],
        'mean_similarity_l3_augmentation_intent': [np.nanmean(cosine_similarities_augmentation[cosine_similarities_augmentation >= cut_off_values['level_3']]) if not no_skill_group else None][0],
        'n_similar_l2': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_2'])) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l2_automation_intent': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_2'] and i in automation_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l2_augmentation_intent': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_2'] and i in augmentation_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l1': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_1'])) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l1_automation_intent': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_1'] and i in automation_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'n_similar_l1_augmentation_intent': [
            len(set(release_idxs[i] for i, sim in enumerate(cosine_similarities) 
                 if sim >= cut_off_values['level_1'] and i in augmentation_indices)) 
            if not no_skill_group 
            else None
        ][0],
        'mean_similarity': np.nanmean(cosine_similarities),
        # 'mean_similarity_automation_intent': np.nanmean(cosine_similarities_automation) if len(cosine_similarities_automation) > 0 else None,
        # 'mean_similarity_augmentation_intent': np.nanmean(cosine_similarities_augmentation) if len(cosine_similarities_augmentation) > 0 else None,
        'max_similarity_capability': all_capabilities[np.nanargmax(cosine_similarities)],
        'max_similarity_capability_automation_intent': all_capabilities[np.nanargmax(cosine_similarities_automation)] if len(cosine_similarities_automation) > 0 else None,
        'max_similarity_capability_augmentation_intent': all_capabilities[np.nanargmax(cosine_similarities_augmentation)] if len(cosine_similarities_augmentation) > 0 else None,
      })

    # save results
    print(f"Total summary scores: {len(summary_scores)}") # 10831
    print(f"Total summary scores with n_similar_l3 != 0: {len([ss for ss in summary_scores if ss['n_similar_l3'] != 0])}") # 1709
    print(f"Total summary scores with n_similar_l3_automation_intent != 0: {len([ss for ss in summary_scores if ss['n_similar_l3_automation_intent'] != 0])}") # 1060
    print(f"Total summary scores with n_similar_l3_augmentation_intent != 0: {len([ss for ss in summary_scores if ss['n_similar_l3_augmentation_intent'] != 0])}") # 1064
    print(f"Total summary scores with n_similar_l3_company_source != 0: {len([ss for ss in summary_scores if ss['n_similar_l3_company_source'] != 0])}") # 1381
    print(f"Total summary scores with n_similar_l3_report_source != 0: {len([ss for ss in summary_scores if ss['n_similar_l3_report_source'] != 0])}") # 790
    print(f"Total summary scores with n_similar_l2 != 0: {len([ss for ss in summary_scores if ss['n_similar_l2'] != 0])}") # 3224
    print(f"Total summary scores with n_similar_l2_automation_intent != 0: {len([ss for ss in summary_scores if ss['n_similar_l2_automation_intent'] != 0])}") # 2177
    print(f"Total summary scores with n_similar_l2_augmentation_intent != 0: {len([ss for ss in summary_scores if ss['n_similar_l2_augmentation_intent'] != 0])}") # 2195
    
    save_list_to_csv(summary_scores, skills_output_file, append=False)
    print(f"Saved final results to {skills_output_file}")
  