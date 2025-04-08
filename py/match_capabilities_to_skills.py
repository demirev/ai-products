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

nlp = spacy.load("en_core_web_lg")

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


def get_mean_text_vector(text, nlp=nlp):
  return(nlp(text).vector)


def get_sentence_embeddings(text, sentence_model=sentence_model):
  return sentence_model.encode(text)


def pull_all_capabilities(scored_releases):
	all_capabilities = []
	import json
	for release in scored_releases:
		capability = release.get('capability_string', '')
		release_type = release.get('document_type', '')
		#intent_type = release.get('intent_type', '')
		if capability != '' and capability != '""' and release_type != 'Not Relevant':
			# Parse the string as a JSON array
			try:  
				capability_list = json.loads(capability.replace("'", '"'))
				all_capabilities.extend(capability_list)
			except:
				print(f"Error parsing capability: {capability}")
	return all_capabilities


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


def calculate_all_similarity_scores(
  all_capabilities, esco_skills, nlp=nlp, threshold=0.7, embedding_function=get_bart_embeddings
):
  # calculate mean vectors
  capability_list = []
  i = 0
  for capability in all_capabilities:
    i += 1
    print(f"Gettin mean vector {i} of {len(all_capabilities)}")
    capability_vector = embedding_function(capability)
    capability_list.append({
      'capability': capability,
      'vector': capability_vector
    })

  i = 0
  for skill in esco_skills:
    i += 1
    print(f"Gettin mean vector {i} of {len(esco_skills)}")
    skill_string = skill.get('preferredLabel', '') + " " + skill.get('description', '') + " " + skill.get('altLabels', '')
    skill_vector = embedding_function(skill_string)
    skill['vector'] = skill_vector

  # calculate pairwise similarity scores
  similarity_scores = []
  i = 0
  for capability in capability_list:
    for skill in esco_skills:
      i+=1
      print(f"Processing {i} of {len(all_capabilities) * len(esco_skills)}")
      similarity = torch.nn.functional.cosine_similarity(
        capability['vector'], skill['vector']
      ).item()

      similarity_scores.append({
        'esco_skill_label': skill.get('preferredLabel', ''),
        'esco_skill_uri': skill.get('conceptUri', ''),
        'ai_capability': capability.get('capability', ''),
        'cosine_similarity': similarity
      })

  return similarity_scores


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


def calculate_all_similarity_scores_batched(
  all_capabilities, 
  esco_skills, 
  threshold=0.7,
  checkpoint_file="data/scored_esco_skills_checkpoint.csv",
  capability_vectors = None,
  embedding_function=get_bart_embeddings
):
  # Calculate mean vectors for capabilities
  if capability_vectors is None:
    capability_vectors = np.array([embedding_function(capability) for capability in all_capabilities])
  
  # Initialize the list for storing similarity scores
  similarity_scores = []

  # Iterate over each skill
  for i, skill in enumerate(esco_skills):
    print(f"Processing skill {i+1} of {len(esco_skills)}")
    # Calculate the mean vector for the current skill
    skill_string = skill.get('preferredLabel', '') + " " + skill.get('description', '') #+ " " + skill.get('altLabels', '')
    skill_vector = embedding_function(skill_string)

    # Calculate cosine similarity between the skill vector and all capability vectors
    # Here, skill_vector needs to be repeated to match the number of capabilities for batch processing
    repeated_skill_vector = np.tile(skill_vector, (len(all_capabilities), 1))
    
    # Use the batch cosine similarity function
    cosine_similarities = cosine_similarity_np(capability_vectors, repeated_skill_vector)
    #all_capabilities = all_capabilities[np.isfinite(cosine_similarities)]
    #cosine_similarities = cosine_similarities[np.isfinite(cosine_similarities)]

    # Store the results
    similarity_scores.append({
      'esco_skill_label': skill.get('preferredLabel', ''),
      'esco_skill_uri': skill.get('conceptUri', ''),
      'max_similarity': np.nanmax(cosine_similarities), 
      'logsumexp_similarity_1': log_sum_exp(cosine_similarities, gamma=1),
      'logsumexp_similarity_2': log_sum_exp(cosine_similarities, gamma=2),
      'logsumexp_similarity_5': log_sum_exp(cosine_similarities, gamma=5),
      'logsumexp_similarity_10': log_sum_exp(cosine_similarities, gamma=10),
      'n_similar': np.sum(cosine_similarities > threshold),
      'mean_similarity': np.nanmean(cosine_similarities),
      'max_similarity_capability': all_capabilities[np.nanargmax(cosine_similarities)]
    })
    for j, capability in enumerate(all_capabilities):
      this_score = {
        'esco_skill_label': skill.get('preferredLabel', ''),
        'esco_skill_uri': skill.get('conceptUri', ''),
        'ai_capability': capability,
        'cosine_similarity': cosine_similarities[j]
      }
      # append to checkpoint file
      if checkpoint_file is not None:
        save_list_to_csv([this_score], checkpoint_file, append=True)

  return similarity_scores


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
    "--checkpoint-file", type=str, help="Path to the checkpoint file",
    default="checkpoints/scored_esco_skills_checkpoint.csv"
  )
  parser.add_argument(
    "--no-similarity", action="store_true", help="Do not calculate similarity scores",
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
  capability_skill_similarity_file = args.capability_skill_similarity_file
  within_skill_similarity_file = "results/scored_esco_skills/within_skill_similarity.csv"
  skill_hierarchy_file = "data/esco/broaderRelationsSkillPillar_en.csv"
  skills_output_file = args.skills_output_file

  scored_releases = read_list_from_csv(scored_releases_file)
  esco_skills = read_list_from_csv(esco_skills_file)
  skill_hierarchy = read_list_from_csv(skill_hierarchy_file)

  # filter only relevant esco_skills
  esco_skills = [skill for skill in esco_skills if skill.get('status', '') == 'released' and skill.get('skillType', '') == 'skill/competence']
  len([sr for sr in scored_releases if sr["document_type"] != "Not Relevant" and sr["capability_string"] != "[]"])
  # 8257

  # only populated capabilities
  all_capabilities = pull_all_capabilities(scored_releases)
  len(all_capabilities) # 27414

  if not args.no_embedding:
    # embed capabilities
    capability_vectors = np.array([get_sentence_embeddings(capability) for capability in all_capabilities])
    
    # Save capabilities with vectors using pandas
    capability_df = pd.DataFrame({
      'capability': all_capabilities,
      'vector': [json.dumps(vector.tolist()) for vector in capability_vectors]
    })
    capability_df.to_csv(capability_vector_file, index=False)
    
    # embed skills
    skill_vectors = np.array([get_sentence_embeddings(skill.get('preferredLabel', '') + " " + skill.get('description', '')) for skill in esco_skills])
    
    # Save skills with vectors using pandas
    skill_df = pd.DataFrame({
      'skill': [json.dumps(skill) for skill in esco_skills],
      'vector': [json.dumps(vector.tolist()) for vector in skill_vectors]
    })
    skill_df.to_csv(skill_vector_file, index=False)

  # Read capability vectors using pandas
  capability_df = pd.read_csv(capability_vector_file)
  if not capability_df.empty:
    capability_vectors = np.array([np.array(json.loads(vector)) for vector in capability_df['vector']])
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

    # calculate summary scores
    summary_scores = []
    i = 0
    for idx, skill in enumerate(esco_skills):
      # get cut-off values for skill groups
      this_skill_group = [ws for ws in within_skill_similarity if ws['skill_uri'] == skill['conceptUri']]
      if len(this_skill_group) == 0:
        no_skill_group = True
        cut_off_values = {}
      else:
        no_skill_group = False
        cut_off_values = {
          'level_3': float(this_skill_group[0]['group_level_3_excl_max']),
          'level_2': float(this_skill_group[0]['group_level_2_excl_max']),
          'level_1': float(this_skill_group[0]['group_level_1_excl_max']),
        }

      i += 1
      print(f"Processing skill {i} of {len(esco_skills)}")
      # calculate summary scores
      cosine_similarities = cosine_similarity_np(
        np.tile(skill_vectors[idx], (len(capability_vectors), 1)), 
        capability_vectors
      )
      
      summary_scores.append({
        'esco_skill_label': skill.get('preferredLabel', ''),
        'esco_skill_uri': skill.get('conceptUri', ''),
        'max_similarity': np.nanmax(cosine_similarities), 
        'top5_similarity': np.nanmean(np.sort(cosine_similarities)[-5:]),
        'logsumexp_similarity_5': log_sum_exp(cosine_similarities, gamma=5),
        'logsumexp_similarity_10': log_sum_exp(cosine_similarities, gamma=10),
        'logsumexp_similarity_100': log_sum_exp(cosine_similarities, gamma=100),
        'n_similar_l3': [np.sum(cosine_similarities >= cut_off_values['level_3']) if not no_skill_group else None][0],
        'n_similar_l2': [np.sum(cosine_similarities >= cut_off_values['level_2']) if not no_skill_group else None][0],
        'n_similar_l1': [np.sum(cosine_similarities >= cut_off_values['level_1']) if not no_skill_group else None][0],
        'mean_similarity': np.nanmean(cosine_similarities),
        'max_similarity_capability': all_capabilities[np.nanargmax(cosine_similarities)]
      })

    # save results
    save_list_to_csv(summary_scores, skills_output_file, append=False)
    
  