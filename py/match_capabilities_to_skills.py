import os
import spacy
import argparse
import numpy as np
from find_relevant_releases import read_list_from_csv, save_list_to_csv
from datetime import datetime
from transformers import AutoTokenizer, AutoModel
import torch

nlp = spacy.load("en_core_web_lg")

# Initialize BART model and tokenizer
model_name = "facebook/bart-large"
tokenizer = AutoTokenizer.from_pretrained(model_name)
bart_model = AutoModel.from_pretrained(model_name)

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
	return embeddings


def pull_all_capabilities(scored_releases):
  all_capabilities = []
  for release in scored_releases:
    capability = release.get('capability_string', '')
    if capability != '' and capability != '""':
      all_capabilities.extend(capability.split("; "))
  return all_capabilities


def find_mean_text_vector(text, nlp):
  return(nlp(text).vector)


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


def calculate_all_similarity_scores(
  all_capabilities, esco_skills, nlp=nlp, threshold=0.7
):
  # calculate mean vectors
  capability_list = []
  i = 0
  for capability in all_capabilities:
    i += 1
    print(f"Gettin mean vector {i} of {len(all_capabilities)}")
    capability_vector = get_bart_embeddings(capability)
    capability_list.append({
      'capability': capability,
      'vector': capability_vector
    })

  i = 0
  for skill in esco_skills:
    i += 1
    print(f"Gettin mean vector {i} of {len(esco_skills)}")
    skill_string = skill.get('preferredLabel', '') + " " + skill.get('description', '') + " " + skill.get('altLabels', '')
    skill_vector = get_bart_embeddings(skill_string)
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
  all_capabilities, esco_skills, threshold=0.7,
  checkpoint_file="data/scored_esco_skills_checkpoint.csv",
  capability_vectors = None
):
  # Calculate mean vectors for capabilities
  if capability_vectors is None:
    capability_vectors = np.array([get_bart_embeddings(capability) for capability in all_capabilities])
  
  # Initialize the list for storing similarity scores
  similarity_scores = []

  # Iterate over each skill
  for i, skill in enumerate(esco_skills):
    print(f"Processing skill {i+1} of {len(esco_skills)}")
    # Calculate the mean vector for the current skill
    skill_string = skill.get('preferredLabel', '') + " " + skill.get('description', '') + " " + skill.get('altLabels', '')
    skill_vector = get_bart_embeddings(skill_string)

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
    default="results/processed_press_releases.csv",
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
    default="results/scored_esco_skills.csv"
  )
  parser.add_argument(
    "--capabilities-output-file", type=str, help="Path to the output file",
    default="results/scored_ai_capabilities.csv" 
  ) # TODO this is not currently used, delete possibly
  parser.add_argument(
    "--capability-vector-file", type=str, 
    help="Path to the file containing the capability vectors",
    default="checkpoints/capability_vectors.csv"
  )
  parser.add_argument(
    "--skill-vector-file", type=str, 
    help="Path to the file containing the skills vectors",
    default="checkpoints/skill_vectors.csv"
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
  scored_releases = read_list_from_csv(args.capabilities_input_file)
  esco_skills = read_list_from_csv(args.skills_input_file)

  # filter only relevant esco_skills
  esco_skills = [skill for skill in esco_skills if skill.get('status', '') == 'released' and skill.get('skillType', '') == 'skill/competence']
  
  # only populated capabilities
  all_capabilities = pull_all_capabilities(scored_releases)

  # calculate capability mean vectors
  capability_vectors = np.array([find_mean_text_vector(capability, nlp) for capability in all_capabilities])

  if args.capability_vector_file is not None:
    save_list_to_csv(
      [{'capability': capability, 'vector': vector} for capability, vector in zip(all_capabilities, capability_vectors)],
      args.capability_vector_file, append=False
    )

  if not args.no_similarity:
    # if checkpoint file is specified, delete it
    if args.checkpoint_file is not None and os.path.exists(args.checkpoint_file):
      os.remove(args.checkpoint_file)

    # calculate pairwise similarity
    scored_skills = calculate_all_similarity_scores_batched(
      all_capabilities, esco_skills, threshold=0.9,
      checkpoint_file=args.checkpoint_file, capability_vectors=capability_vectors
    )

  if args.skill_vector_file is not None:
    skill_vectors = np.array([find_mean_text_vector(skill.get('preferredLabel', '') + " " + skill.get('description', '') + " " + skill.get('altLabels', ''), nlp) for skill in esco_skills])
    save_list_to_csv(
      [{'skill': skill, 'vector': vector} for skill, vector in zip(esco_skills, skill_vectors)],
      args.skill_vector_file, append=False
    )

    # save results
    save_list_to_csv(scored_skills, args.skills_output_file, append=False)
    
  