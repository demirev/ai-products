import os
import json
import re
import csv
import argparse
import torch
import numpy as np
from transformers import AutoTokenizer, AutoModel
from torch.nn.functional import cosine_similarity
from datetime import datetime, timedelta
from sentence_transformers import SentenceTransformer, util

articles_path = "data/articles"

# Group related constants together
MODEL_CONFIG = {
    "bart": {
        "name": "facebook/bart-large",  # or "facebook/bart-base" for smaller model
        "tokenizer": None,  # Will be initialized later
        "model": None       # Will be initialized later
    },
    "sentence_transformer": {
        "name": 'all-MiniLM-L6-v2',
        "model": None       # Will be initialized later
    }
}

# Organize semantic search terms into a single dictionary
SEMANTIC_SEARCH_TERMS = {
    "company_actions": [
        "announce", "launch", "introduce", "release", 
        "implement", "deploy", "adopt", "integrate"
    ],
    "ai_terms": [
        "AI", "artificial intelligence", "machine learning", 
        "LLM", "large language model"
    ],
    "product_terms": [
        "solution", "platform", "system", "technology", 
        "product", "service", "application"
    ],
    "business_impacts": [
        "transform", "enhance", "improve", "optimize", "accelerate"
    ]
}

# Initialize models
def initialize_models():
    # Initialize BART model
    MODEL_CONFIG["bart"]["tokenizer"] = AutoTokenizer.from_pretrained(MODEL_CONFIG["bart"]["name"])
    MODEL_CONFIG["bart"]["model"] = AutoModel.from_pretrained(MODEL_CONFIG["bart"]["name"])
    
    # Initialize Sentence Transformer model
    MODEL_CONFIG["sentence_transformer"]["model"] = SentenceTransformer(MODEL_CONFIG["sentence_transformer"]["name"])

# Generate phrases programmatically
def generate_launch_phrases():
    phrases = []
    for action in SEMANTIC_SEARCH_TERMS["company_actions"][:4]:  # Use first 4 actions for launches
        for ai in SEMANTIC_SEARCH_TERMS["ai_terms"]:
            for product in SEMANTIC_SEARCH_TERMS["product_terms"]:
                phrases.append(f"{action} new {ai}-powered {product}")
                phrases.append(f"{action} {ai}-based {product}")
    return phrases

def generate_adoption_phrases():
    phrases = []
    for action in SEMANTIC_SEARCH_TERMS["company_actions"][4:]:  # Use last 4 actions for adoption
        for ai in SEMANTIC_SEARCH_TERMS["ai_terms"]:
            for impact in SEMANTIC_SEARCH_TERMS["business_impacts"]:
                phrases.append(f"{action} {ai} to {impact} operations")
                phrases.append(f"{action} {ai} for business transformation")
    return phrases

# Move these to be generated after model initialization
launch_phrases = None
adoption_phrases = None

def summarize_similarity(similarities, gamma=5, threshold=0.7):
  """Summarize similarity scores."""
  return {
    'max_similarity': max(similarities),
    'avg_similarity': sum(similarities) / len(similarities),
    'third_highest': sorted(similarities, reverse=True)[2],
    'above_threshold_count': sum(1 for s in similarities if s > threshold),
    'logsumexp': log_sum_exp(similarities, gamma=gamma)
  }


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


def get_bart_embeddings(texts, max_length=512):
    """Get embeddings from BART model for a single text or list of texts."""
    # Handle both single text and list of texts
    is_single_text = isinstance(texts, str)
    texts_list = [texts] if is_single_text else texts
    
    # Prepare inputs
    inputs = MODEL_CONFIG["bart"]["tokenizer"](
        texts_list,
        max_length=max_length,
        padding=True,
        truncation=True,
        return_tensors="pt"
    )
    
    # Get model output
    with torch.no_grad():
        outputs = MODEL_CONFIG["bart"]["model"](**inputs)
    
    # Use [CLS] token embedding (first token) as sentence representation
    embeddings = outputs.last_hidden_state[:, 0, :]
    
    # Return single embedding if input was single text
    return embeddings[0] if is_single_text else embeddings


def get_sentence_embeddings(texts):
    """Get embeddings from sentence transformer for a single text or list of texts."""
    # Handle both single text and list of texts
    is_single_text = isinstance(texts, str)
    texts_list = [texts] if is_single_text else texts
    embeddings = MODEL_CONFIG["sentence_transformer"]["model"].encode(texts_list)
    return embeddings


def calculate_semantic_similarity(
    text, 
    search_queries, 
    text_embedding = None,
    search_queries_embeddings = None, 
    embeddings_function=get_sentence_embeddings
  ):
  """Calculate weighted semantic similarity scores using BART."""

  # Get text embedding
  if text_embedding is None:
    text_embedding = embeddings_function(text)
  
  # Get embeddings for all search queries at once
  if search_queries_embeddings is None:
    query_embeddings = embeddings_function(search_queries)
  else:
    query_embeddings = search_queries_embeddings

  # Calculate cosine similarities
  # Expand text embedding to match query embeddings shape
  # text_embedding_expanded = text_embedding.expand(len(query_embeddings), -1)
  # similarities = cosine_similarity(
  #   text_embedding_expanded, 
  #   query_embeddings
  # ).squeeze().tolist()

  similarities = util.cos_sim(text_embedding, query_embeddings)
  
  if isinstance(similarities, float):
    similarities = [similarities]

  return summarize_similarity(similarities)


def save_list_to_csv(data, file_path, append=False):
  mode = 'a' if append else 'w'
  with open(file_path, mode=mode, newline='', encoding='utf-8') as file:
    fieldnames = data[0].keys() if data else []
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    
    if not append or file.tell() == 0:
      writer.writeheader()
    
    for item in data:
      writer.writerow(item)


def read_list_from_csv(file_path):
    csv.field_size_limit(1000000)
    with open(file_path, mode='r', newline='', encoding='utf-8') as file:
        reader = csv.DictReader(file)
        return list(reader)
    

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
  return lse / gamma - np.log(len(scores)) / gamma


def save_embeddings_to_csv(
    press_releases, 
    csv_path="results/press_releases/embeddings.csv", 
    overwrite=False,
    embeddings_function=get_sentence_embeddings
):
	"""
	Generate and save embeddings for press releases to a CSV file.
	
	Args:
		press_releases: List of press release dictionaries
		csv_path: Path to save the embeddings CSV
		overwrite: Whether to overwrite existing embeddings
	"""
	# Create directory if it doesn't exist
	os.makedirs(os.path.dirname(csv_path), exist_ok=True)
	
	# Get existing file paths if the file exists and we're not overwriting
	existing_file_paths = set()
	if os.path.exists(csv_path) and not overwrite:
		with open(csv_path, 'r', newline='', encoding='utf-8') as file:
			reader = csv.reader(file)
			headers = next(reader)  # Skip header row
			for row in reader:
				if row:  # Skip empty rows
					existing_file_paths.add(row[0])  # First column is file_path
	
	# Filter press releases that need processing
	to_process = []
	for item in press_releases:
		if item['file_path'] not in existing_file_paths or overwrite:
			to_process.append(item)
	
	print(f"Processing embeddings for {len(to_process)} press releases")
	
	# Process each press release
	for i, item in enumerate(to_process):
		# Generate embedding
		text = item['header'] + " " + item['body']
		embedding = embeddings_function(text)
		
		# Convert embedding tensor to list
		if isinstance(embedding, torch.Tensor):
			embedding_list = embedding.cpu().numpy().flatten().tolist()
		else:
			embedding_list = embedding.flatten().tolist()
		
		# Prepare row data
		row_data = [item['file_path']] + embedding_list
		
		# Append to CSV
		file_exists = os.path.exists(csv_path)
		with open(csv_path, 'a', newline='', encoding='utf-8') as file:
			writer = csv.writer(file)
			
			# Write header if file is new
			if not file_exists:
				embedding_dim = len(embedding_list)
				header = ['file_path'] + [f'dim_{j}' for j in range(embedding_dim)]
				writer.writerow(header)
			
			# Write data row
			writer.writerow(row_data)
		
		if (i + 1) % 10 == 0:
			print(f"Processed {i + 1}/{len(to_process)} embeddings")
	
	print(f"Embeddings saved to {csv_path}")


def score_press_release_similarity(
    press_releases, 
    phrases, 
    field_prefix, 
    embeddings_file="results/press_releases/embeddings.csv",
    overwrite=False,
    batch_size=100,
    embeddings_function=get_sentence_embeddings
):
	"""
	Score press releases with multiple similarity metrics using pre-computed embeddings.
	Processes embeddings in batches to avoid loading all into memory at once.
	"""
	# Get phrase embeddings once
	phrase_embeddings = embeddings_function(phrases)
	# Check if phrase_embeddings is already a numpy array or a torch tensor
	if isinstance(phrase_embeddings, torch.Tensor):
		phrase_embeddings_np = phrase_embeddings.cpu().numpy()
	else:
		phrase_embeddings_np = phrase_embeddings
	
	# Create a mapping of file_path to index in press_releases for quick lookup
	press_release_map = {item['file_path']: i for i, item in enumerate(press_releases)}
	
	# Track which press releases need processing
	to_process = set()
	for i, item in enumerate(press_releases):
		if not all(f"{field_prefix}_{metric}" in item for metric in 
			['max', 'avg', 'third_highest', 'threshold_count', 'logsumexp']) or overwrite:
			to_process.add(item['file_path'])
	
	print(f"Need to process {len(to_process)} press releases")
	
	# Process embeddings in batches
	processed_count = 0
	
	if os.path.exists(embeddings_file):
		with open(embeddings_file, 'r', newline='', encoding='utf-8') as file:
			reader = csv.reader(file)
			headers = next(reader)  # Skip header row
			
			current_batch = []
			file_paths_batch = []
			
			for row in reader:
				if not row:  # Skip empty rows
					continue
				
				file_path = row[0]
				
				# Skip if this press release doesn't need processing
				if file_path not in to_process or file_path not in press_release_map:
					continue
				
				# Add to current batch
				embedding_values = [float(val) for val in row[1:]]
				current_batch.append(embedding_values)
				file_paths_batch.append(file_path)
				
				# Process batch when it reaches the desired size
				if len(current_batch) >= batch_size:
					_process_batch(
						batch_embeddings=current_batch, 
						file_paths=file_paths_batch, 
						press_releases=press_releases, 
						press_release_map=press_release_map, 
						phrase_embeddings_np=phrase_embeddings_np, 
						field_prefix=field_prefix
					)
					processed_count += len(current_batch)
					print(f"Processed {processed_count}/{len(to_process)} press releases")
					
					# Clear batch
					current_batch = []
					file_paths_batch = []
			
			# Process any remaining items in the last batch
			if current_batch:
				_process_batch(
					batch_embeddings=current_batch, 
					file_paths=file_paths_batch, 
					press_releases=press_releases, 
					press_release_map=press_release_map, 
					phrase_embeddings_np=phrase_embeddings_np, 
					field_prefix=field_prefix
				)
				processed_count += len(current_batch)
				print(f"Processed {processed_count}/{len(to_process)} press releases")
	else:
		print(f"Warning: Embeddings file {embeddings_file} not found")
	
	print(f"Completed processing {processed_count} press releases")
	return press_releases


def _process_batch(
    batch_embeddings, 
    file_paths, 
    press_releases, 
    press_release_map, 
    phrase_embeddings_np, 
    field_prefix
  ):
	"""Helper function to process a batch of embeddings."""
	# Convert batch to numpy array
	batch_embeddings_np = np.array(batch_embeddings)
	
	# Process each item in the batch
	for i, file_path in enumerate(file_paths):
		# Get the press release index
		pr_index = press_release_map[file_path]
		
		# Calculate similarities for this item with all phrases
		item_embedding = batch_embeddings_np[i:i+1]
		
		# Calculate cosine similarities
		#similarities = cosine_similarity_np(item_embeddings_repeated, phrase_embeddings_np)
		similarities = util.cos_sim(item_embedding, phrase_embeddings_np)
		
		# Convert to list
		similarities_list = similarities.tolist()
		
		# Calculate summary metrics
		similarity_summary = summarize_similarity(similarities_list)
		
		# Store all metrics
		press_releases[pr_index][f"{field_prefix}_max"] = similarity_summary['max_similarity']
		press_releases[pr_index][f"{field_prefix}_avg"] = similarity_summary['avg_similarity']
		press_releases[pr_index][f"{field_prefix}_third_highest"] = similarity_summary['third_highest']
		press_releases[pr_index][f"{field_prefix}_threshold_count"] = similarity_summary['above_threshold_count']
		press_releases[pr_index][f"{field_prefix}_logsumexp"] = similarity_summary['logsumexp']


if __name__ == "__main__":
    print("Starting")
    print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

    parser = argparse.ArgumentParser(
        description="A script to find AI-related press releases"
    )
    parser.add_argument(
        "--source-file", 
        type=str, 
        help="name of file to read press releases from",
        default="results/press_releases/filtered_press_releases.csv"
    )
    parser.add_argument(
        "--results-file", 
        type=str, 
        help="name of file to save results",
        default="results/press_releases/relevant_press_releases.csv"
    )
    parser.add_argument(
        "--save-embeddings", action="store_true",
        help="save embeddings to CSV file",
        default=False
    )
    parser.add_argument(
        "--embeddings-file", type=str,
        help="path to save embeddings CSV file",
        default="results/press_releases/embeddings.csv"
    )
    parser.add_argument(
        "--score-file", type=str,
        help="path to save scores CSV file",
        default="results/press_releases/press_release_scores.csv"
    )

    args = parser.parse_args()

    # Fix: Use args.results_file instead of args.file_name
    file_name = args.results_file
    score_file = args.score_file

    # Initialize models before using them
    initialize_models()
    
    # Generate phrases after model initialization
    launch_phrases = generate_launch_phrases()
    adoption_phrases = generate_adoption_phrases()

    relevant_press_releases = read_list_from_csv(args.source_file)
    
    print(f"Processing {len(relevant_press_releases)} press releases")

    save_embeddings_to_csv(
        press_releases=relevant_press_releases,
        csv_path=args.embeddings_file,
        overwrite=False
    )

    # Extract only needed fields for scoring
    press_release_scores = [
        {k: v for k, v in pr.items() if k == 'header' or k == 'file_path'} 
        for pr in relevant_press_releases
    ]

    # Score press releases
    press_release_scores = score_press_release_similarity(
        press_releases=press_release_scores, 
        phrases=launch_phrases, 
        field_prefix="launch_similarity",
        embeddings_file=args.embeddings_file,
        overwrite=False,
        batch_size=100
    )
    press_release_scores = score_press_release_similarity(
        press_releases=press_release_scores, 
        phrases=adoption_phrases, 
        field_prefix="adoption_similarity",
        embeddings_file=args.embeddings_file,
        overwrite=False,
        batch_size=100
    )
    save_list_to_csv(press_release_scores, score_file)
 
    print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
    print("Done")
    print(f"Results saved to {file_name}")
