import os
import json
import re
import spacy
import csv
import argparse
import torch
import numpy as np
from transformers import AutoTokenizer, AutoModel
from torch.nn.functional import cosine_similarity
from datetime import datetime, timedelta

articles_path = "data/articles"

nlp = spacy.load("en_core_web_lg")
model_name = "facebook/bart-large"  # or "facebook/bart-base" for smaller model
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModel.from_pretrained(model_name)

keywords = [
  "artificial intelligence", "AI", 
  "ChatGPT", "GPT", "GPT-3", "OpenAI", "Open AI", 
  "Claude", "Anthropic", "Llama", "Bard", "Gemini", "Deepmind",
  "LLM", "Turing", "DALL-E", "Codex", "Copilot", "Co-pilot",
  "Large Language Model",   "Deep Learning", "Neural Network", "Machine Learning", "Natural Language Processing",
  "Computer Vision", "Speech Recognition", "Generative Model", "Transformer",
  "Reinforcement Learning", "Supervised Learning", "Unsupervised Learning",
]

company_actions = [
  "announce", "launch", "introduce", "unveil", "release", "debut",
  "implement", "deploy", "adopt", "integrate", "incorporate"
]

ai_terms = [
  "AI", "artificial intelligence", "machine learning", "deep learning",
  "neural network", "LLM", "large language model"
]

product_terms = [
  "solution", "platform", "system", "technology", "product", "service",
  "tool", "application", "capability", "feature"
]

business_impacts = [
  "transform", "enhance", "improve", "optimize", "streamline",
  "revolutionize", "accelerate", "modernize"
]

# Generate phrases programmatically
def generate_launch_phrases():
  phrases = []
  for action in company_actions[:6]:  # Use first 6 actions for launches
    for ai in ai_terms:
      for product in product_terms:
        phrases.append(f"{action} new {ai}-powered {product}")
        phrases.append(f"{action} {ai}-based {product}")
  return phrases

def generate_adoption_phrases():
  phrases = []
  for action in company_actions[6:]:  # Use last 5 actions for adoption
    for ai in ai_terms:
      for impact in business_impacts:
        phrases.append(f"{action} {ai} to {impact} operations")
        phrases.append(f"{action} {ai} for business transformation")
  return phrases

launch_phrases = generate_launch_phrases()
adoption_phrases = generate_adoption_phrases()


def lemmatize_text(text):
  """Lemmatize the input text and return a list of lemmas."""
  doc = nlp(text)
  lemmas = [token.lemma_.lower() for token in doc if not token.is_stop and not token.is_punct]
  return " ".join(lemmas)


def search_keyphrases(content, keyphrases):
  """Search for keyphrases in the content and return a dictionary with the results."""
  # append word separator to each keyphrase
  keyphrases = [" " + kp + " " for kp in keyphrases]
  # replace all word separators with a single space
  content = re.sub(r'\s+', ' ', content)
  # remove punctuation and convert to lowercase
  normalized_content = re.sub(r'[^\w\s]', '', content.lower()) # only words and word spaces
  normalized_keyphrases = [re.sub(r'[^\w\s]', '', kp.lower()) for kp in keyphrases]
  found_keyphrases = {kp: kp in normalized_content for kp in normalized_keyphrases}
  return found_keyphrases


def count_keyphrases(content, keyphrases):
  """Count the occurrences of keyphrases in the content."""
  found_keyphrases = search_keyphrases(content, keyphrases)
  return sum(found_keyphrases.values())


def calculate_spacy_similarity(text, search_queries):
  """Original spaCy-based similarity calculation."""
  doc = nlp(text)
  similarities = [doc.similarity(nlp(sq)) for sq in search_queries]
  
  return similarities


def summarize_similarity(similarities):
  """Summarize similarity scores."""
  return {
    'max_similarity': max(similarities),
    'avg_similarity': sum(similarities) / len(similarities),
    'top_3_avg': sum(sorted(similarities, reverse=True)[:3]) / 3,
    'above_threshold_count': sum(1 for s in similarities if s > 0.5)
  }


def get_bart_embeddings(text, max_length=512):
  """Get embeddings from BART model."""
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
    outputs = model(**inputs)
  
  # Use [CLS] token embedding (first token) as sentence representation
  # Shape: (batch_size, hidden_size)
  embeddings = outputs.last_hidden_state[:, 0, :]
  return embeddings


def calculate_semantic_similarity(text, search_queries, nlp=nlp, use_spacy=False):
  """Calculate weighted semantic similarity scores using BART."""
  if use_spacy:
    similarities = calculate_spacy_similarity(text, search_queries)
  else:
    # Get text embedding
    text_embedding = get_bart_embeddings(text)
    
    # Get embeddings for all search queries
    query_embeddings = torch.cat([
        get_bart_embeddings(query) for query in search_queries
    ])
  
    # Calculate cosine similarities
    # Expand text embedding to match query embeddings shape
    text_embedding_expanded = text_embedding.expand(len(search_queries), -1)
    similarities = cosine_similarity(
      text_embedding_expanded, 
      query_embeddings
    ).squeeze().tolist()
  
  if isinstance(similarities, float):
    similarities = [similarities]

  return summarize_similarity(similarities)


def replace_quotes(text, field_names):
  """Replace single quotes with double quotes and fix some JSON formatting issues."""
  # replace all double quotes with single quotes
  text = re.sub(r'"', "'", text)
  # bring back double quotes for field names and values
  for field in field_names:
    field = re.escape(field)
    text = re.sub(rf"'{field}'", rf'"{field}"', text)
    text = re.sub(rf"\"{field}\":\s*'", rf'"{field}": "', text)
    text = re.sub(rf"',\s*\"{field}\"", rf'", "{field}"', text)
  # bring back double quotes before last }
  text = re.sub(r"'\s*}", '"}', text)
  # replace double escape with single escape
  text = re.sub(r'\\', '', text)
  # remove new lines
  text = re.sub(r'\n', '', text)
  return text


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
    with open(file_path, mode='r', newline='', encoding='utf-8') as file:
        reader = csv.DictReader(file)
        return list(reader)
    

def find_press_releases_with_keyphrases(directory, keyphrases, existing_press_releases=[], overwirte=False):
  """Traverse the directory and find press releases related to AI product launches or adoption."""
  relevant_press_releases = []
  for root, dirs, files in os.walk(directory):
    for file in files:
      if file.endswith(".json"):
        try:
          file_path = os.path.join(root, file)
          if file_path in existing_press_releases and not overwirte:
            continue
          with open(file_path, 'r', encoding='utf-8') as f:
            file_content = f.read()
            #return(file_content)
            file_content = replace_quotes(
              file_content, 
              ['header', 'uploader', 'uploader_link', 'date', 'body']
            ) # fix some wrongly saved json files
            data = json.loads(file_content)
            # Combine header and body to search for keywords
            content = data.get('header', '') + " " + data.get('body', '')
            if count_keyphrases(content, keyphrases) > 0:
              relevant_press_releases.append({
                'header': data.get('header'),
                'date': data.get('date'),
                'file_path': file_path
              })
        except Exception as e:
          print(f"Error processing {os.path.join(root, file)}: {e}")
  return relevant_press_releases


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
  return lse / gamma


def score_press_release_similarity(press_releases, phrases, field_prefix, overwrite=False):
  """Score press releases with multiple similarity metrics."""
  current_index = 0
  for item in press_releases:
    # Check if any of the metrics already exist
    if all(f"{field_prefix}_{metric}" in item for metric in 
      ['max', 'avg', 'top_3_avg', 'threshold_count']) and not overwrite:
      continue

    similarities = calculate_semantic_similarity(
      item['header'], phrases
    )
    # Store all metrics
    item[f"{field_prefix}_max"] = similarities['max_similarity']
    item[f"{field_prefix}_avg"] = similarities['avg_similarity']
    item[f"{field_prefix}_top_3_avg"] = similarities['top_3_avg']
    item[f"{field_prefix}_threshold_count"] = similarities['above_threshold_count']
    item[f"{field_prefix}_logsumexp"] = log_sum_exp(list(similarities.values()))
        
    current_index += 1
    if current_index % 100 == 0:
      print(f"Processed {current_index} press releases")
    
  return press_releases


if __name__ == "__main__":
  print("Starting")
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

  parser = argparse.ArgumentParser(
    description="A script to find AI-related press releases"
  )
  parser.add_argument(
    "--no-keywords", action="store_true", help="skip keyword filtering"
  )
  parser.add_argument(
    "--no-semantic", action="store_true", help="skip semantic similarity"
  )
  parser.add_argument(
    "--file-name", type=str, help="name of file to save results",
    default="results/relevant_press_releases.csv"
  )
  parser.add_argument(
    "--use-spacy", action="store_true",
    help="use spaCy for similarity calculation",
    default=False
  )

  args = parser.parse_args()


  # if no file name is provided, use the default name
  file_name = args.file_name if args.file_name else "results/relevant_press_releases.csv"

  # if file exists, read the data from the file
  if os.path.exists(file_name):
    relevant_press_releases = read_list_from_csv(file_name)
  else:
    relevant_press_releases = []

  if not args.no_keywords:
    relevant_press_releases = find_press_releases_with_keyphrases(
      articles_path, 
      keywords,
      existing_press_releases=[item['file_path'] for item in relevant_press_releases],
      overwirte=False
    )
    save_list_to_csv(relevant_press_releases, file_name)
  else:
    relevant_press_releases = read_list_from_csv(file_name)
  
  if not args.no_semantic:
    print(f"Processing {len(relevant_press_releases)} press releases")
    relevant_press_releases = score_press_release_similarity(
      relevant_press_releases, 
      launch_phrases, "launch_similarity",
      overwirte=False,
      use_spacy=args.use_spacy
    )
    relevant_press_releases = score_press_release_similarity(
      relevant_press_releases, 
      adoption_phrases, "adoption_similarity",
      overwirte=False,
      use_spacy=args.use_spacy
    )
    save_list_to_csv(relevant_press_releases, file_name)
  
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
  print("Done")
  print(f"Results saved to {file_name}")
