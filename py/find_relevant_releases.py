import os
import json
import re
import spacy
import csv
import argparse
from datetime import datetime, timedelta

articles_path = "data/articles"

nlp = spacy.load("en_core_web_lg")

search_query = nlp("AI product launch adoption new technology")

keywords = [
  "artificial intelligence", "AI", 
  # "launch", "adopt", "introduce", "release"
  # "new product", "new technology", "new solution", "new feature", 
  # "innovation", "cutting-edge", "state-of-the-art",
  # "revolutionize", "disrupt", "breakthrough", "innovative",
  "ChatGPT", "GPT", "GPT-3", "OpenAI", "Open AI", 
  "Claude", "Anthropic", "Llama", "Bard", "Gemini", "Deepmind",
  "LLM", "Turing", "DALL-E", "Codex", "Copilot", "Co-pilot",
  "Large Language Model",   "Deep Learning", "Neural Network", "Machine Learning", "Natural Language Processing",
  "Computer Vision", "Speech Recognition", "Generative Model", "Transformer",
  "Reinforcement Learning", "Supervised Learning", "Unsupervised Learning",
]


launch_phrases = [
  "Introducing our new AI-powered solution",
  "Launch of our latest AI innovation",
  "AI-driven technology debuts",
  "Unveiling our AI-based platform",
  "Our new product, powered by artificial intelligence",
  "AI-enhanced solution now available",
  "Introducing AI to our industry or domain",
  "Next-generation AI tool launch",
  "Expanding our portfolio with AI technology",
  "Revolutionizing our industry or domain with AI",
  "AI-powered product launch",
  "AI-driven innovation"
]


adoption_phrases = [
  "Adopting AI for for our process or task",
  "Implementing AI-driven processes",
  "Transforming our business with AI",
  "Embracing AI to enhance our business performance",
  "Integrating artificial intelligence in our workflows",
  "Leveraging AI for better business outcomes",
  "Our journey to AI adoption",
  "Pioneering AI innovation in our industry or domain",
  "AI as a catalyst for change, improvement, innovation in our company",
  "Enhancing our products and services with AI capabilities",
  "AI-powered transformation of our business"
]


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


def calculate_semantic_similarity(text, search_queries, nlp = nlp):
  """Calculate the semantic similarity between the text and the search queries."""
  doc = nlp(text)
  similarities = [doc.similarity(nlp(sq)) for sq in search_queries]
  return max(similarities)


def is_relevant_press_release(text, search_query):
    # Process the text with spaCy to lemmatize
    doc = nlp(text)
    # Lemmatize the text of the press release
    text_lemmatized = " ".join([token.lemma_ for token in doc])
    # Check for semantic similarity
    doc_search_query = nlp(search_query)
    doc_text_lemmatized = nlp(text_lemmatized)
    return doc_text_lemmatized.similarity(doc_search_query) > 0.5


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


def score_press_release_similarity(press_releases, phrases, field_name, overwirte=False):
  """Score the press releases based on semantic similarity with the given phrases."""
  current_index = 0
  for item in press_releases:
    if field_name in item and not overwirte:
      continue # don't overwrite existing scores

    similarity = calculate_semantic_similarity(
      item['header'], phrases
    )

    item[field_name] = similarity
    
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
      overwirte=False
    )
    relevant_press_releases = score_press_release_similarity(
      relevant_press_releases, 
      adoption_phrases, "adoption_similarity",
      overwirte=False
    )
    save_list_to_csv(relevant_press_releases, file_name)
  
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
  print("Done")
  print(f"Results saved to {file_name}")
