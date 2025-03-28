import os
import json
import random
import argparse
from py.filter_press_release_by_keyword import replace_quotes, read_list_from_csv, save_list_to_csv
from py.find_relevant_releases_finetune import clean_text
from datetime import datetime
from dotenv import load_dotenv
from pydantic import BaseModel
from openai import OpenAI
from typing import Literal
load_dotenv()

client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

class DocumentType(BaseModel):
  document_type: Literal[
    "AI Product Launch Announcement", 
    "AI Product Adoption Announcement", 
    "AI Product Market Report",
    "AI Product News",
    "Not Relevant"
  ]

class DocumentTypeWithIntent(DocumentType):
  intent_type: Literal[
    "End-to-End Processing",
    "Replacement Messaging",
    "Volume/Scale Emphasis",
    "Self-Correction Mechanisms",
    "Expert Amplification",
    "Insight Generation",
    "Human-in-the-Loop Design",
    "Not Relevant"
  ]

class DocumentTypeWithIntentAndCapabilities(DocumentTypeWithIntent):
  ai_capabilities: list[str]

class Capabilities(BaseModel):
  capabilities: list[str]

def read_random_article(articles_path = "data/articles", scored_relaese_path = "data/relevant_releases.csv"):
  # Selects a random file from articles_path and reads it. Used for testing extract_verb_noun_pairs
  if scored_relaese_path is None:
    all_dirs = os.listdir(articles_path)
    random_dir = random.choice(all_dirs)
    all_files = os.listdir(os.path.join(articles_path, random_dir))
    random_file = os.path.join(articles_path, random_dir, random.choice(all_files))
  else:
    scored_releases = read_list_from_csv(scored_relaese_path)
    # take only releases with score of > 0.8
    scored_releases = [release for release in scored_releases if float(release['adoption_similarity']) > 0.8]
    random_file = random.choice(scored_releases)['file_path']
    
  with open(random_file, 'r', encoding='utf-8') as f:
    file_content = f.read()
    #return(file_content)
    file_content = replace_quotes(
      file_content, 
      ['header', 'uploader', 'uploader_link', 'date', 'body']
    ) # fix some wrongly saved json files
    data = json.loads(file_content)
    content = data.get('header', '') + " " + data.get('body', '')
    return content


def read_specific_article(article_path):
  with open(article_path, 'r', encoding='utf-8') as f:
    file_content = f.read()
    #return(file_content)
    file_content = replace_quotes(
      file_content, 
      ['header', 'uploader', 'uploader_link', 'date', 'body']
    ) # fix some wrongly saved json files
    data = json.loads(file_content)
    content = data.get('header', '') + " " + data.get('body', '')
    return content


def get_unique_releases(all_releases):
  unique_releases = []
  seen_headers = set()
  for release in all_releases:
    if release['header'] not in seen_headers:
      unique_releases.append(release)
      seen_headers.add(release['header'])
  return unique_releases


def score_press_release_similarity(
  all_releases,
  all_scores,
  threshold_release = 0.7, # > 75% of manually classified releases are above this threshold
  dedup_headers = True
):
  accepted_releases = []
  rejected_releases = []
  
  for release in all_releases:
    # Find the matching score item where file_path matches
    matching_score = next((score for score in all_scores if score['file_path'] == release['file_path']), None)
    if matching_score:
      classification_score = float(matching_score.get('relevance_probability', 0))
    else:
      classification_score = 0  # Default if no matching score found
      
    if classification_score >= threshold_release:
      accepted_releases.append(release)
    else:
      rejected_releases.append(release)

  if dedup_headers:
    # remove releases with duplicate header
    unique_releases = get_unique_releases(accepted_releases)
    accepted_releases = unique_releases

  return accepted_releases, rejected_releases


def query_gpt(
  text, 
  sysprompt = open("prompts/extract_capability_prompt.txt", "r").read(), 
  client=client,
  responseFormat = None,
  model = "gpt-4o",
  seed = None,
  temperature = None
):
  text = clean_text(text)
  
  # Prepare base parameters
  params = {
    "model": model,
    "messages": [
      {"role": "developer", "content": sysprompt},
      {"role": "user", "content": text}
    ]
  }
  
  # Add optional parameters only if they're not None
  if seed is not None:
    params["seed"] = seed
  if temperature is not None:
    params["temperature"] = temperature
  
  if responseFormat is None:
    completion = client.chat.completions.create(**params)
    return completion.choices[0].message.content
  else:
    params["response_format"] = responseFormat
    completion = client.beta.chat.completions.parse(**params)
    return completion.choices[0].message.parsed


def test_gpt_query(
  file_path,
  sysprompt_file = "prompts/extract_capability_prompt.txt",
  client=client,
  responseFormat = None,
  model = "gpt-4o-mini",
  seed = None,
  temperature = None
):
  # for manually ensuring output makes sense, e.g
  # test_gpt_query(
  # file_path='data/articles/business-technology/unipart-announces-financial-results-for-the-year-ended-31-december-2023-302111405.json',
  # sysprompt_file = "prompts/classify_release_prompt.txt",
  # responseFormat=DocumentType,
  # model="gpt-4o-mini"
  # )
  # or 
  # test_gpt_query(
  # file_path='data/articles/business-technology/unipart-announces-financial-results-for-the-year-ended-31-december-2023-302111405.json',
  # sysprompt_file = "prompts/classify_release_extended_prompt.txt",
  # responseFormat=DocumentTypeWithIntent,
  # model="gpt-4o-mini"
  # )
  # or
  # test_gpt_query(
  # file_path='data/articles/business-technology/unipart-announces-financial-results-for-the-year-ended-31-december-2023-302111405.json',
  # sysprompt_file = "prompts/extract_capability_prompt.txt",
  # responseFormat=Capabilities,
  # model="gpt-4o-mini"
  # )
  sysprompt = open(sysprompt_file, "r").read()
  text = read_specific_article(file_path)
  res = query_gpt(
    text=text,
    sysprompt=sysprompt,
    client=client,
    responseFormat=responseFormat,
    model=model,
    seed=seed,
    temperature=temperature
  )
  return res


def count_tokens(texts: list[str], model: str = "gpt-4o") -> int:
	"""
	Count the number of tokens in a list of strings using OpenAI's tokenizer.
	
	Args:
		texts: List of strings to count tokens for
		model: The OpenAI model name to use for tokenization (default: "gpt-4o")
		
	Returns:
		Total token count for all strings in the list
	"""
	import tiktoken
	
	# Get the appropriate tokenizer for the model
	try:
		encoding = tiktoken.encoding_for_model(model)
	except KeyError:
		# Fall back to cl100k_base encoding if model not found
		encoding = tiktoken.get_encoding("cl100k_base")
	
	# Count tokens for each string and sum
	total_tokens = 0
	for text in texts:
		total_tokens += len(encoding.encode(text))
	
	return total_tokens


def extract_capability_strings_for_all_releases(
  scored_releases,
  sysprompt = open("prompts/extract_and_classify_prompt.txt", "r").read(),
  client=client,
  file_name = None
):
  i = 0
  for release in scored_releases:
    i += 1
    print(f"processing release {i} of {len(scored_releases)}")
    if 'capability_string' in release and release['capability_string'] != '':
      print("capability string already exists")
      continue
    content = release.get('header', '') + " " + release.get('body', '')
    if content != '':
      response = query_gpt(
         content, 
          sysprompt=sysprompt, 
          client=client,
          responseFormat=DocumentTypeWithIntentAndCapabilities,
          seed=145,
          temperature=None, # can't use temperature for o3-mini
          model="o3-mini-2025-01-31"
        )
      release.update({
        'document_type': response.document_type,
        'intent_type': response.intent_type,
        'capability_string': response.capabilities
      })
    else:
      release.update({
        'capability_string': ''
      })
    if file_name is not None and i % 10 == 0:
      save_list_to_csv(scored_releases, file_name, append=False)
  
  return scored_releases


if __name__ == "__main__":

  parser = argparse.ArgumentParser(
    description="Extract verb-noun pairs from AI-related press releases"
  )
  parser.add_argument(
    "--no-filtering", action="store_true", help="skip filtering press releases"
  )
  parser.add_argument(
    "--no-llm", action="store_true", help="skip capability extraction via LLM"
  )
  parser.add_argument(
    "--input-file", type=str, help="name of file to read press releases from",
    default="results/press_releases/filtered_press_releases.csv"
  )
  parser.add_argument(
    "-score-file", type=str, help="name of file to save results",
    default="results/press_releases/classified_press_releases.csv"
  )
  parser.add_argument(
    "--file-name", type=str, help="name of file to save results",
    default="results/processed_press_releases.csv"
  )
  parser.add_argument(
    "--llm-checkpoint", type=str, help="name of file to save checkpoint results",
    default="checkpoints/llm_checkpoint.csv"
  )

  args = parser.parse_args()

  print("Starting")
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

  # if no file name is provided, use the default name
  final_file_name = args.file_name if args.file_name else "results/processed_press_releases.csv"
  filtered_file_name = args.input_file.replace('.csv', '_scored.csv')
  checkpoint_file = args.llm_checkpoint if args.llm_checkpoint else "checkpoints/llm_checkpoint.csv"

  if not args.no_filtering:
    print("Reading press releases")
    all_releases = read_list_from_csv(args.input_file) # 132,590 total, 40,116 unique headers
    unique_releases = get_unique_releases(all_releases)
    all_scores = read_list_from_csv(args.score_file)
    processed_releases, rejected_releases = score_press_release_similarity(
      unique_releases,
      all_scores,
      threshold_release = 0.7, # > 75% of manually classified releases are above 0.7, 66-78% of releases above this threshold are deemed relevant
      dedup_headers=False # already done above
    )
    # 14226 approved at threshold 0.5 (80% of all relevant, 66.6% accuracy) (83.0% second stage accuracy, 70% of all relevant)
    # 12234 approved at threshold 0.6 (75.7% of all relevant, 76.8% accuracy) (88.5% second stage accuracy, 65.7% of all relevant)
    # 9916 approved at threshold 0.7, (64.2% of all relevant, 80.3% accuracy) (92.5% second stage accuracy, 52.8% of all relevant) 9002912 tokens (~$10 estimated for o3-mini)
    # XXXX approved at threshold 0.8, (42.8% of all relevant, 88.2% accuracy) (93.1% second stage accuracy, 38.6% of all relevant)

    # Save main filtered file
    save_list_to_csv(processed_releases, filtered_file_name) 
    
    # Save samples
    sample_size = min(200, len(processed_releases))
    accepted_sample = random.sample(processed_releases, sample_size)
    save_list_to_csv(accepted_sample, filtered_file_name.replace('.csv', '_accepted_sample.csv'))
    
    sample_size = min(200, len(rejected_releases))
    rejected_sample = random.sample(rejected_releases, sample_size)
    save_list_to_csv(rejected_sample, filtered_file_name.replace('.csv', '_rejected_sample.csv'))
    
    print(f"Saved {len(processed_releases)} filtered releases")
    print(f"Saved {sample_size} sample accepted releases")
    print(f"Saved {sample_size} sample rejected releases")
  else:
    processed_releases = read_list_from_csv(filtered_file_name)
  
  if not args.no_llm:
    print(f"Processing {len(processed_releases)} press releases")
    checkpoint_releases = read_list_from_csv(checkpoint_file)

    found_in_chkp = 0
    for release in processed_releases:
      if release['header'] in [item['header'] for item in checkpoint_releases]:
        release.update({
          'capability_string': next(
            item['capability_string'] for item in checkpoint_releases if item['header'] == release['header']
          )
        }) # adds previous results if run was interrupted
        found_in_chkp += 1
    print(f"Found {found_in_chkp} releases in checkpoint file")
    
    processed_releases = extract_capability_strings_for_all_releases(
      processed_releases, 
      sysprompt=open("prompts/extract_and_classify_prompt.txt", "r").read(),
      client=client,
      file_name=checkpoint_file
    )
    save_list_to_csv(processed_releases, final_file_name) # save final results
  
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
  print("Done")
  print(f"Results saved to {final_file_name}")