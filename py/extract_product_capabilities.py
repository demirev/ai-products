import os
import json
import spacy
import random
import argparse
from find_relevant_releases import replace_quotes, read_list_from_csv, calculate_semantic_similarity, save_list_to_csv
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
    "Report describing an AI Product",
    "Other"
  ]

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


def score_press_release_similarity(
  scored_releases, 
  threshold_release = {
    "adoption" : {
      "max" : 0.7,
      "avg" : 0.5,
      "top_3_avg" : 0.65,
      "threshold_count" : 0.5,
      "logsumexp" : 0.5
    },
    "launch" : {
      "max" : 0.7,
      "avg" : 0.5,
      "top_3_avg" : 0.65,
      "threshold_count" : 0.5,
      "logsumexp" : 0.5
    }
  }, 
  dedup_headers = True
):
  accepted_releases = []
  rejected_releases = []
  
  for release in scored_releases:
    adoption_criteria = (
      float(release['adoption_similarity_max']) > threshold_release['adoption']['max'] +
      float(release['adoption_similarity_avg']) > threshold_release['adoption']['avg'] +
      float(release['adoption_similarity_top_3_avg']) > threshold_release['adoption']['top_3_avg'] +
      float(release['adoption_similarity_threshold_count']) > threshold_release['adoption']['threshold_count'] +
      float(release['adoption_similarity_logsumexp']) > threshold_release['adoption']['logsumexp'] >= 2
    )
    
    launch_criteria = (
      float(release['launch_similarity_max']) > threshold_release['launch']['max'] +
      float(release['launch_similarity_avg']) > threshold_release['launch']['avg'] +
      float(release['launch_similarity_top_3_avg']) > threshold_release['launch']['top_3_avg'] +
      float(release['launch_similarity_threshold_count']) > threshold_release['launch']['threshold_count'] +
      float(release['launch_similarity_logsumexp']) > threshold_release['launch']['logsumexp'] >= 2
    )
    
    if adoption_criteria or launch_criteria:
      accepted_releases.append(release)
    else:
      rejected_releases.append(release)

  if dedup_headers:
    # remove releases with duplicate header
    seen_headers = set()
    unique_releases = []
    for release in accepted_releases:
      if release['header'] not in seen_headers:
        unique_releases.append(release)
        seen_headers.add(release['header'])
    accepted_releases = unique_releases

  return accepted_releases, rejected_releases


def query_gpt(
  text, 
  sysprompt = open("py/extract_capability_prompt.txt", "r").read(), 
  client=client,
  responseFormat = None,
  model = "gpt-4o"
):
  if responseFormat is None:
    completion = client.chat.completions.create(
      model=model,
      messages=[
        {"role": "developer", "content": sysprompt},
        {"role": "user", "content": text}
      ]
    )
    return completion.choices[0].message.content
  else:
    completion = client.beta.chat.completions.parse(
      model=model,
      messages=[
        {"role": "developer", "content": sysprompt},
        {"role": "user", "content": text}
      ],
      response_format=responseFormat
    )
    return completion.choices[0].message.parsed


def classify_all_releases_llm(
  scored_releases,
  sysprompt = open("py/classify_release_prompt.txt", "r").read(),
  client=client,
  file_name = None
):
  i = 0
  for release in scored_releases:
    i += 1
    print(f"processing release {i} of {len(scored_releases)}")
    if 'document_type' in release and release['document_type'] != '':
      print("document type already exists")
      continue
    content = release.get('header', '') + " " + release.get('body', '')
    if content != '':
      release.update({
        'document_type': query_gpt(
          content, 
          sysprompt=sysprompt, 
          client=client, 
          responseFormat=DocumentType,
          model="gpt-4o-mini"
        ).document_type # 0.7 sec / document with 4o. $0.0045 per document. 0.00025 per document with 4o-mini
      })
    else:
      release.update({
        'document_type': 'Other'
      })
    if file_name is not None and i % 10 == 0:
      save_list_to_csv(scored_releases, file_name, append=False)


def extract_capability_strings_for_all_releases(
  scored_releases,
  sysprompt = open("py/extract_capability_prompt.txt", "r").read(),
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
      release.update({
        'capability_string': query_gpt(
          content, sysprompt=sysprompt, client=client
        )
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
    default="results/relevant_releases.csv"
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
  file_name = args.file_name if args.file_name else "results/processed_press_releases.csv"
  checkpoint_file = args.llm_checkpoint if args.llm_checkpoint else "checkpoints/llm_checkpoint.csv"

  if not args.no_filtering:
    print("Reading press releases")
    all_releases = read_list_from_csv(args.input_file)
    processed_releases, rejected_releases = score_press_release_similarity(
      all_releases,
      threshold_release = {
        "adoption" : {
          "max" : 0.7,
          "avg" : 0.5,
          "top_3_avg" : 0.65,
          "threshold_count" : 0.5,
          "logsumexp" : 0.5
        },
        "launch" : {
          "max" : 0.7,
          "avg" : 0.5,
          "top_3_avg" : 0.65,
          "threshold_count" : 0.5,
          "logsumexp" : 0.5
        }
      }, 
      dedup_headers=True
    )
    
    # Save main filtered file
    save_list_to_csv(processed_releases, file_name)
    
    # Save samples
    sample_size = min(100, len(processed_releases))
    accepted_sample = random.sample(processed_releases, sample_size)
    save_list_to_csv(accepted_sample, file_name.replace('.csv', '_accepted_sample.csv'))
    
    sample_size = min(100, len(rejected_releases))
    rejected_sample = random.sample(rejected_releases, sample_size)
    save_list_to_csv(rejected_sample, file_name.replace('.csv', '_rejected_sample.csv'))
    
    print(f"Saved {len(processed_releases)} filtered releases")
    print(f"Saved {sample_size} sample accepted releases")
    print(f"Saved {sample_size} sample rejected releases")
  else:
    processed_releases = read_list_from_csv(file_name)
  
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
      sysprompt=open("py/extract_capability_prompt.txt", "r").read(),
      client=client,
      file_name=checkpoint_file
    )
    save_list_to_csv(processed_releases, file_name) # save final results
  
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
  print("Done")
  print(f"Results saved to {file_name}")