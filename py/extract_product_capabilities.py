import os
import json
import spacy
import random
import argparse
from find_relevant_releases import replace_quotes, read_list_from_csv, calculate_semantic_similarity, save_list_to_csv
from datetime import datetime
from dotenv import load_dotenv
from openai import OpenAI

load_dotenv()

client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

capability_phrases = [
  "Our AI-powered solution is capable of",
  "We have developed a new algorithm that",
  "Our product can",
  "We have created a new model that",
  "Our product does",
  "We have developed a new system that",
  "Our product will",
  "We have created a new tool that",
  "Our product is able to",
  "We have developed a new software that",
  "Our product has the capability to",
  "We have created a new application that"
]

nlp = spacy.load("en_core_web_lg")

def extract_verb_noun_pairs(text, nlp=nlp):
  verb_noun_pairs = []
  doc = nlp(text)  
  # Iterate through the sentences in the document
  for sent in doc.sents:
    # Find verb tokens
    verbs = [token for token in sent if token.pos_ == "VERB"]
    for verb in verbs:
      # For each verb, find directly connected nouns
      for child in verb.children:
        if child.dep_ in ["dobj", "pobj"]:  # PROPN for proper nouns
          verb_noun_pairs.append((verb.lemma_, child.lemma_))

  return verb_noun_pairs


def extract_verb_subject_noun_pairs(text, nlp=nlp):
  verb_subject_noun_pairs = []
  doc = nlp(text)
  for sent in doc.sents:
    # Find verb tokens
    verbs = [token for token in sent if token.pos_ == "VERB"]
    for verb in verbs:
      subjects = [child for child in verb.children if child.dep_ == "nsubj"]
      objects = [child for child in verb.children if child.dep_ in ["dobj", "pobj"]]
      
      # If both subjects and objects are found, add them to the list
      for subject in subjects:
        for obj in objects:
          verb_subject_noun_pairs.append((subject.lemma_, verb.lemma_, obj.lemma_))

  return verb_subject_noun_pairs


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


def find_similar_sentences(
    text, nlp=nlp, keyphrases=capability_phrases,
    threshold=0.8, verbose=True
):
  doc = nlp(text)
  similar_sentences = []
  for sent in doc.sents:
    similarity_score = calculate_semantic_similarity(
      sent.text, keyphrases, nlp=nlp
    )
    if verbose:
      print(f"{similarity_score:.2f}: {sent.text}")
    if similarity_score >= threshold:
      similar_sentences.append(sent.text)
  return similar_sentences


def find_relevant_sentences_for_all_releases(
  scored_releases, 
  threshold_release = 0.7, threshold_sentence = 0.8,
  dedup_headers = True
):
  if threshold_release is not None:
    scored_releases = [
      release for release in scored_releases if 
      float(release['adoption_similarity']) > threshold_release or
      float(release['launch_similarity']) > threshold_release
    ]
  
  if dedup_headers:
    # remove releases with duplicate header
    seen_headers = set()
    unique_releases = []
    for release in scored_releases:
      if release['header'] not in seen_headers:
        unique_releases.append(release)
        seen_headers.add(release['header'])
    scored_releases = unique_releases

  i = 0
  for release in scored_releases:
    i += 1
    print(f"processing release {i} of {len(scored_releases)}")
    with open(release['file_path'], 'r', encoding='utf-8') as f:
      file_content = f.read()
      file_content = replace_quotes(
        file_content, 
        ['header', 'uploader', 'uploader_link', 'date', 'body']
      ) # fix some wrongly saved json files
      data = json.loads(file_content)
      content = data.get('header', '') + " " + data.get('body', '')
      #print(content)
      release.update({
        'relevant_sentences': find_similar_sentences(
          content, threshold=threshold_sentence, verbose=False
        )
      })

  return scored_releases


def extract_capability_string(
  text, 
  sysprompt = open("py/extract_capability_prompt.txt", "r").read(), 
  client=client
):
  completion = client.chat.completions.create(
    model="gpt-4-turbo-preview",
    messages=[
      {"role": "system", "content": sysprompt},
      {"role": "user", "content": text}
    ]
  )
  return completion.choices[0].message.content


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
    content = release.get('relevant_sentences', '')
    content = " ".join(content)
    if content != '':
      release.update({
        'capability_string': extract_capability_string(
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
    "--no-shortening", action="store_true", help="skip selecting relevant sentences"
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

  if not args.no_shortening:
    print("Reading press releases")
    processed_releases = find_relevant_sentences_for_all_releases(
      read_list_from_csv(args.input_file),
      threshold_release=0.7, 
      threshold_sentence=0.8
    )
    save_list_to_csv(processed_releases, file_name)
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