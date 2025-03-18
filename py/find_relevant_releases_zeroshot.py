import os
import json
import re
import csv
import argparse
import torch
import numpy as np
from transformers import pipeline
from datetime import datetime, timedelta

# Zero-shot classification model config
MODEL_CONFIG = {
    "zero_shot": {
        "name":  "valhalla/distilbart-mnli-12-1", # "valhalla/distilbart-mnli-12-1" or "facebook/bart-large-mnli",
        "model": None,       # Will be initialized later
				#"template": "This press release is about {}."
				"template": "This press release {} contain detailed information about a new AI product being launched or adopted."
    }
}

# Define categories for classification
CLASSIFICATION_CATEGORIES = {
    "primary_categories": [
        "does", 
        "does not"
    ],
    "detailed_categories": [
        "New AI product launch",
        "AI platform update", 
        "AI partnership announcement",
        "AI implementation in operations",
        "AI integration with existing products",
        "Financial report",
        "Executive appointment",
        "Acquisition news",
        "Industry trends",
        "Marketing campaign"
    ]
}

# Initialize models
def initialize_models():
    # Initialize zero-shot classification model
    MODEL_CONFIG["zero_shot"]["model"] = pipeline(
        "zero-shot-classification", 
        model=MODEL_CONFIG["zero_shot"]["name"]
    )


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


def classify_press_releases(
	press_releases,
	batch_size=10,
	overwrite=False
):
	"""
	Classify press releases using zero-shot classification.
	
	Args:
		press_releases: List of press release dictionaries
		batch_size: Number of press releases to process at once
		overwrite: Whether to overwrite existing classifications
	"""
	# Create a list to store press releases that need processing
	to_process = []
	for i, item in enumerate(press_releases):
		if (not all(k in item for k in ['primary_category', 'primary_score', 
										'detailed_category', 'detailed_score']) 
			or overwrite):
			to_process.append((i, item))
	
	print(f"Need to classify {len(to_process)} press releases")
	
	# Process in batches
	for i in range(0, len(to_process), batch_size):
		batch = to_process[i:i+batch_size]
		
		for idx, item in batch:
			# Combine header and body for classification
			text = item['header'] + " " + item['body']
			
			# Run primary classification
			primary_result = MODEL_CONFIG["zero_shot"]["model"](
				text, 
				CLASSIFICATION_CATEGORIES["primary_categories"],
				hypothesis_template=MODEL_CONFIG["zero_shot"]["template"]
			)
			
			# Run detailed classification
			# detailed_result = MODEL_CONFIG["zero_shot"]["model"](
			# 	text, 
			# 	CLASSIFICATION_CATEGORIES["detailed_categories"],
			# 	hypothesis_template="This press release describes {}."
			# )
			
			# Store results in the press release item
			press_releases[idx]['primary_category'] = primary_result['labels'][0]
			press_releases[idx]['primary_score'] = primary_result['scores'][0]
			press_releases[idx]['primary_all_categories'] = ",".join(primary_result['labels'])
			press_releases[idx]['primary_all_scores'] = ",".join([str(s) for s in primary_result['scores']])
			
			# press_releases[idx]['detailed_category'] = detailed_result['labels'][0]
			# press_releases[idx]['detailed_score'] = detailed_result['scores'][0]
			# press_releases[idx]['detailed_all_categories'] = ",".join(detailed_result['labels'])
			# press_releases[idx]['detailed_all_scores'] = ",".join([str(s) for s in detailed_result['scores']])
		
		print(f"Processed {min(i+batch_size, len(to_process))}/{len(to_process)} press releases")
	
	return press_releases


def filter_relevant_releases(press_releases, threshold=0.6):
	"""
	Filter press releases based on classification results.
	
	Args:
		press_releases: List of press release dictionaries
		threshold: Minimum score threshold for relevance
	"""
	relevant_releases = []
	
	for item in press_releases:
		# Check if it's an AI-related press release with high confidence
		is_ai_related = (
			item['primary_category'] in ['AI product announcement', 'AI technology adoption'] and
			float(item['primary_score']) >= threshold
		)
		
		if is_ai_related:
			relevant_releases.append(item)
	
	return relevant_releases


if __name__ == "__main__":
	print("Starting")
	print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

	parser = argparse.ArgumentParser(
		description="A script to find AI-related press releases using zero-shot classification"
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
		default="results/press_releases/relevant_press_releases_zeroshot.csv"
	)
	parser.add_argument(
		"--classification-file", 
		type=str,
		help="path to save classification results CSV file",
		default="results/press_releases/press_release_classifications.csv"
	)
	parser.add_argument(
		"--threshold", 
		type=float,
		help="confidence threshold for relevance",
		default=0.6
	)
	parser.add_argument(
		"--batch-size", 
		type=int,
		help="number of press releases to process at once",
		default=10
	)

	args = parser.parse_args()

	results_file = args.results_file
	classification_file = args.classification_file
	source_file = args.source_file
	threshold = args.threshold
	batch_size = args.batch_size

	# Initialize model before using it
	initialize_models()

	# Read press releases from source file
	press_releases = read_list_from_csv(source_file)
	
	print(f"Processing {len(press_releases)} press releases")

	# Classify press releases
	classified_releases = classify_press_releases(
		press_releases=press_releases,
		batch_size=batch_size,
		overwrite=False
	)
	
	# Save all classification results
	save_list_to_csv(classified_releases, classification_file)
	
	# Filter for relevant releases
	relevant_releases = filter_relevant_releases(
		press_releases=classified_releases,
		threshold=threshold
	)
	
	# Save relevant releases
	save_list_to_csv(relevant_releases, results_file)

	print(f"Identified {len(relevant_releases)} AI-related press releases out of {len(press_releases)} total")
	print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
	print("Done")
	print(f"Classification results saved to {classification_file}")
	print(f"Relevant press releases saved to {results_file}")
