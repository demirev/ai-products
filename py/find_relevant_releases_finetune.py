import os
import json
import re
import csv
import argparse
import torch
import numpy as np
import pandas as pd
from datetime import datetime
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, accuracy_score, roc_auc_score
from transformers import (
	DistilBertTokenizer, 
	DistilBertForSequenceClassification, 
	Trainer, 
	TrainingArguments,
	EarlyStoppingCallback
)
from datasets import Dataset


# Group related constants together
MODEL_CONFIG = {
	"model_name": "distilbert-base-uncased",  
	"tokenizer": None,  # Will be initialized later
	"model": None,      # Will be initialized later
	"max_length": 512
}

OUTPUT_DIR = "results/models/press_release_classifier"


# Initialize models
def initialize_models(model_name=None, num_labels=2):
	"""Initialize the tokenizer and model."""
	if model_name:
		MODEL_CONFIG["model_name"] = model_name
	
	# Initialize tokenizer
	MODEL_CONFIG["tokenizer"] = DistilBertTokenizer.from_pretrained(MODEL_CONFIG["model_name"])
	
	# Initialize model for sequence classification (binary in this case)
	MODEL_CONFIG["model"] = DistilBertForSequenceClassification.from_pretrained(
		MODEL_CONFIG["model_name"], 
		num_labels=num_labels
	)
	
	return MODEL_CONFIG["tokenizer"], MODEL_CONFIG["model"]


def read_list_from_csv(file_path):
	"""Read a list of dictionaries from a CSV file."""
	csv.field_size_limit(1000000)
	with open(file_path, mode='r', newline='', encoding='utf-8') as file:
		reader = csv.DictReader(file)
		return list(reader)


def clean_text(text):
	"""Remove artifacts like 3+ consecutive 't's or 'n's from text."""
	# First remove 3+ consecutive 't's
	text = re.sub(r't{3,}', ' ', text)
	# Then remove 3+ consecutive 'n's
	text = re.sub(r'n{3,}', ' ', text)
	
	# Remove promotional text and anything that follows it
	promo_text = "WANT YOUR COMPANY'S NEWS FEATURED ON PRNEWSWIRE"
	if promo_text in text:
		text = text.split(promo_text)[0].strip()
	
	return text


def load_labeled_data(labels_file, press_releases_file):
	"""
	Load and merge labeled data with press release content.
	
	Args:
		labels_file: Path to CSV file with manually labeled data
		press_releases_file: Path to CSV file with press release content
	
	Returns:
		Dataframe with press release content and labels
	"""
	# Load labels
	labels_df = pd.read_csv(labels_file)
	
	# Load press releases
	press_releases = read_list_from_csv(press_releases_file)
	press_releases_df = pd.DataFrame(press_releases)
	
	# Create a mapping from file_path to press release content
	file_path_to_content = {}
	for pr in press_releases:
		file_path = pr['file_path']
		text = pr['header'] + " " + pr['body'] if 'body' in pr else pr['header']
		# Clean text to remove artifacts
		text = clean_text(text)
		file_path_to_content[file_path] = text
	
	# Add content to labels dataframe
	labels_df['text'] = labels_df['file_path'].map(file_path_to_content)
	
	# Drop rows with missing text
	labels_df = labels_df.dropna(subset=['text'])
	
	print(f"Loaded {len(labels_df)} labeled press releases")
	
	return labels_df


def tokenize_data(texts, labels=None):
	"""Tokenize the texts for the model."""
	tokenizer = MODEL_CONFIG["tokenizer"]
	encodings = tokenizer(
		texts, 
		truncation=True,
		padding='max_length',
		max_length=MODEL_CONFIG["max_length"],
		return_tensors="pt"
	)
	
	# Create a dataset that the trainer can use
	data = {}
	data['input_ids'] = encodings['input_ids']
	data['attention_mask'] = encodings['attention_mask']
	
	if labels is not None:
		data['labels'] = torch.tensor(labels, dtype=torch.long)
	
	return data


def create_dataset(df, is_train=True):
	"""Create a HuggingFace dataset from a pandas dataframe."""
	texts = df['text'].tolist()
	
	if is_train:
		labels = df['is_relevant'].astype(int).tolist()
		data = tokenize_data(texts, labels)
	else:
		data = tokenize_data(texts)
	
	return Dataset.from_dict(data)


def train_model(
		train_dataset, 
		eval_dataset, 
		output_dir=OUTPUT_DIR, 
		batch_size=8, 
		epochs=3
  ):
	"""Train the model on the provided datasets."""
	# Create output directory if it doesn't exist
	os.makedirs(output_dir, exist_ok=True)
	
	# Define training arguments
	training_args = TrainingArguments(
		output_dir=output_dir,
		num_train_epochs=epochs,
		per_device_train_batch_size=batch_size,
		per_device_eval_batch_size=batch_size,
		warmup_steps=500,
		weight_decay=0.01,
		logging_dir=f"{output_dir}/logs",
		logging_steps=10,
		evaluation_strategy="epoch",
		save_strategy="epoch",
		load_best_model_at_end=True,
		metric_for_best_model="accuracy",
		greater_is_better=True,
	)
	
	# Calculate class weights for imbalanced dataset
	model = MODEL_CONFIG["model"]
	
	# Create trainer
	trainer = Trainer(
		model=model,
		args=training_args,
		train_dataset=train_dataset,
		eval_dataset=eval_dataset,
		compute_metrics=compute_metrics,
		callbacks=[EarlyStoppingCallback(early_stopping_patience=3)]
	)
	
	# Train the model
	trainer.train()
	
	# Save the model
	model.save_pretrained(output_dir)
	MODEL_CONFIG["tokenizer"].save_pretrained(output_dir)
	
	return trainer


def compute_metrics(eval_pred):
	"""Compute metrics for evaluation during training."""
	logits, labels = eval_pred
	predictions = np.argmax(logits, axis=1)
	
	# For AUC-ROC we need probabilities of the positive class
	probs = torch.nn.functional.softmax(torch.tensor(logits), dim=1).numpy()
	roc_auc = roc_auc_score(labels, probs[:, 1])  # Using probability of positive class
	
	return {
		'accuracy': accuracy_score(labels, predictions),
		'roc_auc': roc_auc,
		'report': classification_report(labels, predictions, output_dict=True)
	}


def predict(texts, model=None, tokenizer=None):
	"""
	Predict relevance of press releases using the fine-tuned model.
	
	Args:
		texts: List of texts to classify
		model: Optional model to use (uses MODEL_CONFIG["model"] if None)
		tokenizer: Optional tokenizer (uses MODEL_CONFIG["tokenizer"] if None)
	
	Returns:
		Dictionary with predictions and probabilities
	"""
	if model is None:
		model = MODEL_CONFIG["model"]
	
	if tokenizer is None:
		tokenizer = MODEL_CONFIG["tokenizer"]
	
	# Tokenize texts
	inputs = tokenizer(
		texts,
		padding=True,
		truncation=True,
		max_length=MODEL_CONFIG["max_length"],
		return_tensors="pt"
	)
	
	# Move inputs to the same device as model
	device = model.device
	inputs = {k: v.to(device) for k, v in inputs.items()}
	
	# Get predictions
	with torch.no_grad():
		outputs = model(**inputs)
	
	# Get probabilities using softmax
	probabilities = torch.nn.functional.softmax(outputs.logits, dim=1)
	
	# Convert to numpy for easier manipulation
	probabilities = probabilities.cpu().numpy()
	predictions = np.argmax(probabilities, axis=1)
	
	return {
		'predictions': predictions,
		'probabilities': probabilities
	}


def load_model_from_checkpoint(checkpoint_dir=OUTPUT_DIR):
	"""Load a fine-tuned model from checkpoint."""
	tokenizer = DistilBertTokenizer.from_pretrained(checkpoint_dir)
	model = DistilBertForSequenceClassification.from_pretrained(checkpoint_dir)
	
	MODEL_CONFIG["tokenizer"] = tokenizer
	MODEL_CONFIG["model"] = model
	
	return tokenizer, model


def classify_press_releases(
	input_file, 
	output_file, 
	model_dir=OUTPUT_DIR,
	batch_size=16,
	save_frequency=5,  # Save results every N batches
	overwrite=False    # Whether to overwrite existing output file or continue from where it left off
):
	"""
	Classify press releases using the fine-tuned model.
	
	Args:
		input_file: Path to CSV file with press releases
		output_file: Path to save classified press releases
		model_dir: Directory containing the fine-tuned model
		batch_size: Batch size for prediction
		save_frequency: Number of batches to process before saving to file
		overwrite: Whether to overwrite existing output file or continue from where it left off
	
	Returns:
		List of press releases with predictions added
	"""
	# Load model
	tokenizer, model = load_model_from_checkpoint(model_dir)
	device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
	model = model.to(device)
	
	# Load press releases
	press_releases = read_list_from_csv(input_file)
	print(f"Loaded {len(press_releases)} press releases for classification")
	
	# Create output directory if it doesn't exist
	os.makedirs(os.path.dirname(output_file), exist_ok=True)
	
	# Check if output file exists and we're continuing from previous run
	already_processed = set()
	if os.path.exists(output_file) and not overwrite:
		print(f"Output file {output_file} exists. Continuing from where we left off.")
		try:
			# Read the output file to determine which press releases were already processed
			processed_df = pd.read_csv(output_file)
			already_processed = set(processed_df['file_path'].tolist())
			print(f"Found {len(already_processed)} already processed press releases.")
		except Exception as e:
			print(f"Error reading existing file: {e}")
			print("Starting from scratch.")
			already_processed = set()
	
	# Filter out already processed press releases
	if already_processed and not overwrite:
		remaining_releases = [pr for pr in press_releases if pr['file_path'] not in already_processed]
		print(f"Remaining {len(remaining_releases)} out of {len(press_releases)} to process.")
		press_releases_to_process = remaining_releases
	else:
		press_releases_to_process = press_releases
	
	# If no press releases to process, we're done
	if not press_releases_to_process:
		print("All press releases have already been processed.")
		return press_releases
	
	# Determine fieldnames from the first press release and remove 'body'
	fieldnames = list(press_releases[0].keys()) + ['is_relevant_predicted', 'relevance_probability']
	if 'body' in fieldnames:
		fieldnames.remove('body')
	
	# Create output file and write header, or prepare to append
	if overwrite or not os.path.exists(output_file):
		file_mode = 'w'  # Write mode (overwrites)
	else:
		file_mode = 'a'  # Append mode
	
	# If we're starting from scratch, write the header
	if file_mode == 'w':
		with open(output_file, file_mode, newline='', encoding='utf-8') as f:
			writer = csv.DictWriter(f, fieldnames=fieldnames)
			writer.writeheader()
	
	# Process in batches
	all_predictions = []
	all_probabilities = []
	processed_count = 0
	processed_releases = []
	
	for i in range(0, len(press_releases_to_process), batch_size):
		batch = press_releases_to_process[i:i+batch_size]
		texts = [clean_text(pr['header'] + " " + pr.get('body', '')) for pr in batch]
		
		results = predict(texts, model, tokenizer)
		
		batch_predictions = results['predictions']
		batch_probabilities = results['probabilities'][:, 1]  # Probability of class 1
		
		# Add predictions to press releases in this batch
		for j, pr in enumerate(batch):
			pr_copy = pr.copy()  # Create a copy to avoid modifying the original
			# Remove body from the press release
			if 'body' in pr_copy:
				del pr_copy['body']
			if 'header' in pr_copy:
				pr_copy['header'] = clean_text(pr_copy['header'])
			pr_copy['is_relevant_predicted'] = int(batch_predictions[j])
			pr_copy['relevance_probability'] = float(batch_probabilities[j])
			processed_releases.append(pr_copy)
		
		processed_count += len(batch)
		
		# Save periodically or when all data is processed
		batches_processed = (i // batch_size) + 1
		if batches_processed % save_frequency == 0 or i + batch_size >= len(press_releases_to_process):
			# Append to file
			with open(output_file, 'a', newline='', encoding='utf-8') as f:
				writer = csv.DictWriter(f, fieldnames=fieldnames)
				writer.writerows(processed_releases)
			
			print(f"Saved batch of {len(processed_releases)} press releases. Total processed: {processed_count}/{len(press_releases_to_process)}")
			
			# Store predictions and probabilities for the full result
			all_predictions.extend(batch_predictions)
			all_probabilities.extend(batch_probabilities)
			
			# Clear processed releases to free memory
			processed_releases = []
		
		if (i + batch_size) % 100 == 0 or i + batch_size >= len(press_releases_to_process):
			print(f"Processed {min(i + batch_size, len(press_releases_to_process))}/{len(press_releases_to_process)} press releases")
	
	print(f"Saved classified press releases to {output_file}")
	
	# For the return value, merge already processed and newly processed releases
	if already_processed and not overwrite:
		# Create lookup dictionaries for both already processed and newly processed
		file_path_to_predictions = {}
		file_path_to_probabilities = {}
		
		# Map new predictions to file paths
		for i, pr in enumerate(press_releases_to_process):
			file_path = pr['file_path']
			file_path_to_predictions[file_path] = int(all_predictions[i])
			file_path_to_probabilities[file_path] = float(all_probabilities[i])
		
		# Update all press releases with predictions
		for pr in press_releases:
			file_path = pr['file_path']
			if file_path in file_path_to_predictions:
				pr['is_relevant_predicted'] = file_path_to_predictions[file_path]
				pr['relevance_probability'] = file_path_to_probabilities[file_path]
			elif file_path in already_processed:
				# For releases processed in previous runs, we need to read their values from the output file
				# This would be inefficient for large files, so we're assuming the return value
				# isn't critically important in the resume case
				pr['is_relevant_predicted'] = -1  # Placeholder
				pr['relevance_probability'] = -1.0  # Placeholder
	else:
		# Simple case: just update all press releases
		for i, pr in enumerate(press_releases):
			if i < len(all_predictions):
				pr['is_relevant_predicted'] = int(all_predictions[i])
				pr['relevance_probability'] = float(all_probabilities[i])
	
	return press_releases


def save_random_samples(input_file, output_file, n_samples=200):
	"""
	Save a random sample of rows from the input CSV file to the output CSV file.
	
	Args:
		input_file: Path to input CSV file
		output_file: Path to output CSV file for the samples
		n_samples: Number of random samples to save
	"""
	# Read the input file
	df = pd.read_csv(input_file)
	
	# Determine number of samples to take
	n_samples = min(n_samples, len(df))
	
	# Take random samples
	sampled_df = df.sample(n=n_samples, random_state=42)
	
	# Create output directory if it doesn't exist
	os.makedirs(os.path.dirname(output_file), exist_ok=True)
	
	# Save to CSV
	sampled_df.to_csv(output_file, index=False)
	
	print(f"Saved {n_samples} random samples to {output_file}")


if __name__ == "__main__":
	print("Starting")
	print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
	
	parser = argparse.ArgumentParser(
		description="Fine-tune a classifier for AI-related press releases"
	)
	parser.add_argument(
		"--labels-file", 
		type=str, 
		help="Path to CSV file with manually labeled data",
		default="results/press_releases/manual_labels.csv"
	)
	parser.add_argument(
		"--press-releases-file", 
		type=str, 
		help="Path to CSV file with press release content",
		default="results/press_releases/filtered_press_releases.csv"
	)
	parser.add_argument(
		"--output-dir", 
		type=str,
		help="Directory to save the fine-tuned model",
		default=OUTPUT_DIR
	)
	parser.add_argument(
		"--classify", 
		action="store_true",
		help="Run classification using a fine-tuned model",
		default=False
	)
	parser.add_argument(
		"--input-file", 
		type=str,
		help="Path to CSV file with press releases to classify",
		default="results/press_releases/filtered_press_releases.csv"
	)
	parser.add_argument(
		"--output-file", 
		type=str,
		help="Path to save classified press releases",
		default="results/press_releases/classified_press_releases.csv"
	)
	parser.add_argument(
		"--batch-size", 
		type=int,
		help="Batch size for training and prediction",
		default=8
	)
	parser.add_argument(
		"--epochs", 
		type=int,
		help="Number of training epochs",
		default=3
	)
	parser.add_argument(
		"--model-name", 
		type=str,
		help="Pretrained model to fine-tune",
		default="distilbert-base-uncased"
	)
	
	args = parser.parse_args()
	
	# Set output directory
	OUTPUT_DIR = args.output_dir
	
	if args.classify:
		# Classification mode - load model and classify press releases
		print("Classification mode")
		classify_press_releases(
			input_file=args.input_file,
			output_file=args.output_file,
			model_dir=args.output_dir,
			batch_size=args.batch_size
		)
		
		# Save random samples for manual inspection
		samples_output_file = args.output_file.replace('.csv', '_samples.csv')
		save_random_samples(
			args.output_file, 
			samples_output_file, 
			n_samples=500
		)
	else:
		# Training mode - fine-tune model on labeled data
		print("Training mode")
		
		# Initialize models
		initialize_models(model_name=args.model_name)
		
		# Load and prepare data
		labeled_data = load_labeled_data(
			args.labels_file, 
			args.press_releases_file
		)
		
		# Split into train and validation sets
		train_df, eval_df = train_test_split(
			labeled_data, 
			test_size=0.2, 
			stratify=labeled_data['is_relevant'], 
			random_state=42
		)
		
		print(f"Training on {len(train_df)} examples, validating on {len(eval_df)} examples")
		print(f"Class distribution in training set: {train_df['is_relevant'].value_counts().to_dict()}")
		
		# Create datasets
		train_dataset = create_dataset(train_df)
		eval_dataset = create_dataset(eval_df)
		
		# Train model
		trainer = train_model(
			train_dataset=train_dataset,
			eval_dataset=eval_dataset,
			output_dir=OUTPUT_DIR,
			batch_size=args.batch_size,
			epochs=args.epochs
		)
		
		# Evaluate model
		eval_results = trainer.evaluate()
		print("Evaluation results:")
		print(eval_results)
		
		# Create dataset with model scores on evaluation set
		print("Creating dataset with model scores on evaluation set...")
		
		# Get model predictions on evaluation set
		pred_output = trainer.predict(eval_dataset)
		logits = pred_output.predictions
		probabilities = torch.nn.functional.softmax(torch.tensor(logits), dim=1).numpy()
		predictions = np.argmax(logits, axis=1)
		true_labels = pred_output.label_ids
		
		# Create DataFrame with evaluation results
		eval_results_df = pd.DataFrame({
			'text': eval_df['text'].tolist(),
			'file_path': eval_df['file_path'].tolist(),
			'true_label': true_labels,
			'predicted_label': predictions,
			'probability_relevant': probabilities[:, 1]  # Probability of class 1 (relevant)
		})
		
		# Calculate if prediction was correct
		eval_results_df['correct'] = eval_results_df['true_label'] == eval_results_df['predicted_label']
		
		# Save to CSV
		eval_results_path = os.path.join(
			OUTPUT_DIR, "eval_predictions.csv"
		)
		eval_results_df.to_csv(eval_results_path, index=False)
		print(f"Saved evaluation results to {eval_results_path}")
	
	print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
	print("Done")
