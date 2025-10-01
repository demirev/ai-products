import os
import ast
import json
from datetime import datetime
from collections import defaultdict
import statistics

def parse_json_with_single_quotes(file_path):
    """Parse JSON file that uses single quotes instead of double quotes."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        # Use ast.literal_eval to safely parse the single-quoted dictionary
        return ast.literal_eval(content)
    except Exception as e:
        print(f"Error parsing {file_path}: {e}")
        return None

def parse_date(date_str):
    """Parse date string to datetime object."""
    try:
        # Remove timezone part if exists
        date_str = date_str.split(' ET')[0] if ' ET' in date_str else date_str
        
        # Remove time component if exists (format like "16 Feb, 2024, 08:30")
        if date_str.count(',') > 1 and any(c.isdigit() and c != ',' for c in date_str.split(',')[-1]):
            date_str = ','.join(date_str.split(',')[:-1])
            
        # Try multiple date formats
        formats_to_try = [
            "%b %d, %Y",  # Mar 05, 2024
            "%d %b, %Y",  # 13 Sep, 2023
            "%d %B, %Y",  # 16 February, 2024
            "%Y-%m-%d"    # 2024-02-16
        ]
        
        for fmt in formats_to_try:
            try:
                return datetime.strptime(date_str.strip(), fmt)
            except ValueError:
                continue
                
        # If we get here, none of the formats worked
        raise ValueError(f"Could not parse date: {date_str}")
    except Exception as e:
        print(f"Error parsing date '{date_str}': {e}")
        return None

def analyze_press_releases(data_dir):
    """Analyze press release data from the given directory."""
    
    if not os.path.exists(data_dir):
        print(f"Directory {data_dir} does not exist!")
        return
    
    # Statistics storage per folder
    folder_data = {}
    
    # Walk through all subdirectories
    for root, dirs, files in os.walk(data_dir):
        folder_name = os.path.basename(root)
        
        # Skip the root data/articles directory
        if root == data_dir:
            continue
            
        json_files = [f for f in files if f.endswith('.json')]
        file_count = len(json_files)
        
        print(f"Processing {folder_name}: {file_count} files...")
        
        # Initialize folder statistics
        folder_data[folder_name] = {
            'file_count': file_count,
            'dates': [],
            'uploaders': defaultdict(int),
            'body_lengths': []
        }
        
        # Process each JSON file
        for json_file in json_files:
            file_path = os.path.join(root, json_file)
            data = parse_json_with_single_quotes(file_path)
            
            if data is None:
                continue
                
            # Extract date
            if 'date' in data and data['date']:
                parsed_date = parse_date(data['date'].strip())
                if parsed_date:
                    folder_data[folder_name]['dates'].append(parsed_date)
            
            # Extract uploader
            if 'uploader' in data and data['uploader']:
                uploader = data['uploader'].strip()
                if uploader:
                    folder_data[folder_name]['uploaders'][uploader] += 1
            
            # Extract body length
            if 'body' in data and data['body']:
                folder_data[folder_name]['body_lengths'].append(len(data['body']))
    
    # Print results
    print("\n" + "="*80)
    print("PRESS RELEASE DATA ANALYSIS RESULTS - BY FOLDER")
    print("="*80)
    
    total_files = 0
    all_uploaders = set()  # Track all unique uploaders across folders
    
    # Print statistics for each folder
    for folder_name, stats in sorted(folder_data.items()):
        print(f"\n{folder_name.upper()} FOLDER STATISTICS:")
        print("="*60)
        
        # 1. File count
        file_count = stats['file_count']
        total_files += file_count
        print(f"1. NUMBER OF FILES: {file_count}")
        
        # 2. Date range
        print("\n2. DATE RANGE:")
        print("-" * 40)
        dates = stats['dates']
        if dates:
            min_date = min(dates)
            max_date = max(dates)
            print(f"Earliest date: {min_date.strftime('%B %d, %Y')}")
            print(f"Latest date  : {max_date.strftime('%B %d, %Y')}")
            print(f"Date span    : {(max_date - min_date).days} days")
        else:
            print("No valid dates found!")
        
        # 3. Unique uploaders
        print("\n3. UNIQUE UPLOADERS:")
        print("-" * 40)
        uploaders = stats['uploaders']
        print(f"Total unique uploaders: {len(uploaders)}")
        if len(uploaders) <= 10:  # Show all if not too many
            print("\nUploader list (with upload counts):")
            for uploader, count in sorted(uploaders.items(), key=lambda x: (-x[1], x[0])):
                print(f"  - {uploader}: {count} uploads")
        else:
            print("\nTop 5 uploaders by number of uploads:")
            for uploader, count in sorted(uploaders.items(), key=lambda x: (-x[1], x[0]))[:5]:
                print(f"  - {uploader}: {count} uploads")
            print(f"  ... and {len(uploaders) - 5} more")
        
        # 4. Body length statistics
        print("\n4. BODY LENGTH STATISTICS:")
        print("-" * 40)
        body_lengths = stats['body_lengths']
        if body_lengths:
            avg_length = statistics.mean(body_lengths)
            median_length = statistics.median(body_lengths)
            min_length = min(body_lengths)
            max_length = max(body_lengths)
            
            print(f"Average length : {avg_length:8.1f} characters")
            print(f"Median length  : {median_length:8.1f} characters")
            print(f"Minimum length : {min_length:8d} characters")
            print(f"Maximum length : {max_length:8d} characters")
            print(f"Total articles : {len(body_lengths):8d}")
        else:
            print("No valid body content found!")
        
        # Add folder's uploaders to overall set
        all_uploaders.update(stats['uploaders'].keys())
    
    # Overall summary
    print("\n" + "="*60)
    print(f"TOTAL FILES ACROSS ALL FOLDERS: {total_files}")
    print(f"TOTAL UNIQUE UPLOADERS ACROSS ALL FOLDERS: {len(all_uploaders)}")
    print("="*60)

def main():
    """Main function to run the analysis."""
    # You can modify this path as needed
    data_directory = "data/articles"
    
    print("Press Release Data Analyzer")
    print(f"Analyzing directory: {data_directory}")
    print("-" * 60)
    
    analyze_press_releases(data_directory)

if __name__ == "__main__":
    main()