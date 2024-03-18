import requests
import csv
from datetime import datetime, timedelta
import argparse
import os

def fetch_data(timestamp):
  url = f'https://europa.eu/eures/eures-apps/searchengine/page/statistics/getMostRequiredOccupationStats?lang=en&_={timestamp}&app=2.17.1-build-1'
  print(f"Fetching data from {url}")
  response = requests.get(url)
  return response.json()

def add_months(source_date, months):
  month = source_date.month - 1 + months
  year = source_date.year + month // 12
  month = month % 12 + 1
  day = 1 # always use the 1st day of the month
  return datetime(year, month, day)

def main(start_date, end_date, output_file, overwrite):
  if overwrite and os.path.exists(output_file):
    os.remove(output_file)

  file_exists = os.path.isfile(output_file)
  with open(output_file, 'a', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    if not file_exists:
      writer.writerow(['parent_code', 'occupation_code', 'n_jobs', 'n_posts', 'date'])

    current_date = datetime.strptime(start_date, '%Y-%m-%d')
    end_date = datetime.strptime(end_date, '%Y-%m-%d')

    while current_date <= end_date:
      print(f"Processing {current_date.strftime('%Y-%m-%d')}")
      timestamp = int(current_date.timestamp() * 1000)
      data = fetch_data(timestamp)

      for parent in data['statisticDtos']:
        for child in parent['children']:
          date_converted = datetime.utcfromtimestamp(child['date'] / 1000).strftime('%Y-%m-%d')
          writer.writerow([parent['code'], child['code'], child['jobs'], child['posts'], date_converted])
          
      current_date = add_months(current_date, 1)

parser = argparse.ArgumentParser(description='Fetch data from EURES and save to CSV')
parser.add_argument('--start-date', type=str, help='Start date in YYYY-MM-DD format', required=True)
parser.add_argument('--end-date', type=str, help='End date in YYYY-MM-DD format', required=True)
parser.add_argument('--output-file', default='data/eures_data.csv', type=str, help='Output CSV file path', required=False)
parser.add_argument('--overwrite', default=True, action='store_true', help='Overwrite the file if it exists', required=False)
args = parser.parse_args()

if __name__ == "__main__":
  main(args.start_date, args.end_date, args.output_file, args.overwrite)
