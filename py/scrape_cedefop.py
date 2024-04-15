import os
import argparse
import requests
from find_relevant_releases import read_list_from_csv, save_list_to_csv
from datetime import datetime
import time

eu_countries = [
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
  "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
  "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"
]

occupations = {
  "3" : "Associate professionals",
  "3.32" : "Health associate professionals",
  "3.35" : "ICT technicians",
  "3.34" : "Legal & social associate professionals",
  "3.33" : "Office associate professionals",
  "3.31" : "Science & engineering technicians",
  "4" : "Clerks",
  "4.43" : "Accounting clerks",
  "4.42" : "Customer clerks",
  "4.41" : "Office clerks",
  "4.44" : "Other support clerks",
  "9" : "Elementary workers",
  "9.92" : "Agricultural labourers",
  "9.91" : "Cleaners and helpers",
  "9.94" : "Food preparation helpers",
  "9.96" : "Other elementary workers",
  "9.95" : "Street services workers",
  "9.93" : "Technical labourers",
  "6" : "Farm and related workers",
  "6.61" : "Farmworkers and gardeners",
  "6.62" : "Forest & fishery workers",
  "1" : "Managers",
  "1.12" : "Business managers",
  "1.11" : "CEOs, officials & legislators",
  "1.14" : "Hospitality & retail managers",
  "1.13" : "Technical managers",
  "8" : "Operators and assemblers",
  "8.82" : "Assemblers",
  "8.83" : "Drivers & vehicle operators",
  "8.81" : "Machine & plant operators",
  "2" : "Professionals",
  "2.22" : "Health professionals",
  "2.25" : "ICT professionals",
  "2.26" : "Legal & social professionals",
  "2.24" : "Office professionals",
  "2.21" : "Researchers & engineers",
  "2.23" : "Teaching professionals",
  "5" : "Service and sales workers",
  "5.53" : "Care workers",
  "5.51" : "Personal service workers",
  "5.54" : "Protection workers",
  "5.52" : "Sales workers",
  "7" : "Trades workers",
  "7.71" : "Construction workers",
  "7.74" : "Electroengineering workers",
  "7.73" : "Handicraft & printing workers",
  "7.72" : "Metal & machinery workers",
  "7.75" : "Other manufacturing workers"
}


def fetch_cedefop_data(indicator_name, country, occupation):
  base_url = 'https://www.cedefop.europa.eu/dsense/data/keyfact'
  
  # set GET params based on indicator
  if indicator_name == 'total employment':
    params = {
      'indicator': '900',
      'unit': 'number',
      'dimensionsCombination': 'country X year X occupation',
      'aes_x': 'year',
      'aes_z': 'country',
      'country': country,
      'year': 'recent',
      'occupation': occupation
    }
  elif indicator_name == 'percent women':
    params = {
      'indicator': '900',
      'unit': 'percentage',
      'dimensionsCombination': 'country X year X gender X occupation',
      'aes_x': 'year',
      'aes_z': 'country',
      'country': country,
      'year': 'recent',
      'gender': 'F',
      'occupation': occupation
    }
  elif indicator_name == 'percent unemployed':
    params = {
      'indicator': '820',
      'unit': 'percentage',
      'dimensionsCombination': 'country X year X occupation',
      'aes_x': 'year',
      'aes_z': 'country',
      'country': country,
      'year': 'recent',
      'occupation': occupation
    }
  elif indicator_name == 'relative income':
    params = {
      'indicator': '515',
      'unit': 'percentage',
      'dimensionsCombination': 'country X year X occupation X workingstatus',
      'aes_x': 'year',
      'aes_z': 'country',
      #'country': country,
      'year': 'recent',
      'occupation': occupation,
      'workingstatus': 'q2'
    }
  else:
    return {'error': 'Invalid indicator name.'}
  
  # pause for 0.2 seconds to avoid overloading the server
  time.sleep(0.2)
  response = requests.get(base_url, params=params)
  
  if response.status_code == 200:
    data = response.json()
    data_array = data['response']['data'][0] if data['response']['data'] else None
    # If data is found, create the result object
    if data_array:
      result = {
        'indicator': indicator_name,
        'country': country,
        'occupation': occupation,
        'year': data_array[0],
        'value': data_array[2]
      }
      return result
    else:
      print(f"No data found for {indicator_name}, {country}, {occupation}")
      return {'error': 'No data found in response.'}
  else:
    print(f"Error fetching data for {indicator_name}, {country}, {occupation}")
    return {'error': 'Error fetching data.'}


def fetch_all_cedefop_data_for_occupation(occupation):
  print(f"Fetching data for occupation: {occupation}")
  indicator_names = ['total employment', 'percent women', 'percent unemployed', 'relative income']
  results = []
  # run all 4 indicators for the EU level
  for indicator in indicator_names:
    results.append(fetch_cedefop_data(indicator, 'EU', occupation))

  # run total employment and % women by country
  for country in eu_countries:
    print(f"Fetching data for country: {country}, occupation: {occupation}")
    results.append(fetch_cedefop_data('total employment', country, occupation))
    results.append(fetch_cedefop_data('percent women', country, occupation))
  
  return results


def fetch_all_cedefop_data():
  results = []
  for occupation_code in occupations:
    results += fetch_all_cedefop_data_for_occupation(occupation_code)
  return results

# Example usage
# print(fetch_cedefop_data('total employment', 'EU', '3.31'))
# print(fetch_cedefop_data('percent women', 'EU', '3.31'))
# print(fetch_cedefop_data('percent unemployed', 'EU', '3.31'))
# print(fetch_cedefop_data('relative income', 'EU', '3.31'))

if __name__ == "__main__":

  parser = argparse.ArgumentParser(
    description="A script to fetch Cedefop data for occupations"
  )
  parser.add_argument(
    "--file-name", type=str, help="name of file to save results",
    default="data/cedefop/skills_intelligence_data.csv"
  )

  args = parser.parse_args()

  print("Starting")
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

  cedefop_data = fetch_all_cedefop_data()

  cedefop_data_noerror = [d for d in cedefop_data if 'error' not in d]

  save_list_to_csv(cedefop_data_noerror, args.file_name, append=False)

  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
  print("Done")
  print(f"Results saved to {args.file_name}")