import requests
import os
import argparse
from datetime import datetime, timedelta
from bs4 import BeautifulSoup

all_categories = [
  # business and money
  #"travel-latest-news/travel-latest-news-list/",
  "automotive-transportation",
  "business-technology",
  "entertainment-media",
  "financial-services",
  "general-business",
  # science and tech
  "consumer-technology",
  "energy",
  "environment",
  "heavy-industry-manufacturing",
  "telecommunications",
  # lifestyle and health
  "consumer-products-retail",
  "health",
  "sports",
  "travel"
]

class ScrapingError(Exception):
  pass

def scrape_all_articles(
  categories = all_categories,
  input_dir = "../data/links",
  output_parent_dir = "../data/articles"
):
  for category in categories:
    # list all files in input_dir/category
    links = []
    file_names = os.listdir(f"{input_dir}/{category}")
    for file_name in file_names:
      file_full_name = f"{input_dir}/{category}/{file_name}"
      with open(file_full_name, "r") as file:
        links = links + file.readlines()
    links = list(set(links)) # remove duplicates
    links = [link.strip() for link in links]
    print(f"Scraping {len(links)} articles for {category}")

    output_dir = f"{output_parent_dir}/{category}"
    os.makedirs(output_dir, exist_ok=True)
    existing_links = [link.split("/")[-1].split(".")[0] for link in os.listdir(output_dir)]
    links = [link for link in links if link.split("/")[-1].split(".")[0] not in existing_links]
    scrape_and_save_articles(links, output_dir)
  return True

def scrape_and_save_articles(
  article_links,
  output_dir = "../data/articles"  
):
  for article_link in article_links:
    print(f"Scraping {article_link}")
    article = scrape_article(article_link, prefix = "https://www.prnewswire.com")
    # remove .html from article_link
    file_name = f"{output_dir}/{article_link.split('/')[-1].split('.')[0]}.json"
    with open(file_name, "w") as file:
      file.write(str(article))
  return True

def scrape_article(link, prefix = "https://www.prnewswire.com"):
  if prefix is not None:
    link = prefix + link

  response = requests.get(link)

  if response.status_code == 200:
    soup = BeautifulSoup(response.content, "html.parser")
    header = soup.select_one(".detail-headline h1")
    body = soup.select_one(".release-body")
    uploader = soup.select_one(".meta + a strong")
    uploader_link = soup.select_one(".meta + a")
    date = soup.select_one(".mb-no")
    
    if body:
      scraped = {}
      if header:
        scraped["header"] = header.text
      if uploader:
        scraped["uploader"] = uploader.text
      if uploader_link:
        scraped["uploader_link"] = uploader_link["href"]
      if date:
        scraped["date"] = date.text

      scraped["body"] = body.text
      return scraped
    else:
      raise ScrapingError("Unable to find the body of the article.")
  else:
    raise ScrapingError(f"Unable to access the link. Status code: {response.status_code}")

def scrape_all_links(
  categories = all_categories,
  output_dir = "../data/links",
  start_date = "2023-08-09", # up to six months ago
  end_date = "2024-02-05"
):
  for category in categories:
    dir_path = f"{output_dir}/{category}"
    os.makedirs(dir_path, exist_ok=True)
    links = scrape_category_links_period(
      category=category, 
      start_date=start_date, 
      end_date=end_date,
      output_dir=dir_path
    )
    file_name = f"{dir_path}/links_{start_date}_{end_date}.txt"
    with open(file_name, "w") as file:
      for link in links:
        file.write(link + "\n")   

  return True

def scrape_category_links_period(
  category,
  base_url = "https://www.prnewswire.com/news-releases",
  start_date = "2024-02-05",
  end_date = "2024-02-05",
  output_dir = None
):
  hrefs = []
  start_date = datetime.strptime(start_date, "%Y-%m-%d")
  end_date = datetime.strptime(end_date, "%Y-%m-%d")
  this_date = start_date
  while this_date <= end_date:
    date_str = this_date.strftime("%Y-%m-%d")
    # try to scrape links for this date, if not possible, skip
    try:
      new_hrefs = scrape_category_links_day(
        category=category, 
        base_url=base_url, 
        date=date_str,
        output_dir=output_dir
      )
    except ScrapingError as e:
      print(f"Failed to scrape links for {date_str}: {e}")
      this_date += timedelta(days=1)
      continue

    hrefs = hrefs + new_hrefs
    hrefs = list(set(hrefs))
    this_date += timedelta(days=1)
  return hrefs

def scrape_category_links_day(
  category, 
  base_url = "https://www.prnewswire.com/news-releases", 
  date = "2024-02-05",
  n_pages = 15,
  output_dir = None
):
  hrefs = []
  category_url = f"{base_url}/{category}-latest-news/{category}-latest-news-list/"
  date = date.split("-")
  request_params = f"?month={date[1]}&day={date[2]}&year={date[0]}&hour=00"
  url = category_url + request_params

  for page_no in range(1, n_pages):
    page_url = url + f"&page={page_no}&pagesize=100"
    print(f"Getting all links for {page_url}")
    hrefs = hrefs + scrape_links_on_page(page_url)
  
  hrefs = list(set(hrefs)) # remove duplicates

  if output_dir is not None:
    os.makedirs(output_dir, exist_ok=True)
    file_name = f"{output_dir}/links_{date}.txt"
    with open(file_name, "w") as file:
      for link in hrefs:
        file.write(link + "\n")
  
  return hrefs

def scrape_links_on_page(url, selector=".newsreleaseconsolidatelink"):
  hrefs = []
  response = requests.get(url)
  
  if response.status_code == 200:
    soup = BeautifulSoup(response.content, "html.parser")
    links = soup.select(selector)
    for link in links:
      hrefs.append(link["href"])
  else:
    raise ScrapingError("Failed to scrape the URL")

  return hrefs

parser = argparse.ArgumentParser(description="A script to scrape press releases")
parser.add_argument("--no-links", action="store_true", help="skip scraping links")
parser.add_argument("--no-articles", action="store_true", help="skip scraping articles")
parser.add_argument("--start-date", type=str, help="the start date for scraping links in YYYY-MM-DD format")
parser.add_argument("--end-date", type=str, help="the end date for scraping links in YYYY-MM-DD format")
parser.add_argument("--categories", type=str, help="comma separated list of categories")

args = parser.parse_args()

if __name__ == "__main__":
  print("Starting")
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

  if args.start_date and args.end_date:
    start_date = args.start_date
    end_date = args.end_date
  else:
    end_date = datetime.today().strftime("%Y-%m-%d")
    start_date = (datetime.today() - timedelta(180)).strftime("%Y-%m-%d")

  if args.categories:
    categories = args.categories.split(",")
  else:
    categories = all_categories

  if not args.no_links:
    scrape_all_links(
      categories=categories,
      output_dir="data/links",
      start_date=start_date, 
      end_date=end_date
    )
  
  if not args.no_articles:
    scrape_all_articles(
      categories=categories, 
      input_dir="data/links", 
      output_parent_dir="data/articles"
    )
  
  print(datetime.now().strftime("%Y-%m-%d %H:%M:%S"))  
  print("Done")
