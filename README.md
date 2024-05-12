Process:

1. Run `py/scrape_releases.py` - populates folders data/links and data/articles 
2. Run `py/find_relevant_releases.py` - creates results/relevant_press_releases.csv (appends if it exists)
3. Run `py/extract_product_capabilities.py` - creates results/processed_press_releases.csv (appends if it exists). Executes GPT queries.
4. Run `py/match_capabilities_to_skills.py` - creates results/scored_esco_skills.csv (overwrites)
5. Run `py/scrape_cedefop.py` - creates data/skills_intelligence_data.csv (overwrites)
6. Run `R/aggregate_skills_to_occupations.R` - creates results/scored_esco_occuations.csv
7. Run `R/match_to_prior_work.R` - creates results/scored_esco_occupations_matched.csv
8. (Optional) Run `R/clusterize_capability_vectors.R`
9. Run `R/join_to_occupation_statistics.R` - creates results/scored_esco_occupations_matched.csv