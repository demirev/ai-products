Process:

# Occupational Exposure to Disruption from AI Products

This project aims to derive a measure of the degree to which each occupation is exposed to disruption by Artificial Intelligence. To do so, I use public press release data to identify new AI product launches or adoptions. Next, I use various NLP techniques and a Large Language Model (GPT-4) to extract the concrete capabilities of each AI product. I compare these capabilities to a list of job relevant skills compiled by ESCO, and calculate a occupation-level exposure score by avaraging over the job skill - AI capability similarity of all job skills related to each occupation.

The main results are in `results/occupational_exposure_to_ai_products/`. The file `scored_esco_occupations.csv` contains occupational exposure scores for all individual occupations in the ESCO database. I also provide results aggregated to the ESCO 4-digit, 3-digit, 2-digit, or 1-digit level (which are compatible with the corresponding levels of the ISCO classification). Finally, the file `scored_soc_equivalent_occupations.csv` contains occupational exposure scores for occupations in O\*NET, obtained by converting the ESCO scores using the ESCO-O\*NET crosswalk provided by ESCO.

The plot below summarizes the main findings, which put knowledge-heavy and highly-skilled occupation at the top in terms of their exposure to AI, while occupation requiring physical labor are towards the bottom.

![Occupational Exposure to AI-product Disruption](results/plots/occupation_scores_level_2_plot.svg)

Occupation with high exposure scores tend to be highly paid and with low unemployment rates, suggesting that AI product innovation may be targeted towards tasks for which the required labor is relatively scarce and expensive.

![AI Exposure vs Wage Premium](results/plots/exposure_vs_wage_plot.svg)

![AI Exposure vs Unemployment](results/plots/exposure_vs_unemployment_plot.svg)

The full details about the method and the results, as well as connections to existing literature, as described in the file `main_text.pdf`. Please keep in mind, that this project is a work in progress, and it may undergo slight or even significant changes prior to publication.

## Reproducability

1. Run `py/scrape_releases.py` - populates folders data/links and data/articles 
2. Run `py/find_relevant_releases.py` - creates results/relevant_press_releases.csv (appends if it exists)
3. Run `py/extract_product_capabilities.py` - creates results/processed_press_releases.csv (appends if it exists). Executes GPT queries.
4. Run `py/match_capabilities_to_skills.py` - creates results/scored_esco_skills.csv (overwrites)
5. Run `py/scrape_cedefop.py` - creates data/skills_intelligence_data.csv (overwrites)
6. Run `R/aggregate_skills_to_occupations.R` - creates results/scored_esco_occuations.csv
7. Run `R/match_to_prior_work.R` - creates results/scored_esco_occupations_matched.csv
8. (Optional) Run `R/clusterize_capability_vectors.R`
9. Run `R/join_to_occupation_statistics.R` - creates results/scored_esco_occupations_matched.csv