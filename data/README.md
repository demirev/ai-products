# Data

Most data files in this directory are gitignored because they're large, scraped, or freely re-downloadable from their original sources. The small reference datasets (third-party crosswalks and replication appendices) are tracked so the analysis is reproducible end-to-end.

## Tracked

| Path | Source |
| --- | --- |
| `soc_crosswalk/` | BLS SOC 2010 → 2018 crosswalk (https://www.bls.gov/soc/2018/crosswalks.htm) |
| `felten_et_al/appendix_A.csv` | Felten, Raj, Seamans (2021), AI Occupational Exposure, Appendix A |
| `webb/` | Webb (2020), "The Impact of Artificial Intelligence on the Labor Market" replication files |
| `ses2018/wage_coefficients.csv` | Eurostat Structure of Earnings Survey 2018, derived wage coefficients |
| `cedefop/` | Cedefop Skills Intelligence export (see `py/scrape_cedefop.py`) |

## Ignored — how to obtain

| Path | Source / how to regenerate |
| --- | --- |
| `articles/` | Press releases scraped via `py/scrape_releases.py` (per-industry subdirs) |
| `links/` | Press release URL lists collected by the same scraper |
| `esco/` | ESCO v1.1.1 classification, https://esco.ec.europa.eu/en/use-esco/download |
| `eloundou_et_al/` | Replication data from Eloundou et al. (2023), "GPTs are GPTs", https://github.com/openai/GPTs-are-GPTs |
| `eures_data.csv` | EURES job listings collected via `py/collect_job_listings_eures.py` |
