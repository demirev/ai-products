library(tidyverse)
library(ggplot2)
library(argparse)
library(rnaturalearth)
library(sf)
library(showtext)
library(sysfonts)

# define script arguments -------------------------------------------------
parser <- ArgumentParser()

parser$add_argument(
  "--skill_inteligence_data", 
  type = "character",
  help = "Path to data collected from CEDEFOP's Skill Intelligence Report", 
  default = "data/cedefop/skills_intelligence_data.csv"
)
parser$add_argument(
  "--wage_coefficients", 
  type = "character",
  help = "Path to wage premium coefficients collected from the SES 2018 report", 
  default = "data/ses2018/wage_coefficients.csv"
)
parser$add_argument(
  "--scored_occupations", 
  type = "character",
  help = "Path to scored ESCO occupations", 
  default = "results/occupational_exposure_to_ai_products/scored_esco_occupations_matched.csv"
)
parser$add_argument(
  "--output_dir", 
  type = "character",
  help = "Path to output file",
  default = "results"
)

font_add_google("Merriweather", "merriweather")
showtext_auto()

# read data ---------------------------------------------------------------
args <- parser$parse_args()

cedefop_data <- read_csv(args$skill_inteligence_data)
ses_coefficients <- read_csv(args$wage_coefficients)
scored_occupations <- read_csv(args$scored_occupations)

# process data ------------------------------------------------------------
scored_occupations_2digit <- scored_occupations %>%
  mutate(isco_code = substr(isco_group, 1, 2)) %>%
  group_by(isco_code) %>%
  summarise(
    ai_product_exposure_score = mean(ai_product_exposure_score, na.rm = T),
    felten_exposure_score = mean(felten_exposure_score, na.rm = T),
    webb_exposure_score = mean(webb_exposure_score, na.rm = T)
  )

ses_coefficients <- ses_coefficients %>%
  mutate(
    mean_wage_coefficient = rowMeans(select(ses_coefficients, -Occupation), na.rm = T),
    isco_code = substr(Occupation, nchar(Occupation)-1, nchar(Occupation))
  ) %>%
  select(isco_code, mean_wage_coefficient)

cedefop_data_2digit <- cedefop_data %>%
  mutate(
    isco_code = as.character(occupation) 
  ) %>%
  filter(
    nchar(isco_code) > 1
  ) %>%
  mutate(
    isco_code = substr(isco_code, 3, 4),
    indicator = str_replace(indicator, " ", "_")
  ) %>%
  filter(country == "EU") %>%
  select(isco_code, indicator, value) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  select(-relative_income)

exposure_by_country <- cedefop_data %>%
  mutate(
    isco_code = as.character(occupation) 
  ) %>%
  filter(
    nchar(isco_code) > 1
  ) %>%
  mutate(
    isco_code = substr(isco_code, 3, 4),
    indicator = str_replace(indicator, " ", "_")
  ) %>%
  filter(
    indicator == "total_employment" & country != "EU"
  ) %>%
  select(
    country, isco_code, employment=value
  ) %>%
  mutate(
    employment = employment %>%
      str_remove_all(",") %>%
      as.numeric()
  ) %>%
  left_join(
    scored_occupations_2digit %>%
      select(
        isco_code, ai_product_exposure_score, 
        felten_exposure_score, webb_exposure_score
      ), 
    by = "isco_code"
  ) %>%
  group_by(country) %>%
  summarise(
    ai_product_exposure_score = weighted.mean(ai_product_exposure_score, employment),
    felten_exposure_score = weighted.mean(felten_exposure_score, employment),
    webb_exposure_score = weighted.mean(webb_exposure_score, employment)
  ) %>%
  arrange(desc(ai_product_exposure_score)) %>%
  print(n = Inf)

scored_occupations_2digit <- scored_occupations_2digit %>%
  left_join(ses_coefficients, by = "isco_code") %>%
  left_join(cedefop_data_2digit, by = "isco_code") %>%
  mutate(
    total_employment = total_employment %>%
      str_remove_all(",") %>%
      as.numeric(),
    percent_women = percent_women %>%
      str_remove_all("%") %>%
      as.numeric(),
    percent_unemployed = percent_unemployed %>%
      str_remove_all("%") %>%
      as.numeric()
  )

# plot figures ------------------------------------------------------------
vs_wage_plot <- scored_occupations_2digit %>%
  ggplot(aes(x = mean_wage_coefficient, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Mean wage coefficient") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. mean wage coefficient") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_plot_cubic_trend <- scored_occupations_2digit %>%
  ggplot(aes(x = mean_wage_coefficient, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3)) +
  xlab("Mean wage coefficient") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. mean wage coefficient") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

ggplot(scored_occupations_2digit, aes(x = mean_wage_coefficient, y = felten_exposure_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Mean wage coefficient") +
  ylab("Felten exposure score") +
  ggtitle("Felten exposure score vs. mean wage coefficient") +
  theme_minimal()

ggplot(scored_occupations_2digit, aes(x = mean_wage_coefficient, y = webb_exposure_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Mean wage coefficient") +
  ylab("Webb exposure score") +
  ggtitle("Webb exposure score vs. mean wage coefficient")

ggplot(scored_occupations_2digit, aes(x = total_employment, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Total employment") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. total employment")


ggplot(scored_occupations_2digit, aes(x = percent_women, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("% Women in Occupation") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. total employment")


vs_unemployment_plot <- ggplot(scored_occupations_2digit, aes(x = percent_unemployed, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_cubic_trend <- ggplot(scored_occupations_2digit, aes(x = percent_unemployed, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3)) +
  xlab("% Unemployed in Occupation") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

cor.test(
  scored_occupations_2digit$ai_product_exposure_score, 
  scored_occupations_2digit$mean_wage_coefficient
)

cor.test(
  scored_occupations_2digit$ai_product_exposure_score, 
  scored_occupations_2digit$percent_unemployed
)

cor.test(
  scored_occupations_2digit$felten_exposure_score, 
  scored_occupations_2digit$mean_wage_coefficient
)

eu_27 <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
  "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
  "NL", "PL", "PT", "RO", "SE", "SI", "SK"
)
gray_countries <- c(
  "GB", #"IS", 
  "NO", "CH", "LI",
  "ME", "MK", "AL", "RS", "BA", 
  "XK", "MD", "UA", "BY"
)

europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(iso_a2 %in% eu_27 | iso_a2 %in% gray_countries | name_en == "France" | name_en == "Norway") %>%
  mutate(
    iso_a2 = if_else(name_en == "France", "FR", iso_a2),
    iso_a2 = if_else(name_en == "Norway", "NO", iso_a2)
  ) %>%
  left_join(exposure_by_country, by = c("iso_a2" = "country"))

# Plot the map
expsure_map <- ggplot(europe_map) +
  geom_sf(aes(fill = ai_product_exposure_score)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather")) +
  labs(title = "AI Product Exposure Score by Country in Europe",
       fill = "Exposure Score") +
  coord_sf(xlim = c(-10, 40), ylim = c(34, 70))


# save results ------------------------------------------------------------
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_wage_plot.svg"), 
  vs_wage_plot, 
  width = 5, height = 5
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_unemployment_plot.svg"), 
  vs_unemployment_plot, width = 5, height = 5
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_wage_plot_cubic_trend.svg"), 
  vs_wage_plot_cubic_trend, width = 5, height = 5
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_unemployment_plot_cubic_trend.svg"), 
  vs_unemployment_plot_cubic_trend, width = 5, height = 5
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_map.svg"), 
  expsure_map, width = 5, height = 5
)
