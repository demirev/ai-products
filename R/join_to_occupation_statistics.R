library(tidyverse)
library(ggplot2)
library(argparse)
library(rnaturalearth)
library(sf)
library(showtext)
library(sysfonts)
library(scales)

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
  mutate(
    felten_exposure_percentile = ecdf(felten_exposure_score)(felten_exposure_score),
    webb_exposure_percentile = ecdf(webb_exposure_score)(webb_exposure_score),
    eloundou_exposure_percentile = ecdf(beta_eloundou)(beta_eloundou),
    ai_product_automation_percentile = ecdf(ai_product_automation_score)(ai_product_automation_score),
    ai_product_augmentation_percentile = ecdf(ai_product_augmentation_score)(ai_product_augmentation_score)
  ) %>%
  mutate(isco_code = substr(isco_group, 1, 2)) %>%
  group_by(isco_code) %>%
  summarise(
    ai_product_exposure_score = mean(ai_product_exposure_score, na.rm = T),
    ai_product_augmentation_score = mean(ai_product_augmentation_score, na.rm = T),
    ai_product_automation_score = mean(ai_product_automation_score, na.rm = T),
    ai_product_third_party_score = mean(ai_product_third_party_score, na.rm = T),
    felten_exposure_score = mean(felten_exposure_score, na.rm = T),
    webb_exposure_score = mean(webb_exposure_score, na.rm = T),
    eloundou_exposure_score = mean(beta_eloundou, na.rm = T),
    ai_product_exposure_percentile = mean(ai_product_exposure_percentile, na.rm = T),
    ai_product_automation_percentile = mean(ai_product_automation_percentile, na.rm = T),
    ai_product_augmentation_percentile = mean(ai_product_augmentation_percentile, na.rm = T),
    felten_exposure_percentile = mean(felten_exposure_percentile, na.rm = T),
    webb_exposure_percentile = mean(webb_exposure_percentile, na.rm = T),
    eloundou_exposure_percentile = mean(eloundou_exposure_percentile, na.rm = T)
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
  ggtitle("AI product exposure score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_plot_automation <- scored_occupations_2digit %>%
  ggplot(aes(x = mean_wage_coefficient, y = ai_product_automation_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Mean wage coefficient") +
  ylab("AI product automation score") +
  ggtitle("AI product automation score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_plot_augmentation <- scored_occupations_2digit %>%
  ggplot(aes(x = mean_wage_coefficient, y = ai_product_augmentation_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Mean wage coefficient") +
  ylab("AI product augmentation score") +
  ggtitle("AI product augmentation score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_plot_cubic_trend <- scored_occupations_2digit %>%
  ggplot(aes(x = mean_wage_coefficient, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3)) +
  xlab("Mean wage coefficient") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_felten_plot <- scored_occupations_2digit %>%
  ggplot(aes(x = mean_wage_coefficient, y = felten_exposure_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Mean wage coefficient") +
  ylab("Felten exposure score") +
  ggtitle("Felten exposure score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_webb_plot <- scored_occupations_2digit %>% 
  ggplot(aes(x = mean_wage_coefficient, y = webb_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Mean wage coefficient") +
  ylab("Webb exposure score") +
  ggtitle("Webb exposure score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_wage_eloundou_plot <- scored_occupations_2digit %>% 
  ggplot(aes(x = mean_wage_coefficient, y = eloundou_exposure_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  xlab("Mean wage coefficient") +
  ylab("Eloundou exposure score") +
  ggtitle("Eloundou exposure score vs. wage premium") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_employment_plot <- scored_occupations_2digit %>%
  ggplot(aes(x = total_employment, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::comma) + 
  xlab("Total employment") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. total employment") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_perc_women_plot <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_women, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Women in Occupation") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. % Women") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_automation <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = ai_product_automation_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("AI product automation score") +
  ggtitle("AI product automation score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_augmentation <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = ai_product_augmentation_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("AI product augmentation score") +
  ggtitle("AI product augmentation score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_cubic_trend <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = ai_product_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3)) +
  xlab("% Unemployed in Occupation") +
  ylab("AI product exposure score") +
  ggtitle("AI product exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_felten <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = felten_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("Felten exposure score") +
  ggtitle("Felten exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_webb <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = webb_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("Webb exposure score") +
  ggtitle("Webb exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_unemployment_plot_eloundou <- scored_occupations_2digit %>%
  ggplot(aes(x = percent_unemployed, y = eloundou_exposure_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("% Unemployed in Occupation") +
  ylab("Eloundou exposure score") +
  ggtitle("Eloundou exposure score vs. unemployment rate") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

automation_augmentation_wages_plot <- scored_occupations_2digit %>% 
  filter(!is.na(mean_wage_coefficient)) %>% 
  mutate(wage_perc = ecdf(mean_wage_coefficient)(mean_wage_coefficient)) %>% 
  mutate(wage_perc = cut(wage_perc, seq(0,1,0.1))) %>% 
  #mutate(wage_perc = cut(wage_perc, c(0,0.2,0.4,0.6,0.7,0.8,0.9,1))) %>% 
  group_by(wage_perc) %>% 
  summarise(
    #ai_product_exposure_score = mean(ai_product_exposure_score), 
    augmentation = mean(ai_product_augmentation_score), 
    automation = mean(ai_product_automation_score), 
    n = n()
  ) %>% print(n = Inf) %>% 
  pivot_longer(
    cols = c(automation, augmentation),
    names_to = "score"
  ) %>%  
  ggplot(aes(x = wage_perc, y = value, fill = score)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  #coord_flip() + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom") +
  labs(
    y = "% exposed skills",
    x = "wage percentile",
    title = "Wage vs AI Exposure by Product Intent"
  )

scored_occupations_2digit %>% 
  filter(!is.na(percent_unemployed)) %>% 
  mutate(wage_perc = ecdf(percent_unemployed)(percent_unemployed)) %>% 
  mutate(wage_perc = cut(wage_perc, seq(0,1,0.1))) %>% 
  #mutate(wage_perc = cut(wage_perc, c(0,0.2,0.4,0.6,0.7,0.8,0.9,1))) %>% 
  group_by(wage_perc) %>% 
  summarise(
    ai_product_exposure_score = mean(ai_product_exposure_score), 
    ai_product_augmentation_score = mean(ai_product_augmentation_score), 
    ai_product_automation_score = mean(ai_product_automation_score), 
    n = n()
  ) %>% print(n = Inf) %>% 
  pivot_longer(
    cols = c(ai_product_automation_score, ai_product_augmentation_score)
  ) %>%  
  ggplot(aes(x = wage_perc, y = value, fill = name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  #coord_flip() + 
  theme_minimal() +
  theme(legend.position = "bottom")

# compute correlations ----------------------------------------------------
list(
  r_product_exposure_wages = cor.test(
    scored_occupations_2digit$ai_product_exposure_score, 
    scored_occupations_2digit$mean_wage_coefficient,
    method = "spearman"
  ),
  r_product_automation_wages = cor.test(
    scored_occupations_2digit$ai_product_automation_score, 
    scored_occupations_2digit$mean_wage_coefficient,
    method = "spearman"
  ),
  r_product_augmentation_wages = cor.test(
    scored_occupations_2digit$ai_product_augmentation_score, 
    scored_occupations_2digit$mean_wage_coefficient,
    method = "spearman"
  ),
  r_felten_wages = cor.test(
    scored_occupations_2digit$felten_exposure_score, 
    scored_occupations_2digit$mean_wage_coefficient,
    method = "spearman"
  ),
  r_webb_wages = cor.test(
    scored_occupations_2digit$webb_exposure_score, 
    scored_occupations_2digit$mean_wage_coefficient,
    method = "spearman"
  ),
  r_eloundou_wages = cor.test(
    scored_occupations_2digit$eloundou_exposure_score, 
    scored_occupations_2digit$mean_wage_coefficient,
    method = "spearman"
  )
)

# list(
#   r_product_exposure_wages = cor.test(
#     scored_occupations_2digit$ai_product_exposure_percentile, 
#     scored_occupations_2digit$mean_wage_coefficient,
#     method = "spearman"
#   ),
#   r_product_automation_wages = cor.test(
#     scored_occupations_2digit$ai_product_automation_percentile, 
#     scored_occupations_2digit$mean_wage_coefficient,
#     method = "spearman"
#   ),
#   r_product_augmentation_wages = cor.test(
#     scored_occupations_2digit$ai_product_augmentation_percentile, 
#     scored_occupations_2digit$mean_wage_coefficient,
#     method = "spearman"
#   ),
#   r_felten_wages = cor.test(
#     scored_occupations_2digit$felten_exposure_percentile, 
#     scored_occupations_2digit$mean_wage_coefficient,
#     method = "spearman"
#   ),
#   r_webb_wages = cor.test(
#     scored_occupations_2digit$webb_exposure_percentile, 
#     scored_occupations_2digit$mean_wage_coefficient,
#     method = "spearman"
#   ),
#   r_eloundou_wages = cor.test(
#     scored_occupations_2digit$eloundou_exposure_percentile, 
#     scored_occupations_2digit$mean_wage_coefficient,
#     method = "spearman"
#   )
# )

list(
  r_product_exposure_unemployment = cor.test(
    scored_occupations_2digit$ai_product_exposure_score, 
    scored_occupations_2digit$percent_unemployed,
    method = "spearman"
  ),
  r_product_automation_unemployment = cor.test(
    scored_occupations_2digit$ai_product_automation_score, 
    scored_occupations_2digit$percent_unemployed,
    method = "spearman"
  ),
  r_product_augmentation_unemployment = cor.test(
    scored_occupations_2digit$ai_product_augmentation_score, 
    scored_occupations_2digit$percent_unemployed,
    method = "spearman"
  ),
  r_felten_unemployment = cor.test(
    scored_occupations_2digit$felten_exposure_score, 
    scored_occupations_2digit$percent_unemployed,
    method = "spearman"
  ),
  r_webb_unemployment = cor.test(
    scored_occupations_2digit$webb_exposure_score, 
    scored_occupations_2digit$percent_unemployed,
    method = "spearman"
  ),
  r_eloundou_unemployment = cor.test(
    scored_occupations_2digit$eloundou_exposure_score, 
    scored_occupations_2digit$percent_unemployed,
    method = "spearman"
  )
)

# list(
#   r_product_exposure_unemployment = cor.test(
#     scored_occupations_2digit$ai_product_exposure_percentile, 
#     scored_occupations_2digit$percent_unemployed,
#     method = "spearman"
#   ),
#   r_product_automation_unemployment = cor.test(
#     scored_occupations_2digit$ai_product_automation_percentile, 
#     scored_occupations_2digit$percent_unemployed,
#     method = "spearman"
#   ),
#   r_product_augmentation_unemployment = cor.test(
#     scored_occupations_2digit$ai_product_augmentation_percentile, 
#     scored_occupations_2digit$percent_unemployed,
#     method = "spearman"
#   ),
#   r_felten_unemployment = cor.test(
#     scored_occupations_2digit$felten_exposure_percentile, 
#     scored_occupations_2digit$percent_unemployed,
#     method = "spearman"
#   ),
#   r_webb_unemployment = cor.test(
#     scored_occupations_2digit$webb_exposure_percentile, 
#     scored_occupations_2digit$percent_unemployed,
#     method = "spearman"
#   ),
#   r_eloundou_unemployment = cor.test(
#     scored_occupations_2digit$eloundou_exposure_percentile, 
#     scored_occupations_2digit$percent_unemployed,
#     method = "spearman"
#   )
# ) # percentile and score have slightly different values due to the aggregation to the 2-digit level

# same for employment
list(
  r_product_exposure_employment = cor.test(
    scored_occupations_2digit$ai_product_exposure_score, 
    scored_occupations_2digit$total_employment,
    method = "spearman"
  ),
  r_product_automation_unemployment = cor.test(
    scored_occupations_2digit$ai_product_automation_score, 
    scored_occupations_2digit$total_employment,
    method = "spearman"
  ),
  r_product_augmentation_unemployment = cor.test(
    scored_occupations_2digit$ai_product_augmentation_score, 
    scored_occupations_2digit$total_employment,
    method = "spearman"
  ),
  r_felten_employment = cor.test(
    scored_occupations_2digit$felten_exposure_score, 
    scored_occupations_2digit$total_employment,
    method = "spearman"
  ),
  r_webb_employment = cor.test(
    scored_occupations_2digit$webb_exposure_score, 
    scored_occupations_2digit$total_employment,
    method = "spearman"
  ),
  r_eloundou_employment = cor.test(
    scored_occupations_2digit$eloundou_exposure_score, 
    scored_occupations_2digit$total_employment,
    method = "spearman"
  )
)

# same for percent women
list(
  r_product_exposure_women = cor.test(
    scored_occupations_2digit$ai_product_exposure_score, 
    scored_occupations_2digit$percent_women,
    method = "spearman"
  ),
  r_product_automation_unemployment = cor.test(
    scored_occupations_2digit$ai_product_automation_score, 
    scored_occupations_2digit$percent_women,
    method = "spearman"
  ),
  r_product_augmentation_unemployment = cor.test(
    scored_occupations_2digit$ai_product_augmentation_score, 
    scored_occupations_2digit$percent_women,
    method = "spearman"
  ),
  r_felten_women = cor.test(
    scored_occupations_2digit$felten_exposure_score, 
    scored_occupations_2digit$percent_women,
    method = "spearman"
  ),
  r_webb_women = cor.test(
    scored_occupations_2digit$webb_exposure_score, 
    scored_occupations_2digit$percent_women,
    method = "spearman"
  ),
  r_eloundou_women = cor.test(
    scored_occupations_2digit$eloundou_exposure_score, 
    scored_occupations_2digit$percent_women,
    method = "spearman"
  )
)

# make map ----------------------------------------------------------------
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
  labs(title = "AI Product Exposure Score by Country",
       fill = "Exposure Score") +
  # remove axis labels
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(-10, 40), ylim = c(34, 70))


# save results ------------------------------------------------------------
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_wage_plot.eps"), 
  vs_wage_plot, 
  width = 5, height = 5,
  device = cairo_ps
)
ggsave(filename = 
  file.path(args$output_dir, "plots", "exposure_vs_unemployment_plot.eps"), 
  vs_unemployment_plot, 
  width = 5, height = 5,
  device = cairo_ps
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_wage_plot_cubic_trend.eps"), 
  vs_wage_plot_cubic_trend, width = 5, height = 5,
  device = cairo_ps
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_unemployment_plot_cubic_trend.eps"), 
  vs_unemployment_plot_cubic_trend, width = 5, height = 5,
  device = cairo_ps
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_employment_plot.eps"), 
  vs_employment_plot, width = 5, height = 5,
  device = cairo_ps
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_vs_percent_women_plot.eps"), 
  vs_perc_women_plot, width = 5, height = 5,
  device = cairo_ps
)
ggsave(
  file.path(args$output_dir, "plots", "exposure_map.eps"), 
  expsure_map, width = 5, height = 5,
  device = cairo_ps
)

ggsave(
  file.path(args$output_dir, "plots", "automation_augmentation_wages_plot.eps"), 
  automation_augmentation_wages_plot, width = 6, height = 4,
  device = cairo_ps
)

