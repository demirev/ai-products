library(tidyverse)
library(argparse)
library(showtext)
library(sysfonts)
library(jsonlite)

# define script arguments -------------------------------------------------
parser <- ArgumentParser()

parser$add_argument(
  "--scored_releases",
  type = "character",
  help = "Path to classified releases file",
  default = "results/press_releases/processed_press_releases.csv"
)
parser$add_argument(
  "--scored_skills", 
  type = "character",
  help = "Path to scored skills file", 
  default = "results/scored_esco_skills/scored_esco_skills.csv"
)
parser$add_argument(
  "--scored_skill_groups", 
  type = "character",
  help = "Path to scored skills file", 
  default = "results/occupational_exposure_to_ai_products/scored_esco_skills_all_groups.csv"
)
parser$add_argument(
  "--scored_occupations", 
  type = "character",
  help = "Path to scored ESCO occupations", 
  default = "results/occupational_exposure_to_ai_products/scored_esco_occupations_matched.csv"
)
parser$add_argument(
  "--esco_research_skills", 
  type = "character",
  help = "Path to esco research skills file",
  default = "data/esco/researchSkillsCollection_en.csv"
)
# parser$add_argument(
#   "--esco_occupations", 
#   type = "character",
#   help = "Path to esco occupations file",
#   default = "data/esco/occupations_en.csv"
# )
# parser$add_argument(
#   "--esco_skills", 
#   type = "character",
#   help = "Path to esco skills file",
#   default = "data/esco/skills_en.csv"
# )
# parser$add_argument(
#   "--esco_occupation_skill_mapping", 
#   type = "character",
#   help = "Path to esco occupation skill mapping file",
#   default = "data/esco/occupationSkillRelations_en.csv"
# )
# parser$add_argument(
#   "--esco_research_skills", 
#   type = "character",
#   help = "Path to esco research skills file",
#   default = "data/esco/researchSkillsCollection_en.csv"
# )
# parser$add_argument(
#   "--esco_skill_groups",
#   type = "character",
#   help = "Path to esco skill groups file",
#   default = "data/esco/skillGroups_en.csv"
# )
# parser$add_argument(
#   "--esco_skill_hierarchy",
#   type = "character",
#   help = "Path to esco skill to skill group relations file",
#   default = "data/esco/skillsHierarchy_en.csv"
# )
# parser$add_argument(
#   "--esco_skill_relations",
#   type = "character",
#   help = "Path to esco occupation to occupation group relations file",
#   default = "data/esco/broaderRelationsSkillPillar_en.csv"
# )
parser$add_argument(
  "--isco_groups",
  type = "character",
  help = "Path to isco groups file",
  default = "data/esco/ISCOGroups_en.csv"
)
parser$add_argument(
  "--output_dir", 
  type = "character",
  help = "Path to output file",
  default = "results"
)

font_add_google("Merriweather", "merriweather")
showtext_auto()


# helpers -----------------------------------------------------------------


# read data ---------------------------------------------------------------
args <- parser$parse_args()

scored_releases <- read_csv(args$scored_releases)
scored_skills <- read_csv(args$scored_skills)
scored_occupations <- read_csv(args$scored_occupations)
scored_skill_groups <- read_csv(args$scored_skill_groups)
esco_research_skills <- read_csv(args$esco_research_skills)
isco_groups <- read_csv(args$isco_groups)

# release breakdown -------------------------------------------------------
scored_releases <- scored_releases %>%
  mutate(
    n_skills = str_count(capability_string, ",") + 1,
    n_skills = ifelse(
      str_detect(capability_string, "[a-zA-Z]"), # to exclude e.g. [],
      n_skills,
      0
    )
  )

relevant_releases <- scored_releases %>%
  filter(
    document_type != "Not Relevant"
  ) 

relevant_releases %>% pull(header) %>% sample(5)
#  "Humane Ai Pin Now Available for Purchasen" , "Adobe Podcast Taps Wistia As B2B SaaS Software Partner to Launch its AI-Powered Enhance Speech APInttttttnttttttnttttttnttttt" 

quantile(relevant_releases$n_skills, seq(0,1,0.01))
mean(relevant_releases$n_skills)
sum(relevant_releases$n_skills) # 27,940 (27,414 without the parsing failures)

sample(relevant_releases$capability_string, 5)
# "analyze labor market data", "summarize documents", "translate languages in real time"
# "create interactive forecasts", "plan customized travel itineraries"
# "create voiceovers", "identify security vulnerabilities in code"
# "generate lesson plan recommendations"

relevant_releases %>%
  group_by(
    document_type
    #, intent_category
  ) %>%
  summarise(
    n_releases = n()
    , p_releases = n_releases / nrow(.)
    , n_skills = mean(n_skills)
  )

relevant_releases %>%
  #ffilter(intent_category != "Not Relevant") %>%
  group_by(intent_type) %>%
  summarise(
    n_releases = n()
    , p_releases = n_releases / nrow(.)
    , n_skills = mean(n_skills)
  )

all_capabilities <- relevant_releases %>%
  mutate(
    capabilities = map(
      capability_string,
      function(capstr) {
        tryCatch({
          fromJSON(str_replace_all(capstr, "'", '"'))
        }, error = function(e) c("")[0])
      }
    )
  ) %>%
  pull(capabilities) %>%
  unlist()

table(all_capabilities) %>%
  sort(decreasing = TRUE) %>%
  as_tibble() %>%
  filter(n > 1) %>%
  print(n = 20) # 26,524 unique, 1,337 duplicated

# scored skills breakdown -------------------------------------------------
## means ------------------------------------------------------------------
mean(scored_skills$mean_similarity)

quantile(scored_skills$n_similar_l3, seq(0,1,0.01))
sum(scored_skills$n_similar_l3)
sum(scored_skills$n_similar_l3 >= 10) # 115
sum(scored_skills$n_similar_l3 >= 20) # 30

mean(scored_skills$n_similar_l3) # 0.5114025
mean(scored_skills$n_similar_l3 > 0) # 0.1577878
sum(scored_skills$n_similar_l3 > 0) # 1709
mean(scored_skills$n_similar_l3 >= 2)
mean(scored_skills$n_similar_l3 >= 3) # 0.04994922
sum(scored_skills$n_similar_l3 >= 3) # 541

mean(scored_skills$n_similar_l3_automation_intent) # 0.2046902
mean(scored_skills$n_similar_l3_automation_intent > 0) # 0.09786723
sum(scored_skills$n_similar_l3_automation_intent > 0) # 1060
mean(scored_skills$n_similar_l3_automation_intent >= 3) # 0.02123534
sum(scored_skills$n_similar_l3_automation_intent >= 3) # 230
 
mean(scored_skills$n_similar_l3_augmentation_intent) # 0.2122611
mean(scored_skills$n_similar_l3_augmentation_intent > 0) # 0.09823654
sum(scored_skills$n_similar_l3_augmentation_intent > 0) # 1064
mean(scored_skills$n_similar_l3_augmentation_intent >= 3) # 0.02197396
sum(scored_skills$n_similar_l3_augmentation_intent >= 3) # 238

mean(
  scored_skills$n_similar_l3_automation_intent > 0 & 
    scored_skills$n_similar_l3_augmentation_intent > 0
) # 0.04625612
sum(
  scored_skills$n_similar_l3_automation_intent > 0 & 
    scored_skills$n_similar_l3_augmentation_intent > 0
) # 501

mean(
  scored_skills$n_similar_l3_automation_intent > 0 & 
    scored_skills$n_similar_l3_augmentation_intent == 0
) # 0.05161112
sum(
  scored_skills$n_similar_l3_automation_intent > 0 & 
    scored_skills$n_similar_l3_augmentation_intent == 0
) # 559

mean(
  scored_skills$n_similar_l3_automation_intent == 0 & 
    scored_skills$n_similar_l3_augmentation_intent > 0
) # 0.05198043
sum(
  scored_skills$n_similar_l3_automation_intent == 0 & 
    scored_skills$n_similar_l3_augmentation_intent > 0
) # 563

## examples -------------------------------------------------------------------
scored_skills %>%
  filter(n_similar_l3 == 0) %>%
  pull(esco_skill_label) %>%
  sample(5) 

scored_skills %>%
  arrange(desc(n_similar_l3)) %>%
  filter(n_similar_l3 >= 20) %>% # top 30
  select(
    esco_skill_label, 
    n_similar_l3,
    n_similar_l3_automation_intent,
    n_similar_l3_augmentation_intent
  ) %>%
  mutate(
    perc_automation = n_similar_l3_automation_intent / n_similar_l3,
    perc_augmentation = n_similar_l3_augmentation_intent / n_similar_l3
  ) %>%
  print(n = 30)

scored_skills %>%
  arrange(desc(n_similar_l3_automation_intent)) %>%
  select(
    esco_skill_label, 
    n_similar_l3,
    n_similar_l3_automation_intent,
    n_similar_l3_augmentation_intent
  ) %>%
  mutate(
    perc_automation = n_similar_l3_automation_intent / n_similar_l3
  ) %>%
  filter(perc_automation > 0.66) %>%
  #filter(n_similar_l3_automation_intent > n_similar_l3_augmentation_intent) %>%
  print(n = 15)

scored_skills %>%
  arrange(desc(n_similar_l3_augmentation_intent)) %>%
  select(
    esco_skill_label, 
    n_similar_l3,
    n_similar_l3_automation_intent,
    n_similar_l3_augmentation_intent
  ) %>%
  mutate(
    perc_augmentation = n_similar_l3_augmentation_intent / n_similar_l3
  ) %>%
  filter(perc_augmentation > 0.66) %>%
  #filter(n_similar_l3_automation_intent < n_similar_l3_augmentation_intent) %>%
  print(n = 15)
  
## grouped -------------------------------------------------------------------
scored_skill_groups %>%
  group_by(skill_group_level_1 = supergroup_label) %>%
  summarise(
    n_skills = n()
    , ave_n_similar_l3 = mean(n_similar_l3)
    , ave_n_similar_l3_automation_intent = mean(n_similar_l3_automation_intent)
    , ave_n_similar_l3_augmentation_intent = mean(n_similar_l3_augmentation_intent)
    #, ave_n_similar_l3_company_source = mean(n_similar_l3_company_source)
    #, ave_n_similar_l3_report_source = mean(n_similar_l3_report_source)
  ) %>%
  arrange(desc(ave_n_similar_l3)) %>%
  filter(!is.na(skill_group_level_1))

scored_skill_groups %>%
  group_by(skill_group_level_2 = group_label) %>%
  summarise(
    n_skills = n()
    , ave_n_similar_l3 = mean(n_similar_l3)
    , ave_n_similar_l3_automation_intent = mean(n_similar_l3_automation_intent)
    , ave_n_similar_l3_augmentation_intent = mean(n_similar_l3_augmentation_intent)
    #, ave_n_similar_l3_company_source = mean(n_similar_l3_company_source)
    #, ave_n_similar_l3_report_source = mean(n_similar_l3_report_source)
  ) %>%
  filter(n_skills > 3) %>% # are these small groups a bug?
  arrange(desc(ave_n_similar_l3)) %>%
  filter(!is.na(skill_group_level_2)) %>%
  print(n = Inf)

scored_skills_plot <- scored_skill_groups %>%
  filter(!is.na(supergroup_label)) %>%
  group_by(supergroup_label, group_label) %>%
  mutate(group_mean = mean(n_similar_l3)) %>%
  ungroup() %>%
  mutate(
    supergroup_label = factor(
      supergroup_label,
      levels = scored_skill_groups %>%
        group_by(supergroup_label) %>%
        summarize(supergroup_mean = mean(n_similar_l3)) %>%
        arrange((supergroup_mean)) %>%
        pull(supergroup_label)
    )
  ) %>%
  ggplot(aes(x = n_similar_l3, y = supergroup_label)) +
  #geom_jitter(width = 0, height = 0.2, color = "gray", alpha = 0.3) +  # Jittered points in dim gray
  stat_summary(aes(group = group_label), fun = mean, geom = "point", shape = 4, size = 1, color = "black") +  # Group mean as a smaller blue rhombus
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +  # Supergroup mean as a larger red rhombus
  labs(x = "Average Similarity Score", y = "Supergroup Label") +
  theme_minimal() +
  labs(y = "")

## scored research skills --------------------------------------------------
scored_skills %>%
  filter(esco_skill_uri %in% esco_research_skills$conceptUri) %>%
  select(
    esco_skill_label, 
    n_similar_l3,
    n_similar_l3_automation_intent,
    n_similar_l3_augmentation_intent
  ) %>%
  mutate(
    ave_n_similar_l3 = mean(n_similar_l3)
  ) %>%
  arrange(desc(n_similar_l3))
 

# occupations -------------------------------------------------------------
## cumulative exposure ------------------------------------------------------
scored_occupations$ai_product_exposure_score %>% mean() # 0.109345
scored_occupations$ai_product_exposure_score %>% median() # 0.083333
scored_occupations$ai_product_exposure_score %>% quantile(0.1) # 0
scored_occupations$ai_product_exposure_score %>% quantile(0.2) # 0.0
scored_occupations$ai_product_exposure_score %>% quantile(0.3) # 0.0
scored_occupations$ai_product_exposure_score %>% quantile(0.4) # 0.05263158
scored_occupations$ai_product_exposure_score %>% quantile(0.5) # 0.08333333
scored_occupations$ai_product_exposure_score %>% quantile(0.6) # 0.1111111
scored_occupations$ai_product_exposure_score %>% quantile(0.7) # 0.1428571
scored_occupations$ai_product_exposure_score %>% quantile(0.8) # 0.2
scored_occupations$ai_product_exposure_score %>% quantile(0.9) # 0.2727273

cumul_exposure <- scored_occupations %>%
  select(ai_product_exposure_score) %>%
  arrange((ai_product_exposure_score)) %>%
  mutate(cump = (1:n()) / n()) %>%
  arrange(desc(cump))

filter(cumul_exposure, ai_product_exposure_score == 0)$cump[1] # 32.2%
filter(cumul_exposure, ai_product_exposure_score < 0.05)$cump[1] # 38.6%
filter(cumul_exposure, ai_product_exposure_score < 0.10)$cump[1] # 53.4%
filter(cumul_exposure, ai_product_exposure_score < 0.15)$cump[1] # 70.5%
filter(cumul_exposure, ai_product_exposure_score < 0.20)$cump[1] # 79.95%
filter(cumul_exposure, ai_product_exposure_score < 0.25)$cump[2] # 86.68%
filter(cumul_exposure, ai_product_exposure_score < 0.30)$cump[1] # 91.18%
filter(cumul_exposure, ai_product_exposure_score < 0.40)$cump[1] # 97.27%
filter(cumul_exposure, ai_product_exposure_score < 0.50)$cump[1] # 98.83%

cumul_exposure %>%
  ggplot(
    aes(y  = ai_product_exposure_score, x = cump)
  ) +
  geom_line() +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Cumulative % of occupations",
    y = "AI product exposure score (% of skills)",
    title = "AI product exposure score distribution across occupations"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

## most and least exposed occupations --------------------------------
# highest
scored_occupations %>%
  select(
    occupation_title,
    ai_product_exposure_score,
    ai_product_automation_score,
    ai_product_augmentation_score
  ) %>%
  arrange(desc(ai_product_exposure_score)) %>%
  slice(1:30) %>%
  print(n = Inf)

# lowest
scored_occupations %>%
  select(
    occupation_title,
    ai_product_exposure_score,
    ai_product_automation_score,
    ai_product_augmentation_score
  ) %>%
  arrange(ai_product_exposure_score) %>%
  filter(ai_product_exposure_score == 0) %>%
  print(n = 30)

# highest, strong automation intent
scored_occupations %>%
  arrange(desc(ai_product_exposure_score)) %>%
  select(
    occupation_title,
    ai_product_exposure_score,
    ai_product_automation_score,
    ai_product_augmentation_score
  ) %>%
  mutate(
    perc_automation = ai_product_automation_score / ai_product_exposure_score
  ) %>%
  filter(perc_automation > 0.5) %>%
  print(n = 10)

# highest, strong augmentation intent
scored_occupations %>%
  arrange(desc(ai_product_exposure_score)) %>%
  select(
    occupation_title,
    ai_product_exposure_score,
    ai_product_automation_score,
    ai_product_augmentation_score
  ) %>%
  mutate(
    perc_augmentation = ai_product_augmentation_score / ai_product_exposure_score
  ) %>%
  filter(perc_augmentation > 0.5) %>%
  print(n = 10)


## by occupation group -----------------------------------------------------
scored_occupations_with_group_labels <- scored_occupations %>%
  mutate(
    isco_level_1 = substr(isco_group, 1, 1),
    isco_level_2 = substr(isco_group, 1, 2),
    isco_level_3 = substr(isco_group, 1, 3),
    isco_level_4 = substr(isco_group, 1, 4)
  ) %>%
  left_join(
    isco_groups %>%
      select(isco_level_1 = code, isco_level_1_label = preferredLabel),
    by = "isco_level_1"
  ) %>%
  left_join(
    isco_groups %>%
      select(isco_level_2 = code, isco_level_2_label = preferredLabel),
    by = "isco_level_2"
  ) %>%
  left_join(
    isco_groups %>%
      select(isco_level_3 = code, isco_level_3_label = preferredLabel),
    by = "isco_level_3"
  ) %>%
  left_join(
    isco_groups %>%
      select(isco_level_4 = code, isco_level_4_label = preferredLabel),
    by = "isco_level_4"
  ) %>%
  mutate(
    isco_level_2_label = ifelse(
      isco_level_2_label == "Food processing, wood working, garment and other craft and related trades workers",
      "Craft and related trades workers",
      isco_level_2_label
    ) # shorten name
  )

occupation_scores_level_3_table <- scored_occupations_with_group_labels %>%
  group_by(isco_level_3, isco_level_3_label) %>%
  summarize(group_mean = mean(ai_product_exposure_score)) %>%
  ungroup() %>%
  arrange(desc(group_mean)) %>%
  print(n = Inf)

occupation_scores_level_2_plot <- scored_occupations_with_group_labels %>%
  group_by(
    isco_level_2, 
    isco_level_3
  ) %>%
  mutate(group_mean = mean(ai_product_exposure_score)) %>%
  ungroup() %>%
  mutate(
    isco_level_2_label = factor(
      isco_level_2_label,
      levels = scored_occupations_with_group_labels %>%
        group_by(isco_level_2_label) %>%
        summarize(supergroup_mean = mean(ai_product_exposure_score)) %>%
        arrange((supergroup_mean)) %>%
        pull(isco_level_2_label)
    )
  ) %>%
  ggplot(aes(x = ai_product_exposure_score, y = isco_level_2_label)) +
  geom_jitter(width = 0, height = 0.2, color = "lightgray") +  # Jittered points in dim gray
  #stat_summary(aes(group = isco_level_3_label), fun = mean, geom = "point", shape = 4, size = 1, color = "black") +  # Group mean as a smaller blue rhombus
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +  # Supergroup mean as a larger red rhombus
  labs(x = "Exposure score", y = "") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

occupation_scores_level_2_table <- scored_occupations_with_group_labels %>%
  group_by(isco_level_2, isco_level_2_label) %>%
  summarize(group_mean = mean(ai_product_exposure_score)) %>%
  ungroup() %>%
  print(n = Inf)

occupation_scores_level_1_plot <- scored_occupations_with_group_labels %>%
  group_by(isco_level_1, isco_level_2) %>%
  mutate(group_mean = mean(ai_product_exposure_score)) %>%
  ungroup() %>%
  mutate(
    isco_level_1_label = factor(
      isco_level_1_label,
      levels = scored_occupations_with_group_labels %>%
        group_by(isco_level_1_label) %>%
        summarize(supergroup_mean = mean(ai_product_exposure_score)) %>%
        arrange((supergroup_mean)) %>%
        pull(isco_level_1_label)
    )
  ) %>%
  ggplot(aes(x = ai_product_exposure_score, y = isco_level_1_label)) +
  geom_jitter(width = 0, height = 0.2, color = "gray", alpha = 0.3) +  # Jittered points in dim gray
  stat_summary(aes(group = isco_level_2_label), fun = mean, geom = "point", shape = 4, size = 1, color = "black") +  # Group mean as a smaller blue rhombus
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +  # Supergroup mean as a larger red rhombus
  labs(x = "Exposure score", y = "Occupation") +
  theme_minimal()


## compare alternative metrics ---------------------------------------------
occupation_scores_level_2_alt <- scored_occupations_with_group_labels %>%
  group_by(isco_level_2, isco_level_2_label) %>%
  summarize(
    group_mean = mean(ai_product_exposure_score)
    , group_mean_automation = mean(ai_product_automation_score)
    , group_mean_augmentation = mean(ai_product_augmentation_score)
    , group_mean_third_party = mean(ai_product_third_party_score)
    , auto_percent = group_mean_automation / group_mean
    , aug_percent = group_mean_augmentation / group_mean
  ) %>%
  ungroup() %>%
  print(n = Inf)

occupation_scores_level_2_alt %>% 
  arrange(desc(group_mean)) %>%
  print(n = 10)

occupation_scores_level_2_alt %>%
  arrange(desc(group_mean_automation)) %>%
  filter(group_mean_automation > group_mean_augmentation) %>%
  print(n = 10)

occupation_scores_level_2_alt %>%
  arrange(desc(group_mean_augmentation)) %>%
  filter(group_mean_augmentation > group_mean_automation) %>%
  print(n = 10)

occupation_scores_level_2_alt %>%
  arrange(desc(group_mean_third_party)) %>%
  print(n = 10)

occupation_scores_level_3_alt <- scored_occupations_with_group_labels %>%
  group_by(isco_level_3, isco_level_3_label) %>%
  summarize(
    group_mean = mean(ai_product_exposure_score)
    , group_mean_automation = mean(ai_product_automation_score)
    , group_mean_augmentation = mean(ai_product_augmentation_score)
    , group_mean_third_party = mean(ai_product_third_party_score)
    , auto_percent = group_mean_automation / group_mean
    , aug_percent = group_mean_augmentation / group_mean
  ) %>%
  ungroup() %>%
  print(n = Inf)

occupation_scores_level_3_alt %>%
  arrange(desc(group_mean_automation)) %>%
  filter(group_mean_automation > group_mean_augmentation) %>%
  print(n = 10)

occupation_scores_level_3_alt %>%
  arrange(desc(group_mean_augmentation)) %>%
  filter(group_mean_augmentation > group_mean_automation) %>%
  print(n = 10)

# save results ------------------------------------------------------------
ggsave(
  file.path(args$output_dir, "plots", "scored_skills_plot.eps"),
  scored_skills_plot,
  width = 5,
  height = 3
)

ggsave(
  file.path(args$output_dir, "plots", "occupation_scores_level_2_plot.eps"),
  occupation_scores_level_2_plot,
  width = 9,
  height = 7
)

ggsave(
  file.path(args$output_dir, "plots", "occupation_scores_level_1_plot.eps"),
  occupation_scores_level_1_plot,
  width = 9,
  height = 5
)


