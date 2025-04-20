library(tidyverse)
library(argparse)
library(showtext)
library(sysfonts)

# define script arguments -------------------------------------------------
parser <- ArgumentParser()

parser$add_argument(
  "--scored_skills", 
  type = "character",
  help = "Path to scored skills file", 
  default = "results/scored_esco_skills/scored_esco_skills.csv"
)
parser$add_argument(
  "--esco_occupations", 
  type = "character",
  help = "Path to esco occupations file",
  default = "data/esco/occupations_en.csv"
)
parser$add_argument(
  "--esco_skills", 
  type = "character",
  help = "Path to esco skills file",
  default = "data/esco/skills_en.csv"
)
parser$add_argument(
  "--esco_occupation_skill_mapping", 
  type = "character",
  help = "Path to esco occupation skill mapping file",
  default = "data/esco/occupationSkillRelations_en.csv"
)
parser$add_argument(
  "--esco_research_skills", 
  type = "character",
  help = "Path to esco research skills file",
  default = "data/esco/researchSkillsCollection_en.csv"
)
parser$add_argument(
  "--esco_skill_groups",
  type = "character",
  help = "Path to esco skill groups file",
  default = "data/esco/skillGroups_en.csv"
)
parser$add_argument(
  "--esco_skill_hierarchy",
  type = "character",
  help = "Path to esco skill to skill group relations file",
  default = "data/esco/skillsHierarchy_en.csv"
)
parser$add_argument(
  "--esco_skill_relations",
  type = "character",
  help = "Path to esco occupation to occupation group relations file",
  default = "data/esco/broaderRelationsSkillPillar_en.csv"
)
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

# define functions --------------------------------------------------------
aggregate_score_to_occupation_level <- function(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_mean_of_max_weighted",
  remove_extra = T,
  standardize = T,
  discrete_cutoff = 0
) {
  res <- esco_occupation_skill_mapping %>%
    rename(
      occupation_uri = "occupationUri",
      skill_uri = "skillUri",
      relation_type = "relationType",
      skill_type = "skillType"
    ) %>%
    inner_join(scored_skills, by = c("skill_uri" = "esco_skill_uri")) %>%
    inner_join(
      esco_occupations %>%
        select(
          occupation_uri = conceptUri,
          isco_group = iscoGroup,
          occupation_title = preferredLabel,
          alt_titles = altLabels,
          description = description,
          definition = definition
        ),
      by = c("occupation_uri" = "occupation_uri")
    ) %>%
    select(
      occupation_uri,
      occupation_title,
      isco_group,
      alt_titles,
      description,
      definition,
      skill_uri,
      skill_label = esco_skill_label,
      relation_type,
      #skill_type,
      n_similar = n_similar_l3,
      n_similar_augmentation = n_similar_l3_augmentation_intent,
      n_similar_automation = n_similar_l3_automation_intent,
      mean_similar = mean_similarity_l3,
      mean_similar_augmentation = mean_similarity_l3_augmentation_intent,
      mean_similar_automation = mean_similarity_l3_automation_intent,
      logsumexp_similarity = logsumexp_similarity_10,
      top5_simialrity = top5_similarity,
      max_similarity,
      max_similarity_augmentation = max_similarity_augmentation_intent,
      max_similarity_automation = max_similarity_automation_intent,
      mean_similarity
    ) %>%
    mutate(
      across(starts_with("n_similar"), ~ replace_na(.x, 0)),
    ) %>%
    group_by(
      occupation_uri,
      occupation_title,
      isco_group,
      alt_titles,
      description,
      definition
    ) %>%
    summarise(
      n_skills = n(),
      exposure_score_n_similar = sum(n_similar),
      exposure_score_n_similar_augmentation = sum(
        n_similar_augmentation
      ),
      exposure_score_n_similar_automation = sum(
        n_similar_automation
      ),
      exposure_score_p_similar = mean(n_similar > discrete_cutoff),
      exposure_score_p_similar_augmentation = mean(
        n_similar_augmentation > discrete_cutoff
      ),
      exposure_score_p_similar_automation = mean(
        n_similar_automation > discrete_cutoff
      ),
      exposure_score_mean_similar_weighted = weighted.mean(
        ifelse(is.nan(mean_similar), 0, mean_similar),
        ifelse(relation_type == "essential", 1, 0.5),
        na.rm = T
      ),
      exposure_score_mean_similar_augmentation_weighted = weighted.mean(
        ifelse(is.nan(mean_similar_augmentation), 0, mean_similar_augmentation),
        ifelse(relation_type == "essential", 1, 0.5),
        na.rm = T
      ),
      exposure_score_mean_similar_automation_weighted = weighted.mean(
        ifelse(is.nan(mean_similar_automation), 0, mean_similar_automation),
        ifelse(relation_type == "essential", 1, 0.5),
        na.rm = T
      ),
      exposure_score_mean_of_max = mean(max_similarity),
      exposure_score_mean_of_max_augmentation = mean(
        max_similarity_augmentation
      ),
      exposure_score_mean_of_max_automation = mean(
        max_similarity_automation
      ),
      exposure_score_mean_of_logsumexp = mean(
        logsumexp_similarity
      ),
      exposure_score_mean_of_top_5 = mean(
        top5_simialrity
      ),
      exposure_score_mean_of_mean = mean(mean_similarity),
      exposure_score_mean_of_max_essential = weighted.mean(
        max_similarity,
        weights = ifelse(
          relation_type == "essential", 1, 0
        )
      ),
      exposure_score_mean_of_max_optional = weighted.mean(
        max_similarity,
        weights = ifelse(
          relation_type == "optional", 1, 0
        )
      ),
      exposure_score_mean_of_max_weighted = weighted.mean(
        max_similarity,
        weights = ifelse(
          relation_type == "essential", 1, 0.5
        )
      )
    ) %>%
    ungroup() %>%
    arrange(desc(exposure_score_mean_of_max_weighted))
  
  res$ai_product_exposure_score <- res[[final_measure]]
  
  if (standardize) {
    res <- res %>%
      mutate(
        ai_product_exposure_score = (
          ai_product_exposure_score - mean(ai_product_exposure_score)
        ) / sd(ai_product_exposure_score)
      )
  }
  
  res <- res %>%
    mutate(
      ai_product_exposure_percentile = ecdf(ai_product_exposure_score)(
        ai_product_exposure_score
      ) # rank
    )
  
  if (remove_extra) {
    res <- res %>%
      select(
        occupation_uri,
        occupation_title,
        isco_group,
        ai_product_exposure_score,
        ai_product_exposure_percentile
      )
  }
  
  return(res)
}

score_research_skills <- function(
  scored_skills, esco_research_skills, return_all = F  
) {
  res <- scored_skills %>%
    left_join(
      esco_research_skills %>%
        mutate(is_research = T) %>%
        select(conceptUri, is_research),
      by = c("esco_skill_uri"= "conceptUri")
    ) %>%
    mutate(
      is_research = ifelse(is.na(is_research), F, is_research),
      n_similar_l3_standardized = (n_similar_l3 - mean(n_similar_l3))/
        sd(n_similar_l3),
      n_similar_l3_percentile = ecdf(n_similar_l3)(n_similar_l3)
    ) %>%
    select(
      esco_skill_uri,
      esco_skill_label,
      n_similar_l3,
      n_similar_l3_standardized,
      n_similar_l3_percentile,
      is_research
    )
  
  if (return_all) return(res)
  
  res %>%
    filter(is_research) %>%
    arrange(desc(n_similar_l3))
}

plot_research_skills <- function(
  scored_skills, esco_research_skills
) {
  score_research_skills(scored_skills, esco_research_skills, return_all = T) %>%
    ggplot(aes(x = n_similar_l3_standardized)) +
    geom_density(aes(color = "All job skills"), alpha = 0.5) +  # Assign color aesthetic
    geom_point(
      data = . %>% filter(is_research),
      aes(
        x = n_similar_l3_standardized, y = 0, color = "Research-relevant skills"
      ),  # Assign color aesthetic
      size = 2
    ) +
    scale_color_manual(  # Define colors and labels for the legend
      values = c("All job skills" = "black", "Research-relevant skills" = "red"),
      labels = c("All job skills", "Research-relevant skills")
    ) +
    theme_minimal() +
    labs(
      title = "Similarity of Research Skills to AI Product Capabilities",
      x = "AI Capability/Job Skill Similarity Score",
      y = "Density"
    ) +
    theme(legend.position = "right", legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(
      linetype = c("solid", "blank"),  # 'blank' makes the line disappear for the dots
      shape = c(NA, 16)                # NA for no shape for the line, 16 for a dot
    ))) +
    theme(text = element_text(family = "merriweather"))
}

plot_research_skills_boxplot <- function(
  scored_skills, esco_research_skills
) {
  score_research_skills(scored_skills, esco_research_skills, return_all = T) %>%
    ggplot(aes(x = is_research, y = n_similar_l3_standardized, color = is_research)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
      title = "Similarity of Research Skills to AI Product Capabilities",
      x = "Research-relevant skills",
      y = "AI Capability/Job Skill Similarity Score"
    ) +
    theme(text = element_text(family = "merriweather"))
}

aggregate_skills_to_groups <- function(
  scored_skills, esco_skill_hierarchy, esco_skill_relations  
) {
  scored_skills %>%
    mutate(
      n_similar_l3_standardized = (n_similar_l3 - mean(n_similar_l3))/
        sd(n_similar_l3)
    ) %>%
    select(esco_skill_uri, n_similar_l3) %>%
    left_join(
      esco_skill_relations %>%
        filter(broaderType == "SkillGroup") %>%
        select(
          skill_uri = conceptUri,
          subgroup_uri = broaderUri
        ),
      by = c("esco_skill_uri" = "skill_uri")
    ) %>%
    left_join(
      esco_skill_relations %>%
        filter(broaderType == "SkillGroup") %>%
        select(
          subgroup_uri = conceptUri,
          group_uri = broaderUri
        ),
      by = c("subgroup_uri")
    ) %>%
    left_join(
      esco_skill_relations %>%
        filter(broaderType == "SkillGroup") %>%
        select(
          group_uri = conceptUri,
          supergroup_uri = broaderUri
        ),
      by = c("group_uri")
    ) %>%
    left_join(
      esco_skill_hierarchy %>%
        select(
          level_3_uri, 
          subgroup_label = level_3_preferred_term,
          group_label = level_2_preferred_term,
          supergroup_label = level_1_preferred_term
        ),
      by = c("subgroup_uri" = "level_3_uri")
    ) 
}


# read data ---------------------------------------------------------------
args <- parser$parse_args()

scored_skills <- read_csv(args$scored_skills)

# correlation matrix between scored_skills$max_smilarity, scored_skills$top5_similarity, scored_skills$logsumexp_similarity_10
scored_skills %>%
  select(
    max_similarity, top5_similarity, logsumexp_similarity_10,
    max_similarity_automation_intent, 
    max_similarity_augmentation_intent,
    mean_similarity_l3,
    mean_similarity_l3_automation_intent,
    mean_similarity_l3_augmentation_intent,
    n_similar_l3
  ) %>%
  cor(use = "pairwise.complete.obs")

esco_occupations <- read_csv(args$esco_occupations)
esco_skills <- read_csv(args$esco_skills)
esco_occupation_skill_mapping <- read_csv(
  args$esco_occupation_skill_mapping
)
esco_research_skills <- read_csv(
  args$esco_research_skills
)
esco_skill_groups <- read_csv(args$esco_skill_groups)
esco_skill_hierarchy <- read_csv(args$esco_skill_hierarchy)
esco_skill_relations <- read_csv(args$esco_skill_relations)
isco_groups <- read_csv(args$isco_groups)

esco_skill_hierarchy <- esco_skill_hierarchy %>%
  rename_all(~ tolower(gsub(" ", "_", .))) %>%
  filter(!is.na(level_3_uri)) # only keep the most detailed level


# find skill groups -------------------------------------------------------
scored_skills %>% 
  select(esco_skill_label, n_similar_l3) %>% 
  arrange(desc(n_similar_l3)) %>%
  print(n = 200)

scored_skill_groups <- aggregate_skills_to_groups(
  scored_skills,
  esco_skill_hierarchy,
  esco_skill_relations
)

# aggregate occupation scores ---------------------------------------------
scored_occupations <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_p_similar",
  remove_extra = T,
  standardize = F, discrete_cutoff = 0
) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_n_similar",
      remove_extra = T,
      standardize = T, discrete_cutoff = 0
    ) %>%
      select(
        occupation_uri,
        ai_product_count_score = ai_product_exposure_score
      ),
    by = "occupation_uri"
  ) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_augmentation",
      remove_extra = T,
      standardize = F, discrete_cutoff = 0
    ) %>%
      select(
        occupation_uri,
        ai_product_augmentation_score = ai_product_exposure_score
      ),
    by = "occupation_uri"
  ) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_automation",
      remove_extra = T,
      standardize = F, discrete_cutoff = 0
    ) %>%
      select(
        occupation_uri,
        ai_product_automation_score = ai_product_exposure_score
      ),
    by = "occupation_uri"
  )

scored_occupations %>%
  ggplot(aes(
    x = ai_product_count_score,
    y = ai_product_exposure_score
  )) +
  geom_point() +
  geom_smooth(method = "lm")

cor(
  scored_occupations$ai_product_count_score, 
  scored_occupations$ai_product_exposure_score, 
  method = "spearman",
  use = "pairwise.complete.obs"
)

# examine different final measures ----------------------------------------
potential_measures <- c(
  "exposure_score_n_similar",
  "exposure_score_n_similar_augmentation",
  "exposure_score_n_similar_automation",
  "exposure_score_p_similar",
  "exposure_score_p_similar_augmentation",
  "exposure_score_p_similar_automation",
  "exposure_score_mean_of_max_weighted",
  #"exposure_score_mean_of_max_essential",
  "exposure_score_mean_of_max_automation",
  "exposure_score_mean_of_max_augmentation",
  "exposure_score_mean_of_logsumexp",
  "exposure_score_mean_of_top_5",
  "exposure_score_mean_of_mean",
  "exposure_score_mean_similar_weighted",
  "exposure_score_mean_similar_automation_weighted",
  "exposure_score_mean_similar_augmentation_weighted"
)

all_measures <- map(potential_measures, function(ms) {
  print(ms)
  aggregate_score_to_occupation_level(
    scored_skills,
    esco_occupation_skill_mapping,
    esco_occupations,
    final_measure = ms,
    remove_extra = T
  ) %>% 
    select(occupation_uri, occupation_title, ai_product_exposure_percentile) %>%
    rename(
      !!ms := ai_product_exposure_percentile
    )
}) %>%
  reduce(
    left_join,
    by = c("occupation_uri", "occupation_title")
  )

# pairwise rank correlations across measures
all_measures %>%
  select(-occupation_uri, -occupation_title) %>%
  cor(method = "spearman") %>%
  as.data.frame() %>%
  rownames_to_column("measure") %>%
  pivot_longer(-measure, names_to = "measure_2", values_to = "correlation") %>%
  filter(measure != measure_2) %>%
  arrange(desc(correlation)) %>%
  mutate(
    correlation = round(correlation, 3)
  ) %>%
  print(n = Inf)

# plot occupation scores --------------------------------------------------
scored_occupations_with_group_labels <- scored_occupations %>%
  mutate(
    isco_level_1 = substr(isco_group, 1, 1),
    isco_level_2 = substr(isco_group, 1, 2),
    isco_level_3 = substr(isco_group, 1, 3),
    isco_level_4 = substr(isco_group, 1, 4)
  ) %>%
  left_join(
    select(
      isco_groups, isco_level_1 = code, isco_level_1_label = preferredLabel
    ),
    by = "isco_level_1"
  ) %>%
  left_join(
    isco_groups %>%
      select(
        isco_level_2 = code, isco_level_2_label = preferredLabel
      ),
    by = "isco_level_2"
  ) %>%
  left_join(
    isco_groups %>%
      select(
        isco_level_3 = code, isco_level_3_label = preferredLabel
      ),
    by = "isco_level_3"
  ) %>%
  left_join(
    isco_groups %>%
      select(
        isco_level_4 = code, isco_level_4_label = preferredLabel
      ),
    by = "isco_level_4"
  )

scored_occupations_with_group_labels %>%
  arrange(desc(ai_product_exposure_score)) %>%
  select(occupation_title, isco_group, ends_with("_score"))

scored_occupations_with_group_labels %>%
  arrange(desc(ai_product_automation_score)) %>%
  select(occupation_title, isco_group, ends_with("_score"))

scored_occupations_with_group_labels %>%
  arrange(desc(ai_product_augmentation_score)) %>%
  select(occupation_title, isco_group, ends_with("_score"))

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
    isco_level_2_label = ifelse(
      isco_level_2_label == "Food processing, wood working, garment and other craft and related trades workers",
      "Craft and related trades workers",
      isco_level_2_label
    ),
    isco_level_2_label = factor(
      isco_level_2_label,
      levels = scored_occupations_with_group_labels %>%
        mutate(
          isco_level_2_label = ifelse(
            isco_level_2_label == "Food processing, wood working, garment and other craft and related trades workers",
            "Craft and related trades workers",
            isco_level_2_label
          )
        ) %>%
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
  mutate(
    isco_level_2_label = ifelse(
      isco_level_2_label == "Food processing, wood working, garment and other craft and related trades workers",
      "Craft and related trades workers",
      isco_level_2_label
    ),
    isco_level_2_label = factor(
      isco_level_2_label,
      levels = scored_occupations_with_group_labels %>%
        mutate(
          isco_level_2_label = ifelse(
            isco_level_2_label == "Food processing, wood working, garment and other craft and related trades workers",
            "Craft and related trades workers",
            isco_level_2_label
          )
        ) %>%
        group_by(isco_level_2_label) %>%
        summarize(supergroup_mean = mean(ai_product_exposure_score)) %>%
        arrange((supergroup_mean)) %>%
        pull(isco_level_2_label)
    )
  ) %>%
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


# # identify research relevant skills -------------------------------------
scored_research_skills <- score_research_skills(
  scored_skills,
  esco_research_skills,
  return_all = F
)

research_skills_plot <- plot_research_skills(
  scored_skills,
  esco_research_skills
)

research_skills_box_plot <- plot_research_skills_boxplot(
  scored_skills,
  esco_research_skills
) + theme(legend.position = "none")

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

scored_skill_groups %>%
  filter(!is.na(supergroup_label)) %>%
  group_by(supergroup_label, group_label) %>%
  summarize(group_mean = mean(n_similar_l3)) %>%
  arrange(supergroup_label, group_mean) %>%    
  arrange(desc(group_mean)) %>%
  filter(!supergroup_label == group_label) %>%
  print(n = Inf) 


# save results ------------------------------------------------------------
write_csv(
  scored_occupations, 
  file.path(
    args$output_dir, 
    "occupational_exposure_to_ai_products",
    "scored_esco_occupations.csv"
  )
)

write_csv(
  scored_occupations %>%
    group_by(isco_level_4 = isco_group) %>%
    summarise(
      #n_occupations = n(),
      ai_product_exposure_score = mean(ai_product_exposure_score)
    ), 
  file.path(
    args$output_dir,
    "occupational_exposure_to_ai_products",
    "scored_esco_occupations_isco_4_digit.csv"
  )
)

write_csv(
  scored_occupations %>%
    group_by(isco_level_3 = substr(isco_group, 1, 3)) %>%
    summarise(
      #n_occupations = n(),
      ai_product_exposure_score = mean(ai_product_exposure_score)
    ),
  file.path(
    args$output_dir, 
    "occupational_exposure_to_ai_products",
    "scored_esco_occupations_isco_3_digit.csv"
  )
)

write_csv(
  scored_occupations %>%
    group_by(isco_level_2 = substr(isco_group, 1, 2)) %>%
    summarise(
      #n_occupations = n(),
      ai_product_exposure_score = mean(ai_product_exposure_score)
    ),
  file.path(
    args$output_dir, 
    "occupational_exposure_to_ai_products",
    "scored_esco_occupations_isco_2_digit.csv"
  )
)

write_csv(
  scored_occupations %>%
    group_by(isco_level_1 = substr(isco_group, 1, 1)) %>%
    summarise(
      #n_occupations = n(),
      ai_product_exposure_score = mean(ai_product_exposure_score)
    ),
  file.path(
    args$output_dir, 
    "occupational_exposure_to_ai_products",
    "scored_esco_occupations_isco_1_digit.csv"
  )
)

write_csv(
  scored_skill_groups %>%
    group_by(skill_group_level_1 = supergroup_label) %>%
    summarise(
      n_skills = n(),
      ave_max_similarity_standardized = mean(max_similarity_standardized)
    ) %>%
    arrange(desc(ave_max_similarity_standardized)) %>%
    filter(!is.na(skill_group_level_1)), 
  file.path(
    args$output_dir, 
    "scored_esco_skills",
    "scored_esco_skills_level_1_groups.csv"
  ) 
)

write_csv(
  scored_skill_groups %>%
    group_by(skill_group_level_2 = group_label) %>%
    summarise(
      n_skills = n(),
      ave_max_similarity_standardized = mean(max_similarity_standardized)
    ) %>%
    arrange(desc(ave_max_similarity_standardized)) %>%
    filter(!is.na(skill_group_level_2)), 
  file.path(
    args$output_dir, 
    "scored_esco_skills",
    "scored_esco_skills_level_2_groups.csv"
  )
)

ggsave(
  file.path(args$output_dir, "plots", "research_skills_plot.eps"),
  research_skills_plot + theme(legend.position = "bottom"),
  width = 7,
  height = 5
)

ggsave(
  file.path(args$output_dir, "plots", "research_skills_boxplot.eps"),
  research_skills_box_plot + theme(legend.position = "none"),
  width = 7,
  height = 5
)

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

scored_occupations %>%
  group_by(isco_level_3 = substr(isco_group, 1, 3)) %>%
  summarise(
    #n_occupations = n(),
    ai_product_exposure_score = mean(ai_product_exposure_score)
  ) %>%
  arrange(desc(ai_product_exposure_score)) %>%
  left_join(
    isco_groups %>% select(preferredLabel, code), by = c("isco_level_3" = "code")
  ) %>% 
  print(n = Inf) %>% # for table
  slice(c(1:15, (n() - 14):n())) %>%
  print(n = Inf)

