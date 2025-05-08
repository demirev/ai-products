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
      n_similar_company = n_similar_l3_company_source,
      n_similar_report = n_similar_l3_report_source,
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
      n_essential_skills = sum(
        relation_type == "essential"
      ),
      exposure_score_n_similar = sum(n_similar),
      exposure_score_n_similar_augmentation = sum(
        n_similar_augmentation
      ),
      exposure_score_n_similar_automation = sum(
        n_similar_automation
      ),
      exposure_score_p_similar = mean(n_similar > discrete_cutoff),
      exposure_score_p_similar_essential = sum(
        n_similar > discrete_cutoff & relation_type == "essential"
      ) / n_essential_skills,
      exposure_score_p_similar_augmentation = sum(ifelse(
        n_similar > discrete_cutoff,
        n_similar_augmentation / n_similar,
        0
      )) / n_skills,
      exposure_score_p_similar_augmentation_essential = sum(ifelse(
        n_similar > discrete_cutoff & relation_type == "essential",
        n_similar_augmentation / n_similar,
        0
      )) / n_essential_skills,
      exposure_score_p_similar_automation = sum(ifelse(
        n_similar > discrete_cutoff,
        n_similar_automation / n_similar,
        0
      )) / n_skills,
      exposure_score_p_similar_automation_essential = sum(ifelse(
        n_similar > discrete_cutoff & relation_type == "essential",
        n_similar_automation / n_similar,
        0
      )) / n_essential_skills,
      # exposure_score_p_similar_augmentation = mean(
      #   n_similar_augmentation > discrete_cutoff
      # ),
      # exposure_score_p_similar_automation = mean(
      #   n_similar_automation > discrete_cutoff
      # ),
      exposure_score_p_similar_company = mean(
        n_similar_company > discrete_cutoff
      ),
      exposure_score_p_similar_report = mean(
        n_similar_report > discrete_cutoff
      ),
      exposure_score_p_similar_report_essential = sum(
        n_similar_report > discrete_cutoff & relation_type == "essential"
      ) / n_essential_skills,
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
  
  if (any(is.na(res$ai_product_exposure_score))) {
    n_missing <- sum(is.na(res$ai_product_exposure_score))
    message(
      paste0(
        "Warning: ", n_missing, " occupations have NA exposure scores. ",
        "This may be due to no essential skills for a given occupation."
      )
    )
    res <- filter(
      res,
      !is.na(ai_product_exposure_score),
    )
  }
  
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
    # mutate(
    #   n_similar_l3_standardized = (n_similar_l3 - mean(n_similar_l3))/
    #     sd(n_similar_l3)
    # ) %>%
    select(
      esco_skill_uri,
      n_similar_l3, 
      n_similar_l3_automation_intent,
      n_similar_l3_augmentation_intent,
      n_similar_l3_company_source,
      n_similar_l3_report_source
    ) %>%
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
scored_skill_groups <- aggregate_skills_to_groups(
  scored_skills,
  esco_skill_hierarchy,
  esco_skill_relations
)

# aggregate occupation scores ---------------------------------------------
scored_skills$n_similar_l3 %>% quantile(seq(0,1,0.01)) # 97% have < 3, 84% < 0

chosen_cutoff = 2 # at least n+1 required

sum(scored_skills$n_similar_l3 > chosen_cutoff) # 549 yes, 10282 no
sum(scored_skills$n_similar_l3 > 0) # 1709 yes, 9122 no

scored_occupations <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_p_similar_essential",
  remove_extra = T,
  standardize = F, discrete_cutoff = chosen_cutoff
) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_augmentation_essential",
      remove_extra = T,
      standardize = F, discrete_cutoff = chosen_cutoff
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
      final_measure = "exposure_score_p_similar_automation_essential",
      remove_extra = T,
      standardize = F, discrete_cutoff = chosen_cutoff
    ) %>%
      select(
        occupation_uri,
        ai_product_automation_score = ai_product_exposure_score
      ),
    by = "occupation_uri"
  ) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_report_essential",
      remove_extra = T,
      standardize = F, discrete_cutoff = chosen_cutoff
    ) %>%
      select(
        occupation_uri,
        ai_product_third_party_score = ai_product_exposure_score
      ),
    by = "occupation_uri"
  )


# examine using count above threshold -------------------------------------
alt_count_scores <- scored_occupations %>%
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
        ai_product_count_score = ai_product_exposure_score,
        ai_product_count_percentile = ai_product_exposure_percentile
      ),
    by = "occupation_uri"
  )

alt_count_scores %>%
  ggplot(aes(
    x = ai_product_count_score,
    y = ai_product_exposure_score
  )) +
  geom_point() +
  geom_smooth(method = "lm")

cor(
  alt_count_scores$ai_product_count_score, 
  alt_count_scores$ai_product_exposure_score, 
  method = "spearman",
  use = "pairwise.complete.obs"
)

alt_count_scores %>% 
  select(
    occupation_title, 
    discrete = ai_product_exposure_percentile, 
    count = ai_product_count_percentile
  ) %>% 
  mutate(absdiff = abs(discrete - count)) %>% 
  arrange(desc(absdiff)) %>%
  print(n = 50) %>%
  pull(absdiff) %>% 
  hist()

# examine different discrete cutoffs --------------------------------------
discrete_cutoffs <- c(1, 2, 3, 5, 10)

alt_dc_scores <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_p_similar",
  remove_extra = T,
  standardize = F, discrete_cutoff = 0
) %>%
  select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
  rename(
    ai_product_exposure_score_0 = ai_product_exposure_score
  )

for (dc in discrete_cutoffs) {
  new_score <- aggregate_score_to_occupation_level(
    scored_skills,
    esco_occupation_skill_mapping,
    esco_occupations,
    final_measure = "exposure_score_p_similar",
    remove_extra = T,
    standardize = F, discrete_cutoff = dc
  )
  
  colnames(new_score)[colnames(new_score) == "ai_product_exposure_score"] <- paste0(
    "ai_product_exposure_score_",
    dc
  )
  
  alt_dc_scores <- alt_dc_scores %>%
    left_join(
      new_score %>%
        select(occupation_uri, !!sym(paste0("ai_product_exposure_score_", dc))),
      by = "occupation_uri"
    )
  
  cor(
    alt_dc_scores$ai_product_exposure_score_0,
    alt_dc_scores[[paste0("ai_product_exposure_score_", dc)]],
    method = "spearman",
    use = "pairwise.complete.obs"
  ) %>%
    print()
}

alt_dc_scores %>%
  ggplot(
    aes(
      x = ai_product_exposure_score_2,
      y = ai_product_exposure_score_10
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Correlation between different discrete cutoffs",
    x = "Discrete cutoff 0",
    y = "Discrete cutoff 10"
  )

alt_dc_scores %>%
  select(
    occupation_title,
    main = ai_product_exposure_score_0,
    alt = ai_product_exposure_score_2,
  ) %>%
  mutate(
    main_percentile = ecdf(main)(main),
    alt_percentile = ecdf(alt)(alt)
  ) %>%
  filter(
    main > mean(main) |
      alt > mean(alt)
  ) %>%
  mutate(
    absdiff = abs(main - alt),
    percdiff = abs(main_percentile - alt_percentile)
  ) %>%
  arrange(desc(percdiff)) %>%
  print(n = 50) %>%
  pull(absdiff) %>%
  hist()
  

# examine self report vs independent --------------------------------------
alt_source_scores <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_p_similar_company",
  remove_extra = T,
  standardize = F, discrete_cutoff = 0
) %>%
  select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
  rename(
    ai_product_exposure_company = ai_product_exposure_score
  ) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_report",
      remove_extra = T,
      standardize = F, discrete_cutoff = 0
    ) %>%
      select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
      rename(
        ai_product_exposure_independent = ai_product_exposure_score
      ),
    by = c("occupation_uri", "occupation_title")
  )

alt_source_scores_1_0 <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_p_similar_company",
  remove_extra = T,
  standardize = F, discrete_cutoff = 1 # to account for company press releases being 2.46 times more frequent
) %>%
  select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
  rename(
    ai_product_exposure_company = ai_product_exposure_score
  ) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_report",
      remove_extra = T,
      standardize = F, discrete_cutoff = 0
    ) %>%
      select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
      rename(
        ai_product_exposure_independent = ai_product_exposure_score
      ),
    by = c("occupation_uri", "occupation_title")
  )

alt_source_scores_1_1 <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_p_similar_company",
  remove_extra = T,
  standardize = F, discrete_cutoff = 1 # to account for company press releases being 2.46 times more frequent
) %>%
  select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
  rename(
    ai_product_exposure_company = ai_product_exposure_score
  ) %>%
  left_join(
    aggregate_score_to_occupation_level(
      scored_skills,
      esco_occupation_skill_mapping,
      esco_occupations,
      final_measure = "exposure_score_p_similar_report",
      remove_extra = T,
      standardize = F, discrete_cutoff = 0
    ) %>%
      select(occupation_uri, occupation_title, ai_product_exposure_score) %>%
      rename(
        ai_product_exposure_independent = ai_product_exposure_score
      ),
    by = c("occupation_uri", "occupation_title")
  )

cor(
  alt_source_scores$ai_product_exposure_company,
  alt_source_scores$ai_product_exposure_independent,
  method = "spearman",
  use = "pairwise.complete.obs"
)

summary(
  lm(
    ai_product_exposure_independent ~ ai_product_exposure_company,
    data = alt_source_scores
  )
)

alt_source_scores %>%
  ggplot(
    aes(
      x = ai_product_exposure_company,
      y = ai_product_exposure_independent
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Correlation between self-reported and independent measures",
    x = "Self-reported measure",
    y = "Independent measure"
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
  scored_skill_groups, 
  file.path(
    args$output_dir, 
    "occupational_exposure_to_ai_products",
    "scored_esco_skills_all_groups.csv"
  )
)

write_csv(
  scored_skill_groups %>%
    group_by(skill_group_level_1 = supergroup_label) %>%
    summarise(
      n_skills = n(),
      ave_n_similar_l3 = mean(n_similar_l3),
      ave_n_similar_l3_automation_intent = mean(n_similar_l3_automation_intent),
      ave_n_similar_l3_augmentation_intent = mean(n_similar_l3_augmentation_intent),
      ave_n_similar_l3_company_source = mean(n_similar_l3_company_source),
      ave_n_similar_l3_report_source = mean(n_similar_l3_report_source)
    ) %>%
    arrange(desc(ave_n_similar_l3)) %>%
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
      ave_n_similar_l3 = mean(n_similar_l3),
      ave_n_similar_l3_automation_intent = mean(n_similar_l3_automation_intent),
      ave_n_similar_l3_augmentation_intent = mean(n_similar_l3_augmentation_intent),
      ave_n_similar_l3_company_source = mean(n_similar_l3_company_source),
      ave_n_similar_l3_report_source = mean(n_similar_l3_report_source)
    ) %>%
    arrange(desc(ave_n_similar_l3)) %>%
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

