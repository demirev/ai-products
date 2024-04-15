library(tidyverse)

scored_skills <- read_csv("results/scored_esco_skills.csv")
esco_occupations <- read_csv("data/esco/occupations_en.csv")
esco_skills <- read_csv("data/esco/skills_en.csv")
esco_occupation_skill_mapping <- read_csv(
  "data/esco/occupationSkillRelations_en.csv"
)
esco_research_skills <- read_csv(
  "data/esco/researchSkillsCollection_en.csv"
)

aggregate_score_to_occupation_level <- function(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_mean_of_max_weighted",
  remove_extra = T
) {
  res <- esco_occupation_skill_mapping %>%
    rename(
      occupation_uri = "occupationUri",
      skill_uri = "skillUri",
      realtion_type = "relationType",
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
      realtion_type,
      #skill_type,
      n_similar,
      max_similarity,
      mean_similarity
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
      exposure_score_n_similar = mean(n_similar),
      exposure_score_mean_of_max = mean(max_similarity),
      exposure_score_mean_of_mean = mean(mean_similarity),
      exposure_score_mean_of_max_essential = mean(
        max_similarity[realtion_type == "essential"]
      ),
      exposure_score_mean_of_max_optional = mean(
        max_similarity[realtion_type == "optional"]
      ),
      exposure_score_mean_of_max_weighted = weighted.mean(
        c(
          max_similarity[realtion_type == "essential"],
          max_similarity[realtion_type == "optional"]
        ),
        weights = c(
          rep(1, sum(realtion_type == "essential")),
          rep(0.5, sum(realtion_type == "optional"))
        )
      )
    ) %>%
    ungroup() %>%
    arrange(desc(exposure_score_mean_of_max_weighted))
  
  res$ai_product_exposure_score <- res[[final_measure]]
  
  res <- res %>%
    mutate(
      ai_product_exposure_score = (
        ai_product_exposure_score - mean(ai_product_exposure_score)
      ) / sd(ai_product_exposure_score), # standardize
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


# run ---------------------------------------------------------------------
scored_occupations <- aggregate_score_to_occupation_level(
  scored_skills,
  esco_occupation_skill_mapping,
  esco_occupations,
  final_measure = "exposure_score_mean_of_max_weighted",
  remove_extra = T
)


write_csv(scored_occupations, "results/scored_esco_occupations.csv")

scored_skills %>%
  left_join(
    esco_research_skills %>%
      mutate(is_research = T) %>%
      select(conceptUri, is_research),
    by = c("esco_skill_uri"= "conceptUri")
  ) %>%
  mutate(
    is_research = ifelse(is.na(is_research), F, is_research),
    max_similarity_score = (max_similarity - mean(max_similarity))/sd(max_similarity)
  ) %>%
  #ggplot(aes(x = max_similarity_score, fill = is_research)) +
  ggplot(aes(x = max_similarity_score)) +
  geom_density(alpha = 0.5) +
  # add points on the x axis for the research skills
  geom_point(
    data = . %>% filter(is_research),
    aes(x = max_similarity_score, y = 0),
    color = "red",
    size = 2
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of skill similarity scores",
    x = "Skill similarity score",
    y = "Density"
  )

# temp --------------------------------------------------------------------
# scored_skills %>%
#   arrange(desc(max_similarity)) %>%
#   print(n = 200)
# 
# scored_occupations %>%
#   ungroup() %>%
#   group_by(isco_group) %>%
#   summarise(
#     n_occupations = n(),
#     n_skills = sum(n_skills),
#     exposure_score_mean_of_max_weighted = mean(exposure_score_mean_of_max_weighted)
#   ) %>%
#   arrange(desc(exposure_score_mean_of_max_weighted))
# 
# scored_occupations %>%
#   ungroup() %>%
#   group_by(substr(isco_group,1,2)) %>%
#   summarise(
#     n_occupations = n(),
#     n_skills = sum(n_skills),
#     exposure_score_mean_of_max_weighted = mean(exposure_score_mean_of_max_weighted)
#   ) %>%
#   arrange(desc(exposure_score_mean_of_max_weighted))
# 
# esco_occupation_skill_mapping %>% 
#   filter(
#     occupationUri == "http://data.europa.eu/esco/occupation/87d0795a-d41f-47ee-979f-0ab7d73836e7" & 
#       skillType == "skill/competence"
#   ) %>% 
#   left_join(scored_skills, by = c("skillUri" = "esco_skill_uri")) %>% 
#   select(esco_skill_label, max_similarity, max_similarity_capability)
# 
# esco_occupation_skill_mapping %>% 
#   filter(
#     occupationUri == "http://data.europa.eu/esco/occupation/24e9f120-eae8-411a-8daf-c48e8ae2f5f6" & 
#       skillType == "skill/competence"
#   ) %>% 
#   left_join(scored_skills, by = c("skillUri" = "esco_skill_uri")) %>% 
#   select(esco_skill_label, max_similarity, max_similarity_capability)
# 
# esco_occupation_skill_mapping %>%
#   left_join(scored_skills, by = c("skillUri" = "esco_skill_uri")) %>%
#   filter(!is.na(max_similarity)) %>%
#   pull(max_similarity) %>%
#   quantile(probs = seq(0,1,0.01))
# 
# scored_skills %>%
#   mutate(
#     max_max = ifelse(max_similarity > 0.95, max_similarity, 0)
#   ) %>%
#   arrange(desc(max_max)) 
