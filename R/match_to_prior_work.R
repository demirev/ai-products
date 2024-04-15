library(tidyverse)
library(haven)

webb_data <- read_csv("data/webb/exposure_by_occ1990dd_lswt2010.csv")
webb_df <- read_dta("data/webb/final_df_out.dta")
webb_crosswalk <- read_dta("data/webb/onet_to_occ1990dd.dta")
esco_crosswalk <- read_csv("data/esco/onet_esco_crosswalk.csv")
isco_groups <- read_csv("data/esco/ISCOGroups_en.csv")
felten_data <- read_csv("data/felten_et_al/appendix_A.csv")

scored_occupations <- read_csv("results/scored_esco_occupations.csv")

match_to_webb <- function(
  scored_occupations,
  webb_data,
  webb_crosswalk,
  esco_crosswalk
) {
  scored_occupations %>%
    inner_join(
      esco_crosswalk %>%
        select(
          onet_id,
          onet_title,
          esco_uri
        ),
      by = c("occupation_uri" = "esco_uri")
    ) %>%
    group_by(
      onet_id,
      onet_title
    ) %>%
    summarise(
      ai_product_exposure_score = mean(ai_product_exposure_score),
      esco_title = occupation_title %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ",, " # unique separator, so that I don't split actual commas in title below
        )
    ) %>%
    inner_join(
      webb_crosswalk %>%
        select(
          onetsoccode,
          occ1990dd,
          occ1990dd_title
        ),
      by = c("onet_id" = "onetsoccode")
    ) %>%
    group_by(
      occ1990dd,
      occ1990dd_title
    ) %>%
    summarise(
      ai_product_exposure_score = mean(ai_product_exposure_score),
      esco_title = esco_title %>%
        map(function(x) str_split(x, ",, ")[[1]]) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        )
    ) %>%
    right_join(
      webb_data,
      by = c("occ1990dd" = "occ1990dd", "occ1990dd_title" = "occ1990dd_title")
    )
}

match_from_webb <- function(
  scored_occupations,
  webb_data,
  webb_crosswalk,
  esco_crosswalk
) {
  webb_data %>%
    select(occ1990dd, occ1990dd_title, pct_ai) %>%
    left_join(
      webb_crosswalk %>%
        select(
          occ1990dd,
          occ1990dd_title,
          onetsoccode
        ),
      by = c("occ1990dd" = "occ1990dd", "occ1990dd_title" = "occ1990dd_title")
    ) %>%
    group_by(
      onetsoccode
    ) %>%
    summarise(
      pct_ai_webb = mean(pct_ai),
      occ1990dd_title_from_webb = occ1990dd_title %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ",, " # unique separator, so that I don't split actual commas in title below
        )
    ) %>%
    inner_join(
      esco_crosswalk %>%
        select(
          onet_id,
          onet_title,
          esco_uri
        ),
      by = c("onetsoccode" = "onet_id")
    ) %>%
    group_by(
      esco_uri
    ) %>%
    summarise(
      pct_ai_webb = mean(pct_ai_webb),
      matched_occ1990dd_title_from_webb = occ1990dd_title_from_webb %>%
        map(function(x) str_split(x, ",, ")[[1]]) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        ),
      matched_onet_title_from_webb = onet_title %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        ),
      matched_onet_id_from_webb = onetsoccode %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        )
    ) %>%
    inner_join(
      scored_occupations,
      by = c("esco_uri" = "occupation_uri")
    )
}

match_to_felten <- function(
  scored_occupations,
  felten_data,
  esco_crosswalk
) {
  felten_data %>%
    left_join(
      esco_crosswalk %>%
        mutate(
          soc_code = substr(onet_id, 1, 7)
        ) %>%
        select(soc_code, onet_title, esco_uri) %>%
        left_join(
          scored_occupations %>%
            select(
              esco_uri = occupation_uri,
              ai_product_exposure_score = ai_product_exposure_score
            ),
          by = c("esco_uri" = "esco_uri")
        ) %>%
        group_by(
          soc_code
        ) %>%
        summarise(
          ai_product_exposure_score = mean(ai_product_exposure_score)
        ),
      by = c("soc_code" = "soc_code")
    ) %>%
    ungroup()
}

match_from_felten <- function(
  scored_occupations,
  felten_data,
  esco_crosswalk
) {
  felten_data %>%
    inner_join(
      esco_crosswalk %>%
        mutate(
          soc_code = substr(onet_id, 1, 7)
        ) %>%
        select(soc_code, onet_title, esco_uri)
    ) %>%
    group_by(
      esco_uri
    ) %>%
    summarise(
      aioe_felten = mean(aioe),
      matched_onet_id_from_felten = soc_code %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        ),
      matched_onet_title_from_felten = onet_title %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        )
    ) %>%
    right_join(
      scored_occupations,
      by = c("esco_uri" = "occupation_uri")
    ) %>%
    select(
      everything(), 
      aioe_felten,
      matched_onet_id_from_felten, matched_onet_title_from_felten
    )
}

match_from_all <- function(
  scored_occupations,
  felten_data,
  webb_data,
  webb_crosswalk,
  esco_crosswalk
) {
  matched_felten <- scored_occupations %>%
    match_from_felten(
      felten_data,
      esco_crosswalk
    ) %>%
    select(
      esco_uri,
      felten_exposure_score = aioe_felten,
      matched_onet_title_from_felten,
      matched_onet_id_from_felten,
    )
  
  matched_webb <- scored_occupations %>%
    match_from_webb(
      webb_data,
      webb_crosswalk,
      esco_crosswalk
    ) %>%
    select(
      esco_uri,
      webb_exposure_score = pct_ai_webb,
      matched_occ1990dd_title_from_webb,
      matched_onet_title_from_webb,
      matched_onet_id_from_webb,
    )
  
  scored_occupations %>%
    left_join(
      matched_felten,
      by = c("occupation_uri" = "esco_uri")
    ) %>%
    left_join(
      matched_webb,
      by = c("occupation_uri" = "esco_uri")
    )
}

aggregate_webb_to_4digit_onet <- function(
  webb_data,
  webb_crosswalk
) {
  webb_data %>%
    inner_join(
      webb_crosswalk %>%
        select(
          occ1990dd,
          onet_id = onetsoccode
        ),
      by = c("occ1990dd" = "occ1990dd")
    ) %>%
    mutate(
      onet_4digit = substr(onet_id, 1, 5) # e.g. 11-1011.03 to 11-10
    ) %>%
    group_by(
      onet_4digit
    ) %>%
    summarise(
      webb_exposure_score = mean(pct_ai)
    )
}

aggregate_felten_to_4digit_onet <- function(
  felten_data  
) {
  felten_data %>%
    mutate(
      onet_4digit = substr(soc_code, 1, 5) # e.g. 11-1011.03 to 11-10
    ) %>%
    group_by(
      onet_4digit
    ) %>%
    summarise(
      felten_exposure_score = mean(aioe)
    )
}

aggregate_occupations_to_3digit_isco <- function(
  scored_occupations
) {
  scored_occupations %>%
    mutate(
      isco_3digit = substr(isco_group, 1, 3)
    ) %>%
    group_by(
      isco_3digit
    ) %>%
    summarise(
      ai_product_exposure_score = mean(ai_product_exposure_score)
    )
}

aggregate_all_to_3digit_isco <- function(
  scored_occupations,
  felten_data, webb_data, webb_crosswalk, esco_crosswalk
) {
  # get the most common onet_4digit for each isco_3digit
  digit3_crosswalk <- esco_crosswalk %>%
    left_join(
      scored_occupations %>%
        select(
          occupation_uri,
          isco_group
        ),
      by = c("esco_uri" = "occupation_uri")
    ) %>%
    mutate(
      onet_4digit = substr(onet_id, 1, 5),
      isco_3digit = substr(isco_group, 1, 3)
    ) %>%
    group_by(
      isco_3digit
    ) %>%
    summarise(
      onet_4digit = onet_4digit %>%
        table() %>%
        sort(decreasing = TRUE) %>%
        names() %>%
        .[1]
    )
  
  scored_occupations %>%
    aggregate_occupations_to_3digit_isco() %>%
    left_join(
      digit3_crosswalk %>%
        select(
          isco_3digit,
          onet_4digit
        ),
      by = c("isco_3digit" = "isco_3digit")
    ) %>%
    left_join(
      aggregate_webb_to_4digit_onet(
        webb_data,
        webb_crosswalk
      ),
      by = c("onet_4digit" = "onet_4digit")
    ) %>%
    left_join(
      aggregate_felten_to_4digit_onet(
        felten_data
      ),
      by = c("onet_4digit" = "onet_4digit")
    )
}

# run ---------------------------------------------------------------------
scored_occupations_matched <- scored_occupations %>%
  match_from_all(
    felten_data,
    webb_data,
    webb_crosswalk,
    esco_crosswalk
  )

scored_groups_matched <- aggregate_all_to_3digit_isco(
  scored_occupations_matched,
  felten_data, webb_data, webb_crosswalk, esco_crosswalk
) %>%
  arrange(desc(ai_product_exposure_score))

scored_groups_matched <- scored_groups_matched %>%
  left_join(
    isco_groups %>%
      select(
        code,
        group_label = preferredLabel
      ) %>%
      filter(nchar(code) == 3),
    by = c("isco_3digit" = "code")
  ) %>%
  select(isco_3digit, group_label, everything())

write_csv(
  scored_occupations_matched,
  "results/scored_esco_occupations_matched.csv"
)

write_csv(
  scored_groups_matched,
  "results/scored_esco_3_digit_groups.csv"
)

# temp --------------------------------------------------------------------
cor(
  scored_groups_matched$felten_exposure_score, 
  scored_groups_matched$webb_exposure_score,
  use = "complete.obs"
)

cor(
  scored_groups_matched$ai_product_exposure_score, 
  scored_groups_matched$webb_exposure_score,
  use = "complete.obs"
)

cor(
  scored_groups_matched$ai_product_exposure_score, 
  scored_groups_matched$felten_exposure_score,
  use = "complete.obs"
)

plot(
  scored_groups_matched$ai_product_exposure_score,
  scored_groups_matched$felten_exposure_score
)

scored_occupations_plus_webb %>%
  filter(
    pct_ai > 80 &
      exposure_score_mean_of_max_weighted < 
      quantile(exposure_score_mean_of_max_weighted, 0.2)
  ) %>%
  arrange(desc(pct_ai)) %>%
  select(
    occupation_title,
    exposure_score_mean_of_max_weighted,
    pct_ai
  ) %>%
  print(n = Inf)

scored_occupations_plus_webb %>%
  filter(
    pct_ai < 20 &
      exposure_score_mean_of_max_weighted > 
      quantile(exposure_score_mean_of_max_weighted, 0.8)
  ) %>%
  arrange(desc(exposure_score_mean_of_max_weighted)) %>%
  select(
    occupation_title,
    exposure_score_mean_of_max_weighted,
    pct_ai
  ) %>%
  print(n = Inf)

scored_occupations_plus_webb %>%
  filter(
    pct_ai > 80 &
      exposure_score_mean_of_max_weighted > 
      quantile(exposure_score_mean_of_max_weighted, 0.8)
  ) %>%
  arrange(desc(exposure_score_mean_of_max_weighted)) %>%
  select(
    occupation_title,
    exposure_score_mean_of_max_weighted,
    pct_ai
  ) %>%
  print(n = Inf)

scored_occupations_plus_webb %>%
  filter(
    pct_ai < 20 &
      exposure_score_mean_of_max_weighted < 
      quantile(exposure_score_mean_of_max_weighted, 0.2)
  ) %>%
  arrange(desc(exposure_score_mean_of_max_weighted)) %>%
  select(
    occupation_title,
    exposure_score_mean_of_max_weighted,
    pct_ai
  ) %>%
  print(n = Inf)
