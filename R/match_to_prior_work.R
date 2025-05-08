library(tidyverse)
library(haven)
library(argparse)
library(showtext)
library(sysfonts)

# define script arguments -------------------------------------------------
parser <- ArgumentParser()

parser$add_argument(
  "--webb_data", 
  type = "character",
  help = "Path to output from Webb 2022", 
  default = "data/webb/exposure_by_occ1990dd_lswt2010.csv"
)
parser$add_argument(
  "--webb_additional",
  type = "character",
  help = "Path to additional Webb 2022 data",
  default = "data/webb/final_df_out.dta"
)
parser$add_argument(
  "--webb_crosswalk", 
  type = "character",
  help = "Path to crosswalk between Webb 2022 and occ1990dd", 
  default = "data/webb/onet_to_occ1990dd.dta"
)
parser$add_argument(
  "--eloundou_data",
  type = "character",
  help = "Path to Eloundou et al. data",
  default = "data/eloundou_et_al/full_labelset.tsv"
)
parser$add_argument(
  "--esco_crosswalk", 
  type = "character",
  help = "Path to crosswalk between ESCO and ONET", 
  default = "data/esco/onet_esco_crosswalk.csv"
)
parser$add_argument(
  "--isco_groups", 
  type = "character",
  help = "Path to ISCO groups", 
  default = "data/esco/ISCOGroups_en.csv"
)
parser$add_argument(
  "--felten_data", 
  type = "character",
  help = "Path to Felten et al. data", 
  default = "data/felten_et_al/appendix_A.csv"
)
parser$add_argument(
  "--scored_occupations", 
  type = "character",
  help = "Path to scored ESCO occupations", 
  default = "results/occupational_exposure_to_ai_products/scored_esco_occupations.csv"
)
parser$add_argument(
  "--output_dir", 
  type = "character",
  help = "Path to output file",
  default = "results"
)

font_add_google("Merriweather", "merriweather")
showtext_auto()

# functions ---------------------------------------------------------------
convert_to_soc <- function(
  scored_occupations
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
      ai_product_automation_score = mean(ai_product_automation_score),
      ai_product_augmentation_score = mean(ai_product_augmentation_score),
      esco_title = occupation_title %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ",, " # unique separator, so that I don't split actual commas in title below
        )
    )
}

match_to_webb <- function(
  scored_occupations,
  webb_data,
  webb_crosswalk,
  esco_crosswalk
) {
  scored_occupations %>%
    convert_to_soc() %>%
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
  
  # scored_occupations %>%
  #   convert_to_soc() %>%
  #   right_join(
  #     webb_alt,
  #     by = c("onet_id" = "onetsoccode")
  #   )
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

match_to_eloundou <- function(
  scored_occupations,
  eloundou_data,
  esco_crosswalk,
  onet_weights_file = "data/eloundou_et_al/task_weights.tsv"
) {
  
  onet_weights <- read_tsv(onet_weights_file) %>%
    filter(`Scale ID` == "IM") %>% # importance weights
    select(
      task_id = `Task ID`,
      weight = `Data Value`
    )
  
  names(eloundou_data)[1:6] <- c(
    "n", "soc_code",
    "task_id", "task", "task_type",
    "title"
  )
  
  eloundou_data %>%
    left_join(
      onet_weights,
      by = c("task_id" = "task_id")
    ) %>%
    mutate(
      weight = ifelse(
        is.na(weight),
        mean(weight, na.rm = TRUE),
        weight
      ) # some SOC codes have no weights - assume equal weighting
    ) %>%
    group_by(
      soc_code,
      title
    ) %>%
    summarise(
      beta_eloundou = weighted.mean(beta, weight)
    ) %>%
    left_join(
      esco_crosswalk %>%
        mutate(
          soc_code = onet_id #substr(onet_id, 1, 7)
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

match_from_eloundou <- function(
  scored_occupations,
  eloundou_data,
  esco_crosswalk,
  onet_weights_file = "data/eloundou_et_al/task_weights.tsv"
) {
  names(eloundou_data)[1:6] <- c(
    "n", "soc_code",
    "task_id", "task", "task_type",
    "title"
  )
  
  onet_weights <- read_tsv(onet_weights_file) %>%
    filter(`Scale ID` == "IM") %>% # importance weights
    select(
      task_id = `Task ID`,
      weight = `Data Value`
    )
  
  eloundou_data %>%
    left_join(
      onet_weights,
      by = c("task_id" = "task_id")
    ) %>%
    mutate(
      weight = ifelse(
        is.na(weight),
        mean(weight, na.rm = TRUE),
        weight
      ) # some SOC codes have no weights - assume equal weighting
    ) %>%
    group_by(
      soc_code,
      title
    ) %>%
    summarise(
      beta_eloundou = weighted.mean(beta, weight)
    ) %>%
    left_join(
      esco_crosswalk %>%
        mutate(
          soc_code = onet_id #substr(onet_id, 1, 7)
        ) %>%
        select(soc_code, onet_title, esco_uri)
    ) %>%
    group_by(
      esco_uri
    ) %>%
    summarise(
      beta_eloundou = mean(beta_eloundou),
      matched_onet_id_from_eloundou = soc_code %>%
        unique() %>%
        sort() %>%
        paste0(
          collapse = ", "
        ),
      matched_onet_title_from_eloundou = onet_title %>%
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
      beta_eloundou,
      matched_onet_id_from_eloundou, matched_onet_title_from_eloundou
    )
}    


match_from_all <- function(
  scored_occupations,
  felten_data,
  webb_data,
  eloundou_data,
  webb_crosswalk,
  esco_crosswalk,
  onet_weights_file = "data/eloundou_et_al/task_weights.tsv"
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
  
  matched_eloundou <- scored_occupations %>%
    match_from_eloundou(
      eloundou_data,
      esco_crosswalk,
      onet_weights_file
    ) %>%
    select(
      esco_uri,
      beta_eloundou,
      matched_onet_title_from_eloundou,
      matched_onet_id_from_eloundou
    )
  
  scored_occupations %>%
    left_join(
      matched_felten,
      by = c("occupation_uri" = "esco_uri")
    ) %>%
    left_join(
      matched_webb,
      by = c("occupation_uri" = "esco_uri")
    ) %>%
    left_join(
      matched_eloundou,
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

aggregate_eloundou_to_4digit_onet <- function(
  eloundou_data,
  onet_weights_file = "data/eloundou_et_al/task_weights.tsv"
) {
  
  names(eloundou_data)[1:6] <- c(
    "n", "soc_code",
    "task_id", "task", "task_type",
    "title"
  )
  
  onet_weights <- read_tsv(onet_weights_file) %>%
    filter(`Scale ID` == "IM") %>% # importance weights
    select(
      task_id = `Task ID`,
      weight = `Data Value`
    )
  
  eloundou_data <- eloundou_data %>%
    left_join(
      onet_weights,
      by = c("task_id" = "task_id")
    ) %>%
    mutate(
      weight = ifelse(
        is.na(weight),
        mean(weight, na.rm = TRUE),
        weight
      ) # some SOC codes have no weights - assume equal weighting
    ) %>%
    group_by(
      soc_code,
      title
    ) %>%
    summarise(
      beta_eloundou = weighted.mean(beta, weight)
    ) 
  
  eloundou_data %>%
    mutate(
      onet_4digit = substr(soc_code, 1, 5) # e.g. 11-1011.03 to 11-10
    ) %>%
    group_by(
      onet_4digit
    ) %>%
    summarise(
      beta_eloundou = mean(beta_eloundou)
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
      ai_product_exposure_score = mean(ai_product_exposure_score),
      ai_product_automation_score = mean(ai_product_automation_score),
      ai_product_augmentation_score = mean(ai_product_augmentation_score)
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
    ) %>%
    left_join(
      aggregate_eloundou_to_4digit_onet(
        eloundou_data
      ),
      by = c("onet_4digit" = "onet_4digit")
    )
}


# read data ---------------------------------------------------------------
args <- parser$parse_args()

webb_data <- read_csv(args$webb_data)
webb_alt <- read_dta(args$webb_additional)
webb_crosswalk <- read_dta(args$webb_crosswalk)
esco_crosswalk <- read_csv(args$esco_crosswalk)
isco_groups <- read_csv(args$isco_groups)
felten_data <- read_csv(args$felten_data)
eloundou_data <- read_tsv(args$eloundou_data)

scored_occupations <- read_csv(args$scored_occupations)

# run ---------------------------------------------------------------------
scored_occupations_matched <- scored_occupations %>%
  match_from_all(
    felten_data,
    webb_data,
    eloundou_data,
    webb_crosswalk,
    esco_crosswalk
  )

scored_groups_matched <- aggregate_all_to_3digit_isco(
  scored_occupations_matched,
  felten_data, webb_data, webb_crosswalk, esco_crosswalk
) %>%
  arrange(desc(ai_product_exposure_score))

soc_equivalent <- scored_occupations %>%
  convert_to_soc()

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

# calculate pairwise correlations ----------------------------------------
# main exposure score
cor.test(
  scored_occupations_matched$ai_product_exposure_score,
  scored_occupations_matched$felten_exposure_score,
  method = "pearson"
)

cor.test(
  scored_occupations_matched$ai_product_exposure_score,
  scored_occupations_matched$webb_exposure_score,
  method = "pearson"
)

cor.test(
  scored_occupations_matched$ai_product_exposure_score,
  scored_occupations_matched$beta_eloundou,
  method = "pearson"
)

correlations <- scored_occupations_matched %>%
  select(
    ai_product_exposure_score,
    felten_exposure_score,
    webb_exposure_score,
    beta_eloundou
  ) %>%
  as.matrix() %>%
  Hmisc::rcorr() %>%
  (function(x) {
    tibble(
      var1 = rep(colnames(x$r), each = ncol(x$r)),
      var2 = rep(colnames(x$r), ncol(x$r)),
      r = as.vector(x$r),
      p = as.vector(x$P),
      n = as.vector(x$n)
    )
  }) %>%
  filter(var1 != var2) %>%
  filter(!duplicated(r))

# automation score
cor.test(
  scored_occupations_matched$ai_product_automation_score,
  scored_occupations_matched$felten_exposure_score,
  method = "pearson"
)

cor.test(
  scored_occupations_matched$ai_product_automation_score,
  scored_occupations_matched$webb_exposure_score,
  method = "pearson"
)

cor.test(
  scored_occupations_matched$ai_product_automation_score,
  scored_occupations_matched$beta_eloundou,
  method = "pearson"
)

# augmentation score
cor.test(
  scored_occupations_matched$ai_product_augmentation_score,
  scored_occupations_matched$felten_exposure_score,
  method = "pearson"
)

cor.test(
  scored_occupations_matched$ai_product_augmentation_score,
  scored_occupations_matched$webb_exposure_score,
  method = "pearson"
)

cor.test(
  scored_occupations_matched$ai_product_augmentation_score,
  scored_occupations_matched$beta_eloundou,
  method = "pearson"
)

# group exposure score
cor.test(
  scored_groups_matched$ai_product_exposure_score,
  scored_groups_matched$felten_exposure_score,
  method = "pearson"
)

cor.test(
  scored_groups_matched$ai_product_exposure_score,
  scored_groups_matched$webb_exposure_score,
  method = "pearson"
)

cor.test(
  scored_groups_matched$ai_product_exposure_score,
  scored_groups_matched$beta_eloundou,
  method = "pearson"
)

correlations_grouped <- scored_groups_matched %>%
  select(
    ai_product_exposure_score,
    felten_exposure_score,
    webb_exposure_score,
    beta_eloundou
  ) %>%
  as.matrix() %>%
  Hmisc::rcorr() %>%
  (function(x) {
    tibble(
      var1 = rep(colnames(x$r), each = ncol(x$r)),
      var2 = rep(colnames(x$r), ncol(x$r)),
      r = as.vector(x$r),
      p = as.vector(x$P),
      n = as.vector(x$n)
    )
  }) %>%
  filter(var1 != var2) %>%
  filter(!duplicated(r))

# plots -------------------------------------------------------------------
ai_product_score_means <- list(
  exposure = mean(
    scored_occupations_matched$ai_product_exposure_score, na.rm = TRUE
  ), # 0.109345
  automation = mean(
    scored_occupations_matched$ai_product_automation_score, na.rm = TRUE
  ), # 0.04985087
  augmentation = mean(
    scored_occupations_matched$ai_product_augmentation_score, na.rm = TRUE
  ) # 0.04115786
)

# ggplot of ai_product_exposure_score vs felten_exposure_score
vs_felten_plot <- scored_occupations_matched %>%
  ggplot(aes(
    y = ai_product_exposure_score,
    x = felten_exposure_score
  )) +
  geom_point(
    # dark grey points
    color = "darkgray"
  ) +
  geom_smooth(method = "lm") +
  labs(
    y = "AI Product Exposure Score",
    x = "Felten Exposure Score"
  ) +
  # add gray dashed lines at x = 0 and y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = ai_product_score_means$exposure, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_felten_plot_grouped <- scored_groups_matched %>%
  ggplot(aes(
    y = ai_product_exposure_score,
    x = felten_exposure_score
  )) +
  geom_point(
    # dark grey points
    color = "darkgray"
  ) +
  geom_smooth(method = "lm") +
  labs(
    y = "AI Product Exposure Score",
    x = "Felten Exposure Score"
  ) +
  # add gray dashed lines at x = 0 and y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = ai_product_score_means$exposure, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

# examples pf differences with felten
list(
  # by group
  high_felten_low_product_exposure_groups = scored_groups_matched %>%
    filter(
      felten_exposure_score > 0,
      #ai_product_exposure_score < ai_product_score_means$exposure
      ai_product_automation_score < ai_product_score_means$automation,
      ai_product_augmentation_score < ai_product_score_means$augmentation,
    ) %>%
    select(
      isco_3digit,
      group_label,
      felten_exposure_score,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(felten_exposure_score)),
  low_felten_high_product_exposure_groups = scored_groups_matched %>%
    filter(
      felten_exposure_score < 0,
      #ai_product_exposure_score > ai_product_score_means$exposure
      ai_product_automation_score > ai_product_score_means$automation,
      ai_product_augmentation_score > ai_product_score_means$augmentation,
    ) %>%
    select(
      isco_3digit,
      group_label,
      felten_exposure_score,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(ai_product_exposure_score)),
  # by occupation
  high_felten_low_product_exposure = scored_occupations_matched %>%
    filter(
      felten_exposure_score > 0,
      #ai_product_exposure_score < ai_product_score_means$exposure
      ai_product_automation_score < ai_product_score_means$automation,
      ai_product_augmentation_score < ai_product_score_means$augmentation,
    ) %>%
    select(
      occupation_title,
      isco_group,
      felten_exposure_score,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(felten_exposure_score)),
  low_felten_high_product_exposure = scored_occupations_matched %>%
    filter(
      felten_exposure_score < 0,
      #ai_product_exposure_score > ai_product_score_means$exposure
      ai_product_automation_score > ai_product_score_means$automation,
      ai_product_augmentation_score > ai_product_score_means$augmentation,
    ) %>%
    select(
      occupation_title,
      isco_group,
      felten_exposure_score,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(ai_product_exposure_score))
)

vs_eloundou_plot <- scored_occupations_matched %>%
  ggplot(aes(
    y = ai_product_exposure_score,
    x = beta_eloundou
  )) +
  geom_point(
    color = "darkgray"
  ) +
  geom_smooth(method = "lm") +
  labs(
    y = "AI Product Exposure Score",
    x = "Eloundou Exposure Score"
  ) +
  # add gray dashed lines at x = 0 and y = 0
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_hline(
    yintercept = ai_product_score_means$exposure, 
    linetype = "dashed", 
    color = "gray"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_eloundou_plot_grouped <- scored_groups_matched %>%
  ggplot(aes(
    y = ai_product_exposure_score,
    x = beta_eloundou
  )) +
  geom_point(
    color = "darkgray"
  ) +
  geom_smooth(method = "lm") +
  labs(
    y = "AI Product Exposure Score",
    x = "Eloundou Exposure Score"
  ) +
  # add gray dashed lines at x = 0 and y = 0
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = ai_product_score_means$exposure, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

# examples of differences with eloundou
list(
  # by group
  high_eloundou_low_product_exposure_groups = scored_groups_matched %>%
    filter(
      beta_eloundou > 0.5,
      #ai_product_exposure_score < ai_product_score_means$exposure
      ai_product_automation_score < ai_product_score_means$automation,
      ai_product_augmentation_score < ai_product_score_means$augmentation,
    ) %>%
    select(
      isco_3digit,
      group_label,
      beta_eloundou,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(beta_eloundou)),
  low_eloundou_high_product_exposure_groups = scored_groups_matched %>%
    filter(
      beta_eloundou < 0.5,
      #ai_product_exposure_score > ai_product_score_means$exposure
      ai_product_automation_score > ai_product_score_means$automation,
      ai_product_augmentation_score > ai_product_score_means$augmentation,
    ) %>%
    select(
      isco_3digit,
      group_label,
      beta_eloundou,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(ai_product_exposure_score)),
  # by occupation
  high_eloundou_low_product_exposure = scored_occupations_matched %>%
    filter(
      beta_eloundou > 0.5,
      #ai_product_exposure_score < ai_product_score_means$exposure
      ai_product_automation_score < ai_product_score_means$automation,
      ai_product_augmentation_score < ai_product_score_means$augmentation,
    ) %>%
    select(
      occupation_title,
      isco_group,
      beta_eloundou,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(beta_eloundou)),
  low_eloundou_high_product_exposure = scored_occupations_matched %>%
    filter(
      beta_eloundou < 0.5,
      #ai_product_exposure_score > ai_product_score_means$exposure
      ai_product_automation_score > ai_product_score_means$automation,
      ai_product_augmentation_score > ai_product_score_means$augmentation,
    ) %>%
    select(
      occupation_title,
      isco_group,
      beta_eloundou,
      ai_product_exposure_score,
      ai_product_automation_score,
      ai_product_augmentation_score
    ) %>%
    arrange(desc(ai_product_exposure_score))
)

vs_webb_plot <- scored_occupations_matched %>%
  ggplot(aes(
    y = ai_product_exposure_score,
    x = webb_exposure_score
  )) +
  geom_point(
    color = "darkgray"
  ) +
  geom_smooth(method = "lm") +
  labs(
    y = "AI Product Exposure Score",
    x = "Webb Exposure Score"
  ) +
  # add gray dashed lines at x = 0 and y = 0
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = ai_product_score_means$exposure, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

vs_webb_plot_grouped <- scored_groups_matched %>%
  ggplot(aes(
    y = ai_product_exposure_score,
    x = webb_exposure_score
  )) +
  geom_point(
    color = "darkgray"
  ) +
  geom_smooth(method = "lm") +
  labs(
    y = "AI Product Exposure Score",
    x = "Webb Exposure Score"
  ) +
  # add gray dashed lines at x = 0 and y = 0
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = ai_product_score_means$exposure, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

# examples of differences with webb
list(
  # by group
  high_webb_low_product_exposure_groups = scored_groups_matched %>%
    filter(
      webb_exposure_score > 50,
      ai_product_exposure_score < ai_product_score_means$exposure
    ) %>%
    select(
      isco_3digit,
      group_label,
      ai_product_exposure_score,
      webb_exposure_score
    ) %>%
    arrange(desc(webb_exposure_score)),
  low_webb_high_product_exposure_groups = scored_groups_matched %>%
    filter(
      webb_exposure_score < 50,
      ai_product_exposure_score > ai_product_score_means$exposure
    ) %>%
    select(
      isco_3digit,
      group_label,
      ai_product_exposure_score,
      webb_exposure_score
    ) %>%
    arrange(ai_product_exposure_score),
  # by occupation
  high_webb_low_product_exposure = scored_occupations_matched %>%
    filter(
      webb_exposure_score > 50,
      ai_product_exposure_score < ai_product_score_means$exposure
    ) %>%
    select(
      occupation_title,
      isco_group,
      ai_product_exposure_score,
      webb_exposure_score
    ) %>%
    arrange(desc(webb_exposure_score)),
  low_webb_high_product_exposure = scored_occupations_matched %>%
    filter(
      webb_exposure_score < 50,
      ai_product_exposure_score > ai_product_score_means$exposure
    ) %>%
    select(
      occupation_title,
      isco_group,
      ai_product_exposure_score,
      webb_exposure_score
    ) %>%
    arrange(ai_product_exposure_score)
)

# write results -----------------------------------------------------------
write_csv(
  scored_occupations_matched,
  file.path(
    args$output_dir,
    "occupational_exposure_to_ai_products",
    "scored_esco_occupations_matched.csv"
  )
)

# write_csv(
#   scored_groups_matched,
#   "results/occupational_exposure_to_ai_products/scored_esco_occupations_isco_3_digit_matched.csv"
# )

write_csv(
  soc_equivalent,
  file.path(
    args$output_dir,
    "occupational_exposure_to_ai_products",
    "scored_soc_equivalent_occupations.csv"
  )
)

ggsave(
  file.path(
    args$output_dir,
    "plots",
    "ai_product_exposure_score_vs_felten_exposure_score.eps"
  ),
  vs_felten_plot,
  width = 9,
  height = 5,
  device = cairo_ps
)

ggsave(
  file.path(
    args$output_dir,
    "plots",
    "ai_product_exposure_score_vs_eloundou_exposure_score.eps"
  ),
  vs_eloundou_plot,
  width = 9,
  height = 5,
  device = cairo_ps
)

ggsave(
  file.path(
    args$output_dir,
    "plots",
    "ai_product_exposure_score_vs_webb_exposure_score.eps"
  ),
  vs_webb_plot,
  width = 9,
  height = 5,
  device = cairo_ps
)


ggsave(
  file.path(
    args$output_dir,
    "plots",
    "ai_product_exposure_score_vs_felten_exposure_score_grouped.eps"
  ),
  vs_felten_plot_grouped,
  width = 9,
  height = 5,
  device = cairo_ps
)

ggsave(
  file.path(
    args$output_dir,
    "plots",
    "ai_product_exposure_score_vs_eloundou_exposure_score_grouped.eps"
  ),
  vs_eloundou_plot_grouped,
  width = 9,
  height = 5,
  device = cairo_ps
)

ggsave(
  file.path(
    args$output_dir,
    "plots",
    "ai_product_exposure_score_vs_webb_exposure_score_grouped.eps"
  ),
  vs_webb_plot_grouped,
  width = 9,
  height = 5,
  device = cairo_ps
)
