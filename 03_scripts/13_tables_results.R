#### ..................................................................... ####
#### Main results of the PPMLE ####
#### ..................................................................... ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(kableExtra)
library(scales)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Ensure output directory exists ####
dir.create("04_output/tables/", showWarnings = FALSE, recursive = TRUE)

#### Shared term labels ####
term_labels <- c(
  "imbalance"           = "Imbalance",
  "log(dist)"           = "Distance (log)",
  "log(gdp_pair)"       = "GDP Pair (log)",
  "eu_o"                = "EU Origin",
  "eu_d"                = "EU Destination",
  "contig"              = "Contiguous",
  "diplo_disagreement"  = "Diplomatic Disagreement",
  "comlang_off"         = "Common language (official)",
  "comcol"              = "Colonial Relationship",
  "fta_wto"             = "Free Trade Agreement (WTO)"
)

#### Shared descriptive stat labels ####
desc_labels <- c(
  "n.obs"     = "No. Observations",
  "n.pairs"   = "No. Country-Pairs",
  "n.periods" = "No. Periods"
)

#### Helper: make a midrule separator row (3-column version) ####
make_sep3 <- function() {
  tibble(imb = NA_character_, Variable = "\\midrule",
         raw = NA_character_, gravity = NA_character_, twfe = NA_character_)
}

#### Helper: build a 3-column (raw/gravity/twfe) formatted table ####
build_pooled_table <- function(est) {

  est <- est %>%
    mutate(spec = factor(spec, levels = c("raw", "gravity", "twfe"))) %>%
    mutate(
      sig = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE           ~ ""
      ),
      estimate_f  = sprintf("%.3f%s", estimate, sig),
      std.error_f = sprintf("(%.3f)", std.error),
      n.obs       = as.character(n.obs),
      n.pairs     = as.character(n.pairs),
      n.periods   = as.character(n.periods)
    ) %>%
    mutate(term = factor(term, levels = c("imbalance", setdiff(unique(term), "imbalance"))))

  # Long then wide: estimates and SEs
  wide_main <- est %>%
    select(imb, term, spec, estimate_f, std.error_f) %>%
    pivot_longer(cols = c("estimate_f", "std.error_f"),
                 names_to = "stat_type", values_to = "stat_value") %>%
    pivot_wider(names_from = spec, values_from = stat_value)

  # Descriptive stats
  descriptives <- est %>%
    distinct(imb, spec, n.obs, n.pairs, n.periods) %>%
    pivot_longer(cols = c("n.obs", "n.pairs", "n.periods"),
                 names_to = "desc_type", values_to = "desc_value") %>%
    pivot_wider(names_from = spec, values_from = desc_value) %>%
    mutate(desc_type = if_else(desc_type %in% names(desc_labels),
                               desc_labels[desc_type], desc_type))

  wide_main[is.na(wide_main)] <- "-"
  descriptives[is.na(descriptives)] <- "-"

  # Two-row structure (estimate + SE)
  est_df <- wide_main %>% filter(stat_type == "estimate_f") %>% select(-stat_type)
  se_df  <- wide_main %>% filter(stat_type == "std.error_f") %>% select(-stat_type)
  combined <- left_join(est_df, se_df, by = c("imb", "term"), suffix = c("_est", "_se"))
  combined[is.na(combined)] <- "-"
  combined$term <- as.character(combined$term)
  combined <- combined %>%
    mutate(term = if_else(term %in% names(term_labels), term_labels[term], term))

  rows <- combined %>%
    rename(Variable = term) %>%
    mutate(row_id = row_number()) %>%
    select(imb, Variable, raw_est, gravity_est, twfe_est, raw_se, gravity_se, twfe_se, row_id)

  expanded <- rows %>%
    transmute(imb, Variable,
              raw = raw_est, gravity = gravity_est, twfe = twfe_est,
              row_id, line_type = "estimate") %>%
    bind_rows(
      rows %>%
        transmute(imb, Variable = "",
                  raw = raw_se, gravity = gravity_se, twfe = twfe_se,
                  row_id, line_type = "stderr")
    ) %>%
    arrange(row_id, line_type)

  bind_rows(
    expanded %>% select(-row_id, -line_type),
    make_sep3(),
    descriptives %>% rename(Variable = desc_type)
  )
}

#### ..................................................................... ####
#### Table 4: Pooled PPMLE ####
#### ..................................................................... ####

est_pooled <- read_rds("02_gen/04_results/poisson_pooled.rds")

out_pooled <- build_pooled_table(est_pooled)

out_pooled %>%
  filter(imb == "net_imb_f1011" | is.na(imb)) %>%
  select(-imb) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Relative Material Imbalance and Waste Trade (PPMLE)",
        label = "tab:ppmle_pooled",
        col.names = c("", "Raw", "Gravity", "TWFE"),
        align = "lccc") %>%
  kable_styling(latex_options = c("hold_position"), position = "center") %>%
  save_kable("04_output/tables/table4_ppmle_pooled.tex")

#### ..................................................................... ####
#### Table A4: EU-restricted PPMLE (post-2006, EU origin only) ####
#### ..................................................................... ####

est_eu <- read_rds("02_gen/04_results/poisson_pooled_post_2006_only_eu.rds")

out_eu <- build_pooled_table(est_eu)

out_eu %>%
  filter(imb == "net_imb_f1011" | is.na(imb)) %>%
  select(-imb) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "EU-Restricted PPMLE (Post-2006, EU Origin Only)",
        label = "tab:ppmle_eu",
        col.names = c("", "Raw", "Gravity", "TWFE"),
        align = "lccc") %>%
  kable_styling(latex_options = c("hold_position"), position = "center") %>%
  save_kable("04_output/tables/tableA4_eu_restricted.tex")

#### ..................................................................... ####
#### Table 5: PPMLE split by product (TWFE only) ####
#### ..................................................................... ####

est_split <- read_rds("02_gen/04_results/poisson_split.rds")

est_split <- est_split %>%
  filter(spec == "twfe") %>%
  mutate(
    prod_class = factor(prod_class,
                        levels = c("aluminum", "glass", "iron_steel", "paper", "plastic")),
    sig = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE           ~ ""
    ),
    estimate_f  = sprintf("%.3f%s", estimate, sig),
    std.error_f = sprintf("(%.3f)", std.error),
    n.obs       = as.character(n.obs),
    n.pairs     = as.character(n.pairs),
    n.periods   = as.character(n.periods)
  ) %>%
  mutate(term = factor(term, levels = c("imbalance", setdiff(unique(term), "imbalance"))))

# Long then wide on prod_class
wide_split <- est_split %>%
  select(imb, term, prod_class, estimate_f, std.error_f) %>%
  pivot_longer(cols = c("estimate_f", "std.error_f"),
               names_to = "stat_type", values_to = "stat_value") %>%
  pivot_wider(names_from = prod_class, values_from = stat_value)

# Descriptive stats
desc_split <- est_split %>%
  distinct(imb, prod_class, n.obs, n.pairs, n.periods) %>%
  pivot_longer(cols = c("n.obs", "n.pairs", "n.periods"),
               names_to = "desc_type", values_to = "desc_value") %>%
  pivot_wider(names_from = prod_class, values_from = desc_value) %>%
  rename(Aluminum = aluminum, Glass = glass,
         `Iron and Steel` = iron_steel, Paper = paper, Plastic = plastic) %>%
  mutate(desc_type = if_else(desc_type %in% names(desc_labels),
                             desc_labels[desc_type], desc_type))

wide_split[is.na(wide_split)] <- "-"
desc_split[is.na(desc_split)] <- "-"

# Two-row structure
est_s <- wide_split %>% filter(stat_type == "estimate_f") %>% select(-stat_type)
se_s  <- wide_split %>% filter(stat_type == "std.error_f") %>% select(-stat_type)
comb_s <- left_join(est_s, se_s, by = c("imb", "term"), suffix = c("_est", "_se"))
comb_s[is.na(comb_s)] <- "-"
comb_s$term <- as.character(comb_s$term)
comb_s <- comb_s %>%
  mutate(term = if_else(term %in% names(term_labels), term_labels[term], term))

rows_s <- comb_s %>%
  rename(Variable = term) %>%
  mutate(row_id = row_number()) %>%
  select(imb, Variable,
         aluminum_est, glass_est, iron_steel_est, paper_est, plastic_est,
         aluminum_se,  glass_se,  iron_steel_se,  paper_se,  plastic_se,
         row_id)

expanded_s <- rows_s %>%
  transmute(imb, Variable,
            Aluminum = aluminum_est, Glass = glass_est,
            `Iron and Steel` = iron_steel_est, Paper = paper_est, Plastic = plastic_est,
            row_id, line_type = "estimate") %>%
  bind_rows(
    rows_s %>%
      transmute(imb, Variable = "",
                Aluminum = aluminum_se, Glass = glass_se,
                `Iron and Steel` = iron_steel_se, Paper = paper_se, Plastic = plastic_se,
                row_id, line_type = "stderr")
  ) %>%
  arrange(row_id, line_type)

make_sep_split <- function() {
  tibble(imb = NA_character_, Variable = "\\midrule",
         Aluminum = NA_character_, Glass = NA_character_,
         `Iron and Steel` = NA_character_, Paper = NA_character_, Plastic = NA_character_)
}

out_split <- bind_rows(
  expanded_s %>% select(-row_id, -line_type),
  make_sep_split(),
  desc_split %>% rename(Variable = desc_type)
)

out_split %>%
  filter(imb == "net_imb_f1011" | is.na(imb)) %>%
  select(-imb) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Heterogeneous Effects by Material (PPMLE, TWFE)",
        label = "tab:ppmle_split",
        col.names = c("", "Aluminum", "Glass", "Iron \\& Steel", "Paper", "Plastic"),
        align = "lccccc") %>%
  kable_styling(latex_options = c("hold_position"), position = "center") %>%
  save_kable("04_output/tables/table5_ppmle_split.tex")
