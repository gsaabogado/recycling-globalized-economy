#### ..................................................................... #### 
#### Main results of the ZIP (glmmTMB) ####
#### ..................................................................... ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(kableExtra)
library(scales)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load your regression results data frame ####
est = read_rds("02_gen/04_results/zip_pooled_glmm.rds")

#### Prepare and clean the data ####
est <- est %>%
  # Order specifications
  mutate(specification = factor(specification, levels = c("raw", "gravity", "twfe", "trend")),
         model = factor(model, levels = c("conditional", "zero_inflated"))) %>%
  # Add significance stars
  mutate(sig = case_when(
    p.value < 0.02 ~ '***',
    p.value < 0.05 ~ '**',
    p.value < 0.1  ~ '*',
    TRUE           ~ ''),
  estimate_f = sprintf("%.3f%s", estimate, sig),
  std.error_f = sprintf("(%.3f)", boot_se)) %>%
  # Round and format descriptive stats
  mutate(avg_causal = sprintf("%.2f", as.numeric(avg_causal)),
         sd_causal = sprintf("%.2f", as.numeric(sd_causal)),
         avg_dependent = sprintf("%.2f", as.numeric(avg_dependent)),
         sd_dependent = sprintf("%.2f", as.numeric(sd_dependent)),
         nobs = as.character(nobs),
         n.pairs = as.character(n.pairs),
         n.periods = as.character(n.periods),
         aic = sprintf("%.2f", as.numeric(aic)/1000))  # AIC divided by 1000 and rounded

#### Re-order terms so that 'imbalance' appears first #### 
unique_terms <- unique(est$term)
term_levels <- c("imbalance", setdiff(unique_terms, "imbalance"))
est <- est %>% mutate(term = factor(term, levels = term_levels))

#### Rename terms to more descriptive labels ####
term_labels <- c(
  "imbalance" = "Imbalance",
  "log(dist)" = "Distance (log)",
  "log(gdp_pair)" = "GDP Pair (log)",
  "eu_o" = "EU Origin",
  "eu_d" = "EU Destination",
  "contig" = "Contiguous",
  "diplo_disagreement" = "Diplomatic Disagreement",
  "comlang_off" = "Common language (official)",
  "comcol" = "Colonial Relationship",
  "fta_wto" = "Free Trade Agreement (WTO)"
)

#### Create a long format for main results (estimates and std.errors) ####
tidy_est <- est %>%
  select(var, model, term, specification, estimate_f, std.error_f) %>%
  pivot_longer(
    cols = c("estimate_f", "std.error_f"),
    names_to = "stat_type",
    values_to = "stat_value")


#### Transform from log to wide ####
wide_main <- tidy_est %>%
  select(var, model, term, stat_type, specification, stat_value) %>%
  pivot_wider(
    names_from = specification,
    values_from = stat_value)

#### Extract descriptive stats (nobs, n.pairs, n.periods) ####
descriptives <- est %>%
  distinct(var, model, specification, nobs, n.pairs, n.periods) %>%
  pivot_longer(cols = c("nobs", "n.pairs", "n.periods"),
               names_to = "desc_type",
               values_to = "desc_value") %>%
  pivot_wider(
    names_from = specification,
    values_from = desc_value
  )

#### Extract final stats (avg_causal, sd_causal, avg_dependent, sd_dependent, aic) ####
final_stats <- est %>%
  distinct(var, model, specification, avg_causal, sd_causal, avg_dependent, sd_dependent, aic) %>%
  pivot_longer(cols = c("avg_causal", "sd_causal", "avg_dependent", "sd_dependent", "aic"),
               names_to = "final_type",
               values_to = "final_value") %>%
  pivot_wider(
    names_from = specification,
    values_from = final_value
  )

#### Replace NAs with "-" #### 
wide_main[is.na(wide_main)] <- "-"
descriptives[is.na(descriptives)] <- "-"
final_stats[is.na(final_stats)] <- "-"

#### Rename the descriptive stats #### 
desc_labels <- c(
  "nobs" = "No. Observations",
  "n.pairs" = "No. Country-Pairs",
  "n.periods" = "No. Periods"
)

final_labels <- c(
  "avg_causal" = "Average Causal Var.",
  "sd_causal" = "Causal Var. (SD)",
  "avg_dependent" = "Average Dependent Var.",
  "sd_dependent" = "Dependent Var. (SD)",
  "aic" = "AIC/1000"
)

#### Apply renaming #### 
descriptives <- descriptives %>%
  mutate(desc_type = if_else(desc_type %in% names(desc_labels), desc_labels[desc_type], desc_type))

final_stats <- final_stats %>%
  mutate(final_type = if_else(final_type %in% names(final_labels), final_labels[final_type], final_type))

#### Now combine estimate and std.error rows into a two-row-per-variable format ####
estimates_df <- wide_main %>% filter(stat_type == "estimate_f") %>% select(-stat_type)
stderr_df   <- wide_main %>% filter(stat_type == "std.error_f") %>% select(-stat_type)

combined <- left_join(estimates_df, stderr_df, by = c("var","model","term"), suffix = c("_est","_se"))
combined[is.na(combined)] <- "-"

#### Rename term labels #### 
combined$term = as.character(combined$term)
combined <- combined %>% 
  mutate(term = if_else(term %in% names(term_labels), term_labels[term], as.character(term)))

#### Create the two-row structure for each variable #### 
final_table_rows <- combined %>%
  rename(Variable = term) %>%
  mutate(row_id = row_number()) %>%
  select(var, model, Variable, raw_est, gravity_est, twfe_est, trend_est, raw_se, gravity_se, twfe_se, trend_se, row_id)

#### Expand final table #### 
final_table_expanded <- final_table_rows %>%
  # Row with estimates
  transmute(var, model,
            Variable = Variable,
            raw = raw_est, gravity = gravity_est, twfe = twfe_est, trend = trend_est,
            row_id = row_id,
            line_type = "estimate") %>%
  bind_rows(
    # Row with standard errors (blank variable name)
    final_table_rows %>%
      transmute(var, model,
                Variable = "", 
                raw = raw_se, gravity = gravity_se, twfe = twfe_se, trend = trend_se,
                row_id = row_id,
                line_type = "stderr")
  ) %>%
  arrange(row_id, line_type)

#### Add a mid-rule and then descriptive stats #### 
desc_separator <- tibble(
  var = NA_character_,
  model = NA_character_,
  Variable = "\\midrule",
  raw = NA_character_,
  gravity = NA_character_,
  twfe = NA_character_,
  trend = NA_character_
)

#### Another mid-rule before final stats #### 
final_stats_separator <- tibble(
  var = NA_character_,
  model = NA_character_,
  Variable = "\\midrule",
  raw = NA_character_,
  gravity = NA_character_,
  twfe = NA_character_,
  trend = NA_character_
)

#### Combine everything into final output #### 
final_output <- bind_rows(
  final_table_expanded %>% select(-row_id, -line_type),
  desc_separator,
  descriptives %>% rename(Variable = desc_type),
  final_stats_separator,
  final_stats %>% rename(Variable = final_type)
)

#### Filtering by F1011 and conditional part of the model for the latex table #### 
final_output %>%
  filter(var == "net_imb_f1011", model == "conditional" | is.na(model)) %>%
  select(-var, -model) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Heterogeneous effects by PM2.5 average exposure levels",
        label = "het_split", align = 'lcccc') %>%
  kable_styling(latex_options = c("hold_position"), position = "center")

#### Filtering by F1011 and zero-inflated part of the model for the latex table #### 
final_output %>%
  filter(var == "net_imb_f1011", model != "conditional" | is.na(model)) %>%
  select(-var, -model) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Heterogeneous effects by PM2.5 average exposure levels",
        label = "het_split", align = 'lcccc') %>%
  kable_styling(latex_options = c("hold_position"), position = "center")

#### Clear space ####

#### ..................................................................... #### 
#### Main results of the ZIP by product (TWFE) ####
#### ..................................................................... ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(kableExtra)
library(scales)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load your regression results data frame ####
est = read_rds("02_gen/04_results/zip_split_glmm.rds")

#### Only keep the TWFE model ####
est = filter(est, specification == "twfe")
est = select(est, -specification)

#### Prepare and clean the data ####
est <- est %>%
  # Order specifications
  mutate(product = factor(product, levels = c("aluminum", "glass", "iron_steel", "paper", "plastic")),
         model = factor(model, levels = c("conditional", "zero_inflated"))) %>%
  # Add significance stars
  mutate(sig = case_when(
    p.value < 0.02 ~ '***',
    p.value < 0.05 ~ '**',
    p.value < 0.1  ~ '*',
    TRUE           ~ ''
  ),
  estimate_f = sprintf("%.3f%s", estimate, sig),
  std.error_f = sprintf("(%.3f)", boot_se)) %>%
  # Round and format descriptive stats
  mutate(avg_causal = sprintf("%.2f", as.numeric(avg_causal)),
         sd_causal = sprintf("%.2f", as.numeric(sd_causal)),
         avg_dependent = sprintf("%.2f", as.numeric(avg_dependent)),
         sd_dependent = sprintf("%.2f", as.numeric(sd_dependent)),
         nobs = as.character(nobs),
         n.pairs = as.character(n.pairs),
         n.periods = as.character(n.periods),
         aic = sprintf("%.2f", as.numeric(aic)/1000))  # AIC divided by 1000 and rounded

# Re-order terms so that 'imbalance' appears first
unique_terms <- unique(est$term)
term_levels <- c("imbalance", setdiff(unique_terms, "imbalance"))
est <- est %>% mutate(term = factor(term, levels = term_levels))

#### Rename terms to more descriptive labels ####
term_labels <- c(
  "imbalance" = "Imbalance",
  "log(dist)" = "Distance (log)",
  "log(gdp_pair)" = "GDP Pair (log)",
  "eu_o" = "EU Origin",
  "eu_d" = "EU Destination",
  "contig" = "Contiguous",
  "diplo_disagreement" = "Diplomatic Disagreement",
  "comlang_off" = "Common language (official)",
  "comcol" = "Colonial Relationship",
  "fta_wto" = "Free Trade Agreement (WTO)"
)

#### Create a long format for main results (estimates and std.errors) ####
tidy_est <- est %>%
  select(var, model, term, product, estimate_f, std.error_f) %>%
  pivot_longer(
    cols = c("estimate_f", "std.error_f"),
    names_to = "stat_type",
    values_to = "stat_value"
  )



wide_main <- tidy_est %>%
  select(var, model, term, stat_type, product, stat_value) %>%
  pivot_wider(
    names_from = product,
    values_from = stat_value
  )

#### Extract descriptive stats (nobs, n.pairs, n.periods) ####
descriptives <- est %>%
  distinct(var, model, product, nobs, n.pairs, n.periods) %>%
  pivot_longer(cols = c("nobs", "n.pairs", "n.periods"),
               names_to = "desc_type",
               values_to = "desc_value") %>%
  pivot_wider(
    names_from = product,
    values_from = desc_value
  ) %>% 
  rename(Aluminum = aluminum, 
         Glass = glass, `Iron and Steel` = iron_steel, 
         Paper = paper, Plastic = plastic)

#### Extract final stats (avg_causal, sd_causal, avg_dependent, sd_dependent, aic) ####
final_stats <- est %>%
  distinct(var, model, product, avg_causal, sd_causal, avg_dependent, sd_dependent, aic) %>%
  pivot_longer(cols = c("avg_causal", "sd_causal", "avg_dependent", "sd_dependent", "aic"),
               names_to = "final_type",
               values_to = "final_value") %>%
  pivot_wider(
    names_from = product,
    values_from = final_value
  )%>% 
  rename(Aluminum = aluminum, 
         Glass = glass, `Iron and Steel` = iron_steel, 
         Paper = paper, Plastic = plastic)

# Replace NAs with "-"
wide_main[is.na(wide_main)] <- "-"
descriptives[is.na(descriptives)] <- "-"
final_stats[is.na(final_stats)] <- "-"

# Rename the descriptive stats
desc_labels <- c(
  "nobs" = "No. Observations",
  "n.pairs" = "No. Country-Pairs",
  "n.periods" = "No. Periods"
)

final_labels <- c(
  "avg_causal" = "Average Causal Var.",
  "sd_causal" = "Causal Var. (SD)",
  "avg_dependent" = "Average Dependent Var.",
  "sd_dependent" = "Dependent Var. (SD)",
  "aic" = "AIC/1000"
)

# Apply renaming
descriptives <- descriptives %>%
  mutate(desc_type = if_else(desc_type %in% names(desc_labels), desc_labels[desc_type], desc_type))

final_stats <- final_stats %>%
  mutate(final_type = if_else(final_type %in% names(final_labels), final_labels[final_type], final_type))

#### Now combine estimate and std.error rows into a two-row-per-variable format ####
estimates_df <- wide_main %>% filter(stat_type == "estimate_f") %>% select(-stat_type)
stderr_df   <- wide_main %>% filter(stat_type == "std.error_f") %>% select(-stat_type)

combined <- left_join(estimates_df, stderr_df, by = c("var","model","term"), suffix = c("_est","_se"))
combined[is.na(combined)] <- "-"

# Rename term labels
term_labels["fta_wto"]
combined$term = as.character(combined$term)
combined <- combined %>% 
  mutate(term = if_else(term %in% names(term_labels), term_labels[term], as.character(term)))


# Create the two-row structure for each variable
final_table_rows <- combined %>%
  rename(Variable = term) %>%
  mutate(row_id = row_number()) %>%
  select(var, model, Variable, aluminum_est, glass_est, iron_steel_est, paper_est, plastic_est,
         aluminum_se, glass_se, iron_steel_se, paper_se, plastic_se, row_id)

final_table_expanded <- final_table_rows %>%
  # Row with estimates
  transmute(var, model,
            Variable = Variable,
            Aluminum = aluminum_est, 
            Glass = glass_est, `Iron and Steel` = iron_steel_est, 
            Paper = paper_est, Plastic = plastic_est,
            row_id = row_id,
            line_type = "estimate") %>%
  bind_rows(
    # Row with standard errors (blank variable name)
    final_table_rows %>%
      transmute(var, model,
                Variable = "", 
                Aluminum = aluminum_est, 
                Glass = glass_se, `Iron and Steel` = iron_steel_se, 
                Paper = paper_se, Plastic = plastic_se,
                row_id = row_id,
                line_type = "stderr")
  ) %>%
  arrange(row_id, line_type)

# Add a midrule and then descriptive stats
desc_separator <- tibble(
  var = NA_character_,
  model = NA_character_,
  Variable = "\\midrule",
  Aluminum = NA_character_,
  Glass = NA_character_,
  `Iron and Steel` = NA_character_,
  Paper = NA_character_,
  Plastic = NA_character_
)

# Another midrule before final stats
final_stats_separator <- tibble(
  var = NA_character_,
  model = NA_character_,
  Variable = "\\midrule",
  Aluminum = NA_character_,
  Glass = NA_character_,
  `Iron and Steel` = NA_character_,
  Paper = NA_character_,
  Plastic = NA_character_
)

# Combine everything into final output
final_output <- bind_rows(
  final_table_expanded %>% select(-row_id, -line_type),
  desc_separator,
  descriptives %>% rename(Variable = desc_type),
  final_stats_separator,
  final_stats %>% rename(Variable = final_type)
)


# Filtering by a specific var and model, for demonstration:
final_output %>%
  filter(var == "net_imb_f1011", model == "conditional" | is.na(model)) %>%
  select(-var, -model) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Heterogeneous effects by PM2.5 average exposure levels",
        label = "het_split", align = 'lcccc') %>%
  kable_styling(latex_options = c("hold_position"), position = "center")

# Filtering by a specific var and model, for demonstration:
final_output %>%
  filter(var == "net_imb_f1011", model == "conditional" | is.na(model)) %>%
  select(-var, -model) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Heterogeneous effects by PM2.5 average exposure levels",
        label = "het_split", align = 'lcccc') %>%
  kable_styling(latex_options = c("hold_position"), position = "center")