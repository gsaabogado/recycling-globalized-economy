#### --------------------------------------------------------------------- #### 
#### Pooled log-ols with log differences ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(fixest)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data  <- read_rds("02_gen/04_results/data_gravity_pooled_log_differences.rds")

#### Estimate the log-ols model ####
est_ols = list(raw = feols(log(volume) ~ imbalance, data = data, cluster = "pair", split = ~var), 
               
               gravity = feols(log(volume) ~ imbalance + log(dist) + log(gdp_pair) + 
                                 eu_o + eu_d + contig + diplo_disagreement +
                                 comlang_off + comcol + fta_wto, data = data, cluster = "pair", split = ~var), 
               
               twfe = feols(log(volume) ~ imbalance |pair + year, data = data, cluster = "pair", split = ~var)) 

#### Bind the estimates together
est <- lapply(est_ols, function(x) lapply(x, function(x)
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2")))) %>% 
  lapply(., bind_rows, .id = "imb") %>%
  bind_rows(., .id = "spec") 

#### Only keep the net imbalance text ####
est$imb = gsub(".*: ", "", est$imb)

#### Add the average of the outcome variables ####
stat_imb <- data %>%
  group_by(imb = var) %>%
  summarise(
    avg_causal = mean(exp(imbalance), na.rm = TRUE),
    sd_causal  = sd(exp(imbalance),  na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_imb)

#### Add the average of the dependent variables ####
stat_dep <- data %>%
  group_by(imb = var) %>%
  summarise(
    avg_dependent = mean(value, na.rm = TRUE),
    sd_dependent  = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_dep)

#### Add the number of periods and country pairs ####
stat_period_pairs <- data %>% filter(volume != 0) %>% 
  group_by(imb = var) %>%
  summarise(
    n.countries = n_distinct(to),
    n.pairs   = n_distinct(pair),
    n.periods = n_distinct(year),
    .groups = "drop"
  )

est <- left_join(est, stat_period_pairs)

#### Check the data set ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/log_ols_pooled.rds")

#### --------------------------------------------------------------------- #### 
#### Stacked by product with log-ols and log differences ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(fixest)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data  <- read_rds("02_gen/04_results/data_gravity_stacked_log_differences.rds")

#### Exclude zeros from the data ####
#data = filter(data, volume > 0)

#### Estimate the log-ols model ####
function_est <- function(data, imb_var) {
  # Create a list to store the results
  #imb_var = "net_imb_f1011"
  #prod_class = "plastic"
  test = filter(data %>% ungroup(), var == imb_var, volume > 0)
  # Iterate over formulas
  tryCatch({
    # Estimate the model
    results <- list(raw = feols(log(volume) ~ imbalance | prod_class, 
                                data = test, cluster = "pair"),
                    
                    gravity = feols(log(volume) ~ imbalance + log(dist) + log(gdp_pair) + 
                                      eu_o + eu_d + contig + diplo_disagreement +
                                      comlang_off + comcol + fta_wto| prod_class, 
                                    data = test, cluster = "pair"),
                    
                    twfe = feols(log(volume) ~ imbalance | prod_class + pair + year, 
                                 data = test, cluster = "pair")) %>% 
      
      lapply(., function(x) data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>% 
      
      bind_rows(., .id = "spec") %>% 
      
      mutate(imb = imb_var)
    
  }, error = function(e) {
    # Save NULL on error and display the warning
    results <- NULL
    warning(sprintf(
      "Error in '%s' estimation: %s", 
      imb_var,  # Ensure imb_var is properly defined
      e$message))
  })
  
  return(results)
}

#### Check the results ####
est = lapply(c("net_imb_f1011", "net_imb_f1112", "net_imb_f89"), function(x) 
    function_est(data = data, imb_var = x))

#### Exclude all NULL elements ####
est = bind_rows(est)
x = filter(est, spec == "raw")

#### Add the average of the outcome variables ####
stat_imb <- data %>% filter(volume != 0) %>% 
  group_by(imb = var) %>%
  summarise(
    avg_causal = mean(exp(imbalance), na.rm = TRUE),
    sd_causal  = sd(exp(imbalance),  na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_imb)

#### Add the average of the dependent variables ####
stat_dep <- data %>% filter(volume != 0) %>% 
  group_by(imb = var) %>%
  summarise(
    avg_dependent = mean(value, na.rm = TRUE),
    sd_dependent  = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_dep)

#### Add the number of periods and country pairs ####
stat_period_pairs <- data %>% filter(volume != 0) %>% 
  group_by(imb = var) %>%
  summarise(
    n.countries = n_distinct(to),
    n.pairs   = n_distinct(pair),
    n.periods = n_distinct(year),
    .groups = "drop"
  )

est <- left_join(est, stat_period_pairs)

#### Check the data set ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/log_ols_stacked.rds")

#### --------------------------------------------------------------------- #### 
#### Spitted by product with log-ols and log differences ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(fixest)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data  <- read_rds("02_gen/04_results/data_gravity_stacked_log_differences.rds")

#### Exclude zeros from the data ####
#data = filter(data, volume > 0)

#### Estimate the log-ols model ####
function_est <- function(data, imb_var, product) {
  tryCatch({
    # Filter the data
    test <- data %>%
      filter(var == imb_var, prod_class == product, !is.na(volume) & volume > 0)
    
    # Estimate the models
    results <- list(
      raw = feols(log(volume) ~ imbalance, data = test, cluster = "pair"),
      gravity = feols(log(volume) ~ imbalance + log(dist) + log(gdp_pair) +
                        eu_o + eu_d + contig + diplo_disagreement +
                        comlang_off + comcol + fta_wto, data = test, cluster = "pair"),
      twfe = feols(log(volume) ~ imbalance | pair + year, data = test, cluster = "pair")
    ) %>%
      lapply(function(x) data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>%
      bind_rows(.id = "spec") %>%
      mutate(prod_class = product, imb = imb_var)
    
    return(results)
  }, error = function(e) {
    # Save NULL on error and display the warning
    warning(sprintf("Error in '%s & %s' estimation: %s", imb_var, product, e$message))
    return(NULL)
  })
}

#### Check the results ####
est = lapply(c("net_imb_f1011", "net_imb_f1112", "net_imb_f89"), function(x) 
  lapply(c("aluminum", "iron_steel", "paper",  "plastic","glass"), function(y) 
    function_est(data = data, imb_var = x, product = y)))

#### Exclude all NULL elements ####
est = lapply(est, bind_rows) %>% bind_rows(.)

#### Add the average of the outcome variables ####
stat_imb <- data %>%
  group_by(imb = var, prod_class) %>%
  summarise(
    avg_causal = mean(exp(imbalance), na.rm = TRUE),
    sd_causal  = sd(exp(imbalance),  na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_imb)

#### Add the average of the dependent variables ####
stat_dep <- data %>%
  group_by(imb = var, prod_class) %>%
  summarise(
    avg_dependent = mean(value, na.rm = TRUE),
    sd_dependent  = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_dep)

#### Add the number of periods and country pairs ####
stat_period_pairs <- data %>% filter(volume != 0) %>% 
  group_by(imb = var) %>%
  summarise(
    n.countries = n_distinct(to),
    n.pairs   = n_distinct(pair),
    n.periods = n_distinct(year),
    .groups = "drop"
  )

est <- left_join(est, stat_period_pairs)

#### Check the data set ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/log_ols_split.rds")


