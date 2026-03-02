#### ..................................................................... ####
####                         Gravity Model Stacked                         ####
#### ..................................................................... ####
#### --------------------------------------------------------------------- #### 
#### Construct the data for the ZIP model (log differences) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(glmmTMB)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
baci  <- read_rds("02_gen/01_trade/baci_raw.rds")
miso  <- read_rds("02_gen/02_miso/raw_material_imbalance.rds")
macro <- read_rds("02_gen/03_macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Aggregate the BACI data to total values ####
baci <- baci %>%
  group_by(year, from, to, prod_class) %>%
  summarise(value = sum(value), volume = sum(volume), .groups = "drop")

#### Expand the BACI data to include zeros ####
baci_full <- baci %>%
  ungroup() %>%
  complete(year, from, to, prod_class, fill = list(volume = 0, value = 0)) %>%
  filter(from != to)

#### Rename the material category of MISO to the same name as BACI ####
miso <- miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso <- miso %>%
  ungroup() %>%
  filter(grepl("F_7_8|F_8_9|F_10_11|F_11_12", name)) %>%
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso <- miso %>%
  group_by(year, prod_class, country, name) %>%
  summarise(value = sum(value), .groups = "drop")

#### Spread MISO ####
miso <- miso %>%
  pivot_wider(names_from = name, values_from = value)


#### Compute the MISO material imbalance ####
miso <- miso %>%
  mutate(
    `F78-F1011`  = log(F_7_8_prod_finals) - log(F_10_11_supply_EoL_waste),
    `F78-F89`    = log(F_7_8_prod_finals) - log(F_8_9_AC_finals),
    `F78-F1112`  = log(F_7_8_prod_finals) - log(F_11_12_waste_recov_domest),
    year = as.numeric(year)
  )

#### Merge with BACI (destination) ####
data <- baci_full %>%
  left_join(
    miso %>% select(year, prod_class, to = country, 
                    `F78-F89-D`   = `F78-F89`, 
                    `F78-F1011-D` = `F78-F1011`, 
                    `F78-F1112-D` = `F78-F1112`),
    by = c("year", "to", "prod_class"))

#### Merge with BACI (origin) ####
data <- data %>%
  left_join(
    miso %>% select(year, prod_class, from = country, 
                    `F78-F89-O`   = `F78-F89`, 
                    `F78-F1011-O` = `F78-F1011`, 
                    `F78-F1112-O` = `F78-F1112`),
    by = c("year", "from", "prod_class"))

#### Subset 2001-2016 ####
data <- data %>%
  filter(year > 2000 & year <= 2016) %>%
  filter(from %in% unique(miso$country)) %>%
  filter(to %in% unique(miso$country))

#### Include the macro covariates ####
data <- data %>%
  left_join(macro, by = c("year", "from", "to")) %>%
  mutate(
    pair = paste0(from, "_", to),
    gdp_pair = gdp_o + gdp_d)

#### Add net imbalance ####
data <- data %>%
  mutate(
    net_imb_f89   = `F78-F89-D` - `F78-F89-O`,
    net_imb_f1011 = `F78-F1011-D` - `F78-F1011-O`,
    net_imb_f1112 = `F78-F1112-D` - `F78-F1112-O`) %>%
  select(-c(`F78-F89-D`:`F78-F1112-O`))

#### Transform from wide to long ####
data <- data %>%
  pivot_longer(
    cols = starts_with("net_imb_"),
    names_to = "var",
    values_to = "imbalance")

#### Remove infinite/NA imbalances ####
data <- data %>%
  filter(!is.infinite(imbalance) & !is.na(imbalance))

#### Convert volume to integer ####
data$volume <- as.integer(data$volume)

#### Save the data ####
write_rds(data, file = "02_gen/04_results/data_gravity_stacked_log_differences.rds")

#### clear the space ####

#### --------------------------------------------------------------------- #### 
#### Raw ZIP for log diferences ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(glmmTMB)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#### Load the data ####
data  <- read_rds("02_gen/04_results/data_gravity_stacked_log_differences.rds")


#### Demeaned the data by country pair and year ####
prod_var_means <- data %>%
  group_by(prod_class, var) %>%
  summarise(prod_var_mean = mean(volume, na.rm = TRUE), .groups = "drop")

#### Add the demeaned value to the data ####
data <- data %>%
  left_join(prod_var_means,  by = c("var", "prod_class")) %>%
  
  mutate(demeaned_a = volume - prod_var_mean) %>%
  
  group_by(pair, var, prod_class) %>%
  # shift everything so it's positive before converting to integer
  mutate(demeaned_a = as.integer(demeaned_a + abs(min(demeaned_a)))) %>%
  ungroup()

#### Define the bootstrap function ####
fn <- function(
    data, 
    formula, 
    product, 
    imb_var,
    ziformula = ~., 
    family    = poisson(link = "log"),
    n_boot    = 250, 
    cluster_var = "pair", 
    n_cores   = parallel::detectCores() - 2
) {
  
  
  # Convert cluster_var to factor once
  #cluster_var = "pair"
  #product = "plastic" 
  #imb_var = "net_imb_f1011"
  data[[cluster_var]] <- as.factor(data[[cluster_var]])
  
  # Filter based on the product class and imbalance 
  data = dplyr::filter(data, prod_class == product)
  data = dplyr::filter(data, var == imb_var)
  
  # Split the data by cluster
  clusters_list <- split(data, data[[cluster_var]])
  cluster_names <- names(clusters_list)
  n_clusters    <- length(clusters_list)
  
  
  # Fit the original model
  print(paste("Starting the ZIP for:", product, "-", imbalance, "..."))
  original_model <- glmmTMB(
    formula   = formula,
    ziformula = ziformula,
    family    = family,
    data      = data,
    REML      = TRUE
  )
  
  # Extract original estimates
  original_results <- list(
    conditional    = broom.mixed::tidy(original_model, component = "cond") %>%
      mutate(model = "conditional"),
    zero_inflated  = broom.mixed::tidy(original_model, component = "zi") %>%
      mutate(model = "zero_inflated")
  ) %>% 
    bind_rows() %>%
    dplyr::select(model, term, estimate, std.error, statistic, p.value)
  
  # Add AIC and nobs
  original_results$nobs <- stats::nobs(original_model)
  original_results$aic  <- AIC(original_model)
  
  # Identify the terms used in the model, if you need them for the fallback
  all_terms <- unique(c(original_results$term))
  
  print(paste("Finished original model:", product, "-", imbalance, "..."))
  
  # Function for a single bootstrap iteration
  single_bootstrap <- function(i) {
    # Sample cluster indices with replacement
    sampled_clusters <- sample(cluster_names, size = n_clusters, replace = TRUE)
    
    # Create bootstrap sample by row-binding the selected clusters
    boot_sample <- dplyr::bind_rows(clusters_list[sampled_clusters])
    
    # Fit model on bootstrap sample
    boot_model <- tryCatch({
      glmmTMB(
        formula   = formula,
        ziformula = ziformula,
        family    = family,
        data      = boot_sample,
        REML      = TRUE
      )
    }, error = function(e) NULL)
    
    # Extract tidy results
    if (!is.null(boot_model)) {
      list(
        conditional    = broom.mixed::tidy(boot_model, component = "cond") %>%
          mutate(model = "conditional"),
        zero_inflated  = broom.mixed::tidy(boot_model, component = "zi") %>%
          mutate(model = "zero_inflated")
      ) %>% 
        bind_rows()
    } else {
      # Return a dummy tibble if model fails
      tibble(
        model     = rep(c("conditional", "zero_inflated"), each = length(all_terms)),
        term      = rep(all_terms, times = 2),
        estimate  = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value   = NA_real_
      )
    }
  }
  
  #
  print(paste("Started Bootstrap:", product, "-", imbalance, "..."))
  
  # Run bootstrap in parallel
  boot_list <- parallel::mclapply(
    X         = seq_len(n_boot), 
    FUN       = single_bootstrap, 
    mc.cores  = n_cores
  )
  
  print(paste("Finished Bootstrap:", product, "-", imbalance, "..."))
  
  # Combine results
  boot_combined <- bind_rows(boot_list)
  
  # Calculate standard deviation of the bootstrapped estimates
  boot_se <- boot_combined %>%
    group_by(model, term) %>%
    summarize(boot_se = sd(estimate, na.rm = TRUE), .groups = "drop")
  
  # Merge original with bootstrap SE
  final_results <- original_results %>%
    select(-std.error) %>%
    left_join(boot_se, by = c("model", "term")) %>%
    mutate(
      statistic = ifelse(!is.na(boot_se), estimate / boot_se, NA),
      p.value   = ifelse(!is.na(statistic), 2 * (1 - pnorm(abs(statistic))), NA)
    ) %>% 
    mutate(product = product, imb = imb_var)
  
  # End message 
  print(paste("Successufully finished:", product, "-", imbalance, "..."))
  final_results
}

#### Set seed for reproducibility ####
product = c("plastic","aluminum", "iron_steel", "paper", "glass")
imbalance = c("net_imb_f1011", "net_imb_f1112", "net_imb_f89")

#### Run the raw model ####
est_raw <- mclapply(product, function(x) lapply(imbalance, function(y)
  fn(data = data,
     formula    = demeaned_a ~ imbalance,
     ziformula  = demeaned_a ~ imbalance,
     product = x,
     imb_var = y,
     family     = poisson(link = "log"),
     n_boot     = 25,
     n_cores = 100,
     cluster_var = "pair")), mc.cores = 10)

#### Bind the estimates together
est <- lapply(est_raw, bind_rows) %>%
  bind_rows(.)

#### Add the average of the outcome variables ####
stat_imb <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    avg_causal = mean(exp(imbalance), na.rm = TRUE),
    sd_causal  = sd(exp(imbalance),  na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_imb)

#### Add the average of the dependent variables ####
stat_dep <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    avg_dependent = mean(value, na.rm = TRUE),
    sd_dependent  = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_dep)

#### Add the number of periods and country pairs ####
stat_period_pairs <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    n.pairs   = n_distinct(pair),
    n.periods = n_distinct(year),
    .groups = "drop"
  )

est <- left_join(est, stat_period_pairs)

#### Add the estimator and sample ####
est = mutate(est, estimator = "zip", sample = "split", spec = "raw") %>% 
  as.data.frame()

#### Check the data ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/zip_raw_glmm_split_log_diff.rds")

#### --------------------------------------------------------------------- #### 
#### Gravity ZIP for log differences ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(glmmTMB)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#### Load the data ####
data  <- read_rds("02_gen/04_results/data_gravity_stacked_log_differences.rds")

#### Demeaned the data by country pair and year ####
prod_var_means <- data %>%
  group_by(prod_class, var) %>%
  summarise(prod_var_mean = mean(volume, na.rm = TRUE), .groups = "drop")

#### Add the demeaned value to the data ####
data <- data %>%
  left_join(prod_var_means,  by = c("var", "prod_class")) %>%
  
  mutate(demeaned_a = volume - prod_var_mean) %>%
  
  group_by(pair, var, prod_class) %>%
  # shift everything so it's positive before converting to integer
  mutate(demeaned_a = as.integer(demeaned_a + abs(min(demeaned_a)))) %>%
  ungroup()

#### Define the bootstrap function ####
fn <- function(
    data, 
    formula, 
    product, 
    imb_var,
    ziformula = ~., 
    family    = poisson(link = "log"),
    n_boot    = 250, 
    cluster_var = "pair", 
    n_cores   = parallel::detectCores() - 2
) {
  
  
  # Convert cluster_var to factor once
  #cluster_var = "pair"
  #product = "plastic" 
  #imb_var = "net_imb_f1011"
  data[[cluster_var]] <- as.factor(data[[cluster_var]])
  
  # Filter based on the product class and imbalance 
  data = dplyr::filter(data, prod_class == product)
  data = dplyr::filter(data, var == imb_var)
  
  # Split the data by cluster
  clusters_list <- split(data, data[[cluster_var]])
  cluster_names <- names(clusters_list)
  n_clusters    <- length(clusters_list)
  
  
  # Fit the original model
  print(paste("Starting the ZIP for:", product, "-", imbalance, "..."))
  original_model <- glmmTMB(
    formula   = formula,
    ziformula = ziformula,
    family    = family,
    data      = data,
    REML      = TRUE
  )
  
  # Extract original estimates
  original_results <- list(
    conditional    = broom.mixed::tidy(original_model, component = "cond") %>%
      mutate(model = "conditional"),
    zero_inflated  = broom.mixed::tidy(original_model, component = "zi") %>%
      mutate(model = "zero_inflated")
  ) %>% 
    bind_rows() %>%
    dplyr::select(model, term, estimate, std.error, statistic, p.value)
  
  # Add AIC and nobs
  original_results$nobs <- stats::nobs(original_model)
  original_results$aic  <- AIC(original_model)
  
  # Identify the terms used in the model, if you need them for the fallback
  all_terms <- unique(c(original_results$term))
  
  print(paste("Finished original model:", product, "-", imbalance, "..."))
  
  # Function for a single bootstrap iteration
  single_bootstrap <- function(i) {
    # Sample cluster indices with replacement
    sampled_clusters <- sample(cluster_names, size = n_clusters, replace = TRUE)
    
    # Create bootstrap sample by row-binding the selected clusters
    boot_sample <- dplyr::bind_rows(clusters_list[sampled_clusters])
    
    # Fit model on bootstrap sample
    boot_model <- tryCatch({
      glmmTMB(
        formula   = formula,
        ziformula = ziformula,
        family    = family,
        data      = boot_sample,
        REML      = TRUE
      )
    }, error = function(e) NULL)
    
    # Extract tidy results
    if (!is.null(boot_model)) {
      list(
        conditional    = broom.mixed::tidy(boot_model, component = "cond") %>%
          mutate(model = "conditional"),
        zero_inflated  = broom.mixed::tidy(boot_model, component = "zi") %>%
          mutate(model = "zero_inflated")
      ) %>% 
        bind_rows()
    } else {
      # Return a dummy tibble if model fails
      tibble(
        model     = rep(c("conditional", "zero_inflated"), each = length(all_terms)),
        term      = rep(all_terms, times = 2),
        estimate  = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value   = NA_real_
      )
    }
  }
  
  #
  print(paste("Started Bootstrap:", product, "-", imbalance, "..."))
  
  # Run bootstrap in parallel
  boot_list <- parallel::mclapply(
    X         = seq_len(n_boot), 
    FUN       = single_bootstrap, 
    mc.cores  = n_cores
  )
  
  print(paste("Finished Bootstrap:", product, "-", imbalance, "..."))
  
  # Combine results
  boot_combined <- bind_rows(boot_list)
  
  # Calculate standard deviation of the bootstrapped estimates
  boot_se <- boot_combined %>%
    group_by(model, term) %>%
    summarize(boot_se = sd(estimate, na.rm = TRUE), .groups = "drop")
  
  # Merge original with bootstrap SE
  final_results <- original_results %>%
    select(-std.error) %>%
    left_join(boot_se, by = c("model", "term")) %>%
    mutate(
      statistic = ifelse(!is.na(boot_se), estimate / boot_se, NA),
      p.value   = ifelse(!is.na(statistic), 2 * (1 - pnorm(abs(statistic))), NA)
    ) %>% 
    mutate(product = product, imb = imb_var)
  
  # End message 
  print(paste("Successufully finished:", product, "-", imbalance, "..."))
  final_results
}

#### Set seed for reproducibility ####
product = c("plastic","aluminum", "iron_steel", "paper", "glass")
imbalance = c("net_imb_f1011", "net_imb_f1112", "net_imb_f89")

#### Run the raw model ####
est_gravity <- mclapply(product, function(x) lapply(imbalance, function(y)
  fn(data = data,
     formula   = volume ~ imbalance + log(dist) + log(gdp_pair) + 
       eu_o + eu_d + contig + diplo_disagreement +
       comlang_off + comcol + fta_wto,
     ziformula = ~ .,
     product = x,
     imb_var = y,
     family     = poisson(link = "log"),
     n_boot     = 25,
     n_cores = 100,
     cluster_var = "pair")), mc.cores = 10)

#### Bind the estimates together
est <- lapply(est_gravity, bind_rows) %>%
  bind_rows(.)

#### Add the average of the outcome variables ####
stat_imb <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    avg_causal = mean(exp(imbalance), na.rm = TRUE),
    sd_causal  = sd(exp(imbalance),  na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_imb)

#### Add the average of the dependent variables ####
stat_dep <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    avg_dependent = mean(value, na.rm = TRUE),
    sd_dependent  = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_dep)

#### Add the number of periods and country pairs ####
stat_period_pairs <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    n.pairs   = n_distinct(pair),
    n.periods = n_distinct(year),
    .groups = "drop"
  )

est <- left_join(est, stat_period_pairs)

#### Add the estimator and sample ####
est = mutate(est, estimator = "zip", sample = "split", spec = "gravity") %>% 
  as.data.frame()

#### Check the data ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/zip_gravity_glmm_split_log_diff.rds")


#### --------------------------------------------------------------------- #### 
#### TWFEs ZIP for log differences ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(glmmTMB)
library(parallel)

#### Set working directory ####

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#### Load the data ####
data  <- read_rds("02_gen/04_results/data_gravity_stacked_log_differences.rds")

#### Split the data set ####
data = split(data, f = list(data$prod_class, data$var))

#### Demeaned the data by country pair and year ####
data = lapply(data, function(x)
  x = x %>% group_by(pair) %>% 
    mutate(pair_var_mean = mean(volume, na.rm = T), .groups = "drop"))


data = lapply(data, function(x)
  x = x %>% group_by(year) %>% 
    mutate(year_var_mean = mean(volume, na.rm = T), .groups = "drop"))

data = lapply(data, function(x)
  x = mutate(x, demeaned_a = volume - pair_var_mean - year_var_mean) %>% 
    group_by(pair) %>% 
    mutate(demeaned_a = as.integer(demeaned_a + abs(min(demeaned_a)))) %>%
    ungroup() %>% as.data.frame())


data = bind_rows(data)

#### Define the bootstrap function ####
fn <- function(
    data, 
    formula, 
    product, 
    imb_var,
    ziformula = ~., 
    family    = poisson(link = "log"),
    n_boot    = 250, 
    cluster_var = "pair", 
    n_cores   = parallel::detectCores() - 2
) {
  
  
  # Convert cluster_var to factor once
  #cluster_var = "pair"
  #product = "plastic" 
  #imb_var = "net_imb_f1011"
  data[[cluster_var]] <- as.factor(data[[cluster_var]])
  
  # Filter based on the product class and imbalance 
  data = dplyr::filter(data, prod_class == product)
  data = dplyr::filter(data, var == imb_var)
  
  # Split the data by cluster
  clusters_list <- split(data, data[[cluster_var]])
  cluster_names <- names(clusters_list)
  n_clusters    <- length(clusters_list)
  
  
  # Fit the original model
  print(paste("Starting the ZIP for:", product, "-", imbalance, "..."))
  original_model <- glmmTMB(
    formula   = formula,
    ziformula = ziformula,
    family    = family,
    data      = data,
    REML      = TRUE
  )
  
  # Extract original estimates
  original_results <- list(
    conditional    = broom.mixed::tidy(original_model, component = "cond") %>%
      mutate(model = "conditional"),
    zero_inflated  = broom.mixed::tidy(original_model, component = "zi") %>%
      mutate(model = "zero_inflated")
  ) %>% 
    bind_rows() %>%
    dplyr::select(model, term, estimate, std.error, statistic, p.value)
  
  # Add AIC and nobs
  original_results$nobs <- stats::nobs(original_model)
  original_results$aic  <- AIC(original_model)
  
  # Identify the terms used in the model, if you need them for the fallback
  all_terms <- unique(c(original_results$term))
  
  print(paste("Finished original model:", product, "-", imbalance, "..."))
  
  # Function for a single bootstrap iteration
  single_bootstrap <- function(i) {
    # Sample cluster indices with replacement
    sampled_clusters <- sample(cluster_names, size = n_clusters, replace = TRUE)
    
    # Create bootstrap sample by row-binding the selected clusters
    boot_sample <- dplyr::bind_rows(clusters_list[sampled_clusters])
    
    # Fit model on bootstrap sample
    boot_model <- tryCatch({
      glmmTMB(
        formula   = formula,
        ziformula = ziformula,
        family    = family,
        data      = boot_sample,
        REML      = TRUE
      )
    }, error = function(e) NULL)
    
    # Extract tidy results
    if (!is.null(boot_model)) {
      list(
        conditional    = broom.mixed::tidy(boot_model, component = "cond") %>%
          mutate(model = "conditional"),
        zero_inflated  = broom.mixed::tidy(boot_model, component = "zi") %>%
          mutate(model = "zero_inflated")
      ) %>% 
        bind_rows()
    } else {
      # Return a dummy tibble if model fails
      tibble(
        model     = rep(c("conditional", "zero_inflated"), each = length(all_terms)),
        term      = rep(all_terms, times = 2),
        estimate  = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value   = NA_real_
      )
    }
  }
  
  #
  print(paste("Started Bootstrap:", product, "-", imbalance, "..."))
  
  # Run bootstrap in parallel
  boot_list <- parallel::mclapply(
    X         = seq_len(n_boot), 
    FUN       = single_bootstrap, 
    mc.cores  = n_cores
  )
  
  print(paste("Finished Bootstrap:", product, "-", imbalance, "..."))
  
  # Combine results
  boot_combined <- bind_rows(boot_list)
  
  # Calculate standard deviation of the bootstrapped estimates
  boot_se <- boot_combined %>%
    group_by(model, term) %>%
    summarize(boot_se = sd(estimate, na.rm = TRUE), .groups = "drop")
  
  # Merge original with bootstrap SE
  final_results <- original_results %>%
    select(-std.error) %>%
    left_join(boot_se, by = c("model", "term")) %>%
    mutate(
      statistic = ifelse(!is.na(boot_se), estimate / boot_se, NA),
      p.value   = ifelse(!is.na(statistic), 2 * (1 - pnorm(abs(statistic))), NA)
    ) %>% 
    mutate(product = product, imb = imb_var)
  
  # End message 
  print(paste("Successufully finished:", product, "-", imbalance, "..."))
  final_results
}

#### Set seed for reproducibility ####
product = c("plastic","aluminum", "iron_steel", "paper", "glass")
imbalance = c("net_imb_f1011", "net_imb_f1112", "net_imb_f89")

data$year = as.factor(data$year)
#### Run the raw model ####
est_twfe <- lapply(product, function(x) lapply(imbalance, function(y)
  fn(data = data,
     formula    = demeaned_a ~ imbalance,
     ziformula  = demeaned_a ~ imbalance,
     product = x,
     imb_var = y,
     family     = poisson(link = "log"),
     n_boot     = 25,
     n_cores = 100,
     cluster_var = "pair")))

#### Bind the estimates together
est <- lapply(est_twfe, bind_rows) %>%
  bind_rows(.) 

#### Add the average of the outcome variables ####
stat_imb <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    avg_causal = mean(exp(imbalance), na.rm = TRUE),
    sd_causal  = sd(exp(imbalance),  na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_imb)

#### Add the average of the dependent variables ####
stat_dep <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    avg_dependent = mean(value, na.rm = TRUE),
    sd_dependent  = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

est <- left_join(est, stat_dep)

#### Add the number of periods and country pairs ####
stat_period_pairs <- data %>%
  group_by(imb = var, product = prod_class) %>%
  summarise(
    n.pairs   = n_distinct(pair),
    n.periods = n_distinct(year),
    .groups = "drop"
  )

est <- left_join(est, stat_period_pairs)

#### Add the estimator and sample ####
est = mutate(est, estimator = "zip", sample = "split", spec = "twfe") %>% 
  as.data.frame()

#### Check the data ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/zip_twfe_glmm_split_log_diff.rds")

#### --------------------------------------------------------------------- #### 
#### Table with point estimates ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(kableExtra)
library(scales)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load your regression results data frame ####
est = read_rds("02_gen/04_results/zip_split_glmm_twfe_log_difference.rds")

x = filter(est, model == "conditional", term == "imbalance", imb == "net_imb_f1011")

x
head(est)


est[[2]]$specification = "gravity_model"

#### Bind the list elements together ####
est = bind_rows(est)

#### Prepare and clean the data ####
est <- est %>%
  # Order specifications
  mutate(specification = factor(specification, levels = c("raw_model", "gravity_model", "twfe_model", "trend_model")),
         model = factor(model, levels = c("conditional", "zero_inflated"))) %>%
  # Add significance stars
  mutate(sig = case_when(
    p.value < 0.02 ~ '***',
    p.value < 0.05 ~ '**',
    p.value < 0.1  ~ '*',
    TRUE           ~ ''),
    estimate_f = sprintf("%.5f%s", estimate, sig),
    std.error_f = sprintf("(%.5f)", boot_se)) %>%
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
  select(var, model, Variable, raw_model_est, gravity_model_est, twfe_model_est, trend_model_est, 
         raw_model_se, gravity_model_se, twfe_model_se, trend_model_se, row_id)

#### Expand final table #### 
final_table_expanded <- final_table_rows %>%
  # Row with estimates
  transmute(var, model,
            Variable = Variable,
            raw = raw_model_est, gravity = gravity_model_est,
            twfe = twfe_model_est, trend = trend_model_est,
            row_id = row_id,
            line_type = "estimate") %>%
  bind_rows(
    # Row with standard errors (blank variable name)
    final_table_rows %>%
      transmute(var, model,
                Variable = "", 
                raw = raw_model_se, gravity = gravity_model_se, 
                twfe = twfe_model_se, trend = trend_model_se,
                row_id = row_id,
                line_type = "stderr")
  ) %>%
  arrange(row_id, line_type)

#### Add a midrule and then descriptive stats #### 
desc_separator <- tibble(
  var = NA_character_,
  model = NA_character_,
  Variable = "\\midrule",
  raw = NA_character_,
  gravity = NA_character_,
  twfe = NA_character_,
  trend = NA_character_
)

#### Another midrule before final stats #### 
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
descriptives = rename(descriptives, raw = raw_model, gravity = gravity_model, 
                      twfe = twfe_model, trend = trend_model)

final_stats = rename(final_stats, raw = raw_model, gravity = gravity_model, 
                     twfe = twfe_model, trend = trend_model)

final_output <- bind_rows(
  final_table_expanded %>% select(-row_id, -line_type),
  desc_separator,
  descriptives %>% rename(Variable = desc_type),
  final_stats_separator,
  final_stats %>% rename(Variable = final_type)
)

#### Filtering by F1011 and conditional part of the model for the latex table #### 
final_output %>%
  filter(var == "net_imb_f89", model == "conditional" | is.na(model)) %>%
  select(-var, -model) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = "Heterogeneous effects by PM2.5 average exposure levels",
        label = "het_split", align = 'lcccc') %>%
  kable_styling(latex_options = c("hold_position"), position = "center")

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
test = read_rds("02_gen/04_results/zip_split_glmm_twfe_log_difference.rds")

