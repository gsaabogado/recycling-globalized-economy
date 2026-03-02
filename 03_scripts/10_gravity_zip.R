#### ..................................................................... ####
####                              Gravity Model                            ####
#### ..................................................................... ####
#### --------------------------------------------------------------------- #### 
#### ZIP for the Effect of RLM on pooled trade values (absolute differences) ####
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

#### Load the data set ####
baci = read_rds("02_gen/01_trade/baci_raw.rds")
miso = read_rds("02_gen/02_miso/raw_material_imbalance.rds")
macro = read_rds("02_gen/03_macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Aggregate the BACI data to total values ####
baci = baci %>% group_by(year, from, to) %>% 
  summarise(value = sum(value), volume = sum(volume))

#### Expand the BACI data to include zeros ####
baci_full <- baci %>% ungroup() %>% 
  complete(year,from, to, fill = list(volume = 0, value = 0)) %>% 
  filter(from != to)   

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11|F_11_12", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste)/1000,
         `F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000,
         `F78-F1112` = (F_7_8_prod_finals-F_11_12_waste_recov_domest)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% 
  summarise(`F78-F89` = sum(`F78-F89`),
            `F78-F1011` = sum(`F78-F1011`),
            `F78-F1112` = sum(`F78-F1112`))

#### Add the material imbalance to the waste data (destination country) ####
data = left_join(baci_full, miso%>% 
                   select(year, to = country, `F78-F89-D`= `F78-F89`, 
                          `F78-F1011-D`= `F78-F1011`, `F78-F1112-D`= `F78-F1112`))


#### Add the material imbalance to the waste data (ORIGIN country) ####
data = left_join(data, miso%>% 
                   select(year, from = country, `F78-F89-O`= `F78-F89`, 
                          `F78-F1011-O`= `F78-F1011`, `F78-F1112-O`= `F78-F1112`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Restrict the data to only countries in the MISO data ####
data = filter(data, from %in% unique(miso$country))
data = filter(data, to %in% unique(miso$country))

#### Determine the share of zeros in each ICD10 ####
zero_share <- data %>% summarize(zero_obs = sum(value == 0), total_obs = n(),
    share_of_zero_obs = zero_obs / total_obs,mean_value = mean(value, na.rm =T),
    exp_zeros = exp(-mean(value, na.rm =T)))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb_f89 = abs(data$`F78-F89-D` - data$`F78-F89-O`) %>% log(.)
data$net_imb_f1011 = abs(data$`F78-F1011-D` - data$`F78-F1011-O`) %>% log(.)
data$net_imb_f1112 = abs(data$`F78-F1112-D` - data$`F78-F1112-O`) %>% log(.)

#### Transform from wide to long format ####
data = select(data, -c(`F78-F89-D`:`F78-F1112-O`))
data = gather(data, var, imbalance, c(net_imb_f89, net_imb_f1011, net_imb_f1112))

#### Take the log value of net imbalance ####
data$volume = as.integer(data$volume)

#### Estimate the de-meaned values ####
data <- data  %>%
  group_by(pair, var) %>%
  mutate(mean_pair = mean(volume, na.rm = TRUE)) %>% 
  
  group_by(year, var) %>%
  mutate(mean_year = mean(volume, na.rm = TRUE)) %>%
  
  group_by(from, year, var) %>%
  mutate(mean_from_year = mean(volume, na.rm = TRUE)) %>%
  
  group_by(to, year, var) %>%
  mutate(mean_to_year = mean(volume, na.rm = TRUE)) %>%
  
  mutate(demeaned_a = volume - mean_pair - mean_year,
    demeaned_b = volume  - mean_pair - mean_year - mean_from_year - mean_to_year) %>% 
  
  mutate_at(vars(demeaned_a:demeaned_b), function(x) x = as.integer(x)) %>% 
  mutate_at(vars(demeaned_a:demeaned_b), function(x) x = x + abs(min(x)))

#### Exclude observations with zero imbalance ####
# 32 obs only from F89
data = filter(data, is.infinite(imbalance) == F)

####  Define the bootstrap function ####
fn <- function(data, formula, ziformula = ~., family = poisson(link = "log"), 
               n_boot = 250, cluster_var = "pair", n_cores = detectCores() - 1) {
  
  # Ensure the cluster variable is a factor
  data[[cluster_var]] <- as.factor(data[[cluster_var]])
  
  # Fit the original model
  original_model <- glmmTMB(formula = formula,ziformula = ziformula,
    family = family,data = data,REML = TRUE)
  
  # Extract original model components
  original_results <- list(
    conditional = tidy(original_model, component = "cond") %>% 
      mutate(model = "conditional"),
    
    zero_inflated = tidy(original_model, component = "zi") %>%
      mutate(model = "zero_inflated")) %>%
    
    bind_rows(.id = NULL) %>%
    select(model, term, estimate, std.error, statistic, p.value)
  
  # Add the AIC nad nobs 
  original_results$nobs = nobs(original_model)
  original_results$aic = AIC(original_model)
  
  # Function to perform a single bootstrap iteration
  single_bootstrap <- function(i) {
    # Sample clusters with replacement
    sampled_clusters <- sample(levels(data[[cluster_var]]), 
                               size = length(levels(data[[cluster_var]])), 
                               replace = TRUE)
    
    # Create bootstrap sample
    boot_sample <- data %>% filter(!!sym(cluster_var) %in% sampled_clusters)
    
    # Fit the model on the bootstrap sample
    boot_model <- tryCatch(
      glmmTMB(formula = formula, ziformula = ziformula, family = family,
              data = boot_sample, REML = TRUE
      ),
      error = function(e) NULL
    )
    
    # Extract estimates if model fitted successfully
    if (!is.null(boot_model)) {
      list(conditional = tidy(boot_model, component = "cond") %>% 
          mutate(model = "conditional"),
          
        zero_inflated = tidy(boot_model, component = "zi") %>%
          mutate(model = "zero_inflated")) %>%
        bind_rows(.id = NULL)
    } else {
      # Return NA rows if the model failed
      terms <- all.vars(formula)
      tibble(model = rep(c("conditional", "zero_inflated"), each = length(terms)),
        term = rep(terms, times = 2),
        estimate = NA, std.error = NA, statistic = NA, p.value = NA
      )
    }
  }
  
  # Perform bootstrapping using mclapply
  boot_list <- mclapply(1:n_boot, single_bootstrap, mc.cores = n_cores)
  
  # Combine all bootstrap results into one data frame
  boot_combined <- bind_rows(boot_list) %>%
    filter(!is.na(estimate))
  
  # Calculate bootstrapped standard errors
  boot_se <- boot_combined %>%
    group_by(model, term) %>%
    summarize(boot_se = sd(estimate, na.rm = TRUE), .groups = "drop")
  
  # Merge original estimates with bootstrapped SEs
  final_results <- original_results %>% select(-std.error) %>% 
    left_join(boot_se, by = c("model", "term")) %>%
    mutate(
      statistic = ifelse(!is.na(boot_se), estimate / boot_se, NA),
      p.value = ifelse(!is.na(statistic), 2 * (1 - pnorm(abs(statistic))), NA)
    )
  
  return(final_results)
}

#### Split the data by imbalance ####
data = split(data, f = data$var)

#### Set seed for reproducibility ####
set.seed(123)

#### Run the raw model ####
est_raw <- mclapply(data, function(x) 
  fn(data = x,  formula = volume ~ imbalance, 
  ziformula = ~ imbalance, family = poisson(link = "log"), n_boot = 250,                  
  cluster_var = "pair"), mc.cores = 50); est_raw

#### Run the gravity model ####
est_gravity <- lapply(data, function(x) 
  
  fn(data = x,  formula = volume ~ imbalance + log(dist) + log(gdp_pair) +
                    eu_o + eu_d + contig + diplo_disagreement +
                    comlang_off + comcol + fta_wto, 
                  
              ziformula = ~ imbalance + log(dist) + log(gdp_pair) +
                eu_o + eu_d + contig + diplo_disagreement +
                comlang_off + comcol + fta_wto, 
              
              family = poisson(link = "log"), n_boot = 250,                  
              cluster_var = "pair")); est_gravity

#### Run the TWFEs model ####
est_twfe <- lapply(data, function(x) fn(data = x,  formula = demeaned_a ~ imbalance, 
                  
                  ziformula = ~., 
                  
                  family = poisson(link = "log"), n_boot = 250,                  
                  cluster_var = "pair")); est_twfe

#### Run the de-trend model ####
est_trend <- lapply(data, function(x) fn(data = x,  formula = demeaned_b ~ imbalance, 
               
               ziformula = ~., 
               
               family = poisson(link = "log"), n_boot = 250,                  
               cluster_var = "pair", n_cores = 25)); est_trend

#### Put the estimates together####
est = list(raw = est_raw, gravity = est_gravity, twfe = est_twfe, trend = est_trend)

#### Bind the list elements ####
est = lapply(est, function(x) lapply(x, bind_rows)) %>% 
  lapply(., bind_rows, .id = "var") %>% 
  bind_rows(., .id = "specification")

#### Add the average of the outcome variables ####
est = data %>% bind_rows() %>% group_by(var) %>% 
  summarise(avg_causal = mean(exp(imbalance), na.rm = T), 
            sd_causal = sd(exp(imbalance), na.rm = T)) %>% 
  left_join(est, .)

#### Add the average of the dependent variables ####
est = data %>% bind_rows() %>% group_by(var) %>% 
  summarise(avg_dependent = mean(value, na.rm = T), 
            sd_dependent = sd(value, na.rm = T)) %>% 
  left_join(est, .)

#### Add the number of periods and country pairs ####
est = data %>% bind_rows() %>% group_by(var) %>% 
  summarise(n.pairs = length(unique(pair)), 
            n.periods = length(unique(year))) %>% 
  left_join(est, .)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/zip_pooled_glmm.rds")


#### --------------------------------------------------------------------- #### 
#### ZIP for the Effect of RLM on material- stacked trade values  ####
#### --------------------------------------------------------------------- ####

#### Set working directory ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(glmmTMB)
library(parallel)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#### Load the data set ####
baci = read_rds("02_gen/01_trade/baci_raw.rds")
miso = read_rds("02_gen/02_miso/raw_material_imbalance.rds")
macro = read_rds("02_gen/03_macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Aggregate the baci data to total values ####
baci = baci %>% group_by(year, from, to, prod_class) %>% 
  summarise(value = sum(value), volume = sum(volume))

#### Exploit the BACI data to include zeros ####
baci_full <- baci %>% ungroup() %>% 
  complete(year,from, to, prod_class, fill = list(volume = 0, value = 0)) %>% 
  filter(from != to)  

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11|F_11_12", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste)/1000,
         `F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000,
         `F78-F1112` = (F_7_8_prod_finals-F_11_12_waste_recov_domest)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% 
  summarise(`F78-F89` = sum(`F78-F89`),
            `F78-F1011` = sum(`F78-F1011`),
            `F78-F1112` = sum(`F78-F1112`))

#### Add the material imbalance to the waste data (destination country) ####
data = left_join(baci_full, miso%>% 
                   select(year, prod_class, to = country, `F78-F89-D`= `F78-F89`, 
                          `F78-F1011-D`= `F78-F1011`, `F78-F1112-D`= `F78-F1112`))


#### Add the material imbalance to the waste data (ORIGIN country) ####
data = left_join(data, miso%>% 
                   select(year, prod_class, from = country, `F78-F89-O`= `F78-F89`, 
                          `F78-F1011-O`= `F78-F1011`, `F78-F1112-O`= `F78-F1112`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Restrict the data to only countries in the MISO data ####
data = filter(data, from %in% unique(miso$country))
data = filter(data, to %in% unique(miso$country))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb_f89 = abs(data$`F78-F89-D` - data$`F78-F89-O`) %>% log(.)
data$net_imb_f1011 = abs(data$`F78-F1011-D` - data$`F78-F1011-O`) %>% log(.)
data$net_imb_f1112 = abs(data$`F78-F1112-D` - data$`F78-F1112-O`) %>% log(.)

#### Transform from wide to long format ####
data = select(data, -c(`F78-F89-D`:`F78-F1112-O`))
data = gather(data, var, imbalance, c(net_imb_f89, net_imb_f1011, net_imb_f1112))

#### Take the log value of net imbalance ####
data$volume = as.integer(data$volume)

#### Estimate the de-meaned values ####
data <- data  %>%
  
  group_by(var,prod_class) %>%
  mutate(mean_prod = mean(volume, na.rm = TRUE)) %>% 
  
  group_by(pair, var, prod_class) %>%
  mutate(mean_pair = mean(volume, na.rm = TRUE)) %>% 
  
  group_by(year, var, prod_class) %>%
  mutate(mean_year = mean(volume, na.rm = TRUE)) %>%
  
  group_by(from, year, var, prod_class) %>%
  mutate(mean_from_year = mean(volume, na.rm = TRUE)) %>%
  
  group_by(to, year, var, prod_class) %>%
  mutate(mean_to_year = mean(volume, na.rm = TRUE)) %>%
  
  mutate(demeaned_a = volume - mean_prod,
         demeaned_b = volume - mean_pair - mean_year,
         demeaned_c = volume  - mean_pair - mean_year - mean_from_year - mean_to_year) %>% 
  
  mutate_at(vars(demeaned_a:demeaned_c), function(x) x = as.integer(x)) %>% 
  mutate_at(vars(demeaned_a:demeaned_c), function(x) x = x + abs(min(x)))

#### Exclude observations with zero imbalance ####
# 32 obs only from F89
data = filter(data, is.infinite(imbalance) == F)

####  Define the bootstrap function ####
fn <- function(data, formula, ziformula = ~., family = poisson(link = "log"), 
               n_boot = 250, cluster_var = "pair", n_cores = detectCores() - 1) {
  
  # Ensure the cluster variable is a factor
  data[[cluster_var]] <- as.factor(data[[cluster_var]])
  
  # Fit the original model
  original_model <- glmmTMB(formula = formula,ziformula = ziformula,
                            family = family,data = data,REML = TRUE)
  
  # Extract original model components
  original_results <- list(
    conditional = tidy(original_model, component = "cond") %>% 
      mutate(model = "conditional"),
    
    zero_inflated = tidy(original_model, component = "zi") %>%
      mutate(model = "zero_inflated")) %>%
    
    bind_rows(.id = NULL) %>%
    select(model, term, estimate, std.error, statistic, p.value)
  
  # Add the AIC nad nobs 
  original_results$nobs = nobs(original_model)
  original_results$aic = AIC(original_model)
  
  # Function to perform a single bootstrap iteration
  single_bootstrap <- function(i) {
    # Sample clusters with replacement
    sampled_clusters <- sample(levels(data[[cluster_var]]), 
                               size = length(levels(data[[cluster_var]])), 
                               replace = TRUE)
    
    # Create bootstrap sample
    boot_sample <- data %>% filter(!!sym(cluster_var) %in% sampled_clusters)
    
    # Fit the model on the bootstrap sample
    boot_model <- tryCatch(
      glmmTMB(formula = formula, ziformula = ziformula, family = family,
              data = boot_sample, REML = TRUE
      ),
      error = function(e) NULL
    )
    
    # Extract estimates if model fitted successfully
    if (!is.null(boot_model)) {
      list(conditional = tidy(boot_model, component = "cond") %>% 
             mutate(model = "conditional"),
           
           zero_inflated = tidy(boot_model, component = "zi") %>%
             mutate(model = "zero_inflated")) %>%
        bind_rows(.id = NULL)
    } else {
      # Return NA rows if the model failed
      terms <- all.vars(formula)
      tibble(model = rep(c("conditional", "zero_inflated"), each = length(terms)),
             term = rep(terms, times = 2),
             estimate = NA, std.error = NA, statistic = NA, p.value = NA
      )
    }
  }
  
  # Perform bootstrapping using mclapply
  boot_list <- mclapply(1:n_boot, single_bootstrap, mc.cores = n_cores)
  
  # Combine all bootstrap results into one data frame
  boot_combined <- bind_rows(boot_list) %>%
    filter(!is.na(estimate))
  
  # Calculate bootstrapped standard errors
  boot_se <- boot_combined %>%
    group_by(model, term) %>%
    summarize(boot_se = sd(estimate, na.rm = TRUE), .groups = "drop")
  
  # Merge original estimates with bootstrapped SEs
  final_results <- original_results %>% select(-std.error) %>% 
    left_join(boot_se, by = c("model", "term")) %>%
    mutate(
      statistic = ifelse(!is.na(boot_se), estimate / boot_se, NA),
      p.value = ifelse(!is.na(statistic), 2 * (1 - pnorm(abs(statistic))), NA)
    )
  
  return(final_results)
}

#### Split the data by imbalance ####
data = split(data, f = data$var)

#### Set seed for reproducibility ####
set.seed(123)

#### Run the raw model ####
est_raw <- lapply(data, function(x) lapply(x, function(x)
  fn(data = x,  formula = demeaned_a ~ imbalance, 
     ziformula = ~ imbalance, family = poisson(link = "log"), n_boot = 250,                  
     cluster_var = "pair"))); est_raw

#### Run the gravity model ####
est_gravity <- lapply(data, function(x) lapply(x, function(x)
  
  fn(data = x,  formula = demeaned_a ~ imbalance + log(dist) + log(gdp_pair) +
       eu_o + eu_d + contig + diplo_disagreement +
       comlang_off + comcol + fta_wto, ziformula = ~. , 
     
     family = poisson(link = "log"), n_boot = 250,                  
     cluster_var = "pair"))); est_gravity

#### Run the TWFEs model ####
est_twfe <- lapply(data, function(x) lapply(x, function(x)
  
  fn(data = x,  formula = demeaned_b ~ imbalance, 
     
     ziformula = ~ imbalance, 
     
     family = poisson(link = "log"), n_boot = 250,                  
     cluster_var = "pair"))); est_twfe

#### Run the de-trend model ####
est_trend <- lapply(data, function(x) lapply(x, function(x)
  
  fn(data = x,  formula = demeaned_c ~ imbalance, 
     
     ziformula = ~ imbalance, 
     
     family = poisson(link = "log"), n_boot = 250,                  
     cluster_var = "pair"))); est_trend

# View the final bootstrapped standard errors and statistics
#### Put the estimates together####
est = list(raw = est_raw, gravity = est_gravity, twfe = est_twfe, trend = est_trend)

#### Bind the list elements ####
est = lapply(est, function(x) lapply(x, function(x) lapply(x, bind_rows))) %>% 
  lapply(., function(x) lapply(x, bind_rows, .id = "product")) %>% 
  lapply(., bind_rows, .id = "var") %>% 
  bind_rows(., .id = "specification")

#### Add the average of the outcome variables ####
est = lapply(data, bind_rows) %>% bind_rows() %>% group_by(product = prod_class, var) %>% 
  summarise(avg_causal = mean(exp(imbalance), na.rm = T), 
            sd_causal = sd(exp(imbalance), na.rm = T)) %>% 
  left_join(est, .)

#### Add the average of the dependent variables ####
est = lapply(data, bind_rows) %>% bind_rows()  %>% group_by(product = prod_class,var) %>% 
  summarise(avg_dependent = mean(value, na.rm = T), 
            sd_dependent = sd(value, na.rm = T)) %>% 
  left_join(est, .)

#### Add the number of periods and country pairs ####
est = lapply(data, bind_rows) %>% bind_rows() %>% group_by(product = prod_class,var) %>% 
  summarise(n.pairs = length(unique(pair)), 
            n.periods = length(unique(year))) %>% 
  left_join(est, .)

#### Check the data set ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/zip_stacked_glmm.rds")


#### --------------------------------------------------------------------- #### 
#### ZIP for the Effect of RLM on material- stacked trade values  ####
#### --------------------------------------------------------------------- ####

#### Set working directory ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(broom.mixed)
library(glmmTMB)
library(parallel)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")
conflict_prefer("summarize", "dplyr")

#### Load the data set ####
baci = read_rds("02_gen/01_trade/baci_raw.rds")
miso = read_rds("02_gen/02_miso/raw_material_imbalance.rds")
macro = read_rds("02_gen/03_macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Aggregate the baci data to total values ####
baci = baci %>% group_by(year, from, to, prod_class) %>% 
  summarise(value = sum(value), volume = sum(volume))

#### Exploit the BACI data to include zeros ####
baci_full <- baci %>% ungroup() %>% 
  complete(year,from, to, prod_class, fill = list(volume = 0, value = 0)) %>% 
  filter(from != to)  

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11|F_11_12", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste)/1000,
         `F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000,
         `F78-F1112` = (F_7_8_prod_finals-F_11_12_waste_recov_domest)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, prod_class, country) %>% 
  summarise(`F78-F89` = sum(`F78-F89`),
            `F78-F1011` = sum(`F78-F1011`),
            `F78-F1112` = sum(`F78-F1112`))

#### Add the material imbalance to the waste data (destination country) ####
data = left_join(baci_full, miso%>% 
                   select(year, prod_class, to = country, `F78-F89-D`= `F78-F89`, 
                          `F78-F1011-D`= `F78-F1011`, `F78-F1112-D`= `F78-F1112`))


#### Add the material imbalance to the waste data (ORIGIN country) ####
data = left_join(data, miso%>% 
                   select(year, prod_class, from = country, `F78-F89-O`= `F78-F89`, 
                          `F78-F1011-O`= `F78-F1011`, `F78-F1112-O`= `F78-F1112`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Restrict the data to only countries in the MISO data ####
data = filter(data, from %in% unique(miso$country))
data = filter(data, to %in% unique(miso$country))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb_f89 = abs(data$`F78-F89-D` - data$`F78-F89-O`) %>% log(.)
data$net_imb_f1011 = abs(data$`F78-F1011-D` - data$`F78-F1011-O`) %>% log(.)
data$net_imb_f1112 = abs(data$`F78-F1112-D` - data$`F78-F1112-O`) %>% log(.)

#### Transform from wide to long format ####
data = select(data, -c(`F78-F89-D`:`F78-F1112-O`))
data = gather(data, var, imbalance, c(net_imb_f89, net_imb_f1011, net_imb_f1112))

#### Take the log value of net imbalance ####
data$volume = as.integer(data$volume)

#### Estimate the de-meaned values ####
data <- data  %>%
  group_by(pair, var, prod_class) %>%
  mutate(mean_pair = mean(volume, na.rm = TRUE)) %>% 
  
  group_by(year, var, prod_class) %>%
  mutate(mean_year = mean(volume, na.rm = TRUE)) %>%
  
  group_by(from, year, var, prod_class) %>%
  mutate(mean_from_year = mean(volume, na.rm = TRUE)) %>%
  
  group_by(to, year, var, prod_class) %>%
  mutate(mean_to_year = mean(volume, na.rm = TRUE)) %>%
  
  mutate(demeaned_a = volume - mean_pair - mean_year,
         demeaned_b = volume  - mean_pair - mean_year - mean_from_year - mean_to_year) %>% 
  
  mutate_at(vars(demeaned_a:demeaned_b), function(x) x = as.integer(x)) %>% 
  mutate_at(vars(demeaned_a:demeaned_b), function(x) x = x + abs(min(x)))

#### Exclude observations with zero imbalance ####
# 32 obs only from F89
data = filter(data, is.infinite(imbalance) == F)

####  Define the bootstrap function ####
fn <- function(data, formula, ziformula = ~., family = poisson(link = "log"), 
               n_boot = 250, cluster_var = "pair", n_cores = detectCores() - 1) {
  
  # Ensure the cluster variable is a factor
  data[[cluster_var]] <- as.factor(data[[cluster_var]])
  
  # Fit the original model
  original_model <- glmmTMB(formula = formula,ziformula = ziformula,
                            family = family,data = data,REML = TRUE)
  
  # Extract original model components
  original_results <- list(
    conditional = tidy(original_model, component = "cond") %>% 
      mutate(model = "conditional"),
    
    zero_inflated = tidy(original_model, component = "zi") %>%
      mutate(model = "zero_inflated")) %>%
    
    bind_rows(.id = NULL) %>%
    select(model, term, estimate, std.error, statistic, p.value)
  
  # Add the AIC nad nobs 
  original_results$nobs = nobs(original_model)
  original_results$aic = AIC(original_model)
  
  # Function to perform a single bootstrap iteration
  single_bootstrap <- function(i) {
    # Sample clusters with replacement
    sampled_clusters <- sample(levels(data[[cluster_var]]), 
                               size = length(levels(data[[cluster_var]])), 
                               replace = TRUE)
    
    # Create bootstrap sample
    boot_sample <- data %>% filter(!!sym(cluster_var) %in% sampled_clusters)
    
    # Fit the model on the bootstrap sample
    boot_model <- tryCatch(
      glmmTMB(formula = formula, ziformula = ziformula, family = family,
              data = boot_sample, REML = TRUE
      ),
      error = function(e) NULL
    )
    
    # Extract estimates if model fitted successfully
    if (!is.null(boot_model)) {
      list(conditional = tidy(boot_model, component = "cond") %>% 
             mutate(model = "conditional"),
           
           zero_inflated = tidy(boot_model, component = "zi") %>%
             mutate(model = "zero_inflated")) %>%
        bind_rows(.id = NULL)
    } else {
      # Return NA rows if the model failed
      terms <- all.vars(formula)
      tibble(model = rep(c("conditional", "zero_inflated"), each = length(terms)),
             term = rep(terms, times = 2),
             estimate = NA, std.error = NA, statistic = NA, p.value = NA
      )
    }
  }
  
  # Perform bootstrapping using mclapply
  boot_list <- mclapply(1:n_boot, single_bootstrap, mc.cores = n_cores)
  
  # Combine all bootstrap results into one data frame
  boot_combined <- bind_rows(boot_list) %>%
    filter(!is.na(estimate))
  
  # Calculate bootstrapped standard errors
  boot_se <- boot_combined %>%
    group_by(model, term) %>%
    summarize(boot_se = sd(estimate, na.rm = TRUE), .groups = "drop")
  
  # Merge original estimates with bootstrapped SEs
  final_results <- original_results %>% select(-std.error) %>% 
    left_join(boot_se, by = c("model", "term")) %>%
    mutate(
      statistic = ifelse(!is.na(boot_se), estimate / boot_se, NA),
      p.value = ifelse(!is.na(statistic), 2 * (1 - pnorm(abs(statistic))), NA)
    )
  
  return(final_results)
}

#### Split the data by imbalance ####
data = split(data, f = data$var)

#### Set seed for reproducibility ####
set.seed(123)

#### Run the raw model ####
est_raw <- lapply(data, function(x) 
  fn(data = x,  formula = volume ~ imbalance, 
     ziformula = ~ imbalance, family = poisson(link = "log"), n_boot = 250,                  
     cluster_var = "pair")); est_raw

#### Run the gravity model ####
est_gravity <- lapply(data, function(x)
  
  fn(data = x,  formula = volume ~ imbalance + log(dist) + log(gdp_pair) +
                    eu_o + eu_d + contig + diplo_disagreement +
                    comlang_off + comcol + fta_wto, ziformula = ~. , 
                  
                  family = poisson(link = "log"), n_boot = 250,                  
                  cluster_var = "pair")); est_gravity

#### Run the TWFEs model ####
est_twfe <- lapply(data, function(x) 
  
  fn(data = x,  formula = demeaned_a ~ imbalance, 
               
               ziformula = ~ imbalance, 
               
               family = poisson(link = "log"), n_boot = 250,                  
               cluster_var = "pair")); est_twfe

#### Run the de-trend model ####
est_trend <- lapply(data, function(x) 
  
  fn(data = x,  formula = demeaned_b ~ imbalance, 
                
                ziformula = ~ imbalance, 
                
                family = poisson(link = "log"), n_boot = 250,                  
                cluster_var = "pair")); est_trend

# View the final bootstrapped standard errors and statistics
#### Put the estimates together####
est = list(raw = est_raw, gravity = est_gravity, twfe = est_twfe, trend = est_trend)

#### Bind the list elements ####
est = lapply(est., function(x) lapply(x, bind_rows)) %>% 
  lapply(., bind_rows, .id = "var") %>% 
  bind_rows(., .id = "specification")

#### Add the average of the outcome variables ####
est = data %>% bind_rows() %>% group_by(var) %>% 
  summarise(avg_causal = mean(exp(imbalance), na.rm = T), 
            sd_causal = sd(exp(imbalance), na.rm = T)) %>% 
  left_join(est, .)

#### Add the average of the dependent variables ####
est = data %>% bind_rows()  %>% group_by(var) %>% 
  summarise(avg_dependent = mean(value, na.rm = T), 
            sd_dependent = sd(value, na.rm = T)) %>% 
  left_join(est, .)

#### Add the number of periods and country pairs ####
est = data %>% bind_rows() %>% group_by(var) %>% 
  summarise(n.pairs = length(unique(pair)), 
            n.periods = length(unique(year))) %>% 
  left_join(est, .)

#### Check the data set ####
head(est)

#### Save the Poisson plot ####
write_rds(est, file = "02_gen/04_results/zip_stacked_glmm.rds")

