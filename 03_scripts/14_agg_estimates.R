#### ..................................................................... ####
####      Aggregate estimates across all model specifications              ####
#### ..................................................................... ####

library(tidyverse)

# Load estimates
data = list(pois_stacked = read_rds("02_gen/04_results/poisson_stacked.rds"),
            pois_pooled = read_rds("02_gen/04_results/poisson_pooled.rds"),
            pois_split = read_rds("02_gen/04_results/poisson_split.rds"),

            ols_stacked = read_rds("02_gen/04_results/ols_stacked.rds"),
            ols_pooled = read_rds("02_gen/04_results/ols_pooled.rds"),
            ols_split = read_rds("02_gen/04_results/ols_split.rds"),

            lols_stacked = read_rds("02_gen/04_results/log_ols_stacked.rds"),
            lols_pooled = read_rds("02_gen/04_results/log_ols_pooled.rds"),
            lols_split = read_rds("02_gen/04_results/log_ols_split.rds"))

# Bind the list elements together
data = bind_rows(data, .id = "model")

# Extract estimator and sample labels
data = mutate(data, sample = gsub(".*_", "", model))
data = mutate(data, estimator = gsub("_.*", "", model))
data = select(data, -model)

# Organize columns
data = select(data, c(estimator, sample, spec, term, imb, prod_class, estimate:r2, avg_causal:n.periods))

#### Save the aggregated estimates ####
write_rds(data, file = "02_gen/04_results/all_est_log_diff.rds")

cat("14_agg_estimates.R completed successfully.\n")
