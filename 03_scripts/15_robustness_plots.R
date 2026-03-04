#### ..................................................................... ####
####              Robustness plots (Figures A3, A4, A5)                     ####
#### ..................................................................... ####

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load the aggregated estimates ####
data = read_rds("02_gen/04_results/all_est_log_diff.rds")

#### Keep only the imbalance coefficient ####
data = filter(data, term == "imbalance")

#### Compute 90% confidence intervals ####
data = mutate(data,
              ci_lo = estimate - qnorm(0.95) * std.error,
              ci_hi = estimate + qnorm(0.95) * std.error)

#### Clean labels ####
data = mutate(data,
              spec = toupper(spec),
              spec = factor(spec, levels = c("RAW", "GRAVITY", "TWFE")),
              estimator_label = case_when(
                estimator == "pois" ~ "PPMLE",
                estimator == "lols" ~ "LOG-OLS",
                estimator == "ols"  ~ "OLS",
                TRUE ~ toupper(estimator)),
              imb_label = case_when(
                imb == "net_imb_f1011" ~ "F78-F1011",
                imb == "net_imb_f89"   ~ "F78-F89",
                imb == "net_imb_f1112" ~ "F78-F1112",
                TRUE ~ imb))

#### Color palette ####
pal = natparks.pals("Yellowstone")


#### _____________________________________________________________________ ####
#### Figure A3: Imbalance definitions, pooled sample, PPMLE only ####
#### _____________________________________________________________________ ####

plot_a3 = data %>%
  filter(sample == "pooled",
         estimator == "pois",
         imb %in% c("net_imb_f1011", "net_imb_f89")) %>%
  filter(!is.na(spec))

ggplot(plot_a3, aes(x = estimate, y = spec, color = imb_label)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                width = 0, linewidth = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(x = estimate),
             position = position_dodge(width = 0.4),
             shape = "|", size = 3) +
  scale_color_manual(values = pal[c(1, 2)]) +
  labs(x = "Point Estimate and 90% CI", y = "") +
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file = paste0(fig_dir, "rob_imbalance_pooled.png"),
       width = 8, height = 4, bg = "transparent")


#### _____________________________________________________________________ ####
#### Figure A4: Estimators, pooled sample (LOG-OLS, PPMLE, ZIP)           ####
#### _____________________________________________________________________ ####

#### Load ZIP pooled results from per-specification files ####
zip_pooled = bind_rows(
  read_rds("02_gen/04_results/zip_pooled_raw_log_diff.rds"),
  read_rds("02_gen/04_results/zip_pooled_gravity_log_diff.rds"),
  read_rds("02_gen/04_results/zip_pooled_twfe_log_diff.rds")
) %>%
  filter(term == "imbalance", model == "conditional", imb == "net_imb_f1011") %>%
  transmute(spec,
            imb,
            estimate,
            std.error = boot_se,
            estimator = "zip",
            estimator_label = "ZIP",
            ci_lo = estimate - qnorm(0.95) * boot_se,
            ci_hi = estimate + qnorm(0.95) * boot_se)

plot_a4 = data %>%
  filter(sample == "pooled",
         imb == "net_imb_f1011",
         estimator %in% c("pois", "lols"),
         !is.na(spec)) %>%
  select(spec, imb, estimate, std.error, estimator, estimator_label, ci_lo, ci_hi) %>%
  bind_rows(zip_pooled) %>%
  mutate(estimator_label = factor(estimator_label,
                                  levels = c("LOG-OLS", "PPMLE", "ZIP")),
         spec = toupper(spec),
         spec = factor(spec, levels = c("RAW", "GRAVITY", "TWFE")))

ggplot(plot_a4, aes(x = estimate, y = spec, color = estimator_label)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                width = 0, linewidth = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(x = estimate),
             position = position_dodge(width = 0.4),
             shape = "|", size = 3) +
  scale_color_manual(values = pal[c(1, 2, 3)]) +
  labs(x = "Point Estimate and 90% CI", y = "") +
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file = paste0(fig_dir, "rob_estimators_pooled.png"),
       width = 8, height = 4, bg = "transparent")


#### _____________________________________________________________________ ####
#### Figure A5: Estimators, split by product (LOG-OLS, PPMLE, ZIP)        ####
#### _____________________________________________________________________ ####

#### Load ZIP split results from per-specification files ####
zip_split = bind_rows(
  read_rds("02_gen/04_results/zip_raw_glmm_split_log_diff.rds"),
  read_rds("02_gen/04_results/zip_twfe_glmm_split_log_diff.rds")
) %>%
  filter(term == "imbalance", model == "conditional", imb == "net_imb_f1011") %>%
  transmute(spec,
            imb,
            prod_class = product,
            estimate,
            std.error = boot_se,
            estimator = "zip",
            estimator_label = "ZIP",
            ci_lo = estimate - qnorm(0.95) * boot_se,
            ci_hi = estimate + qnorm(0.95) * boot_se)

plot_a5 = data %>%
  filter(sample == "split",
         imb == "net_imb_f1011",
         estimator %in% c("pois", "lols"),
         !is.na(spec), !is.na(prod_class)) %>%
  select(spec, imb, prod_class, estimate, std.error, estimator, estimator_label, ci_lo, ci_hi) %>%
  bind_rows(zip_split) %>%
  mutate(estimator_label = factor(estimator_label,
                                  levels = c("LOG-OLS", "PPMLE", "ZIP")),
         spec = toupper(spec),
         spec = factor(spec, levels = c("RAW", "GRAVITY", "TWFE")))

#### Clean product labels ####
plot_a5$prod_class = gsub("_", " & ", plot_a5$prod_class) %>% str_to_sentence(.)
plot_a5$prod_class = factor(plot_a5$prod_class,
                            levels = c("Aluminum", "Glass", "Iron & steel",
                                       "Paper", "Plastic"))

ggplot(plot_a5, aes(x = estimate, y = spec, color = estimator_label)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                width = 0, linewidth = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(x = estimate),
             position = position_dodge(width = 0.4),
             shape = "|", size = 3) +
  facet_wrap(~prod_class, scales = "free_x", ncol = 3) +
  scale_color_manual(values = pal[c(1, 2, 3)]) +
  labs(x = "Point Estimate and 90% CI", y = "") +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file = paste0(fig_dir, "rob_estimators_split.png"),
       width = 10, height = 6, bg = "transparent")

cat("15_robustness_plots.R completed successfully.\n")
