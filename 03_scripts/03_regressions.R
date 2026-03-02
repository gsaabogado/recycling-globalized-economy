#### ..................................................................... #### 
####                          Main Regressions                             ####
#### ..................................................................... #### 
#### Add something about relative distance
#### --------------------------------------------------------------------- #### 
#### Elasticity between recycling rates and foreign waste trade EU (Pooled) ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Only keep total waste exports #####
data = filter(data, prod_class == "total")

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Get the data on waste generation ####
gen = filter(waste, prod_class == "packaging_waste", unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class == "packaging_waste", unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
data = left_join(waste, agg) %>% left_join(., gen)
data = mutate(data, log_volume = log(volume))

#### Compute the elasticities ####
est = list(feols(log(value) ~ log_volume, cluster = "country", data = data, weights = ~gdp),
           
           feols(log(value) ~ log_volume + log(waste_gen) + log(gdpcap), cluster = "country", 
                 data = data, weights = ~gdp),
           
           feols(log(value) ~ log_volume + log(waste_gen) + log(gdpcap) | 
                   country, cluster = "country", data = data, weights = ~gdp),
           
           feols(log(value) ~ log_volume + log(waste_gen) + log(gdpcap) |
                   country + year, cluster = "country",  data = data, weights = ~gdp))

#### Create the latex table ####
esttable(est)

#### Extract the coefficients ####
sum = lapply(est, function(x) 
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>% 
  bind_rows(., .id = "spec") %>% mutate(var = "pooled") %>% mutate(estimator = "ols")

#### Include the number of countries and years ####
sum$N.countries = length(unique(data$country))
sum$N.periods = length(unique(data$year))

#### Save the data set ####
write_rds(sum, file = "02_gen/04_results/rr_trade_elasticity_pooled.rds")


#### --------------------------------------------------------------------- #### 
#### Elasticity between Recycling and foreign waste trade EU (IV-Pooled) ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("02_gen/01_trade/gravity_data.rds") %>% filter(year < 2022)
costs = read_rds("02_gen/wb_tradecost.rds") %>% filter(year < 2022)

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Only keep total waste exports #####
data = filter(data, prod_class == "total")
data = left_join(data, costs)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Create the three different instruments ####
trade_cost = data %>% group_by(year, country = from) %>% 
  mutate_at(vars(volume, manuf_tradeflow_baci), function(x) x = ifelse(is.na(x), 0, x)) %>% 
  summarise(trade_cost_raw = mean(tradecost, na.rm = T), 
            trade_cost_wg = weighted.mean(tradecost, w = volume, na.rm = T),
            trade_cost_wm = weighted.mean(tradecost, w = manuf_tradeflow_baci, na.rm = T))


#### Get the data on waste generation ####
gen = filter(waste, prod_class == "packaging_waste", unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class == "packaging_waste", unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
test = left_join(waste, agg) %>% left_join(., gen) %>% left_join(., trade_cost)
test = mutate(test, log_volume = log(volume))

#### Compute the elasticities ####
est = list(feols(log(value) ~ 1| log_volume ~ log(trade_cost_wm), 
                 cluster = "country", data = test, weights = ~gdp),
           
           feols(log(value) ~ 1 + log(waste_gen) + log(gdpcap)| 
                   log_volume ~ log(trade_cost_wm), cluster = "country", 
                 data = test, weights = ~gdp),
           
           feols(log(value) ~ 1 + log(waste_gen) + log(gdpcap) | 
                   country| log_volume ~ log(trade_cost_wm), 
                 cluster = "country", data = test, weights = ~gdp),
           
           feols(log(value) ~ 1 + log(waste_gen) + log(gdpcap) |
                   country + year| log_volume ~ log(trade_cost_wm), 
                 cluster = "country",  data = test, weights = ~gdp))

#### Create the latex table ####
esttable(est)

#### Extract the coefficients ####
sum = lapply(est, function(x) 
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"),
             f_stat = fitstat(x, "ivf")[[1]]$stat)) %>% 
  bind_rows(., .id = "spec") %>% mutate(var = "pooled") %>% mutate(estimator = "iv")

#### Include the number of countries and years ####
sum$N.countries = length(unique(test$country))
sum$N.periods = length(unique(test$year))

#### Save the data set ####
write_rds(sum, file = "02_gen/04_results/rr_trade_elasticity_pooled_iv.rds")

#### --------------------------------------------------------------------- #### 
#### Elasticity between Recycling and waste trade EU (IV-Pooled-Restricted) ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("02_gen/01_trade/gravity_data.rds") %>% filter(year < 2022)
costs = read_rds("02_gen/wb_tradecost.rds") %>% filter(year < 2022)

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Only keep total waste exports #####
data = filter(data, prod_class == "total")
data = left_join(data, costs)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Create the three different instruments ####
trade_cost = data %>% group_by(year, country = from) %>% 
  mutate_at(vars(volume, manuf_tradeflow_baci), function(x) x = ifelse(is.na(x), 0, x)) %>% 
  summarise(trade_cost_raw = mean(tradecost, na.rm = T), 
            trade_cost_wg = weighted.mean(tradecost, w = volume, na.rm = T),
            trade_cost_wm = weighted.mean(tradecost, w = manuf_tradeflow_baci, na.rm = T))


#### Get the data on waste generation ####
gen = filter(waste, prod_class == "packaging_waste", unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class == "packaging_waste", unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
test = left_join(waste, agg) %>% left_join(., gen) %>% left_join(., trade_cost)
test = mutate(test, log_volume = log(volume))
test = filter(test, year < 2018)

#### Compute the elasticities with the IV model ####
iv = list(feols(log(value) ~ 1| log_volume ~ log(trade_cost_wm), 
                 cluster = "country", data = test, weights = ~gdp),
           
           feols(log(value) ~ 1 + log(waste_gen) + log(gdpcap)| 
                   log_volume ~ log(trade_cost_wm), cluster = "country", 
                 data = test, weights = ~gdp),
           
           feols(log(value) ~ 1 + log(waste_gen) + log(gdpcap) | 
                   country| log_volume ~ log(trade_cost_wm), 
                 cluster = "country", data = test, weights = ~gdp),
           
           feols(log(value) ~ 1 + log(waste_gen) + log(gdpcap) |
                   country + year| log_volume ~ log(trade_cost_wm), 
                 cluster = "country",  data = test, weights = ~gdp))

#### Compute the elasticities with the OLS model ####
ols = list(feols(log(value) ~ log_volume, 
                cluster = "country", data = test, weights = ~gdp),
          
          feols(log(value) ~ log_volume + log(waste_gen) + log(gdpcap), 
                cluster = "country", 
                data = test, weights = ~gdp),
          
          feols(log(value) ~ log_volume + log(waste_gen) + log(gdpcap) | 
                  country, 
                cluster = "country", data = test, weights = ~gdp),
          
          feols(log(value) ~ log_volume + log(waste_gen) + log(gdpcap) |
                  country + year, 
                cluster = "country",  data = test, weights = ~gdp))


#### Extract the coefficients ####
sum_iv = lapply(iv, function(x) 
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"),
             f_stat = fitstat(x, "ivf")[[1]]$stat)) %>% 
  bind_rows(., .id = "spec") %>% mutate(var = "pooled") %>% 
  mutate(estimator = "iv")

#### Extract the coefficients ####
sum_ols = lapply(ols, function(x) 
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>% 
  bind_rows(., .id = "spec") %>% mutate(var = "pooled") %>% 
  mutate(estimator = "ols")

#### Add both results together ####
sum = bind_rows(list(sum_iv, sum_ols))

#### Include the number of countries and years ####
sum$N.countries = length(unique(test$country))
sum$N.periods = length(unique(test$year))

#### Save the data set ####
write_rds(sum, file = "02_gen/04_results/rr_trade_elasticity_pooled_restricted.rds")


#### --------------------------------------------------------------------- #### 
#### Elasticity between recycling and foreign waste trade EU (by material) ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Only keep total waste exports #####
data = filter(data, prod_class != "total")
data$prod_class = gsub("steel_iron|aluminum","metal",data$prod_class)
data$prod_class = gsub("paper","paper_cardboard",data$prod_class)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from, prod_class) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Get the data on waste generation ####
gen = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value, prod_class)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
data = left_join(waste, agg) %>% left_join(., gen)
data = mutate(data, log_volume = log(volume))

#### Compute the elasticities ####
est = feols(log(value) ~ log_volume + log(waste_gen) + gdpcap| 
                   country + year, cluster = "country", data = data, 
                 split = ~prod_class, weight = ~gdp)

#### Review the results ####
etable(est)

#### Extract the coefficients ####
sum = lapply(est, function(x) 
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>% 
  bind_rows(., .id = "var") %>% mutate(spec = "4") %>% 
  mutate(estimator = "ols")

#### Only keep the product class name ####
sum = mutate(sum, var = gsub(".*:", "", var))

#### Include the number of countries and years ####
sum$N.countries = length(unique(data$country))
sum$N.periods = length(unique(data$year))

#### Save the data set ####
write_rds(sum, file = "02_gen/04_results/rr_trade_elasticity_prod.rds")



#### --------------------------------------------------------------------- #### 
#### Elasticity between recycling and waste trade EU (by material IV) ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
costs = read_rds("02_gen/wb_tradecost.rds") %>% filter(year < 2022)
data = read_rds("02_gen/01_trade/gravity_data.rds") %>% filter(year < 2022)

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Only keep total waste exports #####
data = filter(data, prod_class != "total")
data$prod_class = gsub("steel_iron|aluminum","metal",data$prod_class)
data$prod_class = gsub("paper","paper_cardboard",data$prod_class)
data = left_join(data, costs)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from, prod_class) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Create the three different instruments ####
trade_cost = data %>% group_by(year, country = from) %>% 
  mutate_at(vars(volume, manuf_tradeflow_baci), function(x) x = ifelse(is.na(x), 0, x)) %>% 
  summarise(trade_cost_raw = mean(tradecost, na.rm = T), 
            trade_cost_wg = weighted.mean(tradecost, w = volume, na.rm = T),
            trade_cost_wm = weighted.mean(tradecost, w = manuf_tradeflow_baci, na.rm = T))

#### Get the data on waste generation ####
gen = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value, prod_class)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
data = left_join(waste, agg) %>% left_join(., gen) %>% left_join(., trade_cost)
data = mutate(data, log_volume = log(volume))

#### Compute the elasticities ####
est = feols(log(value) ~ log(waste_gen) + log(gdpcap) |
                   country + year | log_volume ~ log(trade_cost_raw), 
                 cluster = "country", data = data, weights = ~gdp, 
            split = ~prod_class)

#### Check the results ####
etable(est)

#### Extract the coefficients ####
sum = lapply(est, function(x) 
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"),
             f_stat = fitstat(x, "ivf")[[1]]$stat)) %>% 
  bind_rows(., .id = "var") %>% mutate(spec = "4") %>% 
  mutate(estimator = "iv")

#### Only keep the product class name ####
sum = mutate(sum, var = gsub(".*:", "", var))
head(sum)
#### Include the number of countries and years ####
sum$N.countries = length(unique(data$country))
sum$N.periods = length(unique(data$year))

#### Save the data set ####
write_rds(sum, file = "02_gen/04_results/rr_trade_elasticity_material_iv.rds")



#### --------------------------------------------------------------------- #### 
#### Naive gravity model (imbalance trade adjusted)####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Create a country pair variable ####
data = mutate(data, pair = paste0(from, "_", to))

#### Check the results for total waste ####
data = filter(data, prod_class == "total")

#### Start the regressions ####
est = list(fepois(volume ~ imb_adj_o + imb_adj_d, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_adj_o + imb_adj_d + gdp_o, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_adj_o + imb_adj_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_adj_o + imb_adj_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_adj_o + imb_adj_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci|
                    year + from + to, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_adj_o + imb_adj_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist  + diplo_disagreement + manuf_tradeflow_baci|
                    year + pair, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_adj_o + imb_adj_o^2 + imb_adj_d + imb_adj_d^2+ gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist  + diplo_disagreement + manuf_tradeflow_baci|
                    year + pair, 
                  data = data, cluster = "pair"))

#### Check the estimates ####
etable(est, se.below = T)

#### --------------------------------------------------------------------- #### 
#### Naive gravity model (imbalance trade unadjusted) ####
#### --------------------------------------------------------------------- ####
# Add the interaction of EU country and recylcing rates

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))
data = mutate(data, pair = paste0(from, "_", to))
data = filter(data, prod_class == "total")

#### Start the regressions ####
est = list(fepois(volume ~ imb_trade_o + imb_trade_d, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_trade_o + imb_trade_d + gdp_o, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_trade_o + imb_trade_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_trade_o + imb_trade_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_trade_o + imb_trade_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci|
                    year + from + to, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_trade_o + imb_trade_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist  + diplo_disagreement + manuf_tradeflow_baci|
                    year + pair, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_trade_o + imb_trade_o^2 + imb_trade_d + imb_trade_d^2+ gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist  + diplo_disagreement + manuf_tradeflow_baci|
                    year + pair, 
                  data = data, cluster = "pair"))

#### Check the estimates ####
etable(est, se.below = T)

#### --------------------------------------------------------------------- #### 
#### Naive gravity model with indicator variables (imbalance trade adjusted) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Transform the zero trade volume to actual zeros ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))
data = mutate(data, pair = paste0(from, "_", to))

#### Only keep the total class ####
data = filter(data, prod_class == "total")

#### Create a negative or positive imbalance dummy ####
data = mutate(data, imb_dummy_o = ifelse(imb_adj_o > 0, 1, 0))
data = mutate(data, imb_dummy_d = ifelse(imb_adj_d > 0, 1, 0))

#### Start the regressions ####
est = list(fepois(volume ~ imb_dummy_o + imb_dummy_d, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci|
                    year + from + to, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d+ gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist  + diplo_disagreement + manuf_tradeflow_baci|
                    year + pair, 
                  data = data, cluster = "pair"))

#### Check the estimates ####
etable(est, se.below = T)



as.data.frame(e)
class(e)
t = 11870.46*(quantile(data$imb_adj_d, na.rm = T, p = seq(0, 1, 0.1))* (exp(3.06e-7) -1))
plot(t)
ggplot(data) + geom_density(aes(imb_adj_d))
mean(data$volume)
sd(data$imb_adj_d)
* 3.06e-05

summary(data$volume)
4.14


100*(exp(3.06e-7)-1)

etable(est, se.below = T)
colnames(data)
data = arrange(data, volume)
head(data)

#### --------------------------------------------------------------------- #### 
#### Naive gravity model with indicator variables (imbalance trade un-adjusted) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Transform the zero trade volume to actual zeros ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))
data = mutate(data, pair = paste0(from, "_", to))

#### Only keep the total class ####
data = filter(data, prod_class == "total")

#### Create a negative or positive imbalance dummy ####
data = mutate(data, imb_dummy_o = ifelse(imb_trade_o > 0, 1, 0))
data = mutate(data, imb_dummy_d = ifelse(imb_trade_d > 0, 1, 0))

#### Start the regressions ####
est = list(fepois(volume ~ imb_dummy_o + imb_dummy_d, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2, 
                  data = data, cluster = "pair"), 
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d + gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist + contig + diplo_disagreement +
                    comlang_off + comcol + manuf_tradeflow_baci|
                    year + from + to, 
                  data = data, cluster = "pair"),
           
           fepois(volume ~ imb_dummy_o + imb_dummy_d+ gdp_o + gdp_o^2 + gdp_d + gdp_d^2 +
                    eu_o + eu_d + dist  + diplo_disagreement + manuf_tradeflow_baci|
                    year + pair, 
                  data = data, cluster = "pair"))

#### Check the estimates ####
etable(est, se.below = T)




#### --------------------------------------------------------------------- #### 
#### Naive Balance regression ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Load the data set ####
data = read_rds("02_gen/01_trade/balance_data.rds")
data = filter(data, prod_class == "total")

# Fit the model
est <- feols(volume_bal ~ imb_adj + gdp + pop + gdp_pc + year + country, 
             data = data %>% filter(!(country %in% c("CHN", "USA", "TUR"))), 
             cluster = "country")

est
# Extract unique combinations of year and country from the original data
unique_year_country <- data %>%
  select(year, country) %>%
  distinct()

# Compute percentiles of imb_adj
percentiles <- quantile(data$imb_adj, probs = seq(0, 0.9, by = 0.05), na.rm = TRUE)

# Calculate mean values for other covariates
mean_gdp <- mean(data$gdp, na.rm = TRUE)
mean_pop <- mean(data$pop, na.rm = TRUE)
mean_gdp_pc <- mean(data$gdp_pc, na.rm = TRUE)

# Create a new data frame for prediction
pred_data <- expand.grid(imb_adj = percentiles, 
                         year = unique(data$year), 
                         country = unique(data$country),
                         gdp = mean_gdp,
                         pop = mean_pop,
                         gdp_pc = mean_gdp_pc)

# Predict fitted values and standard errors
predictions <- predict(est, newdata = pred_data, se.fit = TRUE)

# Combine predictions with percentiles
results <- data.frame(
  imb_adj = rep(percentiles, length(unique(data$year)) * length(unique(data$country))),
  fitted_values = predictions$fit,
  se = predictions$se.fit
)

# Plotting
ggplot(results, aes(x = imb_adj, y = fitted_values)) +
  geom_ribbon(aes(ymin = fitted_values - 1.96 * se, 
                  ymax = fitted_values + 1.96 * se), alpha = 0.8) +
  labs(title = "Fitted Values of volume_bal with Standard Errors",
       x = "imb_adj",
       y = "Fitted Values") +
  theme_minimal()

#### --------------------------------------------------------------------- #### 
#### Effect of the green fence and national sword (metal vs other products) ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Take away total waste exports #####
data = filter(data, prod_class != "total")

#### Change the name of the products to match with the RR data ####
data$prod_class = gsub("steel_iron|aluminum","metal",data$prod_class)
data$prod_class = gsub("paper","paper_cardboard",data$prod_class)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from, prod_class) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Extract the data on waste generation ####
gen = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value, prod_class)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
data = left_join(waste, agg) %>% left_join(., gen)
data = mutate(data, log_volume = log(volume))

#### Add the treatment indicator ####
data = mutate(data, national_sword = ifelse(year >= 2017, 1, 0))
data = mutate(data, green_fence = ifelse(year >= 2013, 1, 0))
data = mutate(data, treated = ifelse(prod_class == "metal", 0, 1))

#### Estimate the effect of the green fence ####
est_gf = list(feols(value ~  i(prod_class, ref = "paper") + green_fence*treated,
                    data = data %>% filter(year %in% c(2008:2016)), cluster = "prod_class + country"),
              
              feols(value ~ i(prod_class, ref = "paper") + green_fence*treated|
                      year + country[year], 
                    data = data %>% filter(year %in% c(2008:2016)), cluster = "prod_class"),
              
              feols(value ~ i(prod_class, ref = "paper") + green_fence*treated + 
                      log(waste_gen) + log(gdpcap)|
                      year + country[year], 
                    data = data %>% filter(year %in% c(2008:2016)), cluster = "prod_class"),
              
              feols(value ~ i(prod_class, ref = "paper") + green_fence*treated  + 
                      log(waste_gen) + log(gdpcap)|
                      year + country[year], 
                    data = data %>% filter(year %in% c(2010:2016)), cluster = "prod_class"))

#### Review the results ####
etable(est_gf)

#### Run the difference in differences ####
est_ns = list(feols(value ~ i(prod_class, ref = "paper") + national_sword*treated,
                    data = data %>% filter(year %in% c(2010:2019)), cluster = "prod_class"), 
              
              feols(value ~  i(prod_class, ref = "paper") + national_sword*treated | country[year] + year,
                    data = data %>% filter(year %in% c(2010:2019)), cluster = "prod_class"), 
              
              feols(value ~  i(prod_class, ref = "paper") + national_sword*treated + 
                      log(waste_gen) + log(gdpcap)| country[year] + year,
                    data = data %>% filter(year %in% c(2010:2019)), cluster = "prod_class"), 
              
              feols(value ~  i(prod_class, ref = "paper") + national_sword*treated + 
                      log(waste_gen) + log(gdpcap)| country[year] + year,
                    data = data %>% filter(year %in% c(2015:2019)), cluster = "prod_class"))

#### Review the results ####
etable(est_ns)

#### Bind both estimates together ###
est = list(green_fence = est_gf, national_sword = est_ns)

#### Extract the coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>% 
    bind_rows(., .id = "spec") %>% mutate(estimator = "ols")) %>% 
  bind_rows(., .id = "policy")

sum
#### Only keep the product class name ####
sum = filter(sum, grepl(":treated", term))
sum
#### Function to extract fixed effects for each model #### 
extract_info <- function(model, spec_number) {
  fixef_sizes <- if ("fixef_vars" %in% names(model)) fixef(model) else list()
  
  fe_country <- if ("country" %in% names(fixef_sizes)) length(fixef_sizes$country) else 0
  fe_year <- if ("year" %in% names(fixef_sizes)) length(fixef_sizes$year) else 0
  
  data.frame(spec = spec_number %>% as.character(.), 
             fe_country = fe_country, fe_year = fe_year)
}

#### Apply the function to each model in the list and combine results into a data frame #### 
fes <- lapply(est, function(x) 
  do.call(rbind, lapply(seq_along(x), function(i) extract_info(x[[i]], i)))) %>% 
  bind_rows(., .id = "policy")

#### Display the data frame #### 
sum = left_join(sum, fes)

#### Save the data set ####
write_rds(sum, file = "02_gen/04_results/did_china_metal.rds")


#### --------------------------------------------------------------------- #### 
#### Event-time study of the national sword policy####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(cowplot)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Take away total waste exports #####
data = filter(data, prod_class != "total")

#### Change the name of the products to match with the RR data ####
data$prod_class = gsub("steel_iron|aluminum","metal",data$prod_class)
data$prod_class = gsub("paper","paper_cardboard",data$prod_class)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = from, prod_class) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T))

#### Extract the data on waste generation ####
gen = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "kg_pc",
             operation == "generated") %>% select(country, year, waste_gen = value, prod_class)

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class %in% c("paper_cardboard", "plastic", "metal"), unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
data = left_join(waste, agg) %>% left_join(., gen)
data = mutate(data, log_volume = log(volume))

#### Add the treatment indicator ####
data = mutate(data, national_sword = ifelse(year >= 2017, 1, 0))
data = mutate(data, green_fence = ifelse(year >= 2013, 1, 0))
data = mutate(data, treated = ifelse(prod_class == "metal", 0, 1))

#### Estimate the effect of the green fence ####
est_gf = list(feols(value ~  i(year, treated, ref = "2012") + i(prod_class, ref = "paper"),
                    data = data %>% filter(year %in% c(2008:2016)), cluster = "prod_class"),
              
              feols(value ~ i(year, treated, ref = "2012") +i(prod_class, ref = "paper") |
                      country[year], 
                    data = data %>% filter(year %in% c(2008:2016)), cluster = "prod_class"),
              
              feols(value ~ i(year, treated, ref = "2012") +i(prod_class, ref = "paper")  + 
                      log(waste_gen) + log(gdpcap)|
                      country[year], 
                    data = data %>% filter(year %in% c(2008:2016)), cluster = "prod_class"),
              
              feols(value ~ i(year, treated, ref = "2012") +i(prod_class, ref = "paper")   + 
                      log(waste_gen) + log(gdpcap)|
                      country[year], 
                    data = data %>% filter(year %in% c(2010:2016)), cluster = "prod_class"))


#### Review the results ####
iplot(est_gf) 

#### Run the difference in differences ####
est_ns = list(feols(value ~ i(year, treated, ref = "2016") + i(prod_class, ref = "paper"),
                    data = data %>% filter(year %in% c(2010:2019)), cluster = "prod_class"), 
              
              feols(value ~  i(year, treated, ref = "2016") + i(prod_class, ref = "paper")  | country[year],
                    data = data %>% filter(year %in% c(2010:2019)), cluster = "prod_class"), 
              
              feols(value ~  i(year, treated, ref = "2016") + i(prod_class, ref = "paper")  + 
                      log(waste_gen) + log(gdpcap)| country[year] ,
                    data = data %>% filter(year %in% c(2010:2019)), cluster = "prod_class"), 
              
              feols(value ~  i(year, treated, ref = "2016") + i(prod_class, ref = "paper")  + 
                      log(waste_gen) + log(gdpcap)| country[year] + year,
                    data = data %>% filter(year %in% c(2015:2019)), cluster = "prod_class"))

#### Review the results ####
iplot(est_ns)

#### Bind both estimates toghether ###
est = list(green_fence = est_gf, national_sword = est_ns)

#### Extract the coefficients ####
sum = lapply(est, function(x) lapply(x, function(x)
  data.frame(tidy(x), n.obs = nobs(x), r2 = r2(x, type = "ar2"))) %>% 
    bind_rows(., .id = "spec") %>% mutate(estimator = "ols")) %>% 
  bind_rows(., .id = "policy")

#### Only keep the product class name ####
sum = filter(sum, grepl("year::", term))

#### Function to extract fixed effects for each model #### 
sum = mutate(sum, term = gsub("year::|:treated", "", term))
sum = sum %>% mutate(term = as.numeric(term))

#### Make the plot for the green fence policy####
gf = ggplot(filter(sum, policy == "green_fence", spec != 1)) +
  geom_errorbar(aes(x = term, ymax = estimate + std.error*1.975,
                    ymin = estimate - std.error*1.975, color = spec, group = spec), 
                position = position_dodge(width =0.35), width = 0.25) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  geom_vline(aes(xintercept = 2012)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2008, 2016, 2)) +
  guides(color = "none") + labs(y = "Estimate and 95% CIs", x = "", 
                                title = "a) Green Fence") +
  
  theme(axis.line = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted")); gf

#### Make the plot for the national sword policy####
ns = ggplot(filter(sum, policy != "green_fence", spec != 1)) +
  geom_errorbar(aes(x = term, ymax = estimate + std.error*1.975,
                    ymin = estimate - std.error*1.975, color = spec, group = spec), 
                position = position_dodge(width =0.35), width = 0.25) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  geom_vline(aes(xintercept = 2016)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) + 
  labs(y = "", x = "", title = "b) National Sword") +
  
  theme(axis.line = element_line(), 
        legend.justification = 0,
        legend.title = element_blank(),
        plot.title = element_text(size = 10, face = "italic"),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted")); ns

#### Put both plots together ####
plot_grid(gf, ns, rel_widths = c(1,1.2))

#### Save the plot ####
ggsave(file = paste0(fig_dir, "event_study_did_metal.png"), width = 8, height = 3.5)

