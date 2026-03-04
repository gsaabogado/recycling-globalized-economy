#### ..................................................................... ####
####                              Gravity Model                            ####
#### ..................................................................... ####
#### --------------------------------------------------------------------- #### 
#### Naive gravity model (F78-F89 Pooled)####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% summarise(`F78-F89` = sum(`F78-F89`))
baci = baci %>% group_by(year, from, to) %>% summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F89-D`= `F78-F89`))
data = left_join(data, miso%>% select(year, from = country, `F78-F89-O`= `F78-F89`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F89-D` - data$`F78-F89-O`)

-45 -(65) %>% abs(.)


feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) | year + pair, 
      data = data, cluster = "pair", weights = ~gdp_pair)


#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ `F78-F89-O` + `F78-F89-D`, 
                  data = data, cluster = "pair", weights = ~gdp_pair), 
           
           feols(log(volume) ~ `F78-F89-O` + `F78-F89-D` + log(gdp_o) + log(gdp_d), 
                  data = data, cluster = "pair", weights = ~gdp_pair), 
           
           feols(log(volume) ~ `F78-F89-O` + `F78-F89-D` + log(gdp_o) + log(gdp_d) +
                    eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                    comlang_off + comcol +  log(manuf_tradeflow_baci), 
                  data = data, cluster = "pair", weights = ~gdp_pair),
          
           feols(log(volume) ~ `F78-F89-O` + `F78-F89-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement + log(manuf_tradeflow_baci)|
                   year, 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F89-O` + `F78-F89-D` + log(gdp_o) + log(gdp_d) +
                    eu_o + eu_d + log(dist)  + diplo_disagreement +  log(manuf_tradeflow_baci)|
                    year + pair, 
                  data = data, cluster = "pair", weights = ~gdp_pair))

#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$`F78-F89-O`, na.rm = T)
sd_imb = sd(data$`F78-F89-O`, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 5),
                         "_Avg. material imbalance" = rep(mean_imb, 5), 
                         "_SD material imbalance" = rep(sd_imb, 5)))

#### --------------------------------------------------------------------- #### 
#### Naive gravity model (F78-F1011 Pooled)####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000,
         `F78-F1011` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% 
  summarise(`F78-F89` = sum(`F78-F89`), 
            `F78-F1011` = sum(`F78-F1011`))

#### Summarise the BACI lata ####
baci = baci %>% group_by(year, from, to) %>% 
  summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1011-D`= `F78-F1011`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1011-O`= `F78-F1011`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F1011-D` - data$`F78-F1011-O`)


#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ `F78-F1011-O` + `F78-F1011-D`, 
                 data = data, cluster = "pair", weights = ~gdp_pair), 
           
           feols(log(volume) ~ `F78-F1011-O` + `F78-F1011-D` + log(gdp_o) + log(gdp_d), 
                 data = data, cluster = "pair", weights = ~gdp_pair), 
           
           feols(log(volume) ~ `F78-F1011-O` + `F78-F1011-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol +  log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F1011-O` + `F78-F1011-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement + log(manuf_tradeflow_baci)|
                   year, 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F1011-O` + `F78-F1011-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement + log(manuf_tradeflow_baci)|
                   pair, 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F1011-O` + `F78-F1011-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement +  log(manuf_tradeflow_baci)|
                   year + from + to, 
                 data = data, cluster = "pair", weights = ~gdp_pair))

#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$`F78-F1011-O`, na.rm = T)
sd_imb = sd(data$`F78-F1011-O`, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 6),
                         "_Avg. material imbalance" = rep(mean_imb, 6), 
                         "_SD material imbalance" = rep(sd_imb, 6)))


#### --------------------------------------------------------------------- #### 
#### Naive gravity model (F78-F1011-F711 Pooled)####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11|F_7_11", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011-F711` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste - 
                               F_7_11a_waste_recov_semis - F_7_11b_waste_unrecov_semis)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% summarise(`F78-F1011-F711` = sum(`F78-F1011-F711`))
baci = baci %>% group_by(year, from, to) %>% summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1011-F711-D`= `F78-F1011-F711`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1011-F711-O`= `F78-F1011-F711`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)



#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ `F78-F1011-F711-O` + `F78-F1011-F711-D`, 
                 data = data, cluster = "pair", weights = ~gdp_pair), 
           
           feols(log(volume) ~ `F78-F1011-F711-O` + `F78-F1011-F711-D` + log(gdp_o) + log(gdp_d), 
                 data = data, cluster = "pair", weights = ~gdp_pair), 
           
           feols(log(volume) ~ `F78-F1011-F711-O` + `F78-F1011-F711-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol +  log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F1011-F711-O` + `F78-F1011-F711-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement + log(manuf_tradeflow_baci)|
                   year, 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F1011-F711-O` + `F78-F1011-F711-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement + log(manuf_tradeflow_baci)|
                   pair, 
                 data = data, cluster = "pair", weights = ~gdp_pair),
           
           feols(log(volume) ~ `F78-F1011-F711-O` + `F78-F1011-F711-D` + log(gdp_o) + log(gdp_d) +
                   eu_o + eu_d + log(dist)  + diplo_disagreement +  log(manuf_tradeflow_baci)|
                   year + from + to, 
                 data = data, cluster = "pair", weights = ~gdp_pair))

#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$`F78-F1011-F711-O`, na.rm = T)
sd_imb = sd(data$`F78-F1011-F711-O`, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 6),
                         "_Avg. material imbalance" = rep(mean_imb, 6), 
                         "_SD material imbalance" = rep(sd_imb, 6)))


#### --------------------------------------------------------------------- #### 
#### Relative material imbalance (F78-F89) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% summarise(`F78-F89` = sum(`F78-F89`))
baci = baci %>% group_by(year, from, to) %>% summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F89-D`= `F78-F89`))
data = left_join(data, miso%>% select(year, from = country, `F78-F89-O`= `F78-F89`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
head(as.data.frame(data))
data$net_imb = abs(data$`F78-F89-D` - data$`F78-F89-O`)

abs(+100 - (-100) )

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol +  log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair, 
                 data = data, cluster = "pair"))


#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$net_imb, na.rm = T)
sd_imb = sd(data$net_imb, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 5),
                         "_Avg. material imbalance" = rep(mean_imb, 5), 
                         "_SD material imbalance" = rep(sd_imb, 5)))

#### --------------------------------------------------------------------- #### 
#### Relative material imbalance (F78-F1011) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% summarise(`F78-F1011` = sum(`F78-F1011`))
baci = baci %>% group_by(year, from, to) %>% summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1011-D`= `F78-F1011`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1011-O`= `F78-F1011`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F1011-D` - data$`F78-F1011-O`)

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol + fta_wto + log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair, 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb)  + 
                   log(manuf_tradeflow_baci) | year + pair + from^year + to^year, 
                 data = data, cluster = "pair"))


#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$net_imb, na.rm = T)
sd_imb = sd(data$net_imb, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       signif.code = c(" ***"=0.01, " **"=0.05, " *"=0.1),
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 5),
                         "_Avg. material imbalance" = rep(mean_imb, 5), 
                         "_SD material imbalance" = rep(sd_imb, 5)))

#### Check the estimates ####
esttex(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 5),
                         "_Avg. material imbalance" = rep(mean_imb, 5), 
                         "_SD material imbalance" = rep(sd_imb, 5)))

#### --------------------------------------------------------------------- #### 
#### Relative material imbalance (F78-F1011-F711) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11|F_7_11", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011-F711` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste - 
                               F_7_11a_waste_recov_semis - F_7_11b_waste_unrecov_semis)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% summarise(`F78-F1011-F711` = sum(`F78-F1011-F711`))
baci = baci %>% group_by(year, from, to) %>% summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1011-F711-D`= `F78-F1011-F711`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1011-F711-O`= `F78-F1011-F711`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F1011-F711-D` - data$`F78-F1011-F711-O`)

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol +  log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair, 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) +
                   log(manuf_tradeflow_baci) | year + pair + from^year + to^year, 
                 data = data, cluster = "pair"))

#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$net_imb, na.rm = T)
sd_imb = sd(data$net_imb, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 5),
                         "_Avg. material imbalance" = rep(mean_imb, 5), 
                         "_SD material imbalance" = rep(sd_imb, 5)))

#### --------------------------------------------------------------------- #### 
#### Relative material imbalance (F78-F1011) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_11_12", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1112` = (F_7_8_prod_finals-F_11_12_waste_recov_domest)/1000) %>% 
  mutate(year = as.numeric(year))

#### Aggregate the MISO imbalance and the waste trade to total values ####
miso = miso %>% group_by(year, country) %>% summarise(`F78-F1112` = sum(`F78-F1112`))
baci = baci %>% group_by(year, from, to) %>% summarise(value = sum(value), volume = sum(volume))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1112-D`= `F78-F1112`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1112-O`= `F78-F1112`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F1112-D` - data$`F78-F1112-O`)

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol +  log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair, 
                 data = data, cluster = "pair"),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair + from^year + to^year, 
                 data = data, cluster = "pair"))


#### Add descriptives for the table output ####
mean_volume = mean(data$volume, na.rm = T)
sd_volume = sd(data$volume, na.rm = T)
mean_imb = mean(data$net_imb, na.rm = T)
sd_imb = sd(data$net_imb, na.rm = T)

#### Check the estimates ####
etable(est, se.below = T, digits = 4, 
       extralines = list("_Avg. waste trade volume" = round(mean_volume, 3), 
                         "_SD. waste trade volume" = rep(sd_volume, 5),
                         "_Avg. material imbalance" = rep(mean_imb, 5), 
                         "_SD material imbalance" = rep(sd_imb, 5)))

#### --------------------------------------------------------------------- #### 
#### Relative material imbalance by product (F78-F89) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F89` = (F_7_8_prod_finals-F_8_9_AC_finals)/1000) %>% 
  mutate(year = as.numeric(year))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F89-D`= `F78-F89`))
data = left_join(data, miso%>% select(year, from = country, `F78-F89-O`= `F78-F89`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F89-D` - data$`F78-F89-O`)

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol, 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) | year + pair, 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(manuf_tradeflow_baci)| year + pair + from^year + to^year, 
                 data = data, cluster = "pair", split = ~prod_class))

etable(est[[2]], se.below = T, digits = 4)
colnames(data)

#### Add descriptives for the table output ####
extra_vol = data %>% filter(prod_class != "glass") %>%  group_by(prod_class) %>% 
  summarise(mean = mean(volume, na.rm = T) %>% round(., 1), 
            sd = sd(volume, na.rm = T) %>% round(., 1)) %>% 
  mutate(value = paste0(mean, " (", sd, ")")) %>% select(-sd, -mean) %>% 
  spread(., prod_class, value);extra_vol


extra_imb = data %>% filter(prod_class != "glass") %>%  group_by(prod_class) %>% 
  summarise(mean = mean(net_imb, na.rm = T) %>% round(., 1), 
            sd = sd(net_imb, na.rm = T) %>% round(., 1)) %>% 
  mutate(value = paste0(mean, " (", sd, ")")) %>% select(-sd, -mean) %>% 
  spread(., prod_class, value); extra_imb


#### Check the estimates ####
etable(est[[3]], se.below = T, digits = 4)

#### --------------------------------------------------------------------- ####
#### Relative material imbalance by product (F78-F1011) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_10_11", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1011` = (F_7_8_prod_finals-F_10_11_supply_EoL_waste)/1000) %>% 
  mutate(year = as.numeric(year))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1011-D`= `F78-F1011`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1011-O`= `F78-F1011`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F1011-D` - data$`F78-F1011-O`)

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol, 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) | year + pair, 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) | year + pair + from^year + to^year, 
                 data = data, cluster = "pair", split = ~prod_class),

feols(log(volume) ~ log(net_imb) | year + pair + from^year + to^year, 
      data = data, subset=data$net_imb<2.5, cluster = "pair", split = ~prod_class),

          feols(log(volume) ~ log(`F78-F1011-D`) | year + pair, 
            data = data %>% filter(year > 2009), cluster = "pair", split = ~prod_class))
          
    

##DISTRIBUTION 

ggplot(data) + geom_density(aes(x = log(net_imb))) +
  facet_wrap(~prod_class)


ggplot(data) + geom_density(aes(x = log(volume))) +
  facet_wrap(~prod_class)


#### Add descriptives for the table output ####
extra_vol = data %>%  group_by(prod_class) %>% 
  summarise(mean = mean(volume, na.rm = T) %>% round(., 1), 
            sd = sd(volume, na.rm = T) %>% round(., 1)) %>% 
  mutate(value = paste0(mean, " (", sd, ")")) %>% select(-sd, -mean) %>% 
  spread(., prod_class, value);extra_vol


extra_imb = data %>%  group_by(prod_class) %>% 
  summarise(mean = mean(net_imb*1000, na.rm = T) %>% round(., 1), 
            sd = sd(net_imb*1000, na.rm = T) %>% round(., 1)) %>% 
  mutate(value = paste0(mean, " (", sd, ")")) %>% select(-sd, -mean) %>% 
  spread(., prod_class, value); extra_imb


#### Check the estimates ####
etable(est[[4]], se.below = T, digits = 4)

#### Late table with the estimates ####
etable(est[[5]], se.below = T, digits = 4)



#### --------------------------------------------------------------------- #### 
#### Relative material imbalance by product (F78-F1112) ####
#### --------------------------------------------------------------------- ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
baci = read_rds("out/trade/baci_raw.rds")
miso = read_rds("out/miso/raw_material_imbalance.rds")
macro = read_rds("out/macro/gravity_controls.rds")

#### Standardize MISO and BACI material groups ####
baci$prod_class = gsub("aluminium", "aluminum", baci$prod_class)
baci$prod_class = gsub("steel_iron", "iron_steel", baci$prod_class)
miso$material = gsub("glass_cont|glass_flat", "glass", miso$material)
baci = select(baci %>% ungroup(), -product)

#### Rename the material category of MISO to the same name as BACI ####
miso = miso %>% rename(prod_class = material)

#### Subset the relevant MISO variables for this regression #### 
miso = miso %>% ungroup() %>% filter(grepl("F_7_8|F_8_9|F_11_12", name))  %>% 
  select(year, prod_class, name, country, value)

#### Aggregate the glass components from MISO ####
miso = miso %>% group_by(year, prod_class, country, name) %>% 
  summarise(value = sum(value))

#### Spread MISO ####
miso = spread(miso, name, value)

#### Compute the MISO material imbalance ####
miso <- miso %>% 
  mutate(`F78-F1112` = (F_7_8_prod_finals-F_11_12_waste_recov_domest)/1000) %>% 
  mutate(year = as.numeric(year))

#### Add the material imbalance to the waste data (origin and destination country) ####
data = left_join(baci, miso%>% select(year, to = country, `F78-F1112-D`= `F78-F1112`))
data = left_join(data, miso%>% select(year, from = country, `F78-F1112-O`= `F78-F1112`))

#### Only keep data from 2001 to 2016 ####
data = filter(data, year > 2000 & year <= 2016)

#### Set very low volumes to zero ####
data = mutate(data, volume = ifelse(volume == 1e-9, 0, volume))

#### Include the macro covariates of source and destination ####
data = left_join(data, macro)

#### Create a country pair variable and its GDP ####
data = mutate(data, pair = paste0(from, "_", to))
data = mutate(data, gdp_pair = gdp_o + gdp_d)

#### Add the difference of the trade imbalance ####
data$net_imb = abs(data$`F78-F1112-D` - data$`F78-F1112-O`)

#### Run the gravity model specifications ####
est = list(feols(log(volume) ~ log(net_imb), 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   eu_o + eu_d + log(dist) + contig + diplo_disagreement +
                   comlang_off + comcol +  log(manuf_tradeflow_baci), 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair, 
                 data = data, cluster = "pair", split = ~prod_class),
           
           feols(log(volume) ~ log(net_imb) + log(gdp_o) + log(gdp_d) + 
                   log(manuf_tradeflow_baci) | year + pair + from^year + to^year, 
                 data = data, cluster = "pair", split = ~prod_class))

etable(est[[3]], se.below = T, digits = 4)
#### Add descriptives for the table output ####
extra_vol = data %>% filter(prod_class != "glass") %>%  group_by(prod_class) %>% 
  summarise(mean = mean(volume, na.rm = T) %>% round(., 1), 
            sd = sd(volume, na.rm = T) %>% round(., 1)) %>% 
  mutate(value = paste0(mean, " (", sd, ")")) %>% select(-sd, -mean) %>% 
  spread(., prod_class, value);extra_vol


extra_imb = data %>% filter(prod_class != "glass") %>%  group_by(prod_class) %>% 
  summarise(mean = mean(net_imb, na.rm = T) %>% round(., 1), 
            sd = sd(net_imb, na.rm = T) %>% round(., 1)) %>% 
  mutate(value = paste0(mean, " (", sd, ")")) %>% select(-sd, -mean) %>% 
  spread(., prod_class, value); extra_imb


#### Check the estimates ####
etable(est[[4]], se.below = T, digits = 4)


