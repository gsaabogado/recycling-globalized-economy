#### ..................................................................... ####
####              Load the relevant data sets for the paper                ####
#### ..................................................................... ####
#### _____________________________________________________________________ ####
#### Load the BACI trade data (raw data) ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(countrycode)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the BACI data ####
files = list.files("01_data/01_trade/BACI_HS96/", pattern = "BACI", full.names = TRUE)
data = lapply(files, vroom)

#### Load the code book with waste products####
goods = read_excel("01_data/01_trade/codebook.xlsx", sheet = "waste_product_codes")

#### Only keep the waste products ####
data = lapply(data, function(x) filter(x, k %in% goods$product))

#### Bind the waste data together ####
data = bind_rows(data)

#### Change the name of the variables ####
data = select(data, year = t, product = k,  value_tusd= v, volume_mt = q, from = i, to = j)

#### Add the classification of each product ####
data = left_join(data, goods %>% mutate(product = as.character(product)))

#### Load the code book of regions and country codes ####
regions = read_excel("01_data/01_trade/codebook.xlsx", sheet = "regions")

#### Add the characteristics of the origin country ####
data = left_join(data, regions |>
                   select(from = country_code, from_country = iso3,
                          from_region = region, from_inc_lvl = income_lvl))


#### Add the characteristics of the destination country ####
data = left_join(data, regions |>
                   select(to = country_code, to_country = iso3,
                          to_region = region, to_inc_lvl = income_lvl))


#### Exclude small or no longer existing states ####
data = filter(data, is.na(from_country) == F, is.na(to_country) == F)

#### Organize the data set ####
data = select(data, -from, -to, -prod_desc)
data = select(data, year, from = from_country, to = to_country, product, prod_class, volume = volume_mt,
              value = value_tusd, from_inc = from_inc_lvl, to_inc = to_inc_lvl,
              from_reg = from_region, to_reg = to_region)

#### Arrange the data from the largest to lowest value ####
data = arrange(data, desc(value))

#### Make HKG China ####
data = mutate(data, from = ifelse(from == "HKG", "CHN", from))
data = mutate(data, to = ifelse(to == "HKG", "CHN", to))
data = mutate(data, to_inc = ifelse(to == "CHN", "Upper Middle Income", to_inc))
data = mutate(data, from_inc = ifelse(from == "CHN", "Upper Middle Income", from_inc))
data = filter(data, from != to)

#### Make MACAO China ####
data = mutate(data, from = ifelse(from == "MAC", "CHN", from))
data = mutate(data, to = ifelse(to == "MAC", "CHN", to))
data = mutate(data, to_inc = ifelse(to == "CHN", "Upper Middle Income", to_inc))
data = mutate(data, from_inc = ifelse(from == "CHN", "Upper Middle Income", from_inc))
data = filter(data, from != to)

#### Aggregate the HKG and MAC exports and imports from and to china ####
data = data %>%
  group_by(year, from, to, product, prod_class,
           from_inc, to_inc, from_reg, to_reg) %>%
  summarise(volume = sum(volume, na.rm = T), value = sum(value, na.rm = T))

#### Save the trade data ####
write_rds(data, file = "02_gen/01_trade/baci_raw.rds")

#### Clean up ####
rm(data, files, goods, regions); gc()

#### _____________________________________________________________________ ####
#### Load the data with the gravity controls ####
#### _____________________________________________________________________ ####

#### Load the gravity data from CSV (exported from PostgreSQL; see _export_gravity_db.R) ####
cepii <- vroom::vroom("01_data/01_trade/gravity_cepii.csv")

# Only keep data from the 2000s
cepii = filter(cepii, year > 2000)

#### select the relevant variables ####
controls = cepii %>% filter(iso3_o != iso3_d) %>%
  select(year, from = iso3_o, to = iso3_d, pop_o, pop_d, gdp_o, gdp_d,
         gdpcap_o, gdpcap_d, eu_o, eu_d, fta_wto,
         gmt_o = gmt_offset_2020_o, gmt_d = gmt_offset_2020_d,
         distw_harmonic, distw_arithmetic, dist, distcap, contig,
         diplo_disagreement, scaled_sci_2021, comlang_off, comlang_ethno,
         comcol, col45,comrelig, col_dep_ever, tradeflow_baci, manuf_tradeflow_baci) %>%
  distinct() %>% arrange(desc(tradeflow_baci))

#### Solve duplicate values ####
controls = controls %>% ungroup() %>%  group_by(year, from, to) %>%
  summarise_at(vars(pop_o:manuf_tradeflow_baci), mean, na.rm = T)

#### Save the control variables ####
write_rds(controls, file = "02_gen/03_macro/gravity_controls.rds")

#### Clean up ####
rm(cepii, controls); gc()


#### _____________________________________________________________________ ####
#### Construct the data set on material imbalance ####
#### _____________________________________________________________________ ####

#### Load the database ####
data_06 = read_excel("01_data/02_material_imbalance/miso_062024.xlsx", sheet = "data")
data_07 = read_excel("01_data/02_material_imbalance/miso_072024.xlsx", sheet = "data")

#### Bind the data sets ####
data = bind_rows(data_06, data_07)
data = distinct(data)
rm(data_06, data_07)

#### Transform from long to wide format ####
data = gather(data, year, value, -c(region, name, material))

#### Only keep data from 1996 ####
data = filter(data, year > 1995)

#### Spread from long to wide in the product dimension ####
data = spread(data, name, value)

#### change the name of the variables ####
data = rename(data,
              production_use = F_7_8_prod_finals,
              consumption = F_8_9_AC_finals,
              material_exp = F_8_18_EX_finals,
              material_imp = F_18_8_IM_finals,
              consumption_updated = F_9_10_GAS,
              waste_rec_man = F_11_12_waste_recov_domest,
              waste_rec_con = F_10_11_supply_EoL_waste)

#### Calculate material imbalance a) ####
data = mutate(data,
              imb_trade = production_use - consumption,
              imb_con =  production_use - waste_rec_con,
              imb_man = production_use - waste_rec_man,
              imb_man_con = production_use - (waste_rec_man-waste_rec_con))

#### Rename the columns ####
data = arrange(data, desc(imb_con))

#### Rename the region to country name ####
data = rename(data, country_name = region)

#### Load the code book of regions and country codes ####
regions = read_excel("01_data/01_trade/codebook.xlsx", sheet = "regions_imb")

#### Add the characteristics of the origin country ####
test = left_join(data, regions |>
                   select(country_name, country = iso3,
                          region = region, inc_lvl = income_lvl))

#### Make Puerto Rico USA ####
test = mutate(test, country = ifelse(country_name == "Puerto Rico", "USA", country))
test = mutate(test, inc_lvl = ifelse(country == "USA", "High Income", inc_lvl))
test = mutate(test, region = ifelse(country == "USA", "North America", region))
test = filter(test, is.na(country) == F)
test = mutate(test, country_name = ifelse(country == "USA", "United States of America", country_name))

#### Aggregate Puerto Rico to the USA ####
test = test %>% group_by(year, material, country, country_name, region, inc_lvl) %>%
  summarise_at(vars(waste_rec_con:imb_man_con), sum, na.rm = T)

#### Make HKG China ####
test = mutate(test, country = ifelse(country == "HKG", "CHN", country))
test = mutate(test, country_name = ifelse(country == "CHN", "China", country_name))
test = mutate(test, inc_lvl = ifelse(country == "CHN", "Upper Middle Income", inc_lvl))
test = mutate(test, region = ifelse(country == "CHN", "China (Incl Hong Kong)", region))

#### Aggregate Hong-Kong to China ####
test = test %>% group_by(year, material, country, country_name, region, inc_lvl) %>%
  summarise_at(vars(waste_rec_con:imb_man_con), sum, na.rm = T)

#### Load the data on controls ####
gdp = read_rds("02_gen/03_macro/gravity_controls.rds") %>%
  select(year, country = from, pop = pop_o,
         gdp = gdp_o, gfp_pc = gdpcap_o) %>%
  filter(is.na(gdp) == F) %>%  distinct()

#### Add the gdp controls to the data set ####
test = left_join(test, gdp %>% mutate(year = as.character(year)))

#### Organice the columns ####
test = select(test, c(year:inc_lvl, production_use, consumption_updated,
                      consumption,
                      material_imp, material_exp,
                      waste_rec_con, waste_rec_man,
                      imb_trade, imb_con, imb_man, imb_man_con,
                      pop, gdp, gdp_pc = gfp_pc))

#### Save the control variables ####
write_rds(test, file = "02_gen/02_miso/material_imbalance.rds")

#### Clean up ####
rm(data, test, gdp, regions); gc()

#### _____________________________________________________________________ ####
#### Construct the data set for the gravity model ####
#### _____________________________________________________________________ ####

#### Load the waste trade, material imbalance, and controls data ####
data = read_rds("02_gen/01_trade/baci_raw.rds")
imb = read_rds("02_gen/02_miso/material_imbalance.rds")
macro = read_rds("02_gen/03_macro/gravity_controls.rds")

#### Exclude the material classes that are not in both data sets ####
data = filter(data, prod_class != "glass")
imb = filter(imb, material != "copper")

#### Change the error in Aluminum ####
data = mutate(data, prod_class = gsub("aluminium", "aluminum", prod_class))
data = select(data %>% ungroup(), -product)

#### Rename the material category ####
imb = mutate(imb, material = gsub("iron_steel", "steel_iron", material)) %>%
  rename(prod_class = material)

#### Add the material imbalance to the waste data (from) ####
data = imb %>% ungroup() %>%
  select(year, prod_class, country, production_use, consumption, waste_rec_con, imb_trade, imb_con) %>%
  mutate(year = as.numeric(year)) %>% rename(from = country) %>%
  rename(w_rec_o = waste_rec_con, prod_use_o = production_use, con_o = consumption,
         imb_trade_o = imb_trade, imb_adj_o = imb_con) %>%
  left_join(data,.)

#### Add the material imbalance of to the waste data (to) ####
data = imb %>% ungroup() %>%
  select(year, prod_class, country, production_use, consumption, waste_rec_con, imb_trade, imb_con) %>%
  mutate(year = as.numeric(year)) %>% rename(to = country) %>%
  rename(w_rec_d = waste_rec_con, prod_use_d = production_use, con_d = consumption,
         imb_trade_d = imb_trade, imb_adj_d = imb_con) %>%
  left_join(data,.); rm(imb)

#### Only keep data from 2000 ####
data = filter(data, year >2000)

#### Create an additional data set with aggregated waste and imbalance ####
agg = data %>%
  group_by(year, from, to, from_inc, to_inc, from_reg, to_reg) %>%
  summarise_at(vars(volume:imb_adj_d), sum, na.rm = T) %>%
  mutate(prod_class = "total")

#### Add the aggregate imbalance to the data set ####
data = bind_rows(data, agg)

#### Add the gravity controls ####
data = left_join(data, macro)

#### Standardize variables value ####
data = mutate(data, volume = volume, # Metric Tonnes
              value = value/1000)

#### Save the naive gravity model ####
write_rds(data, file = "02_gen/01_trade/gravity_data.rds")

#### Clean up ####
rm(data, agg, macro); gc()

#### _____________________________________________________________________ ####
#### Construct the data set for the fixed effects trade balance model ####
#### _____________________________________________________________________ ####

#### Load the waste trade, material imbalance, and controls data ####
data = read_rds("02_gen/01_trade/baci_raw.rds")
imb = read_rds("02_gen/02_miso/material_imbalance.rds")
macro = read_rds("02_gen/03_macro/gravity_controls.rds")

#### Exclude the material classes that are not in both data sets ####
data = filter(data, prod_class != "glass")
imb = filter(imb, material != "copper")

#### Change the error in Aluminum ####
data = mutate(data, prod_class = gsub("aluminium", "aluminum", prod_class))
data = select(data %>% ungroup(), -product)

#### Rename the material category ####
imb = mutate(imb, material = gsub("iron_steel", "steel_iron", material)) %>%
  rename(prod_class = material)

#### Aggregate the trade data ####
from = data %>% group_by(year, country = from, prod_class) %>%
  summarise(exp_volume = sum(volume, na.rm = T), exp_value = sum(value, na.rm = T)) %>%
  arrange(desc(exp_volume))

#### Aggregate the trade data ####
to = data %>% group_by(year, country = to, prod_class) %>%
  summarise(imp_volume = sum(volume, na.rm = T), imp_value = sum(value, na.rm = T)) %>%
  arrange(desc(imp_volume))

#### Extract the region and income group ####
reg = data %>% select(country = from, inc_group = from_inc, region = from_reg) %>% distinct()

#### create the new data set ####
data = left_join(from, to) %>% left_join(., reg)

#### Compute the volume and value trade balance ####
data = mutate(data, volume_tb = exp_volume - imp_volume,
              value_tb = exp_value - imp_value)

#### Add the material imbalance to the waste data (from) ####
data = imb %>% ungroup() %>%
  select(year, prod_class, country, production_use, consumption, waste_rec_con, imb_trade, imb_con) %>%
  mutate(year = as.numeric(year)) %>%
  rename(w_rec = waste_rec_con, prod_use = production_use, con = consumption,
         imb_trade = imb_trade, imb_adj = imb_con) %>%
  left_join(data,.)


#### Only keep data from 2000 ####
data = filter(data, year >2000 & year <= 2016)

#### Create an additional data set with aggregated waste and imbalance ####
agg = data %>%
  group_by(year, country, inc_group, region) %>%
  summarise_at(vars(exp_volume:imp_value, volume_tb:imb_adj), sum, na.rm = T) %>%
  mutate(prod_class = "total")

#### Add the aggregate imbalance to the data set ####
data = bind_rows(data, agg)

#### Only keep the relevant macro data ####
macro = select(macro, year, country = from, gdp = gdp_o,
               gdp_pc = gdpcap_o, eu = eu_o, pop = pop_o) %>% distinct()

#### Add the gravity controls ####
data = left_join(data, macro)

#### Save the naive gravity model ####
write_rds(data, file = "02_gen/01_trade/balance_data.rds")

#### Clean up ####
rm(data, agg, macro, from, to, reg, imb); gc()

#### _____________________________________________________________________ ####
#### Construct the data set of packaging waste trade for the EU ####
#### _____________________________________________________________________ ####

#### Load the data ####
data = vroom("01_data/03_recycling_rates/env_waspac.csv")

#### Select the relevant variables ####
data = select(data, year = TIME_PERIOD, country = geo,
              prod_class = waste, operation = wst_oper,
              unit = unit,
             value = OBS_VALUE, flag = OBS_FLAG)

#### Change the name of the operations ####
data$operation = gsub("GEN", "generated", data$operation)
data$operation = gsub("RCV_E_PAC", "energy_recovery", data$operation)
data$operation = gsub("RCV_OTH", "other_recovery", data$operation)
data$operation = gsub("RCY_EU_FOR", "recycling_eu", data$operation)
data$operation = gsub("RCY_NAT", "recycling_local", data$operation)
data$operation = gsub("RCY_NEU", "recycling_intl", data$operation)
data$operation = gsub("RCY", "recycling", data$operation)
data$operation = gsub("RCV", "recovery", data$operation)
data$operation = gsub("RPR", "repair", data$operation)

#### Only keep the variables we know the cofes of ####
data = filter(data, prod_class %in% c("W150101", "W150102", "W150103",
                                      "W150104", "W150107", "W1501", "W150199"))

#### Rename the products ####
data$prod_class = gsub("W150101", "paper_cardboard", data$prod_class)
data$prod_class = gsub("W150102", "plastic", data$prod_class)
data$prod_class = gsub("W150103", "wood", data$prod_class)
data$prod_class = gsub("W150104", "metal", data$prod_class)
data$prod_class = gsub("W150107", "glass", data$prod_class)
data$prod_class = gsub("W150199", "other packaging", data$prod_class)
data$prod_class = gsub("W1501", "packaging_waste", data$prod_class)

#### Only keep the total ####
data$unit = gsub("PC", "percent", data$unit)
data$unit = gsub("KG_HAB", "kg_pc", data$unit)
data$unit = gsub("T", "tonnes", data$unit)

#### Filter out non-country codes (like "EU27_2020" and "UKN") ####
data <- subset(data, !country %in% c("EU27_2020", "UKN"))

#### Convert the two-character country codes to ISO3 codes ####
data$country_iso3 <- countrycode(data$country, "iso2c", "iso3c")
data$country_iso3 = ifelse(data$country == "EL", "GRC", data$country_iso3)
data$country_name <- countrycode(data$country_iso3, "iso3c", "country.name")
data = data %>% select(-country) %>% rename(country = country_iso3)

#### Only keep EU-27 countries ####
data = filter(data, !(country_name %in% c("Norway", "Iceland", "Liechtenstein")))

#### Organize the data set ####
data = select(data, year, country, country_name, prod_class, operation, unit, value, flag)

#### Save the EU packaging waste data ####
write_rds(data, file = "02_gen/eu_packaging_waste.rds")

#### Clean up ####
rm(data); gc()


#### _____________________________________________________________________ ####
#### Construct the data set of bilateral trade costs ####
#### _____________________________________________________________________ ####

#### Load the data ####
data = list(read_excel("01_data/05_trade_costs/escap_wb_tradecost_19952010.xlsx",
                  sheet = "D"),

            read_excel("01_data/05_trade_costs/escap_wb_tradecost_20112022.xlsx",
                       sheet = "D"))

##### Bind both data sets together ####
data = bind_rows(data)

#### Select the relevant variables ####
data = select(data, year, from = reporter, to = partner,
              tradecost = tij, nontariff_tradecost = nontariff_tij)

#### Standardize countries to iso3 ####
data$from = gsub("ROM", "ROU", data$from)
data$to = gsub("ROM", "ROU", data$to)

#### Save the data set ####
write_rds(data, file = "02_gen/wb_tradecost.rds")

#### Clean up ####
rm(data); gc()

cat("01_load_data.R completed successfully.\n")
