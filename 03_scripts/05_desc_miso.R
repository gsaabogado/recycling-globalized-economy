#### ..................................................................... ####
####              All descriptives for material imbalance                  ####
#### ..................................................................... ####
#### _____________________________________________________________________ ####
#### Construct the data set on material imbalance ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the database #### 
data = read_excel("01_data/02_material_imbalance/MISO2_allflows_noendofuse.xlsx")

#### Transform from long to wide format ####
data = gather(data, year, value, -c(region, name, material))

#### Only keep data from 1996 ####
data = filter(data, year > 1995)

#### Load the code book of regions and country codes ####
regions = read_excel("01_data/01_trade/codebook.xlsx", sheet = "regions_imb") 
head(regions)
#### Add the characteristics of the origin country ####
test = left_join(data %>% rename(country_name = region), regions |> 
                   select(country_name, country = iso3, 
                          region = region, inc_lvl = income_lvl))

#### Make Puerto Rico USA ####
test = mutate(test, country = ifelse(country_name == "Puerto Rico", "USA", country))
test = mutate(test, inc_lvl = ifelse(country == "USA", "High Income", inc_lvl))
test = mutate(test, region = ifelse(country == "USA", "North America", region))
test = filter(test, is.na(country) == F)
test = mutate(test, country_name = ifelse(country == "USA", "United States of America", country_name))

#### Aggregate Puerto Rico to the USA ####
test = test %>% group_by(year, material, name, country, country_name, region, inc_lvl) %>% 
  summarise_at(vars(value), sum, na.rm = T)

#### Make HKG China ####
test = mutate(test, country = ifelse(country == "HKG", "CHN", country))
test = mutate(test, country_name = ifelse(country == "CHN", "China", country_name))
test = mutate(test, inc_lvl = ifelse(country == "CHN", "Upper Middle Income", inc_lvl))
test = mutate(test, region = ifelse(country == "CHN", "China (Incl Hong Kong)", region))

#### Aggregate Hong-Kong to China ####
test = test %>% group_by(year, material, name, country, country_name, region, inc_lvl) %>% 
  summarise_at(vars(value), sum, na.rm = T)

#### Load the data on controls ####
gdp = read_rds("02_gen/03_macro/gravity_controls.rds") %>% 
  select(year, country = from, pop = pop_o, 
         gdp = gdp_o, gfp_pc = gdpcap_o) %>% 
  filter(is.na(gdp) == F) %>%  distinct()

#### Add the gdp controls to the data set ####
test = left_join(test, gdp %>% mutate(year = as.character(year)))

#### Organice the columns ####
test = select(test, c(year:inc_lvl, name, value,
                      pop, gdp, gdp_pc = gfp_pc))

#### Save the control variables ####
write_rds(test, file = "02_gen/02_miso/raw_material_imbalance.rds")


#### _____________________________________________________________________ ####
#### Descriptives on material production needs ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(NatParksPalettes)
library(tidyverse)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data = read_rds("02_gen/02_miso/raw_material_imbalance.rds")

#### Extract the relevant variables ####
data = filter(data, grepl("F_7_8|F_8_9|F_7_7|F_10_11|F_7_11", name))

#### Spread from long to wide in the product dimension ####
data = spread(data, name, value)

#### Only separate countries into high income and the rest ####
data = mutate(data, inc_lvl = ifelse(grepl("Middle|Low", inc_lvl), "Middle and Low Income", inc_lvl))

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(F78 = sum(F_7_8_prod_finals, na.rm = T), 
            F89 = sum(F_8_9_AC_finals, na.rm = T)) %>% 
  mutate(`Production Needs (F78 - F89)` = F78-F89) %>% 
  mutate(year = as.numeric(year)) %>% 
  gather(., var, value, -inc_lvl, -year, -material)

#### Transform the materials from small to titles ####
plot$material = str_to_title(plot$material)  %>% gsub("_", " ",.)

#### Plot the time series ####
ggplot(plot %>% filter(!grepl("Glass", material), !grepl("Need", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = var, group = var)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "Material production needs (F78) and raw material consumption (F89)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")


#### Plot the time series of raw material imbalance ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Need", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~  material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", title = "Mat. Imbalance: Prod. Needs (F78) - Consumption (F89)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")


#### _____________________________________________________________________ ####
#### Descriptives on material production needs adjsuted by post-consumption waste ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(NatParksPalettes)
library(tidyverse)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data = read_rds("02_gen/02_miso/raw_material_imbalance.rds")

#### Extract the relevant variables ####
data = filter(data, grepl("F_7_8|F_8_9|F_7_7|F_10_11|F_7_11", name))

#### Spread from long to wide in the product dimension ####
data = spread(data, name, value)

#### Only separate countries into high income and the rest ####
data = mutate(data, inc_lvl = ifelse(grepl("Middle|Low", inc_lvl), "Middle and Low Income", inc_lvl))

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(F78 = sum(F_7_8_prod_finals, na.rm = T), 
            F89 = sum(F_8_9_AC_finals, na.rm = T),
            F1011 = sum(F_10_11_supply_EoL_waste, na.rm = T)) %>% 
  mutate(`Production Needs (F78 - F89)` = F78-F89,
         `Production Needs Adj (F78 - F1011` = F78-F1011,
         `Share of post consumption waste from consumption` = (F1011/F89)) %>% 
  mutate(year = as.numeric(year)) %>% 
  gather(., var, value, -inc_lvl, -year, -material)

#### Transform the materials from small to titles ####
plot$material = str_to_title(plot$material)  %>% gsub("_", " ",.)

#### Plot the time series ####
ggplot(plot %>% filter(!grepl("Glass", material), !grepl("Need|Share", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = var, group = var)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "Material production needs (F78), raw material consumption (F89), and post-consumption waste (F1011)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Plot the time series ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Share", var))) + 
  geom_line(aes(x = year, y = value, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~  material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", title = "Share of post-consumption waste (F1011) to material consumption (F89)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Plot the time series of raw material imbalance ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Adj", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~  material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", title = "Mat. Imbalance: Prod. Needs (F78) - Post Consumption waste (F1011)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")





#### _____________________________________________________________________ ####
#### Material prod. needs with post-consumption waste and industrial waste ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(NatParksPalettes)
library(tidyverse)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data = read_rds("02_gen/02_miso/raw_material_imbalance.rds")

#### Extract the relevant variables ####
data = filter(data, grepl("F_7_8|F_8_9|F_7_7|F_10_11|F_7_11", name))

#### Spread from long to wide in the product dimension ####
data = spread(data, name, value)

#### Only separate countries into high income and the rest ####
data = mutate(data, inc_lvl = ifelse(grepl("Middle|Low", inc_lvl), "Middle and Low Income", inc_lvl))

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(F78 = sum(F_7_8_prod_finals, na.rm = T), 
            F89 = sum(F_8_9_AC_finals, na.rm = T),
            F711 = sum(F_7_11a_waste_recov_semis, na.rm = T),
            F1011 = sum(F_10_11_supply_EoL_waste, na.rm = T)) %>% 
  mutate(`Production Needs (F78 - F89)` = F78-F89,
         `Production Needs Adj (F78 - F1011)` = F78-F1011,
         `Production Needs Prod Adj (F78 - F1011 - F711)` = F78-F1011-F711,
         `Ratio of industrial to EoL waste` = (F711/F1011)) %>% 
  mutate(year = as.numeric(year)) %>% 
  gather(., var, value, -inc_lvl, -year, -material)

#### Transform the materials from small to titles ####
plot$material = str_to_title(plot$material)  %>% gsub("_", " ",.)

#### Plot the time series ####
ggplot(plot %>% filter(!grepl("Glass", material), !grepl("Need|Share|Ratio", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = var, group = var)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "Mat. prod. needs (F78)\ raw mat. cons (F89), post-cons waste (F1011), Ind waste (F711)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Plot the time series ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Ratio", var))) + 
  geom_line(aes(x = year, y = value, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~  material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", title = "Ratio of Industrial to End of life waste (F711/F1011)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Plot the time series of raw material imbalance ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Prod Adj", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~  material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", title = "Mat. Imb.: Prod. Needs (F78) - Post Cons. waste (F1011) - Ind Waste (F711)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")


#### Plot the time series of raw material imbalance ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Need", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = var, group = var)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~  inc_lvl + material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", title = "") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")



#### _____________________________________________________________________ ####
#### Waste import and waste export ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(NatParksPalettes)
library(tidyverse)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data = read_rds("02_gen/02_miso/raw_material_imbalance.rds")

#### Extract the relevant variables ####
data = filter(data %>% ungroup(),
              grepl("F_19_12|F_12_19", name))

#### Spread from long to wide in the product dimension ####
data = spread(data, name, value)

#### Only separate countries into high income and the rest ####
data = mutate(data, inc_lvl = ifelse(grepl("Middle|Low", inc_lvl), "Middle and Low Income", inc_lvl))

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(`Waste exports: F1219` = sum(F_12_19_EX_waste_recov, na.rm = T), 
            `Waste imports: F1912` = sum(F_19_12_IM_waste_recov, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(`Waste Trade balance` = `Waste imports: F1912` - `Waste exports: F1219`) %>%
  gather(., var, value, -inc_lvl, -year, -material) 

#### Transform the materials from small to titles ####
plot$material = str_to_title(plot$material)  %>% gsub("_", " ",.)

#### Plot the time series of waste exports and imports ####
ggplot(plot %>% filter(!grepl("Glass", material), !grepl("Trade", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = var, group = var)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
 title = "") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Plot the time series of trade balance ####
ggplot(plot %>% filter(!grepl("Glass", material), grepl("Trade", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### _____________________________________________________________________ ####
#### Check the waste market ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(readxl)
library(vroom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
data = read_rds("02_gen/02_miso/raw_material_imbalance.rds")

#### Spread from long to wide in the product dimension ####
data = spread(data, name, value)

#### Only separate countries into high income and the rest ####
data = mutate(data, inc_lvl = ifelse(grepl("Middle|Low", inc_lvl), "Middle and Low Income", inc_lvl))

#### Summarize total exports from the EU to each region #### 
agg <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(F911 = sum(F_9_11a_waste_recov_finals + F_9_11b_waste_unrecov_finals, na.rm = T),
            F711 = sum(F_7_11a_waste_recov_semis + F_7_11b_waste_unrecov_semis, na.rm = T),
            F511 = sum(F_5_11a_waste_recov_raw_prod + F_5_11b_waste_unrecov_raw_prod, na.rm = T),
            
            F910 = sum(F_9_10_GAS, na.rm = T),
            F1011 = sum(F_10_11_supply_EoL_waste), 
            F1112 = sum(F_11_12_waste_recov_domest), 
            F1114 = sum(F_11_14_waste_unrecov),
            F89 = sum(F_8_9_AC_finals))

#### Compute the share of losses because of construction and assembly ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`Global Addition to Stock (F910)` = sum(F_9_10_GAS, na.rm = T),
            `Apparent consumption of final prod. (F89)`= sum(F_8_9_AC_finals, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) %>% 
    mutate(material = str_to_title(material)  %>% gsub("_", " ",.),
          value = `Apparent consumption of final prod. (F89)`/`Global Addition to Stock (F910)`)


#### Plot the time series ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_line(aes(x = year, y = value, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone", n = 8)) +
  facet_wrap(~material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "Ratio of AC of final products (F89) to the Gross Addition to stock (F910)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Share of losses because of the use phase of material stock ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`Global Addition to Stock (F910)` = sum(F_9_10_GAS, na.rm = T),
            `EoL waste (F1011)`= sum(F_10_11_supply_EoL_waste, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(material = str_to_title(material)  %>% gsub("_", " ",.),
         value = `EoL waste (F1011)`/`Global Addition to Stock (F910)`)

#### Make the plot ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_line(aes(x = year, y = value, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone", n = 8)) +
  facet_wrap(~material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "Ratio of EoL waste to Gross addition to stock (Losses for the use of stocks): F1011/F910") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Ratio of unrecovered to recovered waste by product and income ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`Unrecovered final waste (F1114)` = sum(F_11_14_waste_unrecov, na.rm = T),
            `Recovered final waste (F1112)`= sum(F_11_12_waste_recov_domest, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(material = str_to_title(material)  %>% gsub("_", " ",.),
         value = `Unrecovered final waste (F1114)`/`Recovered final waste (F1112)`)

#### Make the plot ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_line(aes(x = year, y = value, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone", n = 8)) +
  facet_wrap(~material, scales = "free", ncol = 4) +
  
  labs(y = "Giga Tonnes", 
       x = "", 
       title = "Ratio of unrecovered to recovered waste: (F1114/F1112)") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Composition of industrial and end of life waste  ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`d) Waste from raw products (F511)` = sum(F_5_11a_waste_recov_raw_prod + F_5_11b_waste_unrecov_raw_prod, na.rm = T),,
            `c) Waste from semi-finished products (F711)` = sum(F_7_11a_waste_recov_semis + F_7_11b_waste_unrecov_semis, na.rm = T),
            `a) Waste of EoL products (F1011)` = sum(F_10_11_supply_EoL_waste),
            `b) Waste from final products (F911)` = sum(F_9_11a_waste_recov_finals + F_9_11b_waste_unrecov_finals, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(material = str_to_title(material)  %>% gsub("_", " ",.))%>% 
  gather(var, value, -c(inc_lvl, year, material))

#### Make the plot ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_bar(aes(x = year, y = value/1e3, fill = var, group = inc_lvl), 
           stat = "identity") +
  scale_fill_manual(values = natparks.pals("Yellowstone", n = 4)) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  guides(fill = guide_legend(ncol = 2)) +
  
  labs(y = "Giga Tonnes", x = "", 
       title = "Composition of EoL waste materials") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Composition of the supply of recovered waste ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`d) Waste recovery (F1112)` = sum(F_11_12_waste_recov_domest, na.rm = T), 
         `c) Waste exports (F1219)` = sum(F_12_19_EX_waste_recov, na.rm = T), 
         `a) Waste imports (F1912)` = sum(F_19_12_IM_waste_recov, na.rm = T)) %>% 
  mutate(year = as.numeric(year), 
         material = str_to_title(material)  %>% gsub("_", " ",.)) %>% 
  gather(var, value, -c(inc_lvl, year, material))

#### Make the plot ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_bar(aes(x = year, y = value/1e3, fill = var, group = inc_lvl), 
           stat = "identity") +
  scale_fill_manual(values = natparks.pals("Yellowstone", n = 4)) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  guides(fill = guide_legend(ncol = 2)) +
  
  labs(y = "Giga Tonnes", x = "", 
       title = "Composition of the supply of recovered waste") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Composition of unused and recycled recovered waste ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`a) unused recovered waste (F1314)` = sum(F_13_14_waste_recov_unused, na.rm = T), 
         `b) Product recycling and downcycling (F1320)` = sum(F_20_4a_prod_recycle + F_20_4b_prod_downcycl, na.rm = T)) %>% 
  mutate(year = as.numeric(year), 
         material = str_to_title(material)  %>% gsub("_", " ",.)) %>% 
  gather(var, value, -c(inc_lvl, year, material))

#### Make the plot ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_bar(aes(x = year, y = value/1e3, fill = var, group = inc_lvl), 
           stat = "identity") +
  scale_fill_manual(values = natparks.pals("Yellowstone", n = 4)) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  guides(fill = guide_legend(ncol = 2)) +
  
  labs(y = "Giga Tonnes", x = "", 
       title = ) +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

#### Composition of raw materials ####
plot = data %>% group_by(inc_lvl, year, material) %>% 
  summarise(`d) Production (F34)` = sum(F_3_4a_prod_primary_raw_prod_exog, na.rm = T), 
            `c) Recycling and Downcycling (F1320)` = sum(F_13_20a_prod_recycle+ F_13_20b_prod_downcycle, na.rm = T), 
            `a) Imports (F164)` = sum(F_16_4_IM_raw_prod)) %>% 
  mutate(year = as.numeric(year), 
         material = str_to_title(material)  %>% gsub("_", " ",.)) %>% 
  gather(var, value, -c(inc_lvl, year, material))

#### Make the plot ####
ggplot(plot %>% filter(!grepl("Glass", material))) + 
  geom_bar(aes(x = year, y = value/1e3, fill = var, group = inc_lvl), 
           stat = "identity") +
  scale_fill_manual(values = natparks.pals("Yellowstone", n = 4)) +
  facet_wrap(~inc_lvl + material, scales = "free", ncol = 4) +
  guides(fill = guide_legend(ncol = 2)) +
  
  labs(y = "Giga Tonnes", x = "", 
       title = "Composition of the supply of raw materials") +
  
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  ggpubr::grids("y")

