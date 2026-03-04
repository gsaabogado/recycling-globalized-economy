### ..................................................................... ####
####                Descriptive statistics for the paper                   ####
#### ..................................................................... ####
#### _____________________________________________________________________ ####
#### Volume of waste trade and waste trade defecit ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("out/trade/gravity_data.rds")

#### Compute the total trade volume per year and product class ####
plot = data %>% group_by(year, prod_class) %>% 
  summarise(volume = sum(volume), value = sum(value))

#### Determine the total GDP per year and add it to the data frame ####
plot = data %>% select(year, from, gdp_o) %>% distinct() %>% 
  group_by(year) %>% summarise(gdp = sum(gdp_o, na.rm = T)) %>% 
  left_join(plot, .)

#### Transform trade waste value and volume to billion USD ####
plot = mutate(plot, gdp = gdp/1e6, value = value/1e3)

#### Transform volume to Mega Tonnes ####
plot$volume = plot$volume/1e6

#### Filter the total volume data for the secondary axis ####
total_value <- plot %>%
  filter(prod_class == "total") %>%
  select(year, value)

#### Take away the total category #### 
plot <- plot %>%
  filter(prod_class != "total")

#### Change the name of the product class and organice as factors ####
plot$prod_class = str_to_title(plot$prod_class) %>% gsub("_", " and ", .) %>% 
  gsub("Aluminum", "Aluminum", .)

#### Organice the data into factors ####
plot$prod_class = factor(plot$prod_class, levels = c("Plastic","Aluminum", "Paper", "Steel and iron"))

#### Create the plot for the materials ####
barplot = ggplot(plot, aes(x = year, y = volume)) +
  geom_bar(aes(fill = prod_class), stat = "identity", position = "stack") +
  scale_y_continuous(name = "Volume (Megatonnes)", sec.axis = sec_axis(~ ., name = "Value (Billion USD)")) +
  geom_line(data = total_value, color = "red",
            aes(x = year, 
                y = value * (max(plot$volume, na.rm = T) / max(value, na.rm = T))))+
  
  geom_point(data = total_value, 
             aes(x = year, 
                 y = value * (max(plot$volume, na.rm = T) / max(value, na.rm = T))), 
             color = "red", size = 2) +
  scale_fill_manual(values = natparks.pals("Yellowstone")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Year", fill = "Product Class", title = "a) Waste trade aggregates (volume and value)") +
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
  ggpubr::grids("y"); barplot

#### Summarize total exports from the EU to each region #### 
exp <- data %>% 
  group_by(country = from_inc, year) %>% 
  summarize(exports = sum(value, na.rm = T))

#### Determine the share of trade in each year fo each category ####
imp <- data %>% 
  group_by(country = to_inc, year) %>% 
  summarize(imports = sum(value, na.rm = T))

#### Join both data sets together ####
exp = left_join(exp, imp)

#### Determine the trade balance in each income group ####
exp = mutate(exp, balance = exports-imports)

#### Transform volume to Mega Tonnes ####
exp$balance = exp$balance/1e3

#### Plot the time series ####
balance = ggplot(exp) + 
  geom_line(aes(x = year, y = balance, color = country)) +
  geom_point(aes(x = year, y = balance, color = country)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0), color = "black" ) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  
  labs(y = "", title = "b) Waste trade balance in Billion USD",
       x = "Year") +
  guides(color = guide_legend(nrow = 2)) +
  coord_cartesian(ylim = c(-200, 200)) +
  
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
  ggpubr::grids("y"); balance

#### Put both plots together ####
plot_grid(barplot, balance, rel_widths = c(1.25,1))

#### Save the plot ####
ggsave(file = paste0(fig_dir, "waste_volume_balance_b.png"), width = 10, height = 4.5)



#### _____________________________________________________________________ ####
#### Table with top 10 largest exporters ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(reactablefmtr)
library(conflicted)
library(tidyverse)
library(ggplot2)
library(reactable)
library(webshot)
library(knitr)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load data ####
data = read_rds("out/trade/gravity_data.rds")
data = mutate(data, volume = volume/1e6)

#### Summarize total exports for the 10 largest countries #### 
reg_exp <- data %>%  filter(prod_class == "total") %>% 
  group_by(from) %>%
  summarize(total = sum(volume, na.rm = T)) %>% 
  mutate(share = 100*(total/sum(total))) %>% 
  arrange(desc(share)) %>% mutate(cum_share = cumsum(share)) %>% 
  .[1:10, ]

#### Add the sum of each material and left join #### 
mat <- data %>% filter(from %in% reg_exp$from, prod_class != "total") %>% 
  group_by(from, prod_class) %>%
  summarize(total = sum(volume, na.rm = T)) %>% 
  spread(., prod_class, total)

#### Add the total from each product to the total waste flows ####
reg_exp = left_join(reg_exp, mat)

#### Compute the shares of each material ####
reg_exp = mutate(reg_exp, aluminum = 100 * (aluminum/total))
reg_exp = mutate(reg_exp, paper = 100 * (paper/total))
reg_exp = mutate(reg_exp, plastic = 100 * (plastic/total))
reg_exp = mutate(reg_exp, steel_iron = 100 * (steel_iron/total))

#### Round to two digits ####
reg_exp = mutate_at(reg_exp, vars(total:steel_iron), round, 2)

#### Organice the data set ####
reg_exp = select(reg_exp, c(from:cum_share, steel_iron, paper, aluminum, plastic))

#### Create the table of main exporters #####
table = reactable(
  reg_exp, theme = fivethirtyeight(font_size = 12, header_font_size = 12,
                                   cell_padding = 5), 
  defaultPageSize = 10,
  defaultColDef = colDef(align = "left"),
  columns = list(from = colDef(maxWidth = 80, align = "left", name = "Country"),
                 total = colDef(maxWidth = 80, align = "center", name = "Exports (Mt)"),
                 share = colDef(maxWidth = 80, align = "center",  name = "Share (%)"),
                 cum_share = colDef(maxWidth = 80, align = "center", name = "Cum. Share (%)"), 
                 steel_iron = colDef(maxWidth = 80, align = "center", name = "Steel & Iron (%)"),
                 glass = colDef(maxWidth = 80, align = "center", name = "Glass (%)"),
                 plastic = colDef(maxWidth = 90, align = "center", name = "Plastic (%)"),
                 paper = colDef(maxWidth = 80, align = "center", name = "Paper (%)"),
                 aluminum = colDef(maxWidth = 90, align = "center", name = "Aluminium (%)"))); table


#### Transform the table to png to paste it in the paper ####
tryCatch({
  html = "kable.html"
  htmlwidgets::saveWidget(table, html)
  webshot2::webshot(html, paste0(fig_dir, "top_exports.png"),
                    cliprect = c(20, 60, 705, 300), zoom = 2, delay = 0.5)
}, error = function(e) {
  message("Note: webshot2 requires Chrome/Chromium. Skipping top_exports.png: ", e$message)
})

#### _____________________________________________________________________ ####
#### Table with top 10 largest importers ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(ggplot2)
library(reactable)
library(webshot)
library(reactablefmtr)
library(knitr)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load data ####
data = read_rds("out/trade/gravity_data.rds")
data = mutate(data, volume = volume/1e6)

#### Summarize total exports for the 10 largest countries #### 
reg_exp <- data %>%  filter(prod_class == "total") %>% 
  group_by(to) %>%
  summarize(total = sum(volume, na.rm = T)) %>% 
  mutate(share = 100*(total/sum(total))) %>% 
  arrange(desc(share)) %>% mutate(cum_share = cumsum(share)) %>% 
  .[1:10, ]

#### Add the sum of each paterial and left join #### 
mat <- data %>% filter(to %in% reg_exp$to, prod_class != "total") %>% 
  group_by(to, prod_class) %>%
  summarize(total = sum(volume, na.rm = T)) %>% 
  spread(., prod_class, total)

#### Add the total from each product to the total waste flows ####
reg_exp = left_join(reg_exp, mat)

#### Compute the shares of each material ####
reg_exp = mutate(reg_exp, aluminum = 100 * (aluminum/total))
reg_exp = mutate(reg_exp, paper = 100 * (paper/total))
reg_exp = mutate(reg_exp, plastic = 100 * (plastic/total))
reg_exp = mutate(reg_exp, steel_iron = 100 * (steel_iron/total))

#### Round to two digits ####
reg_exp = mutate_at(reg_exp, vars(total:steel_iron), round, 2)

#### Organice the data set ####
reg_exp = select(reg_exp, c(to:cum_share, steel_iron, paper, aluminum, plastic))

#### Create the table of main exporters #####
table = reactable(
  reg_exp, theme = fivethirtyeight(font_size = 12, header_font_size = 12,
                                   cell_padding = 5), 
  defaultPageSize = 10,
  defaultColDef = colDef(align = "left"),
  columns = list(to = colDef(maxWidth = 80, align = "left", name = "Country"),
                 total = colDef(maxWidth = 80, align = "center", name = "Exports (Mt)"),
                 share = colDef(maxWidth = 80, align = "center",  name = "Share (%)"),
                 cum_share = colDef(maxWidth = 80, align = "center", name = "Cum. Share (%)"), 
                 steel_iron = colDef(maxWidth = 80, align = "center", name = "Steel & Iron (%)"),
                 glass = colDef(maxWidth = 80, align = "center", name = "Glass (%)"),
                 plastic = colDef(maxWidth = 90, align = "center", name = "Plastic (%)"),
                 paper = colDef(maxWidth = 80, align = "center", name = "Paper (%)"),
                 aluminum = colDef(maxWidth = 90, align = "center", name = "Aluminium (%)"))); table


#### Transform the table to png to paste it in the paper ####
tryCatch({
  html = "kable.html"
  htmlwidgets::saveWidget(table, html)
  webshot2::webshot(html, paste0(fig_dir, "top_imports.png"),
                    cliprect = c(20, 60, 705, 300), zoom = 2, delay = 0.5)
}, error = function(e) {
  message("Note: webshot2 requires Chrome/Chromium. Skipping top_imports.png: ", e$message)
})

#### _____________________________________________________________________ ####
#### Table with top 10 largest not high income waste importers ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(ggplot2)
library(reactable)
library(webshot)
library(reactablefmtr)
library(knitr)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load data ####
data = read_rds("out/trade/gravity_data.rds")
data = mutate(data, volume = volume/1e6)

#### Summarize total exports for the 10 largest countries #### 
reg_exp <- data %>% filter(prod_class == "total") %>% 
  group_by(to) %>%
  summarize(total = sum(volume, na.rm = T)) %>% 
  mutate(share = 100*(total/sum(total))) %>% 
  arrange(desc(share)) 

#### add the income level ####
reg_exp = data %>% ungroup() %>% 
  select(to, to_inc) %>% distinct() %>% 
  left_join(reg_exp, .)

#### Summarize total exports for the 10 largest countries #### 
reg_exp = reg_exp %>% filter(to_inc != "High Income") %>% select(-to_inc) %>% 
  arrange(desc(share)) %>% .[1:10, ] %>% mutate(cum_share = cumsum(share))

#### Add the sum of each paterial and left join #### 
mat <- data %>% filter(to %in% reg_exp$to, prod_class != "total") %>% 
  group_by(to, prod_class) %>%
  summarize(total = sum(volume, na.rm = T)) %>% 
  spread(., prod_class, total)

#### Add the total from each product to the total waste flows ####
reg_exp = left_join(reg_exp, mat)

#### Compute the shares of each material ####
reg_exp = mutate(reg_exp, aluminum = 100 * (aluminum/total))
reg_exp = mutate(reg_exp, paper = 100 * (paper/total))
reg_exp = mutate(reg_exp, plastic = 100 * (plastic/total))
reg_exp = mutate(reg_exp, steel_iron = 100 * (steel_iron/total))

#### Round to two digits ####
reg_exp = mutate_at(reg_exp, vars(total:steel_iron), round, 2)

#### Organice the data set ####
reg_exp = select(reg_exp, c(to:cum_share, steel_iron, paper, aluminum, plastic))

#### Create the table of main exporters #####
table = reactable(
  reg_exp, theme = fivethirtyeight(font_size = 12, header_font_size = 12,
                                   cell_padding = 5), 
  defaultPageSize = 10,
  defaultColDef = colDef(align = "left"),
  columns = list(to = colDef(maxWidth = 80, align = "left", name = "Country"),
                 total = colDef(maxWidth = 80, align = "center", name = "Exports (Mt)"),
                 share = colDef(maxWidth = 80, align = "center",  name = "Share (%)"),
                 cum_share = colDef(maxWidth = 80, align = "center", name = "Cum. Share (%)"), 
                 steel_iron = colDef(maxWidth = 80, align = "center", name = "Steel & Iron (%)"),
                 glass = colDef(maxWidth = 80, align = "center", name = "Glass (%)"),
                 plastic = colDef(maxWidth = 90, align = "center", name = "Plastic (%)"),
                 paper = colDef(maxWidth = 80, align = "center", name = "Paper (%)"),
                 aluminum = colDef(maxWidth = 90, align = "center", name = "Aluminium (%)"))); table


#### Transform the table to png to paste it in the paper ####
tryCatch({
  html = "kable.html"
  htmlwidgets::saveWidget(table, html)
  webshot2::webshot(html, paste0(fig_dir, "top_imports_low_inc.png"),
                    cliprect = c(20, 60, 705, 300), zoom = 2, delay = 0.5)
}, error = function(e) {
  message("Note: webshot2 requires Chrome/Chromium. Skipping top_imports_low_inc.png: ", e$message)
})


#### _____________________________________________________________________ ####
#### Material Imbalance from waste recovery by world region and material ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("out/trade/gravity_data.rds")

#### Change the name of the product classes and organi ze ####
data = filter(data, prod_class != "total", year < 2017)
data$prod_class = str_to_title(data$prod_class) 
data$prod_class = gsub("Steel_iron", "Steel and Iron", data$prod_class)
data$prod_class = factor(data$prod_class, levels = c(
  "Aluminum", 
  "Paper",
  "Steel and Iron", 
  "Plastic", 
  "Copper"))  

#### Only keep the material imbalance data ####
data = select(data, year, country = from, prod_class, inc_lvl = from_inc, 
              waste_recovery = w_rec_o,
              prod_use = prod_use_o, con_use = con_o, 
              imb_trade = imb_trade_o, imb_recovery = imb_adj_o, 
              pop = pop_o, gdp = gdp_o) %>% distinct()


#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, prod_class) %>% 
  summarize(waste_recovery = sum(waste_recovery, na.rm = T), 
            prod = sum(prod_use, na.rm = T),
            con = sum(con_use, na.rm = T),
            imb_trade = sum(imb_trade, na.rm = T), 
            imb_recovery = sum(imb_recovery, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) 

#### Organize the legend ####
plot$inc_lvl = factor(plot$inc_lvl, levels = c("High Income", 
                                               "Upper Middle Income", 
                                               "Lower Middle Income", 
                                               "Low Income"))

#### Plot the time series ####
ggplot(plot) + 
  geom_line(aes(x = year, y = imb_recovery/1e3, color = inc_lvl, group = inc_lvl)) +
  geom_point(aes(x = year, y = imb_recovery/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0), color = "black" ) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~prod_class, scales = "free", ncol = 4) +
  
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

#### Save the plot ####
ggsave(file = paste0(fig_dir, "miso_f78_f1112.png"), width = 8, height = 3.5)


#### _____________________________________________________________________ ####
#### Material Imbalance from trade by world region and material ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("out/trade/gravity_data.rds")

#### Change the name of the product classes and organi ze ####
data = filter(data, prod_class != "total", year < 2017)
data$prod_class = str_to_title(data$prod_class) 
data$prod_class = gsub("Steel_iron", "Steel and Iron", data$prod_class)
data$prod_class = factor(data$prod_class, levels = c(
  "Aluminum", 
  "Paper",
  "Steel and Iron", 
  "Plastic", 
  "Copper"))  
#### Only keep the material imbalance data ####
data = select(data, year, country = from, prod_class, inc_lvl = from_inc, 
              waste_recovery = w_rec_o,
              prod_use = prod_use_o, con_use = con_o, 
              imb_trade = imb_trade_o, imb_recovery = imb_adj_o, 
              pop = pop_o, gdp = gdp_o) %>% distinct()

#### Set regions into high income and the rest ####
data = mutate(data, inc_lvl = ifelse(inc_lvl == "High Income", "High Income", "Not High Income" ))

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, prod_class) %>% 
  summarize(waste_recovery = sum(waste_recovery, na.rm = T), 
            prod = sum(prod_use, na.rm = T),
            con = sum(con_use, na.rm = T),
            imb_trade = sum(imb_trade, na.rm = T), 
            imb_recovery = sum(imb_recovery, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) 

#### Check the difference in material needs between high and low income ####
plot = select(plot,c(inc_lvl:prod_class, imb_trade)) %>% 
  spread(inc_lvl, imb_trade) %>% mutate(diff =  `Not High Income` - `High Income`)


#### Plot the time series ####
ggplot(plot) + 
  geom_line(aes(x = year, y = diff/1e3, color = prod_class, group = prod_class)) +
  geom_point(aes(x = year, y = diff/1e3, color = prod_class, group = prod_class)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0), color = "black" ) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~prod_class, scales = "free", ncol = 4) +
  
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

#### Save the plot ####
ggsave(file = paste0(fig_dir, "miso_f78_f89_relative.png"), width = 8, height = 3.5)


#### _____________________________________________________________________ ####
#### Different types of material imbalanances ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("out/miso/material_imbalance.rds")

#### Change the name of the product classes and organize ####
data$material = str_to_title(data$material) 

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(waste_rec_con = sum(waste_rec_con, na.rm = T), 
            waste_rec_man = sum(waste_rec_man, na.rm = T), 
            production_use = sum(production_use, na.rm = T),
            consumption = sum(consumption, na.rm = T),
            `a) Trade (F78-F89)` = sum(imb_trade, na.rm = T),
            `b) Consumption F78-F1011` = sum(imb_con, na.rm = T),
            `c) Manufacturing F78-F1112` = sum(imb_man, na.rm = T),
            `d) Net Manufacturing F78-(F1112-F1011)` = sum(imb_man_con, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) 

#### Organize the legend ####
plot$inc_lvl = factor(plot$inc_lvl, levels = c("High Income", 
                                               "Upper Middle Income", 
                                               "Lower Middle Income", 
                                               "Low Income"))

#### Put all the materials together #####
plot = gather(plot, var, value, -c(inc_lvl:material))

#### Only keep the key materials ####
plot = filter(plot, material %in% c("Aluminum", "Plastic", 
                                    "Paper", "Iron_steel"))

#### Add the facet ####
plot$facet = paste(plot$var, "-", plot$material)
plot = filter(plot, inc_lvl %in% c("High Income", "Upper Middle Income"))

#### Plot the time series ####
ggplot(plot %>% filter(grepl("F7", var))) + 
  geom_line(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  geom_point(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0), color = "black" ) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~facet, scales = "free", ncol = 4) +
  
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
#### Production, consumption, and waste recovery####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("out/miso/material_imbalance.rds")

#### Change the name of the product classes and organize ####
data$material = str_to_title(data$material) 

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, year, material) %>% 
  summarize(`a) Prod use F78` = sum(production_use, na.rm = T),
            `b) Consumption F89` = sum(consumption, na.rm = T),
            `c) Waste Con F1011` = sum(waste_rec_con, na.rm = T), 
            `d) Waste Man F1112` = sum(waste_rec_man, na.rm = T), 
            `e) Waste Man-Net (F1112-F1011)` = sum(waste_rec_man - waste_rec_con, na.rm = T)) %>% 
  mutate(year = as.numeric(year)) 

#### Organize the legend ####
plot$inc_lvl = factor(plot$inc_lvl, levels = c("High Income", 
                                               "Upper Middle Income", 
                                               "Lower Middle Income", 
                                               "Low Income"))

#### Put all the materials together #####
plot = gather(plot, var, value, -c(inc_lvl:material))

#### Only keep the key materials ####
plot = filter(plot, material %in% c("Aluminum", "Plastic", 
                                    "Paper", "Iron_steel"))

#### Add the facet ####
plot$facet = paste(plot$var, "-", plot$material)
plot = filter(plot, inc_lvl %in% c("High Income", "Upper Middle Income"))

#### Plot the time series ####
ggplot(plot) + 
  geom_line(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  geom_point(aes(x = year, y = value/1e3, color = inc_lvl, group = inc_lvl)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0), color = "black" ) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~facet, scales = "free", ncol = 4) +
  
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

####
plot$facet = paste(plot$inc_lvl, "-", plot$material)
ggplot(plot) + 
  geom_line(aes(x = year, y = value/1e3, color = var, group = var)) +
  geom_point(aes(x = year, y = value/1e3, color = var, group = var)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  facet_wrap(~facet, scales = "free", ncol = 4) +
  
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
#### Time series of manufacturing trade costs ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load the packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(cowplot)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
dist = read_rds("out/trade/gravity_data.rds") 
data = read_rds("out/wb_tradecost.rds")

#### Construct the time series of trade costs ####
plot = data %>% gather(., var, value, -c(year, from, to)) %>% group_by(from, var, year) %>% 
  summarise(value = mean(value, na.rm = T)) %>% group_by(year, var) %>% 
  summarise(avg = mean(value, na.rm = T), 
            q3 = quantile(value, p = 0.75, na.rm = T), 
            q1 = quantile(value, p = 0.25, na.rm = T))

#### Non-Tariff trade costs only from 2011 ####
ts = ggplot(plot %>% filter(var != "nontariff_tradecost")) +
  geom_ribbon(aes(x = year, ymin = q1, ymax = q3), alpha = 0.5, fill = "lightblue") +
  geom_line(aes(x = year, y = avg), color = "navy") +
  
  
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.x = element_line()) +
  
  grids("y", linetype = "dashed") +
  labs(x = "Year", y = "Avg. trade costs (rel. difference)", 
       title = "a. Time series of average trade costs"); ts

#### Construct the time series of trade costs ####
plot = filter(data, year %in% c(2001, 2010, 2020))
plot$year = as.character(plot$year)

#### Non-Tariff trade costs only from 2011 ####
density = ggplot(plot) +
  geom_density(aes(x = tradecost, fill = year, group = year), alpha = 0.5) +
  
  
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.x = element_line(),
        legend.position = "top",
        legend.justification = 0, 
        legend.title = element_blank()) +
  scale_fill_manual(values = natparks.pals("Yellowstone")) +
  
  grids("y", linetype = "dashed") +
  labs(y = "", x = "Trade Costs (relative difference)",
       title = "b. Density of trade costs for selected years"); density 

#### Subset the distance data set ####
dist = dist %>% select(from, to, dist, year, volume, manuf_tradeflow_baci) %>% 
  distinct() %>% 
  filter(is.na(dist) == F)

#### Add the distance data to the data set ####
data = left_join(data, dist)

#### Construct the time series of trade costs ####
plot = data %>% 
  filter(from == "USA") %>% group_by(from, to) %>%  
  summarise(tradecost = mean(tradecost, na.rm = T), 
            dist = mean(dist, na.rm = T), 
            volume = mean(manuf_tradeflow_baci, na.rm = T)/1000000) 

#### Non-Tariff trade costs only from 2011 ####
trade_usa <- ggplot(plot) +
  geom_point(aes(x = log(dist), y = log(tradecost), 
                 size = volume), alpha = 0.25) +
  
  geom_text_repel(data = filter(plot, volume > quantile(volume, p = 0.95, na.rm = TRUE)), 
                  aes(x = log(dist), y = log(tradecost), 
                      label = to, vjust = -2, color = to)) +
  
  geom_point(data = filter(plot, volume > quantile(volume, p = 0.95, na.rm = TRUE)), 
             aes(x = log(dist), y = log(tradecost), 
                 size = volume, color = to), 
             alpha = 1) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = natparks.pals("Yellowstone", n = 10)) +
  
  geom_smooth(aes(x = log(dist), y = log(tradecost)), method = "lm", 
              formula = y ~ poly(x, 2), se = TRUE, color = "navy") +  # Apply the Yellowstone palette
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.x = element_line(),
        legend.position = "top",
        legend.justification = 0) +
  guides(color = "none") +
  grids("y", linetype = "dashed") +
  labs(x = "Distance between capital cities",
       y = "Trade Costs (Log)", size = "Trade\nVolume (Mt)",
       title = "c. Trade costs and distance (USA)"); trade_usa

#### Country pairs by trade costs and trade volume ####
plot = data %>% mutate(pair = paste(from, to, sep = "-")) %>% 
  group_by(pair) %>%  
  summarise(tradecost = mean(tradecost, na.rm = T), 
            dist = mean(dist, na.rm = T), 
            volume = mean(manuf_tradeflow_baci, na.rm = T)) %>% 
  filter(is.na(tradecost) == F, is.na(volume) == F)

#### Non-Tariff trade costs only from 2011 ####
trade_pair = ggplot(plot) +
  geom_point(aes(x = log(volume), 
                 y = log(tradecost)), alpha = 0.10) +
  
  
  geom_smooth(aes(x = log(volume), y = log(tradecost)), method = "lm", 
              formula = y ~ poly(x, 2), se = TRUE, color = "navy")+
  
  geom_text_repel(data = filter(plot, volume > quantile(volume, p = 0.999, na.rm = TRUE)), 
                  aes(x = log(volume), y = log(tradecost), 
                      label = pair, vjust = -2, color = pair)) +
  
  geom_point(data = filter(plot, volume > quantile(volume, p = 0.999, na.rm = TRUE)), 
             aes(x = log(volume), y = log(tradecost), color = pair, 
                 size = volume/1000000)) +
  
  
  scale_color_manual(values = natparks.pals("Yellowstone", n = 12)) +
  
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.x = element_line(),
        legend.position = "top",
        legend.justification = 0) +
  guides(color = "none") +
  grids("y", linetype = "dashed") +
  labs(x = "Trade Volume (Log)",
       y = "Trade Costs (Log)", size = "Trade\nVolume (Mt)",
       title = "d. Trade volume and costs btw. country pairs"); trade_pair

#### Organize the plots with cow-plot ####
plot_grid(ts, density, trade_usa, trade_pair, rel_widths = c(1,1))

#### Save the plot ####
ggsave(file = paste0(fig_dir, "trade_costs.png"), width = 10, height = 8.5)



#### --------------------------------------------------------------------- #### 
#### Chineese share of waste imports by material ####
#### --------------------------------------------------------------------- ####

#### set the path to store the figures ####
fig_dir = "images/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(fixest)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("out/eu_packaging_waste.rds") %>% select(-flag)
data = read_rds("out/trade/gravity_data.rds")

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Take away total waste exports #####
data = filter(data, prod_class != "total")

#### Change the name of the products to match with the RR data ####
data$prod_class = gsub("steel_iron|aluminum","metal",data$prod_class)
data$prod_class = gsub("paper","paper_cardboard",data$prod_class)

#### Aggregate the exports by year and country ####
agg = data %>% group_by(year, country = to, prod_class) %>% 
  summarise(`Total waste imports from the EU (Mt)` = sum(volume, na.rm = T)/1000) %>% 
  mutate(type = "Total waste imports")

#### Determine the share of waste imports ####
agg = agg %>%  group_by(year, prod_class) %>% 
  mutate(`Share of EU imports` = 100 * `Total waste imports from the EU (Mt)`/sum(`Total waste imports from the EU (Mt)`))

#### Aggregate share and volume ####
agg = gather(agg, var, value,-c(year, country, prod_class, type))

#### Only keep China ###
plot = filter(agg, country == "CHN")

#### Change the name of the product ####
plot$prod_class = str_to_title(plot$prod_class) %>% gsub("_", " and ", .)

#### Plot the share and volume of Chineese waste imports ####
ggplot(plot %>% filter(year %in% c(2010:2019))) + 
  geom_line(aes(x = year, y = value, color = prod_class, group = prod_class)) +
  facet_wrap(~var, scales = "free") +
  geom_point(aes(x = year, y = value, color = prod_class, group = prod_class)) +
  geom_vline(aes(xintercept = 2017), color = "navy", linetype = "dashed") +
  geom_vline(aes(xintercept = 2013), color = "darkgreen", linetype = "dotted") +
  annotate("text", x = 2017, y = Inf, label = "NSP", hjust = -0.25, vjust = 1.5, size = 3, color = "navy") +
  annotate("text", x = 2013, y = Inf, label = "GFI", hjust = -0.25, vjust = 1.5, size = 3, color = "darkgreen") +
  
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  
  scale_x_continuous(breaks = seq(2010, 2019, 2)) +
  labs(x = "", y = "") +
  
  theme(axis.line = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust = 0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"))

#### Calculate the reduction in tones for the first policy years ####
plot_diff = plot %>% filter(year %in% c(2016,2019)) %>% select(year, prod_class, var, value)
plot_diff = mutate(plot_diff, pre = ifelse(year %in% c(2015, 2016), "pre", "post")) %>% 
  group_by(pre, prod_class, var) %>% summarise(value = mean(value))
plot_diff = spread(plot_diff, pre, value)
plot_diff = mutate(plot_diff, diff = pre-post)

plot_diff

#### Save the plot ####
ggsave(file = paste0(fig_dir, "china_waste_share.png"), width = 8, height = 3.5)


