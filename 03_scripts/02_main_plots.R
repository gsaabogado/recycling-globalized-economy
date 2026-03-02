#### ..................................................................... ####
####                Descriptive statistics for the paper                   ####
#### ..................................................................... ####
#### _____________________________________________________________________ ####
#### Volume of waste trade and waste trade deficit ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

data %>% group_by(year) %>% filter(year == 2016, prod_class != "total") %>% 
  summarise(volume = sum(volume)/1e6, value = sum(value))

#### Compute the total trade volume per year and product class ####
plot = data %>% group_by(year, prod_class) %>% 
  summarise(volume = sum(volume), value = sum(value))

#### Determine the total GDP per year and add it to the data frame ####
plot = data %>% select(year, from, gdp_o) %>% distinct() %>% 
  group_by(year) %>% summarise(gdp = sum(gdp_o, na.rm = T)) %>% 
  left_join(plot, .)

#### Transform trade waste value and volume to billion USD ####
plot = mutate(plot, gdp = gdp/1e6, value = value/1e3)

#### Create a column with the share of GDP ####
plot = plot %>% ungroup() %>% group_by(year) %>% 
  mutate(agg_value = sum(value)) %>% mutate(share_gdp = 100*(agg_value/gdp))

#### Transform volume to Mega Tonnes ####
plot$volume = plot$volume/1e6

#### Filter the total volume data for the secondary axis ####
total_volume <- plot %>%
  filter(prod_class == "total") %>%
  select(year, volume)

#### Take away the total category #### 
plot <- plot %>%
  filter(prod_class != "total")

#### Change the name of the product class and organize as factors ####
plot$prod_class = str_to_title(plot$prod_class) %>% gsub("_", " and ", .) %>% 
  gsub("Aluminum", "Aluminum", .)

#### Organize the data into factors ####
plot$prod_class = factor(plot$prod_class, levels = c("Plastic","Aluminum", "Paper", "Steel and iron"))

#### Create the plot for the materials ####
barplot = ggplot(plot, aes(x = year, y = value)) +
  geom_bar(aes(fill = prod_class), stat = "identity", position = "stack") +
  scale_y_continuous(name = "Value (Billion USD)", sec.axis = sec_axis(~ ., name = "Volume (Megatonnes)")) +
  geom_line(data = total_volume, color = "red",
            aes(x = year, 
                y = volume * (max(plot$value, na.rm = T) / max(volume, na.rm = T))))+
  
  geom_point(data = total_volume, 
             aes(x = year, 
                 y = volume * (max(plot$value, na.rm = T) / max(total_volume$volume, na.rm = T))), 
             color = "red", size = 2) +
  scale_fill_manual(values = natparks.pals("Yellowstone")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Year", fill = "Product Class", title = "a) Waste trade aggregates (value and volume)") +
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
  summarize(exports = sum(volume, na.rm = T))

#### Determine the share of trade in each year fo each category ####
imp <- data %>% 
  group_by(country = to_inc, year) %>% 
  summarize(imports = sum(volume, na.rm = T))

#### Join both data sets together ####
exp = left_join(exp, imp)

#### Determine the trade balance in each income group ####
exp = mutate(exp, balance = exports-imports)

#### Transform volume to Mega Tonnes ####
exp$balance = exp$balance/1e6

#### Plot the time series ####
balance = ggplot(exp) + 
  geom_line(aes(x = year, y = balance, color = country)) +
  geom_point(aes(x = year, y = balance, color = country)) +
  scale_shape_manual(values = seq(0, 15, 1)) +
  geom_hline(aes(yintercept = 0), color = "black" ) +
  scale_color_manual(values = natparks.pals("Yellowstone")) +
  
  labs(y = "", title = "b) Waste trade balance in Megatonnes",
       x = "Year") +
  guides(color = guide_legend(nrow = 2)) +
  coord_cartesian(ylim = c(-75, 75)) +
  
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
ggsave(file = paste0(fig_dir, "waste_volume_balance.png"), width = 10, height = 4.5)



#### _____________________________________________________________________ ####
#### Table with income and waste trade shares by income group ####
#### _____________________________________________________________________ ####


#### set the path to store the figures ####
fig_dir = "04_output/figures/"

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
data = read_rds("02_gen/01_trade/gravity_data.rds")
data = mutate(data, value = value/1000)

#### Aggregate to the total waste exports by country ####
exp <- data %>% filter(prod_class != "total") %>% 
  group_by(country = from, year) %>%
  summarize(exports = sum(volume, na.rm = T))


#### Aggregate to the total waste imports by country ####
imp <- data %>% filter(prod_class != "total") %>% 
  group_by(country = to, year) %>%
  summarize(imports = sum(volume, na.rm = T))

#### Join the data sets together ####
exp = left_join(exp, imp)

#### Include the GDP ####
exp  = left_join(exp, select(data %>% ungroup(), country = from, 
                             income = from_inc, year, gdp = gdp_o) %>% distinct())

#### Aggregate to the income level ####
table =exp %>% group_by(income) %>% 
  summarise(gdp = sum(gdp, na.rm = T), 
            exports = sum(exports, na.rm = T), 
            imports = sum(imports, na.rm = T)) %>% 
  mutate_at(vars(gdp, exports, imports), function(x) 100* (x/sum(x))) %>% 
  mutate(balance_ratio = exports/imports, gdp_exports = gdp/exports, gdp_imports = gdp/imports)

#### Add the correlations ####
table = exp %>% group_by(income) %>% 
  summarise(gdp_exports_cor = cor(gdp, exports, use = "complete.obs"),
            gdp_imports_cor = cor(gdp, imports, use = "complete.obs"),
            imports_exports_cor = cor(imports, exports, use = "complete.obs")) %>% 
  left_join(table, .)

#### Arrange the table by the GDP share ####
table = table %>% ungroup() %>% arrange(desc(gdp))

#### Round to two digits ####
table <- mutate_at(table, vars(gdp:imports_exports_cor), ~formatC(., format = "f", digits = 2))

#### Arrange the data ####
table = select(table, income, gdp, exports, imports, gdp_exports, gdp_imports, balance_ratio)

#### Create the table of main exporters #####
tab = reactable(
  table, theme = fivethirtyeight(font_size = 12, header_font_size = 12,
                                 cell_padding = 5), 
  defaultPageSize = 4,
  defaultColDef = colDef(align = "left"),
  columns = list(income = colDef(maxWidth = 120, align = "left", name = "Income Group"),
                 gdp = colDef(maxWidth = 75, align = "center", name = "GDP\nShare"),
                 exports = colDef(maxWidth = 120, align = "center",  name = "Waste\nExports Share"),
                 gdp_exports = colDef(maxWidth = 120, align = "center",  name = "GDP\nExports\nRatio"),
                 gdp_exports_cor = colDef(maxWidth = 120, align = "center",  name = "GDP Exports (Cor.)"),
                 
                 imports = colDef(maxWidth = 120, align = "center",  name = "Waste\nImports Share"),
                 gdp_imports = colDef(maxWidth = 120, align = "center",  name = "GDP\nImports\nRatio"),
                 
                 gdp_imports_cor = colDef(maxWidth = 120, align = "center",  name = "GDP Imports (Cor.)"),
                 balance_ratio = colDef(maxWidth = 120, align = "center",  name = "Imports Exports Ratio"),
                 imports_exports_cor = colDef(maxWidth = 100, align = "center",  name = "Imports Exports (Cor.)"))); tab

#### Transform the table to png to paste it in the paper ####
html = "kable.html"
htmlwidgets::saveWidget(tab, html)
webshot2::webshot(html, paste0(fig_dir, "waste_income_cor.png"), 
                  cliprect = c(0, 60, 850, 140), zoom = 2, delay = 0.5)


#### _____________________________________________________________________ ####
#### Material Imbalance table with average values ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(mmtable2)
library(cowplot)
library(tidyverse)
library(ggplot2)
library(kableExtra)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Change the name of the product classes and organize ####
data = filter(data, prod_class != "total", year < 2017)
data$prod_class = str_to_title(data$prod_class) 
data$prod_class = gsub("Steel_iron", "Steel and Iron", data$prod_class)
data$prod_class = factor(data$prod_class, levels = c(
  "Plastic", 
  "Paper",
  "Aluminum", 
  "Steel and Iron",
  "Copper"))  

#### Only keep the material imbalance data ####
data = select(data, year, country = from, prod_class, inc_lvl = from_inc, 
              waste_recovery = w_rec_o,
              prod_use = prod_use_o, con_use = con_o, 
              imb_trade = imb_trade_o, imb_recovery = imb_adj_o, 
              pop = pop_o, gdp = gdp_o) %>% distinct()

data = mutate(data, inc_lvl = ifelse(inc_lvl == "High Income", "High Income", "Other" ))
data$gdp = data$gdp/1e6

#### Summarize total exports from the EU to each region #### 
plot <- data %>% 
  group_by(inc_lvl, prod_class)  %>% 
  summarize(`a Avg. Population (Million)` = mean(pop, na.rm = T), 
            `b) Avg. GDP (Billion USD)` = mean(gdp, na.rm = T),
            `e) Waste Recovery` = mean(waste_recovery, na.rm = T),
            `c) Production` = mean(prod_use, na.rm = T),
            `d) Consumption` = mean(con_use, na.rm = T),
            `f) Production - Consumption` = mean(imb_trade, na.rm = T), 
            `g) Production - Waste Recovery` = mean(imb_recovery, na.rm = T)); head(plot)

#### Transform to long format ####
plot = gather(plot, var, value, -c(inc_lvl, prod_class))
plot$value = format(round(plot$value, 2), nsmall = 2)

#### Organize the legend ####
latex = mmtable(data = plot, 
                cells = value, table_name = "Descriptives") +
  header_left(inc_lvl) + header_top(var) + 
  header_left_top(prod_class); latex

#### Transform the table to latex to paste it in the paper####
kable(as.data.frame(latex), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))

#### Save the plot ####
ggsave(file = paste0(fig_dir, "miso_f78_f89.png"), width = 8, height = 3.5)


#### _____________________________________________________________________ ####
#### Material Imbalance from trade by world region and material ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(ggplot2)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
data = read_rds("02_gen/01_trade/gravity_data.rds")

#### Change the name of the product classes and organize ####
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
  geom_line(aes(x = year, y = imb_trade/1e3, color = inc_lvl, group = inc_lvl)) +
  geom_point(aes(x = year, y = imb_trade/1e3, color = inc_lvl, group = inc_lvl)) +
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
ggsave(file = paste0(fig_dir, "miso_f78_f89.png"), width = 8, height = 3.5)


#### _____________________________________________________________________ ####
#### Recycling rates and waste exports by country ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(cowplot)
library(tidyverse)
library(fixest)
library(ggplot2)
library(ggrepel)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds")
data = read_rds("02_gen/01_trade/gravity_data.rds")
baci

#### Only keep EU countries in the waste exports data ####
data = filter(data, from %in% unique(waste$country))
data = filter(data, !(to %in% unique(waste$country)))

#### Only keep total waste exports #####
data = filter(data, prod_class == "total")

#### Aggregate the exports by year and country ####
agg = data %>% group_by(country = from, year) %>% 
  summarise(volume = sum(volume, na.rm = T))

#### Only keep the relevant data from the waste data ####
waste = filter(waste, prod_class == "packaging_waste", unit == "percent",
               operation == "recycling")

#### Include the waste trade volume to the recycling percent ####
plot = left_join(waste, agg)
plot = filter(plot, year > 2000) %>% select(-flag)
plot = mutate(plot, log_volume = log(volume))

#### Create a function to get the last point of the regression line #### 
get_last_point <- function(df) {
  model <- lm(value ~ log_volume, data = df)
  max_log_volume <- min(df$log_volume)
  data.frame(
    country = unique(df$country),
    log_volume = max_log_volume,
    value = predict(model, newdata = data.frame(log_volume = max_log_volume))
  )
}

#### Restrict the data to the 15 largest EU economies ####
test = filter(plot, country %in% c("DEU", "FRA", "ITA", "ESP", "NLD",
                                   "POL", "SWE", "BEL", "AUT", "IRL", 
                                   "DNK", "FIN", "CZE", "PRT", "HUN"))

#### Determine the location of the ggplot label ####
label_data <- test %>%
  group_by(country) %>%
  do(get_last_point(.))


#### Make the scatter plot ####
scatter = ggplot(test, aes(y = value, x = log_volume)) +
  geom_point(aes(color = country), alpha = 0.25) +
  scale_color_manual(values = natparks.pals("Yellowstone", n = 15)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = country, group = country), alpha = 0.5) +
  labs(x = "Total waste exports volume outside the EU (Log)",
       y = "Recycling rate (%)") +
  geom_text_repel(data = label_data, 
                  aes(label = country, color = country)) +
  
  theme(axis.line = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank()) +
  ggpubr::grids("y") +
  guides(color = "none"); scatter
  

#### Estimate the slope of each line ####
est = feols(value ~ log(volume), data = plot, split = ~country)

#### Extrsact the point estimates ####
sum = lapply(est, function(x) data.frame(tidy(x))) %>% 
  bind_rows(., .id = "country") %>% 
  filter(grepl("volume", term)) %>% 
  mutate(country = gsub(".*: ", "", country))

order = arrange(sum, desc(estimate))$country

#### Make the plot with the simple point estimates ####
est_plot = ggplot(sum) +
  geom_bar(aes(y = estimate, x = reorder(country, -estimate), fill = estimate), stat = "identity") +
  geom_errorbar(aes(ymax = estimate + std.error*1.645,
                    ymin = estimate - std.error*1.645, 
                    x = country), alpha = 0.75, width = 0, color = "navy") +
  scale_fill_gradient2(low = "#CB7223", mid = "gray", high = "#0067A2", midpoint = 0) +
  labs(y = "Estimate and 90% CIs\n(2001-2021)", x = "") +
  guides(fill = "none") +
  theme(axis.line = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  ggpubr::grids("y"); est_plot

#### Average foreign recycling share in 2021 ####
plot = filter(read_rds("02_gen/eu_packaging_waste.rds"), 
               prod_class == "packaging_waste", unit == "percent",
               operation %in% c("recycling_intl", "recycling")) %>% 
  group_by(country, operation) %>% summarise(value = mean(value, na.rm = T)) %>% 
  spread(operation, value) %>% mutate(share = 100*(recycling_intl/recycling))
plot$country = factor(plot$country, levels = order)

####
intl_plot = ggplot(plot) +
  geom_bar(aes(y = share, x = country, fill = share), stat = "identity") +
  scale_fill_gradient2(low = "#0067A2", mid = "gray", high = "navy", midpoint = 0) +
  labs(y = "Mean foreign recycling share\n(2018-2022)", x = "") +
  guides(fill = "none") +
  theme(axis.line = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  ggpubr::grids("y"); intl_plot


#### Put both plots together ####
vertical_layout <- plot_grid(est_plot, intl_plot, ncol = 1, rel_heights = c(1, 1))
final_plot <- plot_grid(scatter, vertical_layout, rel_widths = c(1, 1)); final_plot

#### Save the plot ####
ggsave(file = paste0(fig_dir, "eu_waste.png"), width = 10, height = 5)


#### _____________________________________________________________________ ####
#### Recycling share outside the European Union ####
#### _____________________________________________________________________ ####

#### set the path to store the figures ####
fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(conflicted)
library(tidyverse)
library(ggplot2)
library(mmtable2)
library(ggrepel)
library(broom)

#### Solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data set ####
waste = read_rds("02_gen/eu_packaging_waste.rds")

#### Only keep the recycling operations ####
waste = filter(waste, operation %in% c("generated","recycling_eu", "recycling_local", 
                                       "recycling_intl", "recycling"))

#### Determine the type of unit and waste type ####
waste = filter(waste, prod_class == "packaging_waste")

#### Keep the waste generation in tonnes and the recycling rate in percentage ####
waste = filter(waste, unit == "percent" | operation == "generated")
waste = filter(waste, unit == "tonnes" | grepl("recycling", operation))

#### Exclude the flag ####
waste = select(waste, -flag, -unit)

#### Spread from long to wide format ####
waste = spread(waste, operation, value)

#### Only keep data from 2018 ####
waste = filter(waste, year > 2017)

#### Exclude instances when there is no information on local recycling rates ####
waste = filter(waste, is.na(recycling_local) == F)

#### Calculate the recycling share in each market ####
waste = mutate(waste, recycling_eu = 100*(recycling_eu/recycling),
               recycling_intl = 100*(recycling_intl/recycling),
               recycling_local = 100*(recycling_local/recycling))

#### Create the table to put in the paper ####  
table = waste %>% group_by(year) %>% 
  summarise(`a) Avg. Waste Generation (MT)` = mean(generated, na.rm = T)/1e6, 
            `b) Recycling Share` = mean(recycling, na.rm = T), 
            `c) Local Recycling Share` = mean(recycling_local, na.rm = T), 
            `e) EU Recycling Share` = mean(recycling_eu, na.rm = T), 
            `f) International Recycling Share` = mean(recycling_intl, na.rm = T))

#### Transform from wide to long format and round ####
table = gather(table, var, value, -year)
table$value = format(round(table$value, 2), nsmall = 2)

#### Organize the legend ####
latex = mmtable(data = table, 
                cells = value, table_name = "Descriptives") +
  header_left(year) + header_top(var); latex

#### Transform the table to latex to paste it in the paper####
kable(as.data.frame(latex), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))



#### _____________________________________________________________________ ####
#### Pooled results ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(mmtable2)
library(kableExtra)
library(texreg)
library(gt)

#### solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
est = list(read_rds("02_gen/04_results/rr_trade_elasticity_pooled.rds"),
           read_rds("02_gen/04_results/rr_trade_elasticity_pooled_iv.rds")) %>% 
  bind_rows(.)

#### Transform the estimate into a character vector ####
est = mutate_at(est, vars(estimate, std.error), function(x) 
  x = formatC(round(x, 3), format = 'f', digits = 3))
est$estimate = as.character(est$estimate)


#### Add the significance codes to the estimates ####
est = mutate(est, 
             estimate = case_when(p.value <= 0.01 ~ paste0(estimate, "***"), TRUE~estimate),
             estimate = case_when(p.value > 0.01 & p.value <= 0.05 ~ paste0(estimate, "**"), TRUE~estimate),
             estimate = case_when(p.value > 0.05 & p.value <= 0.1~paste0(estimate, "*"), TRUE~estimate)) 

#### Round the numeric variables ####
est = mutate_at(est, vars(r2, f_stat), round, 3)

#### Add parenthesis to the standard errors ####
est$std.error = paste0("(", est$std.error, ")")

#### Only keep the relevant variables ####
est = select(est, 
             `a) Estimate` = estimate, 
             `b) Std.Error` = std.error, 
             
             spec, term, estimator,
             
             `c) N.Obs`= n.obs, 
             `d) N.Countries` = N.countries,
             `e) N.periods` = N.periods,
             `f) R2` = r2,
             `g) F-Stat` = f_stat)

#### Spread from long to wide format ####
tab = gather(est, metric, value, -c(spec, estimator, term))

#### Create an area column to organice the data frame ####
tab = mutate(tab, area = ifelse(metric %in% c("a) Estimate", "b) Std.Error"), "a) First", "b) Second"))

#### Organize the data frame ####
tab = arrange(tab, metric)

#### Add the parenthesis to the spec ####
tab$spec = paste0("(", tab$spec, ")")

#### Filter out urban fires and change the title of the fire types ####
tab =  mutate(tab, term = gsub("\\(Intercept\\)", "Intercept", term)) %>% 
  mutate(term = gsub("log_volume|fit_log_volume", "Waste Exports (Log)", term)) %>% 
  mutate(term = gsub("log(waste_gen)", "Total Waste Gen. (Log)", term)) %>% 
  mutate(term = gsub("log(gdpcap)", "GDP-PC (Log)", term))

#### Organice the estimator ####
tab$estimator = factor(tab$estimator, levels = c("ols", "iv"))

#### Construct the table with the MMTABLE2 package ####
latex = mmtable(data = tab %>% filter(term == "Waste Exports (Log)"), 
                cells = value, table_name = "Descriptives") +
  header_top(estimator) +  header_top_left(spec) + 
  header_left(metric); latex

#### Transform the table to latex to paste it in the paper####
kable(as.data.frame(latex), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))

#### _____________________________________________________________________ ####
#### Pooled results (Time restricted) ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(kableExtra)
library(tidyverse)
library(mmtable2)
library(texreg)
library(gt)

#### solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
est = read_rds("02_gen/04_results/rr_trade_elasticity_pooled_restricted.rds")

#### Transform the estimate into a character vector ####
est = mutate_at(est, vars(estimate, std.error), function(x) 
  x = formatC(round(x, 3), format = 'f', digits = 3))
est$estimate = as.character(est$estimate)


#### Add the significance codes to the estimates ####
est = mutate(est, 
             estimate = case_when(p.value <= 0.01 ~ paste0(estimate, "***"), TRUE~estimate),
             estimate = case_when(p.value > 0.01 & p.value <= 0.05 ~ paste0(estimate, "**"), TRUE~estimate),
             estimate = case_when(p.value > 0.05 & p.value <= 0.1~paste0(estimate, "*"), TRUE~estimate)) 

#### Round the numeric variables ####
est = mutate_at(est, vars(r2, f_stat), round, 3)

#### Add parenthesis to the standard errors ####
est$std.error = paste0("(", est$std.error, ")")

#### Only keep the relevant variables ####
est = select(est, 
             `a) Estimate` = estimate, 
             `b) Std.Error` = std.error, 
             
             spec, term, estimator,
             
             `c) N.Obs`= n.obs, 
             `d) N.Countries` = N.countries,
             `e) N.periods` = N.periods,
             `f) R2` = r2,
             `g) F-Stat` = f_stat)

#### Spread from long to wide format ####
tab = gather(est, metric, value, -c(spec, estimator, term))

#### Create an area column to organice the data frame ####
tab = mutate(tab, area = ifelse(metric %in% c("a) Estimate", "b) Std.Error"), "a) First", "b) Second"))

#### Organize the data frame ####
tab = arrange(tab, metric)

#### Add the parenthesis to the spec ####
tab$spec = paste0("(", tab$spec, ")")

#### Filter out urban fires and change the title of the fire types ####
tab =  mutate(tab, term = gsub("\\(Intercept\\)", "Intercept", term)) %>% 
  mutate(term = gsub("log_volume|fit_log_volume", "Waste Exports (Log)", term)) %>% 
  mutate(term = gsub("log(waste_gen)", "Total Waste Gen. (Log)", term)) %>% 
  mutate(term = gsub("log(gdpcap)", "GDP-PC (Log)", term))

#### Organice the estimator ####
tab$estimator = factor(tab$estimator, levels = c("ols", "iv"))

#### Construct the table with the MMTABLE2 package ####
latex = mmtable(data = tab %>% filter(term == "Waste Exports (Log)"), 
                cells = value, table_name = "Descriptives") +
  header_top(estimator) +  header_top_left(spec) + 
  header_left(metric); latex

#### Transform the table to latex to paste it in the paper####
kable(as.data.frame(latex), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))



#### _____________________________________________________________________ ####
#### Results by material ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(kableExtra)
library(tidyverse)
library(mmtable2)
library(texreg)
library(gt)

#### solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
est = list(read_rds("02_gen/04_results/rr_trade_elasticity_prod.rds"),
           read_rds("02_gen/04_results/rr_trade_elasticity_material_iv.rds")) %>% 
  bind_rows(.)

#### Transform the estimate into a character vector ####
est = mutate_at(est, vars(estimate, std.error), function(x) 
  x = formatC(round(x, 3), format = 'f', digits = 3))
est$estimate = as.character(est$estimate)


#### Add the significance codes to the estimates ####
est = mutate(est, 
             estimate = case_when(p.value <= 0.01 ~ paste0(estimate, "***"), TRUE~estimate),
             estimate = case_when(p.value > 0.01 & p.value <= 0.05 ~ paste0(estimate, "**"), TRUE~estimate),
             estimate = case_when(p.value > 0.05 & p.value <= 0.1~paste0(estimate, "*"), TRUE~estimate)) 

#### Round the numeric variables ####
est = mutate_at(est, vars(r2, f_stat), round, 3)

#### Add parenthesis to the standard errors ####
est$std.error = paste0("(", est$std.error, ")")

#### Only keep the relevant variables ####
est = select(est, 
             `a) Estimate` = estimate, 
             `b) Std.Error` = std.error, 
             
             var, term, estimator,
             
             `c) N.Obs`= n.obs, 
             `d) N.Countries` = N.countries,
             `e) N.periods` = N.periods,
             `f) R2` = r2,
             `g) F-Stat` = f_stat)

#### Spread from long to wide format ####
tab = gather(est, metric, value, -c(var, estimator, term))

#### Create an area column to organize the data frame ####
tab = mutate(tab, area = ifelse(metric %in% c("a) Estimate", "b) Std.Error"), "a) First", "b) Second"))

#### Organize the data frame ####
tab = arrange(tab, metric)

#### Add the parenthesis to the spec ####
tab$spec = paste0("(", tab$spec, ")")

#### Filter out urban fires and change the title of the fire types ####
tab =  mutate(tab, term = gsub("\\(Intercept\\)", "Intercept", term)) %>% 
  mutate(term = gsub("log_volume|fit_log_volume", "Waste Exports (Log)", term)) %>% 
  mutate(term = gsub("log(waste_gen)", "Total Waste Gen. (Log)", term)) %>% 
  mutate(term = gsub("log(gdpcap)", "GDP-PC (Log)", term))

#### Organice the estimator ####
tab$estimator = factor(tab$estimator, levels = c("ols", "iv"))
x = tab %>% filter(term == "Waste Exports (Log)")
#### Construct the table with the MMTABLE2 package ####
latex = mmtable(data = tab %>% filter(term == "Waste Exports (Log)"), 
                cells = value, table_name = "Descriptives") +
  header_top(var) +  header_top_left(estimator) + 
  header_left(metric); latex

#### Transform the table to latex to paste it in the paper####
kable(as.data.frame(latex), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))

#### _____________________________________________________________________ ####
#### DiD results of the green fence and national sword policy ####
#### _____________________________________________________________________ ####

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(mmtable2)
library(kableExtra)
library(texreg)
library(gt)

#### solve conflicts ####
conflict_prefer("filter", "dplyr")

#### Load the data ####
est = read_rds("02_gen/04_results/did_china_metal.rds")

#### Transform the estimate into a character vector ####
est = mutate_at(est, vars(estimate, std.error), round, 3)
est$estimate = as.character(est$estimate)

#### Add the significance codes to the estimates ####
est = mutate(est, 
             estimate = case_when(p.value <= 0.01 ~ paste0(estimate, "***"), TRUE~estimate),
             estimate = case_when(p.value > 0.01 & p.value <= 0.05 ~ paste0(estimate, "**"), TRUE~estimate),
             estimate = case_when(p.value > 0.05 & p.value <= 0.1~paste0(estimate, "*"), TRUE~estimate)) 

#### Round the numeric variables ####
est = mutate_at(est, vars(r2), round, 3)

#### Add parenthesis to the standard errors ####
est$std.error = paste0("(", est$std.error, ")")

#### Only keep the relevant variables ####
est = select(est, 
             `a) Estimate` = estimate, 
             `b) Std.Error` = std.error, 
             
             spec, term, estimator,
             
             `c) N.Obs`= n.obs, 
             `d) N.Countries` = fe_country,
             `e) N.periods` = fe_year,
             `f) R2` = r2)

#### Spread from long to wide format ####
tab = gather(est, metric, value, -c(spec, estimator, term))

#### Create an area column to organice the data frame ####
tab = mutate(tab, area = ifelse(metric %in% c("a) Estimate", "b) Std.Error"), "a) First", "b) Second"))

#### Organize the data frame ####
tab = arrange(tab, metric)

#### Add the parenthesis to the spec ####
tab$spec = paste0("(", tab$spec, ")")

#### Filter out urban fires and change the title of the fire types ####
tab = mutate(tab, term = gsub(":.*", "", term) %>% 
               gsub("_", " ",.) %>% str_to_title(.))

#### Construct the table with the MMTABLE2 package ####
latex = mmtable(data = tab, 
                cells = value, table_name = "Descriptives") +
  header_top(spec) +  header_top_left(term) + 
  header_left(metric); latex

#### Transform the table to latex to paste it in the paper####
kable(as.data.frame(latex), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("HOLD_position"))


#### --------------------------------------------------------------------- #### 
#### Plot of waste exports by material ####
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
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T)) %>% 
  mutate(type = "Total waste exports")

#### Aggregate the exports to China by year and country ####
agg_chn = data %>% filter(to == "CHN") %>% group_by(year, country = from, prod_class) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T)) %>% 
  mutate(type = "Exports to China")

#### Aggregate the exports outside of China by year and country ####
agg_no_chn = data %>% filter(to != "CHN") %>% group_by(year, country = from, prod_class) %>% 
  summarise(volume = sum(volume, na.rm = T), gdp = mean(gdp_o, na.rm = T), 
            pop = mean(pop_o, na.rm = T), gdpcap = mean(gdpcap_o, na.rm = T)) %>% 
  mutate(type = "Exports outside China")


#### Include the waste trade volume to the recycling percent ####
data = bind_rows(agg, agg_chn, agg_no_chn)
data = mutate(data, log_volume = log(volume))

#### Fill NAs in the GDP ####
data = data %>% group_by(country) %>% 
  mutate(gdp = ifelse(is.na(gdp), mean(gdp, na.rm = T), gdp)) %>% 
  ungroup()


#### Create the data set for the plot ####
plot = data %>% group_by(year, prod_class, type) %>% 
  summarise(`Waste Exports (Log)` = weighted.mean(log(volume), na.rm = T, w = gdp)) %>% 
  gather(var, value, -year, -prod_class, -type)

#### Change the name of the product ####
plot$prod_class = str_to_title(plot$prod_class) %>% gsub("_", " and ", .)
plot$type = factor(plot$type, levels = c("Exports to China", "Exports outside China", "Total waste exports"))

#### Make the plot for the common trends ####
ggplot(plot %>% filter(year %in% c(2000:2019), !grepl("Total", type))) +
  geom_line(aes(x = year, y = value, color = prod_class, group = prod_class)) + 
  geom_point(aes(x = year, y = value, color = prod_class, group = prod_class)) +
  geom_vline(aes(xintercept = 2017), color = "navy") +
  geom_vline(aes(xintercept = 2013), color = "darkgreen") +
  scale_color_manual(values = natparks.pals("Yellowstone"))+
  labs(x = "", y = "Tonnes of waste (Log)") +
  facet_wrap(~ type, scales = "free") + coord_cartesian(ylim = c(7, 13.5)) +
  annotate("text", x = 2017, y = Inf, label = "NSP", hjust = -0.25, vjust = 1.5, size = 3, color = "navy") +
  annotate("text", x = 2013, y = Inf, label = "GFI", hjust = -0.25, vjust = 1.5, size = 3, color = "darkgreen") +
  
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
        panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"))

#### Save the plot ####
ggsave(file = paste0(fig_dir, "did_waste_value_trends.png"), width = 8, height = 3.5)


#### --------------------------------------------------------------------- #### 
#### Plot of recycling rates by material ####
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
data = mutate(data, treatment = ifelse(year >= 2017, 1, 0))
data = mutate(data, treated = ifelse(prod_class == "metal", 0, 1))

#### Add the gdp of missing data ####
data = data %>% group_by(country) %>% 
  mutate(gdp = ifelse(is.na(gdp), mean(gdp, na.rm = T), gdp)) %>% 
  ungroup()

#### Construct the data set to check for parallel trends ####
plot = data %>% group_by(year, treated) %>% 
  summarise(`Recycling Rate (PP)` = weighted.mean(value, na.rm = T, w = gdp), 
            `Waste Exports (Log)` = weighted.mean(log(volume), na.rm = T, w = gdp)) %>% 
  gather(var, value, -year, -treated) %>% 
  mutate(treated = ifelse(treated == 1, "Paper and Plastic", "Metal"))

plot = data %>% group_by(year, treated) %>% 
  summarise(`Recycling Rate (PP)` = mean(value, na.rm = T), 
            `Waste Exports (Log)` = mean(log(volume), na.rm = T)) %>% 
  gather(var, value, -year, -treated) %>% 
  mutate(treated = ifelse(treated == 1, "Paper and Plastic", "Metal"))

#### Make the plot for the common trends ####
ggplot(plot %>% filter(year %in% c(2005:2019))) +
  geom_line(aes(x = year, y = value, color = treated, group = treated)) + 
  geom_point(aes(x = year, y = value, color = treated, group = treated)) + 
  facet_wrap(~ var, scales = "free") +
  geom_vline(aes(xintercept = 2017), linetype = "dashed") +
  geom_vline(aes(xintercept = 2013), linetype = "dashed") +
  scale_color_manual(values = natparks.pals("Yellowstone"))+
  labs(x = "", y = "") +
  
  annotate("rect", xmin = 2005, xmax = 2010, ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "darkgrey") +
  annotate("rect", xmin = 2019, xmax = Inf, ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "darkgrey") +
  
  annotate("text", x = 2017, y = Inf, label = "NSP", hjust = 1.1, vjust = 1.5, size = 3, color = "navy") +
  annotate("text", x = 2013, y = Inf, label = "GFI", hjust = 1.1, vjust = 1.5, size = 3, color = "darkgreen") +
  
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
        panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"))


#### Save the plot ####
ggsave(file = paste0(fig_dir, "did_rr_trends.png"), width = 8, height = 3.5)


#### --------------------------------------------------------------------- #### 
#### Plastics plot ####
#### --------------------------------------------------------------------- ####


fig_dir = "04_output/figures/"

#### Load packages ####
library(NatParksPalettes)
library(tidyverse)
library(readxl)

#### Load the data ####
data = read_xlsx("01_data/rPET_prices.xlsx")

colnames(data) = c("date", "rPET food-grade", "rPET flakes", "PET bottle-grade" )
#### Plot the prices ####
plot = gather(data, var, value, -date)

ggplot(plot) +
  geom_line(aes(x = date, y = value, color = var, group = var)) +
  scale_color_manual(values = natparks.pals("Yellowstone"))+
  labs(x = "", y = "Euro per ton") +
  theme(axis.line.x = element_line(), 
        legend.position = "top",
        legend.justification = 0,
        legend.title = element_blank(),
        legend.key = element_blank(),
        strip.background  = element_blank(),
        strip.text = element_text(hjust =0, face = "italic"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"))

#### Save the plot ####
ggsave(file = paste0(fig_dir, "pet_prices.png"), width = 7, height = 4.5)



