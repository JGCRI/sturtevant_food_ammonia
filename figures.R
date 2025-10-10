# This analysis script generates figures for the ammonia and food security paper
#
# originally from AGU2024 scenario set
# by Page Kyle, December 2024
#
# Edited by Jillian Sturtevant and Hassan Niazi, Oct 2025


# load environment ----
source("load.R"); paste0("Figures in ", FIGS_DIR)

# load data ----
ghg_gwp <- read_csv(paste0(DATA_DIR, "ghg_gwp.csv"))

# food_ammonia_proj <- loadProject("food_ammonia.proj")
#
# listScenarios(food_ammonia_proj)
# listQueries(food_ammonia_proj)

# analysis constants ----
ANALYSIS_YEARS <- c(2020, 2035, 2050)
ANALYSIS_YEARS_FUTURE <- c(2035, 2050)
ANALYSIS_REGIONS <- c("Africa_Southern", "Brazil", "China", "India", "USA")

# plot vars ----
FIGS_SAVE <- TRUE
scenario_levels <- c("Year 2020", "elec_NH3_hicost", "elec_NH3_locost", "NGCCS_NH3",
                     "elec_NH3_hicost_NH3ship", "elec_NH3_locost_NH3ship", "NGCCS_NH3_NH3ship")

ammonia_tech_colors = c("coal" = "black",
                        "coal CCS" = "darkblue",
                        "electrolysis" = "green4",
                        "gas" = "gray",
                        "gas CCS" = "blue",
                        "refined liquids" = "brown")

hydrogen_colors = c("wind" = "lightblue",
                    "solar" = "yellow",
                    "nuclear" = "orange")

scenario_colors <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_hicost_NH3ship" = "red",
                     "elec_NH3_locost" = "green4",
                     "elec_NH3_locost_NH3ship" = "green4",
                     "NGCCS_NH3" = "blue",
                     "NGCCS_NH3_NH3ship" = "blue")

# plots ----

## NH3 production by tech ----
## Jill Addition 5/27/2025
ammonia_prod_tech <- getQuery(food_ammonia_proj, "ammonia production by tech") %>%
  # filter(year %in% ANALYSIS_YEARS) %>%
  mutate(technology = if_else(technology == "hydrogen", "electrolysis", technology),
         scenario = factor(scenario, levels = scenario_levels)) %>%
  group_by(scenario, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ammonia_prod_tech_filtered <- ammonia_prod_tech %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(year = factor(year, levels = ANALYSIS_YEARS))

ggplot(ammonia_prod_tech_filtered, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~scenario) +
  labs(x = "", y = "Mt NH3", fill = "NH3 Technology") +
  theme_bw() +
  scale_fill_manual(values = ammonia_tech_colors)

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ammonia_prod_tech_filtered.png"), height = 6, width = 8, units = "in")}


# ammonia_prod_tech <- getQuery(food_ammonia_proj, "ammonia production by tech") %>%
#   filter(year %in% ANALYSIS_YEARS) %>%
#   mutate(technology = if_else(technology == "hydrogen", "electrolysis", technology),
#          scenario = factor(scenario, levels = scenario_levels)) %>%
#   group_by(scenario, technology, year) %>%
#   summarise(value = sum(value)) %>%
#   ungroup()
#
# ggplot(ammonia_prod_tech, aes(x = year, y = value, fill = technology)) +
#   geom_bar(stat = "identity", position = "fill") +
#   facet_wrap(~scenario) +
#   ylab("Mt NH3") +
#   xlab("") +
#   labs(fill = "") +
#   theme_bw() +
#   scale_fill_manual(values = ammonia_tech_colors)
#
# ggsave("figures/ammonia_prod_tech.png", height = 6, width = 8, units = "in")


## H2 production by tech ----
##Jill Addition 5/19/2025
# just as a diagnostic, make sure that the elec_NH3 scenarios have only green hydrogen
hydrogen_prod_tech <- getQuery(food_ammonia_proj, "hydrogen production by tech") %>%
  filter(scenario == "elec_NH3_hicost_NH3ship",
         grepl("elec_NH3", scenario)) %>%
  group_by(scenario, subsector, technology, year) %>%
  summarise(value = sum(value) * 1 / H2_GJ_kg) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

ggplot(hydrogen_prod_tech, aes(x = year, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab("Mt H2") +
  xlab("elec_NH3_hicost_NH3ship") +
  labs(fill = "Hydrogen Sources") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = hydrogen_colors)+
  scale_x_continuous(breaks = seq(min(hydrogen_prod_tech$year), max(hydrogen_prod_tech$year), by = 5))

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "hydrogen_sources_elec_NH3_hicost_NH3ship.png"), height = 5, width = 7, units = "in")}
# ggsave("figures/Jill/hydrogen_sources.png", height = 5, width = 7, units = "in")

## H2 prod 2050
# just as a diagnostic, make sure that the elec_NH3 scenarios have only green hydrogen
hydrogen_prod_tech_2050 <- getQuery(food_ammonia_proj, "hydrogen production by tech") %>%
  filter(year == 2050,
         grepl("elec_NH3", scenario)) %>%
  group_by(scenario, subsector, technology, year) %>%
  summarise(value = sum(value) * 1 / H2_GJ_kg) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

ggplot(hydrogen_prod_tech_2050, aes(x = scenario, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Mt H2") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = hydrogen_colors)

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "hydrogen_sources_2050.png"), height = 5, width = 7, units = "in")}
# ggsave("figures/hydrogen_sources.png", height = 5, width = 7, units = "in")

## GHG emissions from shipping ----
# check the emission reduction from switching to ammonia shipping
ghg_shipping <- getQuery(food_ammonia_proj, "GHG emissions by international shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(ghg_gwp, by = c(ghg = "GHG")) %>%
  mutate(MtCO2e = value * GWP) %>%
  group_by(scenario, ghg, year) %>%
  summarise(MtCO2e = sum(MtCO2e)) %>%
  ungroup()

##Jill Addition 5/19/2025
energy_densities <- read_csv(paste0(DATA_DIR, "energy_densities.csv"))

#Multiply value of hydrogen production (EJ) by 1E9 to convert to GJ, then divide by energy density (GJ/t) to get fuel consumption (t)
fuel_consumption <- getQuery(food_ammonia_proj, "energy inputs to maritime shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(energy_densities, by = c(scenario = "Scenario")) %>%
  mutate(Mtyr= (value/Density)) %>%
  group_by(scenario,year,Fuel) %>%
  summarise(Mtyr = sum(Mtyr)) %>%
  ungroup()
#adjust data for new figure
fuel_consumption_2020 <- filter(fuel_consumption, year == 2020 & scenario == "NGCCS_NH3") %>%
  mutate(scenario = "Year 2020")
fuel_consumption_2050 <- filter(fuel_consumption, year == 2050)
fuel_consumption_plot <- bind_rows(fuel_consumption_2020, fuel_consumption_2050) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

# colors
fuel_colors = c("Petroleum" = "chocolate", "Ammonia" = "lightgreen")

#adjust plot
ggplot(fuel_consumption_plot,
            aes(x = scenario, y = Mtyr, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Fuel Consumption (Mt/year)") +
  xlab("") +
  labs(fill = "Fuel Type") +
  theme_bw() +
  scale_fill_manual(values = fuel_colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 90))
#result is how much fuel is required for different energy technologies producing hydrogen

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fuel_consumption.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/fuel_consumption.png", height = 6, width = 8, units = "in")

# calculate the amount of fuel consumed
subset_data_pet <- fuel_consumption %>% filter(scenario == "elec_NH3_locost")
subset_data_amm <- fuel_consumption %>% filter(scenario == "elec_NH3_locost_NH3ship")

locost_pet<-max(subset_data_pet$Mtyr)
locost_amm<-max(subset_data_amm$Mtyr)
locost_amm-locost_pet



ghg_shipping_2020 <- filter(ghg_shipping, year == 2020 & scenario == "NGCCS_NH3") %>%
  mutate(scenario = "Year 2020")
ghg_shipping_2050 <- filter(ghg_shipping, year == 2050)
ghg_shipping_plot <- bind_rows(ghg_shipping_2020, ghg_shipping_2050) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

ggplot(ghg_shipping_plot,
            aes(x = scenario, y = MtCO2e, fill = ghg)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Mt CO2e") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ghg_shipping.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/ghg_shipping.png", height = 6, width = 8, units = "in")



## H2 prices ----
##Jill Addition 5/19/2025
scenario_colors_J <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_locost" = "green4")
h2_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "H2 central production",
         region == "USA",
         grepl("elec_NH3", scenario)) %>%
  mutate(cost = value * CONV_USD_1975_2020 * H2_GJ_kg,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

ggplot(h2_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  ylab("$/kg") +
  xlab("") +
  ylim(0, NA) +
  theme_bw() +
  scale_color_manual(values = scenario_colors_J) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "h2_prices.png"), height = 4, width = 6, units = "in")}
# ggsave("figures/Jill/h2_prices.png", height = 4, width = 6, units = "in")




# prices
h2_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "H2 central production",
         region == "USA",
         grepl("elec_NH3", scenario)) %>%
  mutate(cost = value * CONV_USD_1975_2020 * H2_GJ_kg,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

ggplot(h2_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  ylab("$/kg") +
  xlab("") +
  ylim(0, NA) +
  theme_bw() +
  scale_color_manual(values = scenario_colors) +
  labs(fill = "")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "h2_prices.png"), height = 4, width = 6, units = "in")}
# ggsave("figures/h2_prices.png", height = 4, width = 6, units = "in")


## N fert prices ----
##Jill Addition 5/19/2025
scenario_colors_J1 <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_locost" = "green4",
                     "NGCCS_NH3" = "blue")
Nfert_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "N fertilizer",
         region %in% ANALYSIS_REGIONS) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))
#graph
ggplot(Nfert_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylim(0, NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(x = "", y = "N Fertilizer Price ($/t NH3)", color = "NH3 Technology")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nfert_prices.png"), height = 5, width = 8, units = "in")}
# ggsave("figures/Jill/Nfert_prices.png", height = 5, width = 8, units = "in")



Nfert_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "N fertilizer",
         region %in% ANALYSIS_REGIONS) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

ggplot(Nfert_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("$/t NH3") +
  ylim(0, NA) +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors) +
  labs(fill = "")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nfert_prices.png"), height = 5, width = 8, units = "in")}
# ggsave("figures/Nfert_prices.png", height = 5, width = 8, units = "in")

## food  prices ----
##Jill Addition 5/27/2025
scenario_colors_J1 <- c("elec_NH3_hicost" = "red",
                        "elec_NH3_locost" = "green4",
                        "NGCCS_NH3" = "blue")
### Wheat ----
wheat_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Wheat",
         region %in% ANALYSIS_REGIONS) %>%
           group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)


ggplot(wheat_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "wheat_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/wheat_price_index.png", height = 6, width = 8, units = "in")

### Rice ----
rice_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Rice",
         region %in% ANALYSIS_REGIONS)%>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

ggplot(rice_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "rice_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/rice_price_index.png", height = 6, width = 8, units = "in")")

### Corn ----
corn_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Corn",
         region %in% ANALYSIS_REGIONS) %>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

ggplot(corn_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "corn_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/corn_price_index.png", height = 6, width = 8, units = "in")

### Soybean ----
soybean_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Soybean",
         region %in% ANALYSIS_REGIONS) %>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

ggplot(soybean_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "soybean_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/soybean_price_index.png", height = 6, width = 8, units = "in")

### Legumes ----
legumes_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Legumes",
         region %in% ANALYSIS_REGIONS)%>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

ggplot(legumes_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "legumes_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/legumes_price_index.png", height = 6, width = 8, units = "in")

## food commodity prices in $/t
wheat_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Wheat",
         region %in% ANALYSIS_REGIONS) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

ggplot(wheat_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("$/t") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors) +
  labs(fill = "")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "wheat_prices.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/wheat_prices.png", height = 6, width = 8, units = "in")

## food demand ----
food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  filter(year %in% ANALYSIS_YEARS_FUTURE,
         region %in% ANALYSIS_REGIONS) %>%
  mutate(type = sub("FoodDemand_", "", input)) %>%
  select(scenario, region, type, year, value) %>%
  left_join(getQuery(food_ammonia_proj, "population by region"),
            by = c("scenario", "region", "year"),
            suffix = c(".pcal", ".pop")) %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR)

ggplot(food_demand, aes(x = scenario, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(year ~ region) +
  ylab("kcal/pers/d") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("Staples" = "brown", NonStaples = "blue")) +
  labs(fill = "")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "food_demand.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/food_demand.png", height = 6, width = 8, units = "in")

food_demand_total <- food_demand %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  mutate(diff_from_max = value / pmax(value)) %>%
  ungroup()

