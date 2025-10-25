# This analysis script generates figures for the ammonia and food security paper
#
# originally from AGU2024 scenario set
# by Page Kyle, December 2024
#
# Edited by Jillian Sturtevant, Hassan Niazi, and Paul Wolfram, Oct 2025


# load environment ----
source("load.R"); paste0("Figures in ", FIGS_DIR)

# load data ----
ghg_gwp <- read_csv(paste0(DATA_DIR, "ghg_gwp.csv"))
region_mapping <- read_csv(paste0(DATA_DIR, "region_mapping.csv"))

# food_ammonia_proj <- loadProject("food_ammonia.proj")
#
# listScenarios(food_ammonia_proj)
# listQueries(food_ammonia_proj)

# analysis constants ----
ANALYSIS_YEARS <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)
ANALYSIS_YEARS_FUTURE <- c(2030, 2035, 2040, 2045, 2050)
ANALYSIS_REGIONS <- c("Africa_Southern", "Brazil", "China", "India", "USA")

# plot vars ----
FIGS_SAVE <- FALSE  # set to TRUE to save figures to FIGS_DIR
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
  filter(year %in% c(2020, 2035, 2050)) %>%
  mutate(year = factor(year, levels = c(2020, 2035, 2050)))

ggplot(ammonia_prod_tech_filtered, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~scenario) +
  labs(x = "", y = "Mt NH3", fill = "NH3 Technology") +
  theme_bw() +
  scale_fill_manual(values = ammonia_tech_colors)

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ammonia_prod_tech_filtered.png"), height = 6, width = 8, units = "in")}


ammonia_prod_tech <- getQuery(food_ammonia_proj, "ammonia production by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(technology = if_else(technology == "hydrogen", "electrolysis", technology),
         scenario = factor(scenario, levels = scenario_levels)) %>%
  group_by(scenario, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ggplot(ammonia_prod_tech, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~scenario) +
  ylab("Mt NH3") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  scale_fill_manual(values = ammonia_tech_colors)

ggsave("figures/ammonia_prod_tech.png", height = 6, width = 8, units = "in")

#################Figure 3#################
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
# ggsave("figures/Draft2/hydrogen_sources.png", height = 5, width = 7, units = "in")


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

#################Figure 1#################
## Fuel consumption for maritime shipping ----
##Jill Addition 5/19/2025
energy_densities <- read_csv(paste0(DATA_DIR, "energy_densities.csv"))

Energy_Densities <- tibble(input = c("delivered diesel", "ammonia energy"),
                           Fuel = c("Petroleum", "Ammonia"),
                           Density_GJpertonne = c(41.868, 18.8))


#Multiply value of hydrogen production (EJ) by 1E9 to convert to GJ, then divide by energy density (GJ/t) to get fuel consumption (t)
fuel_consumption <- getQuery(food_ammonia_proj, "energy inputs to maritime shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(Energy_Densities, by = "input") %>%
  mutate(Mtyr= (value * 1000 / Density_GJpertonne)) %>%
  group_by(scenario,year,Fuel) %>%
  summarise(Mtyr = sum(Mtyr),
            EJyr = sum(value)) %>%
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
# ggsave("figures/D5/Figure1_2050_Mt.png", height = 6, width = 8, units = "in")

# try an alternate one with the ammonia colors broken out
alt_fuel_colors = c("Petroleum" = "chocolate",
                    "Ammonia (green)" = "lightgreen",
                    "Ammonia (blue)" = "skyblue")


alt_Fuel_Consumption_plot <- Fuel_Consumption_plot %>%
  mutate(Fuel = if_else(Fuel == "Ammonia" & grepl("NGCCS", scenario), "Ammonia (blue)", Fuel),
         Fuel = if_else(Fuel == "Ammonia" & grepl("elec_NH3", scenario), "Ammonia (green)", Fuel))

p <- ggplot(alt_Fuel_Consumption_plot,
            aes(x = scenario, y = Mtyr, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Fuel Consumption (Mt/year)") +
  xlab("") +
  labs(fill = "Fuel Type") +
  theme_bw() +
  scale_fill_manual(values = alt_fuel_colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 90))

ggsave("figures/D5/Figure1_alt_2050_Mt.png", height = 6, width = 8, units = "in")

Fuel_Consumption_complete <- Fuel_Consumption %>%
  complete(nesting(scenario,Fuel), year = ANALYSIS_YEARS, fill = list(Mtyr = 0, EJyr = 0)) %>%
  mutate(Fuel = if_else(Fuel == "Ammonia" & grepl("NGCCS", scenario), "Ammonia (blue)", Fuel),
         Fuel = if_else(Fuel == "Ammonia" & grepl("elec_NH3", scenario), "Ammonia (green)", Fuel),
         scenario = factor(scenario, levels = scenario_levels))

p <- ggplot(Fuel_Consumption_complete,
            aes(x = year, y = Mtyr, color = Fuel)) +
  geom_line(linewidth = 1) +
  facet_wrap(~scenario) +
  ylab("Fuel Consumption (Mt)") +
  xlab("") +
  labs(color = "Fuel Type") +
  theme_bw() +
  scale_color_manual(values = alt_fuel_colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 90))

ggsave("figures/D5/Figure1_alt_allyr_Mt.png", height = 6, width = 8, units = "in")


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


#################Figure 2#################
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


#################Figure 4#################
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
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(x = "", y = "N Fertilizer Price ($/t NH3)", color = "NH3 Technology")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nfert_prices.png"), height = 5, width = 8, units = "in")}
# ggsave("figures/Jill/Nfert_prices.png", height = 5, width = 8, units = "in")
# ggsave("figures/Draft2/Nfert_prices.png", height = 5, width = 8, units = "in")


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



# Global map of 2050 showing price increase of ammonia fertilizer relative to 2020 due to technology switching and due to ammonia shipping fuel demand
Nfert_prices_map_2020 <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(sector == "N fertilizer") %>%
  mutate(value = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))  %>%
  filter(year == "2020") %>%
  filter(scenario == "NGCCS_NH3") %>%
  mutate(subRegion = region) %>%
  select(subRegion, value)

p_Nfert_prices_map_2020 <- rmap::map(Nfert_prices_map_2020)



Nfert_prices_map_2050 <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(sector == "N fertilizer") %>%
  mutate(value = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))  %>%
  filter(year == "2050") %>%
  filter(scenario == "NGCCS_NH3") %>%
  mutate(subRegion = region) %>%
  select(subRegion, value)

p_Nfert_prices_map_2050 <- rmap::map(Nfert_prices_map_2050)


Nfert_prices_map_2050_ship <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(sector == "N fertilizer") %>%
  mutate(value = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))  %>%
  filter(year == "2050") %>%
  filter(scenario == "NGCCS_NH3_NH3ship") %>%
  mutate(subRegion = region) %>%
  select(subRegion, value)

p_Nfert_prices_map_2050_ship <- rmap::map(Nfert_prices_map_2050_ship)


N_price_impact <- reduce(
  list(Nfert_prices_map_2020, Nfert_prices_map_2050, Nfert_prices_map_2050_ship),
  function(x, y) merge(x, y, by = "subRegion")
) %>%
  mutate(
    delta_tech = (value.y - value.x) / value.x * 100,
    delta_ship = (value - value.y) / value.y * 100,
    delta_total = delta_tech + delta_ship
  ) %>%
  select(subRegion, delta_total, delta_tech, delta_ship) %>%
  tidyr::gather(key = "scenario", value = "value", -subRegion)

N_price_impact = N_price_impact %>%
  dplyr::mutate(scenario = factor(scenario, levels = c("delta_total","delta_tech","delta_ship")))

mapx <- rmap::map(data = N_price_impact,
                  underLayer = rmap::mapCountries,
                  background = T,
                  #legendFixedBreaks = c(0, 1, 2, 15, 25),
                  #legendType = "pretty",
                  #palette = "BrBG"
                  #legendType = "continuous"
)


if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "rel_fert_price_change.png"), height = 8, width = 8, units = "in")}
if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "rel_fert_price_change.pdf"), height = 8, width = 8, units = "in")}



#################Figure 5#################
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
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "wheat_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/wheat_price_index.png", height = 6, width = 8, units = "in")
# ggsave("figures/Draft2/wheat_price_index.png", height = 6, width = 8, units = "in")

#################Jill 5/27/2025- Not in current draft
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
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(color = "Scenario")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "soybean_price_index.png"), height = 6, width = 8, units = "in")}
# ggsave("figures/Jill/soybean_price_index.png", height = 6, width = 8, units = "in")
# ggsave("figures/Draft2/soybean_price_index.png", height = 6, width = 8, units = "in")

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

#################Figure 6#################
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
# ggsave("figures/Draft2/food_demand.png", height = 6, width = 8, units = "in")

food_demand_total <- food_demand %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  mutate(diff_from_max = value / pmax(value)) %>%
  ungroup()


## macro region food demands ----
# Compile macroregion food demands for alternate, mapped version of food demand figure
macroregion_food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  filter(year == 2035) %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(getQuery(food_ammonia_proj, "population by region"),
            by = c("scenario", "region", "year"),
            suffix = c(".pcal", ".pop")) %>%
  left_join(region_mapping, by = "region") %>%
  group_by(scenario, Region, year) %>%
  summarise(value.pcal = sum(value.pcal),
            value.pop = sum(value.pop)) %>%
  ungroup() %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR)

scenario_colors_unique <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_hicost_NH3ship" = "red4",
                     "elec_NH3_locost" = "green4",
                     "elec_NH3_locost_NH3ship" = "darkgreen",
                     "NGCCS_NH3" = "blue",
                     "NGCCS_NH3_NH3ship" = "darkblue")

ggplot(macroregion_food_demand, aes(x = scenario, y = value, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Region) +
  ylab("kcal/pers/d") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = scenario_colors_unique) +
  labs(fill = "")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "food_demand_allreg.png"), height = 6, width = 8, units = "in")}

# Plot the food demand results on a map
world <- map_data("world")

# Use a function and then loop through the regions manually

create_grob <- function(food_data, region_name){
  p_inset <- ggplot(subset(food_data, Region == region_name),
                  aes(x = scenario, y = value, fill = scenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylab("") +
    xlab("") +
    ylim(c(0,3500)) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = scenario_colors_unique) +
    labs(fill = "")

  # Convert the inset plot to a grob
  g_inset <- ggplotGrob(p_inset)
  return(g_inset)
}
g_nam <- create_grob(food_data = macroregion_food_demand, region_name = "North America")
g_lam <- create_grob(food_data = macroregion_food_demand, region_name = "Latin America")
g_afr <- create_grob(food_data = macroregion_food_demand, region_name = "Africa")
g_eur <- create_grob(food_data = macroregion_food_demand, region_name = "Europe")
g_aus <- create_grob(food_data = macroregion_food_demand, region_name = "Australia_NZ")
g_rus <- create_grob(food_data = macroregion_food_demand, region_name = "Russia")
g_sea <- create_grob(food_data = macroregion_food_demand, region_name = "Southeast Asia")
g_eas <- create_grob(food_data = macroregion_food_demand, region_name = "East Asia")
g_swa <- create_grob(food_data = macroregion_food_demand, region_name = "South and West Asia")

ggplot(world, aes(long, lat, group = group)) +
  geom_polygon(fill = "grey90", color = "gray50") +
  coord_fixed(1.3) +
  theme_bw() +
  # Place inset plots at desired coordinates
  annotation_custom(
    g_nam,
    xmin = -120, xmax = -80,  # longitude bounds
    ymin = 20, ymax = 50) +     # latitude bounds
  annotation_custom(g_lam, xmin = -75, xmax = -35, ymin = -30, ymax = 0) +
  annotation_custom(g_afr, xmin = 0, xmax = 40, ymin = 0, ymax = 30) +
  annotation_custom(g_eur, xmin = -5, xmax = 35, ymin = 35, ymax = 65) +
  annotation_custom(g_aus, xmin = 115, xmax = 155, ymin = -40, ymax = -10) +
  annotation_custom(g_rus, xmin = 80, xmax = 120, ymin = 55, ymax = 85) +
  annotation_custom(g_sea, xmin = 115, xmax = 155, ymin = -10, ymax = 20) +
  annotation_custom(g_eas, xmin = 100, xmax = 140, ymin = 20, ymax = 50) +
  annotation_custom(g_swa, xmin = 50, xmax = 90, ymin = 15, ymax = 45)

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "food_demand_map.png"), height = 6, width = 8, units = "in")}
