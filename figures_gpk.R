# This script generates figures from AGU2024 scenario set
# Page Kyle, December 2024

# install.packages('rgcam')
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('readr')

# First, install the devtools package if you haven't already
# install.packages("devtools")
# Load the devtools package
# library(devtools)
# Install the rgcam package from GitHub
# install_github("JGCRI/rgcam", build_vignettes = TRUE)

library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#################Jill 10/6/2025 - show more years in figures 4-6
ANALYSIS_YEARS <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)
ANALYSIS_YEARS_FUTURE <- c(2030, 2035, 2040, 2045, 2050)
ANALYSIS_REGIONS <- c("Africa_Southern", "Brazil", "China", "India", "USA")
CONV_USD_1975_2020 <- 3.8
CONV_KG_T <- 1000
H2_GJ_kg <- 0.1202
DAYS_PER_YEAR <- 365.25
CONV_PCAL_MCAL <- 1e9

#################Jill 10/6/2025
#setwd("C:/Users/jilli/Documents/PostDoc/JGCRI/Manuscripts/Food Security Paper/FS_AGU24/FS_AGU24")
ghg_gwp <- read_csv("ghg_gwp.csv")

#################Jill 10/6/2025
gcam_data.proj <- loadProject("gcam_data.proj")

#################Jill 10/6/2025
REQUERY_OUTPUT <- FALSE
if(REQUERY_OUTPUT){
  conn <- localDBConn("C:/Users/jilli/Documents/PostDoc/JGCRI/gcam-core/output/", "database_basexdb")
  gcam_data.proj <- addScenario(conn, gcam_data.proj, c("elec_NH3_hicost", "elec_NH3_hicost_NH3ship",
                                                        "elec_NH3_locost", "elec_NH3_locost_NH3ship",
                                                        "NGCCS_NH3", "NGCCS_NH3_NH3ship"),
                                "BatchQueries_ammonia.xml", clobber = TRUE)
}

#################Page's Code
gcam_data.proj <- loadProject("gcam_data.proj")
REQUERY_OUTPUT <- FALSE
if(REQUERY_OUTPUT){
  conn <- localDBConn("/Users/d3p747/Desktop/github/gcam-pkyle/output/", "database_basexdb")
  gcam_data.proj <- addScenario(conn, gcam_data.proj, c("elec_NH3_hicost", "elec_NH3_hicost_NH3ship",
                                                        "elec_NH3_locost", "elec_NH3_locost_NH3ship",
                                                        "NGCCS_NH3", "NGCCS_NH3_NH3ship"),
                                "BatchQueries_ammonia.xml", clobber = TRUE)
}

#################Page's Code
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

#################Jill 5/27/2025- Ammonia Production Technology- Not included in most recent draft
library(dplyr)
ammonia_prod_tech <- getQuery(gcam_data.proj, "ammonia production by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(technology = if_else(technology == "hydrogen", "electrolysis", technology),
         scenario = factor(scenario, levels = scenario_levels)) %>%
  group_by(scenario, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ammonia_prod_tech_filtered <- ammonia_prod_tech %>%
  filter(year %in% c(2020, 2035, 2050)) %>%
  mutate(year = factor(year, levels = c(2020, 2035, 2050)))

p <- ggplot(ammonia_prod_tech_filtered, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~scenario) +
  ylab("Mt NH3") +
  xlab("") +
  labs(fill = "NH3 Technology") +
  theme_bw() +
  scale_fill_manual(values = ammonia_tech_colors)

ggsave("figures/Jill/ammonia_prod_tech.png", height = 6, width = 8, units = "in")

#################Page's Code
ammonia_prod_tech <- getQuery(gcam_data.proj, "ammonia production by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(technology = if_else(technology == "hydrogen", "electrolysis", technology),
         scenario = factor(scenario, levels = scenario_levels)) %>%
  group_by(scenario, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

p <- ggplot(ammonia_prod_tech, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~scenario) +
  ylab("Mt NH3") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  scale_fill_manual(values = ammonia_tech_colors)

ggsave("figures/ammonia_prod_tech.png", height = 6, width = 8, units = "in")

#################Figure 3#################
#################Jill 10/6/2025
hydrogen_prod_tech <- getQuery(gcam_data.proj, "hydrogen production by tech") %>%
  filter(scenario == "elec_NH3_hicost_NH3ship",
         grepl("elec_NH3", scenario)) %>%
  group_by(scenario, subsector, technology, year) %>%
  summarise(value = sum(value) * 1 / H2_GJ_kg) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

p <- ggplot(hydrogen_prod_tech, aes(x = year, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "fill") +
  ylab(NULL) +
  xlab("elec_NH3_hicost_NH3ship") +
  labs(fill = "Hydrogen Sources") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = hydrogen_colors)+
  scale_x_continuous(breaks = seq(min(hydrogen_prod_tech$year), max(hydrogen_prod_tech$year), by = 5))

ggsave("figures/Draft2/hydrogen_sources.png", height = 5, width = 7, units = "in")

#################Page's Code
hydrogen_prod_tech_2050 <- getQuery(gcam_data.proj, "hydrogen production by tech") %>%
  filter(year == 2050,
         grepl("elec_NH3", scenario)) %>%
  group_by(scenario, subsector, technology, year) %>%
  summarise(value = sum(value) * 1 / H2_GJ_kg) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

p <- ggplot(hydrogen_prod_tech_2050, aes(x = scenario, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Mt H2") +
  xlab("") +
  labs(fill = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = hydrogen_colors)

ggsave("figures/hydrogen_sources.png", height = 5, width = 7, units = "in")

# check the emission reduction from switching to ammonia shipping
ghg_shipping <- getQuery(gcam_data.proj, "GHG emissions by international shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(ghg_gwp, by = c(ghg = "GHG")) %>%
  mutate(MtCO2e = value * GWP) %>%
  group_by(scenario, ghg, year) %>%
  summarise(MtCO2e = sum(MtCO2e)) %>%
  ungroup()

#################Figure 1#################
#################Jill 5/27/2025- altered for most recent draft
Energy_Densities <- tibble(input = c("delivered diesel", "ammonia energy"),
                           Fuel = c("Petroleum", "Ammonia"),
                           Density_GJpertonne = c(41.868, 18.8))

#Multiply value of hydrogen production (EJ) by 1E9 to convert to GJ, then divide by energy density (GJ/t) to get fuel consumption (t)
Fuel_Consumption <- getQuery(gcam_data.proj, "energy inputs to maritime shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(Energy_Densities, by = "input") %>%
  mutate(Mtyr= (value * 1000 / Density_GJpertonne)) %>%
  group_by(scenario,year,Fuel) %>%
  summarise(Mtyr = sum(Mtyr),
            EJyr = sum(value)) %>%
  ungroup()

#adjust data for new figure
Fuel_Consumption_2020 <- filter(Fuel_Consumption, year == 2020 & scenario == "NGCCS_NH3") %>%
  mutate(scenario = "Year 2020")
Fuel_Consumption_2050 <- filter(Fuel_Consumption, year == 2050)
Fuel_Consumption_plot <- bind_rows(Fuel_Consumption_2020, Fuel_Consumption_2050) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

#colors
fuel_colors = c("Petroleum" = "chocolate",
                    "Ammonia" = "lightgreen")
#adjust plot
p <- ggplot(Fuel_Consumption_plot,
            aes(x = scenario, y = Mtyr, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Fuel Consumption (Mt/year)") +
  xlab("") +
  labs(fill = "Fuel Type") +
  theme_bw() +
  scale_fill_manual(values = fuel_colors) +  # Apply custom colors
  theme(axis.text.x = element_text(angle = 90))

ggsave("figures/D5/Figure1_2050_Mt.png", height = 6, width = 8, units = "in")

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

#################Figure 1#################
#################Jill 10/6/2025- Total Consumption in Mt/yr of ammonia/petroleum fuel and ammonia fertilizer
# Define scenario levels and colors (grouping -ship scenarios together)
scenario_levels <- c(
  "elec_NH3_hicost",
  "elec_NH3_locost",
  "NGCCS_NH3",
  "elec_NH3_hicost_NH3ship",
  "elec_NH3_locost_NH3ship",
  "NGCCS_NH3_NH3ship"
)





#################Figure 2#################
#################Jill 10/6/2025
scenario_colors_J <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_locost" = "green4")

h2_prices <- getQuery(gcam_data.proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "H2 central production",
         region == "USA",
         grepl("elec_NH3", scenario)) %>%
  mutate(cost = value * CONV_USD_1975_2020 * H2_GJ_kg,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

p <- ggplot(h2_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  ylab("$/kg") +
  xlab("") +
  ylim(0, NA) +
  theme_bw() +
  scale_color_manual(values = scenario_colors_J) +
  labs(color = "Scenario")

ggsave("figures/Draft2/h2_prices.png", height = 4, width = 6, units = "in")

#################Page's Code
# prices
h2_prices <- getQuery(gcam_data.proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "H2 central production",
         region == "USA",
         grepl("elec_NH3", scenario)) %>%
  mutate(cost = value * CONV_USD_1975_2020 * H2_GJ_kg,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

p <- ggplot(h2_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  ylab("$/kg") +
  xlab("") +
  ylim(0, NA) +
  theme_bw() +
  scale_color_manual(values = scenario_colors) +
  labs(fill = "")

ggsave("figures/h2_prices.png", height = 4, width = 6, units = "in")

#################Figure 4#################
#################Jill 10/6/2025
scenario_colors_J1 <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_locost" = "green4",
                     "NGCCS_NH3" = "blue")

Nfert_prices <- getQuery(gcam_data.proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "N fertilizer",
         region %in% ANALYSIS_REGIONS) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

p <- ggplot(Nfert_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("$/t NH3") +
  ylim(750, NA) +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(color = "NH3 Technology")

ggsave("figures/Draft2/Nfert_prices.png", height = 5, width = 8, units = "in")

#################Page's Code
Nfert_prices <- getQuery(gcam_data.proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "N fertilizer",
         region %in% ANALYSIS_REGIONS) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

p <- ggplot(Nfert_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("$/t NH3") +
  ylim(0, NA) +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors) +
  labs(fill = "")

ggsave("figures/Nfert_prices.png", height = 5, width = 8, units = "in")

#################Figure 5#################
#################Jill 10/6/2025
# food commodity prices
scenario_colors_J1 <- c("elec_NH3_hicost" = "red",
                        "elec_NH3_locost" = "green4",
                        "NGCCS_NH3" = "blue")
#################Wheat
wheat_prices <- getQuery(gcam_data.proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Wheat",
         region %in% ANALYSIS_REGIONS) %>%
           group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)


p <- ggplot(wheat_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(color = "Scenario")

ggsave("figures/Draft2/wheat_price_index.png", height = 6, width = 8, units = "in")

#################Jill 5/27/2025- Not in current draft
#################Rice
rice_prices <- getQuery(gcam_data.proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Rice",
         region %in% ANALYSIS_REGIONS)%>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

p <- ggplot(rice_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

ggsave("figures/Jill/rice_price_index.png", height = 6, width = 8, units = "in")

#################Jill 5/27/2025- Not in current draft
#################Corn
corn_prices <- getQuery(gcam_data.proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Corn",
         region %in% ANALYSIS_REGIONS) %>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

p <- ggplot(corn_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

ggsave("figures/Jill/corn_price_index.png", height = 6, width = 8, units = "in")

#################Figure 6#################
#################Jill 10/6/2025
#################Soybean
soybean_prices <- getQuery(gcam_data.proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Soybean",
         region %in% ANALYSIS_REGIONS) %>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

p <- ggplot(soybean_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(color = "Scenario")

ggsave("figures/Draft2/soybean_price_index.png", height = 6, width = 8, units = "in")

#################Jill 5/27/2025- Not in current draft
#################Legumes
legumes_prices <- getQuery(gcam_data.proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Legumes",
         region %in% ANALYSIS_REGIONS)%>%
  group_by(scenario, region) %>% mutate(value = value / value[year==2020]) %>% ungroup()

NH3ship = if_else(grepl("NH3ship", wheat_prices$scenario), TRUE, FALSE)

p <- ggplot(legumes_prices, aes(x = year, y = value, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("Price Index") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(color = "Scenario")

ggsave("figures/Jill/legumes_price_index.png", height = 6, width = 8, units = "in")

#################Page's Code
#food commodity prices
wheat_prices <- getQuery(gcam_data.proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "Wheat",
         region %in% ANALYSIS_REGIONS) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

p <- ggplot(wheat_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  ylab("$/t") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors) +
  labs(fill = "")

ggsave("figures/wheat_prices.png", height = 6, width = 8, units = "in")

#################Figure 6#################
#################Jill 10/6/2025
#Food demand
ANALYSIS_YEARS_FUTURE <- c(2035, 2050)

food_demand <- getQuery(gcam_data.proj, "food demand") %>%
  filter(year %in% ANALYSIS_YEARS_FUTURE,
         region %in% ANALYSIS_REGIONS) %>%
  mutate(type = sub("FoodDemand_", "", input)) %>%
  select(scenario, region, type, year, value) %>%
  left_join(getQuery(gcam_data.proj, "population by region"),
            by = c("scenario", "region", "year"),
            suffix = c(".pcal", ".pop")) %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR)

p <- ggplot(food_demand, aes(x = scenario, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(year ~ region) +
  ylab("kcal/pers/d") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("Staples" = "brown", NonStaples = "blue")) +
  labs(fill = "")

ggsave("figures/Draft2/food_demand.png", height = 6, width = 8, units = "in")

food_demand_total <- food_demand %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  mutate(diff_from_max = value / pmax(value)) %>%
  ungroup()


