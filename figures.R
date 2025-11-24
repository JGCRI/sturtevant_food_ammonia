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
# energy_densities <- read_csv(paste0(DATA_DIR, "energy_densities.csv"))

# food_ammonia_proj <- loadProject("food_ammonia.proj")
#
# listScenarios(food_ammonia_proj)
# listQueries(food_ammonia_proj)

# analysis constants ----
ANALYSIS_YEARS <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)
ANALYSIS_YEARS_FUTURE <- c(2030, 2035, 2040, 2045, 2050)
ANALYSIS_REGIONS <- c("Africa_Southern", "Brazil", "China", "India", "USA", "Indonesia")

# plot vars ----
# TODO: rename refined liquids to petroleum
FIGS_SAVE <- FALSE  # set to TRUE to save figures to FIGS_DIR
scenario_levels <- c("Year 2020", "elec_NH3_hicost", "elec_NH3_locost", "NGCCS_NH3",
                     "elec_NH3_hicost_NH3ship", "elec_NH3_locost_NH3ship", "NGCCS_NH3_NH3ship")

ammonia_tech_colors = c("coal" = "black",
                        "coal CCS" = "gray50",
                        "electrolysis" = "lightgreen",
                        "gas" = "dodgerblue",
                        "gas CCS" = "lightblue",
                        "refined liquids" = "brown3")

hydrogen_colors = c("wind" = "lightblue",
                    "solar" = "yellow",
                    "nuclear" = "orange")

scenario_colors <- c("elec_NH3_hicost" = "red",
                     "elec_NH3_hicost_NH3ship" = "red",
                     "elec_NH3_locost" = "green4",
                     "elec_NH3_locost_NH3ship" = "green4",
                     "NGCCS_NH3" = "blue",
                     "NGCCS_NH3_NH3ship" = "blue")

scenario_colors_unique <- c("elec_NH3_hicost" = "red",
                            "elec_NH3_hicost_NH3ship" = "red4",
                            "elec_NH3_locost" = "green4",
                            "elec_NH3_locost_NH3ship" = "darkgreen",
                            "NGCCS_NH3" = "blue",
                            "NGCCS_NH3_NH3ship" = "darkblue")

# for H2 prices
scenario_colors_J <- c("elec_NH3_hicost" = "red",
                       "elec_NH3_locost" = "green4")

# for NH3 prices
scenario_colors_J1 <- c("elec_NH3_hicost" = "red",
                        "elec_NH3_locost" = "green4",
                        "NGCCS_NH3" = "blue")


# fuel colors
fuel_colors = c("Petroleum" = "chocolate", "Ammonia" = "green3")


# for fuels: try an alternate one with the ammonia colors broken out
alt_fuel_colors = c("Petroleum" = "chocolate",
                    "Ammonia (green)" = "green3",
                    "Ammonia (blue)" = "dodgerblue")


# H2 tech and subsector colors
h2_tech_colors = c("biomass to H2" = "green3",
                     "biomass to H2 CCS" = "green",
                     "coal chemical CCS" = "gray20",
                     "electrolysis" = "lightgreen",
                     "gas ATR CCS" = "darkblue",
                     "natural gas steam reforming" = "dodgerblue")

h2_subsector_colors = c("biomass" = "green3",
                         "coal" = "gray20",
                         "electricity" = "lightgreen",
                         "gas" = "dodgerblue",
                         "nuclear" = "orange",
                         "solar" = "yellow1",
                         "wind" = "lightblue2")


mytheme <- theme_minimal() + theme(
  panel.background = element_blank(),
  # panel.grid.major = element_blank(),
  panel.grid.major = element_line(color = "gray95", linewidth = 0.2),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(fill = NA, color = "black"),
  strip.text = element_text(face = "bold"),
  # plot.title = element_text(face = "bold"),
  # show x and y ticks
  axis.ticks = element_line(color = "black"),
  # legend.position = "bottom"
  legend.text = element_text(size = 9),     # labels inside the legend
  legend.title = element_text(size = 9, face = "bold")
)



# plots ----

###############################################################################%

# Fig 1 Fuel consumption for maritime shipping ----
Energy_Densities <- tibble(input = c("delivered diesel", "ammonia energy"),
                           Fuel = c("Petroleum", "Ammonia"),
                           Density_GJpertonne = c(41.868, 18.8))


# multiply value of hydrogen production (EJ) by 1E9 to convert to GJ, then divide by energy density (GJ/t) to get fuel consumption (t)
fuel_consumption <- getQuery(food_ammonia_proj, "energy inputs to maritime shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(Energy_Densities, by = "input") %>%
  mutate(Mtyr= (value * 1e9 / (Density_GJpertonne * 1e6))) %>%
  group_by(scenario, year, Fuel) %>%
  summarise(Mtyr = sum(Mtyr),
            EJyr = sum(value)) %>%
  ungroup()

# chose elements to plot
fuel_consumption_2020 <- filter(fuel_consumption, year == 2020 & scenario == "NGCCS_NH3") %>%
  mutate(scenario = "Year 2020")
fuel_consumption_2050 <- filter(fuel_consumption, year == 2050)
fuel_consumption_plot <- bind_rows(fuel_consumption_2020, fuel_consumption_2050) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

# fuel consumption by scenario
ggplot(fuel_consumption_plot,
       aes(x = scenario, y = Mtyr, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Fuel Consumption (Mt/year)", fill = "Fuel Type") +
  mytheme +
  scale_fill_manual(values = fuel_colors) +
  theme(axis.text.x = element_text(angle = 90))
# result is how much fuel is required for different energy technologies producing hydrogen

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1fuel_consumption.png"), height = 6, width = 8, units = "in")}

# alt plot with blue and green ammonia
alt_Fuel_Consumption_plot <- fuel_consumption_plot %>%
  mutate(Fuel = if_else(Fuel == "Ammonia" & grepl("NGCCS", scenario), "Ammonia (blue)", Fuel),
         Fuel = if_else(Fuel == "Ammonia" & grepl("elec_NH3", scenario), "Ammonia (green)", Fuel))

ggplot(alt_Fuel_Consumption_plot,
       aes(x = scenario, y = Mtyr, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Fuel Consumption (Mt/year)", fill = "Fuel Type") +
  mytheme +
  scale_fill_manual(values = alt_fuel_colors) +
  theme(axis.text.x = element_text(angle = 90))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1_alt_2050_Mt.png"), height = 6, width = 8, units = "in")}

Fuel_Consumption_complete <- fuel_consumption %>%
  complete(nesting(scenario,Fuel), year = ANALYSIS_YEARS, fill = list(Mtyr = 0, EJyr = 0)) %>%
  mutate(Fuel = if_else(Fuel == "Ammonia" & grepl("NGCCS", scenario), "Ammonia (blue)", Fuel),
         Fuel = if_else(Fuel == "Ammonia" & grepl("elec_NH3", scenario), "Ammonia (green)", Fuel),
         scenario = factor(scenario, levels = scenario_levels))

# Mt in weight terms
ggplot(Fuel_Consumption_complete,
       aes(x = year, y = Mtyr, color = Fuel)) +
  geom_line(linewidth = 1) +
  facet_wrap(~scenario) +
  labs(x = "", y = "Fuel Consumption (Mt)", color = "Fuel Type") +
  mytheme +
  scale_color_manual(values = alt_fuel_colors) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1_alt_allyr_Mt.png"), height = 6, width = 8, units = "in")}

# EJ in energy terms
ggplot(Fuel_Consumption_complete,
       aes(x = year, y = EJyr, color = Fuel)) +
  geom_line(linewidth = 1) +
  facet_wrap(~scenario) +
  ylab("Fuel Consumption (EJ)") +
  xlab("") +
  labs(color = "Fuel Type") +
  theme_bw() +
  scale_color_manual(values = alt_fuel_colors) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1_alt_allyr_EJ.png"), height = 6, width = 8, units = "in")}


## combined fuels and energy ----

# scale energy to match fuel consumption visually
scale_factor <- max(Fuel_Consumption_complete$Mtyr, na.rm = TRUE) /
  max(Fuel_Consumption_complete$EJyr, na.rm = TRUE)

# combined lines
ggplot(Fuel_Consumption_complete) +
  geom_line(aes(x = year, y = Mtyr, color = Fuel), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = year, y = EJyr * scale_factor, color = Fuel), linetype = "dashed", linewidth = 1) +
  facet_wrap(~scenario) +
  scale_y_continuous(name = "Fuel Consumption (Mt)",
                     sec.axis = sec_axis(~ . / scale_factor, name = "Energy (EJ/yr)")) +
  labs(x = "", color = "Fuel Type", fill = "Fuel Type") +
  theme_bw() +
  scale_color_manual(values = alt_fuel_colors) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")


# combined with bar chart
fig1 <- ggplot(Fuel_Consumption_complete, aes(x = year)) +
  # bar chart for EJyr (scaled to match Mtyr visually)
  geom_bar(aes(y = EJyr * scale_factor, fill = Fuel),
           stat = "identity", position = "stack", alpha = 0.2) +
  geom_text(data = subset(Fuel_Consumption_complete, EJyr > 0),
            aes(y = EJyr * scale_factor, label = round(EJyr, 1), group = Fuel),
            position = position_stack(vjust = 1), size = 3, color = "gray70") +
  # line for Mtyr
  geom_line(aes(y = Mtyr, color = Fuel), linewidth = 1) +
  facet_wrap(~scenario) +
  scale_y_continuous(name = "<b>Fuel Consumption (Mt)</b> <span style='color:gray;'>── line</span>",
                     sec.axis = sec_axis(~ . / scale_factor,
                                         name = "<b>Fuel Energy (EJ)</b> <span style='color:gray;'>■ bar</span>")) +
  # scale_y_continuous(name = "Fuel Consumption (Mt) — ⎯ line",
  #                    sec.axis = sec_axis(~ . / scale_factor, name = "Energy (EJ/yr) — ■ bar")) +
  # scale_y_continuous(name = "Fuel Consumption (Mt)",
  #                    sec.axis = sec_axis(~ . / scale_factor, name = "Energy (EJ/yr)")) +
  labs(x = "", color = "Fuel Type", fill = "Fuel Type") +
  scale_color_manual(values = alt_fuel_colors) +
  scale_fill_manual(values = alt_fuel_colors) +
  theme_bw() +
  mytheme +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom",
        axis.title.y = element_markdown(size = 11),
        axis.title.y.right = element_markdown(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold")
        )
fig1

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1_fuel_energy.png"), height = 4, width = 6, units = "in")}

# calculate the amount of fuel consumed
subset_data_pet <- fuel_consumption %>% filter(scenario == "elec_NH3_locost")
subset_data_amm <- fuel_consumption %>% filter(scenario == "elec_NH3_locost_NH3ship")

locost_pet<-max(subset_data_pet$Mtyr)
locost_amm<-max(subset_data_amm$Mtyr)
locost_amm-locost_pet



###############################################################################%

# Fig 2 H2 prices ----

h2_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "H2 central production",
         grepl("elec_NH3", scenario)) %>%
  mutate(cost = value * CONV_USD_1975_2020 * H2_GJ_kg,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))


h2_prices_stats <- h2_prices %>%
  summarise(mean_cost = mean(cost, na.rm = TRUE),
            min_cost  = min(cost, na.rm = TRUE),
            max_cost  = max(cost, na.rm = TRUE))

# all regions
ggplot(h2_prices) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship), linewidth = 0.5) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = mean(h2_prices$cost), linetype = "dotted", color = "gray50") +
  geom_hline(yintercept = max(h2_prices$cost), linetype = "dotted", color = "red3") +
  geom_hline(yintercept = min(h2_prices$cost), linetype = "dotted", color = "green3") +
  geom_text(data = h2_prices_stats,
            aes(x = max(h2_prices$year), y = max(h2_prices$cost), label = paste0("Global Max: $", round(max(h2_prices$cost), 2), "/kg")),
            hjust = 1, vjust = -0.5, color = "red3", size = 2) +
  geom_text(data = h2_prices_stats,
            aes(x = max(h2_prices$year), y = mean(h2_prices$cost), label = paste0("Global Mean: $", round(mean(h2_prices$cost), 2), "/kg")),
            hjust = 1, vjust = -0.5, color = "gray50", size = 2) +
  geom_text(data = h2_prices_stats,
            aes(x = max(h2_prices$year), y = min(h2_prices$cost), label = paste0("Global Min: $", round(min(h2_prices$cost), 2), "/kg")),
            hjust = 1, vjust = 1.5, color = "green4", size = 2) +
  labs(x = "", y = "Hydrogen Price (2020$/kgH2)", color = "Scenario") +
  ylim(0, 6.95) +
  facet_wrap(~region, ncol = 8) +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J)

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_h2_prices_allregions.png"), height = 9, width = 16, units = "in")}

# only min, max and USA region
ggplot(h2_prices %>% filter(region %in% c("Indonesia", "USA", "South Africa")) %>%
       mutate(region = factor(region, levels = c("Indonesia", "USA", "South Africa")))) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship), linewidth = 0.5) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = mean(h2_prices$cost), linetype = "dotted", color = "gray50") +
  geom_hline(yintercept = max(h2_prices$cost), linetype = "dotted", color = "red3") +
  geom_hline(yintercept = min(h2_prices$cost), linetype = "dotted", color = "green3") +
  geom_text(data = h2_prices_stats,
            aes(x = min(h2_prices$year), y = max(h2_prices$cost), label = paste0("Global Max: $", round(max(h2_prices$cost), 2), "/kg")),
            hjust = 0, vjust = -0.5, color = "red3", size = 3) +
  geom_text(data = h2_prices_stats,
            aes(x = mean(h2_prices$year), y = mean(h2_prices$cost), label = paste0("Global Mean: $", round(mean(h2_prices$cost), 2), "/kg")),
            hjust = 0.5, vjust = -0.5, color = "gray50", size = 3) +
  geom_text(data = h2_prices_stats,
            aes(x = max(h2_prices$year), y = min(h2_prices$cost), label = paste0("Global Min: $", round(min(h2_prices$cost), 2), "/kg")),
            hjust = 1, vjust = 1.5, color = "green4", size = 3) +
  labs(x = "", y = "Hydrogen Price (2020$/kgH2)", color = "Scenario") +
  ylim(0, 6.95) +
  facet_wrap(~region, ncol = 8) +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J)

# USA prices
fig2 <- ggplot(h2_prices %>% filter(region == "USA")) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(x = "", y = "Hydrogen Price (2020$/kgH2)", color = "Scenario") +
  ylim(0, NA) +
  mytheme +
  theme(axis.title.y = element_text(face = "bold"), legend.position = c(0.15, 0.15))

fig2

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_h2_prices_usa.png"), height = 4, width = 6, units = "in")}


###############################################################################%
# Fig 3 H2 production by tech ----

## everything H2 production ----
hydrogen_prod_tech <- getQuery(food_ammonia_proj, "hydrogen production by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, subsector, technology, year) %>%
  summarise(value = sum(value) * 1 / H2_GJ_kg) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

(
  # stacked by subsector
  ggplot(hydrogen_prod_tech, aes(x = year, y = value, fill = subsector)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~scenario) +
    labs(x = "", y = "H2 production Source (Mt H2)", fill = "Hydrogen Sources") +
    mytheme +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = h2_subsector_colors)+
    scale_x_continuous(breaks = seq(min(ANALYSIS_YEARS), max(ANALYSIS_YEARS), by = 5))

  |
    # filled by subsector
    ggplot(hydrogen_prod_tech, aes(x = year, y = value, fill = subsector)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~scenario) +
    labs(x = "", y = "H2 production Source (Fraction)", fill = "Hydrogen Sources") +
    mytheme +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = h2_subsector_colors)+
    scale_x_continuous(breaks = seq(min(ANALYSIS_YEARS), max(ANALYSIS_YEARS), by = 5))

) / (
  # stacked by tech
  ggplot(hydrogen_prod_tech, aes(x = year, y = value, fill = technology)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~scenario) +
    labs(x = "", y = "H2 production Tech (Mt H2)", fill = "Hydrogen Tech") +
    mytheme +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = h2_tech_colors)+
    scale_x_continuous(breaks = seq(min(ANALYSIS_YEARS), max(ANALYSIS_YEARS), by = 5))
  |
    # filled by tech
    ggplot(hydrogen_prod_tech, aes(x = year, y = value, fill = technology)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~scenario) +
    labs(x = "", y = "H2 production Tech (Fraction)", fill = "Hydrogen Tech") +
    mytheme +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = h2_tech_colors)+
    scale_x_continuous(breaks = seq(min(ANALYSIS_YEARS), max(ANALYSIS_YEARS), by = 5))
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_hydrogen_prod_master.png"), height = 5, width = 7, units = "in")}

# one scenario: make sure that the elec_NH3 scenarios have only green hydrogen
hydrogen_prod_tech_onescen <- hydrogen_prod_tech %>%
  filter(scenario == "elec_NH3_hicost_NH3ship")

ggplot(hydrogen_prod_tech_onescen, aes(x = year, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "elec_NH3_hicost_NH3ship", y = "Fraction of H2 production", fill = "Hydrogen Sources") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = hydrogen_colors)+
  scale_x_continuous(breaks = seq(min(hydrogen_prod_tech$year), max(hydrogen_prod_tech$year), by = 5))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "hydrogen_sources_elec_NH3_hicost_NH3ship.png"), height = 5, width = 7, units = "in")}


## H2 prod 2050 ----

# focus on 2050 only for the elec_NH3 scenarios
hydrogen_prod_tech_2050 <- getQuery(food_ammonia_proj, "hydrogen production by tech") %>%
  filter(year == 2050,
         grepl("elec_NH3", scenario)) %>%
  group_by(scenario, subsector, technology, year) %>%
  summarise(value = sum(value) * 1 / H2_GJ_kg) %>%
  ungroup() %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

ggplot(hydrogen_prod_tech_2050, aes(x = scenario, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Hydrogen production (Mt H2)", fill = "H2 Sources") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = hydrogen_colors)

# get percentages on the bar plot
h2prod_plot <- hydrogen_prod_tech_2050 %>%
  group_by(scenario) %>%
  # arrange by the actual stacking order (bottom to top: wind, solar, nuclear)
  arrange(match(subsector, c("wind", "solar", "nuclear"))) %>%
  mutate(pct = round(100 * value / sum(value), 1),
         pos = cumsum(value) - 0.5 * value,
         # label = paste0(round(value, 1), " Mt\n", round(pct, 0), "%")
         label = paste0(round(pct, 0), "%"))

fig3 <- ggplot(h2prod_plot, aes(x = scenario, y = value, fill = subsector)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label, color = "white"), size = 3, show.legend = T) +
  scale_color_manual(values = "gray60") +
  scale_fill_manual(values = hydrogen_colors) +
  labs(x = "", y = "Hydrogen Production (Mt H2)", fill = "H2 Sources", color = "text") +
  guides(fill = guide_legend(override.aes = list(label = "%", size = 2)), color = "none") +
  mytheme +
  theme(axis.text.x = element_text(angle = 10),
        axis.title.y = element_text(face = "bold"),
        legend.position = c(0.08, 0.9))

fig3

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_hydrogen_prod_2050.png"), height = 5, width = 7, units = "in")}

{# unfinished code

# TODO: backout H2 used for ammonia/fert production and shipping to update figure 1b
h2prodforammonia <- getQuery(food_ammonia_proj, "ammonia production by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  filter(technology == "hydrogen") %>%
  mutate(EJyr= (value * (1 - CONV_NH3_N))) %>%
  group_by(scenario, year) %>%
  summarise(EJyr = sum(EJyr)) %>%
  ungroup()

# get percentages on the bar plot
h2prodnh3_plot <- h2prodforammonia %>%
  filter(year == 2050) %>%
  # scenario == "elec_NH3_hicost_NH3ship") %>%
  group_by(scenario) %>%
  # arrange by the actual stacking order (bottom to top: wind, solar, nuclear)
  # arrange(match(subsector, c("wind", "solar", "nuclear"))) %>%
  mutate(pct = round(100 * EJyr / sum(EJyr), 1),
         pos = cumsum(EJyr) - 0.5 * EJyr,
         # label = paste0(round(value, 1), " Mt\n", round(pct, 0), "%")
         label = paste0(round(pct, 0), "%"))

fig3_ <- ggplot(h2prodnh3_plot, aes(x = scenario, y = EJyr)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label, color = "white"), size = 3, show.legend = T) +
  scale_color_manual(values = "gray60") +
  scale_fill_manual(values = hydrogen_colors) +
  labs(x = "", y = "Hydrogen Production (Mt H2)", fill = "H2 Sources", color = "text") +
  guides(fill = guide_legend(override.aes = list(label = "%", size = 2)), color = "none") +
  mytheme +
  theme(axis.text.x = element_text(angle = 10),
        axis.title.y = element_text(face = "bold"),
        legend.position = c(0.08, 0.9))

fig3_
} # unfinished code

hydrogen_plot_fill <- hydrogen_prod_tech_2050 %>%
  group_by(scenario) %>%
  mutate(share = value / sum(value) * 100) %>%
  ungroup()

ggplot(hydrogen_plot_fill, aes(x = scenario, y = value, fill = subsector)) +
  geom_bar(stat = "identity", position = "fill") +                # normalize to 100%
  geom_text(aes(label = ifelse(share > 1, paste0(round(share, 0), "%"), "")),
            position = position_fill(vjust = 0.5),                # <-- centers text automatically
            size = 3, color = "gray60", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = hydrogen_colors) +
  labs(x = NULL, y = "Share of H2 (%)", fill = "Hydrogen Sources") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom")



###############################################################################%

# True fig 1 combined panels ----
fig1_combo <- (fig1 |
                ((fig3 + theme(legend.position = c(0.1, 0.85),
                              legend.key.size = unit(0.5, "cm"),
                              # axis.text.x = element_text(angle = 0)
                              )) /
                (fig2 + theme(legend.position = c(0.16, 0.19),
                              legend.spacing = unit(0.001, "cm"), # reduce vertical gap between legend breaks
                              legend.key.height = unit(0.4, "cm"), # reduce vertical spacing in legend keys
                              legend.text = element_text(size = 8)))
              ) # + plot_layout(heights = c(0.995, 1.05))
              ) +
  plot_annotation(tag_levels = 'a') +
  plot_layout(widths = c(2, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 14))

fig1_combo

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1_combo.png"), height = 10, width = 15, units = "in")}
if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig1_combo.pdf"), height = 10, width = 15, units = "in")}





###############################################################################%

# Fig 4 NH3 ----

## NH3 production by tech ----
ammonia_prod_tech <- getQuery(food_ammonia_proj, "ammonia production by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(technology = if_else(technology == "hydrogen", "electrolysis", technology),
         scenario = factor(scenario, levels = scenario_levels)) %>%
  group_by(scenario, technology, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# stacked bars
ggplot(ammonia_prod_tech, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~scenario) +
  scale_fill_manual(values = ammonia_tech_colors) +
  labs(x = "", y = "Ammonia Production (Mt NH3)", fill = "NH3 Technology") +
  mytheme # + theme(legend.position = c(0.1, 0.85))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig4_ammonia_prod_tech_vals.png"), height = 6, width = 8, units = "in")}

# fill to 100%
ggplot(ammonia_prod_tech, aes(x = year, y = value, fill = technology)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~scenario) +
  labs(x = "", y = "Ammonia Production (Fraction)", fill = "NH3 Technology") +
  scale_fill_manual(values = ammonia_tech_colors) +
  mytheme +
  theme(legend.position = "bottom")


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig4_ammonia_prod_tech_fracs.png"), height = 6, width = 8, units = "in")}

# add percentages on the bar plot
ammonia_prod_tech_plot <- ammonia_prod_tech %>%
  group_by(scenario, year) %>%
  # arrange by the actual stacking order (bottom to top)
  arrange(match(technology, c("refined liquids", "gas CCS", "gas",
                             "electrolysis", "coal CCS", "coal"))) %>%
  mutate(pct = round(100 * value / sum(value), 1),
         csum = cumsum(value),
         pos = csum - 0.5 * value,
         # label = paste0(round(value, 1), " Mt\n", round(pct, 0), "%")
         label = paste0(round(pct, 0), "%")) %>%
  ungroup()

### Jill addition, 11/14/2025
fig4a_redo <- ggplot(ammonia_prod_tech_plot) +
  geom_bar(aes(x = year, y = value, fill = technology), stat = "identity") +
  facet_wrap(~scenario) +

  geom_label(
    aes(x = year, y = pos, label = ifelse(pct >= 15, label, NA)),
    fill = "white", alpha = 0, color = "deeppink4", size = 2.1, fontface = "bold",
    linewidth = 0, show.legend = FALSE
  ) +

  scale_fill_manual(values = ammonia_tech_colors) +
  labs(x = "", y = "Ammonia Production (Mt NH3)", fill = "NH3 Technology") +
  guides(fill = guide_legend(override.aes = list(label = "%", size = 2))) +
  mytheme +
  theme(axis.title.y = element_text(face = "bold"),
        legend.position = c(0.085, 0.88),
        legend.box.background = element_rect(colour = "deeppink4", size = 0.1),
        legend.spacing = unit(0.001, "cm"),
        legend.key.height = unit(0.4, "cm"))

### Original code by Hassan
fig4a<-ggplot(ammonia_prod_tech_plot) +
  geom_bar(aes(x = year, y = value, fill = technology), stat = "identity") +
  facet_wrap(~scenario) +
  geom_text(aes(x = year, y = pos, label = ifelse(pct >= 15, label, ""),
                color = "gray65"), size = 3, show.legend = F) +
  scale_color_manual(values = "gray65") +
  scale_fill_manual(values = ammonia_tech_colors) +
  labs(x = "", y = "Ammonia Production (Mt NH3)", fill = "NH3 Technology", color = "text") +
  guides(fill = guide_legend(override.aes = list(label = "%", size = 2)), color = "none") +
  mytheme +
  theme(axis.title.y = element_text(face = "bold"),
        legend.position = c(0.085, 0.88),
        legend.box.background = element_rect(colour = "gray60", size = 0.1),
        legend.spacing = unit(0.001, "cm"),
        legend.key.height = unit(0.4, "cm"))

# fill to 100% (with total at the top)
ammonia_totals <- ammonia_prod_tech_plot %>%
  group_by(scenario, year) %>%
  summarise(total = sum(value), .groups = "drop")

ggplot(ammonia_prod_tech_plot) +
  geom_bar(aes(x = year, y = value, fill = technology), stat = "identity", position = "fill") +
  geom_text(data = ammonia_totals,
            aes(x = year, y = 1.05, label = paste0(round(total, 0), "Mt")),
            color = "gray50", size = 3, show.legend = FALSE, fontface = "bold") +
  scale_color_manual(values = "gray65") +
  facet_wrap(~scenario) +
  labs(x = "", y = "Ammonia Production (Fraction)", fill = "NH3 Technology") +
  scale_fill_manual(values = ammonia_tech_colors) +
  mytheme +
  theme(legend.position = "bottom")

fig4a <- ggplot(ammonia_prod_tech_plot) +
  geom_bar(aes(x = year, y = value, fill = technology), stat = "identity", position = "fill") +
  scale_color_manual(values = "gray65") +
  facet_wrap(~scenario) +
  labs(x = "", y = "Ammonia Production (Fraction)", fill = "NH3 Technology") +
  scale_fill_manual(values = ammonia_tech_colors) +
  mytheme +
  theme(legend.position = "bottom")

fig4a


###############################################################################%

## NH3 fert prices ----
Nfert_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector == "N fertilizer") %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T * CONV_NH3_N,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

# analysis regions
ggplot(Nfert_prices %>% filter(region %in% ANALYSIS_REGIONS)) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  facet_wrap(~region, nrow = 1) +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(limits = c(500, NA), breaks = seq(100, 1600, by = 100)) +
  labs(x = "", y = "Ammonia Fertilizer Price (2020$ / t NH3)", color = "Scenario")

# all regions with percentile shading
ggplot(Nfert_prices) +
  # draw an area between 25% and 75% percentile
  geom_ribbon(aes(x = year, y = cost,
                  xmin = min(year), xmax = max(year),
                  ymin = quantile(cost, 0.25), ymax = quantile(cost, 0.75)),
              fill = "gray92", alpha = 0.5) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  facet_wrap(~region, nrow = 1) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = max(Nfert_prices$cost), linetype = "dotted", color = "orange", alpha = 0.5) +
  geom_hline(yintercept = mean(Nfert_prices$cost), linetype = "dotted", color = "gray50", alpha = 1) +
  geom_hline(yintercept = min(Nfert_prices$cost), linetype = "dotted", color = "purple", alpha = 0.5) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  scale_y_continuous(limits = c(500, NA), breaks = seq(100, 1600, by = 100)) +
  labs(x = "", y = "Ammonia Fertilizer Price (2020$ / t NH3)", color = "Scenario") +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90),
        strip.text = element_text(angle = 90, hjust = 0))

# analysis regions with percentile shading
fig4b <- ggplot(Nfert_prices %>% filter(region %in% ANALYSIS_REGIONS)) +
  # draw an area between 25% and 75% percentile
  geom_ribbon(aes(x = year, y = cost,
                  xmin = min(year), xmax = max(year),
                  ymin = quantile(cost, 0.25), ymax = quantile(cost, 0.75)),
              fill = "gray92", alpha = 0.5) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  facet_wrap(~region, nrow = 1) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = max(Nfert_prices$cost), linetype = "dotted", color = "orange", alpha = 0.5) +
  geom_hline(yintercept = mean(Nfert_prices$cost), linetype = "dotted", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = min(Nfert_prices$cost), linetype = "dotted", color = "purple", alpha = 0.5) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  scale_y_continuous(limits = c(500, NA), breaks = seq(100, 1600, by = 100)) +
  labs(x = "", y = "Ammonia Fertilizer Price (2020$ / t NH3)", color = "Scenario") +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

fig4b

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig4_Nfert_prices.png"), height = 5, width = 8, units = "in")}

### all regions ----
ggplot(Nfert_prices) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship), linewidth = 0.5) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = max(Nfert_prices$cost), linetype = "dotted", color = "orange", alpha = 0.5) +
  geom_hline(yintercept = mean(Nfert_prices$cost), linetype = "dotted", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = min(Nfert_prices$cost), linetype = "dotted", color = "purple", alpha = 0.5) +
  geom_text(data = h2_prices_stats,
            aes(x = max(Nfert_prices$year), y = max(Nfert_prices$cost), label = paste0("Global Max: $", round(max(Nfert_prices$cost), 0), "/t")),
            hjust = 1, vjust = -0.5, color = "red3", size = 2, alpha = 0.75) +
  geom_text(data = h2_prices_stats,
            aes(x = max(Nfert_prices$year), y = mean(Nfert_prices$cost), label = paste0("Global Mean: $", round(mean(Nfert_prices$cost), 0), "/t")),
            hjust = 1, vjust = -0.5, color = "gray50", size = 2, alpha = 0.75) +
  geom_text(data = h2_prices_stats,
            aes(x = max(Nfert_prices$year), y = min(Nfert_prices$cost), label = paste0("Global Min: $", round(min(Nfert_prices$cost), 0), "/t")),
            hjust = 1, vjust = 1.5, color = "green4", size = 2, alpha = 0.75) +
  labs(x = "", y = "Ammonia Fertilizer Price (2020$ / t NH3)", color = "Scenario") +
  facet_wrap(~region, ncol = 8) +
  scale_y_continuous(limits = c(500, 1300), breaks = seq(100, 1600, by = 100)) +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1)

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig4_Nfert_prices_allregions.png"), height = 9, width = 16, units = "in")}


# India has the lowest prices, Indonesia has the highest price so plotting those
ggplot(Nfert_prices %>% filter(region %in% c("India", "USA", "South Africa", "Indonesia")) %>%
         mutate(region = factor(region, levels = c("Indonesia", "USA", "South Africa", "India")))) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship), linewidth = 0.5) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = mean(Nfert_prices$cost), linetype = "dotted", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = max(Nfert_prices$cost), linetype = "dotted", color = "red3", alpha = 0.5) +
  geom_hline(yintercept = min(Nfert_prices$cost), linetype = "dotted", color = "green3", alpha = 0.5) +
  geom_text(data = h2_prices_stats,
            aes(x = max(Nfert_prices$year), y = max(Nfert_prices$cost), label = paste0("Global Max: $", round(max(Nfert_prices$cost), 0), "/t")),
            hjust = 1, vjust = -0.5, color = "red3", size = 2, alpha = 0.75) +
  geom_text(data = h2_prices_stats,
            aes(x = max(Nfert_prices$year), y = mean(Nfert_prices$cost), label = paste0("Global Mean: $", round(mean(Nfert_prices$cost), 0), "/t")),
            hjust = 1, vjust = -0.5, color = "gray50", size = 2, alpha = 0.75) +
  geom_text(data = h2_prices_stats,
            aes(x = max(Nfert_prices$year), y = min(Nfert_prices$cost), label = paste0("Global Min: $", round(min(Nfert_prices$cost), 0), "/t")),
            hjust = 1, vjust = 1.5, color = "green4", size = 2, alpha = 0.75) +
  labs(x = "", y = "Ammonia Fertilizer Price (2020$ / t NH3)", color = "Scenario") +
  facet_wrap(~region, ncol = 8) +
  scale_y_continuous(limits = c(500, 1300), breaks = seq(100, 1600, by = 100)) +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1)


# True fig 2 composite ----
## Jill addition 11/14/2025, replacing fig4a with fig4a_redo

fig2_combo <- (
  ((fig4a_redo +
      theme(legend.position = "bottom",
            legend.key.size = unit(0.4, "cm"),
            # axis.text.x = element_text(angle = 0)
      )) |
     (fig4b + theme(legend.position = "bottom",
                    legend.spacing = unit(0.001, "cm"), # reduce vertical gap between legend breaks
                    legend.key.height = unit(0.4, "cm")))
  ) # + plot_layout(heights = c(0.995, 1.05))
) +
  plot_annotation(tag_levels = 'a') +
  plot_layout(widths = c(1, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 14)) &
  theme(axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(face = "bold"))

fig2_combo

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_combo.png"), height = 7, width = 14, units = "in")}
if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_combo.pdf"), height = 7, width = 14, units = "in")}

### Original code from Hassan
fig2_combo <- (
                 ((fig4a + geom_text(data = ammonia_totals,
                                     aes(x = year, y = 1.05, label = paste0(round(total, 0), "Mt")),
                                     color = "gray50", size = 2, show.legend = FALSE, fontface = "bold") +
                     theme(legend.position = "bottom",
                                 legend.key.size = unit(0.4, "cm"),
                                # axis.text.x = element_text(angle = 0)
                 )) |
                   (fig4b + theme(legend.position = "bottom",
                                  legend.spacing = unit(0.001, "cm"), # reduce vertical gap between legend breaks
                                  legend.key.height = unit(0.4, "cm")))
                 ) # + plot_layout(heights = c(0.995, 1.05))
) +
  plot_annotation(tag_levels = 'a') +
  plot_layout(widths = c(1, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 14)) &
  theme(axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(face = "bold"))

fig2_combo

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_combo.png"), height = 7, width = 14, units = "in")}
if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig2_combo.pdf"), height = 7, width = 14, units = "in")}

### NH3 maps ----
# global map of 2050 showing price increase of ammonia fertilizer relative to 2020 due to technology switching and due to ammonia shipping fuel demand
Nfert_prices_map_2020 <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(sector == "N fertilizer") %>%
  mutate(value = value * CONV_USD_1975_2020 * CONV_KG_T * CONV_NH3_N,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))  %>%
  filter(year == "2020") %>%
  filter(scenario == "NGCCS_NH3") %>%
  mutate(subRegion = region) %>%
  select(subRegion, value)

p_Nfert_prices_map_2020 <- rmap::map(Nfert_prices_map_2020, save = F)


Nfert_prices_map_2050 <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(sector == "N fertilizer") %>%
  mutate(value = value * CONV_USD_1975_2020 * CONV_KG_T * CONV_NH3_N,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))  %>%
  filter(year == "2050") %>%
  filter(scenario == "NGCCS_NH3") %>%
  mutate(subRegion = region) %>%
  select(subRegion, value)

p_Nfert_prices_map_2050 <- rmap::map(Nfert_prices_map_2050, save = F)


Nfert_prices_map_2050_ship <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices") %>%
  filter(sector == "N fertilizer") %>%
  mutate(value = value * CONV_USD_1975_2020 * CONV_KG_T * CONV_NH3_N,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))  %>%
  filter(year == "2050") %>%
  filter(scenario == "NGCCS_NH3_NH3ship") %>%
  mutate(subRegion = region) %>%
  select(subRegion, value)

p_Nfert_prices_map_2050_ship <- rmap::map(Nfert_prices_map_2050_ship, save = F)


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
                  save = F
                  #legendFixedBreaks = c(0, 1, 2, 15, 25),
                  #legendType = "pretty",
                  #palette = "BrBG"
                  #legendType = "continuous"
)


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "rel_fert_price_change.png"), height = 8, width = 8, units = "in")}
# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "rel_fert_price_change.pdf"), height = 8, width = 8, units = "in")}


###############################################################################%

# Fig 5 food  prices ----

# TODO: finalize major crops to show
# ANALYSIS_CROPS <- c("Wheat", "Rice", "Corn", "Soybean")

## prices indices ----
ag_prices_index <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS, sector != c("UnmanagedLand")) %>%
  group_by(scenario, sector, region) %>%
  mutate(index = value / value[year == 2020]) %>% ungroup() %>%
  mutate(NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))


### all regions ----
# plot ag prices all regions all crops: sector region
ggplot(ag_prices_index) +
  geom_line(aes(x = year, y = index, color = scenario, linetype = NH3ship)) +
  facet_grid(sector ~ region, # scales = "free_y"
             ) +
  labs(x = "Year", y = "Price Index (rel. 2020)", color = "Scenario") +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ag_price_indices_allregions.png"), height = 19, width = 32, units = "in")}


# plot ag prices all regions all crops: scenario region
ggplot(ag_prices_index %>% filter(grepl("NH3ship", scenario))) +
  geom_line(aes(x = year, y = index, color = sector, linetype = NH3ship)) +
  facet_grid(scenario ~ region, # scales = "free_y"
  ) +
  labs(x = "Year", y = "Price Index (rel. 2020)", color = "Scenario") +
  # scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))


### diff ships ----

# change between NH3ship vs no NH3ship in 2050 as a side plot

# ag prices change in 2050 between NH3ship vs no NH3ship

# separate NH3ship and non-NH3ship scenarios
without_nh3 <- ag_prices_index %>%
  filter(!grepl("_NH3ship$", scenario)) %>%
  select(scenario, sector, region, year, index) %>%
  rename(without_NH3ship = index, base_scenario = scenario)

with_nh3 <- ag_prices_index %>%
  filter(grepl("_NH3ship$", scenario)) %>%
  select(scenario, sector, region, year, index) %>%
  mutate(base_scenario = gsub("_NH3ship$", "", scenario)) %>%
  select(base_scenario, sector, region, year, index) %>%
  rename(with_NH3ship = index)

# join them together
ag_price_diffs <- without_nh3 %>%
  inner_join(with_nh3, by = c("base_scenario", "sector", "year", "region")) %>%
  mutate(diff = with_NH3ship - without_NH3ship,
         pct_diff = round(100 * (with_NH3ship - without_NH3ship) / without_NH3ship, 1),
         abs_pct_diff = abs(pct_diff))


ag_price_diffs_2050 <- ag_price_diffs %>% filter(year == 2050)


### diff ships plots ----
# arrows in 2050 showing price changes due to NH3shipping
ggplot(ag_price_diffs_2050) +
  geom_segment(aes(x = "NH3ship_FALSE", xend = "NH3ship_TRUE",
                   y = without_NH3ship, yend = with_NH3ship),
               arrow = arrow(length = unit(0.1, "cm")), color = "gray50") +
    facet_grid(sector~region)

# heatmap of percentage differences in 2050 prices due to NH3shipping
heat_food_price_indices <- ggplot(ag_price_diffs_2050,
       aes(x = sector, y = region, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~base_scenario) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Change (%)") +
  # if value is positive keep colors red otherwise create the gradient
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(min(ag_price_diffs_2050$pct_diff),
                                                   0,
                                                   max(ag_price_diffs_2050$pct_diff))),
                       name = "Change (%)") +
  labs(x = "", y = "",
       subtitle = paste0("Agricultural Price Index Change in 2050 due to NH3 Shipping. \n",
                  "Positive values indicate price increase due to NH3 shipping. ",
                  # spell out unique region_sector positive combinations
                  "The only positive region_sectors are: ",
                  paste0(ag_price_diffs_2050 %>%
                           filter(pct_diff > 0) %>%
                           mutate(region_sector = paste0(region, "_", sector)) %>%
                           pull(region_sector) %>%
                           unique(), collapse = ", ")
                  )) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10))

heat_food_price_indices


# regions on x axis for composite figure
heat_food_price_indices_reg <- ggplot(ag_price_diffs_2050,
                                  aes(x = region, y = sector, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~base_scenario) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Change (%)") +
  # if value is positive keep colors red otherwise create the gradient
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(min(ag_price_diffs_2050$pct_diff),
                                                  0,
                                                  max(ag_price_diffs_2050$pct_diff))),
                       name = "Change (%)") +
  labs(x = "", y = "",
       subtitle = paste0("Agricultural Price Index Change in 2050 due to NH3 Shipping. \n",
                         "Positive values indicate price increase due to NH3 shipping. ",
                         # spell out unique region_sector positive combinations
                         "The only positive region_sectors are: ",
                         paste0(ag_price_diffs_2050 %>%
                                  filter(pct_diff > 0) %>%
                                  mutate(region_sector = paste0(region, "_", sector)) %>%
                                  pull(region_sector) %>%
                                  unique(), collapse = ", ")
       )) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10))


heat_food_price_indices_reg


# highlight top 50 changes
top_changes <- ag_price_diffs %>%
  group_by(base_scenario) %>%
  slice_max(abs_pct_diff, n = 50)

ggplot(top_changes, aes(x = region, y = sector, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_grid(year~base_scenario) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, name = "% Change"
  ) +
  labs(x = "", y = "", subtitle = "Top 50 Agricultural Price Changes with NH3 Shipping") +
  mytheme +
  theme(plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))


### heat map with labels ----
food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  group_by(scenario, region, input) %>%
  mutate(index = value / value[year == 2020]) %>% ungroup() %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(type = sub("FoodDemand_", "", input)) %>%
  select(scenario, region, type, index, year, value)

# calculate change in food demand from 2020 to 2050
food_demand_changes_lab <- food_demand %>%
  filter(!grepl("_NH3ship$", scenario)) %>% # take out without NH3ship scenarios
  filter(year %in% c(2020, 2050)) %>%
  select(scenario, region, type, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
  mutate(diff = year_2050 - year_2020,
         pct_diff = round(100 * (year_2050 - year_2020) / year_2020, 0),
         # flag significant changes for labeling
         is_significant = (pct_diff) > quantile((pct_diff), 0.75) | (pct_diff) > 50,
         is_significant_abs = abs(pct_diff) > quantile(abs(pct_diff), 0.75) | abs(pct_diff) > 50,
         label_text = paste0(pct_diff, "%"))

# create heatmap with labels
ggplot(food_demand_changes_lab, aes(x = type, y = region, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  # add labels for significant changes
  geom_text(data = filter(food_demand_changes_lab, is_significant),
            aes(label = label_text), color = "gray50", fontface = "bold", size = 2) +
  facet_wrap(~scenario) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),
    values = scales::rescale(c(min(food_demand_changes_lab$pct_diff),
                               0,
                               max(food_demand_changes_lab$pct_diff))),
    name = "Change (%)"
  ) +
  labs(x = "", y = "",
    subtitle = paste0("Food Demand Change from 2020 to 2050. \n",
                      "Range: ", round(min(food_demand_changes_lab$pct_diff), 1), "% to ",
                      round(max(food_demand_changes_lab$pct_diff), 1), "%.",
                      " Labels shown for top 25% of changes.")
  ) +
  mytheme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10))



### crops indices ----

#### Wheat ----
wheat_price_index <- ag_prices_index %>% filter(sector == "Wheat")

fig_wheat_price_index <- ggplot(wheat_price_index %>% filter(region %in% ANALYSIS_REGIONS)) +
  geom_line(aes(x = year, y = index, color = scenario, linetype = NH3ship)) +
  geom_ribbon(aes(x = year, y = index,
                  xmin = min(year), xmax = max(year),
                  ymin = quantile(index, 0.25), ymax = quantile(index, 0.75)),
              fill = "gray90", alpha = 0.5) +
  # geom_hline(yintercept = max(wheat_price_index$index), linetype = "dotted", color = "orange", alpha = 0.75) +
  # geom_hline(yintercept = mean(wheat_price_index$index), linetype = "dotted", color = "gray50", alpha = 0.75) +
  # geom_hline(yintercept = min(wheat_price_index$index), linetype = "dotted", color = "purple", alpha = 0.75) +
  facet_grid(~region) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(limits = c(NA, 1.2)) +
  labs(x = "", y = "Wheat Price Index (rel. 2020)", color = "Scenario") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

fig_wheat_price_index


ggplot(Nfert_prices %>% filter(region %in% ANALYSIS_REGIONS)) +
  # draw an area between 25% and 75% percentile
  geom_ribbon(aes(x = year, y = cost,
                  xmin = min(year), xmax = max(year),
                  ymin = quantile(cost, 0.25), ymax = quantile(cost, 0.75)),
              fill = "gray92", alpha = 0.5) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  facet_wrap(~region, nrow = 1) +
  # add a sample min max mean horizontal lines for reference
  geom_hline(yintercept = max(Nfert_prices$cost), linetype = "dotted", color = "orange", alpha = 0.5) +
  geom_hline(yintercept = mean(Nfert_prices$cost), linetype = "dotted", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = min(Nfert_prices$cost), linetype = "dotted", color = "purple", alpha = 0.5) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  scale_y_continuous(limits = c(500, NA), breaks = seq(100, 1600, by = 100)) +
  labs(x = "", y = "Ammonia Fertilizer Price (2020$ / t NH3)", color = "Scenario") +
  mytheme +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "price_index_wheat.png"), height = 6, width = 8, units = "in")}


#### Rice ----
# not in current draft
rice_price_index <- ag_prices_index %>% filter(sector == "Rice", region %in% ANALYSIS_REGIONS)

ggplot(rice_price_index, aes(x = year, y = index, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(x = "", y = "Rice Price Index (rel. 2020)", color = "Scenario") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "price_index_rice.png"), height = 6, width = 8, units = "in")}


#### Corn ----
corn_price_index <- ag_prices_index %>% filter(sector == "Corn", region %in% ANALYSIS_REGIONS)

ggplot(corn_price_index, aes(x = year, y = index, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  scale_color_manual(values = scenario_colors_J1) +
  labs(x = "", y = "Corn Price Index (rel. 2020)", color = "Scenario") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "price_index_corn.png"), height = 6, width = 8, units = "in")}


#### Soybean ----
soybean_price_index <- ag_prices_index %>% filter(sector == "Soybean")

fig_soybean_price_index <- ggplot(soybean_price_index %>% filter(region %in% ANALYSIS_REGIONS)) +
  geom_line(aes(x = year, y = index, color = scenario, linetype = NH3ship)) +
  geom_ribbon(aes(x = year, y = index,
                  xmin = min(year), xmax = max(year),
                  ymin = quantile(index, 0.25), ymax = quantile(index, 0.75)),
              fill = "gray90", alpha = 0.5) +
  # geom_hline(yintercept = max(soybean_price_index$index), linetype = "dotted", color = "orange", alpha = 0.75) +
  # geom_hline(yintercept = mean(soybean_price_index$index), linetype = "dotted", color = "gray50", alpha = 0.75) +
  # geom_hline(yintercept = min(soybean_price_index$index), linetype = "dotted", color = "purple", alpha = 0.75) +
  facet_grid(~region) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  scale_y_continuous(limits = c(NA, 1.2)) +
  labs(x = "", y = "Soybean Price Index (rel. 2020)", color = "Scenario") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

fig_soybean_price_index

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "price_index_soybean.png"), height = 6, width = 8, units = "in")}


#### Legumes ----
legumes_price_index <- ag_prices_index %>% filter(sector == "Legumes", region %in% ANALYSIS_REGIONS)

ggplot(legumes_price_index, aes(x = year, y = index, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 5)) +
  labs(x = "", y = "Legumes Price Index (rel. 2020)", color = "Scenario") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "price_index_legumes.png"), height = 6, width = 8, units = "in")}


## food commodity prices in $/t ----
ag_prices <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS, sector != c("UnmanagedLand")) %>%
  mutate(cost = value * CONV_USD_1975_2020 * CONV_KG_T,
         NH3ship = if_else(grepl("NH3ship", scenario), TRUE, FALSE))

# plot ag prices all regions all crops: sector region
ggplot(ag_prices) +
  geom_line(aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  facet_grid(sector ~ region, # scales = "free_y"
  ) +
  labs(x = "", y = "Price (2020$/t)", color = "Scenario") +
  scale_color_manual(values = scenario_colors_J1) +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "ag_price_allregions.png"), height = 19, width = 32, units = "in")}

# plot ag prices all regions all crops: scenario region
ggplot(ag_prices %>% filter(!grepl("NH3ship", scenario))) +
  geom_line(aes(x = year, y = cost, color = sector, linetype = NH3ship)) +
  facet_grid(scenario ~ region, # scales = "free_y"
  ) +
  labs(x = "", y = "Price (2020$/t)", color = "Scenario") +
  scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90))


wheat_prices <- ag_prices %>% filter(sector == "Wheat", region %in% ANALYSIS_REGIONS)

ggplot(wheat_prices, aes(x = year, y = cost, color = scenario, linetype = NH3ship)) +
  geom_line() +
  facet_grid(~region) +
  labs(x = "", y = "Wheat Prices (2020$/t)", color = "Scenario") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = scenario_colors_J1)

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "wheat_prices.png"), height = 6, width = 8, units = "in")}

###############################################################################%

# Fig 6 food demand ----
food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  group_by(scenario, region, input) %>%
  mutate(index = value / value[year == 2020]) %>% ungroup() %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(type = sub("FoodDemand_", "", input)) %>%
  select(scenario, region, type, index, year, value)

(
ggplot(food_demand, aes(x = year, y = value, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(type~region) +
  scale_fill_manual(values = scenario_colors_unique) +
  labs(x = "", y = "Food Demand (Pcal/yr)", fill = "") +
  mytheme
) / (
ggplot(food_demand, aes(x = year, y = index, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(type~region) +
  scale_fill_manual(values = scenario_colors_unique) +
  labs(x = "", y = "Food Demand (rel. 2020)", fill = "") +
  mytheme
) + plot_annotation(tag_levels = 'a') +
  plot_layout(guides = "collect") &
  theme(plot.tag = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(face = "bold"))

## per capita daily food demand ----
food_demand_pcd <- food_demand %>%
  # filter(year %in% ANALYSIS_YEARS_FUTURE) %>%
  left_join(getQuery(food_ammonia_proj, "population by region"),
            by = c("scenario", "region", "year"),
            suffix = c(".pcal", ".pop")) %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR)

food_demand_pcd_fut <- food_demand_pcd %>% filter(year %in% ANALYSIS_YEARS_FUTURE)

(
ggplot(food_demand_pcd_fut, aes(x = scenario, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(year ~ region) +
  scale_fill_manual(values = c("Staples" = "forestgreen", NonStaples = "goldenrod1")) +
  labs(x = "", y = "Food Demand (kcal/pers/d)", fill = "") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "right")
)/(
ggplot(food_demand_pcd_fut, aes(x = scenario, y = index, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(year ~ region) +
  scale_fill_manual(values = c("Staples" = "forestgreen", NonStaples = "goldenrod1")) +
  labs(x = "", y = "Food Demand (kcal/pers/d)", fill = "") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "right")
) + plot_annotation(tag_levels = 'a') +
  plot_layout(guides = "collect") &
  theme(plot.tag = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(face = "bold"))

ggplot(food_demand_pcd_fut %>% filter(region %in% ANALYSIS_REGIONS),
       aes(x = scenario, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~ year) +
  scale_fill_manual(values = c("Staples" = "forestgreen", NonStaples = "goldenrod1")) +
  labs(x = "", y = "Food Demand (kcal/pers/d)", fill = "") +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "right")

# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "food_demand.png"), height = 6, width = 8, units = "in")}

food_demand_total <- food_demand %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  mutate(diff_from_max = value / pmax(value)) %>%
  ungroup()


## food demand change ----

# heat maps of change in prices from 2020 to 2050 between staples and nonstaples
# calculate change in food demand from 2020 to 2050
food_demand_changes <- food_demand %>%
  filter(year %in% c(2020, 2050)) %>%
  select(scenario, region, type, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
  mutate(diff = year_2050 - year_2020,
         pct_diff = round(100 * (year_2050 - year_2020) / year_2020, 1))

# heatmap actual values
heat_fooddemandchange <- ggplot(food_demand_changes, aes(x = region, y = scenario, fill = diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~type) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(min(food_demand_changes$diff),
                                                  0,
                                                  max(food_demand_changes$diff))),
                       name = "Change \n(Pcal/yr)") +
  labs(x = "", y = "",
       subtitle = paste0("Food Demand Change from 2020 to 2050. ",
                         "Range: ", round(min(food_demand_changes$diff), 1), " to ",
                         round(max(food_demand_changes$diff), 1), " (Pcal/yr)")) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)
  )

heat_fooddemandchange

# heatmap percentage change
heat_fooddemandchange_pct <- ggplot(food_demand_changes, aes(x = region, y = scenario, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~type) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(min(food_demand_changes$pct_diff),
                                                  0,
                                                  max(food_demand_changes$pct_diff))),
                       name = "Change (%)") +
  labs(x = "", y = "",
    subtitle = paste0("Food Demand Change from 2020 to 2050. ",
                      "Range: ", round(min(food_demand_changes$pct_diff), 1), "% to ",
                      round(max(food_demand_changes$pct_diff), 1), "%")) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)
  )

heat_fooddemandchange_pct

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_food_demand_change_heatmap.png"), height = 6, width = 10, units = "in")}


## food demand per captia change ----

# heat maps of change in prices from 2020 to 2050 between staples and nonstaples
# calculate change in food demand from 2020 to 2050
food_demand_changes_pcd <- food_demand_pcd %>%
  filter(year %in% c(2020, 2050)) %>%
  select(scenario, region, type, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
  mutate(diff = year_2050 - year_2020,
         pct_diff = round(100 * (year_2050 - year_2020) / year_2020, 1))

# heatmap actual values
heat_fooddemandchange_pcd <- ggplot(food_demand_changes_pcd, aes(x = region, y = scenario, fill = diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~type) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(min(food_demand_changes_pcd$diff),
                                                  0,
                                                  max(food_demand_changes_pcd$diff))),
                       name = "Change \n(kcal/pers/d)") +
  labs(x = "", y = "",
       subtitle = paste0("Per Capita Food Demand Change from 2020 to 2050. ",
                         "Range: ", round(min(food_demand_changes_pcd$diff), 1), " to ",
                         round(max(food_demand_changes_pcd$diff), 1), " (kcal/pers/d)")) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)
  )

heat_fooddemandchange_pcd

# heatmap percentage change
heat_fooddemandchange_pct_pcd <- ggplot(food_demand_changes_pcd, aes(x = region, y = scenario, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~type) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(min(food_demand_changes_pcd$pct_diff),
                                                  0,
                                                  max(food_demand_changes_pcd$pct_diff))),
                       name = "Change (%)") +
  labs(x = "", y = "",
       subtitle = paste0("Per Capita Food Demand Change from 2020 to 2050. ",
                         "Range: ", round(min(food_demand_changes_pcd$pct_diff), 1), "% to ",
                         round(max(food_demand_changes_pcd$pct_diff), 1), "%")) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10)
  )

heat_fooddemandchange_pct_pcd

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_food_demand_change_heatmap_pcd.png"), height = 6, width = 10, units = "in")}


# True fig 3 food demand -----
fig3_combo <- (
  # (heat_fooddemandchange + theme(axis.text.x = element_text(angle = 45)) ) /
  # (heat_fooddemandchange_pct + theme(axis.text.x = element_text(angle = 45)) ) /
  (heat_fooddemandchange_pcd +
     theme(plot.tag = element_text(face = "bold", size = 14),
           axis.text.x = element_text(angle = 45)) ) /
  # (heat_fooddemandchange_pct_pcd + theme(axis.text.x = element_text(angle = 45)) ) /
  (
    (fig_wheat_price_index + scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
       theme(plot.tag = element_text(face = "bold", size = 14))
     | fig_soybean_price_index) +
     scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
     plot_layout(guides = "collect") +
      theme(plot.tag = element_text(face = "bold", size = 14),
            legend.position = "right")
    )
  / (heat_food_price_indices)
  # / (heat_food_price_indices_reg)
) +
  plot_annotation(tag_levels = 'a') +
  # plot_layout(guides = "collect") &
  theme(plot.tag = element_text(face = "bold", size = 14),
        legend.position = "right",
        # axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(face = "bold"))

fig3_combo

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_combo_pcd.png"), height = 19, width = 15, units = "in")}
if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_combo_pcd.pdf"), height = 19, width = 15, units = "in")}



###############################################################################%

# macro region food demands ----

# compile macroregion food demands for alternate, mapped version of food demand figure
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




###############################################################################%

# GHG emissions from shipping ----
# check the emission reduction from switching to ammonia shipping
ghg_shipping <- getQuery(food_ammonia_proj, "GHG emissions by international shipping") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(ghg_gwp, by = c(ghg = "GHG")) %>%
  mutate(MtCO2e = value * GWP) %>%
  group_by(scenario, ghg, year) %>%
  summarise(MtCO2e = sum(MtCO2e)) %>%
  ungroup()

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


### Jill addition, macroregion food demand 11/19/2025
### Figure 3 aggregated into macro-regions
#food demand ----
food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  group_by(scenario, region, input) %>%
  mutate(index = value / value[year == 2020]) %>% ungroup() %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(type = sub("FoodDemand_", "", input)) %>%
  select(scenario, region, type, index, year, value)

macroregion_food_demand_pcd <- food_demand %>%
  left_join(region_mapping, by = "region") %>%
  group_by(scenario, Region, type, year) %>%
  summarise(value.pcal = sum(value), .groups = "drop") %>%
  left_join(
    getQuery(food_ammonia_proj, "population by region") %>%
      filter(year %in% c(2020, 2050)) %>%
      left_join(region_mapping, by = "region") %>%
      group_by(scenario, Region, year) %>%
      summarise(value.pop = sum(value), .groups = "drop"),
    by = c("scenario", "Region", "year")
  ) %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR)

#Calculate change from 2020 to 2050
food_demand_changes_macro <- macroregion_food_demand_pcd %>%
  select(scenario, Region, type, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "year_") %>%
  mutate(diff = year_2050 - year_2020,
         pct_diff = round(100 * (year_2050 - year_2020) / year_2020, 1))

#plot
heat_fooddemandchange_macro <- ggplot(food_demand_changes_macro, aes(x = Region, y = scenario, fill = diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~type) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),
    values = scales::rescale(c(min(food_demand_changes_macro$diff),
                               0,
                               max(food_demand_changes_macro$diff))),
    name = "Change \n(kcal/pers/d)"
  )+
  labs(x = "", y = "",
       subtitle = paste0("Per Capita Food Demand Change from 2020 to 2050 by Food Type. ",
                         "Range: ", round(min(food_demand_changes_macro$diff), 1), " to ",
                         round(max(food_demand_changes_macro$diff), 1), " (kcal/pers/d)")) +
  mytheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10))

heat_fooddemandchange_macro

### Line graph for figure 3a
#prepare food demand with type
food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  group_by(scenario, region, input) %>%
  mutate(index = value / value[year == 2020]) %>%
  ungroup() %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(type = tolower(sub("FoodDemand_", "", input))) %>%
  select(scenario, region, type, index, year, value)

#aggregate total calories across all regions
total_food_demand_pcd <- food_demand %>%
  group_by(scenario, type, year) %>%
  summarise(value.pcal = sum(value), .groups = "drop") %>%
  left_join(
    getQuery(food_ammonia_proj, "population by region") %>%
      filter(year %in% ANALYSIS_YEARS) %>%
      group_by(scenario, year) %>%
      summarise(value.pop = sum(value), .groups = "drop"),
    by = c("scenario", "year")
  ) %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR) %>%
  filter(!is.na(value), is.finite(value))

#total per capita calories over time
ggplot(total_food_demand_pcd, aes(x = year, y = value, color = scenario, linetype = type)) +
  geom_line(size = 1.2) +
  scale_linetype_manual(values = c("staples" = "solid", "nonstaples" = "dashed")) +
  labs(x = "Year", y = "Per Capita Calories (kcal/pers/day)",
       color = "Scenario", linetype = "Food Type",
       title = "Global Per Capita Food Demand Over Time") +
  ylim(0, NA) +
  mytheme +
  theme(axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(face = "bold"))

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Scenario_Calories.png"), height = 6, width = 8, units = "in")}

### Run wheat and soybean starting line 897 for query

### Agricultural price index change in 2050
# Add macroregion info
ag_price_diffs_2050_macro <- ag_price_diffs_2050 %>%
  left_join(region_mapping, by = "region")

# Heatmap with macroregions
heat_food_price_indices_macro <- ggplot(ag_price_diffs_2050_macro,
                                        aes(x = sector, y = Region, fill = pct_diff)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(~base_scenario) +
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),
    values = scales::rescale(c(min(ag_price_diffs_2050_macro$pct_diff),
                               0,
                               max(ag_price_diffs_2050_macro$pct_diff))),
    name = "Change (%)"
  ) +
  labs(x = "", y = "",
       subtitle = paste0("Agricultural Price Index Change in 2050 due to NH3 Shipping. \n",
                         "Positive values indicate price increase due to NH3 shipping. ",
                         "The only positive macroregion_sectors are: ",
                         paste0(ag_price_diffs_2050_macro %>%
                                  filter(pct_diff > 0) %>%
                                  mutate(region_sector = paste0(Region, "_", sector)) %>%
                                  pull(region_sector) %>%
                                  unique(), collapse = ", ")
       )) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(size = 10))

heat_food_price_indices_macro

# Fig 3 food demand by macroregion -----
fig3_combo <- (
  # (heat_fooddemandchange + theme(axis.text.x = element_text(angle = 45)) ) /
  # (heat_fooddemandchange_pct + theme(axis.text.x = element_text(angle = 45)) ) /
  (heat_fooddemandchange_macro +
     theme(plot.tag = element_text(face = "bold", size = 14),
           axis.text.x = element_text(angle = 45)) ) /
    # (heat_fooddemandchange_pct_pcd + theme(axis.text.x = element_text(angle = 45)) ) /
    (
      (fig_wheat_price_index + scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
         theme(plot.tag = element_text(face = "bold", size = 14))
       | fig_soybean_price_index) +
        scale_x_continuous(breaks = seq(2020, 2050, by = 10)) +
        plot_layout(guides = "collect") +
        theme(plot.tag = element_text(face = "bold", size = 14),
              legend.position = "right")
    )
  / (heat_food_price_indices_macro)
  # / (heat_food_price_indices_reg)
) +
  plot_annotation(tag_levels = 'a') +
  # plot_layout(guides = "collect") &
  theme(plot.tag = element_text(face = "bold", size = 14),
        legend.position = "right",
        # axis.text.x = element_text(angle = 90),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(face = "bold"))

fig3_combo

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_combo_macro.png"), height = 19, width = 15, units = "in")}
if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "fig3_combo_macro.pdf"), height = 19, width = 15, units = "in")}

### Jill addition 11/20/2025 map
# Mapping food demand total kcal/person/day
library(ggplot2)
library(dplyr)
library(maps)

# Load world map
world_map <- map_data("world")

# Get ag price index by country
ag_price_country_index <- getQuery(food_ammonia_proj, "ag commodity prices") %>%
  filter(year %in% ANALYSIS_YEARS, sector != "UnmanagedLand") %>%
  group_by(region, year) %>%
  summarise(value_sum = sum(value), .groups = "drop") %>%
  mutate(index = value_sum / value_sum[year == 2020]) %>%
  filter(year == 2035)

# call in GCAM region dataset
iso_region_GCAM <- read_csv(paste0(DATA_DIR, "iso_GCAM_regID.csv"))
GCAM_region_names <- read_csv(paste0(DATA_DIR, "GCAM_region_names.csv"), skip = 6)

iso_reg <- iso_region_GCAM %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  select(iso,country_name,GCAM_region_ID, region)

ag_price_country_names <- ag_price_country_index %>%
  left_join(iso_reg, by = "region") %>%
  rename(GCAM_region = region)

# Join index to map
map_index <- world_map %>%
  left_join(ag_price_rmap_countries, by = c("region"="country_name"))

# There are country names that don't match between GCAM and Rmap, determine the differences and correct them
unmatched_regions <- world_map %>%
  anti_join(ag_price_rmap_countries, by = c("region" = "country_name")) %>%
  distinct(region) %>%
  arrange(region)

unmatched_country_names <- ag_price_rmap_countries %>%
  anti_join(world_map, by = c("country_name" = "region")) %>%
  distinct(country_name) %>%
  arrange(country_name)

# Combine into a two-column data frame (with NA padding if lengths differ)
comparison_table <- tibble::tibble(
  region = unmatched_regions$region,
  country_name = c(unmatched_country_names$country_name, rep(NA, max(0, nrow(unmatched_regions) - nrow(unmatched_country_names))))
)

write_csv(comparison_table, "unmatched_country_names.csv")

#altered the csv manually to match the Rmap region names to the country names of
#GCAM since Rmap doesn't report countries by iso

# call in
name_map <- read_csv(paste0("unmatched_countries.csv"))

# Add corrected country_name to world_map
world_map_fixed <- world_map %>%
  left_join(name_map, by = "region") %>%
  mutate(country_name = coalesce(country_name, region))

map_index <- world_map_fixed %>%
  left_join(ag_price_rmap_countries, by = "country_name")

# Macroregion centroids for floating plots
region_coords <- data.frame(
  Region = c("North America", "Latin America", "Africa", "Europe", "Australia_NZ", "Russia", "Southeast Asia", "East Asia", "South and West Asia"),
  lon = c(-150, -100, -10, -30, 210, 210, 210, 210, 70),
  lat = c(35, -20, -20, 35, -55, 70, -10, 35, -20)
)

# Scaling factor for bar height
scale_factor <- 200

# Offsets for axis and labels
x_title_offset <- -20
x_label_offset <- -5
x_axis_offset  <-  0
bar_spacing    <-  1.0

# Prepare food demand data
food_demand <- getQuery(food_ammonia_proj, "food demand") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  mutate(type = tolower(sub("FoodDemand_", "", input))) %>%
  select(scenario, region, type, year, value)

# Aggregate to macroregion level
macroregion_food_demand <- food_demand %>%
  left_join(region_mapping, by = "region") %>%
  group_by(scenario, Region, year) %>%
  summarise(value.pcal = sum(value), .groups = "drop") %>%
  left_join(
    getQuery(food_ammonia_proj, "population by region") %>%
      filter(year %in% ANALYSIS_YEARS) %>%
      left_join(region_mapping, by = "region") %>%
      group_by(scenario, Region, year) %>%
      summarise(value.pop = sum(value), .groups = "drop"),
    by = c("scenario", "Region", "year")
  ) %>%
  mutate(value = value.pcal * CONV_PCAL_MCAL / value.pop / DAYS_PER_YEAR) %>%
  filter(year == 2035)

# Bar plot data
bar_data <- macroregion_food_demand %>%
  left_join(region_coords, by = "Region") %>%
  mutate(
    scenario = factor(scenario, levels = unique(scenario)),
    x = lon + x_axis_offset + (as.numeric(scenario) - mean(as.numeric(scenario))) * bar_spacing,
    y0 = lat,
    y1 = lat + value / scale_factor
  )

# Y-axis tick labels
tick_vals <- c(0, 1000, 2000, 3000)
y_ticks <- expand.grid(
  Region = unique(bar_data$Region),
  tick_val = tick_vals
) %>%
  left_join(region_coords, by = "Region") %>%
  mutate(
    y = lat + tick_val / scale_factor,
    x_label = lon + x_label_offset,
    x_title = lon + x_title_offset,
    label = tick_val
  )

# Y-axis title
axis_titles <- region_coords %>%
  mutate(
    x = lon + x_title_offset,
    y = lat + max(tick_vals) / scale_factor / 2,
    label = "kcal/p/d"
  )

# Region labels
region_labels <- bar_data %>%
  group_by(Region) %>%
  summarise(
    lon = first(lon),
    lat = first(lat),
    max_y = max(y1)
  ) %>%
  mutate(label_y = max_y + 8)

# Final plot
ggplot() +
  # Base map filled by ag price index
  geom_polygon(data = map_index, aes(x = long, y = lat, group = group, fill = index),
               color = "grey60", size = 0.2) +
  scale_fill_gradient(low = "lightyellow", high = "darkred", na.value = "gray", name = "2035 Ag Price Index") +

  # Mini bar plots
  geom_segment(data = bar_data,
               aes(x = x, xend = x, y = y0, yend = y1, color = scenario),
               size = 1.2) +

  # Tick labels
  geom_text(data = y_ticks,
            aes(x = x_label, y = y, label = label),
            hjust = 1, size = 2.2) +

  # Y-axis titles
  geom_text(data = axis_titles,
            aes(x = x, y = y, label = label),
            angle = 90, hjust = 0.5, size = 2.2, fontface = "bold") +

  # Region labels
  geom_label(data = region_labels,
             aes(x = lon, y = label_y, label = Region),
             size = 2.2, fontface = "bold",
             fill = alpha("tan", 0.6), label.size = NA) +

  # Styling
  scale_color_manual(values = scenario_colors_unique, name = "Scenario") +
  coord_fixed(1.3) +
  theme_minimal(base_size = 9) +
  theme(
    legend.title = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Food_Demand_Map.png"), height = 6, width = 8, units = "in")}
