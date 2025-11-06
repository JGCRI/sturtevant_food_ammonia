# Determine food price elasticity by GCAM region
#
# Data from: https://osf.io/zju48/overview
# Paper: https://www.sciencedirect.com/science/article/pii/S095937802500086X
#
# prepared by Jill Sturtevant, edited HN; Nov 2025

source("load.R")

# call in the dataset
ICP_Price_Elasticities <- read_csv(paste0(DATA_DIR, "price_elasticity_ICP.csv")) %>%
  #make the country code lowercase to match GCAM
  mutate(iso = tolower(Country)) %>% select(-Country) %>%
  select(scenario = Year, iso, delta_quantity = Group, delta_price = food, price_elasticity = Price_elasticity)

# copy China for Taiwan
ICP_Price_Elasticities_R <- ICP_Price_Elasticities %>%
  bind_rows(ICP_Price_Elasticities %>% filter(iso == "chn") %>% mutate(iso = "twn"))

# historical: filter s_present and repeat add years HIST_YEARS for all
ICP_Price_Elasticities_R_Yh <- ICP_Price_Elasticities_R %>%
  filter(scenario == "s_present") %>%
  select(-scenario) %>%
  crossing(tibble(year = HIST_YEARS)) %>%
  mutate(scenario = "historical") %>%
  select(scenario, year, iso, delta_quantity, delta_price, price_elasticity)

# future years
ICP_Price_Elasticities_R_Yf <- ICP_Price_Elasticities_R %>%
  filter(scenario != "s_present") %>%
  # split Year into year and scenario
  separate(scenario, into = c("scenario", "year"), sep = "_") %>%
  mutate(year = as.integer(year))

# combine historical and future
ICP_Price_Elasticities_R_Y <- bind_rows(ICP_Price_Elasticities_R_Yh, ICP_Price_Elasticities_R_Yf)

# call in GCAM region dataset
iso_region_GCAM <- read_csv(paste0(DATA_DIR, "iso_GCAM_regID.csv"))
GCAM_region_names <- read_csv(paste0(DATA_DIR, "GCAM_region_names.csv"), skip = 6)

iso_reg <- iso_region_GCAM %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  select(iso, GCAM_region_ID, region)

# merge the csv by iso 3-digit code
# NOTE: only using future elastities to remove the effect of multiple duplicate
# historical entries affecting averages later on
merged_df <- merge(ICP_Price_Elasticities_R_Yf, iso_reg, by = "iso") %>% as_tibble()

# call in food mapping
ICP_to_GCAM_Food <- read_csv(paste0(DATA_DIR, "food_categories.csv"))

# merge ICP data with GCAM regions with food categorization
PE_ICP_GCAM <- left_join(merged_df, ICP_to_GCAM_Food, by = c("delta_price" = "food"))

# Write the data frame to a CSV file in the working directory
# write.csv(PE_ICP_GCAM, file = paste0(DATA_DIR, "price_elasticity_iso_Region.csv"), row.names = FALSE)


# calculate average price elasticity by GCAM region and food category
# NOTE: if later we want to do this by year, can add year to group_by statement

# self price elasticity for SSP2
PE_self_GCAM <- PE_ICP_GCAM %>% filter(!is.na(input)) %>%
  filter(scenario == "SSP2") %>%
  filter(delta_quantity == delta_price) %>% # get self price elasticity
  group_by(scenario, GCAM_region_ID, region, input) %>%
  summarise(self.price.elasticity = mean(price_elasticity, na.rm = TRUE)) %>%
  ungroup()

# cross price elasticity for SSP2
PE_cross_GCAM <- PE_ICP_GCAM %>% filter(!is.na(input)) %>%
  filter(scenario == "SSP2") %>%
  filter(delta_quantity != delta_price) %>% # get cross price elasticity
  group_by(scenario, GCAM_region_ID, region, input) %>%
  summarise(cross.price.elasticity = mean(price_elasticity, na.rm = TRUE)) %>%
  ungroup()

# merge self and cross price elasticity
PE_GCAM_region <- full_join(PE_self_GCAM, PE_cross_GCAM,
                            by = c("scenario", "GCAM_region_ID", "region", "input")) %>%
  # replace Staples with FoodDemand_Staples and NonStaples with FoodDemand_NonStaples
  mutate(input = case_when(input == "Staples" ~ "FoodDemand_Staples",
                           input == "NonStaples" ~ "FoodDemand_NonStaples",
                           TRUE ~ input)) %>%
  arrange(input)

# write to csv
write.csv(PE_GCAM_region, file = paste0(DATA_DIR, "price_elasticity_GCAM_region.csv"), row.names = FALSE)


