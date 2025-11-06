# For the food systems and ammonia analysis
#
# Load libraries and query GCAM output
# Hassan Niazi, Oct 2025

# Change
#  INSTALL_PACKAGES to TRUE to install required packages
#  QUERY_GCAM to TRUE to re-query GCAM output
#  PATH_TO_GCAM to change the path to GCAM output
#  FIGS_DIR to change figures sub-directory e.g., figures/v1, v2, etc.

# install packages ----
INSTALL_PACKAGES <- FALSE
if(INSTALL_PACKAGES){
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('tidyr')
  install.packages('readr')
  install.packages('grid')
  install.packages('maps')
  install.packages("patchwork")
  install.packages("purrr")
  install.packages("ggtext")
  install.packages("egg")


  install.packages("devtools")

  # install JGCRI packages from GitHub
  devtools::install_github("JGCRI/rgcam", build_vignettes = TRUE)
  devtools::install_github("JGCRI/rmap", build_vignettes = TRUE)
}

# load libraries ----
library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(grid)
library(maps)
library(patchwork)
library(rmap)
library(purrr)
library(ggtext)
library(egg)

# paths ----
DATA_DIR <- "data/"
FIGS_DIR <- "figures/v1/" # change the subfolder here if desired

# create figs dir if it doesn't exist
if (!dir.exists(FIGS_DIR)) {
  dir.create(FIGS_DIR)
  paste0("Created directory: ", FIGS_DIR)
  }

# constants ----
CONV_USD_1975_2020 <- 3.8
CONV_KG_T <- 1000
H2_GJ_kg <- 0.1202
DAYS_PER_YEAR <- 365.25
CONV_PCAL_MCAL <- 1e9
CONV_NH3_N <- 14/17    # convert mass of NH3 to mass of N

HIST_YEARS <- c(1975, 1990, 2005, 2010, 2015, 2020, 2025)

# query GCAM ----
QUERY_GCAM <- FALSE # set to TRUE to re-query GCAM output
OUTPUTFILE <- "food_ammonia.proj"

if (QUERY_GCAM) {
  # query variables
  PATH_TO_GCAM <- "model/gcam-core/output/"

  SCENARIOS <- c("elec_NH3_hicost", "elec_NH3_hicost_NH3ship",
                 "elec_NH3_locost", "elec_NH3_locost_NH3ship",
                 "NGCCS_NH3", "NGCCS_NH3_NH3ship")
  QUERYFILE <- "queries_ammonia.xml"

  # query
  conn <- localDBConn(PATH_TO_GCAM, "database_basexdb")
  food_ammonia_proj <- addScenario(conn, OUTPUTFILE, SCENARIOS, QUERYFILE, clobber = TRUE)
}

# load project ----
food_ammonia_proj <- loadProject(OUTPUTFILE)
# food_ammonia_proj <- loadProject("food_ammonia.proj")

print(paste0("Scenarios:", listScenarios(food_ammonia_proj)))
print(paste0("Queries: ", listQueries(food_ammonia_proj)))


# query results ----
# q_ammonia_prod_tech <- getQuery(food_ammonia_proj, "ammonia production by tech")
# q_hydrogen_prod_tech <- getQuery(food_ammonia_proj, "hydrogen production by tech")
# q_nfertilizer_h2_prices <- getQuery(food_ammonia_proj, "N fertilizer and hydrogen prices")
# q_ag_commodity_prices <- getQuery(food_ammonia_proj, "ag commodity prices")
# q_ghg_emiss_ship <- getQuery(food_ammonia_proj, "GHG emissions by international shipping")
# q_food_demand <- getQuery(food_ammonia_proj, "food demand")
# q_population_region <- getQuery(food_ammonia_proj, "population by region")
# q_energy_inputs_ship <- getQuery(food_ammonia_proj, "energy inputs to maritime shipping")
