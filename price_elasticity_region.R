
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tidyr')
install.packages('readr')

# First, install the devtools package if you haven't already
install.packages("devtools")
# Load the devtools package
library(devtools)

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

setwd("C:/Users/jilli/Documents/PostDoc/JGCRI/Manuscripts/Food Security Paper/Price Elasticity")

#call in the dataset
ICP_Price_Elasticities <- read.csv("Price_Elasticity_ICP.csv",check.names = FALSE)

#rename columns to prevent column names switching
names(ICP_Price_Elasticities) <- c("Country", "Year", "Group", "food", "Price_Elasticity")

#make the country code lowercase to match GCAM
ICP_Price_Elasticities$iso <- tolower(ICP_Price_Elasticities$Country)

#call in GCAM region dataset
iso_region_GCAM <- read.csv("iso_GCAM_regID.csv")

#merge the csv by iso 3-digit code
merged_df <- merge(ICP_Price_Elasticities, iso_region_GCAM, by = "iso")

names(merged_df) <- trimws(names(merged_df))          # Remove leading/trailing spaces
names(merged_df) <- gsub("\\.+$", "", names(merged_df))  # Remove trailing periods

#call in food mapping
ICP_to_GCAM_Food <- read.csv("Food_Categories.csv")

#merge ICP data with GCAM regions with food categorization
PE_ICP_GCAM <- left_join(merged_df, ICP_to_GCAM_Food, by = "food")

# Write the data frame to a CSV file in the working directory
write.csv(PE_ICP_GCAM, file = "output/Price_Elasticity_Region.csv", row.names = FALSE)
