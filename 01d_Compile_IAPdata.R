# --------------------------------------- #
#                                         #
# This program extracts and organizes the # 
# global input and output data from the   #
# raw excel file provided by Keith Fuglie #
# and USDA                                #
#                                         #
# --------------------------------------- #

# Open package
library(readxl) # to read data from excel files
library(tidyverse)

# ------------------------------------ #
#### Download and save the raw data ####
# ------------------------------------ #

# Download the data from ERS-USDA ("Machine-readable and long format file of TFP indices and components for countries, regions, countries grouped by income level, and the world, 1961-2020")

  #url <- "https://www.ers.usda.gov/webdocs/DataFiles/51270/AgTFPInternational2020_long.xlsx?v=2423.6"
  #destfile <- "Data/AgTFPInternational2020_long.xlsx"
  #download.file(url, destfile)


# ----------------------- #
#### Read-in the data #####
# ----------------------- #

IAPdata <- read_excel("Data/AgTFPInternational2020_long.xlsx", sheet="Long format data") %>% 
  dplyr::rename(country=ISO3)

#Subset for EU countries and US
IAPdata <- IAPdata %>% 
  filter(country == "AUT" | country == "BLX" | country == "BGR" | country == "CSK" | country == "CYP" | country == "DEU" | country == "DNK" | country == "ESP" | 
         country == "EST" | country == "FIN" | country == "FRA" | country == "GBR" | country == "GRC" | country == "HUN" | country == "IRL" | country == "ITA" | 
         country == "LTU" | country == "LVA" | country == "MLT" | country == "NLD" | country == "POL" | country == "PRT" | country == "ROU" | country == "SWE" |
         country == "USA")
table(IAPdata$country)  

# Note I: I consider BLX instead of BEL and LUX  
#         and CSK instead of CZE and SVK

# Note II: I could add former Yugoslavia ("YSR"), because Slovenia and Croatia are now
#          in the EU. But Bosnia and Herzegovina, Serbia, Montenegro, Kosovo, and North Macedonia are not.


# Select variables required

  # Note: "Total agricultural input is an aggregation of the quantity of labor, land, capital, and intermediate inputs employed in agricultural production"
  # (https://www.ers.usda.gov/data-products/international-agricultural-productivity/documentation-and-methods/#inputs)

IAPdata <- IAPdata %>% 
  select(country,Year,Attribute,Value) %>% 
  filter(Attribute == "Outall_Q" |
         Attribute == "Outcrop_Q" | 
         Attribute == "Outanim_Q" | 
         Attribute == "Outfish_Q" | 
         Attribute == "Land_Q" | 
         Attribute == "Labor_Q" | 
         Attribute == "Capital_Q" | 
         Attribute == "Fertilizer_Q" |
         Attribute == "Feed_Q")

# Bring output and input variables to columns
IAPdata <- IAPdata %>% 
  pivot_wider(
    names_from = Attribute, 
    values_from = Value)

# Rename variables

IAPdata <- IAPdata %>% 
  dplyr::rename(year = Year,
                q_output = Outall_Q,
                q_crops = Outcrop_Q,
                q_animals = Outanim_Q,
                q_fish = Outfish_Q,
                x_land  = Land_Q,
                x_labor  = Labor_Q,
                x_capital  = Capital_Q,
                x_fertilizer = Fertilizer_Q, 
                x_feed = Feed_Q)

# -------------------------- #
#### save this data frame ####
# -------------------------- #

  # Save as .RDA-file 
  save(IAPdata,file="R_output/IAPdata.Rda")
  
  # Save as .CSV-file
  write.csv(IAPdata, "R_output/IAPdata.csv", row.names=TRUE)
