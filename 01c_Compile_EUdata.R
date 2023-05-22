# --------------------------------------- #
#                                         #
# This program extracts and organizes the # 
# European country-level input and output #
# from the Eurostat data base             #
#                                         #
# --------------------------------------- #

# Open packages
library(eurostat) # downloads data from the Eurostat database
library(tidyverse) # for data management 
library(countrycode) # for converting country names

# ------------------------------------------#
#### Outputs and material&capital inputs ####
# ------------------------------------------#

# Download the data from Eurostat
ec_accounts <- eurostat:::get_eurostat_raw("aact_eaa03") %>% 
  separate(col=1, into = c("item", "indic", "unit", "geo"),
           sep = ',')

# Use ISO-3 character code for countries
ec_accounts$geo <- countrycode(sourcevar = ec_accounts$geo, 
                          origin = 'eurostat', 
                          destination = 'iso3c',
                          nomatch = NULL)

# Select required data
ec_accounts <- ec_accounts %>% 
  filter( ( indic == "PROD_PP" & (item == "10000" | item == "13000" | item == "15000" | item == "17000")  | # Crop output, animal output, agricultural services output, secondary activities (inseparable)
         indic == "PROD_BP" & (item == "19000" | item == "21000") ), # intermediate inputs, fixed capital consumption
         unit == "MIO_PPS", # million purchasing power standards
         geo == "AUT" | geo == "BEL" | geo == "BGR" | geo == "CYP" | geo == "CZE" | geo == "DEU" | geo == "DNK" | geo == "ESP" | geo == "EST" | geo == "FIN" | geo == "FRA" |
         geo == "GBR" | geo == "GRC" | geo == "HUN" | geo == "IRL" | geo == "ITA" | geo == "LTU" | geo == "LUX" | geo == "LVA" | geo == "MLT" |
         geo == "NLD" | geo == "POL" | geo == "PRT" | geo == "ROU" | geo == "SVK" | geo == "SVN" | geo == "SWE") 

# Note: For outputs we consider production value at producer price (i.e., taxes and subsidies are considered), to reflect the method of the USDA state-level data
#       For inputs, this is not available, so we use basic prices

# Drop indic and unit variables
ec_accounts <- ec_accounts %>% select(-indic, -unit)

# Change variable names
ec_accounts$item[ec_accounts$item == "10000"] <- "q_crops"
ec_accounts$item[ec_accounts$item == "13000"] <- "q_animals"
ec_accounts$item[ec_accounts$item == "15000"] <- "q_services"
ec_accounts$item[ec_accounts$item == "17000"] <- "q_secondary"
ec_accounts$item[ec_accounts$item == "19000"] <- "x_interm"
ec_accounts$item[ec_accounts$item == "21000"] <- "x_capital"


# ---------------- #
#### Land input ####
# ---------------- #

# Download the data from Eurostat
land <- eurostat:::get_eurostat_raw("apro_cpsh1") %>% 
  separate(col=1, into = c("crops", "prod", "geo"),
           sep = ',')

# Use ISO-3 character code for countries
land$geo <- countrycode(sourcevar = land$geo, 
                        origin = 'eurostat', 
                        destination = 'iso3c',
                        nomatch = NULL)

# Select required data
land <- land %>% 
  filter(crops == "UAA", # Utilised agricultural area
         prod == "MA", # Main area
         geo == "AUT" | geo == "BEL" | geo == "BGR" | geo == "CYP" | geo == "CZE" | geo == "DEU" | geo == "DNK" | geo == "ESP" | geo == "EST" | geo == "FIN" | geo == "FRA" |
         geo == "GBR" | geo == "GRC" | geo == "HUN" | geo == "IRL" | geo == "ITA" | geo == "LTU" | geo == "LUX" | geo == "LVA" | geo == "MLT" |
         geo == "NLD" | geo == "POL" | geo == "PRT" | geo == "ROU" | geo == "SVK" | geo == "SVN" | geo == "SWE") %>% 
  select(-crops, -prod) 

land$item <- "x_land"


# ----------------- #
#### Labor input ####
# ----------------- #  

# Download the data from Eurostat
labor <- eurostat:::get_eurostat_raw("aact_ali01") %>% 
  separate(col=1, into = c("item", "geo"),
           sep = ',')

# Use ISO-3 character code for countries
labor$geo <- countrycode(sourcevar = labor$geo, 
                         origin = 'eurostat', 
                         destination = 'iso3c',
                         nomatch = NULL)

# Select required data
labor <- labor %>% 
  filter(item == "40000", # Total labour force input
         geo == "AUT" | geo == "BEL" | geo == "BGR" | geo == "CYP" | geo == "CZE" | geo == "DEU" | geo == "DNK" | geo == "ESP" | geo == "EST" | geo == "FIN" | geo == "FRA" |
         geo == "GBR" | geo == "GRC" | geo == "HUN" | geo == "IRL" | geo == "ITA" | geo == "LTU" | geo == "LUX" | geo == "LVA" | geo == "MLT" |
         geo == "NLD" | geo == "POL" | geo == "PRT" | geo == "ROU" | geo == "SVK" | geo == "SVN" | geo == "SWE") %>% 
  select(-item)

labor$item <- "x_labor"

# -------------------------------- #
#### Merge and prepare all data ####
# -------------------------------- #  

EUdata <- bind_rows(ec_accounts,land,labor)

# Drop potential superscripts and convert all index variables to numeric
EUdata[, -c(1:2)] <- lapply(EUdata[, -c(1:2)], gsub,  pattern = "e|b|d|: z",
                                 replacement = "")
EUdata[, -c(1:2)] <- lapply(EUdata[, -c(1:2)], as.numeric)
summary(EUdata)

# Reshape the data

  # First, bring data to long format
  EUdata <- EUdata %>% 
    pivot_longer(
      cols = `2022`:`1973`, 
      names_to = "year",
      values_to = "value"
    )
  
  # Second, bring output and input variables to columns
  EUdata <- EUdata %>% 
    pivot_wider(
      names_from = item, 
      values_from = value)
  
# Make year numeric
EUdata$year <- as.numeric(EUdata$year)

# Create other outputs as sum of services and secondary output
  
  EUdata$q_other <- rowSums(EUdata[,c("q_services", "q_secondary")], na.rm=TRUE)
  EUdata$q_secondary <- NULL
  EUdata$q_services <- NULL
  
# Rename geo variable into country
  EUdata <- EUdata %>% dplyr::rename(country=geo)

  
# Keep years 2000 - 2020
  
  EUdata <- EUdata %>% filter(year>1999 & year < 2020)
  
# notes:
  # all outputs and inputs except land are until 2022; but 2022 is estimated only; GBR only until 2020
  # Land until 2021, only GBR until 2019 & FRA until 2020
  # --> I have ALL variables until 2021; GBR only until 2019; No land var in FRA2021 but can assume same value as 2020
  # --> Do until 2019 and keep GBR

  
# Replace missing values
  
  NAs <- EUdata %>% filter(is.na(q_crops) | is.na(q_animals) | is.na(q_other) |
                             is.na(x_interm) | is.na(x_capital) | 
                             is.na(x_land) | is.na(x_labor))
  
  # Drop CYP and BGR for missing data in 2000 - 2002 and 2005
  EUdata <- EUdata %>% filter(country != "BGR",
                              country != "CYP")
  
  # Replace missing land values (land is very stable)
  EUdata$x_land[EUdata$country=="ITA"&EUdata$year=="2009"] <- 
    0.5 * (EUdata$x_land[EUdata$country=="ITA"&EUdata$year=="2008"] +
             EUdata$x_land[EUdata$country=="ITA"&EUdata$year=="2010"])
  
  EUdata$x_land[EUdata$country=="FRA"&EUdata$year=="2006"] <- 
    0.5 * (EUdata$x_land[EUdata$country=="FRA"&EUdata$year=="2005"] +
             EUdata$x_land[EUdata$country=="FRA"&EUdata$year=="2007"])  
  
  EUdata$x_land[EUdata$country=="FRA"&EUdata$year=="2009"] <- 
    0.5 * (EUdata$x_land[EUdata$country=="FRA"&EUdata$year=="2008"] +
             EUdata$x_land[EUdata$country=="FRA"&EUdata$year=="2010"])
  
  summary(EUdata)
  
# -------------------------- #
#### save this data frame ####
# -------------------------- #

  # Save as .RDA-file 
  save(EUdata,file="R_output/EUdata.Rda")
  
  # Save as .CSV-file
  write.csv(EUdata, "R_output/EUdata.csv", row.names=TRUE)
  