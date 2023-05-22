# --------------------------------------- #
#                                         #
# This program produces the descriptive   #
# statistics and converts it to the Latex #
# format                                  # 
#                                         #
# --------------------------------------- #

# Open packages
library(dplyr)
library(kableExtra) # to convert data frames to Latex

# -------------------------- #
#### 1) USDA and EUROSTAT ####
# -------------------------- #

load("R_output/EUdata.Rda")
load("R_output/USdata.Rda")


descriptives <- data.frame(matrix(ncol = 7, nrow = 14))
colnames(descriptives) <- c("Variable", "unit_US", "mean_US", "std_US",
       "unit_EU", "mean_EU", "std_EU") 

descriptives$Variable <- c("Crop output","Livestock output","Other outputs",
                           "Capital input","Land input","Labor input","Materials input",
                           "Crop price","Livestock price","Other price",
                           "Capital price","Land price","Labor price","Materials price")

descriptives$unit_US <- c("Value in M $1996","Value in M $1996","Value in M $1996",
                           "Services in M $1996","Services in M $1996","Services in M $1996","Value in M $1996",
                          "Fisher price index","Fisher price index","Fisher price index","Fisher price index","Fisher price index","Fisher price index","Fisher price index")

descriptives$unit_EU <- c("Value in M PPS2015","Value in M PPS2015","Value in M PPS2015",
                          "Value in M PPS2015","UAA in 1000 hectares","1000 AWU","Value in M PPS2015",
                          "","","","","","","")

descriptives$mean_US <- c(format(round(mean(USdata$q_crops)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$q_livestock)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$q_otheroutp)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$x_capital)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$x_land)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$x_labor)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$x_interm)/1000, 2), nsmall = 2),
                          format(round(mean(USdata$p_crops), 2), nsmall = 2),
                          format(round(mean(USdata$p_livestock), 2), nsmall = 2),
                          format(round(mean(USdata$p_otheroutp), 2), nsmall = 2),
                          format(round(mean(USdata$w_capital), 2), nsmall = 2),
                          format(round(mean(USdata$w_land), 2), nsmall = 2),
                          format(round(mean(USdata$w_labor), 2), nsmall = 2),
                          format(round(mean(USdata$w_interm), 2), nsmall = 2))

descriptives$std_US <- c(format(round(sd(USdata$q_crops)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$q_livestock)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$q_otheroutp)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$x_capital)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$x_land)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$x_labor)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$x_interm)/1000, 2), nsmall = 2),
                         format(round(sd(USdata$p_crops), 2), nsmall = 2),
                         format(round(sd(USdata$p_livestock), 2), nsmall = 2),
                         format(round(sd(USdata$p_otheroutp), 2), nsmall = 2),
                         format(round(sd(USdata$w_capital), 2), nsmall = 2),
                         format(round(sd(USdata$w_land), 2), nsmall = 2),
                         format(round(sd(USdata$w_labor), 2), nsmall = 2),
                         format(round(sd(USdata$w_interm), 2), nsmall = 2))

descriptives$mean_EU <- c(format(round(mean(EUdata$q_crops), 2), nsmall = 2),
                          format(round(mean(EUdata$q_animals),2),nsmall=2),
                          format(round(mean(EUdata$q_other),2),nsmall=2),
                          format(round(mean(EUdata$x_capital),2),nsmall=2),
                          format(round(mean(EUdata$x_land),2),nsmall=2),
                          format(round(mean(EUdata$x_labor),2),nsmall=2),
                          format(round(mean(EUdata$x_interm),2),nsmall=2),
                          "",
                          "",
                          "",
                          "",
                          "",
                          "",
                          "")

descriptives$std_EU <- c(format(round(sd(EUdata$q_crops), 2), nsmall = 2),
                         format(round(sd(EUdata$q_animals), 2), nsmall = 2),
                         format(round(sd(EUdata$q_other), 2), nsmall = 2),
                         format(round(sd(EUdata$x_capital), 2), nsmall = 2),
                         format(round(sd(EUdata$x_land), 2), nsmall = 2),
                         format(round(sd(EUdata$x_labor), 2), nsmall = 2),
                         format(round(sd(EUdata$x_interm), 2), nsmall = 2),
                         "",
                         "",
                         "",
                         "",
                         "",
                         "",
                         "")

# Set global option to produce latex output
options(knitr.table.format = "latex")

# Create table  
Tab_descriptives <- kable(descriptives, booktabs = T, 
                         digits = 2,
                         linesep = "",
                         caption = "Descriptive Statistics for US (1960--2004) and EU (2000--2019) agriculture.",
                         label = "Tab_descriptives",
                         col.names = c('', 
                                       'Unit', 'Mean', 'SD',
                                       'Unit', 'Mean', 'SD')) %>%
  add_header_above(c("", "State-level US data, 1960-2004" = 3, "Country-level EU data, 2000-2019" = 3)) %>%
  footnote(general = "State-level US data is provided by USDA, country-level EU data by Eurostat. \\\\\\\\ PPS2015 denotes purchasing power standards with base year 2015. AWU is annual working \\\\\\\\ 
           units. UAA is utilized agricultural area. Prices in the United States are geometric averaged \\\\\\\\ binary Fisher price indices.",
           footnote_as_chunk = T,
           escape = FALSE,
           threeparttable = T,
           general_title = "Note:") %>%
  kable_styling(latex_options = c("scale_down","HOLD_position"))


# Print Latex file
writeLines(Tab_descriptives, "Tables/Tab_descriptives.tex")


# ---------------------------------------- #
#### 2) USDA International Productivity ####
# ---------------------------------------- #

load("R_output/IAPdata.Rda")

descriptives_iap <- data.frame(matrix(ncol = 4, nrow = 7))
colnames(descriptives_iap) <- c("Variable", "unit", "mean", "std") 

descriptives_iap$Variable <- c("Crop output", "Animal output (excl. fish)", 
                                 "Land input", "Labor input", "Capital input", "Fertilizer input", "Feed input")

descriptives_iap$unit <- c("$1000 at constant 2015 global average farmgate price", 
                           "$1000 at constant 2015 global average farmgate price", 
                             "1000 hectares of rainfed-equivalent cropland",
                             "Number of adults primarily employed in agriculture, 1000 persons",
                             "Value of net capital stock, $1000 at constant 2015 prices",
                             "Metric tonnes of N, P2O5, and K2O",
                             "1000 Mcal of Metabolizable Energy")

descriptives_iap$mean <- c(format(round(mean(IAPdata$q_crops), 2), nsmall = 2),
                             format(round(mean(IAPdata$q_animals), 2), nsmall = 2),
                             format(round(mean(IAPdata$x_land), 2), nsmall = 2),
                             format(round(mean(IAPdata$x_labor), 2), nsmall = 2),
                             format(round(mean(IAPdata$x_capital), 2), nsmall = 2),
                             format(round(mean(IAPdata$x_fertilizer), 2), nsmall = 2),
                             format(round(mean(IAPdata$x_feed), 2), nsmall = 2))

descriptives_iap$std <- c(format(round(sd(IAPdata$q_crops), 2), nsmall = 2),
                            format(round(sd(IAPdata$q_animals), 2), nsmall = 2),
                            format(round(sd(IAPdata$x_land), 2), nsmall = 2),
                            format(round(sd(IAPdata$x_labor), 2), nsmall = 2),
                            format(round(sd(IAPdata$x_capital), 2), nsmall = 2),
                            format(round(sd(IAPdata$x_fertilizer), 2), nsmall = 2),
                            format(round(sd(IAPdata$x_feed), 2), nsmall = 2))


# Set global option to produce latex output
options(knitr.table.format = "latex")

# Create table  
Tab_descriptives_iap <- kable(descriptives_iap, booktabs = T, 
                           digits = 2,
                           linesep = "",
                           caption = "Descriptive Statistics for the International Agricultural Productivity data (1961-2020).",
                           label = "Tab_descriptives_iap",
                           col.names = c('', 
                                         'Unit', 'Mean', 'SD')) %>%
  footnote(general = "Countries and regions considered are Austria, Bulgaria, Belgium-Luxembourg,  \\\\\\\\ 
           Former Czechoslovakia, Cyprus, Germany, Denmark, Spain, Estonia, Finland, France,  \\\\\\\\ 
           United Kingdom, Greece, Hungary, Ireland, Italy, Lithuania, Latvia, Malta, Netherlands,  \\\\\\\\ 
           Poland, Portugal, Romania, Sweden, and the United States of America.",
           footnote_as_chunk = T,
           escape = FALSE,
           threeparttable = T,
           general_title = "Note:") %>%
  kable_styling(latex_options = c("scale_down","HOLD_position"))


# Print Latex file
writeLines(Tab_descriptives_iap, "Tables/Tab_descriptives_iap.tex")


# ---------------------------- #
#### 3) Regions and windows ####
# ---------------------------- #

regwindows <- data.frame(matrix(ncol = 6, nrow = 12))
colnames(regwindows) <- c("Region_US", "States_US", "Windows_US", "Region_EU", "Countries_EU", "Windows_EU") 


regwindows$Region_US <- c("Pacific","Mountain", "", "Northern Plains", 
                       "Southern Plains", "Corn Belt","Southeast",
                       "Northeast", "", "Lake States", "Appalacian",
                       "Delta States")

regwindows$States_US <- c("CA, OR, WA",
                       "AZ, CO, ID, MT, NM, NV,",
                       "  UT, WY",
                       "KS, ND, NE, SD",
                       "OK, TX",
                       "IA, IL, IN, MO, OH",
                       "AL, FL, GA, SC",
                       "CT, DE, MA, MD, ME, NH,",
                       "  NJ, NY, PA, RI, VT",
                       "MI, MN, WI",
                       "KY, NC,TN, VA, WV",
                       "AR, LA, MS")

regwindows$Windows_US <- c(5,2,"",4,8,3,4,2,"",5,3,5)
  
regwindows$Region_EU <- c("Atlantic", "", "Boreal", "", 
                           "Continental /", "  mountainous", "","", "Mediterranean", 
                           "","", "")

regwindows$Countries_EU <- c("BEL, FRA, GBR,", 
                              "  IRL, NLD",
                          "EST, FIN, LTU,", 
                          "  LVA, SWE",
                          "AUT, CZE, DEU,", 
                          "  DNK, HUN,LUX,",
                          "  POL, ROU, SVN",
                          "  SVK",
                          "ESP, GRE, ITA,", 
                          "  MLT, PRT", "","")

regwindows$Windows_EU <- c(3,"",3,"",3,"", "", "",3,"","", "")


# Set global option to produce latex output
options(knitr.table.format = "latex")

# Create table  
Tab_regwindows <- kable(regwindows, booktabs = T,
                           escape = F,
                           align = c("l","l","c","l","l","c"),
                           linesep = "",
                           caption = "Regions and windows used in the productivity analysis.",
                           label = "Tab_regwindows",
                           col.names = c('Region', 'States', 'Window', 
                                         'Region', 'Countries', 'Window')) %>%
  add_header_above(c("USDA-ERS state-level data" = 3, 
                     "Eurostat country-level data" = 3)) %>%
  footnote(general = "Classifications are based on \\\\cite{odonnell_nonparametric_2012} and \\\\cite{european_environment_agency_climate_2017}.",
           footnote_as_chunk = T,
           threeparttable = T,
           general_title = "Note:",
           escape=F) %>%
  kable_styling(latex_options = c("scale_down","HOLD_position"))


# Print Latex file
writeLines(Tab_regwindows, "Tables/Tab_regwindows.tex")
