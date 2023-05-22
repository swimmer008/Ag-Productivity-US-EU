# ------------------------------------------- #
#                                             #
# This program estimates the Global Malmquist #
# TFP index for the IAP country-level data    #
#                                             #
# ------------------------------------------- #

# Open packages
library(tidyverse) #for data manipulation
library(reshape2) #to reshape the data
library(tikzDevice) #to save ggplot object in latex format
library(ggpubr) #to combine plots
library(kableExtra) # to convert data frames to Latex

# More packages (productivity decomposition)
library(doFuture) #for DEA 
library(progressr) #for DEA
library(lpSolveAPI) #for DEA
library(plm) #for DEA (The source codes rely on "is.pbalanced)

# Load the required source codes
source('R_aux/aux_globalmalm.R')
source('R_aux/auxiliaries.R')
source('R_aux/globalmalm.R')

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')

# Load the IAP country-level data and arrange by country name
load("R_output/IAPdata.Rda") 
IAPdata <- IAPdata[order(IAPdata$country),]
    
# Calculate and decompose the index
globmalm <- globalmalm(data = IAPdata, id.var = "country", time.var = "year", 
                       x.vars = c("x_land", "x_labor","x_capital","x_fertilizer", "x_feed"), 
                       y.vars = c("q_crops", "q_animals"),
                       g.var = NULL, 
                       orientation = c("out"), parallel = FALSE, scaled = FALSE,
                       window = 3) 
    
GlobMalm.levels <- globmalm$Levels
GlobMalm.levels <- GlobMalm.levels[order(GlobMalm.levels$country),]
    
# save results
save(GlobMalm.levels, file = "R_output/GlobMalm.levels_IAP.Rda")
    
# ---------------------------------- #
#### Create table for the results ####
# ---------------------------------- #  
    
  # Prepare data for the tables
    
  data_GlobMalm <- data.frame(matrix(ncol = 10, nrow = 25))
  colnames(data_GlobMalm) <- c("country", 
                               "TFP1961", "TFP2020", "DTFP",
                               "OTE1961", "OTE2020", "DOTE",
                               "BPG1961", "BPG2020", "DBPG")
  
  data_GlobMalm$country <- rep(unique(IAPdata$country))
    
    
  # TOTAL FACTOR PRODUCTIVITY 
    
    # TFP1961
    data_GlobMalm$TFP1961 <- GlobMalm.levels$DOGt[GlobMalm.levels$year==1961]
    # TFP2020
    data_GlobMalm$TFP2020 <- GlobMalm.levels$DOGt[GlobMalm.levels$year==2020]
    # DTFP
    data_GlobMalm$DTFP <- (data_GlobMalm$TFP2020 / data_GlobMalm$TFP1961)
    
  # OUTPUT TECHNICAL EFFICIENCY 
    
    # OTE2000
    data_GlobMalm$OTE1961 <- GlobMalm.levels$DOt[GlobMalm.levels$year==1961]
    # OTE2020
    data_GlobMalm$OTE2020 <- GlobMalm.levels$DOt[GlobMalm.levels$year==2020]
    # DOTE
    data_GlobMalm$DOTE <- (data_GlobMalm$OTE2020 / data_GlobMalm$OTE1961)
    
  # BEST PRACTICE GAP (BPG)
    
    # BPG2000
    data_GlobMalm$BPG1961 <- GlobMalm.levels$BPGt[GlobMalm.levels$year==1961]
    # BPG2020
    data_GlobMalm$BPG2020 <- GlobMalm.levels$BPGt[GlobMalm.levels$year==2020]
    # DBPG
    data_GlobMalm$DBPG <- (data_GlobMalm$BPG2020 / data_GlobMalm$BPG1961)
    
  # Add EU-average as geometric mean
  
      data_GlobMalm <- data_GlobMalm %>% 
      add_row(country="EU25",
              TFP1961=NA, TFP2020=NA, DTFP=exp(mean(log(data_GlobMalm$DTFP))),
              OTE1961=NA, OTE2020=NA, DOTE=exp(mean(log(data_GlobMalm$DOTE))),
              BPG1961=NA, BPG2020=NA, DBPG=exp(mean(log(data_GlobMalm$DBPG))))
    
    
  # Write Table: "TFP decomposition for EU and US agriculture (1961-2020) using the Global Malmquist index.
    
    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Create table  
    Tab_GlobMalm_IAP_TFPDecomp <- kable(data_GlobMalm, booktabs = T, 
                                       digits = 2,
                                       row.names = FALSE,
                                       escape = FALSE,
                                       linesep = "",
                                       caption = "TFP decomposition for EU and US agriculture (1961--2020) using the Global Malmquist index and the International Agricultural Productivity data.",
                                       label = "Tab_GlobMalm_IAP_TFPDecomp",
                                       col.names = c('Country', 
                                                     '2000', '2019', "$\\Delta$",
                                                     '2000', '2019', "$\\Delta$",
                                                     '2000', '2019', "$\\Delta$")) %>%
      add_header_above(c("", "TFP" = 3, "OTE" = 3, "BPG" = 3)) %>%
      row_spec(48, hline_after=T) %>% 
      kable_styling(latex_options = c("scale_down", "HOLD_position"))
    
    # Print Latex file
    writeLines(Tab_GlobMalm_IAP_TFPDecomp, "Tables/Tab_GlobMalm_IAP_TFPDecomp.tex")
    