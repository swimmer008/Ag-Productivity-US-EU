# ------------------------------------------- #
#                                             #
# This program estimates the Global Malmquist #
# TFP index for the EU country-level data     #
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
library(plm) #for DEA (The source codes rely on is.pbalanced)

# Load the source codes for the Lowe index
source('R_aux/aux_globalmalm.R')
source('R_aux/auxiliaries.R')
source('R_aux/globalmalm.R')

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')

# Load the EU country-level data and arrange by country name
load("R_output/EUdata.Rda")
EUdata <- EUdata[order(EUdata$country),]

# Create variable with the clusters
EUdata$cluster <- case_when(EUdata$country == "BEL" | EUdata$country == "FRA" | EUdata$country == "GBR" |
                              EUdata$country == "IRL" | EUdata$country == "NLD" ~ "Cluster1",
                            EUdata$country == "EST" | EUdata$country == "FIN" | EUdata$country == "LTU" |
                              EUdata$country == "LVA" | EUdata$country == "SWE" ~ "Cluster2",
                            EUdata$country == "AUT" | EUdata$country == "CZE" | EUdata$country == "DEU" |
                              EUdata$country == "DNK" | EUdata$country == "HUN" | EUdata$country == "LUX" |
                              EUdata$country == "POL" | EUdata$country == "ROU" | EUdata$country == "SVN"|
                              EUdata$country == "SVK" ~ "Cluster3",
                            EUdata$country == "ESP" | EUdata$country == "GRC" | EUdata$country == "ITA" | 
                              EUdata$country == "MLT" |EUdata$country == "PRT" ~ "Cluster4")

table(EUdata$cluster)
    

    
    # Calculate and decompose the index
    globmalm <- globalmalm(data = EUdata, id.var = "country", time.var = "year", 
                           x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                           y.vars = c("q_animals", "q_crops", "q_other"),
                           g.var = ("cluster"), 
                           orientation = c("out"), parallel = FALSE, scaled = FALSE,
                           window = 3)
    
    GlobMalm.levels <- globmalm$Levels
    GlobMalm.levels <- GlobMalm.levels[order(GlobMalm.levels$country),]
    
    
    # ---------------------------------- #
    #### Create table for the results ####
    # ---------------------------------- #  
    
    # Prepare data for the tables
    
    data_GlobMalm <- data.frame(matrix(ncol = 13, nrow = 25))
    colnames(data_GlobMalm) <- c("country", 
                                 "TFP2000", "TFP2019", "DTFP",
                                 "OTE2000", "OTE2019", "DOTE",
                                 "BPG2000", "BPG2019", "DBPG",
                                 "TGR2000", "TGR2019", "DTGR")
    
    data_GlobMalm$country <- rep(unique(EUdata$country))
    
    
    # TOTAL FACTOR PRODUCTIVITY 
    
    # TFP2000
    data_GlobMalm$TFP2000 <- GlobMalm.levels$DOGt[GlobMalm.levels$year==2000]
    # TFP2019
    data_GlobMalm$TFP2019 <- GlobMalm.levels$DOGt[GlobMalm.levels$year==2019]
    # DTFP
    data_GlobMalm$DTFP <- (data_GlobMalm$TFP2019 / data_GlobMalm$TFP2000)
    
    # OUTPUT TECHNICAL EFFICIENCY 
    
    # OTE2000
    data_GlobMalm$OTE2000 <- GlobMalm.levels$DOt[GlobMalm.levels$year==2000]
    # OTE2019
    data_GlobMalm$OTE2019 <- GlobMalm.levels$DOt[GlobMalm.levels$year==2019]
    # DOTE
    data_GlobMalm$DOTE <- (data_GlobMalm$OTE2019 / data_GlobMalm$OTE2000)
    
    # BEST PRACTICE GAP (BPG)
    
    # BPG2000
    data_GlobMalm$BPG2000 <- GlobMalm.levels$BPGt[GlobMalm.levels$year==2000]
    # BPG2019
    data_GlobMalm$BPG2019 <- GlobMalm.levels$BPGt[GlobMalm.levels$year==2019]
    # DBPG
    data_GlobMalm$DBPG <- (data_GlobMalm$BPG2019 / data_GlobMalm$BPG2000)
    
    # TECHNOLOGY GAP RATIO (TGR)
    
    # TGR2000
    data_GlobMalm$TGR2000 <- GlobMalm.levels$TGRt[GlobMalm.levels$year==2000]
    # TGR2019
    data_GlobMalm$TGR2019 <- GlobMalm.levels$TGRt[GlobMalm.levels$year==2019]
    # DTGR
    data_GlobMalm$DTGR <- (data_GlobMalm$TGR2019 / data_GlobMalm$TGR2000)
    
    # Add EU-average as geometric mean
    data_GlobMalm <- data_GlobMalm %>% 
      add_row(country="EU25",
              TFP2000=NA, TFP2019=NA, DTFP=exp(mean(log(data_GlobMalm$DTFP))),
              OTE2000=NA, OTE2019=NA, DOTE=exp(mean(log(data_GlobMalm$DOTE))),
              BPG2000=NA, BPG2019=NA, DBPG=exp(mean(log(data_GlobMalm$DBPG))),
              TGR2000=NA, TGR2019=NA, DTGR=exp(mean(log(data_GlobMalm$DTGR))))
    
    
    # Write Table: "TFP decomposition for EU agriculture (2000-2019) using the Global Malmquist index.
    
    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Create table  
    Tab_GlobMalm_EU_TFPDecomp <- kable(data_GlobMalm, booktabs = T, 
                                       digits = 2,
                                       row.names = FALSE,
                                       escape = FALSE,
                                       linesep = "",
                                       caption = "TFP decomposition for EU agriculture (2000--2019) using the Global Malmquist index.",
                                       label = "Tab_GlobMalm_EU_TFPDecomp",
                                       col.names = c('Country', 
                                                     '2000', '2019', "$\\Delta$",
                                                     '2000', '2019', "$\\Delta$",
                                                     '2000', '2019', "$\\Delta$",
                                                     '2000', '2019', "$\\Delta$")) %>%
      add_header_above(c("", "TFP" = 3, "OTE" = 3, "BPG" = 3, "TGR" = 3)) %>%
      row_spec(25, hline_after=T) %>% 
      footnote(general = "TFP is total factor productivity, OTE is output-oriented
                        technical efficiency, BPG is the best practice gap, and
                        TGR is a technology gap ratio capturing technical and environmental
                        differences across the four biogeographical regions.",
               footnote_as_chunk = T,
               threeparttable = T,
               general_title = "Notes:",
               escape=F) %>%
      kable_styling(latex_options = c("scale_down", "HOLD_position"))
    
    # Print Latex file
    writeLines(Tab_GlobMalm_EU_TFPDecomp, "Tables/Tab_GlobMalm_EU_TFPDecomp.tex")
    