# ------------------------------------------- #
#                                             #
# This program estimates the Global Malmquist #
# TFP index for the US state-level data       #
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

# Load the US state-level data
load("R_output/USdata.Rda")

# Variable for regions
USdata$region <- case_when(USdata$state == "CA" | USdata$state == "OR" | USdata$state == "WA" ~ "Pacific",
                             USdata$state == "AZ" | USdata$state == "CO" | USdata$state == "ID" |
                             USdata$state == "MT" | USdata$state == "NM" | USdata$state == "NV" |
                             USdata$state == "UT" | USdata$state == "WY" ~ "Mountain",
                             USdata$state == "KS" | USdata$state == "ND" | USdata$state == "NE" |
                             USdata$state == "SD" ~ "NPlains",
                             USdata$state == "OK" | USdata$state == "TX" ~ "SPlains",
                             USdata$state == "IA" | USdata$state == "IL" | USdata$state == "IN"| 
                             USdata$state == "MO" |USdata$ state == "OH" ~ "Cornbelt",
                             USdata$state == "AL" | USdata$state == "FL" | USdata$state == "GA" |
                             USdata$state == "SC" ~ "Southeast",
                             USdata$state == "CT" | USdata$state == "DE" | USdata$state == "MA" |
                             USdata$state == "MD" | USdata$state == "ME" | USdata$state == "NH" |
                             USdata$state == "NJ" | USdata$state == "NY"| USdata$state == "PA" |
                             USdata$state == "RI" | USdata$state == "VT" ~ "Northeast",
                             USdata$state == "MI" | USdata$state == "MN" | USdata$state == "WI" ~ "Lake",
                             USdata$state == "KY" | USdata$state == "NC" | USdata$state == "TN"| 
                             USdata$state == "VA" | USdata$state == "WV" ~ "Appalacian",
                             USdata$state == "AR" | USdata$state == "LA" | USdata$state == "MS" ~ "Delta")
                            
  table(USdata$region)
  
  globmalm <- globalmalm(data = USdata, id.var = "state", time.var = "year", 
                         x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                         y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                         g.var = ("region"), 
                         orientation = c("out"), parallel = FALSE, scaled = FALSE,
                         window = c(3,3,5,5,2,2,4,5,4,8)) # Note: Windows must be in alphabetical order for the regions
  
  GlobMalm.levels <- globmalm$Levels
  GlobMalm.levels <- GlobMalm.levels[order(GlobMalm.levels$state),]
  
  # Save results for comparison in "03h_TFP for US_comparison"
  save(GlobMalm.levels, file="R_output/GlobMalm.levels.Rda")
  
 # ---------------------------------- #
 #### Create table for the results ####
 # ---------------------------------- #  
 
 # Prepare data for the tables
 
 data_GlobMalm <- data.frame(matrix(ncol = 13, nrow = 48))
 colnames(data_GlobMalm) <- c("state", 
                              "TFP1960", "TFP2004", "DTFP",
                              "OTE1960", "OTE2004", "DOTE",
                              "BPG1960", "BPG2004", "DBPG",
                              "TGR1960", "TGR2004", "DTGR")
 
 data_GlobMalm$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                                 "GA","IA","ID","IL","IN","KS","KY","LA",
                                 "MA","MD","ME","MI","MN","MO","MS","MT",
                                 "NC","ND","NE","NH","NJ","NM","NV","NY",
                                 "OH","OK","OR","PA","RI","SC","SD","TN",
                                 "TX","UT","VA","VT","WA","WI","WV","WY"))
 

# TOTAL FACTOR PRODUCTIVITY 
 
 # TFP1960
 data_GlobMalm$TFP1960 <- GlobMalm.levels$DOGt[GlobMalm.levels$year==1960]
 # TFP2004
 data_GlobMalm$TFP2004 <- GlobMalm.levels$DOGt[GlobMalm.levels$year==2004]
 # DTFP
 data_GlobMalm$DTFP <- (data_GlobMalm$TFP2004 / data_GlobMalm$TFP1960)
 
 # OUTPUT TECHNICAL EFFICIENCY 
 
 # OTE1960
 data_GlobMalm$OTE1960 <- GlobMalm.levels$DOt[GlobMalm.levels$year==1960]
 # OTE2004
 data_GlobMalm$OTE2004 <- GlobMalm.levels$DOt[GlobMalm.levels$year==2004]
 # DOTE
 data_GlobMalm$DOTE <- (data_GlobMalm$OTE2004 / data_GlobMalm$OTE1960)
 
 # BEST PRACTICE GAP (BPG)
 
 # BPG1960
 data_GlobMalm$BPG1960 <- GlobMalm.levels$BPGt[GlobMalm.levels$year==1960]
 # BPG2004
 data_GlobMalm$BPG2004 <- GlobMalm.levels$BPGt[GlobMalm.levels$year==2004]
 # DBPG
 data_GlobMalm$DBPG <- (data_GlobMalm$BPG2004 / data_GlobMalm$BPG1960)

# TECHNOLOGY GAP RATIO (TGR)
 
 # TGR1960
 data_GlobMalm$TGR1960 <- GlobMalm.levels$TGRt[GlobMalm.levels$year==1960]
 # TGR2004
 data_GlobMalm$TGR2004 <- GlobMalm.levels$TGRt[GlobMalm.levels$year==2004]
 # DTGR
 data_GlobMalm$DTGR <- (data_GlobMalm$TGR2004 / data_GlobMalm$TGR1960)
 
 # Add US-average as geometric mean
 data_GlobMalm <- data_GlobMalm %>% 
   add_row(state="US48",
           TFP1960=NA, TFP2004=NA, DTFP=exp(mean(log(data_GlobMalm$DTFP))),
           OTE1960=NA, OTE2004=NA, DOTE=exp(mean(log(data_GlobMalm$DOTE))),
           BPG1960=NA, BPG2004=NA, DBPG=exp(mean(log(data_GlobMalm$DBPG))),
           TGR1960=NA, TGR2004=NA, DTGR=exp(mean(log(data_GlobMalm$DTGR))))
 
 # Save US-average for comparison in 03h_TFP for US_comparison.R
 Summary_globmalm_US <- list(data_GlobMalm$DTFP[data_GlobMalm$state=="US48"],
                             data_GlobMalm$DBPG[data_GlobMalm$state=="US48"] * data_GlobMalm$DTGR[data_GlobMalm$state=="US48"],
                             data_GlobMalm$DOTE[data_GlobMalm$state=="US48"])
 names(Summary_globmalm_US) <- c("TFP", "TC", "TFPE") 
 save(Summary_globmalm_US, file = "R_output/Summary_globmalm_US.Rda")
 
# Write Table: "TFP decomposition for US agriculture (1960-2004) using the Global Malmquist index.
 
 # Set global option to produce latex output
 options(knitr.table.format = "latex", knitr.kable.NA = '')
 
 # Create table  
 Tab_GlobMalm_US_TFPDecomp <- kable(data_GlobMalm, booktabs = T, 
                                     digits = 2,
                                     row.names = FALSE,
                                     escape = FALSE,
                                     linesep = "",
                                     caption = "TFP decomposition for US agriculture (1960--2004) using the Global Malmquist index.",
                                     label = "Tab_GlobMalm_US_TFPDecomp",
                                     col.names = c('State', 
                                                   '1960', '2004', "$\\Delta$",
                                                   '1960', '2004', "$\\Delta$",
                                                   '1960', '2004', "$\\Delta$",
                                                   '1960', '2004', "$\\Delta$")) %>%
   add_header_above(c("", "TFP" = 3, "OTE" = 3, "BPG" = 3, "TGR" = 3)) %>%
   row_spec(48, hline_after=T) %>% 
   kable_styling(font_size = 7,latex_options = c("scale_down", "HOLD_position"))
 
 
 
 # Print Latex file
 writeLines(Tab_GlobMalm_US_TFPDecomp, "Tables/Tab_GlobMalm_US_TFPDecomp.tex")
 