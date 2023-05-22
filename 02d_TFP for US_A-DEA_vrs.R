# --------------------------------------- #
#                                         #
# This program estimates the additive TFP #
# index under VRS using shadow prices as  #
# weights for the US state-level data     #
#                                         #
# --------------------------------------- #

# Open packages
library(tidyverse) #for data manipulation
library(dplyr) #for data manipulation
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
source('R_aux/aux_lowe.R')
source('R_aux/auxiliaries.R')
source('R_aux/lowe.R')

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')
  
# Load the US state-level data
load("R_output/USdata.Rda")

# ------------------------------------ #
#### Estimation of reference prices #### 
# ------------------------------------ #

# Estimation of each observations' shadow prices
  
  # Input and output vectors
  XREF <- t(as.matrix(USdata[, c("x_capital", "x_land", "x_labor", "x_interm")]))
  YREF <- t(as.matrix(USdata[, c("q_livestock", "q_crops", "q_otheroutp")]))
  
  # Output distance function
  outShad <- foreach(dmu = 1:dim(USdata)[1], .combine = rbind) %do%
    {
      DO.shdu(XOBS = XREF[, dmu], YOBS = YREF[, dmu], XREF = XREF,
              YREF = YREF, rts = "vrs")
    }
  
  # Input distance function
  inpShad <- foreach(dmu = 1:dim(USdata)[1], .combine = rbind) %do%
    {
      DI.shdu(XOBS = XREF[, dmu], YOBS = YREF[, dmu], XREF = XREF,
              YREF = YREF, rts = "vrs")
    }
  
# Use mean shadow prices as reference prices 
  
  # Compute means
  meanY <- apply(outShad, 2, FUN = function(x) mean(x))
  meanX <- apply(inpShad, 2, FUN = function(x) mean(x))

  # Add reference prices to data frame
  USdata$p0_livestock <- meanY[1] 
  USdata$p0_crops <- meanY[2]
  USdata$p0_otheroutp <- meanY[3] 
  
  USdata$w0_capital <- meanX[1]
  USdata$w0_land <- meanX[2]
  USdata$w0_labor <- meanX[3]
  USdata$w0_interm <- meanX[4]

# -------------------------------------------- #
#### Calculate and decompose TFP under VRS  #### 
# -------------------------------------------- #
    
# Subsets for each region
  
  Pacific <- subset(USdata, state == "CA" | state == "OR" | state == "WA")
  Mountain <- subset(USdata,state == "AZ" | state == "CO" | state == "ID" |
                       state == "MT" | state == "NM" | state == "NV" |
                       state == "UT" | state == "WY")
  NPlains <- subset(USdata, state == "KS" | state == "ND" | state == "NE" |
                      state == "SD" )
  SPlains <- subset(USdata, state == "OK" | state == "TX")
  Cornbelt <- subset(USdata, state == "IA" | state == "IL" | state == "IN"| 
                       state == "MO" | state == "OH")
  Southeast <- subset(USdata, state == "AL" | state == "FL" | state == "GA" |
                        state == "SC")
  Northeast <- subset(USdata, state == "CT" | state == "DE" | state == "MA" |
                        state == "MD" | state == "ME" | state == "NH" |
                        state == "NJ" | state == "NY"| state == "PA" |
                        state == "RI" | state == "VT")
  Lake <- subset(USdata,state == "MI" | state == "MN" | state == "WI")
  Appalacian <- subset(USdata, state == "KY" | state == "NC" | state == "TN"| 
                         state == "VA" | state == "WV")
  Delta <- subset(USdata, state == "AR" | state == "LA" | state == "MS")
  
  
  # Note: We use the code from the Lowe index but use the above estimated
  #       shadow prices to obtain the additive TFP index with shadow prices
  #       as weights. 
  
  #Pacific
  ADEA_pacific <- lowe(data = Pacific, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(5), by.year = 1)
  
  #Mountain
  ADEA_mountain <- lowe(data = Mountain, id.var = "state", time.var = "year", 
                        x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                        y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                        w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                        p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                        tech.change = T, tech.reg = T, rts = "vrs", 
                        orientation = "out", cores = 8, scaled = F, 
                        window = c(2), by.year = 1)
  
  #NPlains
  ADEA_nplains <- lowe(data = NPlains, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(4), by.year = 1)
  
  #SPlains
  ADEA_splains <- lowe(data = SPlains, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(8), by.year = 1)
  
  #Cornbelt
  ADEA_cornbelt <- lowe(data = Cornbelt, id.var = "state", time.var = "year", 
                        x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                        y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                        w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                        p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                        tech.change = T, tech.reg = T, rts = "vrs", 
                        orientation = "out", cores = 8, scaled = F, 
                        window = c(3), by.year = 1)
  
  #Southeast
  ADEA_southeast <- lowe(data = Southeast, id.var = "state", time.var = "year", 
                         x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                         y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                         w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                         p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                         tech.change = T, tech.reg = T, rts = "vrs", 
                         orientation = "out", cores = 8, scaled = F, 
                         window = c(4), by.year = 1)
  
  #Northeast
  ADEA_northeast <- lowe(data = Northeast, id.var = "state", time.var = "year", 
                         x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                         y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                         w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                         p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                         tech.change = T, tech.reg = T, rts = "vrs", 
                         orientation = "out", cores = 8, scaled = F, 
                         window = c(2), by.year = 1)
  
  
  #Lake States
  ADEA_lake <- lowe(data = Lake, id.var = "state", time.var = "year", 
                    x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                    y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                    w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                    p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                    tech.change = T, tech.reg = T, rts = "vrs", 
                    orientation = "out", cores = 8, scaled = F, 
                    window = c(5), by.year = 1)
  
  #Appalacian
  ADEA_appalacian <- lowe(data = Appalacian, id.var = "state", time.var = "year", 
                          x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                          y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                          w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                          p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                          tech.change = T, tech.reg = T, rts = "vrs", 
                          orientation = "out", cores = 8, scaled = F, 
                          window = c(3), by.year = 1)
  
  #Delta States
  ADEA_delta <- lowe(data = Delta, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(5), by.year = 1)
  
  #All states (to obtain the true monetary outcomes)
  ADEA_all <- lowe(data = USdata, id.var = "state", time.var = "year", 
                 x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                 y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                 w.vars = c("w_capital", "w_land","w_labor","w_interm"), 
                 p.vars = c("p_livestock", "p_crops", "p_otheroutp"), 
                 tech.change = T, tech.reg = T, rts = "vrs", 
                 orientation = "out", cores = 8, scaled = F, 
                 by.year = 1) # no window here
  
  #Merge monetary with TFP results
  ADEA_pacific <- right_join(ADEA_all$Levels[ ,1:10], 
                             ADEA_pacific$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 
  
  ADEA_mountain <- right_join(ADEA_all$Levels[ ,1:10], 
                              ADEA_mountain$Levels[,c(1,2,11:20)], 
                              by=c("state","year")) 
  
  ADEA_nplains <- right_join(ADEA_all$Levels[ ,1:10], 
                             ADEA_nplains$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 
  
  ADEA_splains <- right_join(ADEA_all$Levels[ ,1:10], 
                             ADEA_splains$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 
  
  ADEA_cornbelt <- right_join(ADEA_all$Levels[ ,1:10], 
                              ADEA_cornbelt$Levels[,c(1,2,11:20)], 
                              by=c("state","year")) 
  
  ADEA_southeast <- right_join(ADEA_all$Levels[ ,1:10], 
                               ADEA_southeast$Levels[,c(1,2,11:20)], 
                               by=c("state","year")) 
  
  ADEA_northeast <- right_join(ADEA_all$Levels[ ,1:10], 
                               ADEA_northeast$Levels[,c(1,2,11:20)], 
                               by=c("state","year")) 
  
  ADEA_lake <- right_join(ADEA_all$Levels[ ,1:10], 
                          ADEA_lake$Levels[,c(1,2,11:20)], 
                          by=c("state","year")) 
  
  ADEA_appalacian <- right_join(ADEA_all$Levels[ ,1:10], 
                                ADEA_appalacian$Levels[,c(1,2,11:20)], 
                                by=c("state","year")) 
  
  ADEA_delta <- right_join(ADEA_all$Levels[ ,1:10], 
                           ADEA_delta$Levels[,c(1,2,11:20)], 
                           by=c("state","year")) 
  
  ADEA.level_vrs <- rbind(ADEA_pacific, ADEA_mountain, ADEA_nplains, 
                          ADEA_splains, ADEA_cornbelt, ADEA_southeast, 
                          ADEA_northeast, ADEA_lake, ADEA_appalacian, 
                          ADEA_delta)
  
  #replace TT
  ADEA.level_vrs$TT <- ADEA.level_vrs$PROF / ADEA.level_vrs$TFP
  
  # Order results by state
  ADEA.level_vrs <- ADEA.level_vrs[order(ADEA.level_vrs$state),]
  
  # Save results for comparison in "03h_TFP for US_comparison"
  save(ADEA.level_vrs, file="R_output/ADEA.level_vrs.Rda")
  
  # Normalize with Base = AL 1960
  ADEA.level_vrs_norm <- ADEA.level_vrs %>% 
    mutate_each(funs(./.[1]), setdiff(names(.), c("state","year")))
  

# ---------------------------------------------------- #
#### Replicate tables 2 and 3 from O'Donnell (2012) ####
# ---------------------------------------------------- #  
  
# Prepare data for the tables
  
  data_tables <- data.frame(matrix(ncol = 28, nrow = 48))
  colnames(data_tables) <- c("state", "PROF1960", "PROF2004", "DPROF",
                                  "TT1960", "TT2004", "DTT",
                                  "TFP1960", "TFP2004", "DTFP",
                                  "MP1960", "MP2004", "DMP",
                                  "TFPE1960", "TFPE2004", "DTFPE",
                                  "OTE1960", "OTE2004", "DOTE",
                                  "OSE1960", "OSE2004", "DOSE",
                                  "OME1960", "OME2004", "DOME",
                                  "OSME1960", "OSME2004", "DOSME")
  
  data_tables$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                                  "GA","IA","ID","IL","IN","KS","KY","LA",
                                  "MA","MD","ME","MI","MN","MO","MS","MT",
                                  "NC","ND","NE","NH","NJ","NM","NV","NY",
                                  "OH","OK","OR","PA","RI","SC","SD","TN",
                                  "TX","UT","VA","VT","WA","WI","WV","WY"))
  
# PROFITABILITY 
  
  # PROF1960
  data_tables$PROF1960 <- ADEA.level_vrs$PROF[ADEA.level_vrs$year==1960]
  # PROF04
  data_tables$PROF2004 <- ADEA.level_vrs$PROF[ADEA.level_vrs$year==2004]
  # DPROF
  data_tables$DPROF <- (data_tables$PROF2004 / data_tables$PROF1960)
  
# TERMS OF TRADE 
  
  # TT1960
  data_tables$TT1960 <- ADEA.level_vrs$TT[ADEA.level_vrs$year==1960]
  # TT04
  data_tables$TT2004 <- ADEA.level_vrs$TT[ADEA.level_vrs$year==2004]
  # DTT
  data_tables$DTT <- (data_tables$TT2004 / data_tables$TT1960)
  
# TOTAL FACTOR PRODUCTIVITY 
  
  # TFP1960
  data_tables$TFP1960 <- ADEA.level_vrs$TFP[ADEA.level_vrs$year==1960]
  # TFP04
  data_tables$TFP2004 <- ADEA.level_vrs$TFP[ADEA.level_vrs$year==2004]
  # DTFP
  data_tables$DTFP <- (data_tables$TFP2004 / data_tables$TFP1960)
  
# MAXIMM TOTAL FACTOR PRODUCTIVITY 
  
  # MP1960
  data_tables$MP1960 <- ADEA.level_vrs$MP[ADEA.level_vrs$year==1960]
  # MP04
  data_tables$MP2004 <- ADEA.level_vrs$MP[ADEA.level_vrs$year==2004]
  # DMP
  data_tables$DMP <- (data_tables$MP2004 / data_tables$MP1960)
  
# TFP EFFICIENCY 
  
  # TFPE1960
  data_tables$TFPE1960 <- ADEA.level_vrs$TFPE[ADEA.level_vrs$year==1960]
  # TFPE04
  data_tables$TFPE2004 <- ADEA.level_vrs$TFPE[ADEA.level_vrs$year==2004]
  # DTFPE
  data_tables$DTFPE <- (data_tables$TFPE2004 / data_tables$TFPE1960)
  
# OUTPUT TECHNICAL EFFICIENCY 
  
  # OTE1960
  data_tables$OTE1960 <- ADEA.level_vrs$OTE[ADEA.level_vrs$year==1960]
  # OTE04
  data_tables$OTE2004 <- ADEA.level_vrs$OTE[ADEA.level_vrs$year==2004]
  # DOTE
  data_tables$DOTE <- (data_tables$OTE2004 / data_tables$OTE1960)
  
# OUTPUT SCALE EFFICIENCY 
  
  # OSE1960
  data_tables$OSE1960 <- ADEA.level_vrs$OSE[ADEA.level_vrs$year==1960]
  # OSE04
  data_tables$OSE2004 <- ADEA.level_vrs$OSE[ADEA.level_vrs$year==2004]
  # DOSE
  data_tables$DOSE <- (data_tables$OSE2004 / data_tables$OSE1960)
  
# OUTPUT MIX EFFICIENCY 
  
  # OME1960
  data_tables$OME1960 <- ADEA.level_vrs$OME[ADEA.level_vrs$year==1960]
  # OME04
  data_tables$OME2004 <- ADEA.level_vrs$OME[ADEA.level_vrs$year==2004]
  # DOME
  data_tables$DOME <- (data_tables$OME2004 / data_tables$OME1960)
  
# OUTPUT SCALE MIX EFFICIENCY 
  
  # OSME1960
  data_tables$OSME1960 <- ADEA.level_vrs$OSME[ADEA.level_vrs$year==1960]
  # OSME04
  data_tables$OSME2004 <- ADEA.level_vrs$OSME[ADEA.level_vrs$year==2004]
  # DOSME
  data_tables$DOSME <- (data_tables$OSME2004 / data_tables$OSME1960)
  
# Add US-average as geometric mean
  data_tables <- data_tables %>% 
    add_row(state="US48",
            PROF1960=NA, PROF2004=NA, DPROF=exp(mean(log(data_tables$DPROF))),
            TT1960=NA, TT2004=NA, DTT=exp(mean(log(data_tables$DTT))),
            TFP1960=NA, TFP2004=NA, DTFP=exp(mean(log(data_tables$DTFP))),
            MP1960=NA, MP2004=NA, DMP=exp(mean(log(data_tables$DMP))),
            TFPE1960=NA, TFPE2004=NA, DTFPE=exp(mean(log(data_tables$DTFPE))),
            OTE1960=NA, OTE2004=NA, DOTE=exp(mean(log(data_tables$DOTE))),
            OSE1960=NA, OSE2004=NA, DOSE=exp(mean(log(data_tables$DOSE))),
            OME1960=NA, OME2004=NA, DOME=exp(mean(log(data_tables$DOME))),
            OSME1960=NA, OSME2004=NA, DOSME=exp(mean(log(data_tables$DOSME))))

# Save US-average for comparison in 03h_TFP for US_comparison.R
  
  Summary_adea_US_vrs <- list(data_tables$DTFP[data_tables$state=="US48"],
                              data_tables$DMP[data_tables$state=="US48"],
                              data_tables$DTFPE[data_tables$state=="US48"])
  names(Summary_adea_US_vrs) <- c("TFP", "TC", "TFPE") 
  save(Summary_adea_US_vrs, file = "R_output/Summary_adea_US_vrs.Rda")
  
# Write Table: "Profitability, TFP, and efficiency change in US agriculture (1960--2004) using the A-DEA index)
  
  # Set global option to produce latex output
  options(knitr.table.format = "latex", knitr.kable.NA = '')
  
  # Select data
  data_ADEA_ProfDecomp <- subset(data_tables, select = c(1:16))
  
  # Write table  
  Tab_ADEA_ProfDecomp_US_vrs <- kable(data_ADEA_ProfDecomp, booktabs = T, 
                                      digits = 2,
                                      row.names = FALSE,
                                      escape = FALSE,
                                      linesep = "",
                                      caption = "Profitability, TFP, and efficiency change in US agriculture (1960--2004) using the A-DEA index under VRS.",
                                      label = "Tab_ADEA_ProfDecomp_US_vrs",
                                      col.names = c('State', 
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$")) %>%
    add_header_above(c("", "PROF" = 3, "TT" = 3, 
                       "TFP" = 3, "TFP*" = 3,
                       "TFPE" = 3)) %>%
    row_spec(48, hline_after=T) %>% 
    kable_styling(font_size = 12,
                  latex_options = c("scale_down", "HOLD_position")) 
  
  # Print Latex file
  writeLines(Tab_ADEA_ProfDecomp_US_vrs, "Tables/Tab_ADEA_ProfDecomp_US_vrs.tex")
  
  
# Write Table: "Output-oriented components of effi ciency change in US agriculture (1960--2004) using the A-DEA index"
  
  # Set global option to produce latex output
  options(knitr.table.format = "latex", knitr.kable.NA = '')
  
  # Select data
  data_ADEA_TPFEDecomp <- subset(data_tables, select = c(1,14:28))
  
  # Write table  
  Tab_ADEA_TFPEDecomp_US_vrs <- kable(data_ADEA_TPFEDecomp, booktabs = T, 
                                      digits = 2,
                                      row.names = FALSE,
                                      escape = FALSE,
                                      linesep = "",
                                      caption = "Output-oriented components of efficiency change in US agriculture (1960--2004) using the A-DEA index under VRS.",
                                      label = "Tab_ADEA_TFPEDecomp_US_vrs",
                                      col.names = c('State', 
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$",
                                                    '1960', '2004', "$\\Delta$")) %>%
    add_header_above(c("", "TFPE" = 3, "OTE" = 3, 
                       "OSE" = 3, "OME" = 3,
                       "OSME" = 3)) %>%
    row_spec(48, hline_after=T) %>% 
    kable_styling(font_size = 12,
                  latex_options = c("scale_down", "HOLD_position"))
  
  # Print Latex file
  writeLines(Tab_ADEA_TFPEDecomp_US_vrs, "Tables/Tab_ADEA_TFPEDecomp_US_vrs.tex")
  
# --------------------------------------------- #
#### Replicate Table 4 from O'Donnell (2012) ####
# --------------------------------------------- #
  
  data_table <- data.frame(matrix(ncol = 17, nrow = 48))
  colnames(data_table) <- c("state", 
                            "TFP60-70", "MP60-70", "OTE60-70", "OSME60-70",
                            "TFP70-80", "MP70-80", "OTE70-80", "OSME70-80",
                            "TFP80-90", "MP80-90", "OTE80-90", "OSME80-90",
                            "TFP90-02", "MP90-02", "OTE90-02", "OSME90-02") 
  
  data_table$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                            "GA","IA","ID","IL","IN","KS","KY","LA",
                            "MA","MD","ME","MI","MN","MO","MS","MT",
                            "NC","ND","NE","NH","NJ","NM","NV","NY",
                            "OH","OK","OR","PA","RI","SC","SD","TN",
                            "TX","UT","VA","VT","WA","WI","WV","WY"))
  
  ADEA1960 <- subset(ADEA.level_vrs, year==1960)
  ADEA1970 <- subset(ADEA.level_vrs, year==1970)
  ADEA1980 <- subset(ADEA.level_vrs, year==1980)
  ADEA1990 <- subset(ADEA.level_vrs, year==1990)
  ADEA2002 <- subset(ADEA.level_vrs, year==2002)
  
  #1960-1970
  data_table$`TFP60-70`   <- log(c(ADEA1970[,"TFP"])/c(ADEA1960[,"TFP"])) / (1970-1960) * 100 
  data_table$`MP60-70`    <- log(c(ADEA1970[,"MP"])/c(ADEA1960[,"MP"])) / (1970-1960) * 100 
  data_table$`OTE60-70`   <- log(c(ADEA1970[,"OTE"])/c(ADEA1960[,"OTE"])) / (1970-1960) * 100 
  data_table$`OSME60-70`  <- log(c(ADEA1970[,"OSME"])/c(ADEA1960[,"OSME"])) / (1970-1960) * 100 
  
  #1970-1980
  data_table$`TFP70-80`   <- log(c(ADEA1980[,"TFP"])/c(ADEA1970[,"TFP"])) / (1980-1970) * 100 
  data_table$`MP70-80`    <- log(c(ADEA1980[,"MP"])/c(ADEA1970[,"MP"])) / (1980-1970) * 100 
  data_table$`OTE70-80`   <- log(c(ADEA1980[,"OTE"])/c(ADEA1970[,"OTE"])) / (1980-1970) * 100 
  data_table$`OSME70-80`  <- log(c(ADEA1980[,"OSME"])/c(ADEA1970[,"OSME"])) / (1980-1970) * 100 
  
  #1980-1990
  data_table$`TFP80-90`   <- log(c(ADEA1990[,"TFP"])/c(ADEA1980[,"TFP"])) / (1990-1980) * 100 
  data_table$`MP80-90`    <- log(c(ADEA1990[,"MP"])/c(ADEA1980[,"MP"])) / (1990-1980) * 100 
  data_table$`OTE80-90`   <- log(c(ADEA1990[,"OTE"])/c(ADEA1980[,"OTE"])) / (1990-1980) * 100 
  data_table$`OSME80-90`  <- log(c(ADEA1990[,"OSME"])/c(ADEA1980[,"OSME"])) / (1990-1980) * 100 
  
  #1990-2002
  data_table$`TFP90-02`   <- log(c(ADEA2002[,"TFP"])/c(ADEA1990[,"TFP"])) / (2002-1990) * 100 
  data_table$`MP90-02`    <- log(c(ADEA2002[,"MP"])/c(ADEA1990[,"MP"])) / (2002-1990) * 100 
  data_table$`OTE90-02`   <- log(c(ADEA2002[,"OTE"])/c(ADEA1990[,"OTE"])) / (2002-1990) * 100 
  data_table$`OSME90-02`  <- log(c(ADEA2002[,"OSME"])/c(ADEA1990[,"OSME"])) / (2002-1990) * 100 
  
  # Add US48-average as arithmetic average (because values are in logs)
  data_table <- data_table %>% 
    add_row(state = "US48",
            `TFP60-70` = mean(data_table$`TFP60-70`),
            `MP60-70`=mean(data_table$`MP60-70`),
            `OTE60-70`=mean(data_table$`OTE60-70`),
            `OSME60-70`=mean(data_table$`OSME60-70`),
            `TFP70-80` = mean(data_table$`TFP70-80`),
            `MP70-80`=mean(data_table$`MP70-80`),
            `OTE70-80`=mean(data_table$`OTE70-80`),
            `OSME70-80`=mean(data_table$`OSME70-80`),
            `TFP80-90` = mean(data_table$`TFP80-90`),
            `MP80-90`=mean(data_table$`MP80-90`),
            `OTE80-90`=mean(data_table$`OTE80-90`),
            `OSME80-90`=mean(data_table$`OSME80-90`),
            `TFP90-02` = mean(data_table$`TFP90-02`),
            `MP90-02`=mean(data_table$`MP90-02`),
            `OTE90-02`=mean(data_table$`OTE90-02`),
            `OSME90-02`=mean(data_table$`OSME90-02`))
  
  
  # Write Table: Average annual rates of growth in TFP and efficiency in US agriculture (1960--2002) using the A-DEA index
  
  # Create table  
  Tab_ADEA_AvgRates_US_vrs <- kable(data_table, booktabs = T, 
                                    digits = 2,
                                    row.names = FALSE,
                                    escape = FALSE,
                                    linesep = "",
                                    caption = "Average annual rates of growth (\\%) in TFP and efficiency in US agriculture (1960--2002) using the A-DEA index under VRS.",
                                    label = "Tab_ADEA_AvgRates_US_vrs",
                                    col.names = c('State', 
                                                  'TFP', 'TFP*', 'OTE', 'OSME',
                                                  'TFP', 'TFP*', 'OTE', 'OSME',
                                                  'TFP', 'TFP*', 'OTE', 'OSME',
                                                  'TFP', 'TFP*', 'OTE', 'OSME')) %>%
    add_header_above(c("", "1960-1970" = 4, "1970-1980" = 4, 
                       "1980-1990" = 4, "1990-2002" = 4)) %>%
    row_spec(48, hline_after=T) %>% 
    kable_styling(latex_options = c("scale_down", "HOLD_position"))
  
  # Print Latex file
  writeLines(Tab_ADEA_AvgRates_US_vrs, "Tables/Tab_ADEA_AvgRates_US_vrs.tex")
  


# ------------------------------------- #
#### Create figures for US aggregate ####
# ------------------------------------- #
  
  # Prepare the data
  
    # Compare every level to the state's *own* 1960 level --> obtains cumulative growth 
    idstates <- unique(ADEA.level_vrs$state)
    ADEA.cumulative_vrs <- list()
    for (i in idstates) {
      ADEA.cumulative_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                         sweep(ADEA.level_vrs[ADEA.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                               STATS = as.numeric(ADEA.level_vrs[ADEA.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    ADEA.cumulative_vrs <- do.call(rbind, ADEA.cumulative_vrs) #turns the list into a data frame
    
    # Calculate US average using the geometric mean
    ADEA.US48_vrs <- list()
    for (yr in 1960:2004) {
      ADEA.US48_vrs[[which(yr == 1960:2004)]] <- c(year = yr, apply(ADEA.cumulative_vrs[ADEA.cumulative_vrs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    ADEA.US48_vrs <- as.data.frame(do.call(rbind, ADEA.US48_vrs)) #turns the list into a data frame
    
    
    # For the efficiencies (last figure), we need arithmetic averages
    ADEA.US48_vrs_eff <- ADEA.level_vrs[,c("state", "year","TFPE", "OTE", "OME", "ROSE", "OSME")]
    ADEA.US48_vrs_eff <- ADEA.US48_vrs_eff %>% 
      group_by(year) %>% 
      summarise_at(.vars = vars(TFPE,OTE,OME,ROSE,OSME),
                   .funs = c(mean="mean"))
    
    
  #-------------------------------------------------------------#
  # Panel A: Profitability, quantity, and price change (1960=1) #
  #-------------------------------------------------------------#
  
    # Prepare data for the plot
    data_plot <- ADEA.US48_vrs %>% 
      select("year", "W", "P", "TFP", "PROF", "TT") %>% 
      reshape2::melt(id.vars="year")
    
    # plot
    panel_A <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = value, color = variable, linetype = variable)) + 
      labs(
        x = "",
        y = "",
        linetype = "") + 
      scale_color_manual(name = "",
                         breaks=c("W", "P", "TFP", "PROF", "TT"),
                         labels=c("?W", "?P", "?TFP", "?PROF", "?TT"),
                         values=c("#0a0a0a","#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) +
      scale_linetype_manual(name = "",
                            breaks=c("W", "P", "TFP", "PROF", "TT"),
                            labels=c("?W", "?P", "?TFP", "?PROF", "?TT"),
                            values=c("12", "42", "solid", "solid", "1141")) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 3)) +
      scale_y_continuous(breaks = seq(0, 7, by = 1), limits=c(0,7.5)) +  
      theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))
    
  # --------------------------------- #
  # Panel B: Components of TFP change #
  # --------------------------------- #
  
    # Prepare data for the plot
    data_plot <- ADEA.US48_vrs %>% 
      select("year", "TFP", "MP", "TFPE") %>% 
      reshape2::melt(id.vars="year")
    
    # plot
    panel_B <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = value, linetype = variable, color = variable)
      ) + labs(
        x = "",
        y = "",
        linetype = ""
      ) + scale_linetype_manual(name  ="",
                                breaks=c("TFP", "MP", "TFPE"),
                                labels=c("?TFP", "?TFP*", "?TFPE"),
                                values=c("solid", "42", "12")) +
      scale_color_manual(name  ="",
                         breaks=c("TFP", "MP", "TFPE"),
                         labels=c("?TFP", "?TFP*", "?TFPE"),
                         values=c("#0a0a0a","#0a0a0a","#0a0a0a")) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 3)) +
      scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits=c(0,2.8)) +  
      theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))
    
  # -------------------------- #
  # Panel C: Efficiency levels #
  # -------------------------- #
    
    # Prepare data for the plot
    data_plot <- ADEA.US48_vrs_eff %>% 
      reshape2::melt(id.vars="year")
    
    # plot
    panel_C <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = value, linetype = variable, color = variable)
      ) + labs(
        x = "",
        y = "",
        linetype = ""
      ) + scale_linetype_manual(name  ="",
                                breaks=c("OTE_mean", "OME_mean", "ROSE_mean", "OSME_mean", "TFPE_mean"),
                                labels=c("OTE", "OME", "ROSE", "OSME", "TFPE"),
                                values=c("solid", "42", "1141", "solid", "12")) +
      scale_color_manual(name  ="",
                         breaks=c("OTE_mean", "OME_mean", "ROSE_mean", "OSME_mean", "TFPE_mean"),
                         labels=c("OTE", "OME", "ROSE", "OSME", "TFPE"),
                         values=c("#0a0a0a","#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) +
      
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 3)) +
      scale_y_continuous(breaks = seq(0, 1.2, by = 0.2), limits=c(0,1.3)) +  
      theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))
  
  # -------------------------------------------------- #
  # Combine panels to one figure and save as tex file  #
  # -------------------------------------------------- #    
  
  # Options for special characters
  options(
    tikzSanitizeCharacters = c('%','$','}','{','^','_','#','&','~','?'),
    tikzReplacementCharacters = c('\\%','\\$','\\}','\\{','\\^{}','\\_{}',
                                  '\\#','\\&','\\char???\\~','$\\Delta$')
  )  
    
  # set filepath for latex output
  tikz(file = "Figures/Fig_ADEA_US48_vrs.tex", width = 7, height = 7, sanitize = TRUE)
  plot <- ggarrange(panel_A, panel_B, panel_C,
                    labels = c("a", "b", "c"),
                    ncol = 2, nrow = 2)
  print(plot)
  dev.off()
  