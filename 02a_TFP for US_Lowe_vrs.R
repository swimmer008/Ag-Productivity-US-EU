# --------------------------------------------- #
#                                               #
# This program estimates the LOWE TFP           #
# index under VRS using the state-level US data #
#                                               #
# --------------------------------------------- #

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

# Calculate reference prices for the entire sample
USdata$p0_livestock <- mean(USdata$p_livestock)
USdata$p0_crops <- mean(USdata$p_crops)
USdata$p0_otheroutp <- mean(USdata$p_otheroutp)

USdata$w0_capital <- mean(USdata$w_capital)
USdata$w0_land <- mean(USdata$w_land)
USdata$w0_labor <- mean(USdata$w_labor)
USdata$w0_interm <- mean(USdata$w_interm)

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


# ------------------------------ #
#### Decompose LOWE under VRS ####
# ------------------------------ #

#Pacific
lowe_pacific <- lowe(data = Pacific, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(5), by.year = 1)

#Mountain
lowe_mountain <- lowe(data = Mountain, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(2), by.year = 1)

#NPlains
lowe_nplains <- lowe(data = NPlains, id.var = "state", time.var = "year", 
                      x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                      y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                      w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                      p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                      tech.change = T, tech.reg = T, rts = "vrs", 
                      orientation = "out", cores = 8, scaled = F, 
                      window = c(4), by.year = 1)

#SPlains
lowe_splains <- lowe(data = SPlains, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(8), by.year = 1)

#Cornbelt
lowe_cornbelt <- lowe(data = Cornbelt, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(3), by.year = 1)

#Southeast
lowe_southeast <- lowe(data = Southeast, id.var = "state", time.var = "year", 
                      x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                      y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                      w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                      p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                      tech.change = T, tech.reg = T, rts = "vrs", 
                      orientation = "out", cores = 8, scaled = F, 
                      window = c(4), by.year = 1)

#Northeast
lowe_northeast <- lowe(data = Northeast, id.var = "state", time.var = "year", 
                  x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                  y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                  w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                  p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                  tech.change = T, tech.reg = T, rts = "vrs", 
                  orientation = "out", cores = 8, scaled = F, 
                  window = c(2), by.year = 1)

#Lake States
lowe_lake <- lowe(data = Lake, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(5), by.year = 1)

#Appalacian
lowe_appalacian <- lowe(data = Appalacian, id.var = "state", time.var = "year", 
                  x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                  y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                  w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                  p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                  tech.change = T, tech.reg = T, rts = "vrs", 
                  orientation = "out", cores = 8, scaled = F, 
                  window = c(3), by.year = 1)

#Delta States
lowe_delta <- lowe(data = Delta, id.var = "state", time.var = "year", 
                        x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                        y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                        w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                        p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                        tech.change = T, tech.reg = T, rts = "vrs", 
                        orientation = "out", cores = 8, scaled = F, 
                        window = c(5), by.year = 1)

#All states (to obtain the true monetary outcomes)
lowe_all <- lowe(data = USdata, id.var = "state", time.var = "year", 
                   x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                   y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                   w.vars = c("w_capital", "w_land","w_labor","w_interm"), 
                   p.vars = c("p_livestock", "p_crops", "p_otheroutp"), 
                   tech.change = T, tech.reg = T, rts = "vrs", 
                   orientation = "out", cores = 8, scaled = F, 
                   by.year = 1) # no window here

#Merge monetary with TFP results
lowe_pacific <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_pacific$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_mountain <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_mountain$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_nplains <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_nplains$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_splains <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_splains$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_cornbelt <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_cornbelt$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_southeast <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_southeast$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_northeast <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_northeast$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_lake <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_lake$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_appalacian <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_appalacian$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 

lowe_delta <- right_join(lowe_all$Levels[ ,1:10], 
                             lowe_delta$Levels[,c(1,2,11:20)], 
                             by=c("state","year")) 


Lowe.level_vrs <- rbind(lowe_pacific, lowe_mountain, lowe_nplains, 
                        lowe_splains, lowe_cornbelt, lowe_southeast, 
                        lowe_northeast, lowe_lake, lowe_appalacian, 
                        lowe_delta)

# Order results by state
Lowe.level_vrs <- Lowe.level_vrs[order(Lowe.level_vrs$state),]

# Save results for comparison in "03h_TFP for US_comparison"
save(Lowe.level_vrs, file="R_output/Lowe.level_vrs.Rda")

# Normalize with Base = AL 1960
Lowe.level_vrs_norm <- Lowe.level_vrs %>% 
  mutate_each(funs(./.[1]), setdiff(names(.), c("state","year")))


# --------------------------------------------- #
#### Replicate figures from O'Donnell (2012) ####
# --------------------------------------------- #

  # ----------------------------------------------------------------------- #
  # Panel A:  Profitability, quantity, and price change in Alabama (1960=1) #
  # ----------------------------------------------------------------------- #

    # Prepare data for the plot
    data_plot <- Lowe.level_vrs_norm %>% 
                filter(state == "AL") %>% 
                select("year", "W", "P", "TFP", "PROF", "TT") %>% 
                reshape2::melt(id.vars="year")

    # plot
    panel_A <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = value, color = variable, linetype = variable)
      ) + labs(
        x = "",
        y = "",
        linetype = "") + 
      scale_color_manual(name = "",
                         breaks=c("W", "P", "TFP", "PROF", "TT"),
                         labels=c("?W", "?P", "?TFP", "?PROF", "?TT"),
                         values=c("#0a0a0a","#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) +
      scale_linetype_manual(name  ="",
                                breaks=c("W", "P", "TFP", "PROF", "TT"),
                                labels=c("?W", "?P", "?TFP", "?PROF", "?TT"),
                                values=c("12", "42", "solid", "solid", "1141")) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
      scale_y_continuous(breaks = seq(0, 7, by = 1), limits=c(0,7.5)) +  
      theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))

  # ------------------------------------------------------------------------ #
  # Panel B:  TFP change in Florida, Alabama, and Wyoming (Alabama 1960 = 1) #
  # ------------------------------------------------------------------------ #
  
    # Prepare data for the plot
    data_plot <- Lowe.level_vrs_norm %>% 
      filter(state=="AL" | state=="FL"| state=="WY") %>% 
      select("year", "state", "TFP")
    
    # plot
    panel_B <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = TFP, linetype = state)
      ) + labs(
        x = "",
        y = "",
        linetype = ""
      ) + scale_linetype_manual(name  ="",
                                breaks=c("AL", "FL", "WY"),
                                labels=c("Alabama", "Florida", "Wyoming"),
                                values=c("solid", "42", "12")) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
      scale_y_continuous(breaks = seq(0, 3.5, by = 0.5), limits=c(0,3.8)) +  
      theme(axis.text.x = element_text(angle=90)) + 
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))
    
  # ------------------------------------------------------ #
  # Panel C:  Components of TFP change in Alabama (1960=1) #
  # ------------------------------------------------------ #
    
    # Prepare data for the plot
    data_plot <- Lowe.level_vrs_norm %>% 
      filter(state=="AL") %>% 
      select("year", "TFP", "MP", "TFPE") %>% 
      reshape2::melt(id.vars="year")
    
    # plot
    panel_C <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = value, linetype = variable)
      ) + labs(
        x = "",
        y = "",
        linetype = "") + 
      scale_linetype_manual(name  ="",
                              breaks=c("TFP", "MP", "TFPE"),
                              labels=c("?TFP", "?TFP*", "?TFPE"),
                              values=c("solid", "42", "12")) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
      scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits=c(0,2.8)) +  
      theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))


  # -------------------------------------- #
  # Panel D:  Efficiency levels in Alabama #
  # -------------------------------------- #
    
    # Prepare data for the plot
    data_plot <- Lowe.level_vrs %>% 
      filter(state=="AL") %>% 
      select("year", "OTE", "OME", "ROSE", "OSME", "TFPE") %>% 
      reshape2::melt(id.vars="year")

    # plot
    panel_D <- ggplot() + 
      geom_line(data = data_plot, aes(x = year, y = value, linetype = variable, color = variable)
      ) + labs(
        x = "",
        y = "",
        linetype = "") + 
      scale_linetype_manual(name  ="",
                              breaks=c("OTE", "OME", "ROSE", "OSME", "TFPE"),
                              labels=c("OTE", "OME", "ROSE", "OSME", "TFPE"),
                              values=c("solid", "42", "1141", "solid", "12")) +
      scale_color_manual(name  ="",
                         breaks=c("OTE", "OME", "ROSE", "OSME", "TFPE"),
                         labels=c("OTE", "OME", "ROSE", "OSME", "TFPE"),
                         values=c("#0a0a0a","#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
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
    tikz(file = "Figures/Fig_replication_vrs.tex", width = 7, height = 7, sanitize = TRUE)
    plot <- ggarrange(panel_A, panel_B, panel_C, panel_D,
                      labels = c("a", "b", "c", "d"),
                      ncol = 2, nrow = 2)
    print(plot)
    dev.off()

  
# ---------------------------------------------------- #
#### Replicate tables 2 and 3 from O'Donnell (2012) ####
# ---------------------------------------------------- #  

  # Prepare data for the tables
    
    data_replication <- data.frame(matrix(ncol = 28, nrow = 48))
    colnames(data_replication) <- c("state", "PROF1960", "PROF2004", "DPROF",
                     "TT1960", "TT2004", "DTT",
                     "TFP1960", "TFP2004", "DTFP",
                     "MP1960", "MP2004", "DMP",
                     "TFPE1960", "TFPE2004", "DTFPE",
                     "OTE1960", "OTE2004", "DOTE",
                     "OSE1960", "OSE2004", "DOSE",
                     "OME1960", "OME2004", "DOME",
                     "OSME1960", "OSME2004", "DOSME")
     
     data_replication$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                            "GA","IA","ID","IL","IN","KS","KY","LA",
                            "MA","MD","ME","MI","MN","MO","MS","MT",
                            "NC","ND","NE","NH","NJ","NM","NV","NY",
                            "OH","OK","OR","PA","RI","SC","SD","TN",
                            "TX","UT","VA","VT","WA","WI","WV","WY"))
     
    # PROFITABILITY 
       
      # PROF1960
      data_replication$PROF1960 <- Lowe.level_vrs$PROF[Lowe.level_vrs$year==1960]
      # PROF04
      data_replication$PROF2004 <- Lowe.level_vrs$PROF[Lowe.level_vrs$year==2004]
      # DPROF
      data_replication$DPROF <- (data_replication$PROF2004 / data_replication$PROF1960)
    
    # TERMS OF TRADE 
      
      # TT1960
      data_replication$TT1960 <- Lowe.level_vrs$TT[Lowe.level_vrs$year==1960]
      # TT04
      data_replication$TT2004 <- Lowe.level_vrs$TT[Lowe.level_vrs$year==2004]
      # DTT
      data_replication$DTT <- (data_replication$TT2004 / data_replication$TT1960)
      
    # TOTAL FACTOR PRODUCTIVITY 
      
      # TFP1960
      data_replication$TFP1960 <- Lowe.level_vrs$TFP[Lowe.level_vrs$year==1960]
      # TFP04
      data_replication$TFP2004 <- Lowe.level_vrs$TFP[Lowe.level_vrs$year==2004]
      # DTFP
      data_replication$DTFP <- (data_replication$TFP2004 / data_replication$TFP1960)
      
    # MAXIMM TOTAL FACTOR PRODUCTIVITY 
      
      # MP1960
      data_replication$MP1960 <- Lowe.level_vrs$MP[Lowe.level_vrs$year==1960]
      # MP04
      data_replication$MP2004 <- Lowe.level_vrs$MP[Lowe.level_vrs$year==2004]
      # DMP
      data_replication$DMP <- (data_replication$MP2004 / data_replication$MP1960)
      
    # TFP EFFICIENCY 
      
      # TFPE1960
      data_replication$TFPE1960 <- Lowe.level_vrs$TFPE[Lowe.level_vrs$year==1960]
      # TFPE04
      data_replication$TFPE2004 <- Lowe.level_vrs$TFPE[Lowe.level_vrs$year==2004]
      # DTFPE
      data_replication$DTFPE <- (data_replication$TFPE2004 / data_replication$TFPE1960)
      
    # OUTPUT TECHNICAL EFFICIENCY 
      
      # OTE1960
      data_replication$OTE1960 <- Lowe.level_vrs$OTE[Lowe.level_vrs$year==1960]
      # OTE04
      data_replication$OTE2004 <- Lowe.level_vrs$OTE[Lowe.level_vrs$year==2004]
      # DOTE
      data_replication$DOTE <- (data_replication$OTE2004 / data_replication$OTE1960)
      
    # OUTPUT SCALE EFFICIENCY 
      
      # OSE1960
      data_replication$OSE1960 <- Lowe.level_vrs$OSE[Lowe.level_vrs$year==1960]
      # OSE04
      data_replication$OSE2004 <- Lowe.level_vrs$OSE[Lowe.level_vrs$year==2004]
      # DOSE
      data_replication$DOSE <- (data_replication$OSE2004 / data_replication$OSE1960)
    
    # OUTPUT MIX EFFICIENCY 
      
      # OME1960
      data_replication$OME1960 <- Lowe.level_vrs$OME[Lowe.level_vrs$year==1960]
      # OME04
      data_replication$OME2004 <- Lowe.level_vrs$OME[Lowe.level_vrs$year==2004]
      # DOME
      data_replication$DOME <- (data_replication$OME2004 / data_replication$OME1960)
      
    # OUTPUT SCALE MIX EFFICIENCY 
      
      # OSME1960
      data_replication$OSME1960 <- Lowe.level_vrs$OSME[Lowe.level_vrs$year==1960]
      # OSME04
      data_replication$OSME2004 <- Lowe.level_vrs$OSME[Lowe.level_vrs$year==2004]
      # DOSME
      data_replication$DOSME <- (data_replication$OSME2004 / data_replication$OSME1960)
      
    # Add US-average as geometric mean
      data_replication <- data_replication %>% 
        add_row(state="US48",
                PROF1960=NA, PROF2004=NA, DPROF=exp(mean(log(data_replication$DPROF))),
                TT1960=NA, TT2004=NA, DTT=exp(mean(log(data_replication$DTT))),
                TFP1960=NA, TFP2004=NA, DTFP=exp(mean(log(data_replication$DTFP))),
                MP1960=NA, MP2004=NA, DMP=exp(mean(log(data_replication$DMP))),
                TFPE1960=NA, TFPE2004=NA, DTFPE=exp(mean(log(data_replication$DTFPE))),
                OTE1960=NA, OTE2004=NA, DOTE=exp(mean(log(data_replication$DOTE))),
                OSE1960=NA, OSE2004=NA, DOSE=exp(mean(log(data_replication$DOSE))),
                OME1960=NA, OME2004=NA, DOME=exp(mean(log(data_replication$DOME))),
                OSME1960=NA, OSME2004=NA, DOSME=exp(mean(log(data_replication$DOSME))))
    
    # Save US-average for comparison in 03h_TFP for US_comparison.R
      
      Summary_lowe_US_vrs <- list(data_replication$DTFP[data_replication$state=="US48"],
                              data_replication$DMP[data_replication$state=="US48"],
                              data_replication$DTFPE[data_replication$state=="US48"])
      names(Summary_lowe_US_vrs) <- c("TFP", "TC", "TFPE") 
      save(Summary_lowe_US_vrs, file = "R_output/Summary_lowe_US_vrs.Rda")
      
  # Write Table 2 from O'Donnell (2012): "Profitability, TFP, and efficiency change in US agriculture (1960-2004) using the Lowe index)

    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Select data
    data_replication_ProfDecomp <- subset(data_replication, select = c(1:16))
    
    # Write table  
    Tab_replication_ProfDecomp_vrs <- kable(data_replication_ProfDecomp, booktabs = T, 
                digits = 2,
                row.names = FALSE,
                escape = FALSE,
                linesep = "",
                caption = "Profitability, TFP, and efficiency change in US agriculture (1960--2004) using the Lowe index under VRS. Replicated results from \\cite{odonnell_nonparametric_2012}.",
                label = "Tab_replication_ProfDecomp_vrs",
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
    writeLines(Tab_replication_ProfDecomp_vrs, "Tables/Tab_replication_ProfDecomp_vrs.tex")
    

# Write Table 3 from O'Donnell (2012)
    
    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Select data
    data_replication_TPFEDecomp <- subset(data_replication, select = c(1,14:28))
    
    # Create table  
    Tab_replication_TFPEDecomp_vrs <- kable(data_replication_TPFEDecomp, booktabs = T, 
                       digits = 2,
                       row.names = FALSE,
                       escape = FALSE,
                       linesep = "",
                       caption = "Output-oriented components of efficiency change in US agriculture (1960--2004) using the Lowe index under VRS. Replicated results from \\cite{odonnell_nonparametric_2012}.",
                       label = "Tab_replication_TFPEDecomp_vrs",
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
    writeLines(Tab_replication_TFPEDecomp_vrs, "Tables/Tab_replication_TFPEDecomp_vrs.tex")
    
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
     
  lowe1960 <- subset(Lowe.level_vrs, year==1960)
  lowe1970 <- subset(Lowe.level_vrs, year==1970)
  lowe1980 <- subset(Lowe.level_vrs, year==1980)
  lowe1990 <- subset(Lowe.level_vrs, year==1990)
  lowe2002 <- subset(Lowe.level_vrs, year==2002)

  #1960-1970
  data_table$`TFP60-70`   <- log(c(lowe1970[,"TFP"])/c(lowe1960[,"TFP"])) / (1970-1960) * 100 
  data_table$`MP60-70`    <- log(c(lowe1970[,"MP"])/c(lowe1960[,"MP"])) / (1970-1960) * 100 
  data_table$`OTE60-70`   <- log(c(lowe1970[,"OTE"])/c(lowe1960[,"OTE"])) / (1970-1960) * 100 
  data_table$`OSME60-70`  <- log(c(lowe1970[,"OSME"])/c(lowe1960[,"OSME"])) / (1970-1960) * 100 
   
  #1970-1980
  data_table$`TFP70-80`   <- log(c(lowe1980[,"TFP"])/c(lowe1970[,"TFP"])) / (1980-1970) * 100 
  data_table$`MP70-80`    <- log(c(lowe1980[,"MP"])/c(lowe1970[,"MP"])) / (1980-1970) * 100 
  data_table$`OTE70-80`   <- log(c(lowe1980[,"OTE"])/c(lowe1970[,"OTE"])) / (1980-1970) * 100 
  data_table$`OSME70-80`  <- log(c(lowe1980[,"OSME"])/c(lowe1970[,"OSME"])) / (1980-1970) * 100 
  
  #1980-1990
  data_table$`TFP80-90`   <- log(c(lowe1990[,"TFP"])/c(lowe1980[,"TFP"])) / (1990-1980) * 100 
  data_table$`MP80-90`    <- log(c(lowe1990[,"MP"])/c(lowe1980[,"MP"])) / (1990-1980) * 100 
  data_table$`OTE80-90`   <- log(c(lowe1990[,"OTE"])/c(lowe1980[,"OTE"])) / (1990-1980) * 100 
  data_table$`OSME80-90`  <- log(c(lowe1990[,"OSME"])/c(lowe1980[,"OSME"])) / (1990-1980) * 100 
  
  #1990-2002
  data_table$`TFP90-02`   <- log(c(lowe2002[,"TFP"])/c(lowe1990[,"TFP"])) / (2002-1990) * 100 
  data_table$`MP90-02`    <- log(c(lowe2002[,"MP"])/c(lowe1990[,"MP"])) / (2002-1990) * 100 
  data_table$`OTE90-02`   <- log(c(lowe2002[,"OTE"])/c(lowe1990[,"OTE"])) / (2002-1990) * 100 
  data_table$`OSME90-02`  <- log(c(lowe2002[,"OSME"])/c(lowe1990[,"OSME"])) / (2002-1990) * 100 
  
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
  
  
# Write Table 4 from O'Donnell (2012): Average annual rates of growth in TFP and efficiency in US agriculture (1960-2002) using the Lowe index.
  
  # Create table  
  Tab_replication_AvgRates_vrs <- kable(data_table, booktabs = T, 
                     digits = 2,
                     row.names = FALSE,
                     escape = FALSE,
                     linesep = "",
                     caption = "Average annual rates of growth (\\%) in TFP and efficiency in US agriculture (1960--2002) using the Lowe index under VRS. Replicated results from \\cite{odonnell_nonparametric_2012}.",
                     label = "Tab_replication_AvgRates_vrs",
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
  writeLines(Tab_replication_AvgRates_vrs, "Tables/Tab_replication_AvgRates_vrs.tex")
  
  
# ----------------------------------------- #
#### Create new figures for US aggregate ####
# ----------------------------------------- #

  # Prepare the data
  
    # Compare every level to the state's *own* 1960 level --> obtains cumulative growth 
    idstates <- unique(Lowe.level_vrs$state)
    Lowe.cumulative_vrs <- list()
    for (i in idstates) {
      Lowe.cumulative_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                      sweep(Lowe.level_vrs[Lowe.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                            STATS = as.numeric(Lowe.level_vrs[Lowe.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    Lowe.cumulative_vrs <- do.call(rbind, Lowe.cumulative_vrs) #turns the list into a data frame
    
    # Calculate US average using the geometric mean
    Lowe.US48_vrs <- list()
    for (yr in 1960:2004) {
      Lowe.US48_vrs[[which(yr == 1960:2004)]] <- c(year = yr, apply(Lowe.cumulative_vrs[Lowe.cumulative_vrs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    Lowe.US48_vrs <- as.data.frame(do.call(rbind, Lowe.US48_vrs)) #turns the list into a data frame
  
  
    # For the efficiencies (last figure), we need arithmetic averages
    Lowe.US48_vrs_eff <- Lowe.level_vrs[,c("state", "year","TFPE", "OTE", "OME", "ROSE", "OSME")]
    Lowe.US48_vrs_eff <- Lowe.US48_vrs_eff %>% 
      group_by(year) %>% 
      summarise_at(.vars = vars(TFPE,OTE,OME,ROSE,OSME),
                   .funs = c(mean="mean"))
      

  #-------------------------------------------------------------#
  # Panel A: Profitability, quantity, and price change (1960=1) #
  #-------------------------------------------------------------#
  
    # Prepare data for the plot
    data_plot <- Lowe.US48_vrs %>% 
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
      scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
      scale_y_continuous(breaks = seq(0, 7, by = 1), limits=c(0,7.5)) +  
      theme(axis.text.x = element_text(angle=90)) +
      theme(legend.position="bottom",
            legend.margin=margin(t = -0.7, unit='cm'),
            legend.text=element_text(size=6))


    
    # --------------------------------- #
    # Panel B: Components of TFP change #
    # --------------------------------- #
      
      # Prepare data for the plot
      data_plot <- Lowe.US48_vrs %>% 
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
        scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
        scale_y_continuous(breaks = seq(0, 2.5, by = 0.5), limits=c(0,2.8)) +  
        theme(axis.text.x = element_text(angle=90)) +
        theme(legend.position="bottom",
              legend.margin=margin(t = -0.7, unit='cm'),
              legend.text=element_text(size=6))
      
    # -------------------------- #
    # Panel C: Efficiency levels #
    # -------------------------- #
      
      # Prepare data for the plot
      data_plot <- Lowe.US48_vrs_eff %>% 
        select(-ROSE_mean) %>% 
        reshape2::melt(id.vars="year")

      # plot
      panel_C <- ggplot() + 
        geom_line(data = data_plot, aes(x = year, y = value, linetype = variable, color = variable)
        ) + labs(
          x = "",
          y = "",
          linetype = ""
        ) + scale_linetype_manual(name  ="",
                                  breaks=c("OTE_mean", "OME_mean", "OSME_mean", "TFPE_mean"),
                                  labels=c("OTE", "OME", "OSME", "TFPE"),
                                  values=c("solid", "42", "solid", "12")) +
        scale_color_manual(name  ="",
                           breaks=c("OTE_mean", "OME_mean", "OSME_mean", "TFPE_mean"),
                           labels=c("OTE", "OME",  "OSME", "TFPE"),
                           values=c("#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) +
      
        theme_bw() +
        scale_x_continuous(breaks = seq(1960, 2004, by = 4)) +
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
      tikz(file = "Figures/Fig_replication_US48_vrs.tex", width = 7, height = 7, sanitize=TRUE)
      plot <- ggarrange(panel_A, panel_B, panel_C,
                        labels = c("a", "b", "c"),
                        ncol = 2, nrow = 2)
      print(plot)
      dev.off()
      