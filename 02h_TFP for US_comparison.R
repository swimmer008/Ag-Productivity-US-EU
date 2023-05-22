# ------------------------------------------ #
#                                            #
# This program compares TFP growth across    #
# all considered indices for the US state-   #
# level data                                 #
#                                            #
# ------------------------------------------ #

library(dplyr)
library(ggpubr) ## for ggarrange
library(tikzDevice) #to save ggplot object in latex format
library(kableExtra) # to convert data frames to Latex
library(reshape)

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')

#----------------------------------------------------#
# comparison 1: Table for Avg. annual rate of change #
#----------------------------------------------------#

  # Load Lowe VRS 
  load("r_output/Summary_lowe_US_vrs.Rda")
  # Load Lowe CRS
  load("r_output/Summary_lowe_US_crs.Rda")
  # Load A-DEA VRS
  load("r_output/Summary_adea_US_vrs.Rda")
  # Load A-DEA CRS
  load("r_output/Summary_adea_US_crs.Rda")
  # Load M-SFA VRS
  load("R_output/Summary_msfa_US_vrs.Rda")
  # Load M-SFA CRS
  load("R_output/Summary_msfa_US_crs.Rda")
  # Load Global Malmquist
  load("R_output/Summary_globmalm_US.Rda")
  
  
  # Create data frame for table
  Summary_US <- data.frame(matrix(ncol = 4, nrow = 7))
  colnames(Summary_US) <- c("index", 
                            "TFP", "TC", "TFPE") 
  
  Summary_US$index <- c("Lowe (VRS)", "Lowe (CRS)",
                        "A-DEA (VRS)", "A-DEA (CRS)",
                        "M-SFA (VRS)", "M-SFA (CRS)",
                        "Global Malmquist")
  
  Summary_US$TFP <- c(Summary_lowe_US_vrs$TFP, Summary_lowe_US_crs$TFP,
                      Summary_adea_US_vrs$TFP, Summary_adea_US_crs$TFP,
                      Summary_msfa_US_vrs$TFP, Summary_msfa_US_crs$TFP,
                      Summary_globmalm_US$TFP)
  
  Summary_US$TC <- c(Summary_lowe_US_vrs$TC, Summary_lowe_US_crs$TC,
                      Summary_adea_US_vrs$TC, Summary_adea_US_crs$TC,
                      Summary_msfa_US_vrs$TC, Summary_msfa_US_crs$TC,
                      Summary_globmalm_US$TC)
  
  Summary_US$TFPE <- c(Summary_lowe_US_vrs$TFPE, Summary_lowe_US_crs$TFPE,
                     Summary_adea_US_vrs$TFPE, Summary_adea_US_crs$TFPE,
                     Summary_msfa_US_vrs$TFPE, Summary_msfa_US_crs$TFPE,
                     Summary_globmalm_US$TFPE)
  
  Summary_US[,-1] <- log(Summary_US[,-1])/(2004-1960)*100
  
  # Write Table: Average annual TFP growth rates in US agriculture (1960-2004) from different indices
  
    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Create table  
    Tab_Summary_US <- kable(Summary_US, booktabs = T, 
                          digits = 2,
                          row.names = FALSE,
                          escape = FALSE,
                          linesep = "",
                          caption = "Average annual growth rates (\\%) in TFP and components in US agriculture (1960--2004) based on different indices.",
                          label = "Tab_Summary_US",
                          col.names = c('TFP index', 
                                        'TFP', 'Technical change', "Efficiency change")) %>%
      footnote(general = "TFP is total factor productivity. VRS and CRS indicate variable returns to scale and constrant returns to scale, respectively.",
               footnote_as_chunk = T,
               threeparttable = T,
               general_title = "Notes:",
               escape=F) %>%
      kable_styling(latex_options = c("HOLD_position")) 
    
    # Print Latex file
    writeLines(Tab_Summary_US, "Tables/Tab_Summary_US.tex")



#----------------------------------------#
# Comparison 2: Kernel density estimator #
#----------------------------------------#

    # Load results
    load("R_output/Lowe.level_vrs.Rda")
    load("R_output/Lowe.level_crs.Rda")
    load("R_output/ADEA.level_vrs.Rda")
    load("R_output/ADEA.level_crs.Rda")
    load("R_output/MSFA.level_vrs.Rda")
    load("R_output/MSFA.level_crs.Rda")
    load("R_output/GlobMalm.levels.Rda")
  
  # LOWE VRS
        
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(Lowe.level_vrs$state)
      Lowe.cum_vrs <- list()
      for (i in idstates) {
        Lowe.cum_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                       sweep(Lowe.level_vrs[Lowe.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                             STATS = as.numeric(Lowe.level_vrs[Lowe.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      Lowe.cum_vrs <- do.call(rbind, Lowe.cum_vrs) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      Lowe.cum_vrs = subset(Lowe.cum_vrs, select = c("year", "TFP") )
      Lowe.cum_vrs <- dplyr::rename(Lowe.cum_vrs, "TFP_lowe_vrs" = "TFP")
      
      # Third: Delete first year
      Lowe.cum_vrs <- Lowe.cum_vrs %>% 
        filter(year>1960) 
      
    # LOWE CRS
      
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(Lowe.level_crs$state)
      Lowe.cum_crs <- list()
      for (i in idstates) {
        Lowe.cum_crs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                    sweep(Lowe.level_crs[Lowe.level_crs$state==i,-c(1:2)],MARGIN = 2,
                                                          STATS = as.numeric(Lowe.level_crs[Lowe.level_crs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      Lowe.cum_crs <- do.call(rbind, Lowe.cum_crs) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      Lowe.cum_crs = subset(Lowe.cum_crs, select = c("year", "TFP") )
      Lowe.cum_crs <- dplyr::rename(Lowe.cum_crs, "TFP_lowe_crs" = "TFP")
      
      # Third: Delete first year
      Lowe.cum_crs <- Lowe.cum_crs %>% 
        filter(year>1960) 
      
    # ADEA VRS
      
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(ADEA.level_vrs$state)
      ADEA.cum_vrs <- list()
      for (i in idstates) {
        ADEA.cum_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                    sweep(ADEA.level_vrs[ADEA.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                          STATS = as.numeric(ADEA.level_vrs[ADEA.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      ADEA.cum_vrs <- do.call(rbind, ADEA.cum_vrs) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      ADEA.cum_vrs = subset(ADEA.cum_vrs, select = c("year", "TFP") )
      ADEA.cum_vrs <- dplyr::rename(ADEA.cum_vrs, "TFP_ADEA_vrs" = "TFP")
      
      # Third: Delete first year
      ADEA.cum_vrs <- ADEA.cum_vrs %>% 
        filter(year>1960) 
      
    # ADEA CRS
      
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(ADEA.level_crs$state)
      ADEA.cum_crs <- list()
      for (i in idstates) {
        ADEA.cum_crs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                    sweep(ADEA.level_crs[ADEA.level_crs$state==i,-c(1:2)],MARGIN = 2,
                                                          STATS = as.numeric(ADEA.level_crs[ADEA.level_crs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      ADEA.cum_crs <- do.call(rbind, ADEA.cum_crs) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      ADEA.cum_crs = subset(ADEA.cum_crs, select = c("year", "TFP") )
      ADEA.cum_crs <- dplyr::rename(ADEA.cum_crs, "TFP_ADEA_crs" = "TFP")
      
      # Third: Delete first year
      ADEA.cum_crs <- ADEA.cum_crs %>% 
        filter(year>1960) 
        
    # MSFA VRS
      
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(MSFA.level_vrs$state)
      MSFA.cum_vrs <- list()
      for (i in idstates) {
        MSFA.cum_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                    sweep(MSFA.level_vrs[MSFA.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                          STATS = as.numeric(MSFA.level_vrs[MSFA.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      MSFA.cum_vrs <- do.call(rbind, MSFA.cum_vrs) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      MSFA.cum_vrs = subset(MSFA.cum_vrs, select = c("year", "TFPit") )
      MSFA.cum_vrs <- dplyr::rename(MSFA.cum_vrs, "TFP_MSFA_vrs" = "TFPit")
      
      # Third: Delete first year
      MSFA.cum_vrs <- MSFA.cum_vrs %>% 
        filter(year>1960) 
      
    # MSFA CRS
      
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(MSFA.level_crs$state)
      MSFA.cum_crs <- list()
      for (i in idstates) {
        MSFA.cum_crs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                    sweep(MSFA.level_crs[MSFA.level_crs$state==i,-c(1:2)],MARGIN = 2,
                                                          STATS = as.numeric(MSFA.level_crs[MSFA.level_crs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      MSFA.cum_crs <- do.call(rbind, MSFA.cum_crs) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      MSFA.cum_crs = subset(MSFA.cum_crs, select = c("year", "TFPit") )
      MSFA.cum_crs <- dplyr::rename(MSFA.cum_crs, "TFP_MSFA_crs" = "TFPit")
      
      # Third: Delete first year
      MSFA.cum_crs <- MSFA.cum_crs %>% 
        filter(year>1960) 
      
    # Global Malmquist
      
      # First: Compare every level to the state's *own* 1960 level
      idstates <- unique(GlobMalm.levels$state)
      GlobMalm.cum <- list()
      for (i in idstates) {
        GlobMalm.cum[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                    sweep(GlobMalm.levels[GlobMalm.levels$state==i,-c(1:3)],MARGIN = 2,
                                                          STATS = as.numeric(GlobMalm.levels[GlobMalm.levels$state==i,-c(1:3)][1,]),FUN = "/")) # for myself: 2 refers to column
      }
      GlobMalm.cum <- do.call(rbind, GlobMalm.cum) #turns the list into a data frame
      
      # Second: Keep only TFP and rename it
      GlobMalm.cum = subset(GlobMalm.cum, select = c("year", "DOGt") )
      GlobMalm.cum <- dplyr::rename(GlobMalm.cum, "TFP_GlobMalm" = "DOGt")
      
      # Third: Delete first year
      GlobMalm.cum <- GlobMalm.cum %>% 
        filter(year>1960) 
      

        
# all indexes
densities_TFP_US48 <- cbind(Lowe.cum_vrs,
                       Lowe.cum_crs,
                       ADEA.cum_vrs,
                       ADEA.cum_crs,
                       MSFA.cum_vrs,
                       MSFA.cum_crs,
                       GlobMalm.cum)

# reshape
densities_TFP_US48 <- as.data.frame(melt(densities_TFP_US48,id="year"))
str(densities_TFP_US48)

#plot
Fig_densities_TFP_US48 <- ggplot() + 
  geom_density(data = densities_TFP_US48, aes(x = value, linetype = variable, color = variable)
  ) + labs(
    x = "",
    y = "",
    linetype = ""
  ) + scale_linetype_manual(name = "",
                            breaks=c("TFP_lowe_vrs", "TFP_lowe_crs", "TFP_ADEA_vrs", "TFP_ADEA_crs", "TFP_MSFA_vrs", "TFP_MSFA_crs", "TFP_GlobMalm"),
                            labels=c("Lowe VRS", "Lowe CRS", "A-DEA VRS", "A-DEA CRS", "M-SFA VRS", "M-SFA CRS", "Global Malmquist"),
                            values=c("solid", "solid", "42", "42", "1141", "1141", "12")) +
  scale_color_manual(name  ="",
                     breaks=c("TFP_lowe_vrs", "TFP_lowe_crs", "TFP_ADEA_vrs", "TFP_ADEA_crs", "TFP_MSFA_vrs", "TFP_MSFA_crs", "TFP_GlobMalm"),
                     labels=c("Lowe VRS", "Lowe CRS", "A-DEA VRS", "A-DEA CRS", "M-SFA VRS", "M-SFA CRS", "Global Malmquist"),
                     values=c("#0a0a0a","#9E9E9E","#0a0a0a","#9E9E9E","#0a0a0a","#9E9E9E","#9E9E9E")) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-1, 7, by = 1)) +
  theme(axis.text.x = element_text(angle=0)) +
  theme(legend.position="none")  

# Note: In Stata I can check how different the distributions are
#       using the Quantile approach by Combes et al. (2012) with 
#       the estquant function. See the file "Stata_CompareDensities.do".

#foreign::write.dta(densities_all[densities_all$variable%in%c("TFP_fp.par","TFP_malm"),],file="R_output/densities.dta")
#for review: export all densities
foreign::write.dta(densities_TFP_US48,file="R_output/densities.dta")


#--------------------------------#
# Comparison 3: Plot average TFP #
#--------------------------------#

# LOWE VRS

    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(Lowe.level_vrs$state)
    Lowe.cum_vrs <- list()
    for (i in idstates) {
      Lowe.cum_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                  sweep(Lowe.level_vrs[Lowe.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                        STATS = as.numeric(Lowe.level_vrs[Lowe.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    Lowe.cum_vrs <- do.call(rbind, Lowe.cum_vrs) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    Lowe_US48.cum_vrs <- list()
    for (yr in 1960:2004) {
      Lowe_US48.cum_vrs[[which(yr == 1960:2004)]] <- c(year = yr, apply(Lowe.cum_vrs[Lowe.cum_vrs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    Lowe_US48.cum_vrs <- as.data.frame(do.call(rbind, Lowe_US48.cum_vrs)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    Lowe_US48.cum_vrs = subset(Lowe_US48.cum_vrs, select = c("year", "TFP") )
    Lowe_US48.cum_vrs <- dplyr::rename(Lowe_US48.cum_vrs, "TFP_lowe_vrs" = "TFP")

# LOWE CRS
    
    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(Lowe.level_crs$state)
    Lowe.cum_crs <- list()
    for (i in idstates) {
      Lowe.cum_crs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                  sweep(Lowe.level_crs[Lowe.level_crs$state==i,-c(1:2)],MARGIN = 2,
                                                        STATS = as.numeric(Lowe.level_crs[Lowe.level_crs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    Lowe.cum_crs <- do.call(rbind, Lowe.cum_crs) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    Lowe_US48.cum_crs <- list()
    for (yr in 1960:2004) {
      Lowe_US48.cum_crs[[which(yr == 1960:2004)]] <- c(year = yr, apply(Lowe.cum_crs[Lowe.cum_crs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    Lowe_US48.cum_crs <- as.data.frame(do.call(rbind, Lowe_US48.cum_crs)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    Lowe_US48.cum_crs = subset(Lowe_US48.cum_crs, select = c("year", "TFP") )
    Lowe_US48.cum_crs <- dplyr::rename(Lowe_US48.cum_crs, "TFP_lowe_crs" = "TFP")
    
  # ADEA VRS
    
    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(ADEA.level_vrs$state)
    ADEA.cum_vrs <- list()
    for (i in idstates) {
      ADEA.cum_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                  sweep(ADEA.level_vrs[ADEA.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                        STATS = as.numeric(ADEA.level_vrs[ADEA.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    ADEA.cum_vrs <- do.call(rbind, ADEA.cum_vrs) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    ADEA_US48.cum_vrs <- list()
    for (yr in 1960:2004) {
      ADEA_US48.cum_vrs[[which(yr == 1960:2004)]] <- c(year = yr, apply(ADEA.cum_vrs[ADEA.cum_vrs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    ADEA_US48.cum_vrs <- as.data.frame(do.call(rbind, ADEA_US48.cum_vrs)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    ADEA_US48.cum_vrs = subset(ADEA_US48.cum_vrs, select = c("year", "TFP") )
    ADEA_US48.cum_vrs <- dplyr::rename(ADEA_US48.cum_vrs, "TFP_ADEA_vrs" = "TFP")
    
  # ADEA CRS
    
    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(ADEA.level_crs$state)
    ADEA.cum_crs <- list()
    for (i in idstates) {
      ADEA.cum_crs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                  sweep(ADEA.level_crs[ADEA.level_crs$state==i,-c(1:2)],MARGIN = 2,
                                                        STATS = as.numeric(ADEA.level_crs[ADEA.level_crs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    ADEA.cum_crs <- do.call(rbind, ADEA.cum_crs) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    ADEA_US48.cum_crs <- list()
    for (yr in 1960:2004) {
      ADEA_US48.cum_crs[[which(yr == 1960:2004)]] <- c(year = yr, apply(ADEA.cum_crs[ADEA.cum_crs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    ADEA_US48.cum_crs <- as.data.frame(do.call(rbind, ADEA_US48.cum_crs)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    ADEA_US48.cum_crs = subset(ADEA_US48.cum_crs, select = c("year", "TFP") )
    ADEA_US48.cum_crs <- dplyr::rename(ADEA_US48.cum_crs, "TFP_ADEA_crs" = "TFP")
    
    
# MSFA VRS
    
    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(MSFA.level_vrs$state)
    MSFA.cum_vrs <- list()
    for (i in idstates) {
      MSFA.cum_vrs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                  sweep(MSFA.level_vrs[MSFA.level_vrs$state==i,-c(1:2)],MARGIN = 2,
                                                        STATS = as.numeric(MSFA.level_vrs[MSFA.level_vrs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    MSFA.cum_vrs <- do.call(rbind, MSFA.cum_vrs) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    MSFA_US48.cum_vrs <- list()
    for (yr in 1960:2004) {
      MSFA_US48.cum_vrs[[which(yr == 1960:2004)]] <- c(year = yr, apply(MSFA.cum_vrs[MSFA.cum_vrs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    MSFA_US48.cum_vrs <- as.data.frame(do.call(rbind, MSFA_US48.cum_vrs)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    MSFA_US48.cum_vrs = subset(MSFA_US48.cum_vrs, select = c("year", "TFPit") )
    MSFA_US48.cum_vrs <- dplyr::rename(MSFA_US48.cum_vrs, "TFP_MSFA_vrs" = "TFPit")
    
# MSFA CRS
    
    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(MSFA.level_crs$state)
    MSFA.cum_crs <- list()
    for (i in idstates) {
      MSFA.cum_crs[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                  sweep(MSFA.level_crs[MSFA.level_crs$state==i,-c(1:2)],MARGIN = 2,
                                                        STATS = as.numeric(MSFA.level_crs[MSFA.level_crs$state==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    MSFA.cum_crs <- do.call(rbind, MSFA.cum_crs) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    MSFA_US48.cum_crs <- list()
    for (yr in 1960:2004) {
      MSFA_US48.cum_crs[[which(yr == 1960:2004)]] <- c(year = yr, apply(MSFA.cum_crs[MSFA.cum_crs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    MSFA_US48.cum_crs <- as.data.frame(do.call(rbind, MSFA_US48.cum_crs)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    MSFA_US48.cum_crs = subset(MSFA_US48.cum_crs, select = c("year", "TFPit") )
    MSFA_US48.cum_crs <- dplyr::rename(MSFA_US48.cum_crs, "TFP_MSFA_crs" = "TFPit")
    
    
# Global Malmquist
    
    # First: Compare every level to the state's *own* 1960 level
    idstates <- unique(GlobMalm.levels$state)
    GlobMalm.cum_ <- list()
    for (i in idstates) {
      GlobMalm.cum_[[which(i==idstates)]] <- cbind(state = i, year=1960:2004,
                                                   sweep(GlobMalm.levels[GlobMalm.levels$state==i,-c(1:3)],MARGIN = 2,
                                                         STATS = as.numeric(GlobMalm.levels[GlobMalm.levels$state==i,-c(1:3)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    GlobMalm.cum_ <- do.call(rbind, GlobMalm.cum_) #turns the list into a data frame
    
    # Second: Calculate the geometric mean
    #  --> Create the US average using a geometric mean (no arithmetic because we do not have logs)
    GlobMalm_US48.cum <- list()
    for (yr in 1960:2004) {
      GlobMalm_US48.cum[[which(yr == 1960:2004)]] <- c(year = yr, apply(GlobMalm.cum_[GlobMalm.cum_$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    GlobMalm_US48.cum <- as.data.frame(do.call(rbind, GlobMalm_US48.cum)) #turns the list into a data frame
    
    # Third: Keep only TFP and rename it
    GlobMalm_US48.cum = subset(GlobMalm_US48.cum, select = c("year", "DOGt") )
    GlobMalm_US48.cum <- dplyr::rename(GlobMalm_US48.cum, "TFP_GlobMalm" = "DOGt")
    
# all indexes
All_TFP_US48 <- left_join(Lowe_US48.cum_vrs,Lowe_US48.cum_crs)
All_TFP_US48 <- left_join(All_TFP_US48,ADEA_US48.cum_vrs)
All_TFP_US48 <- left_join(All_TFP_US48,ADEA_US48.cum_crs)
All_TFP_US48 <- left_join(All_TFP_US48,MSFA_US48.cum_vrs)
All_TFP_US48 <- left_join(All_TFP_US48,MSFA_US48.cum_crs)
All_TFP_US48 <- left_join(All_TFP_US48,GlobMalm_US48.cum)

#reshape
All_TFP_US48 <- melt(All_TFP_US48, id.vars="year")


#plot
Fig_All_TFP_US48 <- ggplot() + 
  geom_line(data = All_TFP_US48, aes(x = year, y = value, linetype = variable, color = variable)
  ) + labs(
    x = "",
    y = "",
    linetype = ""
  ) + scale_linetype_manual(name  ="",
                            breaks=c("TFP_lowe_vrs", "TFP_lowe_crs", "TFP_ADEA_vrs", "TFP_ADEA_crs", "TFP_MSFA_vrs", "TFP_MSFA_crs", "TFP_GlobMalm"),
                            labels=c("Lowe VRS", "Lowe CRS", "A-DEA VRS", "A-DEA CRS", "M-SFA VRS", "M-SFA CRS", "Global Malmquist"),
                            values=c("solid", "solid", "42", "42", "1141", "1141", "12")) +
  scale_color_manual(name  ="",
                     breaks=c("TFP_lowe_vrs", "TFP_lowe_crs", "TFP_ADEA_vrs", "TFP_ADEA_crs", "TFP_MSFA_vrs", "TFP_MSFA_crs", "TFP_GlobMalm"),
                     labels=c("Lowe VRS", "Lowe CRS", "A-DEA VRS", "A-DEA CRS", "M-SFA VRS", "M-SFA CRS", "Global Malmquist"),
                     values=c("#0a0a0a","#9E9E9E","#0a0a0a","#9E9E9E","#0a0a0a","#9E9E9E","#9E9E9E")) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1960, 2004, by = 3)) +
  scale_y_continuous(breaks = seq(0, 2, by = 1), limits=c(0,2.5)) +  
  theme(axis.text.x = element_text(angle=90)) +
  theme(legend.position="bottom",
        legend.margin=margin(t = -0.7, unit='cm'),
        legend.text=element_text(size=7))


#--------------------------#
# both plots in one figure #
#--------------------------#

tikz(file = "Figures/Fig_Compare_TFP_US48.tex", width = 6, height = 6)
plot <- ggarrange(Fig_densities_TFP_US48,Fig_All_TFP_US48,
                  labels = c("a", "b"),
                  ncol = 1, nrow = 2)
print(plot)
dev.off()
    