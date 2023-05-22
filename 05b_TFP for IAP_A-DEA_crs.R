# --------------------------------------- #
#                                         #
# This program estimates the additive TFP #
# index using shadow prices as weights    #
# for the IAP country-level data          #
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
  
# Load the IAP country-level data and arrange by country name
load("R_output/IAPdata.Rda") 
IAPdata <- IAPdata[order(IAPdata$country),]
  

# ------------------------------------ #
#### Estimation of reference prices #### 
# ------------------------------------ #

# Estimation of each observations' shadow prices
  
  # Input and output vectors
  XREF <- t(as.matrix(IAPdata[, c("x_land", "x_labor","x_capital","x_fertilizer", "x_feed")]))
  YREF <- t(as.matrix(IAPdata[, c("q_crops", "q_animals")])) # Note: When I add fish, I get problems with the IDF
  
  # Output distance function
  outShad <- foreach(dmu = 1:dim(IAPdata)[1], .combine = rbind) %do%
    {
      DO.shdu(XOBS = XREF[, dmu], YOBS = YREF[, dmu], XREF = XREF,
              YREF = YREF, rts = "crs")
    }
  
  # Input distance function
  inpShad <- foreach(dmu = 1:dim(IAPdata)[1], .combine = rbind) %do%
    {
      DI.shdu(XOBS = XREF[, dmu], YOBS = YREF[, dmu], XREF = XREF,
              YREF = YREF, rts = "crs")
    }

# Use mean shadow prices as reference prices 
  
  # Compute means
  meanY <- apply(outShad, 2, FUN = function(x) mean(x))
  meanX <- apply(inpShad, 2, FUN = function(x) mean(x))
  
  # Add reference prices to data frame
  IAPdata$p0_crops <- meanY[1] 
  IAPdata$p0_animals <- meanY[2]
  
  IAPdata$w0_land <- meanX[1]
  IAPdata$w0_labor <- meanX[2]
  IAPdata$w0_capital <- meanX[3]
  IAPdata$w0_fertilizer <- meanX[4]
  IAPdata$w0_feed <- meanX[5]


  # -------------------------------------------- #
  #### Calculate and decompose TFP under CRS  #### 
  # -------------------------------------------- #
  
  # Note: We use the code from the Lowe index but use the above estimated
  #       shadow prices to obtain the additive TFP index with shadow prices
  #       as weights. 
  

    ADEA_iap <- lowe(data = IAPdata, id.var = "country", time.var = "year", 
                           x.vars = c("x_land", "x_labor","x_capital","x_fertilizer", "x_feed"), 
                           y.vars = c("q_crops", "q_animals"),
                           w.vars = c("w0_land", "w0_labor","w0_capital","w0_fertilizer", "w0_feed"), 
                           p.vars = c("p0_crops", "p0_animals"), 
                           tech.change = T, tech.reg = T, rts = "crs", 
                           orientation = "out", cores = 8, scaled = F, 
                           by.year = 1) #note: should we use windows??
      
    ADEA.level_crs <- ADEA_iap$Levels
    
    # Normalize with Base = AUT 1961
    ADEA.level_crs_norm <- ADEA.level_crs %>% 
      mutate_each(funs(./.[1]), setdiff(names(.), c("country","year")))

    # Mean productivity by year
    all <- ADEA.level_crs %>%
      group_by(year) %>%
      dplyr::summarize(Mean = mean(TFP, na.rm=TRUE))
    


# ----------------------------------- #
#### Create figure for the results ####
# ----------------------------------- #
    
  # ---------------------------------------------------------------- #
  ##### Panel A: TFP changes in selected EU countries and the US #####
  # ---------------------------------------------------------------- #

    # Prepare data for the plot
    data_plot <- ADEA.level_crs_norm %>% 
      filter(country=="FRA" | country=="ESP"| country=="ROU" | country=="SWE" | country=="USA") %>% 
      select("year", "country", "TFP")
    
    
    # plot
    panel_A <- ggplot() + 
       geom_line(data = data_plot, aes(x = year, y = TFP, color = country, linetype = country)
       ) + labs(
          x = "",
          y = "",
          linetype = "") + 
       scale_linetype_manual(name  ="",
                             breaks=c("FRA", "ESP", "ROU", "SWE", "USA"),
                             labels=c("France", "Spain", "Romania", "Sweden", "USA"),
                             values=c("solid", "42", "1141", "solid", "12")) +
       scale_color_manual(name  ="",
                         breaks=c("FRA", "ESP", "ROU", "SWE", "USA"),
                         labels=c("France", "Spain", "Romania", "Sweden", "USA"),
                         values=c("#0a0a0a","#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) +
       theme_bw() +
       scale_x_continuous(breaks = seq(1961, 2020, by = 3)) +
       scale_y_continuous(breaks = seq(0, 2.0, by = 0.5), limits=c(0,2.2)) +  
       theme(axis.text.x = element_text(angle=90)) + 
       theme(legend.position="bottom",
             legend.margin=margin(t = -0.7, unit='cm'))

  # -------------------------------------------------------------------------- #
  ##### Panel B: omparison of TFP changes between the US and the entire EU #####
  # -------------------------------------------------------------------------- #

      # Create EU-average

         # Compare every level to the country's *own* 1961 level
         idcountries <- unique(ADEA.level_crs$country)
         ADEA.cumulative_crs <- list()
         for (i in idcountries) {
            ADEA.cumulative_crs[[which(i==idcountries)]] <- cbind(country = i, year=1961:2020,
                                                            sweep(ADEA.level_crs[ADEA.level_crs$country==i,-c(1:2)],MARGIN = 2,
                                                                  STATS = as.numeric(ADEA.level_crs[ADEA.level_crs$country==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
         }
         ADEA.cumulative_crs <- do.call(rbind, ADEA.cumulative_crs) #turns the list into a data frame
         
         # Calculate the EU average using the geometric mean
         ADEA.cumulative_crs_EU <- ADEA.cumulative_crs %>% 
            filter(country!="USA")
         
         ADEA.EU_crs <- list()
         for (yr in 1961:2020) {
            ADEA.EU_crs[[which(yr == 1961:2020)]] <- c(year = yr, apply(ADEA.cumulative_crs_EU[ADEA.cumulative_crs_EU$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
         }
         ADEA.EU_crs <- as.data.frame(do.call(rbind, ADEA.EU_crs)) #turns the list into a data frame
         
         
         # Note: For the efficiencies (last figure), I need arithmetic averages
         ADEA.EU_crs_eff <- ADEA.level_crs[,c("country", "year","TFPE", "OTE", "OME", "ROSE", "OSME")]
         
         ADEA.EU_crs_eff <- ADEA.EU_crs_eff %>% 
            group_by(year) %>% 
            summarise_at(.vars = vars(TFPE,OTE,OME,ROSE,OSME),
                         .funs = c(mean="mean"))
         

      # Bind EU and US results
         
         ADEA.EU_crs$country="EU21"   
         ADEA.USA_crs <- ADEA.cumulative_crs %>% 
            filter(country=="USA")
      
         ADEA.USAvsEU <- rbind(ADEA.USA_crs,ADEA.EU_crs)


      # Plot TFP

         panel_B <- ggplot() + 
            geom_line(data = ADEA.USAvsEU, aes(x = year, y = TFP, linetype = country)
            ) + labs(
               x = "",
               y = "",
               linetype = ""
            ) + scale_linetype_manual(name  ="",
                                      breaks=c("EU21", "USA"),
                                      labels=c("EU21", "USA"),
                                      values=c("solid", 42)) +
            theme_bw() +
            #ggtitle("A") +
            scale_x_continuous(breaks = seq(1961, 2020, by = 3)) +
            scale_y_continuous(breaks = seq(0, 2.0, by = 0.5), limits=c(0,2.2)) +  
            theme(axis.text.x = element_text(angle=90)) + 
            theme(legend.position="bottom",
                  legend.margin=margin(t = -0.7, unit='cm'))

         
# -------------------------------------- #
##### Print both plots in one figure #####
# -------------------------------------- #
         
         tikz(file = "Figures/Fig_ADEA_IAP.tex", width = 5.5, height = 5.5)
         plot <- ggarrange(panel_A, panel_B,
                           labels = c("a", "b"),
                           ncol = 1, nrow = 2)
         print(plot)
         dev.off()
         
         
         
# ---------------------------------- #
#### Create table for the results ####
# ---------------------------------- #
         
    # Load Global Malmquist results from file "05d_TFP for WORLD_GlobMalm for comparison" 
         
      load("R_output/GlobMalm.levels_IAP.Rda")
         
   # Prepare data
               
      data_table <- data.frame(matrix(ncol = 7, nrow = 25))
      colnames(data_table) <- c("country", "TFP1961.adea", "TFP2020.adea", "DTFP.adea",
                                            "TFP1961.globmalm", "TFP2020.globmalm", "DTFP.globmalm") 

      data_table$country <- (unique(IAPdata$country))
      

      # TOTAL FACTOR PRODUCTIVITY: A-DEA
         
        # TFP1961
         data_table$TFP1961.adea <- ADEA.level_crs$TFP[ADEA.level_crs$year==1961]
         # TFP2016
         data_table$TFP2020.adea <- ADEA.level_crs$TFP[ADEA.level_crs$year==2020]
         # DTFP
         data_table$DTFP.adea <- (data_table$TFP2020.adea / data_table$TFP1961.adea)
     
      # TOTAL FACTOR PRODUCTIVITY: Global Malmquist
         
         # TFP1961
         data_table$TFP1961.globmalm <- GlobMalm.levels$DOGt[GlobMalm.levels$year==1961]
         # TFP2016
         data_table$TFP2020.globmalm <- GlobMalm.levels$DOGt[GlobMalm.levels$year==2020]
         # DTFP
         data_table$DTFP.globmalm <- (data_table$TFP2020.globmalm / data_table$TFP1961.globmalm)    
        
      
      # Add EU25-average as geometric mean 
          
          data_table <- data_table %>% 
          add_row(country="EU aggregated",
                  TFP1961.adea=NA,
                  TFP2020.adea=NA,
                  DTFP.adea=exp(mean(log(data_table$DTFP.adea[data_table$country!="USA"]))),
                  TFP1961.globmalm=NA,
                  TFP2020.globmalm=NA,
                  DTFP.globmalm=exp(mean(log(data_table$DTFP.globmalm[data_table$country!="USA"]))))
      
      
   # Print to latex 
      
      # Set global option to produce latex output
      options(knitr.table.format = "latex",
              knitr.kable.NA = '')

      # Create table
      Tab_ADEA_vs_GlobMalm_IAP <- kable(data_table, booktabs = T,
                        digits = 2,
                        row.names = FALSE,
                        escape = FALSE, 
                        linesep = "",
                        caption = "TFP in EU and US agriculture using the A-DEA index and the global Malmquist index and the International Agricultural Productivity data (1961--2020).",
                        label = "Tab_ADEA_vs_GlobMalm_IAP",
                        col.names = c('Region', 
                                    '1961', '2020', "$\\Delta$",
                                    '1961', '2020', "$\\Delta$")) %>%
         add_header_above(c("", "A-DEA TFP" = 3, "Global Malmquist TFP" = 3),
                          escape = FALSE) %>%
         row_spec(25, hline_after=T) %>% 
        footnote(general = "TFP is total factor productivity.",
                 footnote_as_chunk = T,
                 threeparttable = T,
                 general_title = "Note:",
                 escape=F) %>%
         kable_styling(latex_options = c("HOLD_position")) #note: this table looks much nicer without "scale_down"
      
      # Print Latex file
      writeLines(Tab_ADEA_vs_GlobMalm_IAP, "Tables/Tab_ADEA_vs_GlobMalm_IAP.tex")
         