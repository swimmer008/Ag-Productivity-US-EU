# --------------------------------------- #
#                                         #
# This program estimates the additive TFP #
# index under CRS using shadow prices as  #
# weights for the EU country-level data   #
#                                         #
# --------------------------------------- #

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
source('R_aux/aux_lowe.R')
source('R_aux/auxiliaries.R')
source('R_aux/lowe.R')

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')
  
# Load the EU country-level data
load("R_output/EUdata.Rda")

# Order data by country
EUdata <- EUdata[order(EUdata$country),]

# ------------------------------------ #
#### Estimation of reference prices #### 
# ------------------------------------ #

# Estimation of each observations' shadow prices
  
  # Input and output vectors
  XREF <- t(as.matrix(EUdata[, c("x_capital", "x_land","x_labor","x_interm")]))
  YREF <- t(as.matrix(EUdata[, c("q_animals", "q_crops", "q_other")]))
  
  # Output distance function
  outShad <- foreach(dmu = 1:dim(EUdata)[1], .combine = rbind) %do%
    {
      DO.shdu(XOBS = XREF[, dmu], YOBS = YREF[, dmu], XREF = XREF,
              YREF = YREF, rts = "crs")
    }
  
  # Input distance function
  inpShad <- foreach(dmu = 1:dim(EUdata)[1], .combine = rbind) %do%
    {
      DI.shdu(XOBS = XREF[, dmu], YOBS = YREF[, dmu], XREF = XREF,
              YREF = YREF, rts = "crs")
    }

# Use mean shadow prices as reference prices 
  
  # Compute means
  meanY <- apply(outShad, 2, FUN = function(x) mean(x))
  meanX <- apply(inpShad, 2, FUN = function(x) mean(x))
  
  # Add reference prices to data frame
  EUdata$p0_animals <- meanY[1] 
  EUdata$p0_crops <- meanY[2]
  EUdata$p0_other <- meanY[3] 
  
  EUdata$w0_capital <- meanX[1]
  EUdata$w0_land <- meanX[2]
  EUdata$w0_labor <- meanX[3]
  EUdata$w0_interm <- meanX[4]

# -------------------------------------------- #
#### Calculate and decompose TFP under CRS  #### 
# -------------------------------------------- #
  
  # Subsets for each region

      Cluster1 <- subset(EUdata, country == "BEL" | 
                          country == "FRA" |
                          country == "GBR"|
                          country == "IRL"|
                          country == "NLD")
      Cluster2 <- subset(EUdata,country == "EST" | 
                           country == "FIN" |
                           country == "LTU" |
                           country == "LVA" |
                           country == "SWE")
      Cluster3 <- subset(EUdata, country == "AUT" | 
                          country == "CZE" |
                          country == "DEU" |
                          country == "DNK" |
                           country == "HUN" |
                           country == "LUX" |
                           country == "POL" |
                           country == "ROU" |
                           country == "SVN" |
                           country == "SVK")
      Cluster4 <- subset(EUdata, country == "ESP" | 
                          country == "GRC" |
                          country == "ITA" |
                          country == "MLT" |
                          country == "PRT")

    # Note: We use the code from the Lowe index but use the above estimated
    #       shadow prices to obtain the additive TFP index with shadow prices
    #       as weights.
      
      #Cluster1
      ADEA_cluster1 <- lowe(data = Cluster1, id.var = "country", time.var = "year", 
                           x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                           y.vars = c("q_animals", "q_crops", "q_other"),
                           w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                           p.vars = c("p0_animals", "p0_crops", "p0_other"), 
                           tech.change = T, tech.reg = T, rts = "crs", 
                           orientation = "out", cores = 8, scaled = F, 
                           window = c(3), by.year = 1)
      
      #Cluster2
      ADEA_cluster2 <- lowe(data = Cluster2, id.var = "country", time.var = "year", 
                            x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                            y.vars = c("q_animals", "q_crops", "q_other"),
                            w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                            p.vars = c("p0_animals", "p0_crops", "p0_other"), 
                            tech.change = T, tech.reg = T, rts = "crs", 
                            orientation = "out", cores = 8, scaled = F, 
                            window = c(3), by.year = 1)
      
      #Cluster3
      ADEA_cluster3 <- lowe(data = Cluster3, id.var = "country", time.var = "year", 
                           x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                           y.vars = c("q_animals", "q_crops", "q_other"),
                           w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                           p.vars = c("p0_animals", "p0_crops", "p0_other"), 
                           tech.change = T, tech.reg = T, rts = "crs", 
                           orientation = "out", cores = 8, scaled = F, 
                           window = c(3), by.year = 1)
      
      #Cluster4
      ADEA_cluster4 <- lowe(data = Cluster4, id.var = "country", time.var = "year", 
                           x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                           y.vars = c("q_animals", "q_crops", "q_other"),
                           w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                           p.vars = c("p0_animals", "p0_crops", "p0_other"), 
                           tech.change = T, tech.reg = T, rts = "crs", 
                           orientation = "out", cores = 8, scaled = F, 
                           window = c(3), by.year = 1)
      
   
# Join the results
      
      ADEA.level_crs <- rbind(ADEA_cluster1$Levels, ADEA_cluster2$Levels,
                              ADEA_cluster3$Levels, ADEA_cluster4$Levels)
      
      # Order results by country
      ADEA.level_crs <- ADEA.level_crs[order(ADEA.level_crs$country),]
      
      # Normalize with Base = AUT 2000
      ADEA.level_crs_norm <- ADEA.level_crs %>% 
        mutate_each(funs(./.[1]), setdiff(names(.), c("country","year")))
      
# ---------------------------------------------------- #
#### Replicate tables 2 and 3 from O'Donnell (2012) ####
# ---------------------------------------------------- #  
      
      
# Prepare data for the tables
      
  data_tables <- data.frame(matrix(ncol = 22, nrow = 25))
  colnames(data_tables) <- c("country", "TFP2000", "TFP2019", "DTFP",
                             "MP2000", "MP2019", "DMP",
                             "TFPE2000", "TFPE2019", "DTFPE",
                             "OTE2000", "OTE2019", "DOTE",
                             "OSE2000", "OSE2019", "DOSE",
                             "OME2000", "OME2019", "DOME",
                             "OSME2000", "OSME2019", "DOSME")
  
  data_tables$country <- (unique(EUdata$country))
      
  
# TOTAL FACTOR PRODUCTIVITY 
      
  # TFP2000
  data_tables$TFP2000 <- ADEA.level_crs$TFP[ADEA.level_crs$year==2000]
  # TFP2019
  data_tables$TFP2019 <- ADEA.level_crs$TFP[ADEA.level_crs$year==2019]
  # DTFP
  data_tables$DTFP <- (data_tables$TFP2019 / data_tables$TFP2000)
      
# MAXIMM TOTAL FACTOR PRODUCTIVITY 
      
  # MP2000
  data_tables$MP2000 <- ADEA.level_crs$MP[ADEA.level_crs$year==2000]
  # MP2019
  data_tables$MP2019 <- ADEA.level_crs$MP[ADEA.level_crs$year==2019]
  # DMP
  data_tables$DMP <- (data_tables$MP2019 / data_tables$MP2000)
      
# TFP EFFICIENCY 
    
  # TFPE2000
  data_tables$TFPE2000 <- ADEA.level_crs$TFPE[ADEA.level_crs$year==2000]
  # TFPE2019
  data_tables$TFPE2019 <- ADEA.level_crs$TFPE[ADEA.level_crs$year==2019]
  # DTFPE
  data_tables$DTFPE <- (data_tables$TFPE2019 / data_tables$TFPE2000)
      
# OUTPUT TECHNICAL EFFICIENCY 
      
  # OTE2000
  data_tables$OTE2000 <- ADEA.level_crs$OTE[ADEA.level_crs$year==2000]
  # OTE2019
  data_tables$OTE2019 <- ADEA.level_crs$OTE[ADEA.level_crs$year==2019]
  # DOTE
  data_tables$DOTE <- (data_tables$OTE2019 / data_tables$OTE2000)
  
# OUTPUT SCALE EFFICIENCY 
      
  # OSE2000
  data_tables$OSE2000 <- ADEA.level_crs$OSE[ADEA.level_crs$year==2000]
  # OSE2019
  data_tables$OSE2019 <- ADEA.level_crs$OSE[ADEA.level_crs$year==2019]
  # DOSE
  data_tables$DOSE <- (data_tables$OSE2019 / data_tables$OSE2000)
      
# OUTPUT MIX EFFICIENCY 
      
  # OME2000
  data_tables$OME2000 <- ADEA.level_crs$OME[ADEA.level_crs$year==2000]
  # OME2019
  data_tables$OME2019 <- ADEA.level_crs$OME[ADEA.level_crs$year==2019]
  # DOME
  data_tables$DOME <- (data_tables$OME2019 / data_tables$OME2000)
      
# OUTPUT SCALE MIX EFFICIENCY 
      
  # OSME2000
  data_tables$OSME2000 <- ADEA.level_crs$OSME[ADEA.level_crs$year==2000]
  # OSME2019
  data_tables$OSME2019 <- ADEA.level_crs$OSME[ADEA.level_crs$year==2019]
  # DOSME
  data_tables$DOSME <- (data_tables$OSME2019 / data_tables$OSME2000)  

# Add EU25-average (geometric, because we have not used logs)
  data_tables <- data_tables %>% 
  add_row(country="EU25", 
          TFP2000=NA, TFP2019=NA, DTFP=exp(mean(log(data_tables$DTFP))),
          MP2000=NA, MP2019=NA, DMP=exp(mean(log(data_tables$DMP))),
          TFPE2000=NA, TFPE2019=NA, DTFPE=exp(mean(log(data_tables$DTFPE))),
          OTE2000=NA, OTE2019=NA, DOTE=exp(mean(log(data_tables$DOTE))),
          OSE2000=NA, OSE2019=NA, DOSE=exp(mean(log(data_tables$DOSE))),
          OME2000=NA, OME2019=NA, DOME=exp(mean(log(data_tables$DOME))),
          OSME2000=NA, OSME2019=NA, DOSME=exp(mean(log(data_tables$DOSME))))


# Write Table: "TFP, technical change, and efficiency change in EU agriculture (2000--2019) using the A-DEA index)

  # Set global option to produce latex output
  options(knitr.table.format = "latex", knitr.kable.NA = '')

  # Select data
  data_ADEA_TFPDecomp <- subset(data_tables, select = c(1:10))
  
  # Write table  
  Tab_ADEA_TFPDecomp_EU_crs <- kable(data_ADEA_TFPDecomp, booktabs = T,
                     digits = 2,
                     row.names = FALSE,
                     escape = FALSE, 
                     linesep = "",
                     caption = "TFP, technical change, and efficiency change in EU agriculture (2000--2019) using the A-DEA index under CRS.",
                     label = "Tab_ADEA_TFPDecomp_EU_crs",
                     col.names = c('Country', 
                                   '2000', '2019', "$\\Delta$",
                                   '2000', '2019', "$\\Delta$",
                                   '2000', '2019', "$\\Delta$")) %>%
    add_header_above(c("", "TFP" = 3, "TFP* (=OET)" = 3,
                       "TFPE" = 3)) %>%
    row_spec(25, hline_after=T) %>% 
    footnote(general = "TFP is total factor productivity, TFP* is the maximum \\\\\\\\ 
                        possible TFP based on the output-oriented  
                        environment and technology (OET) index, and TFPE is TFP efficiency.",
             footnote_as_chunk = T,
             threeparttable = T,
             general_title = "Notes:",
             escape=F) %>%
    kable_styling(latex_options = c("HOLD_position")) #note: this table looks much nicer without "scale_down"
  
  # Print Latex file
  writeLines(Tab_ADEA_TFPDecomp_EU_crs, "Tables/Tab_ADEA_TFPDecomp_EU_crs.tex")


# Write Table: "Output-oriented components of efficiency change in EU agriculture (2000--2019) using the A-DEA index"
  
  # Set global option to produce latex output
  options(knitr.table.format = "latex", knitr.kable.NA = '')
  
  # Select data
  data_ADEA_TFPEDecomp <- subset(data_tables, select = c(1,8:22))

  # Write table  
  Tab_ADEA_TFPEDecomp_EU_crs <- kable(data_ADEA_TFPEDecomp, booktabs = T, 
                     digits = 2,
                     row.names = FALSE,
                     escape = FALSE, 
                     linesep = "",
                     caption = "Output-oriented components of efficiency change in EU agriculture (2000--2019) using the A-DEA index under CRS.",
                     label = "Tab_ADEA_TFPEDecomp_EU_crs",
                     col.names = c('Country', 
                                   '2000', '2019', "$\\Delta$",
                                   '2000', '2019', "$\\Delta$",
                                   '2000', '2019', "$\\Delta$",
                                   '2000', '2019', "$\\Delta$",
                                   '2000', '2019', "$\\Delta$")) %>%
    add_header_above(c("", "TFPE=OTE$\\\\times$OSME" = 3, "OTE" = 3, 
                       "OSE" = 3, "OME" = 3,
                       "OSME" = 3),
                     escape=F) %>%
    row_spec(25, hline_after=T) %>% 
    footnote(general = "TFPE is TFP efficiency, OTE is output-oriented technical efficiency, OSE is output-oriented scale efficiency, 
                        OME is output-oriented mix efficiency, and OSME is output-oriented scale-mix efficiency.",
             footnote_as_chunk = T,
             threeparttable = T,
             general_title = "Notes:",
             escape=F) %>%
    #kable_styling(latex_options = c("HOLD_position")) %>% #note: this table looks much nicer without "scale_down"
    landscape()

# Print Latex file
writeLines(Tab_ADEA_TFPEDecomp_EU_crs, "Tables/Tab_ADEA_TFPEDecomp_EU_crs.tex")


# ------------------------------------- #
#### Create figures for EU aggregate ####
# ------------------------------------- #
  
  # Prepare the data

    # Compare every level to the country's *own* 2000 level --> obtains cumulative growth 
    idcountries <- unique(ADEA.level_crs$country)
    ADEA.cumulative_crs <- list()
    for (i in idcountries) {
      ADEA.cumulative_crs[[which(i==idcountries)]] <- cbind(country = i, year=2000:2019,
                                                         sweep(ADEA.level_crs[ADEA.level_crs$country==i,-c(1:2)],MARGIN = 2,
                                                               STATS = as.numeric(ADEA.level_crs[ADEA.level_crs$country==i,-c(1:2)][1,]),FUN = "/")) # for myself: 2 refers to column
    }
    ADEA.cumulative_crs <- do.call(rbind, ADEA.cumulative_crs) #turns the list into a data frame
    
    # Calculate US average using the geometric mean
    ADEA.EU25_crs <- list()
    for (yr in 2000:2019) {
      ADEA.EU25_crs[[which(yr == 2000:2019)]] <- c(year = yr, apply(ADEA.cumulative_crs[ADEA.cumulative_crs$year==yr,-c(1:2)],2,FUN = function(x) exp(mean(log(x)))))
    }
    ADEA.EU25_crs <- as.data.frame(do.call(rbind, ADEA.EU25_crs)) #turns the list into a data frame
    
    
    # For the efficiencies (last figure), we need arithmetic averages
    ADEA.EU25_crs_eff <- ADEA.level_crs[,c("country", "year","TFPE", "OTE", "OME", "OSME")]
    ADEA.EU25_crs_eff <- ADEA.EU25_crs_eff %>% 
      group_by(year) %>% 
      summarise_at(.vars = vars(TFPE,OTE,OME,OSME),
                   .funs = c(mean="mean"))


# --------------------------------- #
# Panel A: Components of TFP change #
# --------------------------------- #

    # Note: We do not plot profitability change due to the lack of consistent prices
  
  # Prepare data for the plot
  data_plot <- ADEA.EU25_crs %>% 
    select("year", "TFP", "MP", "TFPE") %>% 
    reshape2::melt(id.vars="year")
    
  # plot
  panel_A <- ggplot() + 
    geom_line(data = data_plot, aes(x = year, y = value, linetype = variable)
    ) + labs(
      x = "",
      y = "",
      linetype = ""
    ) + scale_linetype_manual(name  ="",
                              breaks=c("TFP", "MP", "TFPE"),
                              labels=c("?TFP", "?TFP*", "?TFPE"),
                              values=c("solid", "12", "42")) +
    theme_bw() +
    scale_x_continuous(breaks = seq(2000, 2019, by = 2)) +
    scale_y_continuous(breaks = seq(0, 1.5, by = 0.25), limits=c(0,1.6)) +  
    theme(axis.text.x = element_text(angle=90)) +
    theme(legend.position="bottom",
          legend.margin=margin(t = -0.7, unit='cm'),
          legend.text=element_text(size=6))


# -------------------------- #
# Panel B: Efficiency levels #
# -------------------------- #

  # Prepare data for the plot
  data_plot <- ADEA.EU25_crs_eff %>% 
    reshape2::melt(id.vars="year")
  
  # plot
  panel_B <- ggplot() + 
    geom_line(data = data_plot, aes(x = year, y = value, color = variable, linetype = variable)
    ) + labs(
      x = "",
      y = "",
      linetype = "") + 
    scale_color_manual(name  ="",
                       breaks=c("OTE_mean", "OME_mean", "OSME_mean", "TFPE_mean"),
                       labels=c("OTE", "OME", "OSME", "TFPE"),
                       values=c("#0a0a0a","#0a0a0a","#9E9E9E","#0a0a0a")) + 
    scale_linetype_manual(name = "",
                            breaks=c("OTE_mean", "OME_mean", "OSME_mean", "TFPE_mean"),
                            labels=c("OTE", "OME", "OSME", "TFPE"),
                            values=c("solid", "42", "solid", "12")) +
    theme_bw() +
    #ggtitle("C") +
    scale_x_continuous(breaks = seq(2000, 2019, by = 2)) +
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
  tikz(file = "Figures/Fig_ADEA_EU25_crs.tex", width = 7, height = 3.5, sanitize=TRUE)
  plot <- ggarrange(panel_A, panel_B,
                    labels = c("a", "b"),
                    ncol = 2, nrow = 1)
  print(plot)
  dev.off()
