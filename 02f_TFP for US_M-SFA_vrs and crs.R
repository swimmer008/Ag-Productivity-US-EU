# --------------------------------------- #
#                                         #
# This program estimates the stochastic   #
# multiplicative TFP index for the state- # 
# level US data  index for the US, under  # 
# both VRS and CRS                        #
#                                         #
# --------------------------------------- #

# Open packages

library(fastDummies) #to greate dummy variables
library(dplyr) #for data manipulation
library(msm) #for delta method

library(ggplot2) #to create plots
library(ggthemes) #to format plots
library(gridExtra) #to arrange plots
library(reshape2) #to reshape the data

library(tikzDevice) #to save ggplot object in latex format
library(ggpubr) ## for ggarrange
library(kableExtra) # to convert data frames to Latex

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')
  
#-----------------------#
#### 0) Preparations ####
#-----------------------#

  # load US production data
  load("R_output/USdata.Rda")

  # Create trend variable
  trend <- data.frame(matrix(ncol = 2, nrow = length(unique(USdata$year))))
  colnames(trend) <- c("trend","year")
  trend$trend <- seq(from = 1, to = length(unique(USdata$year)))
  trend$year <- seq(from = min(USdata$year), to = max(USdata$year))
  USdata <- right_join(USdata, trend)
  USdata$trend <- as.numeric(USdata$trend)
  
  # For semi-decade specific time trend
  USdata <- USdata %>% 
    mutate(semidecade = case_when(
      year >= 1960 & year < 1965 ~ "sdec60s",
      year >= 1965 & year < 1970 ~ "sdec65s",
      year >= 1970 & year < 1975 ~ "sdec70s",
      year >= 1975 & year < 1980 ~ "sdec75s",
      year >= 1980 & year < 1985 ~ "sdec80s",
      year >= 1985 & year < 1990 ~ "sdec85s",
      year >= 1990 & year < 1995 ~ "sdec90s",
      year >= 1995 & year < 2000 ~ "sdec95s",
      year >= 2000               ~ "sdec00s")
    )
  
  USdata <- USdata %>% 
    dummy_cols(select_columns = "semidecade",remove_first_dummy=FALSE)
  

  # State dummy variables
    USdata <- USdata %>% 
      dummy_cols(select_columns = "state",remove_first_dummy=TRUE) 

  # Regional variables
    
    USdata$region[USdata$state == "CA"| 
                    USdata$state == "OR" |
                    USdata$state == "WA"] <- "Pacific" 
    
    USdata$region[USdata$state == "AZ" | 
                    USdata$state == "CO" |
                    USdata$state == "ID" |
                    USdata$state == "MT" |
                    USdata$state == "NM" |
                    USdata$state == "NV" |
                    USdata$state == "UT" |
                    USdata$state == "WY"] <- "Mountain" 
    
    USdata$region[USdata$state == "KS" | 
                    USdata$state == "ND" |
                    USdata$state == "NE" |
                    USdata$state == "SD"] <- "NPlains" 
    
    USdata$region[USdata$state == "OK" | 
                    USdata$state == "TX"] <- "SPlains"  
    
    USdata$region[USdata$state == "IA" | 
                    USdata$state == "IL" |
                    USdata$state == "IN" |
                    USdata$state == "MO" |
                    USdata$state == "OH"] <- "Cornbelt"     
    
    USdata$region[USdata$state == "AL" | 
                    USdata$state == "FL" |
                    USdata$state == "GA" |
                    USdata$state == "SC"] <- "Southeast" 
    
    USdata$region[USdata$state == "CT" | 
                    USdata$state == "DE" |
                    USdata$state == "MA" |
                    USdata$state == "MD" |
                    USdata$state == "ME" |
                    USdata$state == "NH" |
                    USdata$state == "NJ" |
                    USdata$state == "NY"|
                    USdata$state == "PA" |
                    USdata$state == "RI" |
                    USdata$state == "VT"] <- "Northeast"     
    
    USdata$region[USdata$state == "MI" | 
                    USdata$state == "MN" |
                    USdata$state == "WI"] <- "Lake"      
    
    USdata$region[USdata$state == "KY" | 
                    USdata$state == "NC" |
                    USdata$state == "TN"| 
                    USdata$state == "VA" |
                    USdata$state == "WV"] <- "Appalacian"       
    
    USdata$region[USdata$state == "AR" | 
                    USdata$state == "LA" |
                    USdata$state == "MS"] <- "Delta"       
 
# ------------------------------------ #
#### 1) Compute TFP index under VRS ####
# ------------------------------------ #   
    
  # ------------------------------- #
  ##### 1a) Frontier estimation #####
  # ------------------------------- #   
    
    sfa_string <- as.formula(paste0(c("-log(q_crops)", 
                                          paste0(c("log(q_livestock/q_crops)",
                                                   "log(q_otheroutp/q_crops)", 
                                                   "log(x_capital)", "log(x_land)", 
                                                   "log(x_labor)", "log(x_interm)", 
                                                   "factor(state)",
                                                   "trend:factor(region):factor(semidecade)"), 
                                                 collapse = " + ")),
                                        collapse = " ~ "))
    
    model_sfa <- sfaR::sfacross(sfa_string, S=-1, data = USdata)
    summary(model_sfa)
    
    # Estimate model as OLS to find noise component below
    model_ols <- lm(sfa_string, data = USdata)
    summary(model_ols)
    
  # ------------------------------- #
  ##### 1b) Decompose TFP index #####
  # ------------------------------- #

    # Scale elasticity
    scale_elas <- -model_sfa$mlParam["log(x_capital)"] + -model_sfa$mlParam["log(x_land)"] + 
      -model_sfa$mlParam["log(x_labor)"] + -model_sfa$mlParam["log(x_interm)"]
    
    # Aggregate output
    USdata$Qit <- USdata$q_livestock^(model_sfa$mlParam["log(q_livestock/q_crops)"]) * 
      USdata$q_otheroutp^(model_sfa$mlParam["log(q_otheroutp/q_crops)"]) * 
      USdata$q_crops^(1 - model_sfa$mlParam["log(q_livestock/q_crops)"] - model_sfa$mlParam["log(q_otheroutp/q_crops)"])
    
    # Aggregate input
    USdata$Xit <- USdata$x_capital^(-model_sfa$mlParam["log(x_capital)"]/scale_elas) * 
      USdata$x_land^(-model_sfa$mlParam["log(x_land)"]/scale_elas) * 
      USdata$x_labor^(-model_sfa$mlParam["log(x_labor)"]/scale_elas) * 
      USdata$x_interm^(-model_sfa$mlParam["log(x_interm)"]/scale_elas)
    
    # Total factor productivity
    USdata$TFPit <- USdata$Qit/USdata$Xit 
    
    # Output technical efficiency
    USdata$OTEit <- sfaR::efficiencies(model_sfa)[, "teJLMS"]  
    
    # Noise component
    USdata$NCit <- exp(log(USdata$q_crops) - as.numeric(crossprod(matrix(-model_sfa$mlParam[1:(length(model_sfa$mlParam) - 2)]),
                                                             t(model.matrix(model_ols)))) + sfaR::efficiencies(model_sfa)[, "u"]) # noise component
    
    # Output scale component
    USdata$OSEit <- USdata$x_capital^(-model_sfa$mlParam["log(x_capital)"] * (1 - 1/scale_elas)) * 
      USdata$x_land^(-model_sfa$mlParam["log(x_land)"] * (1 - 1/scale_elas)) * 
      USdata$x_labor^(-model_sfa$mlParam["log(x_labor)"] * (1 - 1/scale_elas)) * 
      USdata$x_interm^(-model_sfa$mlParam["log(x_interm)"] * (1 - 1/scale_elas))  
    
    # Technical change component
    USdata$TCit <- exp(apply(sweep(model.matrix(~factor(state),data=USdata),MARGIN=2, STATS=c(-model_sfa$mlParam[1],-model_sfa$mlParam[stringr::str_detect(names(model_sfa$mlParam),
                                                                                                                             "state")]), FUN="*"),1, sum) + 
                       apply(sweep(model.matrix(~-1 + trend:factor(region):factor(semidecade), data = USdata), MARGIN = 2, STATS = -model_sfa$mlParam[stringr::str_detect(names(model_sfa$mlParam),
                        "trend")], FUN = "*"), 1, sum))  ## technical change component
    
    # Summarize the results
    summary(USdata[c("OTEit", "NCit", "OSEit", "TCit")])
    

  # Subset data to only contain the index components
      MSFA.level_vrs <- USdata %>% 
        dplyr::select(year,state,Qit,Xit,TFPit,TCit,OTEit,OSEit,NCit)
  
  # Check if they add up
      MSFA.level_vrs$TFPcomp <- MSFA.level_vrs$TCit * MSFA.level_vrs$OTEit * MSFA.level_vrs$OSEit * MSFA.level_vrs$NCit
      summary(MSFA.level_vrs$TFPit-MSFA.level_vrs$TFPcomp)
      
  # Save results for comparison in "03h_TFP for US_comparison"
      save(MSFA.level_vrs, file="R_output/MSFA.level_vrs.Rda")

# --------------------------------- #
##### 1c) Create results tables #####
# --------------------------------- #
    
    # ----------------------------------------- #
    # Table to summarize the regression results #
    # ----------------------------------------- #
    
    df.sfa_vrs <- data.frame(matrix(ncol = 3, nrow = 11))
    x <- c("Variable", "Estimate", "Standard Error")
    colnames(df.sfa_vrs) <- x
    
    df.sfa_vrs$Variable <- c("ln(livestock/Crops)",
                          "ln(Other output/Crops)",
                          "ln(Capital)",
                          "ln(Land)",
                          "ln(Labor)",
                          "ln(Intermediate inputs)",
                          "Binary variables for states",
                          "Regional- and semidecade- \\\\ specific time trends",
                          "$\\delta$",
                          "$\\phi$",
                          "Average efficiency")
    
    estimates <- model_sfa$mlParam 
    stderrs <- sqrt(diag(model_sfa$invHessian))
    zvals <- estimates/stderrs
    pvals <- 2*pnorm(q=abs(zvals), lower.tail=FALSE)
    
    
    df.sfa_vrs[1,2] <- paste0(format(round(model_sfa$mlParam["log(q_livestock/q_crops)"],3),nsmall=3),
                           ifelse(pvals[2]<0.01,"***",
                                  ifelse(pvals[2]<0.05,"**",
                                         ifelse(pvals[2]<0.1,"*",""))))
    df.sfa_vrs[1,3] <- paste0("(",format(round(stderrs[2],3),nsmall=3),")")
    
    df.sfa_vrs[2,2] <- paste0(format(round(model_sfa$mlParam["log(q_otheroutp/q_crops)"],3),nsmall=3),
                           ifelse(pvals[3]<0.01,"***",
                                  ifelse(pvals[3]<0.05,"**",
                                         ifelse(pvals[3]<0.1,"*",""))))
    df.sfa_vrs[2,3] <- paste0("(",format(round(stderrs[3],3),nsmall=3),")")
    
    df.sfa_vrs[3,2] <- paste0(format(round(model_sfa$mlParam["log(x_capital)"],3),nsmall=3),
                           ifelse(pvals[4]<0.01,"***",
                                  ifelse(pvals[4]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_vrs[3,3] <- paste0("(",format(round(stderrs[4],3),nsmall=3),")")
    
    df.sfa_vrs[4,2] <- paste0(format(round(model_sfa$mlParam["log(x_land)"],3),nsmall=3),
                           ifelse(pvals[5]<0.01,"***",
                                  ifelse(pvals[5]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_vrs[4,3] <- paste0("(",format(round(stderrs[5],3),nsmall=3),")")
    
    df.sfa_vrs[5,2] <- paste0(format(round(model_sfa$mlParam["log(x_labor)"],3),nsmall=3),
                           ifelse(pvals[6]<0.01,"***",
                                  ifelse(pvals[6]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_vrs[5,3] <- paste0("(",format(round(stderrs[6],3),nsmall=3),")")
    
    df.sfa_vrs[6,2] <- paste0(format(round(model_sfa$mlParam["log(x_interm)"],3),nsmall=3),
                           ifelse(pvals[7]<0.01,"***",
                                  ifelse(pvals[7]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_vrs[6,3] <- paste0("(",format(round(stderrs[7],3),nsmall=3),")")
    
    df.sfa_vrs[7,2] <- "not reported"
    df.sfa_vrs[7,3] <- ""
    df.sfa_vrs[8,2] <- "not reported"
    df.sfa_vrs[8,3] <- ""
    
    df.sfa_vrs[9,2] <- paste0(format(round(model_sfa$mlParam["Zu_(Intercept)"],3),nsmall=3),
                           ifelse(pvals[145]<0.01,"***",
                                  ifelse(pvals[145]<0.05,"**",
                                         ifelse(pvals[145]<0.1,"*",""))))
    df.sfa_vrs[9,3] <- paste0("(",format(round(stderrs[145],3),nsmall=3),")")
    
    df.sfa_vrs[10,2] <- paste0(format(round(model_sfa$mlParam["Zv_(Intercept)"],3),nsmall=3),
                            ifelse(pvals[146]<0.01,"***",
                                   ifelse(pvals[146]<0.05,"**",
                                          ifelse(pvals[146]<0.1,"*",""))))
    df.sfa_vrs[10,3] <- paste0("(",format(round(stderrs[146],3),nsmall=3),")")
    
    df.sfa_vrs[11,2] <- "0.942"
    df.sfa_vrs[11,3] <- ""
    
    # Set global option to produce latex output
    options(knitr.table.format = "latex")
    
    # Create table 
    Tab_sfa_vrs <- kable(df.sfa_vrs, booktabs = T,
                           row.names = FALSE,
                            escape = FALSE,
                            linesep = "",
                           caption = "Coefficient estimates of the Cobb-Douglas distance function for US agriculture (1960-2004) under VRS.",
                           label = "Tab_sfa_vrs",
                           col.names = c('Variable', 'Estimate', 
                                         'St. Err.')) %>%
      footnote(general = "Dependent variable is -ln(crops). Estimation is based on 2160 observations. $exp(\\\\delta)$ is 
               the variance of the one-sided error term $u_{it}$ and $exp(\\\\phi)$ is the variance of the two-sided 
               error term $\\\\nu_{it}$ in equation (\\\\ref{eq:CD_frontier}).",
               footnote_as_chunk = T,
               threeparttable = T,
               general_title = "Note:",
               escape=F) %>%
      kable_styling(latex_options = c("HOLD_position")) 
    
    # Print Latex file
    writeLines(Tab_sfa_vrs, "Tables/Tab_sfa_vrs.tex")

  # ---------------------------------- #
  #### Create table for the results ####
  # ---------------------------------- #  
    
  # Prepare data for the tables
    
    data_MSFA_vrs <- data.frame(matrix(ncol = 16, nrow = 48))
    colnames(data_MSFA_vrs) <- c("state", 
                                 "TFP1960", "TFP2004", "DTFP",
                                 "MP1960", "MP2004", "DMP",
                                 "OTE1960", "OTE2004", "DOTE",
                                 "OSE1960", "OSE2004", "DOSE",
                                 "SNI1960", "SNI2004", "DSNI")
    
    data_MSFA_vrs$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                                 "GA","IA","ID","IL","IN","KS","KY","LA",
                                 "MA","MD","ME","MI","MN","MO","MS","MT",
                                 "NC","ND","NE","NH","NJ","NM","NV","NY",
                                 "OH","OK","OR","PA","RI","SC","SD","TN",
                                 "TX","UT","VA","VT","WA","WI","WV","WY"))
    
    
  # TOTAL FACTOR PRODUCTIVITY 
    
    # TFP1960
    data_MSFA_vrs$TFP1960 <- MSFA.level_vrs$TFPit[MSFA.level_vrs$year==1960]
    # TFP2004
    data_MSFA_vrs$TFP2004 <- MSFA.level_vrs$TFPit[MSFA.level_vrs$year==2004]
    # DTFP
    data_MSFA_vrs$DTFP <- (data_MSFA_vrs$TFP2004 / data_MSFA_vrs$TFP1960)
    
  # TECHNICAL CHANGE
    
    # MP1960
    data_MSFA_vrs$MP1960 <- MSFA.level_vrs$TCit[MSFA.level_vrs$year==1960]
    # MP2004
    data_MSFA_vrs$MP2004 <- MSFA.level_vrs$TCit[MSFA.level_vrs$year==2004]
    # DMP
    data_MSFA_vrs$DMP <- (data_MSFA_vrs$MP2004 / data_MSFA_vrs$MP1960)
    
  # OUTPUT TECHNICAL EFFICIENCY (OTE)
    
    # OTE1960
    data_MSFA_vrs$OTE1960 <- MSFA.level_vrs$OTEit[MSFA.level_vrs$year==1960]
    # OTE2004
    data_MSFA_vrs$OTE2004 <- MSFA.level_vrs$OTEit[MSFA.level_vrs$year==2004]
    # DOTE
    data_MSFA_vrs$DOTE <- (data_MSFA_vrs$OTE2004 / data_MSFA_vrs$OTE1960)
    
  # OUTPUT SCALE EFFICIENCY (OSE)
    
    # OSE1960
    data_MSFA_vrs$OSE1960 <- MSFA.level_vrs$OSEit[MSFA.level_vrs$year==1960]
    # OSE2004
    data_MSFA_vrs$OSE2004 <- MSFA.level_vrs$OSEit[MSFA.level_vrs$year==2004]
    # DOSE
    data_MSFA_vrs$DOSE <- (data_MSFA_vrs$OSE2004 / data_MSFA_vrs$OSE1960)
    
  # STATISTICAL NOISE INDEX (SNI)
    
    # SNI1960
    data_MSFA_vrs$SNI1960 <- MSFA.level_vrs$NCit[MSFA.level_vrs$year==1960]
    # SNI2004
    data_MSFA_vrs$SNI2004 <- MSFA.level_vrs$NCit[MSFA.level_vrs$year==2004]
    # DSNI
    data_MSFA_vrs$DSNI <- (data_MSFA_vrs$SNI2004 / data_MSFA_vrs$SNI1960)
    
  # Add US-average as geometric mean
  data_MSFA_vrs <- data_MSFA_vrs %>% 
    add_row(state="US48",
            TFP1960=NA, TFP2004=NA, DTFP=exp(mean(log(data_MSFA_vrs$DTFP))),
            MP1960=NA, MP2004=NA, DMP=exp(mean(log(data_MSFA_vrs$DMP))),
            OTE1960=NA, OTE2004=NA, DOTE=exp(mean(log(data_MSFA_vrs$DOTE))),
            OSE1960=NA, OSE2004=NA, DOSE=exp(mean(log(data_MSFA_vrs$DOSE))),
            SNI1960=NA, SNI2004=NA, DSNI=exp(mean(log(data_MSFA_vrs$DSNI))))
    
  # Save US-average for comparison in 03h_TFP for US_comparison.R
  Summary_msfa_US_vrs <- list(data_MSFA_vrs$DTFP[data_MSFA_vrs$state=="US48"],
                              data_MSFA_vrs$DMP[data_MSFA_vrs$state=="US48"],
                              data_MSFA_vrs$DOTE[data_MSFA_vrs$state=="US48"] * data_MSFA_vrs$DOSE[data_MSFA_vrs$state=="US48"])
  names(Summary_msfa_US_vrs) <- c("TFP", "TC", "TFPE") 
  save(Summary_msfa_US_vrs, file = "R_output/Summary_msfa_US_vrs.Rda")

  
  # Write Table: "TFP decomposition for US agriculture (1960-2004) using the Global Malmquist index.
  
    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Create table  
    Tab_MSFA_US_TFPDecomp_vrs <- kable(data_MSFA_vrs, booktabs = T, 
                                       digits = 2,
                                       row.names = FALSE,
                                       escape = FALSE,
                                       linesep = "",
                                       caption = "TFP decomposition for US agriculture (1960--2004) using the M-SFA index under VRS.",
                                       label = "Tab_MSFA_US_TFPDecomp_vrs",
                                       col.names = c('State', 
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$")) %>%
      add_header_above(c("", "TFP" = 3, "TFP*" = 3, "OTE" = 3, "OSE" = 3, "SN" = 3)) %>%
      row_spec(48, hline_after=T) %>% 
      kable_styling(font_size = 12,
                    latex_options = c("scale_down", "HOLD_position"))
    
    # Print Latex file
    writeLines(Tab_MSFA_US_TFPDecomp_vrs, "Tables/Tab_MSFA_US_TFPDecomp_vrs.tex")
    
# ------------------------------------ #
#### 2) Compute TFP index under CRS ####
# ------------------------------------ #   
    
  # ------------------------------- #
  ##### 2a) Frontier estimation #####
  # ------------------------------- #
    
    # Normalize dependent variable and input variables by intermediate inputs to impose CRS    
    sfa_string <- as.formula(paste0(c("-log(q_crops/x_interm)", 
                                          paste0(c("log(q_livestock/q_crops)",
                                                   "log(q_otheroutp/q_crops)", 
                                                   "log(x_capital/x_interm)", "log(x_land/x_interm)", 
                                                   "log(x_labor/x_interm)", 
                                                   "factor(state)",
                                                   "trend:factor(region):factor(semidecade)"), 
                                                 collapse = " + ")),
                                        collapse = " ~ "))
    
    model_sfa <- sfaR::sfacross(sfa_string, S=-1, data = USdata)
    summary(model_sfa)
    
    # for Delta method: mean and variance of parameters
    estmean <- coef(model_sfa)
    estvar <- model_sfa$invHessian #The covariance matrix of the parameters obtained from the ML estimation
    
    # Estimate model as OLS to find noise component below
    model_ols <- lm(sfa_string, data = USdata)
    summary(model_ols)
    
  # ------------------------------- #
  ##### 2b) Decompose TFP index #####
  # ------------------------------- #
    
    # Recover estimate for intermediate inputs
    coef_interm <- -1 - estmean[4] - estmean[5] - estmean[6]
    se_interm   <- deltamethod (~ -1-x4-x5-x6, estmean, estvar)
    z_interm <- coef_interm / se_interm
    pval_interm <- 2*pnorm(q=abs(z_interm), lower.tail=FALSE)
    
    # Scale elasticity
    scale_elas <- -model_sfa$mlParam["log(x_capital/x_interm)"] + -model_sfa$mlParam["log(x_land/x_interm)"] + 
      -model_sfa$mlParam["log(x_labor/x_interm)"] + -coef_interm
    
    # Aggregate output
    USdata$Qit <- USdata$q_livestock^(model_sfa$mlParam["log(q_livestock/q_crops)"]) * 
      USdata$q_otheroutp^(model_sfa$mlParam["log(q_otheroutp/q_crops)"]) * 
      USdata$q_crops^(1 - model_sfa$mlParam["log(q_livestock/q_crops)"] - model_sfa$mlParam["log(q_otheroutp/q_crops)"])
    
    # Aggregate input
    USdata$Xit <- USdata$x_capital^(-model_sfa$mlParam["log(x_capital/x_interm)"]) * 
      USdata$x_land^(-model_sfa$mlParam["log(x_land/x_interm)"]) * 
      USdata$x_labor^(-model_sfa$mlParam["log(x_labor/x_interm)"]) * 
      USdata$x_interm^(-coef_interm)
    
    # Total factor productivity
    USdata$TFPit <- USdata$Qit/USdata$Xit 
    
    # Output technical efficiency
    USdata$OTEit <- sfaR::efficiencies(model_sfa)[, "teBC"]  
    
    # Noise component
    USdata$NCit <- exp(log(USdata$q_crops/USdata$x_interm) - as.numeric(crossprod(matrix(-model_sfa$mlParam[1:(length(model_sfa$mlParam) - 2)]),
                                                                  t(model.matrix(model_ols)))) + sfaR::efficiencies(model_sfa)[, "u"]) # noise component
    
    # Output scale component
    USdata$OSEit <- USdata$x_capital^(-model_sfa$mlParam["log(x_capital/x_interm)"] * (1 - 1/scale_elas)) * 
      USdata$x_land^(-model_sfa$mlParam["log(x_land/x_interm)"] * (1 - 1/scale_elas)) * 
      USdata$x_labor^(-model_sfa$mlParam["log(x_labor/x_interm)"] * (1 - 1/scale_elas)) * 
      USdata$x_interm^(-coef_interm * (1 - 1/scale_elas))  
    
    # Technical change component
    USdata$TCit <- exp(apply(sweep(model.matrix(~factor(state),data=USdata),MARGIN=2, STATS=c(-model_sfa$mlParam[1],-model_sfa$mlParam[stringr::str_detect(names(model_sfa$mlParam),
                                                                                                                                                           "state")]), FUN="*"),1, sum) + 
                         apply(sweep(model.matrix(~-1 + trend:factor(region):factor(semidecade),
                                                data = USdata), MARGIN = 2, STATS = -model_sfa$mlParam[stringr::str_detect(names(model_sfa$mlParam),
                                                                                                                         "trend")], FUN = "*"), 1, sum))  ## technical change component
    
    # Summarize the results
    summary(USdata[c("OTEit", "NCit", "OSEit", "TCit")])
    
    # Subset data to only contain the index components
    MSFA.level_crs <- USdata %>% 
      dplyr::select(year,state,Qit,Xit,TFPit,TCit,OTEit,OSEit,NCit)
    
    # Save results for comparison in "03h_TFP for US_comparison"
    save(MSFA.level_crs, file="R_output/MSFA.level_crs.Rda")
    
  # --------------------------------- #
  ##### 2c) Create results tables #####
  # --------------------------------- #
    
    # ----------------------------------------- #
    # Table to summarize the regression results #
    # ----------------------------------------- #
    
    df.sfa_crs <- data.frame(matrix(ncol = 3, nrow = 11))
    colnames(df.sfa_crs) <- c("Variable", "Estimate", "Standard Error")
    
    df.sfa_crs$Variable <- c("ln(livestock/Crops)",
                          "ln(Other output/Crops)",
                          "ln(Capital)",
                          "ln(Land)",
                          "ln(Labor)",
                          "ln(Intermediate inputs)",
                          "Binary variables for states",
                          "Regional- and semidecade- \\\\ specific time trends",
                          "$\\delta$",
                          "$\\phi$",
                          "Average efficiency")
    
    estimates <- model_sfa$mlParam 
    stderrs <- sqrt(diag(model_sfa$invHessian))
    zvals <- estimates/stderrs
    pvals <- 2*pnorm(q=abs(zvals), lower.tail=FALSE)
    
    
    df.sfa_crs[1,2] <- paste0(format(round(model_sfa$mlParam["log(q_livestock/q_crops)"],3),nsmall=3),
                           ifelse(pvals[2]<0.01,"***",
                                  ifelse(pvals[2]<0.05,"**",
                                         ifelse(pvals[2]<0.1,"*",""))))
    df.sfa_crs[1,3] <- paste0("(",format(round(stderrs[2],3),nsmall=3),")")
    
    df.sfa_crs[2,2] <- paste0(format(round(model_sfa$mlParam["log(q_otheroutp/q_crops)"],3),nsmall=3),
                           ifelse(pvals[3]<0.01,"***",
                                  ifelse(pvals[3]<0.05,"**",
                                         ifelse(pvals[3]<0.1,"*",""))))
    df.sfa_crs[2,3] <- paste0("(",format(round(stderrs[3],3),nsmall=3),")")
    
    df.sfa_crs[3,2] <- paste0(format(round(model_sfa$mlParam["log(x_capital/x_interm)"],3),nsmall=3),
                           ifelse(pvals[4]<0.01,"***",
                                  ifelse(pvals[4]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_crs[3,3] <- paste0("(",format(round(stderrs[4],3),nsmall=3),")")
    
    df.sfa_crs[4,2] <- paste0(format(round(model_sfa$mlParam["log(x_land/x_interm)"],3),nsmall=3),
                           ifelse(pvals[5]<0.01,"***",
                                  ifelse(pvals[5]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_crs[4,3] <- paste0("(",format(round(stderrs[5],3),nsmall=3),")")
    
    df.sfa_crs[5,2] <- paste0(format(round(model_sfa$mlParam["log(x_labor/x_interm)"],3),nsmall=3),
                           ifelse(pvals[6]<0.01,"***",
                                  ifelse(pvals[6]<0.05,"**",
                                         ifelse(pvals[4]<0.1,"*",""))))
    df.sfa_crs[5,3] <- paste0("(",format(round(stderrs[6],3),nsmall=3),")")
    
    df.sfa_crs[6,2] <- paste0(format(round(coef_interm,3),nsmall=3),
                           ifelse(pval_interm<0.01,"***",
                                  ifelse(pval_interm[7]<0.05,"**",
                                         ifelse(pval_interm[4]<0.1,"*",""))))
    df.sfa_crs[6,3] <- paste0("(",format(round(se_interm,3),nsmall=3),")")
    
    df.sfa_crs[7,2] <- "not reported"
    df.sfa_crs[7,3] <- ""
    df.sfa_crs[8,2] <- "not reported"
    df.sfa_crs[8,3] <- ""
    
    df.sfa_crs[9,2] <- paste0(format(round(model_sfa$mlParam["Zu_(Intercept)"],3),nsmall=3),
                           ifelse(pvals[144]<0.01,"***",
                                  ifelse(pvals[144]<0.05,"**",
                                         ifelse(pvals[144]<0.1,"*",""))))
    df.sfa_crs[9,3] <- paste0("(",format(round(stderrs[144],3),nsmall=3),")")
    
    df.sfa_crs[10,2] <- paste0(format(round(model_sfa$mlParam["Zv_(Intercept)"],3),nsmall=3),
                            ifelse(pvals[145]<0.01,"***",
                                   ifelse(pvals[145]<0.05,"**",
                                          ifelse(pvals[145]<0.1,"*",""))))
    df.sfa_crs[10,3] <- paste0("(",format(round(stderrs[145],3),nsmall=3),")")
    
    df.sfa_crs[11,2] <- "0.942"
    df.sfa_crs[11,3] <- ""
    
    # Set global option to produce latex output
    options(knitr.table.format = "latex")
    
    # Create table 
    Tab_sfa_crs <- kable(df.sfa_crs, booktabs = T,
                       row.names = FALSE,
                       escape = FALSE,
                       linesep = "",
                       caption = "Coefficient estimates of the Cobb-Douglas distance function for US agriculture (1960-2004) under CRS.",
                       label = "Tab_sfa_crs",
                       col.names = c('Variable', 'Estimate', 
                                     'St. Err.')) %>%
      footnote(general = "Dependent variable is -ln(crops). Estimation is based on 2160 observations. $exp(\\\\delta)$ is 
               the variance of the one-sided error term $u_{it}$ and $exp(\\\\phi)$ is the variance of the two-sided 
               error term $\\\\nu_{it}$ in equation (\\\\ref{eq:CD_frontier}). The dependent variable and the input variables 
               have been normalized by intermediate inputs to impose constant returns to scale. The standard
               error of this numeraire has been obtained using the Delta method.",
               footnote_as_chunk = T,
               threeparttable = T,
               general_title = "Note:",
               escape=F) %>%
      kable_styling(latex_options = c("HOLD_position")) 
    
    # Print Latex file
    writeLines(Tab_sfa_crs, "Tables/Tab_sfa_crs.tex")
    
    
  # ---------------------------------- #
  #### Create table for the results ####
  # ---------------------------------- #  
    
    # Prepare data for the tables
    
    data_MSFA_crs <- data.frame(matrix(ncol = 16, nrow = 48))
    colnames(data_MSFA_crs) <- c("state", 
                                 "TFP1960", "TFP2004", "DTFP",
                                 "MP1960", "MP2004", "DMP",
                                 "OTE1960", "OTE2004", "DOTE",
                                 "OSE1960", "OSE2004", "DOSE",
                                 "SNI1960", "SNI2004", "DSNI")
    
    data_MSFA_crs$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                                 "GA","IA","ID","IL","IN","KS","KY","LA",
                                 "MA","MD","ME","MI","MN","MO","MS","MT",
                                 "NC","ND","NE","NH","NJ","NM","NV","NY",
                                 "OH","OK","OR","PA","RI","SC","SD","TN",
                                 "TX","UT","VA","VT","WA","WI","WV","WY"))
    
    
    # TOTAL FACTOR PRODUCTIVITY 
    
    # TFP1960
    data_MSFA_crs$TFP1960 <- MSFA.level_crs$TFPit[MSFA.level_crs$year==1960]
    # TFP2004
    data_MSFA_crs$TFP2004 <- MSFA.level_crs$TFPit[MSFA.level_crs$year==2004]
    # DTFP
    data_MSFA_crs$DTFP <- (data_MSFA_crs$TFP2004 / data_MSFA_crs$TFP1960)
    
    # TECHNICAL CHANGE
    
    # MP1960
    data_MSFA_crs$MP1960 <- MSFA.level_crs$TCit[MSFA.level_crs$year==1960]
    # MP2004
    data_MSFA_crs$MP2004 <- MSFA.level_crs$TCit[MSFA.level_crs$year==2004]
    # DMP
    data_MSFA_crs$DMP <- (data_MSFA_crs$MP2004 / data_MSFA_crs$MP1960)
    
    # OUTPUT TECHNICAL EFFICIENCY (OTE)
    
    # OTE1960
    data_MSFA_crs$OTE1960 <- MSFA.level_crs$OTEit[MSFA.level_crs$year==1960]
    # OTE2004
    data_MSFA_crs$OTE2004 <- MSFA.level_crs$OTEit[MSFA.level_crs$year==2004]
    # DOTE
    data_MSFA_crs$DOTE <- (data_MSFA_crs$OTE2004 / data_MSFA_crs$OTE1960)
    
    # OUTPUT SCALE EFFICIENCY (OSE)
    
    # OSE1960
    data_MSFA_crs$OSE1960 <- MSFA.level_crs$OSEit[MSFA.level_crs$year==1960]
    # OSE2004
    data_MSFA_crs$OSE2004 <- MSFA.level_crs$OSEit[MSFA.level_crs$year==2004]
    # DOSE
    data_MSFA_crs$DOSE <- (data_MSFA_crs$OSE2004 / data_MSFA_crs$OSE1960)
    
    # STATISTICAL NOISE INDEX (SNI)
    
    # SNI1960
    data_MSFA_crs$SNI1960 <- MSFA.level_crs$NCit[MSFA.level_crs$year==1960]
    # SNI2004
    data_MSFA_crs$SNI2004 <- MSFA.level_crs$NCit[MSFA.level_crs$year==2004]
    # DSNI
    data_MSFA_crs$DSNI <- (data_MSFA_crs$SNI2004 / data_MSFA_crs$SNI1960)
    
    # Add US-average as geometric mean
    data_MSFA_crs <- data_MSFA_crs %>% 
      add_row(state="US48",
              TFP1960=NA, TFP2004=NA, DTFP=exp(mean(log(data_MSFA_crs$DTFP))),
              MP1960=NA, MP2004=NA, DMP=exp(mean(log(data_MSFA_crs$DMP))),
              OTE1960=NA, OTE2004=NA, DOTE=exp(mean(log(data_MSFA_crs$DOTE))),
              OSE1960=NA, OSE2004=NA, DOSE=exp(mean(log(data_MSFA_crs$DOSE))),
              SNI1960=NA, SNI2004=NA, DSNI=exp(mean(log(data_MSFA_crs$DSNI))))
    
    # Save US-average for comparison in 03h_TFP for US_comparison.R
    Summary_msfa_US_crs <- list(data_MSFA_crs$DTFP[data_MSFA_crs$state=="US48"],
                                data_MSFA_crs$DMP[data_MSFA_crs$state=="US48"],
                                data_MSFA_crs$DOTE[data_MSFA_crs$state=="US48"] * data_MSFA_crs$DOSE[data_MSFA_crs$state=="US48"])
    names(Summary_msfa_US_crs) <- c("TFP", "TC", "TFPE") 
    save(Summary_msfa_US_crs, file = "R_output/Summary_msfa_US_crs.Rda")
    
    
    # Write Table: "TFP decomposition for US agriculture (1960-2004) using the Global Malmquist index.
    
    # Set global option to produce latex output
    options(knitr.table.format = "latex", knitr.kable.NA = '')
    
    # Create table  
    Tab_MSFA_US_TFPDecomp_crs <- kable(data_MSFA_crs, booktabs = T, 
                                       digits = 2,
                                       row.names = FALSE,
                                       escape = FALSE,
                                       linesep = "",
                                       caption = "TFP decomposition for US agriculture (1960--2004) using the M-SFA index under CRS.",
                                       label = "Tab_MSFA_US_TFPDecomp_crs",
                                       col.names = c('State', 
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$",
                                                     '1960', '2004', "$\\Delta$")) %>%
      add_header_above(c("", "TFP" = 3, "TFP*" = 3, "OTE" = 3, "OSE" = 3, "SN" = 3)) %>%
      row_spec(48, hline_after=T) %>% 
      kable_styling(font_size = 12,
                    latex_options = c("scale_down", "HOLD_position"))
    
    # Print Latex file
    writeLines(Tab_MSFA_US_TFPDecomp_crs, "Tables/Tab_MSFA_US_TFPDecomp_crs.tex")
    