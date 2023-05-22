# ------------------------------------- #
#                                       #
# This program estimates the LOWE TFP   #
# index for different windows using the #
# state-level US data                   #
#                                       #
# ------------------------------------- #

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

# Load the source codes for the Lowe index
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


# ---------------------- #
#### Original windows ####
# ---------------------- #

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

#merge all results
lowe_pacific <- lowe_pacific$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_mountain <- lowe_mountain$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_nplains <- lowe_nplains$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_splains <- lowe_splains$Levels[,c(1,2,11:13)]  %>% 
  filter(year==1960 | year==2004)
lowe_cornbelt <- lowe_cornbelt$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_southeast <- lowe_southeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_northeast <- lowe_northeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_lake <- lowe_lake$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_appalacian <- lowe_appalacian$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_delta <- lowe_delta$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)

Lowe.level <- rbind(lowe_pacific, lowe_mountain, lowe_nplains, 
                    lowe_splains, lowe_cornbelt, lowe_southeast, 
                    lowe_northeast, lowe_lake, lowe_appalacian, 
                    lowe_delta)

# Order results by state
Lowe.level <- Lowe.level[order(Lowe.level$state),]

# Calculate changes (worig=windows original)
Lowe_worig <- data.frame(matrix(ncol = 5, nrow = 48))
x <- c("state", "windows", "DTFP", "DMP", "DTFPE")
colnames(Lowe_worig) <- x

Lowe_worig$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                       "GA","IA","ID","IL","IN","KS","KY","LA",
                       "MA","MD","ME","MI","MN","MO","MS","MT",
                       "NC","ND","NE","NH","NJ","NM","NV","NY",
                       "OH","OK","OR","PA","RI","SC","SD","TN",
                       "TX","UT","VA","VT","WA","WI","WV","WY"))

lowe1960 <- subset(Lowe.level, year==1960)
lowe2004 <- subset(Lowe.level, year==2004)

# DTFP
Lowe_worig$DTFP <- (lowe2004[,"TFP"] / lowe1960[,"TFP"])

# DMP
Lowe_worig$DMP <- (lowe2004[,"MP"] / lowe1960[,"MP"])

# DTFPE
Lowe_worig$DTFPE <- (lowe2004[,"TFPE"] / lowe1960[,"TFPE"])

# Add US-average as geometric mean
Lowe_worig <- Lowe_worig %>% 
  add_row(state="US48",
          DTFP=exp(mean(log(Lowe_worig$DTFP))),
          DMP=exp(mean(log(Lowe_worig$DMP))),
          DTFPE=exp(mean(log(Lowe_worig$DTFPE))))

#Windows Pacific
Lowe_worig$windows[Lowe_worig$state=="CA" |
                   Lowe_worig$state=="OR" |
                   Lowe_worig$state=="WA" ] <- "5 (original)"
#Windows Mountain
Lowe_worig$windows[Lowe_worig$state=="AZ" |
                     Lowe_worig$state=="CO" |
                     Lowe_worig$state=="ID" |
                     Lowe_worig$state=="MT" |
                     Lowe_worig$state=="NM" |
                     Lowe_worig$state=="NV" |
                     Lowe_worig$state=="UT" |
                     Lowe_worig$state=="WY" ] <- "2 (original)"
#Windows Northern Plains
Lowe_worig$windows[Lowe_worig$state=="KS" |
                     Lowe_worig$state=="ND" |
                     Lowe_worig$state=="NE" |
                     Lowe_worig$state=="SD"] <- "4 (original)"
#Windows Southern Plains
Lowe_worig$windows[Lowe_worig$state=="OK" |
                     Lowe_worig$state=="TX"] <- "8 (original)"
#Windows Corn Belt
Lowe_worig$windows[Lowe_worig$state=="IA" |
                     Lowe_worig$state=="IL" |
                     Lowe_worig$state=="IN" |
                     Lowe_worig$state=="MO" |
                     Lowe_worig$state=="OH"] <- "3 (original)"
#Windows Southeast
Lowe_worig$windows[Lowe_worig$state=="AL" |
                     Lowe_worig$state=="FL" |
                     Lowe_worig$state=="GA" |
                     Lowe_worig$state=="SC"] <- "4 (original)"
#Windows Northeast
Lowe_worig$windows[Lowe_worig$state=="CT" |
                     Lowe_worig$state=="DE" |
                     Lowe_worig$state=="MA" |
                     Lowe_worig$state=="MD" |
                     Lowe_worig$state=="ME" |
                     Lowe_worig$state=="NH" |
                     Lowe_worig$state=="NJ" |
                     Lowe_worig$state=="NY" |
                     Lowe_worig$state=="PA" |
                     Lowe_worig$state=="RI" |
                     Lowe_worig$state=="VT" ] <- "2 (original)"
#Windows Lake States
Lowe_worig$windows[Lowe_worig$state=="MI" |
                     Lowe_worig$state=="MN" |
                     Lowe_worig$state=="WI"] <- "5 (original)"
#Windows Appalacian
Lowe_worig$windows[Lowe_worig$state=="KY" |
                     Lowe_worig$state=="NC" |
                     Lowe_worig$state=="TN" |
                     Lowe_worig$state=="VA" |
                     Lowe_worig$state=="WV"] <- "3 (original)"
#Windows Delta States
Lowe_worig$windows[Lowe_worig$state=="AR" |
                     Lowe_worig$state=="LA" |
                     Lowe_worig$state=="MS"] <- "5 (original)"

#Windows US48
Lowe_worig$windows[Lowe_worig$state=="US48"] <- "(original)"

# save these results
save(Lowe_worig, file="R_output/Lowe_worig.Rda")


# --------------------- #
#### 2-years windows ####
# --------------------- #

#Pacific
lowe_pacific <- lowe(data = Pacific, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(2), by.year = 1)

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
                      window = c(2), by.year = 1)

#SPlains
lowe_splains <- lowe(data = SPlains, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(2), by.year = 1)

#Cornbelt
lowe_cornbelt <- lowe(data = Cornbelt, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(2), by.year = 1)

#Southeast
lowe_southeast <- lowe(data = Southeast, id.var = "state", time.var = "year", 
                      x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                      y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                      w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                      p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                      tech.change = T, tech.reg = T, rts = "vrs", 
                      orientation = "out", cores = 8, scaled = F, 
                      window = c(2), by.year = 1)

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
                       window = c(2), by.year = 1)

#Appalacian
lowe_appalacian <- lowe(data = Appalacian, id.var = "state", time.var = "year", 
                  x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                  y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                  w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                  p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                  tech.change = T, tech.reg = T, rts = "vrs", 
                  orientation = "out", cores = 8, scaled = F, 
                  window = c(2), by.year = 1)

#Delta States
lowe_delta <- lowe(data = Delta, id.var = "state", time.var = "year", 
                        x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                        y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                        w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                        p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                        tech.change = T, tech.reg = T, rts = "vrs", 
                        orientation = "out", cores = 8, scaled = F, 
                        window = c(2), by.year = 1)

#merge all results
lowe_pacific <- lowe_pacific$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_mountain <- lowe_mountain$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_nplains <- lowe_nplains$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_splains <- lowe_splains$Levels[,c(1,2,11:13)]  %>% 
  filter(year==1960 | year==2004)
lowe_cornbelt <- lowe_cornbelt$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_southeast <- lowe_southeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_northeast <- lowe_northeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_lake <- lowe_lake$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_appalacian <- lowe_appalacian$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_delta <- lowe_delta$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)

Lowe.level <- rbind(lowe_pacific, lowe_mountain, lowe_nplains, 
                    lowe_splains, lowe_cornbelt, lowe_southeast, 
                    lowe_northeast, lowe_lake, lowe_appalacian, 
                    lowe_delta)

Lowe.level <- Lowe.level[order(Lowe.level$state),]

# Calculate changes (w2=2-years windows)
Lowe_w2 <- data.frame(matrix(ncol = 5, nrow = 48))
x <- c("state", "windows", "DTFP", "DMP", "DTFPE")
colnames(Lowe_w2) <- x

Lowe_w2$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                        "GA","IA","ID","IL","IN","KS","KY","LA",
                        "MA","MD","ME","MI","MN","MO","MS","MT",
                        "NC","ND","NE","NH","NJ","NM","NV","NY",
                        "OH","OK","OR","PA","RI","SC","SD","TN",
                        "TX","UT","VA","VT","WA","WI","WV","WY"))

Lowe_w2$windows = 2

lowe1960 <- subset(Lowe.level, year==1960)
lowe2004 <- subset(Lowe.level, year==2004)

# DTFP
Lowe_w2$DTFP <- (lowe2004[,"TFP"] / lowe1960[,"TFP"])

# DMP
Lowe_w2$DMP <- (lowe2004[,"MP"] / lowe1960[,"MP"])

# DTFPE
Lowe_w2$DTFPE <- (lowe2004[,"TFPE"] / lowe1960[,"TFPE"])

# Add US-average as geometric mean
Lowe_w2 <- Lowe_w2 %>% 
  add_row(state="US48",
          DTFP=exp(mean(log(Lowe_w2$DTFP))),
          DMP=exp(mean(log(Lowe_w2$DMP))),
          DTFPE=exp(mean(log(Lowe_w2$DTFPE))))
Lowe_w2$windows[Lowe_w2$state=="US48"] <- 2

# save these results
save(Lowe_w2, file="R_output/Lowe_w2.Rda")


# --------------------- #
#### 4-years windows ####
# --------------------- #


#Pacific
lowe_pacific <- lowe(data = Pacific, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(4), by.year = 1)

#Mountain
lowe_mountain <- lowe(data = Mountain, id.var = "state", time.var = "year", 
                      x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                      y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                      w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                      p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                      tech.change = T, tech.reg = T, rts = "vrs", 
                      orientation = "out", cores = 8, scaled = F, 
                      window = c(4), by.year = 1)

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
                     window = c(4), by.year = 1)

#Cornbelt
lowe_cornbelt <- lowe(data = Cornbelt, id.var = "state", time.var = "year", 
                      x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                      y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                      w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                      p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                      tech.change = T, tech.reg = T, rts = "vrs", 
                      orientation = "out", cores = 8, scaled = F, 
                      window = c(4), by.year = 1)

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
                       window = c(4), by.year = 1)

#Lake States
lowe_lake <- lowe(data = Lake, id.var = "state", time.var = "year", 
                  x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                  y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                  w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                  p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                  tech.change = T, tech.reg = T, rts = "vrs", 
                  orientation = "out", cores = 8, scaled = F, 
                  window = c(4), by.year = 1)

#Appalacian
lowe_appalacian <- lowe(data = Appalacian, id.var = "state", time.var = "year", 
                        x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                        y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                        w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                        p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                        tech.change = T, tech.reg = T, rts = "vrs", 
                        orientation = "out", cores = 8, scaled = F, 
                        window = c(4), by.year = 1)

#Delta States
lowe_delta <- lowe(data = Delta, id.var = "state", time.var = "year", 
                   x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                   y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                   w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                   p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                   tech.change = T, tech.reg = T, rts = "vrs", 
                   orientation = "out", cores = 8, scaled = F, 
                   window = c(4), by.year = 1)

#merge all results
lowe_pacific <- lowe_pacific$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_mountain <- lowe_mountain$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_nplains <- lowe_nplains$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_splains <- lowe_splains$Levels[,c(1,2,11:13)]  %>% 
  filter(year==1960 | year==2004)
lowe_cornbelt <- lowe_cornbelt$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_southeast <- lowe_southeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_northeast <- lowe_northeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_lake <- lowe_lake$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_appalacian <- lowe_appalacian$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_delta <- lowe_delta$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)

Lowe.level <- rbind(lowe_pacific, lowe_mountain, lowe_nplains, 
                    lowe_splains, lowe_cornbelt, lowe_southeast, 
                    lowe_northeast, lowe_lake, lowe_appalacian, 
                    lowe_delta)

Lowe.level <- Lowe.level[order(Lowe.level$state),]

# Calculate changes
Lowe_w4 <- data.frame(matrix(ncol = 5, nrow = 48))
x <- c("state", "windows", "DTFP", "DMP", "DTFPE")
colnames(Lowe_w4) <- x

Lowe_w4$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                       "GA","IA","ID","IL","IN","KS","KY","LA",
                       "MA","MD","ME","MI","MN","MO","MS","MT",
                       "NC","ND","NE","NH","NJ","NM","NV","NY",
                       "OH","OK","OR","PA","RI","SC","SD","TN",
                       "TX","UT","VA","VT","WA","WI","WV","WY"))

Lowe_w4$windows = 4

lowe1960 <- subset(Lowe.level, year==1960)
lowe2004 <- subset(Lowe.level, year==2004)

# DTFP
Lowe_w4$DTFP <- (lowe2004[,"TFP"] / lowe1960[,"TFP"])

# DMP
Lowe_w4$DMP <- (lowe2004[,"MP"] / lowe1960[,"MP"])

# DTFPE
Lowe_w4$DTFPE <- (lowe2004[,"TFPE"] / lowe1960[,"TFPE"])

# Add US-average as geometric mean
Lowe_w4 <- Lowe_w4 %>% 
  add_row(state="US48",
          DTFP=exp(mean(log(Lowe_w4$DTFP))),
          DMP=exp(mean(log(Lowe_w4$DMP))),
          DTFPE=exp(mean(log(Lowe_w4$DTFPE))))
Lowe_w4$windows[Lowe_w2$state=="US48"] <- 4

# save these results
save(Lowe_w4, file="R_output/Lowe_w4.Rda")


# --------------------- #
#### 8-years windows ####
# --------------------- #


#Pacific
lowe_pacific <- lowe(data = Pacific, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(8), by.year = 1)

#Mountain
lowe_mountain <- lowe(data = Mountain, id.var = "state", time.var = "year", 
                      x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                      y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                      w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                      p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                      tech.change = T, tech.reg = T, rts = "vrs", 
                      orientation = "out", cores = 8, scaled = F, 
                      window = c(8), by.year = 1)

#NPlains
lowe_nplains <- lowe(data = NPlains, id.var = "state", time.var = "year", 
                     x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                     y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                     w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                     p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                     tech.change = T, tech.reg = T, rts = "vrs", 
                     orientation = "out", cores = 8, scaled = F, 
                     window = c(8), by.year = 1)

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
                      window = c(8), by.year = 1)

#Southeast
lowe_southeast <- lowe(data = Southeast, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(8), by.year = 1)

#Northeast
lowe_northeast <- lowe(data = Northeast, id.var = "state", time.var = "year", 
                       x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                       y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                       w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                       p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                       tech.change = T, tech.reg = T, rts = "vrs", 
                       orientation = "out", cores = 8, scaled = F, 
                       window = c(8), by.year = 1)

#Lake States
lowe_lake <- lowe(data = Lake, id.var = "state", time.var = "year", 
                  x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                  y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                  w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                  p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                  tech.change = T, tech.reg = T, rts = "vrs", 
                  orientation = "out", cores = 8, scaled = F, 
                  window = c(8), by.year = 1)

#Appalacian
lowe_appalacian <- lowe(data = Appalacian, id.var = "state", time.var = "year", 
                        x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                        y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                        w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                        p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                        tech.change = T, tech.reg = T, rts = "vrs", 
                        orientation = "out", cores = 8, scaled = F, 
                        window = c(8), by.year = 1)

#Delta States
lowe_delta <- lowe(data = Delta, id.var = "state", time.var = "year", 
                   x.vars = c("x_capital", "x_land","x_labor","x_interm"), 
                   y.vars = c("q_livestock", "q_crops", "q_otheroutp"),
                   w.vars = c("w0_capital", "w0_land","w0_labor","w0_interm"), 
                   p.vars = c("p0_livestock", "p0_crops", "p0_otheroutp"), 
                   tech.change = T, tech.reg = T, rts = "vrs", 
                   orientation = "out", cores = 8, scaled = F, 
                   window = c(8), by.year = 1)

#merge all results
lowe_pacific <- lowe_pacific$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_mountain <- lowe_mountain$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_nplains <- lowe_nplains$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_splains <- lowe_splains$Levels[,c(1,2,11:13)]  %>% 
  filter(year==1960 | year==2004)
lowe_cornbelt <- lowe_cornbelt$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_southeast <- lowe_southeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_northeast <- lowe_northeast$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_lake <- lowe_lake$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_appalacian <- lowe_appalacian$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)
lowe_delta <- lowe_delta$Levels[,c(1,2,11:13)] %>% 
  filter(year==1960 | year==2004)

Lowe.level <- rbind(lowe_pacific, lowe_mountain, lowe_nplains, 
                    lowe_splains, lowe_cornbelt, lowe_southeast, 
                    lowe_northeast, lowe_lake, lowe_appalacian, 
                    lowe_delta)

Lowe.level <- Lowe.level[order(Lowe.level$state),]

# Calculate changes
Lowe_w8 <- data.frame(matrix(ncol = 5, nrow = 48))
x <- c("state", "windows", "DTFP", "DMP", "DTFPE")
colnames(Lowe_w8) <- x

Lowe_w8$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                       "GA","IA","ID","IL","IN","KS","KY","LA",
                       "MA","MD","ME","MI","MN","MO","MS","MT",
                       "NC","ND","NE","NH","NJ","NM","NV","NY",
                       "OH","OK","OR","PA","RI","SC","SD","TN",
                       "TX","UT","VA","VT","WA","WI","WV","WY"))

Lowe_w8$windows = 8

lowe1960 <- subset(Lowe.level, year==1960)
lowe2004 <- subset(Lowe.level, year==2004)

# DTFP
Lowe_w8$DTFP <- (lowe2004[,"TFP"] / lowe1960[,"TFP"])

# DMP
Lowe_w8$DMP <- (lowe2004[,"MP"] / lowe1960[,"MP"])

# DTFPE
Lowe_w8$DTFPE <- (lowe2004[,"TFPE"] / lowe1960[,"TFPE"])

# Add US-average as geometric mean
Lowe_w8 <- Lowe_w8 %>% 
  add_row(state="US48",
          DTFP=exp(mean(log(Lowe_w8$DTFP))),
          DMP=exp(mean(log(Lowe_w8$DMP))),
          DTFPE=exp(mean(log(Lowe_w8$DTFPE))))
Lowe_w8$windows[Lowe_w8$state=="US48"] <- 8

# save these results
save(Lowe_w8, file="R_output/Lowe_w8.Rda")




# ------------------------------------ #
#### Create a table for all results ####
# ------------------------------------ #

load("R_output/Lowe_worig.Rda")
load("R_output/Lowe_w2.Rda")
load("R_output/Lowe_w4.Rda")
load("R_output/Lowe_w8.Rda")

TFPwindows <- rbind(Lowe_worig,Lowe_w2, Lowe_w4, Lowe_w8)

TFPwindows <- TFPwindows[order(TFPwindows$state),]

# Set global option to produce latex output
options(knitr.table.format = "latex",
        knitr.kable.NA = '')

# Select subset of states
TFPwindows <- TFPwindows %>% 
  filter(state == "CA" | 
           state == "AZ" | 
           state == "KS" | 
           state == "OK" | 
           state == "IA" | 
           state == "AL" | 
           state == "CT" | 
           state == "MI" | 
           state == "KY" | 
           state == "AR" | 
           state == "US48")

# Create table  
Tab_TFPwindows <- kable(TFPwindows, booktabs = T, 
                   digits = 2,
                   row.names = FALSE,
                   escape = FALSE,
                   linesep = "",
                   caption = "TFP and efficiency change in US agriculture (1960--2004) using the Lowe index with different windows.",
                   label = "Tab_TFPwindows",
                   col.names = linebreak(c('State', 'Window length \n in years',
                                 "$\\Delta$TFP",
                                 "$\\Delta$TFP*",
                                 "$\\Delta$TFPE"))) %>%
  row_spec(c(4,8,12,16,20,24,28,32,36,40), hline_after=T) %>% 
  kable_styling(font_size = 10,
                latex_options = c("HOLD_position")) 

# Print Latex file
writeLines(Tab_TFPwindows, "Tables/Tab_TFPwindows.tex")
