# -------------------------------------- #
#                                        #
# This program calculates the Lowe TFP   #
# index using the aggregate US data from #
# 1948 - 2019                            #
#                                        #
# -------------------------------------- #

# Open packages
library(tidyverse) #for data manipulation
library(reshape2) #to reshape the data
library(tikzDevice) #to save ggplot object in latex format
library(ggpubr) #to combine plots
library(kableExtra) # to convert data frames to Latex

# Set path to Latex compiler if figures should be stored in Latex format
options("tikzLatex"='C:/Program Files/MiKTeX/miktex/bin/x64/pdflatex.exe')

# Load the US aggregate data
load("R_output/AggUSdata.Rda")

# Define reference prices
p0_livestock <- mean(AggUSdata$p_livestock)
p0_crops <- mean(AggUSdata$p_crops)
p0_otheroutp <- mean(AggUSdata$p_otheroutp)

w0_capital <- mean(AggUSdata$w_capital)
w0_labor <- mean(AggUSdata$w_labor)
w0_interm <- mean(AggUSdata$w_interm)

# Calculate profitability index
AggUSdata$prof <- (AggUSdata$p_livestock * AggUSdata$q_livestock +
                      AggUSdata$p_crops * AggUSdata$q_crops +
                      AggUSdata$p_otheroutp * AggUSdata$q_otheroutp) /
  (AggUSdata$w_capital * AggUSdata$x_capital +
     AggUSdata$w_labor * AggUSdata$x_labor + 
     AggUSdata$w_interm * AggUSdata$x_interm) 

# Calculate productivity index
AggUSdata$TFP <- (p0_livestock * AggUSdata$q_livestock +
                     p0_crops * AggUSdata$q_crops +
                     p0_otheroutp * AggUSdata$q_otheroutp) /
  (w0_capital * AggUSdata$x_capital +
     w0_labor * AggUSdata$x_labor + 
     w0_interm * AggUSdata$x_interm)  

# Calculate TT index
AggUSdata$tt <- AggUSdata$prof / AggUSdata$TFP

# Normalize indexes such that 1948 = 1
AggUSdata_norm <- AggUSdata %>% 
  mutate_each(funs(./.[1]), setdiff(names(.), c("state","year")))

# ----------------------------- #
#### Create figure for Latex ####
# ----------------------------- #

# Prepare data for the plot
data_plot <- AggUSdata_norm %>% 
  select("year", "TFP") %>% 
  reshape2::melt(id.vars="year")

# set filepath for latex output
tikz(file = "Figures/Fig_Lowe_aggUS.tex", width = 4, height = 3)

# plot
Fig_Lowe_aggUS <- ggplot() + 
  geom_line(data = data_plot, aes(x = year, y = value, linetype = variable)
  ) + labs(
    x = "",
    y = "",
    linetype = ""
  ) + 
  theme_bw() +
  #ggtitle("B") +
  scale_x_continuous(breaks = seq(1948, 2019, by = 3)) +
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits=c(0,4.5)) +  
  theme(axis.text.x = element_text(angle=90)) +
  theme(legend.position="none",
        legend.margin=margin(t = -0.7, unit='cm'))

# print latex output
print(Fig_Lowe_aggUS)

# close latex output
dev.off()
