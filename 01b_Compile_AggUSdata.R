# --------------------------------------- #
#                                         #
# This program extracts and organizes the # 
# aggregate US input and output data from #
# the raw .csv file provided by USDA      #
#                                         #
# --------------------------------------- #

# Open package
library(readxl) # to read data from excel files

# ------------------------------------ #
#### Download and save the raw data ####
# ------------------------------------ #

# Download the data from ERS-USDA ("Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004")
  
  #url <- "https://www.ers.usda.gov/webdocs/DataFiles/47679/table01a.xlsx?v=9050.4"
  #destfile <- "Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx"
  #download.file(url, destfile)


# ----------------------- #
#### Read-in the data #####
# ----------------------- #

p_livestock <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "D4:D75", col_names = FALSE)
q_livestock <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "E4:E75", col_names = FALSE)

p_crops <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "L4:L75", col_names = FALSE)
q_crops <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "M4:M75", col_names = FALSE)

p_otheroutp <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "Z4:Z75", col_names = FALSE)
q_otheroutp <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AA4:AA75", col_names = FALSE)

# Agricultural inputs

w_capital <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AD4:AD75", col_names = FALSE)
x_capital <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AE4:AE75", col_names = FALSE)

w_labor <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AN4:AN75", col_names = FALSE)
x_labor <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AO4:AO75", col_names = FALSE)

w_interm <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AT4:AT75", col_names = FALSE)
x_interm <- read_excel("Data/Table 1a. Price indices and implicit quantities of farm output and inputs for the United States, 1948-2019.xlsx", range = "AU4:AU75", col_names = FALSE)

#Note: Land is here included in the capital variable


# ---------------------------------- #
#### Store data in one data frame ####
# ---------------------------------- #

AggUSdata <- data.frame(p_livestock,q_livestock,p_crops,q_crops,p_otheroutp,q_otheroutp,w_capital,x_capital,w_labor,x_labor,w_interm,x_interm)
colnames(AggUSdata) <- c("p_livestock","q_livestock","p_crops","q_crops","p_otheroutp","q_otheroutp","w_capital","x_capital","w_labor","x_labor","w_interm","x_interm")
AggUSdata$year <- seq(1948, 2019)

summary(AggUSdata)

# -------------------------- #
#### save this data frame ####
# -------------------------- #

  # Save as .RDA-file 
  save(AggUSdata,file="R_output/AggUSdata.Rda")

  # Save as .CSV-file
  write.csv(AggUSdata, "R_output/AggUSdata.csv", row.names=TRUE)
