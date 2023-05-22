# ----------------------------------------- #
#                                           #
# This program extracts and organizes the   # 
# US state-level input and output data      #
# from the raw excel file provided by USDA. #
#                                           #
# ----------------------------------------- #

# Open package
library(readxl) # to read data from excel files

# ------------------------------------ #
#### Download and save the raw data ####
# ------------------------------------ #

  # Download the data from ERS-USDA ("Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004")

    #url <- "https://www.ers.usda.gov/webdocs/DataFiles/47679/StatePriceIndicesAndQ.xls?v=4846.9"
    #destfile <- "Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls"
    #download.file(url, destfile)

# ------------------------------------ #
#### Read in the required variables ####
# -------------------------------------#

# Agricultural outputs
  
  p_livestock <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C131:AU178", col_names = FALSE)
  q_livestock <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C182:AU229", col_names = FALSE)
  
  p_crops <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C235:AU282", col_names = FALSE)
  q_crops <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C286:AU333", col_names = FALSE)
  
  p_otheroutp <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C339:AU386", col_names = FALSE)
  q_otheroutp <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C390:AU437", col_names = FALSE)

# Agricultural inputs
  
  w_capital <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C547:AU594", col_names = FALSE)
  x_capital <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C598:AU645", col_names = FALSE)
  
  w_land <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C651:AU698", col_names = FALSE)
  x_land <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C702:AU749", col_names = FALSE)
  
  w_labor <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C755:AU802", col_names = FALSE)
  x_labor <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C806:AU853", col_names = FALSE)
  
  w_interm <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C1067:AU1114", col_names = FALSE)
  x_interm <- read_excel("Data/Table 23. Price indices and implicit quantities of farm outputs and inputs by State, 1960-2004.xls", range = "C1118:AU1165", col_names = FALSE)


# ------------------------------------ #
#### Create data frame for the data ####
# ------------------------------------ #
  
USdata <- data.frame(matrix(ncol = 16, nrow = 2160))
x <- c("year", "state", 
       "p_livestock", "q_livestock", "p_crops", "q_crops", "p_otheroutp", "q_otheroutp", 
       "w_capital", "x_capital", "w_land", "x_land", "w_labor", "x_labor", "w_interm", "x_interm")
colnames(USdata) <- x

USdata$year  <- seq(1960, 2004, length.out = 45)
USdata$state <- rep(c("AL","AR","AZ","CA","CO","CT","DE","FL",
                   "GA","IA","ID","IL","IN","KS","KY","LA",
                   "MA","MD","ME","MI","MN","MO","MS","MT",
                   "NC","ND","NE","NH","NJ","NM","NV","NY",
                   "OH","OK","OR","PA","RI","SC","SD","TN",
                   "TX","UT","VA","VT","WA","WI","WV","WY"),each=45)
  
# ------------------------------------ #
#### Write values to the data frame ####
# ------------------------------------ #

    # p_livestock
    x <- c(p_livestock[1,]) 
    for(i in 2:48) {
      x <- c(x,p_livestock[i,])
    }
    USdata$p_livestock <- as.numeric(x) 
    
    # q_livestock
    x <- c(q_livestock[1,]) 
    for(i in 2:48) {
      x <- c(x,q_livestock[i,])
    }
    USdata$q_livestock <- as.numeric(x)    
    
    # p_crops
    x <- c(p_crops[1,]) 
    for(i in 2:48) {
      x <- c(x,p_crops[i,])
    }
    USdata$p_crops <- as.numeric(x) 
    
    
    # q_crops
    x <- c(q_crops[1,]) 
    for(i in 2:48) {
      x <- c(x,q_crops[i,])
    }
    USdata$q_crops <- as.numeric(x)   
    
    # p_otheroutp
    x <- c(p_otheroutp[1,]) 
    for(i in 2:48) {
      x <- c(x,p_otheroutp[i,])
    }
    USdata$p_otheroutp <- as.numeric(x) 
    
    # q_otheroutp
    x <- c(q_otheroutp[1,]) 
    for(i in 2:48) {
      x <- c(x,q_otheroutp[i,])
    }
    USdata$q_otheroutp <- as.numeric(x)  
    
    # w_capital
    x <- c(w_capital[1,]) 
    for(i in 2:48) {
      x <- c(x,w_capital[i,])
    }
    USdata$w_capital <- as.numeric(x) 
    
    # x_capital
    x <- c(x_capital[1,]) 
    for(i in 2:48) {
      x <- c(x,x_capital[i,])
    }
    USdata$x_capital <- as.numeric(x)  
    
    # w_land
    x <- c(w_land[1,]) 
    for(i in 2:48) {
      x <- c(x,w_land[i,])
    }
    USdata$w_land <- as.numeric(x) 
    
    # x_land
    x <- c(x_land[1,]) 
    for(i in 2:48) {
      x <- c(x,x_land[i,])
    }
    USdata$x_land <- as.numeric(x)  
    
    # w_labor
    x <- c(w_labor[1,]) 
    for(i in 2:48) {
      x <- c(x,w_labor[i,])
    }
    USdata$w_labor <- as.numeric(x) 
    
    # x_labor
    x <- c(x_labor[1,]) 
    for(i in 2:48) {
      x <- c(x,x_labor[i,])
    }
    USdata$x_labor <- as.numeric(x)  
    
    # w_interm
    x <- c(w_interm[1,]) 
    for(i in 2:48) {
      x <- c(x,w_interm[i,])
    }
    USdata$w_interm <- as.numeric(x) 
    
    # x_interm
    x <- c(x_interm[1,]) 
    for(i in 2:48) {
      x <- c(x,x_interm[i,])
    }
    USdata$x_interm <- as.numeric(x) 
    
    
summary (USdata)

# -------------------------- #
#### save this data frame ####
# -------------------------- #
  
  # Save as .RDA-file
  save(USdata,file="R_output/USdata.Rda")
    
  # Save as .CSV-file 
  write.csv(USdata, "R_output/USdata.csv", row.names=TRUE)
