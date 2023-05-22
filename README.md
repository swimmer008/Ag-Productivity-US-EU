# Ag-Productivity-US-EU

This code replicates the figures and tables presented in the article “Components of Agricultural Productivity Change: Replication of US Evidence and Extension to the EU”, published in Applied Economics Perspectives and Policy. 

The files are as follows:

-	01a-01e: R codes to compile all data needed for estimation. Data are either automatically downloaded by the program, or the links and instructions to do so are provided.
-	02a-02h: R codes to estimate productivity indices and components for the state-level data provided by USDA.
-	03: R codes to estimate the productivity index for the country-level data provided by USDA.
-	04a-04b: R codes to estimate productivity indices and components for the country-level data provided by Eurostat. 
-	05a-05b: R codes to estimate productivity indices and components for the country-level International Agricultural Productivity data set provided by USDA.
-	06_Stata_CompareDensities.do: Stata codes to estimate the kernel-analysis procedure

The files in the subfolder "R_aux" contain the data envelopment analysis (DEA) programs to estimate and decompose all productivity indices computed in this study. 

The script was successfully run in R version 4.2.0 and Stata version 16.1.
