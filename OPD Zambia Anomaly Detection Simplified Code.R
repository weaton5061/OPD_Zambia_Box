
# Simplified code for Anomaly Detection -----------------------------------

# Remove all objects form the current workspace
rm(list = ls())
#This function closes the specified plot (by default the current device) and if it is an imguR device, uploads the plots for web hosting
dev.off()
#Clear startup screen/console in R / RStudio
cat("\014") 
## ---- Load appropriate packages -------------------------------------------------------------------------------------
library(openxlsx)
library(readxl)
library(ggplot2)
library(tidyverse)
library(anomalize)
library(dplyr)

## ---- Set Working Drive -------------------------------------------------------------------------------------
setwd("/Users/willeaton/Box/OPD Cleaning/OPD Zambia Project Cloned Git Repository/OPD_Zambia_Project_Box")

## ---- Import Inpatient Outpatient Facility Data  -------------------------------------------------------------------------------------
inpatient_outpatient.df <- read.csv("/Users/willeaton/Box/OPD Cleaning/inpatient_outpatient_facility_data.csv")

# Examine variable classes ------------------------------------------------
lapply(inpatient_outpatient.df, class)

# convert factor variables to character variables ----------------------------------
inpatient_outpatient.df <- inpatient_outpatient.df %>% mutate_if(is.factor, as.character)

# change Org_Unit_ID to factor
inpatient_outpatient.df$Org_Unit_ID <- as.factor(inpatient_outpatient.df$Org_Unit_ID)   

# Ensure class conversion executed appropriately --------------------------
lapply(inpatient_outpatient.df, class)

# create smaller dataset to test out anomalize package, n = 606 observations, n = 7 health facilities
# Org_Unit_IDs: [1] 1763 1764 1709 1728 1837 1627 1784
# Org_Units :   [1] lu Mbereshi Mission Hospital                          lu Mbereshi Mission Hospital Affiliated Health Centre
#               [3] lu Kazembe Zonal Rural Health Centre                  lu Lufubu Rural Health Centre                        
#               [5] lu Salanga Rural Health Centre                        lu Chipunka Rural Health Centre                      
#               [7] lu Mukamba Rural Health Centre  

# Test out anomalize package on oneprov
oneprov.df <- inpatient_outpatient.df %>%
    filter(Province == "Luapula Province" & District =="Mwansabombwe District" &
               Org_Unit_ID == 1763 | Org_Unit_ID == 1709)

##----aggregate data -------------------------------------------------------------------------------
# test same code as above with smaller dataset, n = 326 obs
outpatient.df <-oneprov.df[ which(oneprov.df$Data_Element=='OPD First Attendance'), ]

#Aggregate Value Data by the HF, Month and Year
#outpatient1 <- outpatient.df %>% group_by(Year, Month, Org_Unit, District) %>% summarize(Value = sum(Value))
# commenting this out for now (on 6-4-20) so that Province is also included in dataframe
# outpatient2 <- outpatient.df %>% group_by(Year, Month, Org_Unit, District, Province) %>% summarize(Total_Value = sum(Value))
outpatient2 <- outpatient.df %>% group_by(Year, Month, Org_Unit_ID, District, Province) %>% summarize(Total_Value = sum(Value))

#Create Date from Month, Year
outpatient2$month_num <- match(outpatient2$Month, month.abb)
outpatient2$date <- as.Date(with(outpatient2, paste(Year, month_num, 1, sep="-")), "%Y-%m-%d")

# Analyze Outliers --------------------------------------------------------

# Drop the columns of the dataframe
# mydata <- subset(outpatient2, select = c(Province, Total_Value, date)) # Province, District, Org_Unit
mydata <- subset(outpatient2, select = c(Org_Unit_ID, Total_Value, date)) # Province, District, Org_Unit

# sort by date
#mydata2<-mydata[order(as.Date(mydata$date, format = "%d/%m/%Y")),]
mydata2<-mydata[order(as.Date(mydata$date, format = "%Y-%m-%d")),]

# Time series decomposition -----------------------------------------------
mydata2 %>% 
    ungroup()
mydata2_anomaly <-  mydata2 %>% 
    time_decompose(Total_Value)
mydata2_anomaly
mydata2_anomaly %>% glimpse()


