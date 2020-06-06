## ---- METADATA ------------------------------------------------------------------------------------------------------
# Author: Will Eaton
# Purpose: Zambia DiD analysis of PAMO project iCCM impact on malaria IPD and/or death (Luapula, Northern, Muchinga)
# Notes: Ruth requests concentrating on 6 Provinces: Luapula, Muchinga, Northern, Eastern, Northwestern, Western
#        n = approx 70 districts total
# Created: 2020-06-04
# Last updated: 
# Status: in progress
# Notes: Editing for OPD anomolies 

# ---- GAMEPLAN -------------------------------------------------------------------------------------------------------
# 1) Examine data
# 2) Observe any missingness
# 3) Subset
# 4) Show missingness
# 5) Show anomalies
# 6) Produce Figures
# 7) Produce Tables

# -------------------------------------------------------------------------------------------------------------------------------------
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
library(coindeskr)

## ---- Set Working Drive -------------------------------------------------------------------------------------
setwd("/Users/willeaton/Box/OPD Cleaning/OPD Zambia Project Cloned Git Repository/OPD_Zambia_Project_Box")

## ---- import -------------------------------------------------------------------------------------
## Import datasets

## ---- Inpatient Outpatient Facility Data  -------------------------------------------------------------------------------------
inpatient_outpatient.df <- read.csv("/Users/willeaton/Box/OPD Cleaning/inpatient_outpatient_facility_data.csv")

## ---- View dataset --------------------------------------------------------------------------------------------
## ---- View descriptives ---------------------------------------------------------------------------------------
## ---- CAN SKIP THIS SECTION OF CODE ---------------------------------------------------------------------------

#View(inpatient_outpatient.df)
names(inpatient_outpatient.df)

## Are there any missing values?
table(inpatient_outpatient.df$Year, useNA="always") # no missing values
# 2015   2016   2017   2018   2019   2020   <NA> 
#  327 209266 243851 236156 239987  69356      0 

table(inpatient_outpatient.df$Month, useNA="always") # no missing values
# Apr   Aug   Dec   Feb   Jan   Jul   Jun   Mar   May   Nov   Oct   Sep  <NA> 
# 85334 79823 77526 94197 91710 78327 78689 96413 79508 76644 80392 80380     0 

table(inpatient_outpatient.df$Province, useNA="always") # no missing values
# Central Province    Copperbelt Province       Eastern Province       Luapula Province        Lusaka Province 
# 108490                 107190                 127133                  97286                  78323 
# Muchinga Province North Western Province      Northern Province      Southern Province       Western Province 
# 60364                  90624                  85288                 128654                 115591 
# <NA> 
#    0 

table(inpatient_outpatient.df$Data_Element, useNA="always") # no missing values
# Inpatient Admissions OPD First Attendance                 <NA> 
#     174873               824070                    0 

table(inpatient_outpatient.df$Age, useNA="always") # no missing values
# (>=15)y  (1-4)y (5-14)Y    <1 y    <NA> 
#  275444  254607  244660  224232       0 

table(inpatient_outpatient.df$Gender, useNA="always") # no missing values
# Female   Male   <NA> 
#     503953 494990      0 

table(inpatient_outpatient.df$Value, useNA="always")
table(inpatient_outpatient.df$Org_Unit_ID, useNA="always")
table(inpatient_outpatient.df$Org_Unit, useNA="always")
table(inpatient_outpatient.df$District, useNA="always")

## -------------------------------------------------------------------------------------------------
## ---- CAN SKIP TO HERE ---------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------

## ---- subset to appropriate 6 Provinces  -------------------------------------------------------------------------------
#  ---- Luapula, Muchinga, Northern, Eastern, Northwestern, Western ------------------------------------------------------
#  ----n = 68 districts
sixprov.df <- inpatient_outpatient.df %>%
    filter(Province == "Luapula Province" | Province ==  "Muchinga Province" | Province == 'Northern Province' | 
               Province == 'Eastern Province' | Province == 'North Western Province' | Province ==  'Western Province')

# create smaller dataset to test out anomalize package, n = 606 observations
# test out anomalize package on oneprov
oneprov.df <- inpatient_outpatient.df %>%
    filter(Province == "Luapula Province" & District =="Mwansabombwe District" & 
            Org_Unit =="lu Mbereshi Mission Hospital")

# Note: in_out_pt_6_pro.df has n = 463,739 OPD First Attendance observations
# Inpatient Admissions OPD First Attendance                 <NA> 
#             112547               463739                    0 

##----aggregate data -------------------------------------------------------------------------------

outpatient.df <- sixprov.df[ which(sixprov.df$Data_Element=='OPD First Attendance'), ]

# test same code as above with smaller dataset, n = 326 obs
outpatient.df <-oneprov.df[ which(oneprov.df$Data_Element=='OPD First Attendance'), ]

# Note: confirmed that outpatient.df also has n = 463,739 OPD First Attedance observations
# table(outpatient.df$Data_Element, useNA = "always")
# Inpatient Admissions OPD First Attendance                 <NA>
#                   0               463739                    0 

# Are there any missing values in Value?
unique(outpatient.df$Value, useNA = "always")

#Aggregate Value Data by the HF, Month and Year
#outpatient1 <- outpatient.df %>% group_by(Year, Month, Org_Unit, District) %>% summarize(Value = sum(Value))
# commenting this out for now (on 6-4-20) so that Province is also included in dataframe
outpatient2 <- outpatient.df %>% group_by(Year, Month, Org_Unit, District, Province) %>% summarize(Total_Value = sum(Value))

#Create Date from Month, Year
outpatient2$month_num <- match(outpatient2$Month, month.abb)
outpatient2$date <- as.Date(with(outpatient2, paste(Year, month_num, 1, sep="-")), "%Y-%m-%d")

#Identify all Unique Values for District
Districts.df <- data.frame(unique(outpatient2$District)) # n = 68 unique districts
order("unique.outpatient2.District.") #is this producing what it is supposed to?

# Note: not sure what this code does?
Districts.df[,order("unique.outpatient2.District."(df))]

# ---- write to csv file
write.csv(outpatient2, file = "/Users/willeaton/Box/OPD Cleaning/OPD Zambia Project Cloned Git Repository/OPD_Zambia_Project_Box/OPD_Zambia_R_Working_Drive/csv/outpatient2.csv")

# Produce time series figures for the following districts -----------------

# [1] Kaoma District        Kaputa District       Mansa District        Manyinga District     Kawambwa District     Lukulu District       Kalumbila District   
# [8] Nalolo District       Senga District        Petauke District      Senanga District      Chifunabuli District  Chienge District      Solwezi District     
# [15] Mushindamo District   Sinda District        Sikongo District      Mungwi District       Vubwi District        Kabompo District      Mbala District       
# [22] Lundazi District      Mwense District       Zambezi District      Mwandi District       Mambwe District       Chipata District      Nchelenge District   
# [29] Chinsali District     Mufumbwe District     Chama District        Kasama District       Nsama District        Chipili District      Kalabo District      
# [36] Chavuma District      Nyimba District       Kasempa District      Shiwang'andu District Lunte District        Nakonde District      Sesheke District     
# [43] Lunga District        Katete District       Samfya District       Chilubi District      Isoka District        Luwingu District      Ikelenge District    
# [50] Mwansabombwe District Mpulungu District     Milenge District      Mporokoso District    Mwinilunga District   Mulobezi District     Mpika District       
# [57] Sioma District        Limulunga District    Luampa District       Mitete District       Mafinga District      Lavushimanda District Kanchibiya District  
# [64] Mongu District        Shang'ombo District   Chadiza District      Chembe District       Nkeyema District   

outpatient2 %>%
    filter(District=="Kaoma District") %>%
    ggplot() +
    geom_line(mapping = (aes(x=date, y=Total_Value))) +
    facet_wrap(~ Org_Unit, scales="free_y") + theme(strip.text = element_text(size=8))


# Analyze Outliers --------------------------------------------------------

# Analyze outliers: Outlier Report is available with verbose = TRUE
iqr_outliers <- iqr(mydata2, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)$outlier_report

gesd_outliers <- gesd(mydata2, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)$outlier_report

# ploting function for anomaly plots
ggsetup <- function(mydata2) {
    mydata2 %>%
        ggplot(aes(rank, value, color = outlier)) +
        geom_point() +
        geom_line(aes(y = limit_upper), color = "red", linetype = 2) +
        geom_line(aes(y = limit_lower), color = "red", linetype = 2) +
        geom_text(aes(label = index), vjust = -1.25) +
        theme_bw() +
        scale_color_manual(values = c("No" = "#2c3e50", "Yes" = "#e31a1c")) +
        expand_limits(y = 13) +
        theme(legend.position = "bottom")
}

# Visualize
p3 <- iqr_outliers %>% 
    ggsetup() +
    ggtitle("IQR: Top outliers sorted by rank") 

p4 <- gesd_outliers %>% 
    ggsetup() +
    ggtitle("GESD: Top outliers sorted by rank") 

# Show plots
p3
p4

# Alternative code for assessing outliers ---------------------------------

# try deleting the year, month, and month num variables

# How to delete first, second and seventh column
mydata <- outpatient2

# Drop the columns of the dataframe
mydata <- subset(outpatient2, select = c(Province, Total_Value, date))

# sort by date
mydata2<-mydata[order(as.Date(mydata$date, format = "%d/%m/%Y")),]

# Try Twitter and GESD Method ---------------------------------------------
mydata2 %>%    # Twitter and GESD
    time_decompose(Total_Value, method = "twitter", trend = "12 months", merge = TRUE) %>% 
    anomalize(remainder, method = "gesd") %>% 
    time_recompose() %>% 
    #Anomaly Visualization
    plot_anomalies(time_recomposed = TRUE) + #, ncol = 3, alpha_dots = 0.25)
    labs(title = "Time seriesd data - Twitter + GESD Method", x = "Time",
         y = "Total outpatient visits", subtitle = "insert subtitle")









    
