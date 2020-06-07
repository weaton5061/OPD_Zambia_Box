## ---- METADATA ------------------------------------------------------------------------------------------------------
# Author: Will Eaton
# Purpose: Zambia DiD analysis of PAMO project iCCM impact on malaria IPD and/or death (Luapula, Northern, Muchinga)
# Notes: Ruth requests concentrating on 6 Provinces: Luapula, Muchinga, Northern, Eastern, Northwestern, Western
#        n = approx 70 districts total
# Created: 06-04-2020
# Last updated: 06-06-2020
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

## ---- subset to appropriate n= 6 Provinces  -------------------------------------------------------------------------------
#  ---- Luapula, Muchinga, Northern, Eastern, Northwestern, Western ------------------------------------------------------
#  ----n = 68 Districts, over 1000 health facilities
sixprov.df <- inpatient_outpatient.df %>%
    filter(Province == "Luapula Province" | Province ==  "Muchinga Province" | Province == 'Northern Province' | 
               Province == 'Eastern Province' | Province == 'North Western Province' | Province ==  'Western Province')

# create smaller dataset to test out anomalize package, n = 606 observations, n = 7 health facilities
# Org_Unit_IDs: [1] 1763 1764 1709 1728 1837 1627 1784
# Org_Units :   [1] lu Mbereshi Mission Hospital                          lu Mbereshi Mission Hospital Affiliated Health Centre
#               [3] lu Kazembe Zonal Rural Health Centre                  lu Lufubu Rural Health Centre                        
#               [5] lu Salanga Rural Health Centre                        lu Chipunka Rural Health Centre                      
#               [7] lu Mukamba Rural Health Centre  
# Test out anomalize package on oneprov
oneprov.df <- inpatient_outpatient.df %>%
    filter(Province == "Luapula Province" & District =="Mwansabombwe District" &
               Org_Unit_ID == 1837)

# create health facility dataset, n = 
hf <- subset(sixprov.df, select = c(Province, District, Org_Unit))
hf_names<-distinct(hf)
# sort health facilities by multiple columns
hf_names_2 <- hf_names[
    with(hf_names, order(Province, District)),
    ]


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


#attempt to change factors to character variables
# is this necessary?
# outpatient.df %>% mutate_if(is.factor, as.character) -> outpatient.df


#Aggregate Value Data by the HF, Month and Year
#outpatient1 <- outpatient.df %>% group_by(Year, Month, Org_Unit, District) %>% summarize(Value = sum(Value))
# commenting this out for now (on 6-4-20) so that Province is also included in dataframe
# outpatient2 <- outpatient.df %>% group_by(Year, Month, Org_Unit, District, Province) %>% summarize(Total_Value = sum(Value))
outpatient2 <- outpatient.df %>% group_by(Year, Month, Org_Unit_ID, District, Province) %>% summarize(Total_Value = sum(Value))


#Create Date from Month, Year
outpatient2$month_num <- match(outpatient2$Month, month.abb)
outpatient2$date <- as.Date(with(outpatient2, paste(Year, month_num, 1, sep="-")), "%Y-%m-%d")

#Identify all Unique Values for District
# Districts.df <- data.frame(unique(outpatient2$District)) # n = 68 unique districts
# order("unique.outpatient2.District.") #is this producing what it is supposed to?
# 
# # Note: not sure what this code does?
# Districts.df[,order("unique.outpatient2.District."(df))]

# ---- write to csv file
# write.csv(outpatient2, file = "/Users/willeaton/Box/OPD Cleaning/OPD Zambia Project Cloned Git Repository/OPD_Zambia_Project_Box/OPD_Zambia_R_Working_Drive/csv/outpatient2.csv")

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

# -- this produces faceted time series line plot --
# outpatient2 %>%
#     filter(District=="Mwansabombwe District") %>%
#     ggplot() +
#     geom_line(mapping = (aes(x=date, y=Total_Value))) +
#     facet_wrap(~ Org_Unit, scales="free_y") + theme(strip.text = element_text(size=8))


# Analyze Outliers --------------------------------------------------------

# Drop the columns of the dataframe
# mydata <- subset(outpatient2, select = c(Province, Total_Value, date)) # Province, District, Org_Unit
mydata <- subset(outpatient2, select = c(Org_Unit_ID, Total_Value, date)) # Province, District, Org_Unit

# sort by date
#mydata2<-mydata[order(as.Date(mydata$date, format = "%d/%m/%Y")),]
mydata2<-mydata[order(as.Date(mydata$date, format = "%Y-%m-%d")),]


# Try Twitter and GESD Method ---------------------------------------------

mydata2 %>%    # Twitter and GESD
    time_decompose(Total_Value, method = "twitter", # The time series decomposition method. One of "stl" or "twitter". The
                   # STL method uses seasonal decomposition (see decompose_stl()). The Twitter
                   # method uses trend to remove the trend (see decompose_twitter()).
                   #target = "Org_Unit_ID",
                   frequency = "auto",  # Controls the seasonal adjustment (removal of seasonality). Input can be either "auto", a time-based definition (e.g. "2 weeks"), or a numeric number of obser- vations per frequency (e.g.)
                   # Consider frequency = "auto" or 12, however obtain more anomalies if 12
                   trend = "auto", # Controls the trend component For stl, the trend controls the sensitivity of the lowess smoother, which is used to remove the remainder. 
                   # For twitter, the trend controls the period width of the median, which are used to remove the trend and center the remainder.
                   merge = TRUE) %>%  # A boolean. FALSE by default. If TRUE, will append results to the original data.
    anomalize(remainder, method = "gesd",  
               alpha = 0.05,               # We can decrease alpha, which increases the bands making it more difficult to be an outlier. In reality, you’ll probably want to leave alpha in the range of 0.10 to 0.02, but it makes a nice illustration of how you can also use max_anoms to ensure only the most aggregious anomalies are identified.
               max_anoms = 0.20) %>%            # the maximum percentage of data that can be an anomaly
               #verbose = TRUE) %>%         # return a report of useful information related to the outliers
    time_recompose() %>% 
    #Anomaly Visualization
    #plot_anomalies(ncol = 3, alpha_dots = 0.25) # this line of code plots without the grey area
    plot_anomalies(time_recomposed = TRUE) + #, ncol = 3, alpha_dots = 0.25)
    labs(title = "Time seriesd data - Twitter + GESD Method", x = "Time",
         y = "Total outpatient visits") #subtitle = "insert subtitle" 


# details
# mydata2 %>% glimpse()

# plot_anomaly_decomposition() for visualizing the inner workings of how 
# algorithm detects anomalies in the “remainder”.
# mydata2 %>%
#     time_decompose(Total_Value, frequency = "auto", trend = "auto", method = "twitter", merge = TRUE) %>%  # consider frequency = 12, however obtain more anomalies
#     anomalize(remainder, method = "gesd") %>% 
#     time_recompose() %>% 
#     plot_anomaly_decomposition() +
#     ggtitle("Freq/Trend = 'auto'")




for (i in hf_names_2)
{
    mydata2 %>%    # Twitter and GESD
        time_decompose(Total_Value, method = "twitter", # The time series decomposition method. One of "stl" or "twitter". The
                       # STL method uses seasonal decomposition (see decompose_stl()). The Twitter
                       # method uses trend to remove the trend (see decompose_twitter()).
                       frequency = "auto", # Controls the seasonal adjustment (removal of seasonality). Input can be either "auto", a time-based definition (e.g. "2 weeks"), or a numeric number of obser- vations per frequency (e.g.)
                       # Consider frequency = "auto" or 12, however obtain more anomalies if 12
                       trend = "auto", # Controls the trend component For stl, the trend controls the sensitivity of the lowess smoother, which is used to remove the remainder. 
                       # For twitter, the trend controls the period width of the median, which are used to remove the trend and center the remainder.
                       merge = TRUE) %>%  # A boolean. FALSE by default. If TRUE, will append results to the original data.
        anomalize(remainder, method = "gesd", 
                  alpha = 0.05,               # We can decrease alpha, which increases the bands making it more difficult to be an outlier. In reality, you’ll probably want to leave alpha in the range of 0.10 to 0.02, but it makes a nice illustration of how you can also use max_anoms to ensure only the most aggregious anomalies are identified.
                  max_anoms = 0.20,           # the maximum percentage of data that can be an anomaly
                  verbose = TRUE) %>%         # return a report of useful information related to the outliers
        time_recompose() %>% 
        #Anomaly Visualization
        #plot_anomalies(ncol = 3, alpha_dots = 0.25) # this line of code plots without the grey area
        plot_anomalies(time_recomposed = TRUE) #+ #, ncol = 3, alpha_dots = 0.25)
    labs(title = "Time seriesd data - Twitter + GESD Method", x = "Time",
         y = "Total outpatient visits", subtitle = "insert subtitle") 
}







    
