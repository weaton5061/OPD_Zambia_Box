## ---- METADATA ------------------------------------------------------------------------------------------------------
# Author: Will Eaton
# Purpose: Zambia DiD analysis of PAMO project iCCM impact on malaria IPD and/or death (Luapula, Northern, Muchinga)
# Notes: Ruth requests concentrating on 6 Provinces: Luapula, Muchinga, Northern, Eastern, Northwestern, Western
#        n = approx 70 districts total
# Created: 06-04-2020
# Last updated: 06-07-2020
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
# 8) Idea: subset and make a bunch of independent subset tibbles grouped by Org_Unit_ID and then create loop code to run on each tibble

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

# Ensure class conversion executed appropriately --------------------------
lapply(inpatient_outpatient.df, class)


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
                Org_Unit_ID == 1763 | Org_Unit_ID == 1709)

# create health facility dataset, n = 
hf <- subset(sixprov.df, select = c(Province, District, Org_Unit_ID))
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

# Try Twitter and GESD Method ---------------------------------------------
# The workflow of anomalize is divided into three parts:
#     
# 1) Time series decomposition with time_decompose(). - The measured value or the numerical value on which detection needs to be performed for a particular group 
#    is decomposed into four columns that are observed, season, trend, and remainder. The default method used for decomposition is stl, which is a seasonal decomposition utilizing a Loess smoother.
#    There is a second technique which you can use for seasonal decomposition in time series based on median that is the Twitter method which is also used AnomalyDetection package. 
#    It is identical to STL for removing the seasonal component. The difference is in removing the trend is that it uses piece-wise median of the data(one or several median split at specified intervals) rather than fitting a smoother. 
#    This method works well where seasonality dominates the trend in time series.
# 2) Anomaly detection of remainder with anomalize().
# 3) Anomaly lower and upper bound transformation with time_recompose()

# test out code from website:

# Time series decomposition -----------------------------------------------
mydata2 %>% 
    ungroup()
mydata2_anomaly <-  mydata2 %>% 
    time_decompose(Total_Value)
mydata2_anomaly
mydata2_anomaly %>% glimpse()


# Detect anomalies in the remainder ---------------------------------------
mydata2_anomaly = mydata2_anomaly %>% 
    anomalize(remainder)
mydata2_anomaly
mydata2_anomaly %>% glimpse()

# Anomaly lower and upper bound transformation ----------------------------
mydata2_anomaly = mydata2_anomaly %>% 
    time_recompose()

mydata2_anomaly %>% glimpse()
mydata2_anomaly
View(mydata2_anomaly)

# Visualize the inner workings of how algorithm detects anomalies, plot anomaly decomoposition
mydata2 %>%
    ungroup() %>%
    time_decompose(Total_Value) %>%
    anomalize(remainder) %>%
    plot_anomaly_decomposition() +
    labs(title = "Anomaly Decomposition Default")

# Adjusting parameters for Anomaly detection
# THIS IS WORKING!!!!
mydata2 %>%
    ungroup() %>% # group or ungroup
    time_decompose(Total_Value, method = "twitter") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.20) %>%
    time_recompose() %>% 
    plot_anomalies(time_recompose = T) +
    labs(title = "Anomaly Default")

# plot_anomaly_decomposition() for visualizing the inner workings of how 
# algorithm detects anomalies in the “remainder”.
mydata2 %>%
    ungroup() %>% 
    time_decompose(Total_Value, frequency = "auto", trend = "auto", method = "twitter", merge = TRUE) %>%  # consider frequency = 12, however obtain more anomalies
    anomalize(remainder, method = "gesd") %>%
     time_recompose() %>%
    plot_anomaly_decomposition() +
    ggtitle("Freq/Trend = 'auto'")

# Visualize line plot for comparison
mydata2 %>%
    filter(Org_Unit_ID==1763) %>%
    ggplot() +
    geom_line(mapping = (aes(x=date, y=Total_Value))) + 
    facet_wrap(~ Org_Unit_ID, scales="free_y") + theme(strip.text = element_text(size=8))

# Plot anomalies (THIS IS WORKING TOO - on one heatlth facility)
mydata2 %>%    # Twitter and GESD
    ungroup() %>% 
    time_decompose(Total_Value, method = "twitter") %>% #, # The time series decomposition method. One of "stl" or "twitter". The
                   # STL method uses seasonal decomposition (see decompose_stl()). The Twitter
                   # method uses trend to remove the trend (see decompose_twitter()).
                   #target = "Org_Unit_ID",
                   #frequency = "auto",  # Controls the seasonal adjustment (removal of seasonality). Input can be either "auto", a time-based definition (e.g. "2 weeks"), or a numeric number of obser- vations per frequency (e.g.)
                   # Consider frequency = "auto" or 12, however obtain more anomalies if 12
                   #trend = "auto", # Controls the trend component For stl, the trend controls the sensitivity of the lowess smoother, which is used to remove the remainder. 
                   # For twitter, the trend controls the period width of the median, which are used to remove the trend and center the remainder.
                   #merge = TRUE) %>%  # A boolean. FALSE by default. If TRUE, will append results to the original data.
    anomalize(remainder, method = "gesd",   # The GESD Method (Generlized Extreme Studentized Deviate Test) progressively eliminates out- liers using a Student’s T-Test comparing the test statistic to a critical value. 
                                            # Each time an outlier is removed, the test statistic is updated. Once test statistic drops below the critical value, all outliers are considered removed. Because this method involves continuous updating via a loop, it is slower than the IQR method. 
                                            # However, it tends to be the best performing method for outlier removal.
               alpha = 0.05,               # We can decrease alpha, which increases the bands making it more difficult to be an outlier. In reality, you’ll probably want to leave alpha in the range of 0.10 to 0.02, but it makes a nice illustration of how you can also use max_anoms to ensure only the most aggregious anomalies are identified.
               max_anoms = 0.20) %>%            # the maximum percentage of data that can be an anomaly
               #verbose = TRUE) %>%         # return a report of useful information related to the outliers
    time_recompose() %>% 
    #Anomaly Visualization
    #plot_anomalies(ncol = 3, alpha_dots = 0.25) # this line of code plots without the grey area
    plot_anomalies(time_recomposed = TRUE) + #, ncol = 3, alpha_dots = 0.25)
    labs(title = "Time seriesd data - Twitter + GESD Method", x = "Time",
         y = "Total outpatient visits") #subtitle = "insert subtitle" 

# try for two health facilities
gapply(inpatient_outpatient.df, class)
gapply(mydata2, which, FUN, form, level, groups, ...)

# defining a function
anomalies_health_fac <- function(anomalies_hf) { 
    mydata2 %>%
        ungroup() %>% # group or ungroup
        time_decompose(Total_Value, method = "twitter") %>%
        anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.20) %>%
        time_recompose() %>% 
        plot_anomalies(time_recompose = T) +
        labs(title = "Anomaly Default")
}

# 6-7-10 LEFT OFF HERE - Try to figure out how to run code on groups with use of ungroup function.

# run function by group
gapply(mydata2, anomalies_health_fac)

# run function by group on tibbles
group_map(mydata2, anomalies_health_fac, .keep = FALSE)




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




for (i in mydata2$Org_Unit_ID)
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







    
