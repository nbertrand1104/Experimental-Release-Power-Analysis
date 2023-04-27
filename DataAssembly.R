#this script assembles the simple data file for Pete Nelson data analysis
#by Nick Bertrand  4/20/2023
#nbertrand@usbr.gov
library(readr)
library(tidyverse)

setwd("C:/Users/nbertrand/Desktop/Bertrand/GitHub/ExperimentalReleasePowerAnalysis")
#imports data about the release events
ReleaseEventData <- read_csv("data/ReleaseEventData.csv", 
                             col_types = cols(start_date = col_date(format = "%Y-%m-%d"), 
                                              End_date = col_date(format = "%Y-%m-%d")))
#View(ReleaseEventData)

#imports data on the recaptured delta smelt
WY2023RecaptureData <- read_csv("data/WY2023RecaptureData.csv", 
                                col_types = cols(Date = col_date(format = "%Y-%m-%d")))
#View(WY2023RecaptureData)

#Joins release event data to the recapture data
fulldata <- left_join(WY2023RecaptureData, ReleaseEventData, by = "Tag")
#view(fulldata)


nelson_data <-fulldata %>% 
  #filter all unmarked fish from the dataset
  filter(Marked != "unmarked") %>% 
  #filter all trailer fish from the dataset
  filter(release_type != "trailer") %>% 
  #drops one broodstock collection without tag data
  drop_na(Tag) %>% 
  #calculates the days from release to recapture
  mutate(daystorecap = as.numeric(Date-End_date)) %>% 
  #selects down to relevant columns
  #select(release_type,daystorecap, site, num_fish)
  select(release_type, End_date, Date, daystorecap, site, num_fish)
  
view(nelson_data)
#writes .csv file to data directory
#write.csv(nelson_data, file = "data/DeltaSmeltDataNelson.csv") 
 
  
  