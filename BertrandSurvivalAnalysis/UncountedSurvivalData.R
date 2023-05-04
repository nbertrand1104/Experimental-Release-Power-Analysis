#Nick Bertrand
#nbertrand@usbr.gov
#5/4/2023

#goal is to ressemble data on experimentally released fish accounting for every fish released and 
#then assigning a censorsed or uncesored status to feed back to the survival analysis

library(readr)
library(tidyverse)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)
library(readr)

ReleaseEventData <- read_csv("data/ReleaseEventData.csv", 
                             col_types = cols(start_date = col_date(format = "%Y-%m-%d"), 
                                              End_date = col_date(format = "%Y-%m-%d")))
View(ReleaseEventData)

uc_DS <- ReleaseEventData %>% 
  select(num_fish, start_date, End_date, site, release_type, Tag) %>% 
  uncount(num_fish) %>% 
  group_by(Tag) %>% 
  mutate(ID = row_number()) %>% 
  unite(TagID, Tag:ID )
?unite
view(uc_DS)

#imports data on the recaptured delta smelt
WY2023RecaptureData <- read_csv("data/WY2023RecaptureData.csv", 
                                col_types = cols(Date = col_date(format = "%Y-%m-%d")))
View(WY2023RecaptureData)


uc_recap <- WY2023RecaptureData %>% 
  group_by(Tag) %>% 
  mutate(ID = row_number())%>% 
  unite(TagID, Tag,ID )

view(uc_recap)

uc_final <- left_join(uc_DS,uc_recap, by = "TagID") %>% 
  separate_wider_delim("TagID", delim = "_", names = c("Tag", "ID"))

view(uc_final)

uc_data <- uc_final %>% 
  #filter all trailer fish from the dataset
  filter(release_type != "trailer") %>% 
  #calculates the days from release to recapture
  mutate(daystorecap = as.numeric(Date-End_date))# %>% 
  #selects down to relevant columns
  #select(release_type,daystorecap, site, num_fish)
  #select(release_type, End_date, Date, daystorecap, site)

#view(uc_data)

#changes object to shorter name and assigns numeric value to release type
dssurv <- uc_data %>% 
  mutate(release_code = ifelse(release_type == "hard", 2, 1)) %>% 
  mutate(status = ifelse(Survey == "NA", 0,1)) %>% 
  replace_na(list(status = 0))

view(dssurv)

Surv(dssurv$daystorecap,dssurv$status)

s1 <- survfit(Surv(daystorecap, status)~ 1, data = dssurv)%>%
  #creates line graph of the survival object
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall probability of failure to recapture fish")+ 
  add_confidence_interval() +
  add_risktable()


s1


#uses summary function to generate statistics for probability of survival past as specific time interval
prob_surv_20 <- summary(survfit(Surv(daystorecap, status)~ 1, data = dssurv), times = 20)

prob_surv_20

#creates a quick table of the probability of surviving past a specific time.
#I randomly chose 20 days
survfit(Surv(daystorecap, status)~ 1, data = dssurv) %>% 
  tbl_survfit(
    times = 20,
    label_header =  "**20 Day survival (95% CI)**")


#Median Survival times
#Survival times are not expected to be normally distributed so the mean 
#is not an appropriate summary.

survfit(Surv(daystorecap, status)~ 1, data = dssurv)

#makes a quick table
survfit(Surv(daystorecap, status)~ 1, data = dssurv) %>% 
  tbl_survfit(
    probs = 0.5,
    label_header = "**Median survival (95% CI)**")


#Comparing survival times between groups

survdiff(Surv(daystorecap, status)~ release_code, data = dssurv)


survdiff(Surv(daystorecap, status)~ site, data = dssurv)

#Cox Regression model
#The quantity of interest from a Cox regression model is a hazard ratio (HR). 
#The HR represents the ratio of hazards between two groups at any particular point in time.

# I am uncertain this is the correct format for multiple variables
coxph(Surv(daystorecap, status)~ release_code, data = dssurv) %>% 
  tbl_regression(exp = TRUE) 

#A HR < 1 indicates reduced hazard of death 
#whereas a HR > 1 indicates an increased hazard of death.

#my interpretation of the above sentence for this project
# An HR < 1 indicates a reduced chance of recapture
# An HR > 1 indicates an increased chance of recapture

coxph(Surv(daystorecap, status)~ release_code + site, data = dssurv) %>% 
  tbl_regression(exp = TRUE) 


#####################################
#devtools::install_github("LucyMcGowan/survivalpwr")

library(survivalpwr)


#Cox Regression power calculation 

#used the survival probability for 20 day which was the median surival from the dataset
# for the event probability 
pwr_coxph(
  hr = 0.74,
  eventprob = 0.5,
  n = 50000)

#power calculation to determine sample size/number of events from a target power
pwr_coxph(
  hr = 0.74,
  eventprob = 0.05,
  power = 0.80)

#incorporates a r-squared for covariates that explain some variation in the predictor of interest. 
pwr_coxph(
  hr = 1.5,
  eventprob = 0.5,
  power = 0.80,
  rsquare = 0.20)

