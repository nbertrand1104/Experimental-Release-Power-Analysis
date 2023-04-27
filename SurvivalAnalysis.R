#by Nick Bertrand
#nbertrand@usbr.gov

#attempts to design a power analysis based on a survival analysis using data from the 
#Delta Smelt Experimental release recapture data


# install.packages(c("survival", "lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
# remotes::install_github("zabore/condsurv")
# remotes::install_github("zabore/ezfun")
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)
library(readr)

# imports the data assembled by DataAssembly.R in data directory
DeltaSmeltDataNelson <- read_csv("data/DeltaSmeltDataNelson.csv", 
                                 col_types = cols(...1 = col_skip()))
#View(DeltaSmeltDataNelson)

#changes object to shorter name and assigns numeric value to release type
dssurv <- DeltaSmeltDataNelson %>% 
  mutate(status = ifelse(release_type == "hard", 0, 1))

#view(dssurv)
#str(dssurv)

s1 <- survfit(Surv(daystorecap, status)~ 1, data = dssurv)%>%
  #creates line graph of the survival object
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability")+ 
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

survfit(Surv(daystorecap, status)~ site, data = dssurv)










