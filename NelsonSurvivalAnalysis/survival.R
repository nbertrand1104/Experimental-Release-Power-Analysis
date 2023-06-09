# Pete Nelson, PhD
# Dept of Water Resources
# 20 April 2023
# with Nick Bertrand, USBR, and others
# Study design: compare two release strategies for Delta smelt, hard v soft, using elapsed time from release until recapture as a proxy for survival. Hypothesis: soft (experimental treatment) release results in improved survival, evidenced by longer times at liberty.
# Questions: is there a treatment effect (ie difference in survival between fish subject to hard v soft release)? What is the minimum sample size required to detect a treatment effect of 20% (power analysis)?

# read in data ----
library(tidyverse)
library(lubridate)
read_csv("DeltaSmeltDataNelson.csv")

## data mod -----
dat <- read_csv("DeltaSmeltDataNelson.csv") %>% 
  select(-1) %>% 
  mutate(group = as_factor(release_type),
         release = End_date, recapture = Date,
         time = daystorecap,
         site = as_factor(site),
         num = num_fish,
         .keep = "unused")

print(dat, n = Inf)
  
# site is the release site (Rio Vista or SDWSC)
# group tells you which treatment (release type, hard or soft) the fish belongs to
# release is the date fish were released
# recapture is the date fish were recaptured
# time is the number of days from release to recapture (ie survival)
# num is the number of fish released

# treatment effect ----
library(survival)

# allows right-censored data only?
survdiff(Surv(time) ~ group, data = dat) 
# no treatment effect

survdiff(Surv(time) ~ group + site, data = dat) 
# no treatment effect, no site effect

## alternatively ----

fit <- survfit(Surv(time) ~ group,
               data = dat)
print(fit)

library(ggplot2)
library(survminer)
ggsurvplot(fit,
           pval = T,
           pval.method = T,
           surv.median.line = "hv") +
  labs(title = "Delta smelt survival",
       x = "days", y = "suvival probability")

# power analysis -----
# https://www2.ccrb.cuhk.edu.hk/stat/survival/Lachin1981.htm

# set alpha = 0.05
# set 1-beta = 1-0.8 = 0.2
# set sigma, minimum hazards ratio (control:experimental) = 0.9
# control median survival = mc, calling treatment "hard" the control
(Ms <- median(dat[dat$group == "hard",]$time)) # control median survival (days)
(Qc <- sum(dat[dat$group == "hard",]$num) / sum(dat$num)) # proportion in control group
(Qe <- sum(dat[dat$group == "soft",]$num) / sum(dat$num)) # proportion in experimental group
# or Qe = 1 - Qc, but the line above provides a good check on your code

# number of days between first & last release
dat %>% summarise(day(max(release) - day(min(release))))
T0 <- 27
  
# accrual duration (days), the length of time to add fish to the pool
(Td <- max(dat$time) - T0) # duration (days), the number of days to the end of the study, T-T0

# N = 5600, suggesting that numbers used are probably ball-park or better (ie ca 6500 is super good enough)
# This is based on the questionable assumption that recapture rates are purely a function of survival, and don't vary over the study period. 

# more resources -----
# the mark-recapture resources available for R are almost certainly important
# the package 'powerSurvEpi' seems worth a look
# this looks like a really good "next step" for understanding survival analyses: https://rviews.rstudio.com/2022/09/06/deep-survival/ 
# ^ with lots of references
# there's an open-source power analysis tool available here
# https://www.psychologie.hhu.de/arbeitsgruppen/allgemeine-psychologie-und-arbeitspsychologie/gpower
# that I've installed but not yet tried
# package: survivalpwr