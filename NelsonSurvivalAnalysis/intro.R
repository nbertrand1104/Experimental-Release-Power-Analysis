# Pete Nelson, PhD
# Dept of Water Resources
# lifted w mods 18 April 2023
# from https://statsandr.com/blog/what-is-survival-analysis/
# goal: intro to survival analysis

# simple example -----
# create dataset
dat <- data.frame(
  time = c(3, 5, 7, 2, 18, 16, 2, 9, 16, 5),
  event = c(0, 1, 1, 1, 0, 1, 1, 1, 1, 0)
)

# subject identifies the individuals in the study
# time is the time to event in days
# event is the event status where 0 = censored (eg individual fish survives beyond the study period, sheds the tag or otherwise becomes unavailable) and 1 = event happened (ie individual died and you recovered the tag)

# run Kaplan-Meier (non-parametric, no assumptions about the distribution) estimator with the survfit() and Surv() functions
library(survival)
km <- survfit(Surv(time, event) ~ 1, # '~ 1' indicates no grouping
              data = dat)
summary(km) # note that censored data have been dropped
plot(km,
     xlab = "days",
     ylab = "survival probability",
     conf.int = FALSE)

# alt set of functions
library(survminer)
ggsurvplot(km,
           conf.int = FALSE,
           legend = "none")
# crosses on the curve indicate censored observations
# add median survival to the plot
ggsurvplot(km,
           conf.int = FALSE,
           surv.median.line = "hv",
           legend = "none") +
  labs(title = "Delta smelt survival",
       x = "days",
       y = "survival probabilty")

# find median survival, when survival is 50% or when half the fish are expected to have died
summary(km)$table["median"]
# or 
km

# bigger dataset -----
library(KMsurv)
data(tongue)
library(tidyverse)

# type is tumor DNA profile (1 = aneuploid tumor, 2 = diploid tumor)
# time is time to death or on-study time (in weeks)
# delta is the death indicator (0 = alive, 1 = dead)

anaploid <- tongue %>% filter(type == "1") # ie drop diploid data
head(anaploid)
  
fit <- survfit(Surv(time, delta) ~ 1, # right-censored so time is follow-up time; intercept only model with no groups
          data = anaploid,
          conf.type = "log-log")
fit
ggsurvplot(fit,
           surv.median.line = "hv",
           legend = "none") +
  labs(title = "patient survival",
       x = "time (weeks)",
       y = "survival probability")

# comparing 2 groups -----
dat <- data.frame(
  group = c(rep(1, 6), rep(2, 6)),
  time = c(4.1, 7.8, 10, 10, 12.3, 17.2, 9.7, 10, 11.1, 13.1, 19.7, 24.1),
  event = c(1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0))

# group tells you which group the fish belongs to
# time is the time to death
# event is the event status (0 = censored, 1 = death)...realizing after the fact that either you catch a fish or you don't and censored 'subjects' probably aren't a factor in this study!

dat

## basic comparison ----
# uses log-rank test (non-parametric)
survdiff(Surv(time, event) ~ group,
         data = dat) 
# no difference btwn groups

## alternatively ----
# test for a difference btwn the 2 groups and plot the curves at the same time
fit <- survfit(Surv(time, event) ~ group,
               data = dat)
ggsurvplot(fit,
           pval = T,
           pval.method = T) +
  labs(title = "Delta smelt survival",
       x = "days", y = "suvival probability")

# SCRATCH ###################################
library(tidyverse)
library(ggplot2)
library(reshape2)

data_coll <- read_csv("fig8.13data.csv")
mdat <- melt(data_coll, measure.vars = c("entry", 
                                         ifelse(is.na(data_coll$censored), data_coll$dead, data_coll$censored))) # bad!
ggplot(mdat,
       aes(x = ,
           y = subject,
           group = subject)) +
  geom_line(size = 4) +
  labs(title = "data collection process",
       x = NULL, y = NULL) +
  theme_minimal()
