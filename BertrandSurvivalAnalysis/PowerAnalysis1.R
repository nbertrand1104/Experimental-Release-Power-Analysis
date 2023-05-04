#Nick Bertrand 
#nbertrand@usbr.gov

#initial experimenting with different types of power analysis
#probably not relevant to overall project.

library(tidyverse)
library(pwr)

# paired t-test
pwrpt <- pwr.t.test(d=0.2,
                    n=51,
                    sig.level=0.05,
                    type="paired",
                    alternative="two.sided")
# inspect
pwrpt
?pwr.anova.test()
#one way Anova
# calculate minimal sample size
pwr.anova.test(k=18,            # 5 groups are compared
               f=.25,          # moderate effect size
               sig.level=.05,  # alpha/sig. level = .05
               power=.8)       # confint./power = .8

# calculate minimal sample size
pwr.anova.test(k=6,            # 5 groups are compared
               f=.25,          # moderate effect size
               sig.level=.05,  # alpha/sig. level = .05
               n=)           # n of participants

##############################
#########
######
#
library(powerMediation)
sample_size <-SSizeLogisticBin(p1 = 0.2,
                               p2 = 0.3,
                               B = 0.5,
                               alpha = 0.05,
                               power = 0.8) 

sample_size




