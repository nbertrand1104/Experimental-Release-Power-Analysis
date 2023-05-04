#by Alex Jensen

#edits by Nick Bertrand

# Load necessary packages
library(simr)
library(tidyverse)
library(lme4)
library(boot)
require(dplyr)
require(ggplot2)

# Create power analysis function
  # Necessary inputs
    # Number of simulations per set of inputs
    # Sampling effort (# of trawls)
    # Base detection probability (function of release abundance)
    # Effect of soft release (multiplicative)
    # *Note: product of detection probability and soft release effect cannot exceed 1

power <- function(sims, effort, detect, effect){
  results <- numeric(sims)
  for(i in 1:sims){
    sim_data_hard <- rbinom(effort, 1, detect)
    sim_data_soft <- rbinom(effort, 1, detect*effect)
    df <- data.frame(hard=sim_data_hard, soft=sim_data_soft) %>%
      pivot_longer(cols=1:2, names_to="type")
    test<- glm(value ~ type, data = df, family = "binomial")
    results[i] <- ifelse(summary(test)$coef[8]<0.05 & summary(test)$coef[2]>0,1,0)
  }
  return(length(which(results==1))/length(results)) # this value, *100%, provides the % power
}

# Run power analysis function over a set of inputs
  # For this example, assume sims=1000, effort=500, and effect=1.3
    # Vary detect from 0.05 to 0.20

reps <- 10 # number of separate power analyses for each set of inputs
detect <- rep(c(0.05,0.1,0.15,0.2), each=reps)
sims=1000; effort=500; effect=1.3
power.df <- data.frame(sims=sims, effort=effort, detect=detect, effect=effect)

for(i in 1:length(power.df$sims)){
  power.df$power[i] <- power(sims=sims, effort=effort, detect=detect[i], effect=effect)
  i
}

ggplot(data=power.df, aes(x=as.factor(detect), y=power*100))+
  geom_boxplot()+
  labs(x="Base Detection Probability", y="Power (%)")
