###############################################################
# HW Week 14 
###############################################################


###########################################
# OBJECTIVE 1 
###########################################

# A 
# -----------------------------------------

# I used Nathan's tick data 
# Ecological Scenario: For our newly-discovered tick species Tickus bittus, we are interested in seeing if the different life stages (nymphs and adults) have different vulnerability to desiccation, and how this might affects their questing patterns.

#Our question: How does sensitivity of questing duration to a humidity index differ between tick nymphs and adults?

#Continuous response variable (questing_duration, in minutes), continuous predictor variable (humidity_index, does not exist outside of this exercise), and two-level factor (life_stage).

#loading libraries 
library(tidyverse)   #data manipulation, dplyr 
library(ggpubr)      #creating easily publication ready plots
library(rstatix)     # pipe friendly stat analyses
library(broom)       #printing summary of stat tests as data frames
library(datarium)   #contains example datasets from this
library(dplyr)       #so we can pipe 
library(emmeans)     # so we can do a post-hoc test like Tukey for ANCOVA
library(lmtest)      # so we can do the lm test function 

#loading data 
ticks = read.csv("tick_data.csv")

# B 
# ------------------------------------------


#Initial data visualization 

#y = mx + b 
#questing_duration ~ humidity*life_stage + b + e 

#Plots 
 
base_plot = ggplot(ticks, aes(x = humidity_index, y = questing_duration)) + 
  geom_point(aes(color = life_stage))
base_plot + 
  labs(title = "Ticks by Lifestage", 
       x = "Humidity Index",
       y = "Questing Duration") 
# we want to post the base plot + a title, but we don't want to save the title to the baseplot because we will be changing this quite a lot 

base_plot + 
  facet_wrap(~life_stage, nrow = 1)  #visualizing the baseplot but facet wrapped 

# Full model 

model_full = aov(questing_duration~life_stage*humidity_index, data = ticks)
summary(model_full)

# model without interaction 
model_reduced = aov(questing_duration~life_stage + humidity_index, data = ticks)
summary(model_reduced)

# C 
# ------------------------------------------

logLik(model_full)
logLik(model_reduced)

# Cool! The yhave hte same log likelihood, but the full one has 5 degrees of freedom while the reduced one has four degrees of freedom 

# D 
# --------------------------------------------

lrtest(model_full)
lrtest(model_reduced)

# these tests are comparing the full model against a null model with only an intercept term, and the reduced model against a null model with only an intercept term. 
# model_full vs reduced shows that we added 3 degrees of freedom, 

###########################################
# OBJECTIVE 2
###########################################