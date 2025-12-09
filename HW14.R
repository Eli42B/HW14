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

# model_full vs reduced shows that we added 3 degrees of freedom
# log likelihood is showing how well the model fits the data, higher log likelihood = better fit 
# so bigger number = better 
# full model is better than the null hypothesis 

# model_reduced vs null shows we added two degrees of freedom 
#  the reduced model is also better than the null hypothesis 

# both models have a chisq probability of improvement by about 45 x (ie these models are much better than guessing) 
# both models have a similar increase in improvement

# this is similar to backwards regression in that we are finding that hte interaction doesn't improve much 

###########################################
# OBJECTIVE 2
###########################################

# A 
# ------------------------------------------

# loading libraries 
library(AICcmodavg)

# filtering and subsetting data 
tick_adults = ticks %>% 
  filter(life_stage == "Adults")

tick_nymphs = ticks %>% 
  filter(life_stage == "Nymphs") 

# candidate models 

model1 = lm(questing_duration ~ life_stage*humidity_index, data = ticks)    # full model + interaction 
model2 = lm(questing_duration~life_stage + humidity_index, data = ticks)    # full model w/out interaction 
model3 = lm(questing_duration ~ humidity_index, data = tick_adults)         # model of adults 
model4 = lm(questing_duration ~ humidity_index, data = tick_nymphs)         # model of nymphs 
model5 = lm(questing_duration ~ 1, data = ticks)                            # null model 


# AICc 

model_list = list(model1, model2, model3, model4, model5) 
aictab(cand.set = model_list, second.ord = TRUE)

# B 
# --------------------------------------------

# AICc liked 4 the most, closely followed by model 3  
# model 4 accounted for 74% of the cumulative weight of hte models' importance, followed by model 3 
# Model 4 was the model which only looked at humidity index among nymphs, while model 3 was the closely related model that only looked at data from adults 
# in other words, since AICc punishes pointless extra factors in your model, it's unsurprising htat it preferred hte simpler models 

summary(model4)

# Ecologically, we are seeing that ticks are willing to quest for victims for longer periods of time when the humidity index increases 