### Technology Adoption
# We conclude that there has been strong convergence in use of consumption technologies with 
# somewhat slower and more partial convergence in production technologies. This reflects 
# considerably stronger global convergence in quality of life than in income, but we note that 
# universal convergence in use of production technologies is not required for income 
# convergence (only that countries are approaching the technology frontier in the goods and 
# services that they produce).

###########################################################################################
### Load Packages
###########################################################################################
library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)
library(skimr)
library(janitor)
library(broom)
library(tidymodels)
library(GGally)
library(tidytext)
library(textrecipes)
library(themis)





###########################################################################################
### Load Data
###########################################################################################
tuesdata <- tidytuesdayR::tt_load('2022-07-19')
tech <- tuesdata$technology

head(tech)
names(tech)
dim(tech)
skim(tech)
lapply(tech, class)







