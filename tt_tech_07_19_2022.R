### Technology Adoption
# We conclude that there has been strong convergence in use of consumption technologies with 
# somewhat slower and more partial convergence in production technologies. This reflects 
# considerably stronger global convergence in quality of life than in income, but we note that 
# universal convergence in use of production technologies is not required for income 
# convergence (only that countries are approaching the technology frontier in the goods and 
# services that they produce).

# Just did EDA and did not go much into detail. Not the most interesting dataset.


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

tech2 <- tech %>%
    group_by(iso3c, year) %>%
    mutate(meanval = mean(value))
tech2$variable <- as.factor(tech2$variable)
tech2$iso3c <- as.factor(tech2$iso3c)

head(tech)
names(tech)
dim(tech)
skim(tech$value)
lapply(tech, class)
# names below
# "variable" "label"    "iso3c"    "year"     "group"    "category" "value"   


###########################################################################################
### EDA
###########################################################################################
# variable EDA
tech %>%
    count(variable, sort = T)
tech %>%
    mutate(variable = fct_lump(variable, 10)) %>%
    summarise(n = n(), variable = variable) %>%
    filter(variable != 'Other') %>%
    ggplot(aes(variable, n)) +
    geom_col() + 
    scale_y_continuous()
tech %>%
    mutate(variable = fct_lump(variable, 10)) %>%
    group_by(variable) %>%
    summarise(n = n(), mean_val = mean(value)) %>%
    ggplot(aes(variable, mean_val)) +
    geom_point() 

tech2 %>%
    filter(year >= '1950') %>%
    ggplot(aes(year, meanval)) +
    geom_line() +
    facet_wrap(~ fct_lump(variable, 10))

# Most production since 1950 comes from things like agriculture, electricity, aviation, railing,
# other








# label EDA (Going to ignore because too much noise)
tech %>%
    count(label, sort = T)








# iso3c (looks like country codes) 
tech %>%
    count(iso3c, sort = T)

tech %>% # 
    filter(variable == 'railline') %>%
    mutate(iso3c = fct_lump(iso3c, 10)) %>%
    group_by(iso3c) %>% # need group by to get mean for each iso, not just total mean of value
    summarise(n = n(), mean_value = mean(value), iso3c = iso3c) %>%
    ggplot(aes(iso3c, mean_value)) +
    geom_point()

tech %>%  # need to figure how to add facet wrap of variable
    mutate(iso3c = fct_lump(iso3c, 20))
    group_by(iso3c) %>%
    summarise(n = n(), mean_val = mean(value)) %>%
    ggplot(aes(variable, mean_val)) +
    geom_pint() +

tech %>% 
    filter(year >= '1950') %>%
    mutate(iso3c = fct_lump(iso3c, 30)) %>%
    group_by(iso3c) %>%
    summarise(n = n(), mean_val = mean(value)) %>%
    arrange(desc(mean_val))

tech2 %>%
    filter(year >= '1950',
           iso3c %in% c('USA', 'JPN', 'DEU', 'CAN', 'GBR', 'FRA', 'ITA')) %>%
    ggplot(aes(iso3c, meanval)) +
    geom_col() +
    facet_wrap(~ fct_lump(variable, 11), scales = 'free')
    






# year (starts from 1820 to 2020)
tech %>%
    count(year, sort = T) %>%
    summarise(min_year = min(year), max_year = max(year))

tech %>% # how would I facet wrap countries?
    mutate(iso3c = fct_lump_n(iso3c, 12)) %>%
    filter(year >= '2000', iso3c != 'Other') %>%
    group_by(year, iso3c) %>%
    summarise(value = sum(value), meanval = mean(value)) %>%
    ggplot(aes(year, value)) +
    geom_col()  +
    facet_wrap(~ iso3c)
tech %>% 
    filter(year >= '2000') %>%
    ggplot(aes(year, value)) +
    geom_col()  +
    facet_wrap(~ iso3c)


    # this is better
test <- tech %>%
    group_by(iso3c, year) %>%
    mutate(meanval = mean(value))

test %>%
    filter(year >= '1950',
           iso3c %in% c('USA', 'FRA', 'GBR', 'DEU', 'RUS', 'CHN')) %>%
    ggplot(aes(year, meanval)) +
    geom_line() +
    facet_wrap(~ iso3c)



# group 
tech %>%
    count(group, sort = T) # remove creation
tech %>%
    filter(year >= '1980',
           iso3c %in% c('USA', 'DEU', 'FRA', 'GBR', 'CHN', 'RUS'),
           group != 'Creation') %>%
    ggplot(aes(year, value, color = group)) +
    geom_area() +
    facet_wrap(~ iso3c, scales = 'free_y')




# category - decide to focus on vaccine
tech %>%
    count(category, sort = T)
tech %>%
    filter(category == 'Vaccines',
           iso3c == 'USA',
           year >= '2000') %>%
    distinct(variable) %>% # 12 distinct vaccines
    group_by(value)

tech2 %>%
    filter(category == 'Vaccines',
           iso3c == 'USA',
           year >= '2000') %>%
    group_by(variable) %>%
    summarise(mean = mean(value)) %>%
    arrange(desc(mean)) 

tech2 %>%
    filter(category == 'Vaccines',
           year >= '2000',
           iso3c %in% c('USA', 'DEU', 'GBR', 'ITA', 'TUR', 'ESP', 'CAN', 'CHN', 'KOR')) %>%
    group_by(variable, iso3c) %>%
    summarise(mean = mean(value)) %>%
    arrange(desc(mean)) %>%
    ggplot(aes(variable, mean)) +
    geom_col() +
    facet_wrap(~ iso3c, scales = 'free')




tech %>% 
    filter(category == "Vaccines") %>% 
    filter(iso3c %in% c("USA", "GBR", "FRA", "DEU", "FIN", "NOR", "RUS", "CHN", "IND")) %>%
    filter(str_detect(label, "\\(TWH\\)")) %>% 
    filter(str_detect(label, "Gross", negate = TRUE)) %>% 
    ggplot(aes(year, value, group = label, fill = label)) + 
    geom_area() + 
    expand_limits(y = 0) +
    facet_wrap(~iso3c, scales = "free_y") + 
    labs(x = "Year", y = "TWH of Electricity", 
         title = "Total energy production by country and year, by source",
         caption = "Source: data.nber.org via TidyTuesday")



# value























tech %>% 
    filter(category == "Energy") %>% 
    filter(iso3c %in% c("USA", "GBR", "FRA", "DEU", "FIN", "NOR", "RUS", "CHN", "IND")) %>%
    filter(str_detect(label, "\\(TWH\\)")) %>% 
    filter(str_detect(label, "Gross", negate = TRUE)) %>% 
    ggplot(aes(year, value, group = label, fill = label)) + 
    geom_area() + 
    expand_limits(y = 0) +
    facet_wrap(~iso3c, scales = "free_y") + 
    labs(x = "Year", y = "TWH of Electricity", 
         title = "Total energy production by country and year, by source",
         caption = "Source: data.nber.org via TidyTuesday")













