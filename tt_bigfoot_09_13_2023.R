### Bigfoot Sightings
# 








############################################################################################
### Load Packages
############################################################################################
library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)
library(skimr)
library(janitor)
library(broom)
library(tidymodels)
library(GGally)
library(ggthemes)
library(mapproj)
library(tidytext)
library(textrecipes)
library(themis)
library(vip)
library(gganimate)
library(widyr)





###########################################################################################
### Load Data
###########################################################################################
bf_data <- tt_load('2022-09-13')
bigfoot_raw <- bf_data$bigfoot

head(bigfoot_raw)
names(bigfoot_raw)
dim(bigfoot_raw)
lapply(bigfoot_raw, class)





###########################################################################################
### EDA
###########################################################################################
bigfoot_raw %>%
    count(season, sort = T)
bigfoot_raw <- bigfoot_raw %>%
    mutate(season = na_if(season, 'Unknown'), 
           title = str_remove(title, 'Report \\d+\\:'),
           year = year(date))


bigfoot_raw %>%
    count(classification)

bigfoot_raw %>%
    filter(year >= 1950) %>%
    ggplot(aes(year, fill = classification)) + 
    geom_bar()


bigfoot_raw %>%
    count(state, sort = T)
bigfoot_raw %>%
    filter(state != 'Alaska',
           longitude > -130) %>% # these were mislabeled
    ggplot(aes(longitude, latitude)) +
    borders('state') +
    geom_point(size = 0.5) +
    theme_map() +
    coord_map()

bf_filtered <- bigfoot_raw %>%
    filter(state != 'Alaska',
           longitude > -130,
            year >= 1950)

# Animated map!
bf_filtered %>%
    filter(classification != 'Class C',
           year <= 1970) %>%
    ggplot(aes(longitude, latitude, color = classification)) +
    borders('state') +
    geom_point(size = 0.5) +
    theme_map() +
    coord_map() +
    transition_manual(year, cumulative = T) +
    labs(title = 'Bigfoot Sightings: {current_frame}',
         color = '')


bf_filtered %>%
    mutate(state = fct_lump(state, 8),
           state = fct_infreq(state)) %>%
    ggplot(aes(5 * year %/% 5)) +
    geom_bar() +
    facet_wrap(~ state, scales = 'free') + 
    labs(x = 'Year')

bf_filtered %>%
    filter(!is.na(season)) %>%
    ggplot(aes(year, fill = season)) +
    geom_bar() +
    facet_wrap(~ season)

bf_filtered %>% # check how seasons differ within state
    filter(!is.na(season)) %>%
    count(season, state = fct_lump(state, 8)) %>%
    mutate(state = fct_reorder(state, n, sum)) %>%
    ggplot(aes(n, season)) +
    geom_col() +
    facet_wrap(~ state, scales = 'free_x')


bigfoot_raw %>%
    filter(!is.na(temperature_high)) %>%
    summarise(min = min(temperature_high),
              max = max(temperature_high),
              mean = mean(temperature_high))




### Looking at text
bf_title_words <- bigfoot_raw %>%
    select(id = number, season, year, classification, state, title, observed, location_details) %>%
    unnest_tokens(word, title) %>%
    filter(!is.na(word)) %>%
    anti_join(stop_words, by = 'word')

bf_title_words %>%
    count(year, word = fct_lump(word, 16)) %>%
    filter(word != 'Other') %>%
    ggplot(aes(year, n)) +
    geom_col() + 
    facet_wrap(~ word, scales = 'free')

bf_title_words %>%
    group_by(decade = 10 * year %/% 10) %>%
    mutate(year_total = n_distinct(word)) %>%
    count(decade, year_total, word = fct_lump(word, 16)) %>%
    filter(word != 'Other') %>%
    ggplot(aes(decade, n / year_total)) +
    geom_line() + 
    expand_limits(y = 0) +
    facet_wrap(~ word, scales = 'free_y') +
    scale_y_continuous(labels = percent_format())
    
bf_title_words %>%
    pairwise_cor(word, id) %>%
    arrange(desc(correlation))
 

############################################################################################
### Machine Learning
############################################################################################
# Predicting class A or B based on data
set.seed(21)
bf_spl <- bigfoot_raw %>%
    filter(classification != 'Class A') %>%
    initial_split()
bf_train <- training(bf_spl)
bf_test <- testing(bf_spl)

bf_folds5 <- bf_train %>%
    vfold_cv(5)

recipe(classification ~ season + year + year, bf_train) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_ns(year, deg_free = tune()) %>%
    step_other(all_nominal_predictors(), threshold = 0.4) %>%
    workflow(logistic_reg()) %>%
    tune_grid(bf_folds5, 
              metrics = metric_set(roc_auc), grid = tibble(deg_free = 1:4)) %>%
    collect_metrics()
tune %>%
    autoplot() # if we tuned threshold


log_wf <- recipe(classification ~ season + year + year, bf_train) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_ns(year, deg_free = 6) %>%
    step_other(all_nominal_predictors(), threshold = 0.1) %>%
    workflow(logistic_reg()) %>%
    fit_resamples(bf_folds5, metrics = metric_set(roc_auc)) %>%
    collect_metrics()


# Random Forest
rf_wf <- recipe(classification ~ season + year, bf_train) %>%
    step_other(all_nominal_predictors(), threshold = 0.1) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    workflow(rand_forest(mode = 'classification')) %>%
    fit_resamples(bf_folds5, metrics = metric_set(roc_auc)) %>%
    collect_metrics()










