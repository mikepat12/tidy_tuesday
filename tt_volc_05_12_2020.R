### Volcano Eruptions using multinomial classification model
# Use other info about volcanoes to predict type of volcano. Many types of volcanoes








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
library(tidytext)
library(textrecipes)
library(themis)
library(vip)





###########################################################################################
### Load Data
###########################################################################################
vol_data <- tidytuesdayR::tt_load('2020-05-12')
volc <- vol_data$volcano

head(volc)
names(volc)
dim(volc)
lapply(volc, class)





###########################################################################################
### EDA
###########################################################################################
volc %>%
    count(primary_volcano_type, sort = T) # Only going to take 

volc_df <- volc %>%
    transmute(volc_type = case_when(
        str_detect(primary_volcano_type, 'Stratovolcano') ~ 'Stratovolcano',
        str_detect(primary_volcano_type, 'Shield') ~ 'Shield',
              TRUE ~ 'Other'),
              volcano_number, latitude, longitude, elevation, tectonic_settings, major_rock_1) %>%
    mutate_if(is.character, factor)
head(volc_df)

volc_df %>%
    count(volc_type, sort = TRUE)

###########################################################################################
### Building map to visualize volcanoes
###########################################################################################
world <- map_data('world')
world %>%
    as_tibble() # to find variables

ggplot() +
    geom_map(data = world, map = world,
             aes(long, lat, map_id = region),
             color = 'white', fill = 'gray50', alpha = 0.2) +
    geom_point(data = volc_df,
               aes(longitude, latitude, color = volc_type), alpha = 0.8) +
    labs(x = 'Longitude', y = 'Latitude', title = 'Map of Volcanic Types') +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))




###########################################################################################
### Building a model
###########################################################################################
volc_boot <- bootstraps(volc_df)
volc_boot

# Deal with class imbalance between volcano types 
# Using SMOTE algorithm
library(themis)

# Make dummy variables. Center and scale
volc_recipe <- recipe(volc_type ~ ., data = volc_df) %>%
    update_role(volcano_number, new_role = 'ID') %>%
    step_other(tectonic_settings) %>%
    step_other(major_rock_1) %>%
    step_dummy(tectonic_settings, major_rock_1) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    step_smote(volc_type)
volc_recipe

volc_prep <- prep(volc_recipe) # prepping goes through variables and calculates
volc_prep
### check prepped results with juice 
juice(volc_prep) 
juice(volc_prep) %>%
    count(volc_type) # same number for each type now bc we oversampled
names(juice(volc_prep))


rf_spec <- rand_forest(trees = 1000) %>%
    set_mode('classification') %>%
    set_engine('ranger')
rf_spec

# Making workflow
volc_wf <- workflow() %>%
    add_recipe(volc_recipe) %>%
    add_model(rf_spec)
volc_wf


volc_results <- fit_resamples(volc_wf, resamples = volc_boot, 
              control = control_resamples(save_pred = T, verbose = T))



###########################################################################################
### Explore results
###########################################################################################
volc_results
str(volc_results) # interesting

volc_results %>%
    collect_metrics()
volc_results %>%
    collect_predictions() %>%
    conf_mat(volc_type, .pred_class)

####### Other confusion matrices
library(caret)
caret_mat <- volc_results %>%
    collect_predictions() 
confusionMatrix(data = caret_mat$volc_type, reference = caret_mat$.pred_class)

library(gmodels)
gmodel_mat <- volc_results %>%
    collect_predictions() 
CrossTable(gmodel_mat$volc_type, gmodel_mat$.pred_class)

library(cvms)
test <- caret_mat %>% # how to get N to work? Maybe using count?
    select(.pred_class, volc_type, .row) %>% 
    rename(Target = volc_type, 
           Prediction = .pred_class) %>%
    mutate(N = n())
plot_confusion_matrix(test)





### Moving on now
volc_results %>%
    collect_predictions() %>%
    conf_mat(volc_type, .pred_class)
volc_results %>%
    collect_predictions() %>%
    group_by(id) %>%
    ppv(volc_type, .pred_class)
volc_results %>%
    collect_predictions() %>%
    group_by(id) %>%
    ppv(volc_type, .pred_class) %>%
    ggplot(aes(.estimate)) +
    geom_histogram(bins = 10)


# Understand model. What is impacting the predictions
library(vip)

rf_spec %>%
    set_engine('ranger', importance = 'permutation') %>%
    fit(volc_type ~ .,
        data = juice(volc_prep) %>% select(-volcano_number)) %>%
    vip(geom = 'point')



###########################################################################################
### Making 1 last map with bootstrap resamples
###########################################################################################
volc_results %>%
    collect_predictions() %>%
    mutate(correct = volc_type == .pred_class) %>%
    count(correct, sort = T) 
volc_pred <- volc_results %>%
    collect_predictions() %>%
    mutate(correct = volc_type == .pred_class) %>%
    left_join(volc_df %>%
                  mutate(.row = row_number()))
volc_pred


ggplot() + # shows where we predicted correctly around the world!
    geom_map(data = world, map = world,
             aes(long, lat, map_id = region),
             color = 'white', fill = 'gray50', alpha = 0.2) +
    stat_summary_hex(data = volc_pred,
               aes(longitude, latitude, z = as.integer(correct)), alpha = 0.7, bins = 50) +
    scale_fill_gradient(high = 'cyan3', labels = percent) +
    theme_void(base_family = 'IBMPlexSans') %>%
    labs(fill = 'Percent Classified Correctly', x = NULL, y = NULL)





