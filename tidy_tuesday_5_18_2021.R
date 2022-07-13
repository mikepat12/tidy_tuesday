### Tidy Tuesday 5-18-2021 ###
# The salary survey a few weeks ago got a huge response — 24,000+ people shared their salaries 
# and other info, which is a lot of raw data to sift through. Reader Elisabeth Engl kindly took 
# the raw data and analyzed some of the trends in it and here’s what she found. 
# (She asked me to note that she did this as a fun project to share some insights from the 
# survey, rather than as a paid engagement.)

#This data does not reflect the general population; it reflects Ask a Manager readers who 
# self-selected to respond, which is a very different group (as you can see just from the 
# demographic breakdown below, which is very white and very female).


##### Notes for next time. When cleaning variable types, ordering, names, to do so from the base dataset
# and not to new datasets from the original.
# Fct_lump will lump together the most common of the variable, and we can specify the n.
    # for example Fct_lump(x, n = 9) will lump the top 9 obs. of x (9 most common)

# Packages
library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)
library(skimr)
library(janitor)
library(broom)
library(tidymodels)




# Load data
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey




# Looking at the data basics
head(survey)
names(survey)
dim(survey)
skim(survey)
lapply(survey, class)





# Checking variable types and other. EDA!
    # Changing time variable
survey <- survey %>%
    mutate(timestamp = mdy_hms(timestamp))

    # Checking number and levels of how_old_are_you. Reordered and changed name
survey %>%
    count(how_old_are_you)

survey <- survey %>%
    mutate(age_cat = fct_relevel(fct_reorder(how_old_are_you, parse_number(how_old_are_you)), "under 18"))
    
survey %>%
    count(age_cat) # Most aged between 25-44 
s1 <- table(survey$age_cat)
addmargins(s1)
round(prop.table(s1), digits = 2) # wanted to see proportion of age_cat variable

    # Checking industry type. Free text with many answers
survey %>%
    count(industry, sort = TRUE)

    # Checking job title. Free text with many answers
survey %>%
    count(job_title, sort = TRUE)
survey %>%
    filter(job_title == "Data Scientist") # just curious

    # Looking at annual salary
summary(survey$annual_salary)
survey %>%
    count(currency, sort = TRUE) # changes how we look at salary

survey %>%
    filter(currency == "USD") %>%
    filter(annual_salary >= 10000, annual_salary <= 1e6) %>%
    ggplot(aes(annual_salary)) + 
    geom_histogram() + 
    scale_x_log10(labels = dollar_format()) + 
    labs(x = "Annual Salary")
s2 <- survey %>%
    filter(currency == "USD") %>%
    filter(annual_salary >= 10000, annual_salary <= 1e6) %>%
    select(annual_salary)
summary(s2) # just wanted to see the summary stats of this plot
s2 %>%
    arrange(desc(annual_salary))
            
survey_usd <- survey %>% # only usd dataset that is filtered
    filter(currency == "USD") %>%
    filter(annual_salary >= 5000, annual_salary <= 2e6)

survey_usd %>%
    ggplot(aes(annual_salary)) + 
    geom_histogram() + 
    scale_x_log10(labels = dollar_format()) + 
    labs(x = "Annual Salary")

    
    # Looking at country and state
survey_usd %>%
    count(country, sort = TRUE)
survey_usd %>%
    count(state, sort = TRUE)
    
    # counting only the first state people list
survey_usd <- survey %>% # only usd dataset that is filtered
    filter(currency == "USD") %>%
    filter(annual_salary >= 5000, annual_salary <= 2e6) %>%
    mutate(state = str_remove(state, ", .*"))
survey_usd %>%
    count(state, sort = TRUE) %>%
    view() # 52 states so there is missing data. Should be only 51

summarize_salary <- function(tbl) {
    tbl %>%
        summarize(n = n(), median_salary = median(annual_salary)) %>%
        arrange(desc(n))
}

survey_usd %>%
    filter(!is.na(state)) %>%
    mutate(state = fct_lump(state, 6), state = fct_reorder(state, annual_salary)) %>%
    group_by(state) %>%
    summarize_salary() # comment out to run the plot 
    ggplot(aes(annual_salary, state)) + 
    geom_boxplot() + 
    scale_x_log10(labels = dollar_format())

survey_usd %>%
        filter(!is.na(state)) %>%
        mutate(state = fct_lump(state, 9), state = fct_reorder(state, annual_salary)) %>%
        group_by(state) %>%
        summarize_salary() %>%
    ggplot(aes(median_salary, state)) + 
        geom_col() + 
        scale_x_continuous(labels = dollar_format())  

    # doing top 9 industries, and then other
survey_usd %>%
    filter(!is.na(industry)) %>%
    mutate(industry = fct_lump(industry, 9), industry = fct_reorder(industry, annual_salary)) %>%
    group_by(industry) %>%
    summarize_salary() %>%
    ggplot(aes(median_salary, industry)) + 
    geom_col() + 
    scale_x_continuous(labels = dollar_format())  

    # make this plot into a function
plot_cat <- function(tbl, column, n_levels = 9, reorder = TRUE) {
    lumped_tbl <- tbl %>%
        filter(!is.na({{column}})) %>%
        mutate({{column}} := fct_lump({{column}}, n_levels))
               
    if(reorder) { # allows us to not always order by salary
        lumped_tbl <- lumped_tbl %>%
            mutate({{column}} := fct_reorder({{column}}, annual_salary))
    }    
    lumped_tbl %>%
        group_by({{column}}) %>%
        summarize_salary() %>%
        ggplot(aes(median_salary, {{column}})) + 
        geom_col() + 
        scale_x_continuous(labels = dollar_format()) +
        labs(x = "Median Salary")
}


survey_usd %>%
    plot_cat(state)
survey_usd %>%
    plot_cat(job_title, n_levels = 15)

    # looking at years of experience variable
survey_usd %>%
    count(year_exp)

survey_usd <- survey_usd %>%
    mutate(overall_exp = str_replace(overall_years_of_professional_experience, " - ", "-"), 
           year_exp = fct_reorder(overall_exp, parse_number(overall_exp)),
           field_exp = str_replace(years_of_experience_in_field, " - ", "-"), 
           field_exp = fct_reorder(field_exp, parse_number(field_exp)),
           gender = fct_collapse(coalesce(gender, "Other or prefer not to answer"), "Other or prefer not to answer" = 
                                     c("Other or prefer not to answer", "Prefer not to answer")),
           race = fct_lump(coalesce(race, "Other"), 4)) # making missing data into other
survey_usd %>%
    count(year_exp)
survey_usd %>%
    count(field_exp)
survey_usd %>%
    plot_cat(year_exp, reorder = FALSE)
    # decided to collapse 2 gender variables into 1
survey_usd %>%
    plot_cat(gender)

survey_usd %>%
    count(race, sort = TRUE)
survey_usd %>%
    plot_cat(race, n_levels = 4)

survey_usd %>%
    count(highest_level_of_education_completed, sort = TRUE)












# Statistical Tests

survey_usd %>%
    filter(!is.na(year_exp)) %>%
    ggplot(aes(annual_salary, year_exp)) + 
    geom_boxplot() + 
    scale_x_log10(labels = dollar_format())
    
    # look at variance among and between groups
lm(log2(annual_salary) ~ year_exp, data = survey_usd) %>% # all comparing to 1yr or less (reference group)
    summary() # all except 2-4 are statistically significantly different from reference for salary
lm(log2(annual_salary) ~ year_exp, data = survey_usd) %>% 
    confint()
lm(log2(annual_salary) ~ year_exp, data = survey_usd) %>% 
    anova() %>%
    tidy() # multiple R^2 tells us how much our model explains variance

lm(log2(annual_salary) ~ field_exp, data = survey_usd) %>% 
    summary() 

survey_usd %>%
    mutate(job_title = fct_lump(job_title, 10)) %>%
    lm(log2(annual_salary) ~ job_title, data = .) %>% 
    summary() %>%
    tidy()
    # adding the other terms to the model
survey_usd %>%
    mutate(job_title = fct_lump(job_title, 10),
           state = fct_lump(state, 10)) %>%
    lm(log2(annual_salary) ~ job_title + state + field_exp, data = .) %>% 
    summary()

survey_usd %>%
    mutate(job_title = fct_lump(job_title, 10),
           state = fct_lump(state, 10),
           industry = fct_lump(industry, 10)) %>%
    lm(log2(annual_salary) ~ job_title + state + field_exp + industry + gender + race, data = .) %>% 
    anova() %>%
    tidy() %>%
    mutate(pct_variation = sumsq / sum(sumsq))








# Machine Learning using tidy models (stopped video at 46:04)

    # splitting the data
set.seed(1234)
usd_split <- initial_split(survey_usd)

usd_train_split <- training(usd_split)
usd_test_split <- testing(usd_split)

head(usd_train_split)
head(usd_test_split)


    # creating a recipe. Specifying variables and adding steps
first_recipe <- usd_train_split %>%
    recipe(annual_salary ~ job_title + state + field_exp + industry + gender + race + highest_level_of_education_completed) %>%
    step_unknown(job_title, industry, state, highest_level_of_education_completed, gender) %>%
    step_mutate(highest_level_of_education_completed = as.factor(highest_level_of_education_completed)) %>%
    step_log(annual_salary, base = 2) %>%
    step_other(job_title, industry, state, threshold = 0.001) %>% # pools rare obs. into other bucket
    step_dummy(all_nominal())
first_recipe
    # prepping the recipe
prep_rec <- prep(first_recipe)
prep_rec$var_info

    # bake it now
baked_rec_train <- bake(prep_rec, new_data = usd_train_split)
glimpse(baked_rec)


    # Linear regression model. Probably can get the same with tidymodels parsnip but his code is after
lin_reg_model <- lin_reg_model  %>% 
    parsnip::set_engine("lm") %>%

#########################################
# David code

training_cv <- vfold_cv(usd_train_split, v = 10)
linear_mod_cv <- linear_reg() %>%
    set_engine("lm") %>%
    fit_resamples(prep_rec, training_cv)

linear_mod_cv %>%
    collect_metrics()


    # We can tune this now
tuned_recipe <- usd_train_split %>%
    recipe(annual_salary ~ job_title + state + field_exp + industry + gender + race + highest_level_of_education_completed) %>%
    step_novel(all_nominal()) %>% # need to have this step first
    step_unknown(job_title, industry, state, highest_level_of_education_completed, gender) %>%
    step_mutate(highest_level_of_education_completed = as.factor(highest_level_of_education_completed)) %>%
    step_log(annual_salary, base = 2) %>%
    step_other(job_title, industry, state, threshold = tune()) %>% # pools rare obs. into other bucket
    step_dummy(all_nominal()) %>%
    step_nzv(everything())

threshold_grid <- crossing(threshold = c(0.001, 0.003, 0.01, 0.03, 0.1))
linear_mod_cv_tune <- linear_reg() %>%
    set_engine("lm") %>%
    tune_grid(tuned_recipe, training_cv, grid = threshold_grid) # not using prepped recipe

linear_mod_cv_tune
linear_mod_cv_tune %>%
    collect_metrics()

    # plotting
linear_mod_cv_tune %>%
    collect_metrics() %>%
    ggplot(aes(threshold, mean, color = .metric)) +
    geom_line() + 
    scale_x_log10() # shows thresholds could probably be lower


    # updating recipe
rec_w_threshold <- tuned_recipe %>%
    finalize_recipe(list(threshold = 0.001))

linear_mod_cv_thresh <- linear_reg() %>%
    set_engine("lm") %>%
    fit_resamples(rec_w_threshold, training_cv)

library(ranger)
random_forest_test <- rand_forest(mode = "regression", mtry = 2, trees = 10) %>%
    set_engine("ranger") %>%
    fit_resamples(rec_w_threshold, training_cv)
 
    # comparing the 2 models. LM seems to be much better. Has lower rmse and higher rsq
random_forest_test %>%
    collect_metrics()
lm_cv_metric <- linear_mod_cv %>%
    collect_metrics()


    # tuning random forest
rf_grid <- crossing(mtry = c(2, 3, 4), trees = c(30, 100, 300))

library(doParallel)
doParallel::registerDoParallel(cores = 7)

random_forest_tune <- rand_forest(mode = "regression", mtry = tune(), trees = tune()) %>%
    set_engine("ranger") %>%
    tune_grid(rec_w_threshold, training_cv, grid = rf_grid)
random_forest_tune %>% # the more trees the higher the rsq. And higher the split
    collect_metrics() %>%
    filter(.metric == "rsq") %>%
    ggplot(aes(trees, mean, color = factor(mtry))) +
    geom_line() + 
    geom_hline(yintercept = lm_cv_metric$mean[2], lty = 2) +
    labs(x = "Number of Trees", y = "R Squared", color = "Number of Splits")
    # we can see the lm performs much better

# Trying a prediction. Did not work haha need to figure out how to do this
y_predrf = predict(random_forest_tune, tbl(job_title = "Software Engineer"))



