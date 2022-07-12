### Tidy Tuesday 6-08-2021 ###

# Data for commercial fishing on the great lakes. Time series data as it dates from 1867-2015



# Packages
library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)
library(skimr)
library(janitor)
library(broom)
library(tidymodels)
library(ggnewscale)
library(sweep)
library(timetk)
library(forecast)




# Load data
tuesdata <- tidytuesdayR::tt_load('2021-06-08')
fishing <- tuesdata$fishing
    # data cleaning (variable cleaning)
    ### Do not run this string remove anymore. Run the one after. Kept for the cool code to know
fishing <- fishing %>%
    filter(values >= 0) %>%
    mutate(species = str_remove(str_to_title(species), "s$")) # removing s at the end of species
    
    # Run this!
fishing <- fishing %>%
    filter(values >= 0) %>%
    mutate(species = str_replace(str_to_title(species), "([^s])s$", "\\1")) # replaces the s with the letter before now. Now we can keep bass



# Looking at the data basics
head(fishing)
names(fishing)
dim(fishing)
skim(fishing)
lapply(fishing, class)






# Checking what the difference is b/w grand total and values variables. Deciding not to use
# grand total!!
fishing %>%
    ggplot(aes(grand_total + 1)) + 
    geom_histogram() + 
    scale_x_log10()
fishing %>%
    ggplot(aes(values + 1)) + 
    geom_histogram() + 
    scale_x_log10()

fishing %>%
    group_by(year, lake, species) %>%
    summarize(total_values = sum(values, na.rm = TRUE),
              first_grand_total = min(grand_total, na.rm = TRUE),
              n_grand_total = n_distinct(grand_total, na.rm = TRUE)) %>%
    ungroup() %>%
    count(n_grand_total) # except once, grand total is always 1 or 0


# Checking variable types and other. EDA!
fishing %>%
    count(lake, sort = T)

fishing %>%
    count(species, sort = T)

    # Bc we are using summarize a lot, lets make a function
summarize_fish <- function(tbl) {
    tbl %>%
        summarize(n_obs = n(), total_production = sum(values, na.rm = T)) %>%
        arrange(desc(total_production))
}
fishing %>%
    group_by(year) %>%
    summarize_fish() %>%
    ggplot(aes(year, total_production)) + 
    geom_line()
plot1 <- fishing %>% # doing by decade now
    group_by(decade = 10 * year %/% 10) %>%
    summarize_fish() %>%
    ggplot(aes(decade, total_production)) + 
    geom_line() + 
    geom_point() + ### fish have been rapidly declining thanks to the invasive species
    scale_y_continuous(labels = comma_format()) + 
    labs(x = "Decade", y = "Total Production (thousands of pounds)", 
         title = "Total Production in the Great Lakes from 1867 - 2015") + 
    theme(plot.title = element_text(hjust = 0.5)) # to center the title


fishing %>%
    count(species, sort = T)
plot2 <- fishing %>%
    mutate(species = fct_lump(species, 8, w = values), 
           species = fct_reorder(species, values, sum)) %>%
    group_by(decade = 10 * year %/% 10, species) %>%
    summarize_fish() %>%
    ggplot(aes(decade, total_production, fill = species)) + 
    geom_area() + # can do col or area
    scale_y_continuous(labels = comma_format()) + 
    facet_wrap(~ species) + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    labs(x = "Decade", y = "Total Production (thousands of pounds)", 
         title = "Total Production for the Top 8 Species in the Great Lakes from 1867 - 2015")

 

fishing %>% # we need to clean the dataset for cisco and chub variations. After data cleaning only 2 now
    filter(str_detect(species, "Cisco")) %>%
    mutate(species = fct_lump(species, 8, w = values), 
           species = fct_reorder(species, values, sum)) %>%
    group_by(decade = 10 * year %/% 10, species) %>%
    summarize_fish() %>%
    ggplot(aes(decade, total_production, fill = species)) + 
    geom_area() + # can do col or area
    scale_y_continuous(labels = comma_format()) + 
    facet_wrap(~ species) + 
    theme(legend.position = "none") 

fishing %>%
    group_by(species) %>%
    summarize_fish() %>%
    view()


plot3 <- fishing %>%
    group_by(species, year) %>%
    summarize_fish() %>%
    summarize(total_production = sum(total_production), 
              peak_year = year[which.max(total_production)]) %>% # cool function to know. Shows when did production peak or have max
    arrange(desc(total_production)) %>%
    head(n = 25) %>%
    mutate(species = fct_reorder(species, peak_year)) %>%
    ggplot(aes(peak_year, species)) + 
    geom_point(aes(size = total_production)) + 
    scale_size_continuous(labels = comma_format()) +
    labs(x = "Year of Peak Production", y = "Species", size = "All-Time Production", 
         title = "Peak Year of Production for the Top 25 Species from 1867 - 2015") + 
    theme(plot.title = element_text(hjust = 0.5))
    # Plot for each lake. Not too insightful
fishing %>%
    mutate(lake = fct_reorder(lake, values, sum, .desc = T)) %>%
    group_by(decade = 10 * year %/% 10, lake) %>%
    summarize_fish() %>%
    ggplot(aes(decade, total_production, fill = lake)) + 
    geom_area() + # can do col or area
    scale_y_continuous(labels = comma_format()) + 
    facet_wrap(~ lake) + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
    labs(x = "Decade", y = "Total Production (thousands of pounds)", 
         title = "Total Production for each of the Great Lakes from 1867 - 2015")


    # Will now look the the relationship between species and lake
library(ggnewscale)
plot4 <- fishing %>%
    filter(lake != "Saint Clair") %>%
    mutate(species = fct_lump(species, 20, w = values), 
           species = fct_reorder(species, values, sum),
           lake = fct_reorder(lake, values, sum)) %>%
    group_by(lake, species) %>%
    summarize_fish() %>%
    ggplot(aes(lake, species, fill = total_production)) + 
    geom_tile() +   
    scale_fill_gradient2(low = "white", high = "darkblue", labels = comma_format()) + 
    expand_limits(fill = 0) +
    theme(panel.grid = element_blank()) +
    labs(x = "Lake", y = "Species", fill = "All-Time Production", 
         title = "Tile Plot of Species vs Lake") + 
    theme(plot.title = element_text(hjust = 0.5))

by_lake_species <- fishing %>% # saving this code so we don't always have to input
    filter(lake != "Saint Clair") %>%
    mutate(species = fct_lump(species, 20, w = values), 
           species = fct_reorder(species, values, sum),
           lake = fct_reorder(lake, values, sum)) %>%
    group_by(lake, species) %>%
    summarize_fish()


plot5 <- by_lake_species %>%
    group_by(lake) %>%
    mutate(pct = total_production / sum(total_production)) %>%
    ggplot(aes(lake, species, fill = pct)) + 
    geom_tile() +   
    scale_fill_gradient2(low = "white", high = "darkblue", 
                         labels = percent_format(accuracy = 1)) + 
    expand_limits(fill = 0) +
    theme(panel.grid = element_blank()) +
    labs(x = "Lake", y = "Species", fill = "% of Lake's Production")










# Now looking at the stocked dataset
stocked <- tuesdata$stocked %>%
    clean_names()
head(stocked)
names(stocked)
dim(stocked)
skim(stocked)
lapply(stocked, class)


stocked %>%
    count(site, sort = T)
stocked %>%
    count(species, sort = T)

    # longitude and latitude missing most variables so cannot make a map

stocked %>%
    count(is.na(stat_dist)) # we have most the data

stocked %>%
    ggplot(aes(length)) + 
    geom_histogram() +
    scale_x_log10() # nice normal curve to the length of fish


### Not going to do more with stocked. It's more confusing with the codes and variables










# Forecasting
library(sweep)
library(timetk)
library(forecast)
fishing %>%
    group_by(species) %>%
    summarize_fish()

    # looks best to predict Lake Whitefish. Going to use Yellow Perch though
fishing %>%
    filter(species == "Yellow Perch") %>%
    group_by(year, species) %>%
    summarize_fish() %>%
    ggplot(aes(year, total_production)) + 
    geom_line()

by_year_species <- fishing %>%
    mutate(year = as.Date(paste0(year, "-01-01"))) %>% # paste the quotes after the year
    group_by(year, species) %>%
    summarize_fish() %>%
    ungroup() %>%
    select(-n_obs)

ets_model <- by_year_species %>%
    filter(species == "Yellow Perch") %>%
    tk_ts(start = min(year(.$year)), frequency = 1) %>% # now has it as a time series
    ets() # applying a model to the data
ets_model
 
ets_model %>%
    sw_tidy() 
ets_model %>%
    sw_augment() 

ets_model %>%
    sw_augment() %>%
    ggplot(aes(index, .actual)) + 
    geom_line() + 
    geom_line(aes(y = .fitted), color = "blue") + 
    labs(x = "Year", y = "Total Production of Yellow Perch", 
         title = "Forecast of the Total Production of Yellow Perch") +
    theme(plot.title = element_text(hjust = 0.5))
    
decomp_fit <- sw_tidy_decomp(ets_model)
decomp_fit



    # Now forecasting
fcast <- ets_model %>%
    forecast(h = 10) # horizon of 10 years in the future
fcast # No change so we would have a flat plot. 


    # Trying other forecasting models. Using holt
ts_data <- by_year_species %>%
    filter(species == "Yellow Perch") %>%
    tk_ts(start = min(year(.$year)), frequency = 1)


fcast_holt <- ts_data %>%
    holt()
fcast_holt

autoplot(ts_data) + 
    autolayer(fcast_holt)


    # Trying SES
fcast_ses <- ts_data%>%
    ses()
fcast_ses

autoplot(ts_data) + 
    autolayer(fcast_holt, series = "Holt", PI = F) + 
    autolayer(fcast_ses, series = "SES", PI = F) # PI is prediction  interval
    # Seems yellow perch is just not exciting to predict





# Now forecasting different fish. Carp
ts2_data <- by_year_species %>%
    filter(species == "Carp") %>%
    tk_ts(start = min(year(.$year)), frequency = 1)

    # Holt
fcast_holt <- holt(ts2_data)
fcast_holt

autoplot(ts_data) + 
    autolayer(fcast_holt)

# Trying SES
fcast_ses <- ses(ts_data)
fcast_ses

autoplot(ts_data) + 
    autolayer(fcast_holt, series = "Holt", PI = F) + 
    autolayer(fcast_ses, series = "SES", PI = F)

# And now hw with damped
fcast_hw <- holt(ts2_data, damped = T)
autoplot(ts_data) + 
    autolayer(fcast_holt, series = "Holt", PI = F) + 
    autolayer(fcast_hw, series = "Holt (damped)", PI = F) +
    autolayer(fcast_ses, series = "SES", PI = F)

    # Going to try and facet species forecasting plots now
many_forecasts <- by_year_species %>%
    mutate(species = fct_lump(species, 8)) %>%
    group_by(species, year) %>%
    summarize(total_production = sum(total_production), .groups = "drop") %>%
    nest(data = c(-species)) %>%
    mutate(time_series = map(data, ~ tk_ts(., start = min(year(.$year)), frequency = 1))) %>%
    mutate(holt = map(time_series, holt, h = 20), 
           ses = map(time_series, ses, h = 20))
many_forecasts

sw_tidy(many_forecasts$time_series[[1]])
sw_sweep(many_forecasts$holt[[1]]) # tidy function for forecast objects
sw_sweep(many_forecasts$ses[[1]])


many_forecasts %>%
    mutate(time_series_tidy = map(time_series, sw_tidy)) %>%
    unnest(time_series_tidy)
    
plot6ts <- many_forecasts %>%
    mutate(forecast_sweep = map(holt, sw_sweep)) %>%
    unnest(forecast_sweep) %>%
    ggplot(aes(index, total_production)) + 
    geom_line() + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), alpha = 0.2) +
    facet_wrap(~ species) +
    labs(x = "Year", y = "Total Production", 
         title = "Forecast of the All-Time Production Fish in the Great Lakes") +
    theme(plot.title = element_text(hjust = 0.5))
    
    
    

