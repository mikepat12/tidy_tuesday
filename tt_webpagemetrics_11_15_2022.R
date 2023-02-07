### Webpage Metrics
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
image_alt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv') %>%
    clean_data()
color_contrast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv') %>%
    clean_data()
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv') %>%
    clean_data()
bytes_total <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv') %>%
    clean_data()
speed_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv') %>%
    clean_data()

head()
names()
dim()
lapply(x, class)


###########################################################################################
### EDA
###########################################################################################
head(speed_index)
head(image_alt)
unique(speed_index$measure)
# All datasets are similar so we will clean data with function

clean_data <- . %>% # rerun loading data with this function
    select(-timestamp) %>%
    mutate(date = ymd(date))

speed_index %>%
    ggplot(aes(date, p50, color = client)) +
    geom_line() +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.25)


head(image_alt)
image_alt %>%
    ggplot(aes(date, percent, color = client)) +
    geom_line()



head(color_contrast)
color_contrast %>%
    ggplot(aes(date, percent, color = client)) +
    geom_line()


head(ally_scores)
ally_scores %>%
    ggplot(aes(date, p50, color = client)) +
    geom_line() +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.25)



head(bytes_total)
bytes_total %>%
    ggplot(aes(date, p50, color = client)) +
    geom_line() +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.25)


combined_percentiles <- bind_rows(speed_index, bytes_total, ally_scores)
head(combined_percentiles)
combined_percentiles %>%
    ggplot(aes(date, p50, color = client)) +
    geom_line() +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.25) +
    facet_wrap(~ measure, scales = 'free_y') +
    labs(y = 'Median (with 25th adn 75th percentiles)', color = 'Client')



###########################################################################################
### Big Query - Google Cloud
###########################################################################################
# Using public dataset (baseball games) because was not working for webpage metrics query
devtools::install_github('r-dbi/bigrquery')
library(bigrquery)
library(googlesheets4)
gs4_auth_configure(api_key = '') # do not run again. Find on bigquery
bq_auth(email = 'mikepat12@gmail.com')



bq_project_query('big-query-test-377117', 
                 'SELECT * FROM `bigquery-public-data.baseball.games_wide` LIMIT 10')



# Something else to get database data
devtools::install_github('chriscardillo/dbcooper')
con <- DBI::dbConnect(bigrquery::bigquery(), # now we have a connection object
                      project = 'bigquery-public-data',
                      dataset = 'baseball',
                      billing = 'big-query-test-377117')
library(dbcooper)
dbc_init(con, 'baseball')
# Now we have specific functions. Some do not work though because of strsplit
baseball_list()




### Will stick with normal query connection for the rest
con2 <- dbConnect(bigrquery::bigquery(), # now we have a connection object
                  project = 'bigquery-public-data',
                  dataset = 'baseball',
                  billing = 'big-query-test-377117')
dbListTables(con2)
btable <- tbl(con2, "games_wide")


# Need new lines to get back up on github


# This works to get table from google bigquery!!!
test <- bq_project_query('big-query-test-377117', 
                 'SELECT * FROM `bigquery-public-data.baseball.games_wide` LIMIT 10')
x <- bq_table_download(test, n_max = 10)










