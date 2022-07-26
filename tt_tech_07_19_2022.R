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













###########################################################################################
### Cool graphs from others
###########################################################################################






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




###########################################################################################

USA_vaccine <- technology %>%
    filter(category == "Vaccines") %>%
    filter(label != "Extracorporeal shock wave lithotripters") %>%
    filter(variable != "Hib3") %>%
    filter(variable != "MCV2") %>%
    filter(variable != "HepB3") %>%
    filter(variable != "IPV1") %>%
    filter(variable != "PCV3") %>%
    filter(variable != "RotaC") %>%
    filter(variable != "pctimmunizmeas") %>%
    filter(iso3c == "USA") %>%
    group_by(iso3c, year, variable) %>%
    summarize(n = mean(value))
USA_vaccine$variable <- str_replace(USA_vaccine$variable, "MCV1", "Measles")
USA_vaccine$variable <- str_replace(USA_vaccine$variable, "RCV1", "Rubella")
USA_vaccine$variable <- str_replace(USA_vaccine$variable, "Pol3", "Polio")
USA_vaccine$n <- USA_vaccine$n/100
ggplot(USA_vaccine, aes(x = year, y = n)) +
    geom_line() +
    geom_point(size = 0.5) +
    facet_wrap(~variable) +
    geom_vline(xintercept = 1998, color = "red") +
    geom_text(data = data.frame(x = 1999,
                                y = 0.85,
                                variable = "DPT",
                                label = "1998: \nLancet MMR paper is published"),
              aes(x = x, y = y, label = label), 
              size = 2, 
              hjust = 0,
              fontface = "italic") +
    ylab("Number of Flights") +
    labs(title = "Percent of Children who Received Immunizations in the U.S.",
         subtitle = "Strong convergence in use of consumption technologies, like vaccines, compared to production technologies, indicates increasing quality of life. \nDespite a deeply controversial paper linking vaccines to autism, vaccine rates nationwide have not greatly changed in the past decade.",
         caption = "Source: data.nber.org | github: julia-tache | July 25th, 2022") +
    theme(panel.background = element_rect(fill = "#E4FDE1"),
          plot.background = element_rect(fill = "#F3EAAF"),
          strip.background = element_rect(fill = "#648381"),
          strip.text = element_text(colour = 'white'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 7, face = "italic"),
          plot.caption = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_x_continuous(breaks = round(seq(min(USA_vaccine$year), max(USA_vaccine$year), by = 5), 1)) +
    scale_y_continuous(breaks = pretty_breaks(5),
                       labels = percent)

###########################################################################################

library(dplyr)
library(reactable)
library(reactablefmtr)

x <- 
    tech |> 
    filter(
        iso3c == "MEX"
        & stringr::str_detect(label, "Electricity|electric energy")
    ) |> 
    select(!variable) |> 
    pivot_wider(
        names_from = label,
        values_from = value
    ) |> 
    select(-one_of("Electricity Generating Capacity, 1000 kilowatts", "iso3c",
                   "group", "category"
    )) |> 
    filter(year >= 1995) |> 
    rename(Period = year) 

x |> 
    reactable(
        theme = nytimes(centered = TRUE, font_color = '#666666'),
        defaultColDef = colDef(format = colFormat(digits = 2)),
        highlight = TRUE,
        defaultPageSize = 18,
        columns = list(
            #`Gross output of electric energy (TWH)` = colDef(name = "Total production of energy")
            Period = colDef(format = colFormat(digits = 0), align = "left"),
            `Gross output of electric energy (TWH)` = colDef(
                align = 'left',
                minWidth = 250,
                cell = data_bars(
                    data = x,
                    fill_color = viridis::viridis(5),
                    background = '#FFFFFF',
                    bar_height = 4,
                    number_fmt = scales::comma,
                    text_position = 'outside-end',
                    max_value = 400,
                    icon = 'circle',
                    #icon_color = '#226ab2',
                    icon_size = 15,
                    text_color = '#226ab2',
                    round_edges = TRUE
                )
            ),
            `Electricity from oil (TWH)` = colDef(
                cell =  color_tiles(x, bias = 1.4, box_shadow = TRUE,
                                    number_fmt = scales::comma)
            )
            
        )
    ) |> 
    add_title(
        title = reactablefmtr::html("Energy production in Mexico <img src='https://svgsilh.com/svg/146443.svg' alt='Bee' width='40' height='40'>"),
        margin = reactablefmtr::margin(t=0,r=0,b=3,l=0)
    ) |> 
    add_subtitle(
        subtitle = 'By power source. Values until 2020.',
        font_weight = 'normal',
        font_size = 20,
        margin = reactablefmtr::margin(t=0,r=0,b=6,l=0)
    ) |> 
    add_source("Table created by: Jorge Hernández with {reactablefmtr}", font_size = 12)


