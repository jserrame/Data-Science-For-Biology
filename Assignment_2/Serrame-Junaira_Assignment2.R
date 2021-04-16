# Project owner: Junaira Serrame, 500711485
### Date: February 26, 2021
### Project detail: This project is for BLG610 Assignment 2: Projects, Coding Practice, Workflows, and R Markdown.

## The R packages used in this project are ggplot2, tidyverse, and dplyr.
library(ggplot2)
library(tidyverse)
library(dplyr)

## Task 1: Read the surveys_complete.csv file

getwd()
surveys_complete <- read.csv("/Users/airaserrame/Desktop/BLG610 - Data Science for Biology/R_proj/BLG610_Assignment2/data/surveys_complete.csv")
view(surveys_complete)
dim(surveys_complete)


## Task 2: Using the dplyr filter(), find all sample records for animals with a weight of less than or equal to 11 grams and more than 8 grams.
##### There are 2056 rodents that weigh more than 8 grams but weigh less than or equal to 11 grams. 

filter(surveys_complete, weight > 8 & weight <= 11)
weight_filtered <- filter(surveys_complete, weight > 8 & weight <= 11)
view(weight_filtered)
nrow(weight_filtered)

## Task 3a: Using dplyr, query the number of Sigmodon hispidus that were caught in each plot_type surveyed.
##### There are 42 S. hispidus in the control plot, 61 in the Long-term Krat Exclosure, 9 in the Rodent Exclosure,
##### 15 in the Short-term Krat Exclosure, and 1 in the Spectab Exclosure. 

task3_query <- surveys_complete %>%
                  select(genus, species, plot_type, weight) %>% 
                  filter(genus == "Sigmodon") %>% 
                  filter(species == "hispidus") %>% 
                  group_by(plot_type) %>% 
                  count(plot_type)

task3_query 
   
## Task 3b: arrange the filtered data for this species (Sigmodon hispidus) by descending animal weight. Save the result to a <.csv> file, which should be saved in your output folder.
##### The heaviest Sigmodon hispidus weighs 140 g.
task3_arrange <- surveys_complete %>%
                    select(genus, species, plot_type, weight) %>% 
                    filter(genus == "Sigmodon") %>% 
                    filter(species == "hispidus") %>% 
                    arrange(desc(weight))

head(task3_arrange)
?write.csv
getwd()
write.csv(task3_arrange, file = "/Users/airaserrame/Desktop/BLG610 - Data Science for Biology/R_proj/BLG610_Assignment2/output/Sigmodon-hispidus_descending-weight.csv")


## Task 4: Using group_by() and summarize(), find the mean, min, and max weight for each species.
stats_species <- surveys_complete %>% 
                    group_by(species_id) %>% 
                    summarize(
                      count = n(),                                 # counts the most abundant species
                      ave_weight = mean(weight, na.rm = TRUE),     # calculates the mean weight of each species 
                      min_weight = min(weight, na.rm = TRUE),      # calculates the minimum weight of each species
                      max_weight = max(weight, na.rm = TRUE))      # calculates the maximum weight of each species

##### The most abundant species is DM.
arrange(stats_species, desc(count))

##### The rodent species with the largest mean weight is NL.
arrange(stats_species, desc(ave_weight))

##### The heaviest rodent belongs to species id NL.
arrange(stats_species, desc(max_weight))

##### The lightest rodents belong to species id's PF, PP, and RM.
arrange(stats_species, min_weight)


## Task 5: Create a scatterplot of the complete rodent survey data.

rodent_length_weight <- select(surveys_complete, species_id, hindfoot_length, weight) 
rodent_scatterplot <- mutate(rodent_length_weight, 
                        ln_hindfoot = log(hindfoot_length),
                        ln_weight = log(weight))

ggplot(data = rodent_scatterplot) +
  geom_point(mapping = aes(x = ln_weight, y = ln_hindfoot, colour = species_id)) +
  labs(y = "log(hindfoot_length)",
       x = "log(weight)",
       title = "Scatterplot of Rodent Weight and Hindfoot Length",
       subtitle = "Rodent weight in grams (g) and hindfoot length in millimeters (mm). 
Both variables were transformed by using log base e.")

## Task 6: Create a graph using a template from the R Graph gallery
##### We'll create a density chart of the hindfoot length mode of individual species using the "small multiple density charts with facet_wrap()" template from this website: https://www.r-graph-gallery.com/135-stacked-density-graph.html

install.packages("hrbrthemes")
library(hrbrthemes)
library(tidyr)
library(viridis)


ggplot(data = surveys_complete, aes(x = hindfoot_length, group = species_id, fill = species_id)) +
  geom_density(adjust = 2) +
  theme_dark() +
  facet_wrap(~species_id) +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x = element_blank()) +
  labs(y = "Density",
       x = "Hindfoot Length (mm)",
       title = "The Hindfoot Length Mode of Individual Species")

##### This plot shows the hindfoot length mode of the individual species.
