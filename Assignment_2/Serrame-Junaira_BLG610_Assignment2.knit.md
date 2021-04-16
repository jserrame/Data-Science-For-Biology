---
title: "BLG610 Assignment 2: Projects, Coding Practice, Workflows, and R Markdown"
author: "Junaira Serrame"
date: "26/02/2021"
output: html_document
---



***
**Project detail:** The surveys_complete.csv data file contains records from a survey of rodents on different treatment plots. Using the tools learned from weeks 3 and 4, data visualization and transformation were done on the surveys_complete.csv data.

***

> #### The R packages used in this project are ggplot2, tidyverse, and dplyr.

```r
library(ggplot2)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.3     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.0
## ✓ purrr   0.3.4
```

```
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(dplyr)
```



> #### The first step is to read the surveys_complete.csv file.

```r
surveys_complete <- read.csv("/Users/airaserrame/Desktop/BLG610 - Data Science for Biology/R_proj/BLG610_Assignment2/data/surveys_complete.csv")
view(surveys_complete)
dim(surveys_complete)
```

```
## [1] 30463    14
```
##### From this, we find that the survey includes 30,463 rodents and 14 different attributes.


> #### In this script, we'll look at the number of ***Sigmodon hispidus*** that were caught in each plot_type surveyed. 

```r
#To make our script look neater, we'll use the pipe tool (%>%) in our code
surveys_complete %>% #creates a table with subsets that we're only interested in. 
  select(genus, species, plot_type, weight) %>%  #only selects for tgenus, species, plot_type, and weight
  filter(genus == "Sigmodon") %>% #this line only includes the genus Sigmodon
  filter(species == "hispidus") %>% #this line only includes the species hispidus
  group_by(plot_type) %>% #this line groups the dataframe based on plot_type
  count(plot_type) #this line counts the number of ***Sigmodon hispidus*** that in each plot_type
```

```
## # A tibble: 5 x 2
## # Groups:   plot_type [5]
##   plot_type                     n
##   <chr>                     <int>
## 1 Control                      42
## 2 Long-term Krat Exclosure     61
## 3 Rodent Exclosure              9
## 4 Short-term Krat Exclosure    15
## 5 Spectab exclosure             1
```
##### 





