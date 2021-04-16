# Project owner: Aira Serrame, 500711485
### Date: March 13, 2021
### Project detail: This project is for BLG610 Assignment 3: Tidy Data and QA/QC

## Task 1: Tidy the BBS_Messy_test_table.csv file

library(tidyverse)
getwd()
BBS_messy <- read.csv("/Users/airaserrame/Desktop/BLG610-Data_Science_for_Biology/R_proj/Serrame-Junaira_Assignment_3/data/BBS_Messy_test_table.csv")

### For a tidier dataset:
BBS_tidy <- gather(BBS_messy, "stop_number", "bird_abundance", 3:7)

### checking if data types are correctly assigned:
class(BBS_tidy$SpeciesCode) 
class(BBS_tidy$Route)           
class(BBS_tidy$stop_number)
class(BBS_tidy$bird_abundance)  #returns character => check table to see which values weren't integers

### conversion of the strings in BBS_tidy$bird_abundance into integers:
which(BBS_tidy$bird_abundance == "two" | BBS_tidy$bird_abundance == "three")
BBS_tidy$bird_abundance[8] <- "2"
BBS_tidy$bird_abundance[11]<- "3"

class(BBS_tidy$bird_abundance)  #still returns as character
BBS_tidy$bird_abundance <- as.numeric(BBS_tidy$bird_abundance) #now returns as factor
class(BBS_tidy$bird_abundance)

### writing the resultant csv file into a .csv file:
sum(is.na(BBS_tidy))
BBS_tidy_csv <- write_csv(BBS_tidy, file = "/Users/airaserrame/Desktop/BLG610-Data_Science_for_Biology/R_proj/Serrame-Junaira_Assignment_3/output/BBS_tidy_test_table.csv", na = "2")


## Task 2: Tidy and plot the LakeTrout_fish_attributes_small.csv file

?read_delim
LakeTrout_Fish <- read_delim("/Users/airaserrame/Desktop/BLG610-Data_Science_for_Biology/R_proj/Serrame-Junaira_Assignment_3/data/LakeTrout_Fish_attributes_small.csv", 
                     delim = ",", 
                     col_types = cols(
                       Fish_Individual_ID = col_integer(),
                       Fork_Length = col_number(),
                       Total_Length = col_number(),
                       Round_Weight = col_number(),
                       Sex_ID = col_factor(levels = c("1","2","3","4")),
                       Assessed_Fish_Age = col_integer(),
                       Assessed_Age_Confidence = col_integer()))

initial_plot <- plot(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, xlab = "Lake Trout Fish Total Length(mm)",
     ylab = "Lake Trout Fish Fork Length (mm)")

identify(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, labels=row.names(LakeTrout_Fish))

### potential "illegal" values: 287, 868, 1650, 8345, 9126
### manually identify erroneous rows:
LakeTrout_Fish[285:290,]    #row 287 => erroneous point
LakeTrout_Fish[867:869,]    #row 868 => erroneous point
LakeTrout_Fish[1649:1651,]  #row 1650 => not an erroneous point
LakeTrout_Fish[8344:8346,]  #row 8435 => not an erroneous point
LakeTrout_Fish[9125:9127,]  #row 9126 => value seems quite too big => need to compare with 10 other rows
LakeTrout_Fish[9121:9131,]  #row 9126 => outlier confirmed

LakeTrout_Fish <- LakeTrout_Fish[-287,]
LakeTrout_Fish <- LakeTrout_Fish[-868,]
LakeTrout_Fish <- LakeTrout_Fish[-9126,]

plot(LakeTrout_Fish$Total_Length)
plot(LakeTrout_Fish$Total_Length[1:100]) # the first 100 values
barplot(LakeTrout_Fish$Total_Length[1:100]) # easier to look at
hist(LakeTrout_Fish$Total_Length, breaks = 20)

qqnorm(LakeTrout_Fish$Total_Length)
qqline(LakeTrout_Fish$Total_Length)
ks.test(LakeTrout_Fish$Total_Length, pnorm)

qqnorm(log(LakeTrout_Fish$Total_Length))
qqline(log(LakeTrout_Fish$Total_Length))
ks.test(log(LakeTrout_Fish$Total_Length), pnorm)

clean_plot <- plot(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, xlab = "Lake Trout Fish Total Length(mm)",
                     ylab = "Lake Trout Fish Fork Length (mm)")     #still has an outlier point
identify(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, labels=row.names(LakeTrout_Fish))
qqnorm(log(LakeTrout_Fish$Total_Length))
qqline(log(LakeTrout_Fish$Total_Length))
ks.test(log(LakeTrout_Fish$Total_Length), pnorm)

LakeTrout_Fish <- LakeTrout_Fish[-867,]   #remove the outlier on the graph
cleaner_plot <- plot(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, xlab = "Lake Trout Fish Total Length(mm)",
                   ylab = "Lake Trout Fish Fork Length (mm)")
identify(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, labels=row.names(LakeTrout_Fish))
qqnorm(log(LakeTrout_Fish$Total_Length))
qqline(log(LakeTrout_Fish$Total_Length))
ks.test(log(LakeTrout_Fish$Total_Length), pnorm)

LakeTrout_Fish <- LakeTrout_Fish[-9123,]   #remove the outlier on the graph
cleaner_plot <- plot(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, xlab = "Lake Trout Fish Total Length(mm)",
                     ylab = "Lake Trout Fish Fork Length (mm)")
identify(LakeTrout_Fish$Total_Length, LakeTrout_Fish$Fork_Length, labels=row.names(LakeTrout_Fish))
qqnorm(log(LakeTrout_Fish$Total_Length))
qqline(log(LakeTrout_Fish$Total_Length))
ks.test(log(LakeTrout_Fish$Total_Length), pnorm)

ggplot(data = LakeTrout_Fish) +
  geom_point(mapping = aes(x = log(Total_Length), y = log(Fork_Length), colour = Sex_ID)) +
  labs(y = "Lake Trout Fish Fork Length (mm)",
       x = "Lake Trout Fish Total Length (mm)",
       title = "Scatterplot of Lake Trout Fork Length and Total Length",
       subtitle = "Lake Trout Fish Fork Length and Total Length in millimeters (mm). 
Both variables were transformed by using log base e.")

## Task 3: Data screen Lake_Partners1.csv

?read_delim
Lake_Partners <- read_delim("/Users/airaserrame/Desktop/BLG610-Data_Science_for_Biology/R_proj/Serrame-Junaira_Assignment_3/data/Lake_Partners1.csv", delim = ",",
                        col_types = "ccddcddcddcc")

### Answer to question 4: 
### Since only the date of the data collection was provided and the time of data collection was not specified, 
### one can assume that TP1 and TP2 were both measured simultaneously or between very short time intervals. 
### TP1 and TP2 were also measured in the same exact location. From these,  
### we can make an assumption that TP1 will not cause a response in TP2, nor vice versa.
### Both TP1 and TP2 simply measure the same variable, total phosphorus of the lake, under identical conditions. 
### So they should not show any relationship in a scatterplot. In other words,
### when TP1 changes, TP2 is not influenced, or vice versa.

plot(Lake_Partners$`TP1 (µg/L)`, Lake_Partners$`TP2 (µg/L)`, xlab = "TP1 (µg/L)",
     ylab = "TP2 (µg/L)")
plot(Lake_Partners$`TP1 (µg/L)`[1:50], Lake_Partners$`TP2 (µg/L)`[1:50], xlab = "TP1 (µg/L)",
     ylab = "TP2 (µg/L)")   #easier to look at


### Answer to question 5:
### To screen the data for possible outliers, create a dataframe that contains only the variables we're interested in.
### Then, find the difference between TP1 and TP2. After, create a new column that contains the calculated differences.
### Make sure to remove any rows that has an empty cell before moving to the next step. 
### Then, find the average difference to determine the appropriate limit for a value to be considered as an outlier. 
### Note: Ideally, we should impose a stringent limit because TP1 and TP2 are measuring the same variable; they should both
### display the same value. 
### Finally, we'll use the filter function to remove the outliers.


### Question 6: Implement your data screening steps and make a scatterplot of the relationship between TP1 & TP2
### after performing your data screening steps.

TP1_TP2 <- select(Lake_Partners, `Site Description`, `TP1 (µg/L)`, `TP2 (µg/L)`) 
TP_scatterplot <- mutate(TP1_TP2, 
                         difference_TP = abs(`TP1 (µg/L)` - `TP2 (µg/L)`))    #creates a table of differences

sum(is.na(TP_scatterplot))  #determines whether there are NA values present in the data
TP_scatterplot_NA <- na.omit(TP_scatterplot)  #removes all rows with NA values
summary(TP_scatterplot_NA)
sum(is.na(TP_scatterplot_NA))

mean(TP_scatterplot_NA$difference_TP)   # average difference is 1.831869
TP_scatterplot_NA <- filter(TP_scatterplot_NA, difference_TP <= 2)   #let's put a very stringent filter

plot(TP_scatterplot_NA$`TP1 (µg/L)`, TP_scatterplot_NA$`TP2 (µg/L)`, xlab = "TP1 (µg/L)",
     ylab = "TP2 (µg/L)",
     main = "Scatterplot of Total Phosporus 1 and Total Phosporus 2")

#### Although the plot shows a linear relationship between the Total Phosporus 1 (TP1) and Total 
#### Phosporus 2 (TP2), TP1 and TP2 should not be treated as linear data because they both measure 
#### the same variable. This scatterplot plot should not be given any meaning. What would be potentially 
#### more useful is plotting the average of TP1 and TP2 against the site description or date.


