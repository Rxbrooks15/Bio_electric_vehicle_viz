
## Data Visualization (GOVT16-QSS17) Winter 2023
## Ggplot 2, Part III
## Name: Raine Brookshire
## Date: February 9-17, 2023
##Pdf of BEV summary data attached for overview 

# Libraries ---------------------------------------------------------------

#import the packages
#use ggplot2 to add layer for visualization
library(ggplot2)
library(reshape2)
# library(nycflights13)
# library(lubridate)
library(tidyr)
library(tidyverse)
library(readr)
library(dplyr)
library(skimr)
library(forcats)
library(grid)
install.packages("usmap")
library(usmap)#For potential heatmpa visualization
library(scales)
library(extrafont)
5+5

# Data exploration --------------------------------------------------------

#In recent years, Electric cars have been a topic of conversation 
#Are they more cost efficient ?
#Even though BEV vehicles are more expensive upfront... are they more prefered by the rich ?
#Do more people prefer or own BEV ?
#Does a persons party  determine whether they buy a BEV... Democrat vs Republican
#My hypothesis would be that richer individuals would seek out BIO electric cars 
#becuase EV are usually expsensive and require electricity and maintenence eg. Tesla
#Additionally, rich people are snobby and want to feel like they are saving the environment*

BEV <- read.csv(file.choose())
BEV
glimpse(BEV)
head(BEV)
tail(BEV)
str(BEV)
skim(BEV)
summary(BEV)
unique(BEV)
ncol(BEV)
dim(BEV)
class(BEV)
View(BEV)
colnames(BEV)

BEV %>% 
  View
BEV %>% 
  select(ppinc7)

BEV %>% 
  distinct(ppinc7)
  
BEV
levels(BEV$ppinc7)
class(BEV$ppinc7)
summary(BEV$ppinc7) 

#Looking at the data. I notice some important variables that could be of use

# income <- ppinc7
#corepar <- The answer yes or no to the question of whether the individual owns a Bio electric car
# gender <- ppgender 
#Region <- urb_sub_
#level of highschool 
#ppethm <- race
#number of individuals in a house etc.


#A heatmap plot would be interersting as a way of using three variables while 
#using a unique data visualization 
# The three variables could maybe be income(for the legend), Yes or no (x axis ), and region(urban rural or suburban)
#or I could use two vars Yes or no for x axis and income for y


#Was thinking about stacked bar chart but with so may rectanges
#it becomes harder to discriminate smaller rectanges between each other 

BEV %>% 
  select(core_par, ppinc7, urb_sub_) %>%
  group_by(urb_sub_) %>% 
  count(urb_sub_)

BEV %>% 
  select(core_par, ppinc7, urb_sub_) %>%
  group_by(ppinc7) %>%
  count(ppinc7)

BEV %>% 
  select(core_par, ppinc7, urb_sub_) %>%
  group_by(core_par) %>% 
  count(core_par)

BEV %>% 
  group_by(ppstaten) %>% 
  count(ppstaten)


BEV %>%
  filter(core_par != "No") %>% 
  count()

BEV %>%
  filter(core_par == "Yes") %>% 
  count()
class(BEV$ppinc7)
BEV %>%
  filter(core_par %in% c("Yes", "No")) %>% 
  filter(!is.na(core_par)) %>% 
  group_by(core_par) %>% 
  summarise(n = n(),
            mean_income = mean(as.numeric(ppinc7), na.rm = TRUE))

BEV %>% 
  filter(core_par == "No") %>% 
  group_by(ppstaten, ppinc7) %>%
  count() 

# Heat map testing ---------------------------------------------------------

#COuld possibly use us map to create heatmap of those who voted no or yes to see if
# the state has some effect on a persons desire to purchase a BEV

View(US_temp_annual)
US_temp_

plot_usmap(regions = "states", labels = TRUE) + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())+
  theme_minimal()


ggplot(BEV, aes(x = ppstaten,
                y = ppinc7,
                fill = n))+
    geom_tile(color = "white", linewidth = 5) +
    scale_fill_gradient() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = "BEV Counts by Income Level and State",
         subtitle = "This is a heatmap of BEV counts by income level and state.",
         fill = "Number of individuals")+
    theme_minimal()

# Create a basic heatmap plot using ggplot to see some relationship
#between people who have BEV and where they live since it would make more sense that 
#those who own BEV would be in suburban or Urban locations 

heatmap_data <- BEV %>%
  filter(core_par %in% c("Yes", "No")) %>%
  group_by(urb_sub_, core_par) %>%
  count() %>% 
  group_by(core_par) %>% 
  mutate(percent_of_peeps = n/sum(n)*100) %>% 
  ggplot(aes(x = core_par, y = urb_sub_, fill = percent_of_peeps)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(x = "Urban/Suburban/Rural", y = "Income (in thousands of dollars)", 
       title = "Count of Observations by Urban/Suburban/Rural Status and Income Level")

ggsave("heatmap10.pdf", plot = heatmap_data, width = 10, height = 8, dpi = 300)

#It will probably make sense to use income for y axis since there is more data to look at 

# Testing mapping of incomes to objects -----------------------------------

BEV %>%
  filter(core_par %in% c("Yes", "No")) %>%  # filter for only Yes and No responses
  filter(!is.na(core_par)) %>% # filter out missing data in core_par
    # group_by(ppstaten, ppinc7, urb_sub_, core_par) %>%
  group_by(core_par, ppinc7) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percent_of_peeps = n/sum(n)*100) %>% 
  mutate(ppinc7_num = case_when(
    ppinc7 == "Under $10,000" ~ 1,
    ppinc7 == "$10,000 to $24,999" ~ 2,
    ppinc7 == "$25,000 to $49,999" ~ 3,
    ppinc7 == "$50,000 to $74,999" ~ 4,
    ppinc7 == "$75,000 to $99,999" ~ 5,
    ppinc7 == "$100,000 to $149,999" ~ 6,
    ppinc7 == "$150,000 or more" ~ 7)) %>%
  mutate(percent_of_peeps_weighted
         = percent_of_peeps*ppinc7_num/100)

# Final working code ------------------------------------------------------

#Summation of vars
#cor_par <- Core part of survery.. Answer to the question - do you own a BEV
#ppinc7 <- household income of family in the prior 12 months 

BEV %>%
  filter(core_par %in% c("Yes", "No")) %>%   # filter for only Yes and No responses
  filter(!is.na(core_par)) %>% # filter out missing data in core_par
  # group_by(ppstaten, ppinc7, urb_sub_, core_par) %>%
  
  # group data by core_par and ppinc7 and count number of observations
  group_by(core_par, ppinc7) %>%
  summarise(n = n()) %>%
  
  # with .groups = drop in summarise function the argument doesnt take 
  #whether the individuals own a BEV into acount and just looks at the percentage based on the number 
  # of individuals and the total population... This created a problem calculating the weighted average.
  
  
  mutate(percent_of_peeps = n/sum(n)*100) %>% # calculates relative percentage of those based
  #on their income and whether Yes own a BEV or not
  
  # maps income groups to numbers to find weighted averages (observations in ppinct are also characters and ranges not numbers )
  mutate(ppinc7_num = case_when(
    ppinc7 == "Under $10,000" ~ 1,
    ppinc7 == "$10,000 to $24,999" ~ 2,
    ppinc7 == "$25,000 to $49,999" ~ 3,
    ppinc7 == "$50,000 to $74,999" ~ 4,
    ppinc7 == "$75,000 to $99,999" ~ 5,
    ppinc7 == "$100,000 to $149,999" ~ 6,
    ppinc7 == "$150,000 or more" ~ 7)) %>%
  
  # calculate weighted percentage for each ppinc7 group
  mutate(percent_of_peeps_weighted
         = percent_of_peeps*ppinc7_num/100) %>% 

  # filter(core_par == "No") %>%
  # summarise(ppinc7_weighted_no = sum(percent_of_peeps_weighted))
  # ungroup() %>%
  # filter(core_par == "Yes") %>%
  # summarise(ppinc7_weighted_yes = sum(percent_of_peeps_weighted))

  # calculates weighted average income for each group based 
  # on whether it is a "No" or "Yes" in core_par
  mutate(ppinc7_weSighted_yes = case_when(
    core_par == "Yes" ~ sum(percent_of_peeps_weighted)
  )) %>% 
  #weighted average income(=5.02) of those that
    #do own a BEV... using the previous mapping and rounding would mean that
    # the average income for those who voted no is 75,0000- 99,000
  
    mutate(ppinc7_weighted_no = case_when(
    core_par == "No" ~ sum(percent_of_peeps_weighted)
  )) %>%
  
  #weighted average income (=4.76)of those that
    #dont own a BEV... using the previous mapping and rounding would mean that
    # the average income for those who voted no is slightly lower than  75,0000- 99,000

    # calculate percentage for each group
  ggplot(aes(x = core_par, y = ppinc7,
               fill = percent_of_peeps)) +
  
  #Creates tiled heat map
  geom_tile(color = "white", linewidth = 5) +
  #used geom_tile instead of geom_col for a stacked bar chart because the 
  # the colors would be too close and it would be harder to discriminate against colors 
  #additionally there were some scaling issues on y axis 
  
  # add green circles to show the average income for each group
  #Also add line segment to show difference in weighted mean income levels 
  geom_point(aes(x = 1, y = 4.76,
                 size = 50),
             color = "green")+
  geom_point(aes(x = 2, y = 5.02,
                 size = 50),
             color = "green")+
  geom_segment(aes(x = 1,
                   y = 4.76,
                   xend = 2,
                   yend = 5.02),
               size = 1.2,
               color = "green")+
  #Create gradient that goes from lowest percentage of individuals to highest 
  #find complementary colors*
  scale_fill_gradient(low = "#041CF6",
                      high = "#ff4500",
                      limits = c(0, 23.1),
                      n.breaks = 10)+ 
  # upper limit has to be greater than 23 for some reason
  #even though the highest percentage of those who Dont own a BEV is 23%
  
  # scale_fill_gradientn(colors = c("#00563F", "#33ff99",
  #                                          "#66ff99", "#99ff99",
  #                                          "#ccff99", "#ffcc99",
  #                                          "#ff9966", "#ff704d", 
  #                                          "#ff471a", "#ff0000", n.breaks = 10)+

  #set axis labes and titles
  scale_x_discrete(limits = c("No",
                              "Yes")) +
  scale_x_discrete(labels = c("Doesn't own a BEV",
                              "Owns a BEV"))+
  scale_y_discrete(limits = c("Under $10,000", 
                              "$10,000 to $24,999",
                              "$25,000 to $49,999",
                              "$50,000 to $74,999",
                              "$75,000 to $99,999",
                              "$100,000 to $149,999",
                              "$150,000 or more"))+

    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = margin(10, 10, 30, 10))+# Adjusts the bottom margin to make room for the text box
  
    labs(x = "", y = "Estimated household income",
         title = "Heatmap of Bio Electric Vehicle  ownership based on attributed income ",
         subtitle = "Distribution depicts relationship between levels of income and one's onwership of a BEV.",
         fill = "Percentage of individual income based
on ownership of a BEV 

Purple: very low percentage 
Orange: very high percentage")+
  
  labs(caption="Figure 1: *Note: There are 791 people that own a BEV and 213 that don't") +

  guides(size = FALSE)+
  theme_linedraw()+
  theme(text = element_text(family = "A"))+
  theme(panel.background = 
          element_rect(fill = "white",
                       colour = "white"))+
  theme(plot.background =
          element_rect(fill = "#F1F1F1"))


help("case_when")
help("scale_x_discrete")
help("theme")
help("scale_y_gradient")
help("geom_tile")








