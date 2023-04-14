
# To create two original graphs

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------

EV_2010_to_2021 <-
  read_csv('data_own/IEA-EV-data.csv')

# Data wrangling ----------------------------------------------------------

EV_stock_and_sale_by_type <-
  EV_2010_to_2021 %>% 
  
  # select the interested traits: we are going to find out the sales & stocks 
  # of all kinds of EVs across different regions
  
  filter(parameter %in% 
           c('EV sales', 'EV stock')) %>% 
  
  # 

  filter(region %in% 
           c('China', 'Europe', 'USA', 'India', 'World')) %>%
  
  # we are going to specify that 'cars' counts for the most increase in the 
  # electric vehicles, especially in the US, comparing to other types of 
  # vehicles: trucks, buses, and vans.
  
  mutate(mode_new = 
           if_else(mode == 'Cars',
                   'Cars',
                   'others')) %>% 
  
  # to sum the values across different types of EVs, including PHEV and BEV.
  # to do so, we retain other grouping characteristics
  
  group_by(region, parameter, mode_new) %>% 
  summarise(
    value_new = sum(value))

# Data visualization 01 ---------------------------------------------------

# 我真正想传达的信息：美国除了cars，其他的存量和销量都很少

# 我知道了，我第一个图可以做一个，中国、美国、欧洲和其他地区Cars和Others
# 电动汽车拥有量的比率，在好几年之间改变

# 离大谱，根本没有美国buses和vans，trucks的数据，一整天白忙。论文也🈚️了。



# Data visualization 02 ---------------------------------------------------

# Since now there are 4 columns in total in our data frame, the goal is to 
# create a bar chart with both position dodge and position stack in the same
# graph. We refers to the solution from Stack Overflow.

# Split the data frame into two

EV_cars <-
  EV_without_China %>% 
  filter(mode_new == 'Cars')

EV_others <-
  EV_without_China%>% 
  filter(mode_new == 'others')

# Create a bar chart with 2 layers

ggplot() +
  geom_bar(data = EV_cars,
           mapping = aes(x = region,
                         y = value_new,
                         fill = as.factor(parameter)),
           
           # 这个到底是什么意思
           
           stat = 'identity',
           position = 'stack')

# For thesis --------------------------------------------------------------

thesis <-
  EV_2010_to_2021 %>% 
  filter(year == 2021,
         parameter == 'EV stock') %>% 
  filter(!region %in% 
           
           # exclude the regional data, in order to focus on countries
           
           c('Europe', 'Rest of the world', 'Other Europe', 'World')) %>%
  filter(category == 'Historical') %>% 
  group_by(mode) %>% 
  summarise(value_new = sum(value))

thesis_2 <-
  EV_2010_to_2021 %>% 
  filter(year == 2021,
         parameter == 'EV stock',
         region == 'World',
         category == 'Historical') %>% 
  group_by(mode) %>% 
  summarise(value_new = sum(value)) %>% 
  mutate(all = sum(value_new)) %>% 
  mutate(precentage = value_new / all)













