
# Coding challenge --------------------------------------------------------

# New package: 'ggridges'

library(ggridges)
library(readxl)
library(tidyverse)

data <-
  read_excel('class_practice/p4v2015.1.xls') %>% 
  select(country, year, polity2) %>% 
  filter(
    !is.na(polity2)) %>% 
  filter(year %in% 
           seq(1950, 2010, 5))

# factorize the variable and put it into the right order

data$year <-
  factor(data$year) %>% 
  fct_rev()

data %>% 
  ggplot(
    mapping = 
      aes(x = polity2,
          y = year)) +
  geom_density_ridges(
    fill = 'blue3',
    alpha = 0.2,
    color = 'black',
    stroke = 1) +
  theme_minimal() +
  labs(x = 'Polity Democratization Score') +
  theme(axis.title.y = element_blank())
  
# Moving average time series ----------------------------------------------

# Optional video provided online for 03/30

# Tableau -----------------------------------------------------------------








