
# Load packages -----------------------------------------------------------

library(ggplot2)
library(readxl)
library(treemapify)
library(dplyr)

# Load data ---------------------------------------------------------------

# Load 'correlates of war data' on national military spending

NMC_5_0 <-
  read_excel('class_practice/cow_w_regions.xlsx',
             
             # encode these data as missing data, 
             # and 'na' represents missing data here
             
             na = c('', -9, 'NA'))

milexp_1991 <-
  NMC_5_0 %>% 
  filter(year == 1991)

milexp_2011 <-
  NMC_5_0 %>% 
  filter(year == 2011)

# Create tree maps --------------------------------------------------------

# Tree map of nations' and regions' share of global military spending, 2011

milexp_2011 %>% 
  ggplot(
    aes(area = milex,
        fill = region,
        label = name,
        subgroup = region)) +
  geom_treemap(
    show.legend = FALSE) +
  geom_treemap_subgroup_border(
    color = 'white',
    show.legend = FALSE) +
  
  # these two arguments are naturally including the legends needed
  
  geom_treemap_text(
    fontface = 'italic',
    color = 'white',
    place = 'center',
    
    # If TRUE, text will be grown as well as shrunk to fill the box.
    # Here, we chose a consistent font size.
    
    grow = F,
    
    # If TRUE, text will be reflowed (wrapped) to better fit the box.
    
    reflow = T) +
  geom_treemap_subgroup_text(
    place = 'center',
    
    # If TRUE, text will be grown as well as shrunk to fill the box.
    
    grow = T,
    alpha = 0.5,
    color = '#FAFAFA',
    
    # Minimum font size, in points. If provided, text that would need to be 
    # shrunk below this size to fit the box will not be drawn. 
    # Defaults to 4 pt.
    
    min.size = 0)

# Adding a variable: total national population 
# 增加一个颜色深浅以表示人口多少的数据维度
# to use 'scale_fill_gradient()' function to fulfill

milexp_2011 %>% 
  ggplot(
    aes(area = milex,
        
        # use total population, but not region here, to fill the color of 
        # each tiles
        
        fill = tpop,
        label = name,
        subgroup = region)) +
  geom_treemap() +
  geom_treemap_subgroup_border(
    color = 'white') +
  geom_treemap_text(
    fontface = 'italic',
    color = 'white',
    place = 'center',
    grow = FALSE,
    reflow = TRUE) +
  geom_treemap_subgroup_text(
    place = 'center',
    grow = TRUE,
    alpha = 0.5,
    color = 'gray98',
    min.size = 0) +
  labs(title = paste('Regional and National Share of Global Military Spending',
                     'in 2011'),
       subtitle = paste('The former Soviet Union`s share of spending fell',
                        'sharply after the Cold War'),
       caption = paste('Data: The Correlates of War Project National Material',
       'Capabilities')) +
  scale_fill_gradient(low = 'deepskyblue',
                      high = 'dark blue',
                      breaks = 
                        seq(0, 1400000, by = 250000),
                      
                      # to break the name into 3 lines
                      
                      name = 'Population\nin 1000s\nof people',
                      
                      # use these labels, to include ',' inside the numbers
                      # to be more clear
                      
                      labels = c('0',
                                 '250,000',
                                 '500,000',
                                 '750,000',
                                 '1,000,000',
                                 '1,250,000'),
                      limits = c(0, 1400000))


# Visualizing Correlation Matrices ----------------------------------------























