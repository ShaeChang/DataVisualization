
# Load library ------------------------------------------------------------

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

vasol <-
  read_excel('class_practice/vasol.xls')

# the wealthier part of the state: north VA, we are going to study the 
# difference of their past rate in different counties

nva_dists <- c("Arlington", "Clarke", "Culpeper", 
               "Fairfax", "Fauquier", "Frederick", 
               "King George", "Loudoun", "Prince William", 
               "Spotsylvania", "Stafford", "Warren", 
               "Alexandria", "Falls Church", "Fredericksburg", 
               "Manassas", "Manassas Park")

# Select the interested variables

nva.math <-
  vasol %>% 
  filter(
    
    # the division is in North VA region
    
    Division %in% nva_dists,
    
    # and the subject is Math
    
    Subject == 'Mathematics') %>% 
  
  # Change the proportion data record to the rate
  
  rename(pass_rate = 'Pass Rate',
         test_year = 'Test Year') %>% 
  
  mutate(pass = pass_rate * 100)

# Create line plots -------------------------------------------------------

# It does shows the sudden fall of the pass rate after 2012:

nva.math %>% 
  ggplot(
    mapping = aes(
      x = test_year,
      y = pass)) +
  geom_line(
    mapping = aes(color = Division))

# Look at both math and English reading:

nva.data <-
  vasol %>% 
  filter(Division %in% nva_dists) %>% 
  rename(pass_rate = 'Pass Rate',
         test_year = 'Test Year') %>% 
  mutate(pass = pass_rate * 100)

# Line plot, with one more imported variable

nva.data %>% 
  ggplot() +
  geom_line(mapping = 
              aes(x = test_year,
                  y = pass,
                  color = Subject),
            
            # make the line more dense
            
            size = 0.8) +
  
  # create a subplot for each value of a categorical variable
  
  facet_wrap(~ Division, ncol = 5) +
  labs(y = 'SOL Pass Rate') +
  
  # change the ticks on the x-axis to specify the breaks
  
  scale_x_continuous(
    breaks = c(2006, 2009, 2012, 2015)) +
  scale_y_continuous(
    breaks = c(40, 60, 80, 100)) +
  scale_color_manual(
    name = 'Subject',
    values = c(Mathematics = 'orange',
               
               # use this special quotation mark to quote a name of a variable
               
               `English Reading` = 'dodgerblue'),
    
    # give names to the categorical variable, 
    # a way to give the third variable labels
    
    labels = c(Mathematics = 'Math',
               `English Reading` = 'Reading')) +
  
  # apply a minimalistic theme with no background annotations
  
  theme_minimal() +
  
  # adjust the legend's position to the bottom, 
  # to avoid the figure to be compressed horizontally, 
  # cause there are more items horizontally comparing to vertical columns
  
  theme(legend.position = 'bottom')

# Create a heat map -------------------------------------------------------

# the 1st version of the heat map

p1 <-
  nva.math %>% 
  ggplot(
    mapping = 
      aes(x = test_year,
          y = Division)) +
  geom_tile(
    aes(fill = pass))

# Reverse order of factor levels for the y-axis:

  # 1. turn the division into a factor

nva.math$Division <-
  factor(nva.math$Division)

  # 2. reverse it

nva.math$Division <-
  fct_rev(nva.math$Division)

# Make the heat map again

p2 <-
  nva.math %>% 
  ggplot(
    mapping = 
      aes(x = test_year,
          y = Division)) +
  geom_tile(
    aes(fill = pass)) +
  
  # adjust the x-axis ticks and marks
  
  scale_x_continuous(
    breaks = 
      
      # generate a sequence of values, with 'from', 'to', and default 'by = 1'
      
      seq(2005, 2016)) +
  
  # adjust the color gradient
  
  scale_fill_gradient(
    
    # color for the low end of the gradient
    
    low = 'steelblue1',
    
    # color for the high end of the gradient
    
    high = 'slateblue4',
    
    # vector that specifies the minimum and maximum values of the scale
    
    limits = 
      c(40, 100))

# Or, made the 3rd heat map with 'scale_fill_gradient2', for a diverging scale

p3 <-
  nva.math %>% 
  ggplot(
    mapping = 
      aes(x = test_year,
          y = Division)) +
  geom_tile(
    aes(fill = pass)) +
  scale_x_continuous(
    breaks = 
      seq(2005, 2016)) +
  
  # use a more advanced statement
  
  scale_fill_gradient2(
    
    # a custom name for the scale. '\n' for start a new line
    
    name = 'Pass\nRate',
    
    # color for the low end of the gradient
    
    low = 'red3',
    
    # color for the middle of the gradient, for diverging scale
    
    mid = 'gray99',
    high = 'green4',
    
    # specify the midpoint that the category diverges
    
    midpoint = 70,
    limits = 
      
      # use a color gradient from (40, 100) but not (0, 100):
      # more variation is created and made it more clear
      
      c(40, 100)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank())

# Layers ------------------------------------------------------------------

# create a subset of the data that is 2009 to 2012

nva.math.2009to2012 <-
  nva.math %>% 
  filter(test_year %in% 
           seq(2009, 2012, 1))

# create a multiple-layer graph

ggplot() +
  
  # 1st layer: the reference rectangle
  # create plot area region that indicates the post-treatment period of time
  # draws a rectangle using locations of four corners
  
  geom_rect(
    
    # you should not include any dataset inside this statement,
    # cause it will iterate through every line of your dataset and
    # made your rectangular really opaque, cause each line of your data
    # are seen as true
    
    aes(xmin = 2011,
        xmax = 2012,
        
        # negative and positive infinity
        
        ymin = -Inf,
        ymax = Inf),
    
    # transparency
    
    alpha = 0.2,
    fill = 'orange') +
  
  # 2nd layer: lines, with dataset
  
  geom_line(
    
    # ??
    
    data = nva.math.2009to2012,
    mapping = aes(x = test_year,
                  y = pass_rate * 100,
                  
                  # in 'group' argument should be a character data type
                  
                  group = Division),
    color = 'gray',
    linewidth = 0.8) +
  
  # 3rd layer: points, with dataset
  
  geom_point(
    data = nva.math.2009to2012,
    mapping = aes(x = test_year,
                  y = pass_rate * 100,
                  group = Division),
    color = 'gray',
    size = 1.8) +
  theme_minimal()

# With the same logic of layers, we could also create a separate line that 
# highlights the performance of the Falls Church division schools.
















