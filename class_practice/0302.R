
# Load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)

# Factor review -----------------------------------------------------------

exp <-
  read_excel('class_practice/factor_example.xlsx')

# costume a level order

exp_levels <-
  c('E', 'G', 'A', 'U')

# costume an ordered label

exp_labels <-
  c('Excellent', 'Good', 'Acceptable', 'Unsatisfactory')

# what if the level is set wrongly

exp_goof <-
  c('G', 'A', 'U')

# lessons: if end up in creating a lot of missing data, make sure all of the 
# possible values are in the vector being our levels

# create a factor with correct levels & labels

exp$score.factor <-
  exp$Score %>% 
  factor(levels = exp_levels,
         labels = exp_labels)

str(exp) # the variable 'score.factor' is shown in order, 
# and in numbers (the values), and with the level values

# create a bar chart with a costumed level order/label

exp %>% 
  ggplot(mapping = 
           aes(x = score.factor)) +
  geom_bar() +
  
  # cause there are 4 levels in all, this step introduces the 'Good' level
  
  scale_x_discrete(drop = FALSE)

# Bar charts --------------------------------------------------------------

# add in colors

exp %>% 
  ggplot(mapping = 
           aes(x = score.factor)) +
  geom_bar(fill = '#7297BF') +
  scale_x_discrete(drop = FALSE)

# add a grouping factor, and create a side-by-side bar

exp %>% 
  ggplot(mapping = 
           aes(x = score.factor)) +
  geom_bar(aes(fill = Gender),
           
           # 'dodge' will make the grouped bars side-by-side
           
           position = 'dodge') +
  scale_x_discrete(drop = FALSE)

# Stacked bar charts ------------------------------------------------------

# reshaping data from wide format to long format:
# need to create a new variable called 'party ID' (?)

congappxpid <-
  read_excel('class_practice/congappxpid.xlsx')

congapp <-
  congappxpid %>% 
  pivot_longer(
    
    # the names of variables that will contain the names of columns
    
    names_to = 'pid',
    
    # the vector short for 'percents'
               
    values_to = 'pct', 
    
    # range between column 2 and column 4
    
    2:4)

# customizing the appearance of a plot's non-data elements

# setting a theme element to 'element_blank()' for better Adobe Illustrator use

# add costumed levels to the bar chart

approval_levels <-
  c('Approve strongly', 
    'Approve not strongly',
    'Disapprove not strongly',
    'Disapprove strongly')

# factorize the wanted variable

congapp$approval <- factor(congapp$approval,
                           levels = approval_levels)

congapp %>% 
  ggplot(
    
    # 'aes()' 放在ggplot()里和geom_bar()里都是一样的，因为在这里只有一层，
    # 这相当于是global function可以被geom_bar使用
    
    aes(x = pid, 
        y = pct,
        fill = approval)) +
  geom_bar(
    
    # that means don't do any calculating, just use the data in the data set(?)
    
    stat = 'identity',
    
    # reduce the width of the bar
    
    width = 0.20) +
  theme(plot.margin = 
          unit(c(1, 1, 1, 1), 'cm')) +
  
  # Add value labels to the bar segments with 'geom_text()'
  
  geom_text(
    
    # add value labels, round the number to no decimal number for labels
    
    aes(label = round(pct, 0)), 
    
    # stack the bar segment labels(?)
    
    position = position_stack(vjust = 0.5)) +
  
  # to rotate the whole graph from vertical into horizontal
  
  coord_flip() +
  
  # add a manual color palette
  
  scale_fill_manual(values = c('#B8E186', 
                               '#E6f5D0',
                               '#F1B6DA',
                               '#DE77AE')) +
                                          
  # make it easier for Adobe Illustrator
  
  theme(
    
    # could check by yourself to see the function of each line
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    panel.background = element_blank())

# Now, it is fine to export to a PDF file and refine in Adobe Illustrator.
# Include: annotation, positioning, font family and size, data source, etc.

# Time series  ------------------------------------------------------------

# After spring break
















