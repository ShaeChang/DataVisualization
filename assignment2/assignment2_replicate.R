
# Assignment 02

# Load packages -----------------------------------------------------------

library(readr)
library(ggplot2)

# Replication of figure 6-15 -------------------------------------------

# This graph tries to replicate a bubble chart that introduces a third 
# dimension of state population in visualization of the relationship between the
# burglary rate and the murder rate.

# Referred from graph 6-15 in than Yau’s book Visualize This.

crimeRatesByState2005 <-
  read_delim('http://datasets.flowingdata.com/crimeRatesByState2005.tsv',
             '\t', escape_double = FALSE, trim_ws = TRUE)
View(crimeRatesByState2005)

# Create the replication of the graph

BubbleChartReplication <-
  
  # use 'symbols' to create the third dimension with circles
  
  symbols(crimeRatesByState2005$murder, 
          crimeRatesByState2005$burglary,
          
          # to set the area of each bubble to represent population
          
          circles = 
            sqrt(crimeRatesByState2005$population / pi),
          
          # to scale down the circles to appropriate size
          
          inches = 0.2,
          
          # change stroke color and fill color, respectively
          
          fg = 'white',
          bg = 'red',
          
          # add the labs
          
          xlab = 'Murder Rate',
          ylab = 'Burglary Rate') +
  
  # set the name of each bubbles as the third dimension using 'text'
  
  text(crimeRatesByState2005$murder,
       crimeRatesByState2005$burglary,
       
       # the actual labels are state names as the third argument
       
       crimeRatesByState2005$state,
       
       # 'cex' argument controls text size
       
       cex = 0.5)















