
# It is an on class practice on 02/16/2023

# This script is used to check the political polarization over time. See the 
# material "A brief digression on subsetting data"

# Load packages and data --------------------------------------------------

library(haven)
library(ggplot2)
library(dplyr)

# load the data with the package 'haven', which could import '.DTA' format

dwnom_wk31 <-
  read_dta('class_practice/Weekly_DW-NOMINATE_31.DTA')

# data wrangling ----------------------------------------------------------

hr82to112 <-
  
  dwnom_wk31 %>% 
  
  # 'cd' is congressional district, 国会选区; 去掉缺失值
  # party 100 & 200 stands for D and R, respectively
  
  filter(cd != 0 
         & (party == 100 | party ==200)) %>% 
  
  # only include the representative from the congress No.82 to No.112
  
  filter(cong %in% 
           c(82, 87, 92, 97, 102, 107, 112))

# data visualization 01 ------------------------------------------------------

# Investigating univariate distribution of quantitative data

hr82to112 %>% 
  ggplot(mapping = 
           
           # 'dwnom1' here indicates the dw_number (?)
           
           aes(x = dwnom1)) +
  
  geom_histogram(mapping = 
                   
                   # to fill the 2 party into different colors
                   # change the numeric to character variable to create discrete
                   # variables to fill 2 colors
                   
                   aes(fill = as.character(party)),
                 
                 # change the 'bins' here to change the number of the bars
                 
                 bins = 20,
                 
                 # add transparency
                 
                 alpha = 0.5) +
  
  # to separate the multiple histograms by different congress numbers
  
  facet_wrap(~cong) +
  
  # customize the color fill and add the labels
  
  scale_fill_manual(values = c('100' = 'blue',
                               '200' = 'red'),
                    labels = c('100' = 'Democrates',
                               '200' = 'Republicans')) +
  
  # add titles and so on
  
  labs(title = paste('Polarization of Conservative Scale Scores of Members',
                     
                     # if sentences are too long, use 'paste()' to concatenate
                     # strings
                      
                      'of the US House of Representatives'),
       subtitle = paste('Decennial Distribution:',
                        '82nd Congress (1951-52) to 112th Congress (2011-12)'),
       
       # data source
       
       caption = 'Data: Voteview.org',
       
       # what does x-axis mean
       
       x = 'DW-Nominate Scores') +
  
  # change the theme of the pic, to remove the legend name 去除图例的名称
  
  theme(legend.title = element_blank())

# data visualization 02 ---------------------------------------------------

hr82to112 %>% 
  ggplot(mapping = aes(x = dwnom1)) +
  
  # change from the histogram to the density curve
  
  geom_density(mapping = 
                 
                 # the former is the color fill whilst the latter is the line
                 
                 aes(fill = as.character(party),
                     color = as.character(party)),
               alpha = 0.3) +
  facet_wrap(~cong) +
  
  # use 'scale_fill_manual' and 'scale_color_manual' separately to change the 
  # color as before
  
  scale_fill_manual(values = c('100' = 'blue',
                               '200' = 'red'),
                    labels = c('100' = 'Democrates',
                               '200' = 'Republicans')) +
  scale_color_manual(values = c('100' = 'blue',
                               '200' = 'red'),
                    labels = c('100' = 'Democrates',
                               '200' = 'Republicans')) +
  labs(title = paste('Polarization of Conservative Scale Scores of Members',
                      'of the US House of Representatives'),
       subtitle = paste('Decennial Distribution:',
                        '82nd Congress (1951-52) to 112th Congress (2011-12)'),
       caption = 'Data: Voteview.org',
       x = 'DW-Nominate Scores') +
  theme(legend.title = element_blank())

# Drawbacks of this density curve: what does the y-axis indicate? What does the
# '3' means? 没有得到解释
# 可能就是下方的面积乘起来等于1，所以纵轴的数值是被计算出来的

# data viz 03 Frequency Polygon -------------------------------------------

# 类别频率折线图
# 优点：y轴更易解释，因为y轴就代表着数量

hr82to112 %>% 
  ggplot(mapping = aes(x = dwnom1)) +
  geom_freqpoly(mapping = 
                  aes(color = as.character(party)),
                
                # change the bins here to smoothing the curve
                
                bins = 20) +
  facet_wrap(~cong) +
  labs(title = paste('Polarization of Conservative Scale Scores of Members',
                     'of the US House of Representatives'),
       subtitle = paste('Decennial Distribution:',
                        '82nd Congress (1951-52) to 112th Congress (2011-12)'),
       caption = 'Data: Voteview.org',
       x = 'DW-Nominate Scores') +
  scale_color_manual(values = c('100' = 'blue',
                                '200' = 'red'),
                     labels = c('100' = 'Democrates',
                                '200' = 'Republicans')) +
  theme(strip.background = element_blank(),
        legend.title = element_blank())

# data viz 04 boxplots ----------------------------------------------------

hr82to112 %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = cong,
                             y = dwnom1,
                             
                             # 'cong' is a grouping variable but not a numeric
                             
                             group = cong))

# But, the x-axis is not acurate to what exact congress it is.

# A better way is to convert 'cong' directly into a factor
# 在这两种情况下，都可以省略'facet_wrap'这一步的操作，
# 因为可以在一个图里加很多箱线图

hr82to112 %>% 
  mutate(cong = factor(cong)) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = cong,
                             y = dwnom1))

# However, we are not able to directly see the polarization trend in this 
# boxplots.
# We could use (boxplots + histogram) to demonstrate the data better.

hr82to112 %>% 
  mutate(cong = factor(cong)) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = cong,
                             y = dwnom1,
                             
                             # use different color fills to distinguish 2 parties
                             
                             fill = as.character(party)))
















