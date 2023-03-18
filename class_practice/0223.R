library(readxl)
library(ggplot2)

factor_example <-
  read_excel('class_practice/factor_example.xlsx')

# To string data, we could display the structure of an arbitrary R object in 
# the console
# in this case, we could see the difference before and after construct a factor

str(factor_example)

# convert to a factor

factor_example$Score.fac <- factor(factor_example$Score)

# create a graph, to show that in a factor, there is a level in sequence

ggplot(data = factor_example,
       mapping = aes(x = Score.fac)) +
  geom_bar()
