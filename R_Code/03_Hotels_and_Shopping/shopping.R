# read csv files
shopping <- read.csv("shopping.csv", header=T)
str(shopping)

# create a subset
shopping <- na.omit(shopping)
table(shopping$state)
shops <- subset(shopping, state %in% c("AZ", "EDH", "NV", "WI"))

library(ggplot2)
library(dplyr)

# summarize the data by price range
summary1 <- shopping %>% group_by(price.range) %>% mutate(N=length(rating))
summary2 <- summary1 %>% group_by(price.range, rating) %>% 
  summarise(perc=length(rating)/N[1])
qplot(factor(rating), perc, facets=~price.range, data=summary2, geom="bar", 
      stat="identity", fill=factor(rating)) + scale_fill_brewer(palette="Set2")

# summarize the data by price range and region
summary3 <- shops %>% group_by(price.range, state) %>% mutate(N=length(rating))
summary4 <- summary3 %>% group_by(price.range, state, rating) %>% 
  summarise(perc=length(rating)/N[1])
qplot(factor(rating), perc, facets=price.range~state, data=summary4, geom="bar", 
      stat="identity", fill=factor(rating)) + scale_fill_brewer(palette="Set2")