# read csv files
hotels <- read.csv("hotels.csv", header=T)
str(hotels)

# create a subset
hotels <- na.omit(hotels)
table(hotels$state)
hotel <- subset(hotels, state %in% c("AZ", "EDH", "NV", "WI"))

library(ggplot2)
library(dplyr)

# summarize the data by price range
summary1 <- hotels %>% group_by(price.range) %>% mutate(N=length(rating))
summary2 <- summary1 %>% group_by(price.range, rating) %>% 
  summarise(perc=length(rating)/N[1])
qplot(factor(rating), perc, facets=~price.range, data=summary2, geom="bar", 
      stat="identity", fill=factor(rating)) + scale_fill_brewer(palette="Set2")

# summarize the data by price range and region
summary3 <- hotel %>% group_by(price.range, state) %>% mutate(N=length(rating))
summary4 <- summary3 %>% group_by(price.range, state, rating) %>% 
  summarise(perc=length(rating)/N[1])
qplot(factor(rating), perc, facets=price.range~state, data=summary4, geom="bar", 
      stat="identity", fill=factor(rating)) + scale_fill_brewer(palette="Set2")