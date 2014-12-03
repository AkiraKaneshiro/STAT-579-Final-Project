# read csv files
hotels <- read.csv("hotels.csv", header=T)
str(hotels)

# create a new variable
hotels$popularity <- "low"
hotels$popularity[which(hotels$review.count>100)] <- "medium"
hotels$popularity[which(hotels$review.count>1000)] <- "high"

# create a subset
hotels <- na.omit(hotels)
table(hotels$state)
hotel <- subset(hotels, state %in% c("AZ", "EDH", "NV", "WI"))

# plot histograms
library(ggplot2)
qplot(rating, ..density.., geom="histogram", facets=~popularity, data=hotels)
qplot(rating, ..density.., geom="histogram", facets=price.range~wi.fi, data=hotels)
qplot(rating, ..density.., geom="histogram", facets=price.range~state, data=hotel)