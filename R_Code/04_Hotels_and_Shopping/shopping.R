# read csv files
shopping <- read.csv("shopping.csv", header=T)
str(shopping)

# create new variables
shopping$lot <- "no"
shopping$lot[grep("'lot': True", shopping$parking)] <- "yes"
shopping$lot[which(shopping$parking %in% c("", "{}"))] <- NA

shopping$popularity <- "low"
shopping$popularity[which(shopping$review.count>100)] <- "medium"
shopping$popularity[which(shopping$review.count>1000)] <- "high"

# create a subset
shopping <- na.omit(shopping)
table(shopping$state)
shops <- subset(shopping, state %in% c("AZ", "EDH", "NV", "WI"))

# plot histograms
library(ggplot2)
qplot(rating, ..density.., geom="histogram", facets=~popularity, data=shopping)
qplot(rating, ..density.., geom="histogram", facets=price.range~lot, data=shopping)
qplot(rating, ..density.., geom="histogram", facets=price.range~state, data=shops)