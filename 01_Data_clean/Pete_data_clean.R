# Initial data cleaning

business <- read.csv("Classes and Work/yelp/business.csv")

# Let's focus on data for restaurants 
idx <- grep("restaurant", business$categories, ignore.case=T)
food <- business[idx,]
rm(business)

names(food)
unique(food$type)
unique(food$state)
summary(food$stars)
summary(food$review_count)
head(food$open)
summary(food$open)
head(food$name)
str(food)
unique(food$neighborhoods)
unique(food$city)

food$attributes_Good.For <- as.character(food$attributes_Good.For)
idx <- grep("lunch': True", food$attributes_Good.For)
lunch <- food[idx,]
