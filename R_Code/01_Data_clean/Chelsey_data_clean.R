# This is Pete's code for cleaning the data for the restaurants except for some reason the rename() function
# did not work for me, so I didn't rename the variables.  I used it for the analysis of the 
# categories of the restaurants.
#Initial data cleaning

business <- read.csv("/Users/chelsey1221/Desktop/yelp/business.csv")
library(dplyr)
library(ggplot2)

# Let's focus on data for restaurants 
idx <- grep("restaurant", business$categories, ignore.case=T)
food <- business[idx,]
rm(business)


# get rid of unwanted variables
food <- food %>% select(-c(2,9,22,42:49,58))

food$attributes_Good.For <- as.character(food$attributes_Good.For)
food$name <- as.character(food$name)

# Accepts credit cards?
food$Accepts_CC <- NA
food$Accepts_CC[which(food$attributes_Accepts.Credit.Cards == "True")] <- TRUE
food$Accepts_CC[which(food$attributes_Accepts.Credit.Cards == "False")] <- FALSE

# Attire
levels(food$attributes_Attire)
food$attributes_Attire[food$attributes_Attire == ""] <- NA
food$attributes_Attire <- droplevels(food$attributes_Attire)


# Ambience:
# romantic, intimate, touristy, hipster, divey, classy, trendy, upscale, casual
levels(food$attributes_Ambience)
food$Romantic <- FALSE
food$Romantic[grep("romantic': True", food$attributes_Ambience)] <- TRUE
food$Intimate <- F
food$Intimate[grep("intimate': True", food$attributes_Ambience)] <- TRUE
food$Touristy <- F
food$Touristy[grep("touristy': True", food$attributes_Ambience)] <- TRUE
food$Hipster <- F
food$Hipster[grep("hipster': True", food$attributes_Ambience)] <- TRUE
food$Divey <- F
food$Divey[grep("divey': True", food$attributes_Ambience)] <- TRUE
food$Classy <- F
food$Classy[grep("classy': True", food$attributes_Ambience)] <- TRUE
food$Trendy <- F
food$Trendy[grep("trendy': True", food$attributes_Ambience)] <- TRUE
food$Upscale <- F
food$Upscale[grep("upscale': True", food$attributes_Ambience)] <- TRUE
food$Casual <- F
food$Casual[grep("casual': True", food$attributes_Ambience)] <- TRUE

# Alcohol
food$attributes_Alcohol[food$attributes_Alcohol == ""] <- NA
food$attributes_Alcohol <- droplevels(food$attributes_Alcohol)



# Noise level

food$attributes_Noise.Level[food$ attributes_Noise.Level == ""] <- NA
food$attributes_Noise.Level <- droplevels(food$attributes_Noise.Level)


# Remove some more variables
food <- food %>% select(-c(9, 4, 1, 8, 18, 19, 24, 25))

# Remove "ages" because it is not very helpful
food <- food %>% select(-1)


# Delivery
food$attributes_Delivery <- as.logical(food$attributes_Delivery)

# Dogs
food$attributes_Dogs.Allowed <- as.logical(food$attributes_Dogs.Allowed)

# Dancing
food$attributes_Good.For.Dancing<- as.logical(food$attributes_Good.For.Dancing)

# Drivethru
food$attributes_Drive.Thru <- as.logical(food$attributes_Drive.Thru)

# Groups (Good for groups?)
food$attributes_Good.For.Groups <- as.logical(food$attributes_Good.For.Groups)

# Happy hour
food$attributes_Happy.Hour  <- as.logical(food$attributes_Happy.Hour )

# TV
food$attributes_Has.TV <- as.logical(food$attributes_Has.TV)

# Outdoor (Outdoor seating?)
food$attributes_Outdoor.Seating <- as.logical(food$attributes_Outdoor.Seating)

# Smoking
food$attributes_Smoking[food$attributes_Smoking == ""] <- NA
food$Sattributes_Smoking <- droplevels(food$attributes_Smoking)

# Takeout
food$attributes_Take.out <- as.logical(food$attributes_Take.out)

# Reservations
food$attributes_Takes.Reservations <- as.logical(food$attributes_Takes.Reservations)

# Waiters (Waiter service?)
food$attributes_Waiter.Service <- as.logical(food$attributes_Waiter.Service)

# Wifi
food$attributes_Wi.Fi[food$attributes_Wi.Fi == ""] <- NA
food$attributes_Wi.Fi<- droplevels(food$attributes_Wi.Fi)

# open (Is the restaurant still open?)
food$open <- as.logical(food$open)

# get rid of Payments
food <- food %>% select(-19)

# Parking
food$attributes_Parking <- FALSE
food$attributes_Parking[grep("lot': True", food$attributes_Parking)] <- TRUE
food$Valet <- FALSE
food$Valet[grep("valet': True", food$attributes_Parking)] <- TRUE
food <- food %>% select(-18)

# Music
food$Live_music <- FALSE
food$Live_music[grep("live': True", food$attributes_Music)] <- TRUE
food$karaoke <- FALSE
food$karaoke[grep("karaoke': True", food$attributes_Music)] <- TRUE
food <- food %>% select(-15)

# Dietary is pretty boring
food <- food %>% select(-7)

names(food) <- tolower(names(food))


##### Additional cleaning #####


food$type <- NA

food$type[grep("pizza", food$categories, ignore.case=T)] <- "pizza"
food$type[grep("american", food$categories, ignore.case=T)] <- "american"
food$type[grep("asian", food$categories, ignore.case=T)] <- "asian"
food$type[grep("chinese", food$categories, ignore.case=T)] <- "asian"
food$type[grep("fast food", food$categories, ignore.case=T)] <- "fast food"
food$type[grep("italian", food$categories, ignore.case=T)] <- "italian"
food$type[grep("indian", food$categories, ignore.case=T)] <- "indian"
food$type[grep("mediterranean", food$categories, ignore.case=T)] <- "mediterranean"
food$type[grep("mexican", food$categories, ignore.case=T)] <- "mexican"

write.csv(food, "/Users/chelsey1221/Desktop/Stat579/food")

############ Analysis of food by category#################################

#Simple plot of just the counts of each, boring
qplot(type, data = food)+ coord_flip()

#Price range
qplot(attributes_price.range, data = food, fill = type, binwidth = 0.5)
qplot(stars, data = food, fill = as.factor(attributes_price.range), binwidth = 0.5)


#Category with delivery
qplot(type, data = food, fill = attributes_delivery) + coord_flip()

#Facet by type.
# add in delivery. Not terribly interesting
qplot(stars, data = food, facets = .~type, fill = attributes_delivery) 

#Delviery and type of food
qplot(review_count, data = food, facets = .~type, fill= attributes_delivery) 

#takeout
qplot(stars, data = food, facets = .~type, fill= attributes_take.out) 
qplot(attributes_take.out, data = food, fill = type) # I like this one better

qplot(stars, data = food, facets = .~type, fill= open)
qplot(attributes_waiter.service, data = food, fill = type)






################## Bars ########################
# In order to do the bars analysis, I wanted to look at not only the bars in restaurants, but also the
# bars that don't serve food.  I subset by "Full bar" because this includes clubs and hotels that serve drinks,
# (under "categories" some don't say "Bars" they say "Nightlife" or a variety of other things) but not nessesarily food.


business <- read.csv("/Users/chelsey1221/Desktop/yelp/business.csv")
library(dplyr)
library(ggplot2)
library(plyr)
# Subset the business data set to contain only the information on the places with full bars

bar = subset(business, attributes_Alcohol == "full_bar")

bar <-bar
bar$attributes_Good.For <- as.character(bar$attributes_Good.For)
bar$name <- as.character(bar$name)

# Accepts credit cards?
bar$Accepts_CC <- NA
bar$Accepts_CC[which(bar$attributes_Accepts.Credit.Cards == "True")] <- TRUE
bar$Accepts_CC[which(bar$attributes_Accepts.Credit.Cards == "False")] <- FALSE

# Attire
#levels(bar$attributes_Attire)
bar$attributes_Attire[which(bar$attributes_Attire == "")] <- NA
bar$attributes_Attire <- droplevels(bar$attributes_Attire)


# Ambience:
# romantic, intimate, touristy, hipster, divey, classy, trendy, upscale, casual
levels(bar$attributes_Ambience)
bar$Romantic <- FALSE
bar$Romantic[grep("romantic': True", bar$attributes_Ambience)] <- TRUE
bar$Intimate <- F
bar$Intimate[grep("intimate': True", bar$attributes_Ambience)] <- TRUE
bar$Touristy <- F
bar$Touristy[grep("touristy': True",bar$attributes_Ambience)] <- TRUE
bar$Hipster <- F
bar$Hipster[grep("hipster': True",bar$attributes_Ambience)] <- TRUE
bar$Divey <- F
bar$Divey[grep("divey': True", bar$attributes_Ambience)] <- TRUE
bar$Classy <- F
bar$Classy[grep("classy': True", bar$attributes_Ambience)] <- TRUE
bar$Trendy <- F
bar$Trendy[grep("trendy': True",bar$attributes_Ambience)] <- TRUE
bar$Upscale <- F
bar$Upscale[grep("upscale': True", bar$attributes_Ambience)] <- TRUE
bar$Casual <- F
bar$Casual[grep("casual': True", bar$attributes_Ambience)] <- TRUE

# Alcohol
bar$attributes_Alcohol[bar$attributes_Alcohol == ""] <- NA
bar$attributes_Alcohol <- droplevels(bar$attributes_Alcohol)


# Noise level

bar$attributes_Noise.Level[bar$attributes_Noise.Level == ""] <- NA
bar$attributes_Noise.Level <- droplevels(bar$attributes_Noise.Level)


# Delivery
bar$attributes_Delivery <- as.logical(bar$attributes_Delivery)

# Dogs
bar$attributes_Dogs.Allowed <- as.logical(bar$attributes_Dogs.Allowed)

# Dancing
bar$attributes_Good.For.Dancing <- as.logical(bar$attributes_Good.For.Dancing)



# Groups (Good for groups?)
bar$attributes_Good.For.Groups <- as.logical(bar$attributes_Good.For.Groups)

# Happy hour
bar$attributes_Happy.Hour <- as.logical(bar$attributes_Happy.Hour)

# TV
bar$attributes_Has.TV <- as.logical(bar$attributes_Has.TV)

# Outdoor (Outdoor seating?)
bar$attributes_Outdoor.Seating <- as.logical(bar$attributes_Outdoor.Seating)

# Smoking
bar$attributes_Smoking[bar$attributes_Smoking == ""] <- NA
bar$attributes_Smoking <- droplevels(bar$attributes_Smoking)

# Takeout
bar$attributes_Take.out <- as.logical(bar$attributes_Take.out)

# Reservations
bar$attributes_Takes.Reservations <- as.logical(bar$attributes_Takes.Reservations)

# Waiters (Waiter service?)
bar$attributes_Waiter.Service <- as.logical(bar$attributes_Waiter.Service)


# Wifi
bar$attributes_Wi.Fi[bar$attributes_Wi.Fi == ""] <- NA
bar$attributes_Wi.Fi <- droplevels(bar$attributes_Wi.Fi)

# open (Is the bar still open?)
bar$open <- as.logical(bar$open)


# Parking
bar$attributes_Parking <- FALSE
bar$attributes_Parking[grep("lot': True", bar$attributes_Parking)] <- TRUE
bar$Valet <- FALSE
bar$Valet[grep("valet': True", bar$attributes_Parking)] <- TRUE

# Music
bar$Live_music <- FALSE
bar$Live_music[grep("live': True", bar$attributes_Music)] <- TRUE
bar$karaoke <- FALSE
bar$karaoke[grep("'karaoke': True", bar$attributes_Music)] <- TRUE



names(bar) <- tolower(names(bar))

##### I kept the type because though not all serve food, it might be interesting to see the most common
# food served


bar$type <- NA

bar$type[grep("pizza", bar$categories, ignore.case=T)] <- "pizza"
bar$type[grep("american", bar$categories, ignore.case=T)] <- "american"
bar$type[grep("asian", bar$categories, ignore.case=T)] <- "asian"
bar$type[grep("chinese", bar$categories, ignore.case=T)] <- "asian"
bar$type[grep("fast food", bar$categories, ignore.case=T)] <- "fast food"
bar$type[grep("italian", bar$categories, ignore.case=T)] <- "italian"
bar$type[grep("indian", bar$categories, ignore.case=T)] <- "indian"
bar$type[grep("mediterranean", bar$categories, ignore.case=T)] <- "mediterranean"
bar$type[grep("mexican", bar$categories, ignore.case=T)] <- "mexican"

# Get rid of unwanted variables
bar = bar %>% select(-c(2,3,4,6:9,12,14,15,20,21,22,28,37,39,43:49))
names(bar)


write.csv(bar, "/Users/chelsey1221/Desktop/Stat579/Bars" )

########Analysis#########################



#Most have tvs
qplot(stars, data = bar,  fill  = attributes_has.tv )
# Symmetric most average or quiet
qplot(stars, data = bar,  fill  = attributes_noise.level)
# Not many cheap bars, or very expensive bars
qplot(stars, data = bar,  fill  = as.factor(attributes_price.range), binwidth=0.5)

# Lots of American food served at bars
qplot(stars, data = bar,  fill  = type)
#Analysis of food served at bar:
# Number of each type of restaurant

table(bar$type)
sum(table(bar$type)) #=3185
Americanpct = 1594/3185
Americanpct
Italianpct = 510/3185
Italianpct
Mexpct = 528/3185
Mexpct

# Number of each type of restaurant given a number of stars




# Good for dancing....
qplot(stars, data = bar,  fill  = attributes_good.for.dancing)
# Most are good for groups regardless of what people think to rate them....more facts
qplot(stars, data = bar,  fill  = attributes_good.for.groups)

# Find out if higher rated bars have a higher chance of being open...
qplot(stars, data = bar,  fill  = open)
#something with stars and divey
qplot(casual, data = bar )
qplot(attributes_happy.hour, data = bar, fill = attributes_has.tv)
qplot(attributes_wi.fi, data = bar, fill = attributes_happy.hour)
qplot(attributes_good.for.groups, data = bar, fill = attributes_noise.level)
qplot(casual, data = bar, fill = attributes_good.for.groups)
qplot(live_music, data = bar)
