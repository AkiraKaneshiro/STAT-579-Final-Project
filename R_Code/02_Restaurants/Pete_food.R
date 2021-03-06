food <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/food.csv")
bfast <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/bfast.csv")
lunch <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/lunch.csv")
dinner <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/dinner.csv")


library(ggplot2)
library(ggmap)
library(dplyr)
library(googleVis)

names(food)
unique(food$state)

## Mapping with ggmap ##

wisc <- bfast %>% 
  filter(latitude > 43, latitude < 44, longitude < -89, longitude > -90)

# breakfast places near u wisconsin
map1 <- qmap("university of wisconsin", zoom = 12, maprange = TRUE,
     base_layer = ggplot(aes(x=longitude, y=latitude), data = wisc)) +
  geom_point()
# same result as above
wiscmap <- get_googlemap("university of wisconsin", zoom = 12)
ggmap(wiscmap) + 
  geom_point(aes(x = longitude, y = latitude, size=stars, colour=type), 
             data = wisc)

qmap("university of michigan", zoom = 12)
mich <- food %>% 
  filter(latitude > 42, latitude < 43, longitude < -83, longitude > -84)


map <- get_googlemap("new york city")
ggmap(map)


###########################################################################

## Mapping with googleVis ##

food <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/food.csv")
food$location <- as.character(food$location)

library(ggplot2)
library(dplyr)
library(googleVis)

# Create gvisGeoMap for dinner 
# Put data in proper format for googleVis
# df <- dinner %>% select(c(26, 27, 28, 32)) %>% 
#   mutate(location = paste(as.character(latitude), as.character(longitude),
#                           sep = ":"),
#          tip = name %>% paste(as.character(stars), sep = ": ") %>%
#            paste("stars", sep = " "))
# non-interactive
# G1 <- gvisGeoMap(df, locationvar = "location", numvar = "stars", 
#                  hovervar = "name", 
#                  options = list(region = "US",
#                                 dataMode = "markers"))
# plot(G1)
# 
# # interactive
# gvis1 <- gvisMap(df, "location", "tip",
#                  options = list(showTip = TRUE, 
#                                 enableScrollWheel = TRUE,
#                                 useMapTypeControl = TRUE,
#                                 mapType = "normal"))
# plot(gvis1)

ggplot(subset(food, latitude > 40 & longitude > -85 & longitude < -75),
       aes(x=longitude, y=latitude,group=state,colour=state))+geom_point()

# This subset is in Edinburgh, Scotland 
# (state %in% c("EDH","ELN","FIF","KHL","MLN","XGL"))
gvis1 <- food %>% 
  subset(latitude > 55 & longitude > -15) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis1)

# Wisconsin (state = WI) note: state = GA is also something near Madison, WI
# there is only 1 observation, though
gvis2 <- food %>% 
  subset(latitude > 40 & longitude < -85) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis2)

# Ontario (state = ON)
gvis3 <- food %>% 
  subset(latitude > 40 & longitude > -85 & longitude < -75) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis3)

# Nevada (state = NV)
gvis4 <- food %>% 
  subset(latitude > 35 & longitude < -105) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis4)

# Arizona (state = AZ)
gvis5 <- food %>% 
  subset(latitude < 35 & longitude < -105) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis5)

###########################################################################

### Data mutating ###

# Make another variable for well-known categories, e.g. Chinese, pizza, etc.
# Chinese / Asian, pizza, American, fast food, Italian, Indian, Mediterranean, 
# Mexican

# food$type <- NA
# 
# grep("pizza", food$categories[1:10], ignore.case=T)
# food$type[grep("pizza", food$categories, ignore.case=T)] <- "pizza"
# food$type[grep("american", food$categories, ignore.case=T)] <- "american"
# food$type[grep("asian", food$categories, ignore.case=T)] <- "asian"
# food$type[grep("chinese", food$categories, ignore.case=T)] <- "asian"
# food$type[grep("fast food", food$categories, ignore.case=T)] <- "fast food"
# food$type[grep("italian", food$categories, ignore.case=T)] <- "italian"
# food$type[grep("indian", food$categories, ignore.case=T)] <- "indian"
# food$type[grep("mediterranean", food$categories, ignore.case=T)] <- "mediterranean"
# food$type[grep("mexican", food$categories, ignore.case=T)] <- "mexican"


###########################################################################

## Plotting ##

qplot(type, stars, data=food, geom="jitter")
qplot(type, stars, data=bfast, geom="jitter")
qplot(type, stars, data=lunch, geom="jitter")
qplot(type, stars, data=dinner, geom="jitter")

qplot(type, stars, data=dinner, colour=price, 
      geom="jitter", size=I(5), alpha=I(0.5))

qplot(type, stars, data=dinner, colour=price, 
      geom="jitter", size=I(5), alpha=I(0.8))

qplot(type, stars, data=dinner, colour=price, geom="jitter", size=I(5), 
      alpha=I(0.8), facets=~attire)

qplot(type, price, size=stars, colour=stars,
      data=dinner, geom="jitter", alpha=I(0.5))

qplot(type, stars, data=dinner, facets=~smoking, geom="jitter")

qplot(open, stars, data=food, geom="jitter")

qplot(price, stars, data=bfast, geom="jitter")
qplot(price, stars, data=lunch, geom="jitter")
qplot(hipster, stars, data=food, geom="jitter")

qplot(type, data=food, facets=~open)
qplot(type, data=food, fill=open)

###########################################################################

## Summaries ##

counts <- food %>% group_by(city, state) %>% summarize(count = length(name))
counts$country <- "Scotland"
counts$country[counts$state %in% c("AZ", "WI", "GA", "NV")] <- "USA"
counts$country[counts$state == "ON"] <- "Canada"
counts <- counts[,c(1,2,4,3)]

#write.csv(counts, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/city_counts.csv", row.names=F)

food$country <- "Scotland"
food$country[food$state %in% c("AZ", "WI", "GA", "NV")] <- "USA"
food$country[food$state == "ON"] <- "Canada"

# Summaries by country

food %>% group_by(country) %>%
  summarize(pct.smoking = 100*length(which(smoking == "yes")) / 
              length(which(!is.na(smoking))),
            outdoor.smoking = 100*length(which(smoking=="outdoor")) / 
              length(which(!is.na(smoking))))

food %>% group_by(country) %>%
  summarize(pct.takes.res = 100*sum(reservations, na.rm=T)/
              length(which(!is.na(reservations))))

food %>% group_by(country) %>%
  summarize(pct.live.music = 100*sum(live_music, na.rm=T)/
              length(which(!is.na(live_music))))

# Summaries by type

food %>% group_by(type) %>%
  summarize(pct.takes.res = 100*sum(reservations, na.rm=T)/
             length(which(!is.na(reservations))))

food %>% group_by(type) %>%
  summarize(price = mean(price, na.rm=T),
            stars = mean(stars))

food %>% group_by(type) %>% summarize(avg.stars = mean(stars),
                                      s = sd(stars))

food %>% group_by(type, open) %>% summarize(avg.stars = mean(stars),
                                            s = sd(stars))

# Question: What is the best deal, i.e. which types of (sit-down) restaurants 
# provide the greatest average stars for the lowest price?

food %>% filter(waiters == TRUE, !is.na(reservations)) %>%
  group_by(type, reservations) %>%
  summarize(price = mean(price, na.rm=T),
            stars = mean(stars))
# Seems like you pay a premium to be able to make reservations yet don't
# necessarily get better quality
food %>% filter(waiters == TRUE, !is.na(reservations)) %>%
  group_by(type, reservations) %>%
  summarize(price = mean(price, na.rm=T),
            stars = mean(stars)) %>%
  mutate(x = paste(type, as.character(reservations), sep = ":")) %>%
  ggplot(aes(x = x, y = price)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("type:(takes reservations)")

food %>% filter(waiters == TRUE, !is.na(reservations)) %>%
  group_by(type, reservations) %>%
  summarize(price = mean(price, na.rm=T),
            stars = mean(stars)) %>%
  mutate(x = paste(type, as.character(reservations), sep = ":")) %>%
  ggplot(aes(x = x, y = stars, fill = type)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("type:(takes reservations)")



###########################################################################

## ggvis plotting ##

library(ggvis)

food %>% ggvis(~type, fill = ~as.factor(open)) %>% layer_bars()

food %>% subset(city == "Las Vegas") %>% 
  ggvis(~longitude, ~latitude, key := ~tip) %>% 
  layer_points(size:=input_slider(10, 100, value = 20, label = "size"), 
               opacity:=input_slider(0.1, 0.9, value=0.5, label="opacity")) %>% 
  add_tooltip(function(df) df$tip)


food %>% subset(city == "Las Vegas") %>% 
  mutate(takes_reservations = as.factor(reservations)) %>%
  ggvis(~longitude, ~latitude, key := ~tip) %>% 
  layer_points(fill = ~takes_reservations,
               opacity:=input_slider(0.1, 0.9, value=0.5, label="opacity")) %>% 
  add_tooltip(function(df) df$tip)


###########################################################################

## wordclouds ##
library(wordcloud)
library(tm)

wordcloud("May our children and our children's children to a
thousand generations, continue to enjoy the benefits conferred
upon us by a united country, and have cause yet to rejoice under
those glorious institutions bequeathed us by Washington and his
compeers.",colors=brewer.pal(6,"Dark2"),random.order=FALSE)

# Maybe apply this to tips dataset

x <- paste(food$categories[3], food$categories[6], sep=" ")
x <- gsub(",", ", ", x)
wordcloud(x, random.order=F)