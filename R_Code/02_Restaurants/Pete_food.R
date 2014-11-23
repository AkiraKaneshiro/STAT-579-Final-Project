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

wisc <- bfast %>% 
  filter(latitude > 43, latitude < 44, longitude < -89, longitude > -90)

# breakfast places near u wisconsin
map1 <- qmap("university of wisconsin", zoom = 12, maprange = TRUE,
     base_layer = ggplot(aes(x=longitude, y=latitude), data = wisc)) +
  geom_point()
# same result as above
wiscmap <- get_googlemap("university of wisconsin", zoom = 12)
ggmap(wiscmap) + geom_point(aes(x = longitude, y = latitude), data = wisc)

qmap("university of michigan", zoom = 12)
mich <- food %>% 
  filter(latitude > 42, latitude < 43, longitude < -83, longitude > -84)


map <- get_googlemap("new york city")
ggmap(map)

# Create gvisGeoMap for dinner ##############
# Put data in proper format for googleVis
df <- dinner %>% select(c(26, 27, 28, 32)) %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                          sep = ":"),
         tip = name %>% paste(as.character(stars), sep = ": ") %>%
           paste("stars", sep = " "))
# non-interactive
G1 <- gvisGeoMap(df, locationvar = "location", numvar = "stars", 
                 hovervar = "name", 
                 options = list(region = "US",
                                dataMode = "markers"))
plot(G1)

# interactive
gvis1 <- gvisMap(df, "location", "tip",
                 options = list(showTip = TRUE, 
                                enableScrollWheel = TRUE,
                                useMapTypeControl = TRUE,
                                mapType = "normal"))
plot(gvis1)




###########################################################################

food <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/food.csv")
food$location <- as.character(food$location)

library(ggplot2)
library(dplyr)
library(googleVis)

ggplot(subset(food, latitude < 35 & longitude < -105),
       aes(x=longitude, y=latitude,group=state,colour=state))+geom_point()

# This subset is in Edinburgh, Scotland (state = EDH)
gvis1 <- food %>% 
  subset(latitude > 55 & longitude > -15) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis1)

# Wisconsin (state = WI)
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
