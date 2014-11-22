food <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/food.csv")
bfast <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/bfast.csv")
lunch <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/lunch.csv")
dinner <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/dinner.csv")


library(ggplot2)
library(ggmap)
library(dplyr)

names(food)
unique(food$state)

wisc <- bfast %>% 
  filter(latitude > 43, latitude < 44, longitude < -89, longitude > -90)

# breakfast places near u wisconsin
map1 <- qmap("university of wisconsin", zoom = 12, maprange = TRUE,
     base_layer = ggplot(aes(x=longitude, y=latitude), data = wisc)) +
  geom_point()

qmap("university of michigan", zoon = 12)
mich <- food %>% 
  filter(latitude > 42, latitude < 43, longitude < -83, longitude > -84)