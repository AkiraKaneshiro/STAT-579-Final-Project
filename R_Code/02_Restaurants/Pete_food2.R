# Figures for use in paper

## Section: Restaurants

library(ggplot2)
library(ggmap)
library(dplyr)

WI <- get_googlemap("university of wisconsin", zoom = 12)
ggmap(WI) + geom_point(aes(x = longitude, y = latitude, size=stars, 
                           colour = "red", alpha = 0.7), 
             data = subset(food, state == "WI"))

library(googleVis)
# Madison, WI
gvisWI <- dinner %>% 
  subset(state == "WI" & reservations == TRUE & parking_lot == TRUE) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            mapType = "normal"))
plot(gvisWI)

# Las Vegas, NV
gvisNV <- food %>%
  subset(state == "NV" & type %in% c("asian", "indian") & 
           takeout == TRUE & alcohol == "full_bar") %>% 
  gvisMap("location", "tip", options = list(showTip = TRUE,
                                            mapType = "normal"))
plot(gvisNV)

res# Pheonix, AZ
gvisAZ <- food %>%
  subset(state == "AZ" & (hipster == T | divey == T) & 
           live_music == F & tv == T & review_count > 35) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE,
                                            mapType = "normal"))
plot(gvisAZ)


