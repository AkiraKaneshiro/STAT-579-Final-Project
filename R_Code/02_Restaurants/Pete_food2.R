# Figures for use in paper

## Section: Restaurants

library(ggplot2)
library(dplyr)
library(googleVis)

# Las Vegas, NV
gvisNV <- food %>%
  subset(state == "NV" & type %in% c("asian", "indian") & 
           takeout == TRUE & alcohol == "full_bar") %>% 
  gvisMap("location", "tip", options = list(showTip = TRUE,
                                            mapType = "normal"))
plot(gvisNV)

# Pheonix, AZ
gvisAZ <- food %>%
  subset(state == "AZ" & (hipster == T | divey == T) & 
           live_music == F & tv == T & review_count > 35) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE,
                                            mapType = "normal"))
plot(gvisAZ)


## Subsection: Premium of reservations

food %>% filter(waiters == TRUE, !is.na(reservations)) %>%
  group_by(type, reservations) %>%
  summarize(price = mean(price, na.rm=T),
            stars = mean(stars)) %>%
  mutate(x = paste(type, as.character(reservations), sep = ":")) %>%
  ggplot(aes(x = x, y = price, fill = type)) + 
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

# reorder levels of 'noise' 
# food$noise <- factor(food$noise, levels = levels(food$noise)[c(3,1,2,4)])
food %>% subset(!is.na(noise)) %>% 
  ggplot(aes(x = noise, y = stars, group=live_music)) + 
  geom_point(position="jitter", alpha = 0.7) + facet_grid(~live_music) + 
  geom_smooth(method="loess")

food %>% group_by(noise) %>%
  summarize(stars = mean(stars))

food %>% group_by(noise, live_music) %>%
  summarize(stars = mean(stars))