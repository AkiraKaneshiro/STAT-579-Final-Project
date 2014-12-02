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

food %>% filter(waiters == TRUE, !is.na(reservations)) %>%
  mutate(price_level = as.factor(price)) %>%
  group_by(price_level, reservations) %>%
  summarize(stars = mean(stars)) %>%
  mutate(x = paste(price_level, as.character(reservations), sep = ":")) %>%
  ggplot(aes(x = x, y = stars, fill = price_level)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("type:(takes reservations)")

food %>% filter(waiters == TRUE, !is.na(reservations), !is.na(price)) %>%
  mutate(price_level = as.factor(price)) %>%
  group_by(price_level, type, reservations) %>%
  summarize(stars = mean(stars)) %>%
  mutate(x = paste(type, as.character(reservations), sep = ":")) %>%
  ggplot(aes(x = x, y = stars, fill = type)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("type:(takes reservations)") + facet_grid(~price_level)


## Subsection: noise pollution

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

## Subsection: wordclouds

library(wordcloud)
library(tm)

top <- tips %>% subset(stars > 4) %>% select(2)
top <- sample(top$text, 500) %>% 
  paste(sep = '', collapse = ' ')
top <- gsub("[[:punct:]]", " ", top)
top <- tolower(top)
top <- gsub("great", '', top)
top <- gsub("good", '', top)
top <- gsub("best", '', top)
top <- gsub("food", '', top)
top <- gsub("love", '', top)
top <- gsub("don", '', top)
top <- gsub("get" , '', top)
top <- gsub("try", '', top)
top <- gsub("place", '', top)
top <- gsub("amazing", '', top)
wordcloud(top, max.words = 100, random.order=F, 
          scale=c(3,0.5),colors=brewer.pal(4,"Dark2"))

bottom <- tips %>% subset(stars < 2) %>% select(2)
bottom <- sample(bottom$text, 600) %>% 
  paste(sep = '', collapse = ' ')
bottom <- gsub("-", '', bottom)
bottom <- gsub("[[:punct:]]", " ", bottom)
bottom <- tolower(bottom)
bottom <- gsub("great", '', bottom)
bottom <- gsub("good", '', bottom)
bottom <- gsub("best", '', bottom)
bottom <- gsub("food", '', bottom)
bottom <- gsub("love", '', bottom)
bottom <- gsub("don", '', bottom)
bottom <- gsub("get" , '', bottom)
bottom <- gsub("try", '', bottom)
bottom <- gsub("place", '', bottom)
bottom <- gsub("amazing", '', bottom)
bottom <- gsub("bad", '', bottom)
wordcloud(bottom, max.words = 100, random.order=F, 
          scale=c(3,0.5),colors=brewer.pal(4,"Set1"))

