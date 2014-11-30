# Initial data cleaning

business <- read.csv("Classes and Work/yelp/business.csv")
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
food <- food %>% rename(Attire = attributes_Attire)

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
food <- food %>% rename(Alcohol = attributes_Alcohol)

# Price range
food <- food %>% rename(Price = attributes_Price.Range)

# Noise level
food <- food %>% rename(Noise = attributes_Noise.Level)
food$Noise[food$Noise == ""] <- NA
food$Noise <- droplevels(food$Noise)

# More renaming
food <- food %>% rename(BYOB = attributes_BYOB, 
                        Corkage = attributes_Corkage,
                        Dancing = attributes_Good.For.Dancing,
                        Groups = attributes_Good.For.Groups,
                        Happy_hour = attributes_Happy.Hour,
                        Music = attributes_Music,
                        Outdoor = attributes_Outdoor.Seating,
                        Smoking = attributes_Smoking,
                        TV = attributes_Has.TV,
                        Ages = attributes_Ages.Allowed,
                        Parking = attributes_Parking,
                        Reservations = attributes_Takes.Reservations,
                        Wifi = attributes_Wi.Fi,
                        Waiters = attributes_Waiter.Service,
                        Takeout = attributes_Take.out,
                        Delivery = attributes_Delivery,
                        BYOB_corkage = attributes_BYOB.Corkage,
                        Dogs = attributes_Dogs.Allowed,
                        Payments = attributes_Payment.Types,
                        Wheelchairs = attributes_Wheelchair.Accessible,
                        Drivethru = attributes_Drive.Thru,
                        Dietary = attributes_Dietary.Restrictions)

# Remove some more variables
food <- food %>% select(-c(9, 4, 1, 8, 18, 19, 24, 25))

# Remove "ages" because it is not very helpful
food <- food %>% select(-1)

# BYOB
food$BYOB <- as.character(food$BYOB)
food$BYOB[food$BYOB == "True"] <- TRUE
food$BYOB[food$BYOB == "False"] <- FALSE
food$BYOB[food$BYOB == ""] <- NA
food$BYOB <- as.logical(food$BYOB)

# BYOB corkage
food$BYOB_corkage[food$BYOB_corkage == ""] <- NA
food$BYOB_corkage <- droplevels(food$BYOB_corkage)

# Corkage
food$Corkage <- as.logical(food$Corkage)

# Delivery
food$Delivery <- as.logical(food$Delivery)

# Dogs
food$Dogs <- as.logical(food$Dogs)

# Dancing
food$Dancing <- as.logical(food$Dancing)

# Drivethru
food$Drivethru <- as.logical(food$Drivethru)

# Groups (Good for groups?)
food$Groups <- as.logical(food$Groups)

# Happy hour
food$Happy_hour <- as.logical(food$Happy_hour)

# TV
food$TV <- as.logical(food$TV)

# Outdoor (Outdoor seating?)
food$Outdoor <- as.logical(food$Outdoor)

# Smoking
food$Smoking[food$Smoking == ""] <- NA
food$Smoking <- droplevels(food$Smoking)

# Takeout
food$Takeout <- as.logical(food$Takeout)

# Reservations
food$Reservations <- as.logical(food$Reservations)

# Waiters (Waiter service?)
food$Waiters <- as.logical(food$Waiters)

# Wheelchairs (Wheelchair accessible?)
food$Wheelchairs <- as.logical(food$Wheelchairs)

# Wifi
food$Wifi[food$Wifi == ""] <- NA
food$Wifi <- droplevels(food$Wifi)

# open (Is the restaurant still open?)
food$open <- as.logical(food$open)

# get rid of Payments
food <- food %>% select(-19)

# Parking
food$Parking_lot <- FALSE
food$Parking_lot[grep("lot': True", food$Parking)] <- TRUE
food$Valet <- FALSE
food$Valet[grep("valet': True", food$Parking)] <- TRUE
food <- food %>% select(-18)

# Music
food$Live_music <- FALSE
food$Live_music[grep("live': True", food$Music)] <- TRUE
food$karaoke <- FALSE
food$karaoke[grep("karaoke': True", food$Music)] <- TRUE
food <- food %>% select(-15)

# Dietary is pretty boring
food <- food %>% select(-7)

names(food) <- tolower(names(food))


##### Additional cleaning #####

# Add variable for university that the business is near
food <- read.csv("Github/STAT-579-Final-Project/Data/Restaurants/food.csv")

library(ggplot2)
library(dplyr)
library(googleVis)

ggplot(subset(food, latitude < 35 & longitude < -105),
       aes(x=longitude, y=latitude,group=state,colour=state))+geom_point()

# This subset is in Edinburgh, Scotland (state = EDH)
gvis1 <- food %>% 
  select(c(24, 26, 27, 28, 32)) %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                          sep = ":"),
         tip = name %>% paste(as.character(stars), sep = " (") %>%
           paste("stars)", sep = " ") %>% paste(categories, sep = " ")) %>%
  subset(latitude > 55 & longitude > -15) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis1)

# Wisconsin (state = WI)
gvis2 <- food %>% 
  select(c(24, 26, 27, 28, 32)) %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                          sep = ":"),
         tip = name %>% paste(as.character(stars), sep = " (") %>%
           paste("stars)", sep = " ") %>% paste(categories, sep = " ")) %>%
  subset(latitude > 40 & longitude < -85) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis2)

# Ontario (state = ON)
gvis3 <- food %>% 
  select(c(24, 26, 27, 28, 32)) %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                          sep = ":"),
         tip = name %>% paste(as.character(stars), sep = " (") %>%
           paste("stars)", sep = " ") %>% paste(categories, sep = " ")) %>%
  subset(latitude > 40 & longitude > -85 & longitude < -75) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis3)

# Nevada (state = NV)
gvis4 <- food %>% 
  select(c(24, 26, 27, 28, 32)) %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                          sep = ":"),
         tip = name %>% paste(as.character(stars), sep = " (") %>%
           paste("stars)", sep = " ") %>% paste(categories, sep = " ")) %>%
  subset(latitude > 35 & longitude < -105) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis4)

# Arizona (state = AZ)
gvis5 <- food %>% 
  select(c(24, 26, 27, 28, 32)) %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                          sep = ":"),
         tip = name %>% paste(as.character(stars), sep = " (") %>%
           paste("stars)", sep = " ") %>% paste(categories, sep = " ")) %>%
  subset(latitude < 35 & longitude < -105) %>%
  gvisMap("location", "tip", options = list(showTip = TRUE, 
                                            enableScrollWheel = TRUE,
                                            useMapTypeControl = TRUE,
                                            mapType = "normal"))
plot(gvis5)

food$type <- "other"

food$type[grep("pizza", food$categories, ignore.case=T)] <- "pizza"
food$type[grep("american", food$categories, ignore.case=T)] <- "american"
food$type[grep("asian", food$categories, ignore.case=T)] <- "asian"
food$type[grep("chinese", food$categories, ignore.case=T)] <- "asian"
food$type[grep("fast food", food$categories, ignore.case=T)] <- "fast food"
food$type[grep("italian", food$categories, ignore.case=T)] <- "italian"
food$type[grep("indian", food$categories, ignore.case=T)] <- "indian"
food$type[grep("mediterranean", food$categories, ignore.case=T)] <- "mediterranean"
food$type[grep("mexican", food$categories, ignore.case=T)] <- "mexican"

#### Output files ########

food <- food %>% 
  mutate(location = paste(as.character(latitude), as.character(longitude),
                                         sep = ":"),
        tip = name %>% paste(as.character(stars), sep = " (") %>%
            paste("stars)", sep = " ") %>% paste(categories, sep = " "))

idx <- grep("lunch': True", food$attributes_good.for)
lunch <- food[idx,]
bfast <- food[grep("breakfast': True", food$attributes_good.for),]
dinner <- food[grep("dinner': True", food$attributes_good.for),]

write.csv(food, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/food.csv", row.names=F)
# variables_food <- names(food)
# write.csv(variables_food, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/variables_food.csv", row.names=F)

lunch <- lunch %>% select(-9)
bfast <- bfast %>% select(-9)
dinner <- dinner %>% select(-9)

write.csv(lunch, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/lunch.csv", row.names=F)
write.csv(bfast, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/bfast.csv", row.names=F)
write.csv(dinner, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/dinner.csv", row.names=F)

## Tips ##
business <- read.csv("Classes and Work/yelp/business.csv")
tips <- read.csv("Classes and Work/yelp/tip.csv")

idx <- grep("restaurant", business$categories, ignore.case=T)
business <- business[idx,]
business <- business %>% select(c(39, 56))

head(tips)
tips <- tips %>% select(c(1,4))
tips$text <- as.character(tips$text)

write.csv(tips, "/Users/marianwaitwalsh/Github/STAT-579-Final-Project/Data/Restaurants/tips.csv", row.names=F)

