# Initial data cleaning

business <- read.csv("Classes and Work/yelp/business.csv")
#library(dplyr)
#library(ggplot2)

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

# Need to work on Payments, Parking, Music, and Dietary

idx <- grep("lunch': True", food$attributes_Good.For)
lunch <- food[idx,]
bfast <- food[grep("breakfast': True", food$attributes_Good.For),]
dinner <- food[grep("dinner': True", food$attributes_Good.For),]