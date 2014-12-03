# open csv files
business <- read.csv("business.csv", header=T)
review <- read.csv("review.csv", header=T)

# create subsets
hotels <- business[grep("Hotels", business$categories), 
                     c("attributes_Price.Range", "attributes_Wi.Fi", 
                       "business_id", "city", "name", "review_count", "state")]
colnames(hotels) <- c("price.range", "wi.fi", "id", "city", "name", 
                      "review.count", "state")
levels(hotels$wi.fi) <- c(NA, "free", "no", "paid")

shopping <- business[grep("Shopping", business$categories), 
                     c("attributes_Parking", "attributes_Price.Range",  
                       "business_id", "city", "name", "review_count", "state")]
colnames(shopping) <- c("parking", "price.range", "id", "city", "name", 
                      "review.count", "state")

review <- review[, c("business_id", "stars")]
colnames(review) <- c("id", "rating")

# merge datasets
hotels <- merge(hotels, review, by="id", all=F)
shopping <- merge(shopping, review, by="id", all=F)

# write csv files
write.csv(hotels, "hotels.csv", row.names=F)
write.csv(shopping, "shopping.csv", row.names=F)