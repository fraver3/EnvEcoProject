NO <- read.csv(file.choose())

str(NO)

table(NO$city)

colSums(is.na(NO)) # Nitric.oxide is the response variable. Probably remove 
                   # the 531 observations


