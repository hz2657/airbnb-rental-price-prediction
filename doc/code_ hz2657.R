# load the data

install.packages('dplyr')
library(dplyr)

# explotary data analysis
glimpse(analysisData)
str(analysisData)
sum(is.na(analysisData))
summary(is.na(analysisData))

# explore correlation
data_numeric = analysisData %>%
  select(price,
         beds,
         cleaning_fee,
         reviews_per_month,
         accommodates,
         bathrooms,
         bedrooms,
         guests_included,
         extra_people,
         minimum_nights,
         maximum_nights,
         availability_30,
         availability_365,
         number_of_reviews,
         number_of_reviews_ltm,
         review_scores_rating,
         review_scores_accuracy,
         review_scores_cleanliness,
         review_scores_checkin,
         review_scores_communication,
         review_scores_location,
         review_scores_value,
         calculated_host_listings_count,
         calculated_host_listings_count_entire_homes,
         calculated_host_listings_count_private_rooms,
         calculated_host_listings_count_shared_rooms)
describe(data_numeric)
# correlation plot
library(corrplot)
corrplot(cor(data_numeric[,-25]),method = 'square',type = 'lower',diag = F)

# calculate correlation
cor(data_numeric[,-25])



# clean and select data in cleandata1
cleandata1 = analysisData %>%
  select(price,
         host_is_superhost,
         neighbourhood_group_cleansed,
         cleaning_fee,
         reviews_per_month,
         is_location_exact,
         room_type,
         accommodates,
         bathrooms,
         bedrooms,
         minimum_nights,
         maximum_nights,
         availability_365,
         availability_30, 
         number_of_reviews, 
         property_type, 
         review_scores_communication, 
         review_scores_value,
         review_scores_rating,
         instant_bookable,
         cancellation_policy,
         calculated_host_listings_count,
         host_listings_count,
         host_has_profile_pic,
         host_identity_verified,
         guests_included,
         extra_people) 

# find the median of missing values
median(analysisData$host_listings_count,na.rm = T) #  median
summary(analysisData$host_has_profile_pic,na.rm = T)
summary(analysisData$host_identity_verified,na.rm = T)

# replace missing values by median
cleandata1[is.na(cleandata1$host_listings_count), ]$host_listings_count <- 1 #  median
cleandata1[is.na(cleandata1$host_has_profile_pic), ]$host_has_profile_pic <- TRUE
cleandata1[is.na(cleandata1$host_identity_verified), ]$host_identity_verified <- TRUE 
cleandata1$host_has_profile_pic <- as.factor(cleandata1$host_has_profile_pic)
cleandata1$host_identity_verified <- as.factor(cleandata1$host_identity_verified)


# add new variable (1): wordcount
install.packages('ngram')
library(ngram)
cleandata1$wordcount_description<- sapply(strsplit(analysisData$description, " "), length)
cleandata1$wordcount_transit<- sapply(strsplit(analysisData$transit, " "), length)
cleandata1$wordcount_interaction<- sapply(strsplit(analysisData$interaction, " "), length)
cleandata1$wordcount_access<- sapply(strsplit(analysisData$access, " "), length)
cleandata1$wordcount_host_about<- sapply(strsplit(analysisData$host_about, " "), length)
cleandata1$wordcount_host_verifications<- sapply(strsplit(analysisData$host_verifications, ","), length)

# add new variable (2): neighborhood
cleandata1$Bedford_Stuyvesant <- ifelse(grepl('Bedford-Stuyvesant', analysisData$neighbourhood_cleansed), "1","0")
cleandata1$Bushwick <- ifelse(grepl('Bushwick', analysisData$neighbourhood_cleansed), "1","0")
cleandata1$Chelsea <- ifelse(grepl('Chelsea', analysisData$neighbourhood_cleansed), "1","0")
cleandata1$Crown_Heights <- ifelse(grepl('Crown Heights', analysisData$neighbourhood_cleansed), "1","0")
cleandata1$East_Village <- ifelse(grepl('East Village', analysisData$neighbourhood_cleansed), "1","0")
cleandata1$Harlem <- ifelse(grepl('Harlem', analysisData$neighbourhood_cleansed), "1","0")
cleandata1$West_Village <- ifelse(grepl('West Village', analysisData$neighbourhood_cleansed), "1","0")

# add new variable (3): amenities
cleandata1$Airconditioning <- ifelse(grepl('Air conditioning', analysisData$amenities), "1","0")
cleandata1$Kitchen <- ifelse(grepl('Kitchen', analysisData$amenities), "1","0")
cleandata1$Essentials <- ifelse(grepl('Essentials', analysisData$amenities), "1","0")
cleandata1$Elevator <- ifelse(grepl('Elevator', analysisData$amenities), "1","0")
cleandata1$Doorman <- ifelse(grepl('Doorman', analysisData$amenities), "1","0")
cleandata1$Gym <- ifelse(grepl('Gym', analysisData$amenities), "1","0")
cleandata1$Internet <- ifelse(grepl('Internet', analysisData$amenities), "1","0")
cleandata1$Smoking <- ifelse(grepl('Smoking allowed', analysisData$amenities), "1","0")
cleandata1$parking <- ifelse(grepl('Free street parking', analysisData$amenities), "1","0")
cleandata1$Heating <- ifelse(grepl('Heating', analysisData$amenities), "1","0")

sum(is.na(cleandata1))


# continue to replace NA by median in cleandata1
cleandata1[is.na(cleandata1$cleaning_fee), ]$cleaning_fee <- 0 # 20  
cleandata1[is.na(cleandata1$host_is_superhost), ]$host_is_superhost <- FALSE # after help
cleandata1[is.na(cleandata1$reviews_per_month),]$reviews_per_month <- 0.96
median(analysisData$reviews_per_month, na.rm = T) #0.96

# convert data to factors (in order to run random forest)
cleandata1$neighbourhood_group_cleansed <- as.factor(cleandata1$neighbourhood_group_cleansed)
cleandata1$room_type <- as.factor(cleandata1$room_type)
cleandata1$cancellation_policy <- as.factor(cleandata1$cancellation_policy)
cleandata1$property_type <- as.factor(cleandata1$property_type)
cleandata1$host_is_superhost <- as.factor(cleandata1$host_is_superhost)
cleandata1$is_location_exact <- as.factor(cleandata1$is_location_exact)
cleandata1$instant_bookable <- as.factor(cleandata1$instant_bookable)
cleandata1$Airconditioning <- as.numeric(cleandata1$Airconditioning)
cleandata1$Kitchen <- as.numeric(cleandata1$Kitchen)
cleandata1$Essentials <- as.numeric(cleandata1$Essentials)
cleandata1$Elevator <- as.numeric(cleandata1$Elevator)
cleandata1$Doorman <- as.numeric(cleandata1$Doorman)
cleandata1$Gym <- as.numeric(cleandata1$Gym)
cleandata1$Internet <- as.numeric(cleandata1$Internet)
cleandata1$Smoking <- as.numeric(cleandata1$Smoking)
cleandata1$parking <- as.numeric(cleandata1$parking)
cleandata1$Heating <- as.numeric(cleandata1$Heating)

# explore clean dataset
str(cleandata1)
sum(is.na(cleandata1))
glimpse(cleandata1)

# run random forest model
install.packages('randomForest')
library(randomForest)
set.seed(66)
forestmodel = randomForest(price~., data = cleandata1,ntree = 300)
plot(forest)
varImplot(forest)
importantlist3 = importance(forestmodel)
varImpPlot(forest); importance(forest)  ## see variable importance



# clean data in test (scroing data)
cleandata1_test = scoringData %>%
  select(host_is_superhost,
         neighbourhood_group_cleansed,
         cleaning_fee,
         reviews_per_month,
         is_location_exact,
         room_type,
         accommodates,
         bathrooms,
         bedrooms,
         minimum_nights,
         maximum_nights,
         availability_365,
         availability_30, 
         number_of_reviews, 
         property_type, 
         review_scores_communication, 
         review_scores_value,
         review_scores_rating,
         instant_bookable,
         cancellation_policy,
         calculated_host_listings_count,
         host_listings_count,
         host_has_profile_pic,
         host_identity_verified,
         guests_included,
         extra_people) 

# replace missing values by median
cleandata1_test[is.na(cleandata1_test$host_listings_count), ]$host_listings_count <- 1 #  median
cleandata1_test[is.na(cleandata1_test$host_has_profile_pic), ]$host_has_profile_pic <- TRUE
cleandata1_test[is.na(cleandata1_test$host_identity_verified), ]$host_identity_verified <- TRUE 
cleandata1_test$host_has_profile_pic <- as.factor(cleandata1_test$host_has_profile_pic)
cleandata1_test$host_identity_verified <- as.factor(cleandata1_test$host_identity_verified)

median(cleandata1_test$host_listings_count,na.rm = T) #  median
summary(scoringData$host_has_profile_pic,na.rm = T)
summary(scoringData$host_identity_verified,na.rm = T)

# (1) add new variable: wordcount
library(ngram)
cleandata1_test$wordcount_description<- sapply(strsplit(scoringData$description, " "), length)
cleandata1_test$wordcount_transit<- sapply(strsplit(scoringData$transit, " "), length)
cleandata1_test$wordcount_interaction<- sapply(strsplit(scoringData$interaction, " "), length)
cleandata1_test$wordcount_access<- sapply(strsplit(scoringData$access, " "), length)
cleandata1_test$wordcount_host_about<- sapply(strsplit(scoringData$host_about, " "), length)
cleandata1_test$wordcount_host_verifications<- sapply(strsplit(scoringData$host_verifications, ","), length)

# (2) add new variable: neighborhood
cleandata1_test$Bedford_Stuyvesant <- ifelse(grepl('Bedford-Stuyvesant', scoringData$neighbourhood_cleansed), "1","0")
cleandata1_test$Bushwick <- ifelse(grepl('Bushwick', scoringData$neighbourhood_cleansed), "1","0")
cleandata1_test$Chelsea <- ifelse(grepl('Chelsea', scoringData$neighbourhood_cleansed), "1","0")
cleandata1_test$Crown_Heights <- ifelse(grepl('Crown Heights', scoringData$neighbourhood_cleansed), "1","0")
cleandata1_test$East_Village <- ifelse(grepl('East Village', scoringData$neighbourhood_cleansed), "1","0")
cleandata1_test$Harlem <- ifelse(grepl('Harlem', scoringData$neighbourhood_cleansed), "1","0")
cleandata1_test$West_Village <- ifelse(grepl('West Village', scoringData$neighbourhood_cleansed), "1","0")

# (3) add new variable: amenities
cleandata1_test$Airconditioning <- ifelse(grepl('Air conditioning', scoringData$amenities), "1","0")
cleandata1_test$Kitchen <- ifelse(grepl('Kitchen', scoringData$amenities), "1","0")
cleandata1_test$Essentials <- ifelse(grepl('Essentials', scoringData$amenities), "1","0")
cleandata1_test$Elevator <- ifelse(grepl('Elevator', scoringData$amenities), "1","0")
cleandata1_test$Doorman <- ifelse(grepl('Doorman', scoringData$amenities), "1","0")
cleandata1_test$Gym <- ifelse(grepl('Gym', scoringData$amenities), "1","0")
cleandata1_test$Internet <- ifelse(grepl('Internet', scoringData$amenities), "1","0")
cleandata1_test$Smoking <- ifelse(grepl('Smoking allowed', scoringData$amenities), "1","0")
cleandata1_test$parking <- ifelse(grepl('Free street parking', scoringData$amenities), "1","0")
cleandata1_test$Heating <- ifelse(grepl('Heating', scoringData$amenities), "1","0")

cleandata1_test$Airconditioning <- as.numeric(cleandata1_test$Airconditioning)
cleandata1_test$Kitchen <- as.numeric(cleandata1_test$Kitchen)
cleandata1_test$Essentials <- as.numeric(cleandata1_test$Essentials)
cleandata1_test$Elevator <- as.numeric(cleandata1_test$Elevator)
cleandata1_test$Doorman <- as.numeric(cleandata1_test$Doorman)
cleandata1_test$Gym <- as.numeric(cleandata1_test$Gym)
cleandata1_test$Internet <- as.numeric(cleandata1_test$Internet)
cleandata1_test$Smoking <- as.numeric(cleandata1_test$Smoking)
cleandata1_test$parking <- as.numeric(cleandata1_test$parking)
cleandata1_test$Heating <- as.numeric(cleandata1_test$Heating)
sum(is.na(cleandata1_test))

# (4) replace NA value
cleandata1_test[is.na(cleandata1_test$cleaning_fee), ]$cleaning_fee <- 0 # after help
cleandata1_test[is.na(cleandata1_test$host_is_superhost), ]$host_is_superhost <- FALSE # after help
cleandata1_test[is.na(cleandata1_test$reviews_per_month),]$reviews_per_month <- 0.95
median(cleandata1_test$reviews_per_month, na.rm = T) #0.95

# (5) convert to factors
cleandata1_test$neighbourhood_group_cleansed <- as.factor(cleandata1_test$neighbourhood_group_cleansed)
cleandata1_test$room_type <- as.factor(cleandata1_test$room_type)
cleandata1_test$cancellation_policy <- as.factor(cleandata1_test$cancellation_policy)
cleandata1_test$bed_type <- as.factor(cleandata1_test$bed_type)
cleandata1_test$property_type <- as.factor(cleandata1_test$property_type)

# (6) Deal with new levels in Property type
# replace with levels that are similar
cleandata1_test[cleandata1_test$property_type == 'Castle',]$property_type <- 'Resort'
cleandata1_test[cleandata1_test$property_type == 'Farm stay',]$property_type <- 'House'
cleandata1_test[cleandata1_test$property_type == 'Casa particular (Cuba)',]$property_type <- 'House'
dada = cleandata1_test[cleandata1_test$property_type == 'Casa particular (Cuba)',]
levels(cleandata1$property_type)
levels(cleandata1_test$property_type)
str(cleandata1)
str(cleandata1_test)
?droplevels.factor
library(gdata)
drop.levels(cleandata1_test$property_type)

x <- x[x!="Casa particular (Cuba)"]
df <- data.frame(a=x,b=x,c=x)
df <- drop.levels(df) 
factor
cleandata1_test$property_type <- droplevels(cleandata1_test$property_type )
cleandata3_test = select(cleandata1_test , -c('minutes','train','subway'))


# convert test data to types of train data
cleandatatestaaaa = cleandata1_test
cleandatatestaaaa <- rbind(cleandata1[1, -1], cleandatatestaaaa)
cleandatatestaaaa <- cleandatatestaaaa[-1,]
str(cleandata3_test)
str(cleandata1)
sum(is.na(cleandata1_test))

# predict testing data
install.packages('caret')
install.packages('ggplot2')
install.packages('kenlab')
library(randomForest)
library(caret)

library(kenlab)
library(ggplot2)
library(caret)
predforest2 = predict(forestmodel,newdata=cleandatatestaaaa)
predforest2
# score = 58.77108

# write file in csv
submission14 = data.frame(id=scoringData$id, price = predforest2)
write.csv(submission14,'14submission.csv',row.names = F)

