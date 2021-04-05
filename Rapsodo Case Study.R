library(tidyverse)
library(convertr)
library(weathermetrics)
library(caret)


# Import Case Study Data
hit_class <- read.csv("caseStudyData.csv")

# Review structure of the data frame and check for NA values
str(hit_class) # From looking at the structure of the code, its either in integers or numbers, the data is good to perform statistical analysis on

round(prop.table(table(hit_class$hitClass)) * 100, digits = 1) # majority of the dataset is in hit class 2 - ground balls

summary(hit_class) # After looking at the summary, the hit class is binary meaning the hit class is an outcome and will be our dependent variable in this case study

sum(is.na(hit_class)) # After checking for NA values, we see there are not NA values in the data frame
# A common characteristic of messy data is having NA values or rows with zeros, the next step is to clean the data

# Clean up the data for further analysis, it is hard to conduct analysis with messy data
hit_class <- hit_class[apply(hit_class!=0, 1, all), ]
# The reason to remove zeros, with rows without any data is the assumption that the rapsodo unit did not read or there was an error in receiving and processing the data, in other words the unit was not working, making the data not usable for analysis 


# Review data for metric data and convert to imperial: exitSpeed seems to be in metric
#A great way to check for metric vs imperial numbers is to check values you normally see big numbers of like distance and exit velocity, normally in the US we see them in ft and mph
hit_class %>%
  summarize(Max_EV = max(exitSpeed, na.rm = T),
            Max_Dist = max(distance, na.rm = T)) # After reviewing the data we see exit speed's max is 61, looking at the rapsodo glossary we see its measured in meters per second which is a metric unit, 
# Distance is a little harder to tell, but we can assume with over 4 thousand data points and looking at the glossary those are our metric units we need to convert


# Convert Exit Speed and Distance into imperial numbers of MPH and Ft, using the convertr package we can use the convert function to convert exit speed M/S to MPH and distance meters to feet
hit_class <- hit_class %>%
  mutate(distance = convert(distance, "m", "ft"),
         exitSpeed = convert_wind_speed(hit_class$exitSpeed, old_metric = "mps", new_metric = "mph"),
         hitClass = as.factor(hitClass)) %>%
  rename(Hit_Class = hitClass, Exit_Velo = exitSpeed, Launch_Angle = launchAngle, Distance = distance, Direction = direction,
         Playing_Level = playingLevel)


# Average variables grouped by hit class to find out the average variables by hit class
hit_class_avg <- hit_class%>%
  group_by(Hit_Class) %>%
  summarize('Avg_EV' = mean(Exit_Velo, na.rm = T),
            'Avg_LA' = mean(Launch_Angle, na.rm = T),
            'Avg_Distance' = mean(Distance, na.rm = T),
            'Avg_Direction' = mean(Direction, na.rm = T))

# Playing Level Averages
playing_level <- hit_class %>%
  group_by(Playing_Level) %>%
  summarize('Avg_EV' = mean(Exit_Velo, na.rm = T),
            'Avg_LA' = mean(Launch_Angle, na.rm = T),
            'Avg_Distance' = mean(Distance, na.rm = T),
            'Avg_Direction' = mean(Direction, na.rm = T)) # Youth is absent from the data as there may not be enough data to gain averages from

# Visualize Batted Ball Events
ggplot(hit_class, aes(x=Exit_Velo, y=Launch_Angle, color = Hit_Class)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_classic()

ggplot(hit_class, aes(x=Distance, y=Exit_Velo, color = Hit_Class)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_classic()

# Bucket the data to see any data carryover with distance 
hit_class$dist_bucket <- cut(hit_class$Distance, seq(from = 0, to = 400, by = 45), include.lowest = TRUE)
hit_class_bucket <- hit_class %>%
  group_by(dist_bucket, Hit_Class) %>%
  summarize('Avg_EV' = mean(Exit_Velo, na.rm = T),
            'Avg_LA' = mean(Launch_Angle, na.rm = T)) %>%
  mutate(Avg_EV = round(Avg_EV, 1),
         Avg_LA = round(Avg_LA, 1))

  # Linear Regression
hit_class_lm <- lm(Hit_Class ~ Exit_Velo + Launch_Angle + Distance, data = hit_class)
summary(hit_class_lm)


# Split the data up for K nearest Neighbors Model for classification
# How many rows for split for test and train
set.seed(123)
hit_index <- createDataPartition(hit_class$Hit_Class, p=0.8, list=FALSE)
hit_class_training <- hit_class[hit_index, ]
hit_class_test <- hit_class[-hit_index, ]

# Train the model 
hit_knn <- train(hit_class_training[,c(2,3,5)], hit_class_training[,1],
                 method = "knn", preProcess=c("center", "scale"))
# Predict values
predictions_hit1 <-predict.train(object=hit_knn,hit_class_test[,c(2,3,5)], type="raw")

# Confusion matrix
confusionMatrix(predictions_hit1 ,hit_class_test[,1])

plot(hit_knn) # Visualize the accuracy over the set of neighbors 

# Model 2 with variables just exit speed and distance
hit_knn2 <- train(hit_class_training[,c(2,5)], hit_class_training[,1],
                 method = "knn", preProcess=c("center", "scale"))

predictions_hit2 <-predict.train(object=hit_knn2,hit_class_test[,c(2,5)], type="raw")

confusionMatrix(predictions_hit2 ,hit_class_test[,1])

plot(hit_knn2) # Visualize the accuracy over the set of neighbors 
