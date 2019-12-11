library(tidyverse)
library(lattice)
library(caret)


# import training data ====

training_orig <- read.csv("C:/Users/Splendora/OneDrive/Ubiqum/Module_3_Task_3_EvaluateWiFi/trainingData.csv", header = TRUE)
# validation_orig <- read.csv("C:/Users/Splendora/OneDrive/Ubiqum/Module_3_Task_3_EvaluateWiFi/validationData.csv", header = TRUE)
# 
# ggplot(validation_orig, aes(x = LONGITUDE, y = LATITUDE)) +
#   geom_point()

# Rename attribute columns
colnames(training_orig)[521] <- "Longitude"
colnames(training_orig)[522] <- "Latitude"
colnames(training_orig)[523] <- "Floor"
colnames(training_orig)[524] <- "Building_ID"
colnames(training_orig)[525] <- "Space_ID"
colnames(training_orig)[526] <- "Relative_Position"
colnames(training_orig)[527] <- "User_ID"
colnames(training_orig)[528] <- "Phone_ID"
colnames(training_orig)[529] <- "Timestamp"

# Visualize non-RSSI attributes
nonRSSI_orig <- training_orig %>%
  select(Longitude, Latitude, Floor, Building_ID, Space_ID, Relative_Position, User_ID, Phone_ID, Timestamp)

str(nonRSSI_orig)
summary(nonRSSI_orig)

# # Find the min/max per floor to categorize longitude range of each floor
# nonRSSI_filterFloor <- nonRSSI_orig %>%
#   filter(Floor == 4)
# summary(nonRSSI_filterFloor)


# Remove columns then rows with WAP 100
training_WAPno100 <- Filter(var, training_orig)
training_WAProw_no100 <- apply(training_WAPno100[,1:465], 1, function(x) length(unique(x[!is.na(x)])) != 1)
training_no100s  <- training_WAPno100[training_WAProw_no100, ]




##=============================================================================================
# summary(training_orig$WAP002)

# # check attributes
# attributes(training_orig)
# 
# # summarize data
# summary(training_orig$Latitude)
# 
# # check data type
# str(training_orig)
# 
# # check for missing values
# sum(is.na(training_orig))
# 

ggplot(training_orig, aes(x = Longitude, y = Latitude)) +
  geom_point() +
  geom_jitter()
