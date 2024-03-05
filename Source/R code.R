install.packages('dplyr')
car_data <- read.csv(file.choose())
car_data

#a
colSums(is.na(car_data))
car_data <- data.frame(car_data)

km_driven_median <- median(car_data$Kilometers_Driven, na.rm = TRUE)
km_driven_median
car_data$Kilometers_Driven <- ifelse(is.na(car_data$Kilometers_Driven), km_driven_median, car_data$Kilometers_Driven)

car_data$Seats
seats_median <- median(car_data$Seats, na.rm = TRUE)
car_data$Seats <- ifelse(is.na(car_data$Seats), seats_median, car_data$Seats)
car_data

#b                      
car_data$Mileage <- as.character(car_data$Mileage)
car_data$Power <- as.character(car_data$Power)
car_data$Engine <- as.character(car_data$Engine)
car_data$New_Price <- as.character(car_data$New_Price)
car_data$Mileage
car_data$Mileage <- sapply(strsplit(car_data$Mileage, " "), function(x) x[1])
car_data$Power <- sapply(strsplit(car_data$Power, " "), function(x) x[1])
car_data$Engine <- sapply(strsplit(car_data$Engine, " "), function(x) x[1])
car_data$New_Price <- sapply(strsplit(car_data$New_Price, " "), function(x) x[1])
car_data
#c
install.packages(caret)
library(caret)

# Sample dataframe
df_fuel_type <- data.frame(car_data$Fuel_Type)
df_transmission <- data.frame(car_data$Transmission)

# Create dummy variables
dummies_fuel_type <- dummyVars("~ .", data = df_fuel_type)
dummies_transmission <- dummyVars("~ .", data = df_transmission)

# Apply to dataframe
df_encoded_fuel_type <- predict(dummies_fuel_type, newdata = df_fuel_type)
df_encoded_transmission <- predict(dummies_transmission, newdata = df_transmission)

# Convert to a dataframe
df_final_fuel_type <- as.data.frame(df_encoded_fuel_type)
df_final_transmission <- as.data.frame(df_encoded_transmission)

# Combine with original data
df_final <- cbind(df_fuel_type, df_final_fuel_type, df_transmission, df_final_transmission)

# Print the final dataframe
print(df_final)

car_data_latest <- cbind(car_data, df_final)
car_data_latest

head(car_data_latest)
car_data
#d
library(dplyr)
car_data <- mutate(car_data, Current_Age = as.numeric(format(Sys.Date(), "%Y")) - Year)
car_data

#select
selected_df <- select(car_data, Model_Name, Year, Selling_Price, Location, Owner_Type)
#filter
filtered_df <- filter(car_data, Year == 2018, Seats == 5)
#rename
renamed_df <- rename(car_data, Updated_Price = New_Price)
#mutate has already been used
car_data <- mutate(car_data, Current_Age = as.numeric(format(Sys.Date(), "%Y")) - Year)
car_data

#arrange
arranged_data <- arrange(car_data_latest, Year)
#summary
summary_data <- car_data_latest %>%
  group_by(Transmission) %>%
  summarize(Avg_Price = mean(Selling_Price), Max_Year = max(Year))

