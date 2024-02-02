library(tidyverse)
library(RColorBrewer)
data_frame<-read.csv("C:\\Users\\chara\\Desktop\\stat 515\\final_project\\global_power_plant_database.csv")

df<-subset(data_frame, select = -c(owner,url,other_fuel1,other_fuel2,other_fuel3
                                   ,gppd_idnr,geolocation_source,wepp_id,
                                   generation_data_source))
str(df)
head(df,5)
summary(df)

#RE1
library(dplyr)
library(ggplot2)

# Group the data by country and calculate the total capacity of power plants in each country
capacity_by_country <- df %>%
  group_by(country_long) %>%
  summarize(total_capacity = sum(capacity_mw)) %>%
  arrange(desc(total_capacity))
top_10_countries <- head(capacity_by_country, n = 10)
top_10_countries
#code for top_10_countries total capacity

ggplot(top_10_countries, aes(x = country_long, y = total_capacity, fill = country_long)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_capacity), vjust = -0.5, size = 2) +
  ggtitle("Top 10 Countries by Total Capacity of Power Plants") +
  xlab("Country") +
  ylab("Total Capacity (MW)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Plot the top 10 countries with the highest total capacity and the distribution of fuel types across those countries

colors <- c("#FF0000","#00FF00", "#0000FF","#FFA500", "#FFFF00",
                     "#800080","#FFC0CB","#A52A2A","#808080", "#000000",
                     "#ADD8E6","#006400", "#FFD700","#E6E6FA", "#8B0000")
ggplot(df %>% filter(country_long %in% top_10_countries$country_long),
                      aes(x = country_long, y = capacity_mw, fill = primary_fuel)) +
                       geom_bar(stat = "identity") +
                       scale_fill_manual(values = colors) +
                       ggtitle("Total Capacity of Power Plants by Country") +
                       theme(plot.title = element_text(hjust = 0.5))

                     
                     
                     
                     
                     
#prediction model 2&3
df$primary_fuel <- as.factor(df$primary_fuel)

df$primary_fuel
          
df <- na.omit(df)


cols_to_check <- c("latitude","longitude","estimated_generation_gwh_2013","estimated_generation_gwh_2014","estimated_generation_gwh_2015","estimated_generation_gwh_2016","estimated_generation_gwh_2017")

# Calculating the first and third quartiles for each column
q1 <- apply(df[, cols_to_check], 2, quantile, probs = 0.25)
q3 <- apply(df[, cols_to_check], 2, quantile, probs = 0.75)

# Calculate the IQR for each column
iqr <- q3 - q1

# Define the threshold for outlier detection
thresh <- 1.5 * iqr

# Subset data to remove outliers
df <- df %>% 
  filter(abs(latitude - median(latitude)) <= thresh[1], 
         abs(longitude - median(longitude)) <= thresh[2], 
         abs(estimated_generation_gwh_2013 - median(estimated_generation_gwh_2013)) <= thresh[3], 
         abs(estimated_generation_gwh_2014 - median(estimated_generation_gwh_2014)) <= thresh[4], 
         abs(estimated_generation_gwh_2015 - median(estimated_generation_gwh_2015)) <= thresh[5], 
         abs(estimated_generation_gwh_2016 - median(estimated_generation_gwh_2016)) <= thresh[6], 
         abs(estimated_generation_gwh_2017 - median(estimated_generation_gwh_2017)) <= thresh[7])



library(lmtest)
library(caret)
library(caTools)
library(gridExtra)

set.seed(123)
sample <- sample.split(df$capacity_mw, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)

# Full model with all the predictors

fullmodel <- lm(capacity_mw ~ latitude+longitude+primary_fuel+estimated_generation_gwh_2013+ 
                  estimated_generation_gwh_2014+estimated_generation_gwh_2015+estimated_generation_gwh_2016+
                  estimated_generation_gwh_2017, data = train)
summary(fullmodel)


par(mfrow=c(2,2))

# Create the four plots
plot1 <- plot(fullmodel, which = 1)
plot2 <- plot(fullmodel, which = 2)
plot3 <- plot(fullmodel, which = 3)
plot4 <- plot(fullmodel, which = 4)

#AIC model
AIC = 2*(1:10) + n*log(rs$rss/n) 
plot(AIC ~ I(1:10), xlab = "number of predictors", ylab = "AIC")
AIC

#reduced model
redmodel <- lm(capacity_mw ~ latitude+longitude+primary_fuel+estimated_generation_gwh_2013+
                 estimated_generation_gwh_2015+estimated_generation_gwh_2017, data = train)
summary(redmodel)

par(mfrow=c(2,2))

plot1 <- plot(redmodel, which = 1)
plot2 <- plot(redmodel, which = 2)
plot3 <- plot(redmodel, which = 3)
plot4 <- plot(redmodel, which = 4)

#anova testing 

anova(redmodel, fullmodel)

#plot

library(ggplot2)

df2 <- data.frame(actual = test$capacity_mw, predicted = predict(redmodel, newdata = test))

dev.off()

ggplot(df2, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted", x = "Actual", y = "Predicted")









#capacity output

powerplant_data_1<-subset(df, select = c(capacity_mw, latitude, longitude, primary_fuel,
                                         estimated_generation_gwh_2013, estimated_generation_gwh_2014,
                                         estimated_generation_gwh_2015, estimated_generation_gwh_2016, 
                                         estimated_generation_gwh_2017))

# Split the data into training and testing sets
set.seed(123)
train <- sample(1:nrow(powerplant_data_1), nrow(powerplant_data_1)*0.8, replace = FALSE)
train_data <- powerplant_data_1[train,]
test_data <- powerplant_data_1[-train,]

# Fit the linear regression model
model <- lm(capacity_mw ~ latitude + longitude + primary_fuel +
              estimated_generation_gwh_2013+estimated_generation_gwh_2014
            +estimated_generation_gwh_2015+estimated_generation_gwh_2016
            +estimated_generation_gwh_2017, data = train_data)

# Use the model to make predictions on the testing set
predictions <- predict(model, newdata = test_data)

# Calculate the mean squared error (MSE) of the predictions
mse <- mean((test_data$capacity_mw - predictions)^2)

# Print the MSE
print(paste("MSE:", mse))
# predict power output for testing data set
predictions <- predict(model, newdata = test_data)

# calculate correlation between predicted values and actual values
accuracy <- cor(test_data$capacity_mw, predictions)

# print accuracy
print(paste("Accuracy:", accuracy))




#power output
powerplant_data<-subset(df, select = c(latitude, longitude, capacity_mw,
                                         estimated_generation_gwh_2013, estimated_generation_gwh_2014,
                                         estimated_generation_gwh_2015, estimated_generation_gwh_2016, 
                                       estimated_generation_gwh_2017))

# Split the data into training and testing sets
set.seed(123)
train <- sample(1:nrow(powerplant_data_1), nrow(powerplant_data_1)*0.8, replace = FALSE)
train_data <- powerplant_data_1[train,]
test_data <- powerplant_data_1[-train,]

# Fit the linear regression model
model <- lm(estimated_generation_gwh_2016 ~ latitude + longitude +
              estimated_generation_gwh_2017+estimated_generation_gwh_2013
            +estimated_generation_gwh_2014+estimated_generation_gwh_2015
          , data = train_data)

# Use the model to make predictions on the testing set
predictions <- predict(model, newdata = test_data)

# Calculate the mean squared error (MSE) of the predictions
mse <- mean((test_data$estimated_generation_gwh_2016 - predictions)^2)

# Print the MSE
print(paste("MSE:", mse))
# predict power output for testing data set
predictions <- predict(model, newdata = test_data)

# calculate correlation between predicted values and actual values
accuracy <- cor(test_data$estimated_generation_gwh_2016, predictions)

# print accuracy
print(paste("Accuracy:", accuracy))





