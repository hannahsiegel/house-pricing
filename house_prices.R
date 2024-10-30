
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

## read in the data
house_prices<- read.csv("House Price Prediction Dataset.csv")

##### initial analysis of the data ###

# dimensions of the data
ncol(house_prices)
nrow(house_prices) 

# column names and types
(house_prices)

# dataset includes houses that were built from 1900 to 2023
min(house_prices$YearBuilt)
max(house_prices$YearBuilt)

# preprocessing of data. check if any NA or blank prices
check<- house_prices %>% filter(is.na(Price) | Price=="" | Price==" ")
nrow(check)

# preprocessing of data: convert categorical variables (ie. location, condition, garage)
house_prices<- house_prices %>% mutate(location_encoded = case_when(Location=="Downtown" ~ 0,
                                                                    Location=="Suburban" ~ 1,
                                                                    Location=="Urban" ~ 2,
                                                                    Location=="Rural" ~ 3),
                                       condition_encoded = case_when(Condition=="Excellent" ~ 0,
                                                                     Condition=="Good" ~ 1,
                                                                     Condition=="Fair" ~ 2,
                                                                     Condition=="Poor" ~ 3),
                                       garage_encoded = case_when(Garage=="No" ~ 0,
                                                                  Garage == "Yes" ~ 1))



#locations included
unique(house_prices$Location)

#conditions included
unique(house_prices$Condition)

#bedrooms included
sort(unique(house_prices$Bedrooms))

#bedrooms included
sort(unique(house_prices$Bathrooms))

# let's start by looking at the visual relationship between area (ie. square footage) and price
ggplot(house_prices, aes(x = Area, y = Price)) + 
  geom_point()

# let's take it one step further and see the relationship between area and price within each location.
# unclear from this the relationship
ggplot(house_prices, aes(x = Area, y = Price)) + 
  geom_point()+
  facet_wrap(~Location)

# let's see what the average price is per location
per_location<- house_prices %>% group_by(Location) %>% summarise(avg_price = mean(Price)) %>% 
  arrange(desc(avg_price))

# let's see what the average price is per # of bedrooms
per_bedroom<- house_prices %>% group_by(Bedrooms) %>% summarise(avg_price = mean(Price)) %>% 
  arrange(desc(avg_price))

# let's see what the average price is per # of bathrooms
per_bathroom<- house_prices %>% group_by(Bathrooms) %>% summarise(avg_price = mean(Price)) %>% 
  arrange(desc(avg_price))

# let's see the average price of houses with a garage as opposed to without a garage
per_garage<- house_prices %>% group_by(Garage) %>% summarise(avg_price = mean(Price)) %>% 
  arrange(desc(avg_price))

# keep and drop relevant columns for our analysis
house_prices<- house_prices %>% select(-c(Location, Condition, Garage, Id))

# split the data into test and training set. we will use 40% of the data as our test set
set.seed(100)
test_index <- createDataPartition(y = house_prices$Price, times = 1, p = 0.4, list = FALSE)
train <- house_prices[-test_index,]
test <- house_prices[test_index,]

# in all models, I will train the set and the RMSE shown in the table is tested on the test set. 

## model 1: baseline model, average price
mu<- mean(train$Price)

model1_rmse <- RMSE(test$Price, mu)

rmse_table <- data.frame(
  Model = c("Model 1"),
  RMSE = round(model1_rmse, 5)
)
rmse_table




## model 2: location effect
location_effect <- train %>%
  group_by(location_encoded) %>%
  summarize(location_effect_val = mean(Price - mu) ) 

test<- left_join(test, location_effect, by = "location_encoded")

model2_rmse<- RMSE(test$Price, mu + test$location_effect_val)

rmse_table<- rmse_table %>% rbind(c("Model 2", round(model2_rmse,5)))
rmse_table



## model 3: regularized location effect
location_effect_reg<- function(alpha){
  location_effect <- train %>%
    group_by(location_encoded) %>%
    summarize(location_effect_val_reg = sum(Price - mu)/(n() + alpha))
  
  temp<- left_join(test, location_effect, by = "location_encoded")
  
  model3_rmse<- RMSE(temp$Price, mu + temp$location_effect_val_reg)
  return(model3_rmse)
}

alphas<- seq(0, 50, by = 1)
reg_results<- sapply(alphas, location_effect_reg)
best_alpha<- alphas[which.min(reg_results)]
model3_rmse<- reg_results[which.min(reg_results)] 

rmse_table<- rmse_table %>% rbind(c("Model 3", round(model3_rmse,5)))
rmse_table

#regularization does not make much of a difference.


## model 4: location and bedroom effect
bed_effect <- train %>% left_join(location_effect, by = "location_encoded") %>% 
group_by(Bedrooms) %>%
  summarize(bed_effect_val = mean(Price - mu - location_effect_val)) 

test<- left_join(test, bed_effect, by = "Bedrooms")

model4_rmse<- RMSE(test$Price, mu +test$location_effect_val+ test$bed_effect_val)

rmse_table<- rmse_table %>% rbind(c("Model 4", round(model4_rmse,5)))
rmse_table



## Let's switch it up and try some linear regression models with the lm function

## model 6: linear regression with only non-categorical variables
fit<- lm(Price ~ Area + Bedrooms + Bathrooms + Floors + YearBuilt, data = train)
fit_summary<- summary(fit)

# predict on test data
p_hat <- predict(fit, newdata = house_prices$test)
model6_rmse<- RMSE(p_hat, test$Price)
fit_summary$r.squared

rmse_table<- rmse_table %>% rbind(c("Model 6", round(model6_rmse,5)))
rmse_table



### model 7: linear regression with all variables
fit<- lm(Price ~., data = train)
fit_summary<- summary(fit)

# predict on test data
y_hat <- predict(fit, newdata = house_prices$test)
model7_rmse<- RMSE(y_hat, test$Price)
fit_summary$r.squared

rmse_table<- rmse_table %>% rbind(c("Model 7", round(model7_rmse,5)))
rmse_table


#select smallest RMSE
rmse_table_winner<- rmse_table %>% arrange(RMSE) %>% head(1)



