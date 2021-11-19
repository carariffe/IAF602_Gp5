library(dplyr)
library(ggplot2)
library(tidyr)
library(mosaic)
library(modeldata)
library(rsample)

#Data Cleaning
#import original data set
dek_v1 = read.csv("car details v3.csv")

#remove variables that we are not evaluating
dek_v2 = select(dek_v1, 
                -name,
                -year,
                -owner,
                -engine,
                -max_power,
                -torque)

#remove observations with missing data
dek_v3 = na.omit(dek_v2) #219 observations omitted

#split "mileage" variable into "fuel_econ" and "FE_unit"
dek_v4 = separate(dek_v3, mileage, into=c("fuel_econ","FE_unit"),
                  sep = " ")

#remove observations that use km/kg as fuel economy unit, as this is not
#a standard unit and it is unclear whether the author of the data set is
#using km/kg as an equivalent to kmpl or whether a conversion is needed

dek_v5 <- dek_v4[!(dek_v4$FE_unit=="km/kg"),] #88 observations omitted

#change data type of fuel_econ to numeric

dek_v5$fuel_econ = as.numeric(dek_v5$fuel_econ)

#remove observations with fuel economy reported as 0 kmpl (equivalent to N/A)

dek_v6 <-dek_v5[!(dek_v5$fuel_econ==0),]

#remove fuel economy unit variable
dek_v7 = select(dek_v6, -FE_unit)

#convert selling price currency from INR to USD (conversion rate from 17Nov21)
dek_v8 <- dek_v7 %>%
  mutate(selling_price = selling_price * .013) %>%
  mutate(selling_price = round(selling_price, digits = 0))

#split into train and test data sets 
set.seed(101)
dek_split <- initial_split(dek_v8)
dek_train <- training(dek_split)
dek_test <- testing(dek_split)

#selling_price
ggplot(dek_train, aes(x=selling_price)) +
  geom_histogram(bins=200) +
  labs(title = "Histogram of Selling Price",
       x = "Selling Price (USD)")

ggplot(dek_train, aes(y=selling_price)) +
  geom_boxplot() +
  labs(title = "Box Plot of Selling Price",
       y = "Selling Price (USD)")

favstats(dek_train$selling_price)

#log_price

dek_train <- dek_train %>%
  mutate(log_price = log(selling_price))

ggplot(dek_train, aes(x=log_price)) +
  geom_histogram(bins=200) +
  labs(title = "Histogram of log(Selling Price)",
       x = "log(USD)")

#km_driven
ggplot(dek_train, aes(x=km_driven, y=selling_price)) +
  geom_point(size =1) +
  labs(title = "Selling Price vs. Kilometers Driven",
       x = "Kilometers",
       y = "Price (USD)") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(dek_train, aes(x=km_driven, y=selling_price)) +
  geom_point(size = 1, aes(color=fuel)) +
  labs(title = "Selling Price vs. Kilometers Driven",
       x = "Kilometers",
       y = "Price (USD)",
       color = "Fuel Type") +
  facet_grid(rows = vars(fuel))

ggplot(dek_train, aes(x=km_driven, y=selling_price)) +
  geom_point(size = 1, aes(color=seller_type)) +
  labs(title = "Selling Price vs. Kilometers Driven",
       x = "Kilometers",
       y = "Price (USD)",
       color = "Seller Type") +
  facet_grid(rows = vars(seller_type))

ggplot(dek_train, aes(x=km_driven, y=selling_price)) +
  geom_point(size = 1, aes(color=transmission)) +
  labs(title = "Selling Price vs. Kilometers Driven",
       x = "Kilometers",
       y = "Price (USD)",
       color = "Transmission") +
  facet_grid(rows = vars(transmission))

#log_price vs km_driven

ggplot(dek_train, aes(x=km_driven, y=log_price)) +
  geom_point(size =1) +
  labs(title = "log(Selling Price) vs. Kilometers Driven",
       x = "Kilometers",
       y = "log(USD)") +
  geom_smooth(method = "lm", se = FALSE)

#log_km

dek_train <- dek_train %>%
  mutate(log_km = log(km_driven))

ggplot(dek_train, aes(x=log_km, y=log_price)) +
  geom_point(size =1) +
  labs(title = "log(Selling Price) vs. log(Kilometers Driven)",
       x = "log(km)",
       y = "log(USD)") +
  geom_smooth(method = "lm", se = FALSE)

#fuel
ggplot(dek_train, aes(x=fuel)) +
  geom_bar(aes(fill=as.factor(fuel))) +
  labs(title =  "Fuel Type",
       x = "")

#seller_type
ggplot(dek_train, aes(x=seller_type)) +
  geom_bar(aes(fill = as.factor(seller_type))) +
  labs(title = "Seller Type", 
       x = " ")

#transmission
ggplot(dek_train, aes(x=transmission)) +
  geom_bar(aes(fill = as.factor(transmission))) +
  labs(title = "Transmission Type", 
       x = " ")

#fuel_econ
ggplot(dek_train, aes(x=fuel_econ, y=selling_price)) +
  geom_point(size = 1) +
  labs(title = "Selling Price vs. Fuel Economy",
       x = "Fuel Economy (kmpl)",
       y = "Price (USD)") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(dek_train, aes(x=fuel_econ, y=log_price)) +
  geom_point(size = 1) +
  labs(title = "log(Selling Price) vs. Fuel Economy",
       x = "Fuel Economy (kmpl)",
       y = "log(USD)") +
  geom_smooth(method = "lm", se = FALSE)

#log_econ
dek_train <- dek_train %>%
  mutate(log_econ = log(fuel_econ))

ggplot(dek_train, aes(x=log_econ, y=log_price)) +
  geom_point(size = 1) +
  labs(title = "log(Selling Price) vs. log(Fuel Economy)",
       x = "log(kmpl)",
       y = "log(USD)") +
  geom_smooth(method = "lm", se = FALSE)

#seats
ggplot(dek_train, aes(x=seats, y=selling_price)) +
  geom_point(size = 1) +
  labs(title = "Selling Price vs. Number of Seats",
       x = "# Seats",
       y = "Price (USD)") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(dek_train, aes(x=seats, y=log_price)) +
  geom_point(size = 1) +
  labs(title = "log(Selling Price) vs. Number of Seats",
       x = "# Seats",
       y = "log(USD)") +
  geom_smooth(method = "lm", se = FALSE)

#Fit 1
fit1 = lm(log_price ~ km_driven + fuel_econ + seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data = dek_train)
summary(fit1)

#Fit 2
fit2 = lm(log_price ~ log_km + fuel_econ + seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data = dek_train)
summary(fit2)

#Fit 3
#Transform all Trustmark Dealer observations to Dealer
dek_train1 <- dek_train %>%
  mutate(seller_type = if_else(seller_type == "Trustmark Dealer",
                               "Dealer", seller_type))

dek_train %>%
  filter(seller_type == "Dealer") %>%
  summarize(num_ob = n()) #n = 817, Dealer

dek_train %>%
  filter(seller_type == "Trustmark Dealer") %>%
  summarize(num_ob = n()) #n = 172, Trustmark Dealer

dek_train1 %>%
  filter(seller_type == "Dealer") %>%
  summarize(num_ob = n()) #n = 989, Dealer (817 + 172)

#Run Fit 3 (same as Fit 2, but with revised data)
fit3 = lm(log_price ~ log_km + fuel_econ + seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data = dek_train1)
summary(fit3)

#Scatterplot Matrices
pairs(~selling_price + km_driven + fuel_econ + seats,
      dek_train1,
      main = "Simple Scatterplot Matrix")

pairs(~log_price + km_driven + fuel_econ + seats,
      dek_train1,
      main = "Scatterplot Matrix with log(Price)")

pairs(~log_price + log_km +log_econ + seats,
      dek_train1,
      main = "Scatterplot Matrix with log(Price), log(km), log(kmpl)")

#Fit 4
fit4 = lm(log_price ~ log_km + log_econ + seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data = dek_train1)
summary(fit4)

#Fit 5
fit5 = lm(log_price ~ log_km + fuel_econ + seats +
            log_km*log_km + log_km*fuel_econ + log_km*seats +
            fuel_econ*fuel_econ + fuel_econ*seats + seats*seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data= dek_train1)
summary(fit5)

#Fit 6
fit6 = lm(log_price ~ log_km + fuel_econ + seats + 
            log_km*fuel_econ + log_km*seats + fuel_econ*seats +
            log_km*log_km*fuel_econ + log_km*log_km*seats +
            log_km*fuel_econ*seats + log_km*fuel_econ*fuel_econ +
            fuel_econ*fuel_econ*seats + log_km*seats*seats +
            fuel_econ*seats*seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data = dek_train1)
summary(fit6)

#Fit 7
fit7 = lm(log_price ~ log_km + fuel_econ + seats + 
            log_km*fuel_econ + log_km*seats + fuel_econ*seats +
            log_km*fuel_econ*seats +
            as.factor(fuel) +
            as.factor(seller_type) +
            as.factor(transmission),
          data = dek_train1)
summary(fit7)

#Transform all Trustmark Dealer observations to Dealer in Test dataset
dek_test1 <- dek_test %>%
  mutate(seller_type = if_else(seller_type == "Trustmark Dealer",
                               "Dealer", seller_type))

dek_test %>%
  filter(seller_type == "Dealer") %>%
  summarize(num_ob = n()) #n = 285, Dealer

dek_test %>%
  filter(seller_type == "Trustmark Dealer") %>%
  summarize(num_ob = n()) #n = 64, Trustmark Dealer

dek_test1 %>%
  filter(seller_type == "Dealer") %>%
  summarize(num_ob = n()) #n = 349, Dealer (285 + 64)

#checking independence of variables
cor(dek_train1$km_driven, dek_train1$fuel_econ) #21.9%  negative correlation

cor(dek_train1$km_driven, dek_train1$seats) #27.7% positive correlation

cor(dek_train1$fuel_econ, dek_train1$seats) #48.3% negative correlation

#Evaluate fit of Model 7
plot(fit7)

#next step as of 19Nov21: remove outliers as identified by Res vs Fitted and Leverage plots