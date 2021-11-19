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