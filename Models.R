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