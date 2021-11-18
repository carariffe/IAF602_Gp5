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
