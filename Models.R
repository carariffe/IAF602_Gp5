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