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
           