flight_price_df = read.csv("/Users/izzy/Downloads/flight_price/Clean_Dataset.csv")

unique(flight_price_df$class)
unique(flight_price_df$departure_time)
unique(flight_price_df$stops)

flight_price_df$class_encoded = ifelse(flight_price_df$class == "Economy", 0, 1)
stops_one_hot = model.matrix(~ stops - 1, data = flight_price_df)
flight_price_df = cbind(flight_price_df, stops_one_hot)


airline_one_hot = model.matrix(~ airline - 1, data = flight_price_df)
flight_price_df = cbind(flight_price_df, airline_one_hot)

departure_time_one_hot = model.matrix(~ departure_time - 1, data = flight_price_df)
flight_price_df = cbind(flight_price_df, departure_time_one_hot)


lm_fitted = lm(price~
                 duration +
                 days_left + 
                 class_encoded +
                 stopsone +
                 stopszero +
                 departure_timeEarly_Morning +
                 departure_timeEvening +
                 departure_timeLate_Night +
                 departure_timeMorning + 
                 departure_timeNight +
                 airlineAir_India +
                 airlineAirAsia +
                 airlineGO_FIRST +
                 airlineIndigo +
                 airlineSpiceJet, data=flight_price_df)

summary(lm_fitted)
max(flight_price_df$price)

new_observation <- flight_price_df[7, ]
prediction <- predict(lm_fitted, newdata = new_observation)
print(prediction)



vars <- subset(flight_price_df, select = c("duration",
                                                      "days_left",
                                                      "class_encoded",
                                                      "stopsone",
                                                      "stopszero",
                                                      "departure_timeEarly_Morning",
                                                      "departure_timeEvening",
                                                      "departure_timeLate_Night",
                                                      "departure_timeMorning",
                                                      "departure_timeNight",
                                                      "airlineAir_India",
                                                      "airlineAirAsia",
                                                      "airlineGO_FIRST",
                                                      "airlineIndigo",
                                                      "airlineSpiceJet"
                                                      ))

std_error <- function(dependent_vars, independent_vars){
  Y <- as.vector(dependent_vars)
  X <- cbind(constant=1, as.matrix(independent_vars))
  betas <- solve(t(X) %*% X, t(X) %*% Y)
  sigmasq <- sum((Y - X%*%betas)^2)/(nrow(X)-ncol(X))
  varcovar <- sigmasq*chol2inv(chol(t(X)%*%X))  
  stderr <- sqrt(diag(varcovar))
  results <- cbind(betas, stderr)
  
  # return für class_encoded
  return(stderr[4]) 
}

print(std_error(flight_price_df$price, vars))

beta_class_encoded <- 44992.19330

t_wert <- function(estimate, s_error){
  return(estimate / s_error)
}

t_wert(beta_class_encoded, std_error(flight_price_df$price, vars))


freiheits_grade <- nrow(vars) - ncol(vars)
p_value <- 2 * pt(q = abs(t_wert(44992.19330, std_error(flight_price_df$price, vars))), df = freiheits_grade, lower=FALSE)
print(p_value)


# RSE
predictions <- predict(lm_fitted)
residuals <- flight_price_df$price - predictions
rss <- sum(residuals^2)

rse <- sqrt(rss/ nrow(flight_price_df) - ncol(flight_price_df) - 2)
print(rse)

# R^2
mean_price <- mean(flight_price_df$price)
r_squared <- 1 - rss / sum((flight_price_df$price - mean_price)^2)
print(r_squared)

# F-Statistic
p <- ncol(vars)
df_num <- p
df_denom <- nrow(vars) - p - 1

ess <- sum((predictions - mean(Y))^2)

ms_num <- ess / df_num
ms_denom <- rss / df_denom

f_wert <- ms_num / ms_denom

p_value <- pf(f_stat, df_num, df_denom, lower.tail = FALSE)

print(f_wert)
print(p_value)


