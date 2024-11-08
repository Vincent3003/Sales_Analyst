############### Question for sales forecasting ############
# # How does the rating of products correlate with their sales over time?

# Is there a relationship between the price of a product and its sales volume?

# Do products with higher recommendation rates (is_recommended) show different 
# sales patterns compared to less recommended products?

# How does the helpfulness of reviews relate to product popularity and sales?

# Is there a seasonal pattern in product ratings or recommendations that might 
# influence sales forecasts?

# Do certain brands show more consistent or predictable sales patterns than others?


# How do different skin types, skin tones, or other customer characteristics 
# relate to product popularity and potential sales trends?

# Is there a correlation between the number of reviews (total_feedback_count) and sales volume?

# Do products with more positive feedback (total_pos_feedback_count) show 
# different sales trends compared to those with more negative feedback?

# How does the submission_time of reviews relate to sales peaks or troughs?


# Are there any specific product categories (based on product_name) that show 
# more predictable or stable sales patterns?

# Is there a price point (price_usd) at which sales patterns become more or less predictable?





################# Time Series and Forecasting for review_new dataset ###########3
summary(review_new)   # to find mean & median of each X variable and Y variable
# rating:                   mean < median (4.299 < 5.000)   ==> left skewed
# is_recommended:           mean < median (0.84 < 1.00)     ==> left skewed
# helpfulness:              mean < median (0.8 < 0.9)       ==> left skewed
# total_feedback_count:     mean > median (4.177 > 0.000)   ==> right skewed
# total_neg_feedback_count: mean > median (0.8949 > 0.000)  ==> right skewed
# total_pos_feedback_count: mean > median (3.282 > 0.000)   ==> right skewed
# price_usd:                mean > median (49.01 > 39.00)   ==> right skewed

str(review_new)

# Check for outliers in total feedback counts
ggplot(review_new, aes(x = total_feedback_count)) + geom_boxplot() + ggtitle("Total Feedback Count")
ggplot(review_new, aes(x = price_usd)) + geom_boxplot() + ggtitle("Total Price in USD")


boxplot(review_new$total_feedback_count, horizontal = TRUE, main="Total Feedback Count")
boxplot(review_new$price_usd, horizontal = TRUE, main="Total Price in USD")


# calculate the correlations between your X variables and Y variable
table <- review_new %>% select(rating, is_recommended, helpfulness, 
                               total_feedback_count, total_pos_feedback_count, 
                               total_neg_feedback_count, price_usd) %>% data.frame()
cor(cbind(table), use = "complete.obs")
pairs(table, upper.panel = NULL)

# find the unique of each skin_tone, eye_color, skin_type, hair_color, submission_time
unique(review_new$skin_tone)
unique(review_new$eye_color)
unique(review_new$skin_type)
unique(review_new$hair_color)
unique(review_new$brand_name)
unique(year(review_new$submission_time))

# Aggregate data by month
monthly_reviews <- aggregate(review_new$author_id, 
                             by = list(month = format(review_new$submission_time, "%Y-%m")), 
                             FUN = length)

# Rename the columns for clarity
colnames(monthly_reviews) <- c("month", "review_count")

# Convert to a time series object
review_ts <- ts(monthly_reviews$review_count, 
                start = c(2008,08), 
                frequency = 12)  # Monthly data

review_mult <- decompose(review_ts, type="multiplicative")
# Plot the time series
autoplot(review_mult, facets = TRUE) +
  ggtitle("Classical multiplicativ of Monthly Reviews Over Time") +
  xlab("Year") + ylab("Number of Reviews")

review_additive <- decompose(review_ts, type="additive")
# Plot the time series
plot(review_additive)

ggsdc(review_additive, aes(x = month, y = review_count), 
      frequency = 12, method = "seas", type = "multiplicative") + geom_line() +
  ggtitle("Classical multiplicative decomposition of Review") + xlab("Time") + ylab("Review by Monthly")

autoplot(review_additive) +
  ggtitle("Classical additive of Monthly Reviews Over Time") +
  xlab("Year") + ylab("Number of Reviews")

ggsdc(review_additive, aes(x = month, y = review_count), 
      frequency = 12, method = "decompose", type = "multiplicative") + geom_line() +
  ggtitle("Classical multiplicative decomposition of Review") + xlab("Time") + ylab("Review by Monthly")

ggsdc(review_additive, aes(x = month, y = review_count), 
      frequency = 7, method = "decompose", type = "additive") + geom_line() +
  ggtitle("Classical additive decomposition of windspeed") + xlab("Time") + ylab("Windspeed by day value")


# Looking at the normal ACF & PACF for windspeed
diff_windspeeds_turbine10.ts <- diff(windspeeds_turbine10.ts)
ggPacf(windspeeds_turbine10.ts) + ggtitle("PACF of Windspeed for Turbine 10")
ggAcf(windspeeds_turbine10.ts) + ggtitle("ACF of Windspeed for Turbine 10")

# Autoregression Model AR(1) Model
acf(review_ts, lag.max = 10, plot = FALSE)
acf(review_ts, lag.max = 10, plot = TRUE)

# Autoregression Model AR(1) Model
pacf(review_ts, lag.max = 10, plot = FALSE)
pacf(review_ts, lag.max = 10, plot = TRUE)

review_ts_1 = review_ts[-1]
review_ts_176 = review_ts[-176]
AR1 = lm(review_ts_1 ~ review_ts_176)
summary(AR1)
