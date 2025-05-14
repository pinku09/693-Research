# Select all the columns with the data for the stock you want to do analysis for
stocks_df = read.table(file='clipboard',header=TRUE)
stocks_df

# Perform Dickey-Fuller test
# If p value is less than or equal to 0.05 indicates the time series data is stationary
if(!require('tseries')){
  install.packages('tseries')
}

library(tseries) 
adf_result = adf.test(stocks_df$CHG_PCT_1D, alternative="stationary")
print(adf_result)

# Normalizing Sentiments - min-max normalization
if(!require('scales')){
  install.packages('scales')
}

library(scales) 
sentiment_cols  = c("NEWS_POS_SENTIMENT_COUNT", 
                    "NEWS_NEG_SENTIMENT_COUNT",
                    "NEWS_NEUTRAL_SENTIMENT_COUNT")

stocks_df[sentiment_cols] = lapply(stocks_df[sentiment_cols], function(x) rescale(x, to=c(0,1)))


# Correlation matrix for sentiment counts and price change
cor(stocks_df$CHG_PCT_1D, stocks_df$NEWS_POS_SENTIMENT_COUNT)
cor(stocks_df$CHG_PCT_1D, stocks_df$NEWS_NEG_SENTIMENT_COUNT)
cor(stocks_df$CHG_PCT_1D, stocks_df$NEWS_NEUTRAL_SENTIMENT_COUNT)


# Plotting a correlation heatmap
if(!require('corrplot')){
  install.packages('corrplot')
}

library(corrplot)
cor_matrix = cor(stocks_df[, c("CHG_PCT_1D", "NEWS_POS_SENTIMENT_COUNT", "NEWS_NEG_SENTIMENT_COUNT", "NEWS_NEUTRAL_SENTIMENT_COUNT")])
corrplot(cor_matrix, method = "circle")

# Plot a graph to see all the dates in stocks_df, sentiments and price percent change
if(!require('tidyr')){
  install.packages('tidyr')
}
if(!require('ggplot2')){
  install.packages('ggplot2')
}
if(!require('dplyr')){
  install.packages('dplyr')
}

library(ggplot2)
library(dplyr)
library(tidyr)

# Ensure 'Date' is in date format
stocks_df$Date = as.Date(stocks_df$Dates, format="%m/%d/%Y")

# Select only positive and negative sentiment columns
df_long = stocks_df %>%
  select(Date, NEWS_POS_SENTIMENT_COUNT, NEWS_NEG_SENTIMENT_COUNT, CHG_PCT_1D) %>%
  pivot_longer(cols = c("NEWS_POS_SENTIMENT_COUNT", "NEWS_NEG_SENTIMENT_COUNT"),
               names_to = "Sentiment_Type",
               values_to = "Sentiment_Value")

# Scale factor for dual-axis clearly
scale_factor = max(stocks_df$CHG_PCT_1D, na.rm=TRUE)

ggplot() +
  # Positive and Negative Sentiments as points
  geom_point(data = df_long, aes(x = Date, y = Sentiment_Value, color = Sentiment_Type), size = 2.5, alpha = 0.8) +
  
  # Price change as dashed line, scaled appropriately
  geom_line(data = stocks_df, aes(x = Date, y = CHG_PCT_1D / scale_factor), 
            color = "black", linewidth = 1, alpha = 0.7, linetype = "dashed") +
  
  # Dual-axis setup
  scale_y_continuous(
    name = "Normalized Sentiment (0-1)",
    sec.axis = sec_axis(~.*scale_factor, name = "Price Change (%)")
  ) +
  
  labs(title = "Positive & Negative Sentiments and Stock Price Change (%) Over Time",
       x = "Date",
       color = "Sentiment Type") +
  
  scale_color_manual(values = c("NEWS_POS_SENTIMENT_COUNT" = "green",
                                "NEWS_NEG_SENTIMENT_COUNT" = "red")) +
  
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "black"),
        legend.position = "bottom")


# Model 1 -Simple Model - Linear Regression
model_original = lm(CHG_PCT_1D ~ 1 + NEWS_POS_SENTIMENT_COUNT +
                      NEWS_NEG_SENTIMENT_COUNT +
                      NEWS_NEUTRAL_SENTIMENT_COUNT, data = stocks_df)

anova(model_original)
summary(model_original)


# Multicolinearity check
# Less than 5 indicates no multicolinearity
if (!require("car")) {
  install.packages("car")
}
library(car)

vif_values = vif(model_original)
mean(vif_values)

# Visual test - Cook's D bar test
if (!require("olsrr")) {
  install.packages("olsrr")
}
library(olsrr)
ols_plot_cooksd_bar(model_original)

# Checking for heteroscedasticity (Breusch-Pagan test)
# p-value of is less than 0.05, indicates Unequal variance (heteroscedasticity)
if (!require("lmtest")) {
  install.packages("lmtest")
}
library(lmtest)
bp_test = bptest(model_original)
print(bp_test)


# Calculating Lagged sentiments on say 2 day window 
# (making sure earnings 2 window doesn't consider earnings 1 sentiments)
df = stocks_df %>%
  arrange(as.Date(Date)) %>%
  mutate(NEWS_POS_SENTIMENT_LAG1 = lag(NEWS_POS_SENTIMENT_COUNT, 1),
         NEWS_NEG_SENTIMENT_LAG1 = lag(NEWS_NEG_SENTIMENT_COUNT, 1),
         NEWS_NEUTRAL_SENTIMENT_LAG1 = lag(NEWS_NEUTRAL_SENTIMENT_COUNT, 1)) %>%
  ungroup()


# Model 2 - Lagged sentiment variables model
model_lagged = lm(CHG_PCT_1D ~ 1 + NEWS_POS_SENTIMENT_COUNT +
                    NEWS_NEG_SENTIMENT_COUNT +
                    NEWS_NEUTRAL_SENTIMENT_COUNT +
                    NEWS_POS_SENTIMENT_LAG1 +
                    NEWS_NEG_SENTIMENT_LAG1 +
                    NEWS_NEUTRAL_SENTIMENT_LAG1, 
                  data = df)
anova(model_lagged)
summary(model_lagged)

# Model 3 - Robust Regression on lagged sentiments - to deal with heteroskedascidty
if (!require("robust")) {
  install.packages("robust")
}
library(robust)
model_robust = lmRob(CHG_PCT_1D ~ 1 + NEWS_POS_SENTIMENT_COUNT +
                       NEWS_NEG_SENTIMENT_COUNT +
                       NEWS_NEUTRAL_SENTIMENT_COUNT +
                       NEWS_POS_SENTIMENT_LAG1 +
                       NEWS_NEG_SENTIMENT_LAG1 +
                       NEWS_NEUTRAL_SENTIMENT_LAG1, 
                     data = df)
anova(model_robust)
summary(model_robust)

# Comparing all 3 models - model_original, model_lagged, model_robust

if (!require("texreg")) {
  install.packages("texreg")
}
library(texreg)


# Export regression table to HTML
screenreg(
  list(model_original, model_lagged, model_robust), caption = "Stock Sentiment Regression Models", include.ci = FALSE)
