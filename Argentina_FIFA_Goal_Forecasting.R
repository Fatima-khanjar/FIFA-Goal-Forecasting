#part1 
#a
df_results <- read.csv("D:\\fall semster 23-24\\Time Series & Forecasting\\Project\\Dataset-20240123\\results.csv")
df_goal.scorers <- read.csv("D:\\fall semster 23-24\\Time Series & Forecasting\\Project\\Dataset-20240123\\goalscorers.csv")
View(df_results)
View(df_goal.scores)

#b
head(df_results)
str(df_results)
#This dataframe contains information about sports results, possibly including details such as match dates, teams involved, scores, and other relevant information.
head(df_goal.scorers)
str(df_goal.scorers)
#This dataframe likely contains information about goal scorers in sports matches. It may include details such as player names, the number of goals scored, match details, and any other relevant information.

#c
library(dplyr)
my_team <- "Argentina"
filtered_results <- df_results %>%
  filter(home_team == my_team, home_score > away_score)
View(filtered_results)

#d
specific_date <- "2022-02-01"
specific_match <- filtered_results %>%
  filter(date == specific_date)
if (nrow(specific_match) > 0) {
  match_result <- specific_match %>%
    select(date, home_team, away_team, home_score, away_score, tournament, city, country)
cat("Match Result on", specific_date, ":\n")
print(match_result)
goal_scorers <- df_goal.scorers %>%
  filter(date == specific_date, team == my_team)
if (nrow(goal_scorers) > 0) {
  cat("\nGoal Scorers:\n")
  print(goal_scorers)
} else {
  cat("\nNo goal scorer information available.\n")
  }
} else {
cat(paste("No match for", my_team, "on", specific_date, "\n"))
}
View(goal_scorers)

#e
if (nrow(goal_scorers) > 0) {
  cat("\nGoal Scorers:\n")
  print(goal_scorers)
  top_scorer <- goal_scorers %>%
    group_by(scorer) %>%
    summarize(total_goals = sum(!is.na(minute)))
  cat("\nTop Goal Scorer:\n")
  print(top_scorer[which.max(top_scorer$total_goals),])
} else {
  cat("\nNo goal scorer information available.\n")
}

#f
tournament_counts <- df_results %>%
  group_by(tournament) %>%
  count(sort = TRUE) %>%
  arrange(desc(n))
cat("Types of Tournaments:\n")
print(unique(df_results$tournament))
cat("\nTop 5 Tournaments:\n")
print(head(tournament_counts, 5))
View(head(tournament_counts, 5))

#part2
#a
library(lubridate)
df_fifa_results <-df_results %>% mutate(year=year(ymd(df_results$date)))
View(df_fifa_results)

#b
df_fifa_results <- df_fifa_results %>%
  filter(year != 2023)

#c
df_fifa_results_home <- df_fifa_results %>%
  group_by(year, home_team) %>%
  summarise(sum_home_score = sum(home_score, na.rm = TRUE))
View(df_fifa_results_home)
print(df_fifa_results_home)

df_fifa_results_away <- df_fifa_results %>%
  group_by(year, away_team) %>%
  summarise(sum_away_score = sum(away_score, na.rm = TRUE))
View(df_fifa_results_away)
print(df_fifa_results_away)

df_fifa_goals <- inner_join(df_fifa_results_home, df_fifa_results_away, by = c("year", "home_team" = "away_team"))
df_fifa_goals <- df_fifa_goals %>%
  mutate(nb_of_goals = sum_home_score + sum_away_score) %>%
  select(year, team = home_team, nb_of_goals)
View(df_fifa_goals)
print(df_fifa_goals)

library(ggplot2)
df_argentina_goals <- df_fifa_goals %>%
  filter(team == "Argentina")
View(df_argentina_goals)
ggplot(df_argentina_goals, aes(x = year, y = nb_of_goals)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Number of Goals by Argentina in Each Year",
       x = "Year",
       y = "Number of Goals") +
  theme_minimal()


#part3
mean_goals <- mean(df_fifa_goals$nb_of_goals)
sd_goals <- sd(df_fifa_goals$nb_of_goals)
cat("Mean of total number of goals scored:", mean_goals, "\n")
cat("Standard Deviation of total number of goals scored:", sd_goals, "\n")
all_years <- unique(df_fifa_goals$year)
missing_years <- setdiff(seq(min(all_years), max(all_years)), all_years)

if (length(missing_years) > 0) {
  cat("Argentina team missed the following years:", missing_years, "\n")
  df_fifa_goals <- bind_rows(
    df_fifa_goals,
    data.frame(
      year = missing_years,
      team = "Argentina",
      nb_of_goals = 0
    )
  )
  df_fifa_goals <- df_fifa_goals %>%
    arrange(year)
  
  cat("Missing years filled in with zero goals.\n")
} else {
  cat("Argentina team did not miss any years.\n")
}


#ARIMA MODELING
library(dplyr)
library(stats)
#part1
ts_data <- df_fifa_goals %>%
  filter(team == "Argentina") %>%
  select(year, nb_of_goals)
ts_data <- ts(ts_data$nb_of_goals, start = min(ts_data$year), end = max(ts_data$year), frequency = 1)

#part2
par(mfrow=c(1,3))
plot(ts_data)
acf(ts_data,lag.max = 40)
pacf(ts_data,lag.max = 40)

#part3
#No, it  does not look stationary, it has trend
#remove trend
argentina.diff<-diff(ts_data$nb_of_goals)
install.packages("tseries")
library(tseries)
#perform Augmented Dickey-Fuller (ADF) test to check if the data becomes stationary
adf_test_result <- adf.test(argentina.diff, alternative = "stationary")
print(adf_test_result)

#part4
plot(argentina.diff)
acf(ts(argentina.diff),lag.max = 20)
pacf(ts(argentina.diff),lag.max = 20)

#part5-part6
library(forecast)
##model 1: ARIMA(3,1,0)
fit.ar31<-arima(ts_data,order = c(3,1,0))
##model 2: ARIMA(0,1,1)
fit.ma11<-arima(ts_data,order = c(0,1,1))
##model 3: ARIMA(0,1,5)
fit.ma15<-arima(ts_data,order = c(0,1,5))
##model 4: ARIMA(3,1,1)
fit.arima311<-arima(ts_data,order = c(3,1,1))
print(summary(fit.ar31))
print(summary(fit.ma11))
print(summary(fit.ma15))
print(summary(fit.arima311))
#select the best model
fit.ar31$aic
fit.ma11$aic
fit.ma15$aic
fit.arima311$aic
# According to the AIC values the model 1 (ARIMA(3,1,0)) has the lowest value so it is the best model
##we use the auto.arima() to make sure of selected model is the best model
fit.auto<-auto.arima(ts_data)
fit.auto

#part7
##residuals diagnosis
tsdiag(fit.ar31)
Box.test(fit.ar31$residuals, lag = 20, type = "Ljung-Box")
#according to Ljung-Box test p_value = 0.7769 > 0.05 so we fail to reject the null hypothesis that the residuals resemble white noise.
##check the normality of residuals
shapiro.test(fit.ar31$residuals)
par(mfrow=c(1,1))
hist(residuals_ar31, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")
#according to shapiro test p_value = 0.1612 > 0.05 so we fail to reject the null hypothesis that the residuals can be considered approximately normally distributed.


#part8
library(fpp3)
forecast::forecast(fit.ar31,h=1)
plot(forecast::forecast(fit.ar31,h=1))


#part9
#I want to add the lagged feature and then solve the forecasting problem using Neural Network
install.packages("keras")
library(keras)
library(reticulate)
new_data <- matrix(ts_data,ncol = 1)
View(new_data)
max_value <- max(new_data)
min_value <- min(new_data)
scaled_data <- (new_data - min_value)/(max_value - min_value)
lag <- 1
arg_dataset <- NULL
for(i in 1:lag){
  arg_dataset <- cbind(arg_dataset, stats::lag(scaled_data,-i))
}
arg_dataset <- cbind(arg_dataset,scaled_data[lag+1:length(scaled_data)])
colnames(arg_dataset) <- c(paste0("Lag_1",1:lag),"Goal")
arg_dataset <- na.omit(arg_dataset)

set.seed(123) 
train_size <- floor(0.8 * nrow(arg_dataset))
train_data <- arg_dataset[1:train_size, ]
test_data <- arg_dataset[(train_size+1):nrow(arg_dataset), ]
# split the data into training ans testing
x_train <- train_data[, -ncol(train_data)]
y_train <- train_data[, ncol(train_data)]
x_test <- test_data[, -ncol(test_data)]
y_test <- test_data[, ncol(test_data)]
# Define the neural network architecture
model <- keras_model_sequential()
model %>%
  layer_dense(units = 8, activation = 'relu', input_shape = c(lag)) %>%
  layer_dense(units = 1)
# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error', 
  metrics = c('mean_absolute_error')  
)
# Train the model
history <- model %>% fit(
  x_train, y_train,
  epochs = 200,
  batch_size = 1,
  validation_split = 0.2
)
# Evaluate the model
performance <- model %>% evaluate(x_test, y_test, verbose = 0)
# Predict and rescale the predictions
predictions <- model %>% predict(x_test)
predictions <- predictions * (max_value - min_value) + min_value