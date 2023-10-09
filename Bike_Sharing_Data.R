---
title: "Forecast daily bike rental demand using time series models"
author: "KANKAR MALLIKARJUN"
date: "r Sys.Date()"
output: html_document
---
  
# About Data Analysis Report

  cat("This RMarkdown file contains the report of the data analysis done for the project on forecasting daily bike rental demand using time series models in R.",
      "It contains analysis such as data exploration, summary statistics and building the time series models.",
      "The final report was completed on", format(Sys.Date(), "%B %d, %Y"), ".\n\n",
      
      "*Data Description:*\n",
      "This dataset contains the daily count of rental bike transactions between years 2011 and 2012 in Capital bikeshare system with the corresponding weather and seasonal information.\n\n",
      
      "*Data Source:* https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset\n\n",
      
      "*Relevant Paper:*\n",
      "Fanaee-T, Hadi, and Gama, Joao. Event labeling combining ensemble detectors and background knowledge, Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg", sep="\n")


# Task One: Load and explore the data

## Load data and install packages

{r}
## Import required packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tibble")
install.packages("tidyr")
install.packages("readr")
install.packages("purrr")
install.packages("dplyr")
install.packages("stringr")
install.packages("forcats")
install.packages("lubridate")


library("tidyverse")

library("ggplot2")
library("dplyr")

install.packages("timetk")
library("timetk")

install.packages("tseries")
library("tseries")

library("forecast")

install.packages("rmarkdown")
install.packages("knitr")
library(timetk)
library(knitr)

#importing the data

day_data <- read.csv("C:\\Users\\MALLIKARJUN\\Downloads\\bike_sharing_dataset.csv")

View(day_data)

## Describe and explore the data

{r}
day_data[,"dteday"] <- as.Date(day_data[,"dteday"])
day_data[,"ncnt"] <- day_data[,"cnt"] / max(day_data[,"cnt"])
day_data[,"nr"] <- day_data[,"registered"] / max(day_data[,"registered"])
day_data[,"rr"] <- day_data[,"cnt"] / max(day_data[,"registered"])
summary(day_data)

{r}

# Task Two: Create interactive time series plots

{r}

#day_data
day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, temp, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Temperature", 
                  .title = "Normalized Temperature vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, hum, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Humidity", 
                    .title = "Normalized Humidity vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, windspeed, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Windspeed", 
                  .title = "Windspeed vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, ncnt, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Bike Rentals", 
                  .title = "Normalized Bike Rentals vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, nr, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Registered Rentals", 
                  .title = "Normalized Registered Rentals vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, rr, .color_var = season, .x_lab = "Date", .y_lab = "Ratio", 
                .title = "Ratio of Rentals to Registration vs Date for Day Data", .interactive = TRUE)

{r}

# Task Three: Smooth time series data

{r}
#since the humdity and windspeed data has so much noise, I will focus on temperature and bike rentals
#data cleaning
day_data[,"temp"] <- tsclean(day_data[,"temp"])
day_data[,"ncnt"] <- tsclean(day_data[,"ncnt"])
day_data[,"nr"] <- tsclean(day_data[,"nr"])
day_data[,"rr"] <- tsclean(day_data[,"rr"])
head(day_data)

#day_data
day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, temp, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Temperature", 
                   .title = "Normalized Temperature vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, ncnt, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Bike Rentals", 
                  .title = "Normalized Bike Rentals vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, nr, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Registered Rentals", 
              .title = "Normalized Registered Rentals vs Date for Day Data", .interactive = TRUE)

day_data %>%
  group_by(yr) %>%
  plot_time_series(dteday, rr, .color_var = season, .x_lab = "Date", .y_lab = "Normalized Registered Rentals", 
              .title = "Ratio of Rentals to Registration vs Date for Day Data", .interactive = TRUE)

{r}

# Task Four: Decompose and assess the stationarity of time series data

{r}

#day data
day_data[,"temp"] %>% adf.test()

day_data[,"ncnt"] %>% adf.test()

day_data[,"nr"] %>% adf.test()

day_data[,"rr"] %>% adf.test()


#decomposes the data
freq <- 365

#normalized day rentals
norm_rentals <- ts(day_data[, "nr"], frequency = freq)
decomped1 <- stl(norm_rentals, "periodic")
plot(decomped1$time.series[,2], ylab = "Stationary of the Normalized Rental Reservations", 
     xlab = "Day of the Year")

checkresiduals(decomped1$time.series[, 3])

#normalized day counts
norm_cnt <- ts(day_data[, "ncnt"], frequency = freq)
decomped2 <- stl(norm_cnt, "periodic")
plot(decomped2$time.series[,2], ylab = "Stationary of the Normalized Rental Counts", 
     xlab = "Day of the Year")

checkresiduals(decomped2$time.series[, 3])

#normalized day rental rates
norm_rr <- ts(day_data[, "rr"], frequency = freq)
decomped3 <- stl(norm_rr, "periodic")
plot(decomped3$time.series[,2], ylab = "Stationary of the Normalized Rental Counts to Reservations", 
     xlab = "Day of the Year")

checkresiduals(decomped3$time.series[, 3])

#returns the stats
print("-------Noramlized Rental Reservations")
## [1] "-------Noramlized Rental Reservations"
shapiro.test(decomped1$time.series[, 3])

print("-------Normalized Count of Rentals")
## [1] "-------Normalized Count of Rentals"
shapiro.test(decomped2$time.series[, 3])

print("-------Normalized Ratio of Rentals to Reservations")
## [1] "-------Normalized Ratio of Rentals to Reservations"
shapiro.test(decomped3$time.series[, 3])

{r}

# Task Five: Fit and forecast time series data using ARIMA models

{r}
#bike count predictions
fit1 <- auto.arima(norm_cnt, seasonal = TRUE, )
hist(fit1$residuals, xlab = "Residual", ylab = "Distribution", main = "Histogram of Model Errors - Bike Count")

shapiro.test(fit1$residuals)

prediction1 <- forecast(fit1, 25)
plot(prediction1, xlab = "Date", ylab = "Normalized Count of Rentals", main = "Prediction of Bike Rental Counts")

#predictions of number of reservations
fit2 <- auto.arima(norm_rentals, seasonal = TRUE, )
hist(fit2$residuals, xlab = "Residual", ylab = "Distribution", main = "Histogram of Model Errors - Rental Count")

shapiro.test(fit2$residuals)

prediction2 <- forecast(fit2, 25)
plot(prediction2, xlab = "Date", ylab = "Normalized Rentals", main = "Prediction of Bike Rentals")

#bike count predictions
fit2 <- auto.arima(norm_cnt, seasonal = TRUE, )
hist(fit2$residuals, xlab = "Residual", ylab = "Distribution", main = "Histogram of Model Errors - Count to Rental Ratio")

shapiro.test(fit2$residuals)

prediction3 <- forecast(fit2, 25)
plot(prediction3, xlab = "Date", ylab = "Normalized Rental Ratio", main = "Prediction of Bike Rentals to Reservations")

{r}

## Task Six: Findings and Conclusions

{r}
cat("After analyzing the raw data and employing the ARIMA model to forecast ride-sharing trends,",
    "I was able to project the demand for the upcoming 25 days. The data suggests a positive correlation",
    "between warmer weather and an increase in bike rentals. Over two years, there's a discernible rise in rentals",
    "compared to the preceding year. Given that the data concludes at the end of a cycle, I anticipate rental numbers",
    "to surpass the previous year's figures, aligning with the model's predictions. In essence, the data showcases",
    "a yearly fluctuation, trending towards an overall increase in rentals.", sep="\n")

{r}