shinyServer(
  function (input, output, session) {
    output$myPlot <- renderPlot({

      inpyear <- input$year
      inpmon <- input$month
      inpalg <- input$Algorithm
      tamilnadu_data <- read.csv("~/Desktop/Project review1/prediction_of_rainfall/tamilnadu_data.csv")

      #library(MASS)
      #library(tseries)
      #rain_data_year <- ts(tamilnadu_data$YEAR)
      #randomVec <- rnorm(inpyear, mean = as.numeric(input$year), sd = as.numeric(input$year))
      #hist(randomVec, col = "blue")

      library(forecast)

      a <- inpyear - 2014

      if (inpalg == "HoltWinters Model") {

        if (inpmon == "January") {
          x <- ts(tamilnadu_data$JAN, frequency = 1, start = c(1901))
          rain_data_jan_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_jan_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)
        } else if (inpmon == "February") {

          x <- ts(tamilnadu_data$FEB, frequency = 1, start = c(1901))
          rain_data_feb_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_feb_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "March") {
          x <- ts(tamilnadu_data$MAR, frequency = 1, start = c(1901))
          rain_data_mar_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_mar_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "April") {
          x <- ts(tamilnadu_data$APR, frequency = 1, start = c(1901))
          rain_data_apr_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_apr_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "May") {
          x <- ts(tamilnadu_data$MAY, frequency = 1, start = c(1901))
          rain_data_may_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_may_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "June") {
          x <- ts(tamilnadu_data$JUN, frequency = 1, start = c(1901))
          rain_data_jun_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_jun_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "July") {
          x <- ts(tamilnadu_data$JUL, frequency = 1, start = c(1901))
          rain_data_jul_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_jul_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "August") {
          x <- ts(tamilnadu_data$AUG, frequency = 1, start = c(1901))
          rain_data_aug_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_aug_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "September") {
          x <- ts(tamilnadu_data$SEP, frequency = 1, start = c(1901))
          rain_data_sep_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_sep_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "October") {
          x <- ts(tamilnadu_data$OCT, frequency = 1, start = c(1901))
          rain_data_oct_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_oct_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "November") {
          x <- ts(tamilnadu_data$NOV, frequency = 1, start = c(1901))
          rain_data_nov_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_nov_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        } else if (inpmon == "December") {
          x <- ts(tamilnadu_data$DEC, frequency = 1, start = c(1901))
          rain_data_dec_hw <- HoltWinters(x, beta = F, gamma = F)
          w <- forecast::: forecast.HoltWinters(rain_data_dec_hw, h = a)
          forecast::: plot.forecast(w)
          summary(w)

        }

      }
      if (inpalg == "ARIMA Model") {

        if (inpmon == "January") {
          x <- ts(tamilnadu_data$JAN, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "February") {
          x <- ts(tamilnadu_data$FEB, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "March") {
          x <- ts(tamilnadu_data$MAR, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "April") {
          x <- ts(tamilnadu_data$APR, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "May") {
          x <- ts(tamilnadu_data$MAY, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "June") {
          x <- ts(tamilnadu_data$JUN, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "July") {
          x <- ts(tamilnadu_data$JUL, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "August") {
          x <- ts(tamilnadu_data$AUG, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "September") {
          x <- ts(tamilnadu_data$SEP, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "October") {
          x <- ts(tamilnadu_data$OCT, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "November") {
          x <- ts(tamilnadu_data$NOV, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }
        if (inpmon == "December") {
          x <- ts(tamilnadu_data$DEC, frequency = 1, start = c(1901))
          fit_x <- auto.arima(x)
          forecast_x = forecast(fit_x, h = a)
          plot(forecast_x, h = a)
          summary(forecast_x)
        }

      }

    })

  }
)
