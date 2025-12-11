library(forecast)
library(ggplot2)
library(TSA)
library(tseries)



perform_eda <- function(ts_data, dataset_name) {
  plot(ts_data, main = paste(dataset_name, "Time Series"), ylab = "Values", xlab = "Time")
  acf(ts_data, main = paste("ACF of", dataset_name))
  pacf(ts_data, main = paste("PACF of", dataset_name))
}

decom <- function(ts_data){
  decom_data <- decompose(ts_data)
  plot(decom_data)
  return(decom_data)
}
fit_arima <- function(ts_data){
  adf <- adf.test(ts_data)
  if (adf$p.value>0.05){
    ts_data <- diff(ts_data)
    plot(ts_data, main="Diff")
  }
  arima_model <- auto.arima(ts_data , seasonal = FALSE)
  forcase <- forecast(arima_model,h=12 )
  plot(forcase ,main="forcase")
  return(arima_model)
}
fit_sarima <- function(ts_data){
  sarima_model <- auto.arima(ts_data , seasonal = TRUE)
  forcase <- forecast(arima_model,h=12 )
  plot(forcase ,main="forcase")
  return (sarima_model)
}

compare_models <- function(arima_model,sarima_model,h=12,ts_data){
  h <- min(h,length(ts_data))
  arima_fc <- forecast(arima_model,h=h)$mean
  sarima_fc <- forecast(sarima_model,h=h)$mean
  actucal_value <- ts_data[(length(ts_data)-h+1):length(ts_data)]
  arima_acc <- accuracy(arima_fc,actucal_value)
  sarima_acc <- accuracy(sarima_fc,actucal_value)
  
  list(
    arima_fc=arima_fc,
    sarima_fc=sarima_fc,
    actucal_value=actucal_value,
    sarima_acc=sarima_acc,
    arima_acc=arima_acc
  )
  
}





data(milk)
milk_data <- milk

# EDA
perform_eda(milk_data, "Monthly Milk Production")
decom_data <- decom(milk_data)
arima_model <- fit_arima(milk_data)
sarima_model <- fit_sarima(milk_data)
com_res <- compare_models(arima_model,sarima_model,h=12,milk_data)

h_milk <- length(com_res$actucal_value)
time_points_milk <- time(milk_data)[(length(milk_data) - h_milk + 1):length(milk_data)]
plot_pred(com_res$actucal_value,
                         as.numeric(com_res$arima_fc),
                         as.numeric(com_res$sarima_fc),
                         time_points_milk)



plot_pred <- function(acutal,arima,sarima,time){
  a_rmse <- sqrt(mean((arima-acutal)^2))
  sa_rmse <- sqrt(mean((sarima-acutal)^2))
  
  better <- ifelse(a_rmse<sa_rmse,"green","red")
  worst <- ifelse(a_rmse<sa_rmse,"red","green")
  plot(time, acutal, type = "o",xlab = "Time", ylab = "Values", main = "Forecast Comparison")
  lines(time, arima, col = better, lty = 2, lwd = 2)
  lines(time, sarima, col = worst, lty = 3, lwd = 2)
}


