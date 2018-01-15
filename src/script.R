# FORECASTING THE STOCK MARKET (R) script
# Project and visuals can be found at 
# https://www.inertia7.com/projects/8

# LOAD YOUR PACKAGES
library(here)
# Rproj should be created before running script
here()
# Output should be:
# "/working/directory/timeSeries_sp500_R"

# Source the helper functions script to load custom function
source(here("src",'helper_functions.R'))

# NOTE: For more information on helper functions use ?function_name
# LOAD DATA
data_master <- read.csv(here("data", "data_master_1.csv"))
attach(data_master)

# EXPLORATORY ANALYSIS
sp_500 <- ts(data_master$sp_500, start=c(1995, 1), freq=12)

# TESTS FOR STATIONARITY
Box.test(sp_500, lag = 20, type = 'Ljung-Box')
adf.test(sp_500)
# p-values are relatively high so we should so visual inspection and
# look at ACF and PACF plots to make appropriate transformation 
# for stationarity. 

# TIME SERIES PLOT OF S&P
tsSp <- plot_time_series(sp_500, 'S&P 500')

tsSp
ggplotly(tsSp)

# Here we create the training set where we will compare the values for 2015 
sp500_training <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)

plot_time_series(sp500_training, 'S&P 500 Training Set')
# Remove comment if you wish to publish plot on ploty
# See GitHub repo for more details
# plotly_POST(timeSeriesPlot, filename = "timeSeriesPlot")

# DECOMPOSING TIME SERIES
sp500_stl <- plot_decomp(sp500_training, 'S&P 500')

sp500_stl
ggplotly(sp500_stl)

# SEASONAL PLOT 
sp <- plot_seasonal(sp500_training, 'S&P 500')

sp
ggplotly(sp)

# DIAGNOSING ACF AND PACF PLOTS
plot_acf_pacf(sp500_training, 'S&P 500')
# TRANSFORMING OUR DATA TO ADJUST FOR NON STATIONARY
sp500_diff <- diff(sp_500)

tsDiff <- plot_time_series(sp500_diff, 'First Difference')
tsDiff
ggplotly(tsDiff)

# TESTS FOR STATIONARITY FOR DIFFERENCED TIME SERIES OBJECT
Box.test(sp500_diff, lag = 20, type = 'Ljung-Box')
adf.test(sp500_diff)

# p-values seems small enough to infer stationarity for the first difference
# Let's begin analysis with visually inspecting ACF and PACF plots

# DIAGNOSING ACF AND PACF PLOTS FOR DIFFERENCED TIME SERIES OBJECT
plot_acf_pacf(sp500_diff, 'First Difference Time Series Object')

# SEASONAL PLOT FOR DIFFERENCED TIME SERIES OBJECT
spDiff <- plot_seasonal(sp500_diff, 'First Difference Time Series Object')

spDiff
ggplotly(spDiff)

# AUTO.ARIMA ESTIMATION
auto.arima(sp500_training)

# From our visual inspection and auto.arima model we will choose an
# ARIMA(0, 1, 1) with drift 

# BUILD MODEL 
fit <- Arima(sp500_training, order = c(0,1,1), include.drift = TRUE)
summary(fit)

# RESIDUAL DIAGNOSTICS
ggtsdiag_custom(fit, 'ARIMA(0,1,1)') + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 

residFit <- ggplot(data=fit, aes(residuals(fit))) + 
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col="turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals") 

residFit

# TEST SET THAT WE WILL COMPARE OUR FORECAST AGAINST 
sp500_test <- window(sp_500, 2015, c(2015, 12))

# FORECASTING
# METHOD CHOSEN THROUGH BOX JENKINS METHODOLOGY WAS ARIMA(0,1,1) WITH DRIFT
## ARIMA MODEL CHOSEN 
fit_arima <- forecast(fit, h = 12)

# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'arima.rds'))){
  saveRDS(fit_arima, file = here("models", 'arima.rds'))
}

forSp500 <- autoplot(fit_arima, 
                     holdout = sp500_test, 
                     ts_object_name = 'ARIMA')

forSp500
ggplotly(forSp500)
# OTHER TRANSFORMATIONS

## BOX COX TRANSFORMATION
lambda <- BoxCox.lambda(sp500_training)
fit_sp500_BC <- ar(BoxCox(sp500_training,lambda))

fit_BC <- forecast(fit_sp500_BC,h=12,lambda=lambda)
ggtsdiag_custom(fit_sp500_BC, 'Box-Cox Transformation (AR(2))')
# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'box_cox.rds'))){
  saveRDS(fit_BC, file = here("models", 'box_cox.rds'))
}

s <- autoplot(fit_BC, 
              holdout = sp500_test,
              ts_object_name = 'Box-Cox Transformation')
s
ggplotly(s)

# NEURAL NETWORKS
fit_sp500_net <- nnetar(sp500_training, lambda = lambda) # Using BC lambda
fit_net <- forecast(fit_sp500_net, h = 12, PI = TRUE)

# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'box_cox.rds'))){
  saveRDS(fit_net, file = here("models", 'neural_net.rds'))
}

n <- autoplot(fit_net, 
              holdout = sp500_test,
              ts_object_name = 'Neural Networks Forecast')
n
ggplotly(s)

# MEAN FORECAST METHOD
fit_meanf <- meanf(sp500_training, h = 12)

# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'meanf.rds'))){
  saveRDS(fit_meanf, file = here("models", 'meanf.rds'))
}

e <- autoplot(fit_meanf, 
              holdout = sp500_test,
              ts_object_name = 'Mean Forecast') 
e
ggplotly(e)

# NAIVE METHOD
fit_naive <- naive(sp500_training, h = 12)

# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'naive.rds'))){
  saveRDS(fit_naive, file = here("models", 'naive.rds'))
}

f <- autoplot(fit_naive, 
              holdout = sp500_test,
              ts_object_name = "Naive Forecast") 
f
ggplotly(f)

# SEASONAL NAIVE METHOD
fit_snaive <- snaive(sp500_training, h = 12)

# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'snaive.rds'))){
  saveRDS(fit_snaive, file = here("models", 'snaive.rds'))
}

g <- autoplot(fit_snaive, 
              holdout = sp500_test,
              ts_object_name = "Seasonal Naive")
g
ggplotly(g)  


# EXPONENTIAL SMOOTHING METHOD
fit_ets <- forecast(ets(sp500_training), h = 12)

# Will download the rds file only if its not present in the models directory 
if (is.null(here("models", 'ets.rds'))){
  saveRDS(fit_ets, file = here("models", 'ets.rds'))
}

h <- autoplot(fit_ets, 
              holdout=sp500_test,
              ts_object_name = "Exponential Smoothing")

h
ggplotly(h)  


# COMPARE FORECAST ACCURACIES ACROSS DIFFERENT METHODS USED
round(accuracy(fit_arima, sp500_test), 3)
round(accuracy(fit_BC, sp500_test), 3)
round(accuracy(fit_net, sp500_test), 3)
round(accuracy(fit_meanf, sp500_test), 3)
round(accuracy(fit_naive, sp500_test), 3)
round(accuracy(fit_snaive, sp500_test), 3)
round(accuracy(fit_ets, sp500_test), 3)

# CONCLUSIONS
# The model with the best diagnostics is our ARIMA Model 

# ARCH Modeling
# Here we first square the residuals and plot the time series/ACF/PACF 
# to see if there is correlation within the residuals.
# If there is we can continue adding on to our ARIMA model with a gARCH 
# aspect that helps in the volatity of our data.
squared_res_fit <- fit$residuals^2

sq_res <- plot_time_series(squared_res_fit, "Squared Residuals")

sq_res
ggplotly(sq_res)

# ACF AND PACF PLOT FOR SQUARED RESIDUALS 
plot_acf_pacf(squared_res_fit, 'S&P 500 Residuals^2')
# The acf plot shows one significant lag, as does the pacf, 
# but that isn't enough to suggest we need GARCH modeling
gfit <- garch(fit$residuals, order = c(1,1), trace = TRUE)
