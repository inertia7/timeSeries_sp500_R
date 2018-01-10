# FORECASTING THE STOCK MARKET (R) script
# Project and visuals can be found at 
# https://www.inertia7.com/projects/8

#RUN THESE COMMANDS IF THESE THIRD PARTY PACKAGES HAVE NOT BEEN DOWNLOADED YET

# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("plotly")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("docstring")
# install.packages("here")

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
dataMaster <- read.csv(here("data", "data_master_1.csv"))
attach(dataMaster)

# EXPLORATORY ANALYSIS
sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)

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
sp500_TR <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)


plot_time_series(sp_500, 'S&P 500 Training Set')
# Remove comment if you wish to publish plot on ploty
# See GitHub repo for more details
# plotly_POST(timeSeriesPlot, filename = "timeSeriesPlot")

# DECOMPOSING TIME SERIES
sp500_stl <- plot_decomp(sp500_TR, 'S&P 500')

sp500_stl
ggplotly(sp500_stl)

# SEASONAL PLOT 
sp <- plot_seasonal(sp500_TR, 'S&P 500')

sp
ggplotly(sp)

# DIAGNOSING ACF AND PACF PLOTS
plot_acf_pacf(sp500_TR, 'S&P 500')
# TRANSFORMING OUR DATA TO ADJUST FOR NON STATIONARY
diff <- diff(sp_500)

tsDiff <- plot_time_series(diff, 'First Difference')
tsDiff
ggplotly(tsDiff)

# TESTS FOR STATIONARITY FOR DIFFERENCED TIME SERIES OBJECT
Box.test(diff, lag = 20, type = 'Ljung-Box')
adf.test(diff)

# p-values seems small enough to infer stationarity for the first difference
# Let's begin analysis with visually inspecting ACF and PACF plots

# DIAGNOSING ACF AND PACF PLOTS FOR DIFFERENCED TIME SERIES OBJECT
plot_acf_pacf(diff, 'First Difference Time Series Object')

# SEASONAL PLOT FOR DIFFERENCED TIME SERIES OBJECT
spDiff <- plot_seasonal(diff, 'First Difference Time Series Object')

spDiff
ggplotly(spDiff)

# AUTO.ARIMA ESTIMATION
auto.arima(sp500_TR)

# From our visual inspection and auto.arima model we will choose an
# ARIMA(0, 1, 1) with drift 

# BUILD MODEL 
fit <- Arima(sp500_TR, order = c(0,1,1), include.drift = TRUE)
summary(fit)

# RESIDUAL DIAGNOSTICS
ggtsdiag(fit) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 

residFit <- ggplot(data=fit, aes(residuals(fit))) + 
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals") 

residFit

# TEST SET THAT WE WILL COMPARE OUR FORECAST AGAINST 
dataMaster_TS <- dataMaster[-c(1:240), ]
act_sp500_2015_ts <- ts(dataMaster_TS$sp_500, start = c(2015, 1), freq = 12)
act_sp500_2015_ts


# FORECASTING
# METHOD CHOSEN THROUGH BOX JENKINS METHODOLOGY WAS ARIMA(0,1,1) WITH DRIFT
## ARIMA MODEL CHOSEN 
fit_arima <- forecast(fit, h = 12)
saveRDS(fit_arima, file = here("models", 'arima.rds'))

forSp500 <- autoplot(fit_arima, 
                     holdout = act_sp500_2015_ts, 
                     ts_object_name = 'ARIMA')

forSp500
ggplotly(forSp500)
# OTHER TRANSFORMATIONS

## BOX COX TRANSFORMATION
lambda <- BoxCox.lambda(sp500_TR)
fit_sp500_BC <- ar(BoxCox(sp500_TR,lambda))

fit_BC <- forecast(fit_sp500_BC,h=12,lambda=lambda)
saveRDS(fit_BC, file = here("models", 'box_cox.rds'))

s <- autoplot(fit_BC, 
              holdout = act_sp500_2015_ts,
              ts_object_name = 'Box-Cox Transformation')
s
ggplotly(s)

# NEURAL NETWORKS
fit_sp500_net <- nnetar(sp500_TR, lambda = lambda) # Using BC lambda
fit_net <- forecast(fit_sp500_net, h = 12, PI = TRUE)
saveRDS(fit_net, file = here("models", 'neural_net.rds'))
n <- autoplot(fit_net, 
              holdout = act_sp500_2015_ts,
              ts_object_name = 'Neural Networks Forecast')
n
ggplotly(s)

# MEAN FORECAST METHOD
fit_meanf <- meanf(sp500_TR, h = 12)
saveRDS(fit_meanf, file = here("models", 'meanf.rds'))

e <- autoplot(fit_meanf, 
              holdout = act_sp500_2015_ts,
              ts_object_name = 'Mean Forecast') 
e
ggplotly(e)

# NAIVE METHOD
fit_naive <- naive(sp500_TR, h = 12)
saveRDS(fit_naive, file = here("models", 'naive.rds'))
f <- autoplot(fit_naive, 
              holdout = act_sp500_2015_ts,
              ts_object_name = "Naive Forecast") 
f
ggplotly(f)

# SEASONAL NAIVE METHOD
fit_snaive <- snaive(sp500_TR, h = 12)
saveRDS(fit_snaive, file = here("models", 'snaive.rds'))
g <- autoplot(fit_snaive, 
              holdout = act_sp500_2015_ts,
              ts_object_name = "Seasonal Naive")
g
ggplotly(g)  


# EXPONENTIAL SMOOTHING METHOD
fit_ets <- forecast(ets(sp500_TR), h = 12)
saveRDS(fit_ets, file = here("models", 'ets.rds'))
h <- autoplot(fit_ets, 
              holdout=act_sp500_2015_ts,
              ts_object_name = "Exponential Smoothing")

h
ggplotly(h)  

# COMPARE FORECAST ACCURACIES ACROSS DIFFERENT METHODS USED
accuracy(fit_arima)
accuracy(fit_BC)
accuracy(fit_net)
accuracy(fit_meanf)
accuracy(fit_naive)
accuracy(fit_snaive)
accuracy(fit_ets)

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
