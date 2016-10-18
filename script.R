#######
#######
#######
#######
#######
#######
#######
####### Time Series Model for S&P 500
#######
#######
#######
#######
#######
#######
#######



##
####
######
#     LOADING DATA, PACKAGES
######
####
##

### REMEMBER YOU WILL NEED TO RUN THE 'multiplot' before running this script
print("Time Series Model for S&P 500")
# Add the directory path of data_1/data_master_1.csv file
wd <- getwd()
parent <- getwd()
setwd(wd)
print(parent)
# Load the data file
dataMaster <- read.csv(file.path(parent, "/data_master_1.csv"))

attach(dataMaster)
# This next step is if you want to publish the images in plotly! 
# So if you don't you can disregrad these two following lines
Sys.setenv("plotly_username"="userName") # For plotly credentials and publishing 
Sys.setenv("plotly_api_key"="passWord") # DO NOT POST IN GITHUB

# install.packages() the following packages, run this on the terminal

# ggplot2
# forecast
# astsa
# plotly
# ggfortify
# tseries


# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("astsa")
# install.packages("plotly")
# install.packages("ggfortify")
# install.packages("tseries")

# load the packages
require(ggplot2)
require(forecast)
require(astsa)
require(plotly)
require(ggfortify)
require(tseries)

# outputting work

pdf("timeSeries_sp_500.pdf")


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     TESTING that data loads properly
######
####
##

head(dataMaster)
str(dataMaster)



###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     CREATING TIME-SERIES OBJECTS WITH FINANCIAL DATA 
######
####
##


sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)



###########################################################
print(" ")
print(" ")
print(" ")
###########################################################



##
####
######
#     TIME SERIES STUFF
######
####
##

sp_500
# We create the time series object that ends at 2014. 
# We do this to predict 2015 and compare to see how our models did!
sp500_TR <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)
sp500_TR
# First we plot the time series plot to get an understanding of the necessary modeling
ts <- autoplot(sp_500, main = "Plot of S&P 500 Time Series(1995-2015)", 
               ts.colour = "turquoise4", size=0.75) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.ticks  = element_blank(),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values")
ts
ggplotly(ts)
# The next line is to publish the ggplot time series object to your plotly account!
# plotly_POST(ts, filename = "sp_500_timeSeries")
# NOTE: I will not include this after every ggplotly object but if you are going to 
# publish you're plots on your plotly account run plotly_POST after every ggplotly 
# object

# Next we plot the acf and pacf to get a picture of our current time series object
# Which is pretty obvious that its not stationary now but we still will plot it
# ggplot2 for acf and pacf
a <- autoplot(acf(sp500_TR, plot = FALSE), conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(title = "ACF and PACF plots of S&P 500")

b <- autoplot(pacf(sp500_TR, plot = FALSE), conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + labs(y="PACF") 

# The next function was borrowed from cookbook-r which allows us to plot two ggplot objects!
# You must run the multiplot script before this script or else it will not output the acf and pacf plot
multiplot(a, b, cols = 1) # Grabbed from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  

# Next we created some plots to get a "feel" for our data 
# This next plot takes a closer look at the seasonal components of our time series
# Which if we lack seasonality we should see no distinct pattern! 
# NOTE: If you want see when this plot is useful, use the AirPassenger (available in the data sets package)
# ref: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/AirPassengers.html
# Here's what I did:
# tsAir <- diff(log(AirPassengers))
# sp <- ggseasonplot(tsAir, xlab="Year", main="Seasonal Plot of First Difference of S&P 500",
# 	year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) +
# theme(panel.background = element_rect(fill = "gray98"),
# 	axis.line.y = element_line(colour="gray"),
# 	axis.line.x = element_line(colour="gray")) 
# That plot will give you a good visual of seasonality!
# sp 
sp <- ggseasonplot(sp500_TR, xlab="Year", 
	main="Seasonal Plot of First Difference of S&P 500",
	year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) +
theme(panel.background = element_rect(fill = "gray98"),
 	axis.line.y = element_line(colour="gray"),
 	axis.line.x = element_line(colour="gray")) 
sp
ggplotly(sp)
# There's not much seasonality for this time series so this plot doesn't give us much help but it can helpful 
# for other time series objects with a lot of seasonality as stated earlier

# SEASONAL DECOMPOSITION: following plot decomposes the time series into its seasonal, trend and irregular components!
stl <- autoplot(stl(sp500_TR, s.window = "periodic"), 
	main = "Decomposition Plot of S&P 500", ts.colour = "turquoise4") +
	 theme(panel.background = element_rect(fill = "gray98"),
	 	axis.line.y   = element_line(colour="gray"),
	 	axis.line.x = element_line(colour="gray")) 
stl
ggplotly(stl)
# Notice that this plot is not stationary so an appropriate transformation must be made
# The variability can not be seen at first glance but one the transformation is made, we can see if the model
# is heteroskedastic
diff <- diff(sp500_TR)

# Since we took a difference we have to take it into consideration
tsDiff <- autoplot(diff, main = "Time series plot of First Difference", 
                   ts.colour = "turquoise4", size=0.75) + 
  theme(panel.background = element_rect(fill = "gray98"), 
        axis.line.y = element_line(colour="gray"), 
        axis.line.x = element_line(colour="gray")) +
  labs(x = "Year", y = "Differenced Values")

tsDiff

# Let's check the seasonal plot of our differenced time series to make sure there is no seasonality
spDiff <- ggseasonplot(diff, xlab="Year", main="Seasonal Plot of First Difference of S&P 500",
                   year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 
spDiff
ggplotly(spDiff)
# This plot shows that the differenced time series shows no clear pattern so therefore no seasonality
# So our next step is to plot the ACF and PACF for our differenced time series since it showed us 
# non-stationary patterns which we can then deduce a Box-Jenkins ARMA/ARIMA/SARIMA model
c <- autoplot(acf(diff, plot = FALSE),
	conf.int.fill = '#0000FF', conf.int.value = 0.95, 
	conf.int.type = 'ma', ts.colour = "turquoise4") + 
theme(panel.background = element_rect(fill = "gray98"),
	axis.line.y   = element_line(colour="gray"),
	axis.line.x = element_line(colour="gray")) +
  labs(title = "ACF and PACF of S&P 500 (First Difference)")

d <- autoplot(pacf(diff, plot = FALSE), 
	conf.int.fill = '#0000FF', conf.int.value = 0.95, 
	conf.int.type = 'ma', ts.colour = "turquoise4") + 
theme(panel.background = element_rect(fill = "gray98"),
	panel.grid.minor = element_blank(),
	axis.line.y   = element_line(colour="gray"),
	axis.line.x = element_line(colour="grey80")) + 
labs(y = "PACF")

multiplot(c, d, cols = 1)  # Grabbed from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# We estimate the model as an MA(1) but since its a differenced time series
# We end up with an ARIMA(0, 1, 1)
# We used both the auto.arima and our own inspection of the acf and pacf plot to deduce that the best model was Arima(0,1,1)

auto.arima(sp500_TR)
fit <- Arima(sp500_TR, order = c(0,1,1), include.drift = T)
fit

# Thus once fitted we check the residual diagnostics to make sure our residuals are white noise!
residFit <- ggtsdiag(fit) + theme(panel.background = element_rect(fill = "gray98"),
                           panel.grid.minor = element_blank(),
                           axis.line.y   = element_line(colour="gray"),
                           axis.line.x = element_line(colour="gray")) 
residFit
# We can see that the residuals are pretty good! We plot the residuals as a bar plot to add on to the
# analysis of the residuals
residBar <- ggplot(data=fit, aes(residuals(fit))) + 
  geom_histogram(aes(y =..density..), 
                 col="black", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  labs(title = "Plot of S&P 500 ARIMA Model Residuals") 
residBar
ggplotly(residBar)
# declaring asp500_ACT vector with actual sp500 values for year 2015, for comparison purposes
sp500_for <- forecast(fit, 12, level = 95) 

forSp500 <- autoplot(sp500_for) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values") +
  labs(title = "Plot of 2015 Forecast for S&P 500") 
forSp500
ggplotly(forSp500)

##
####
######
#     OTHER TRANSFORMATIONS
######
####
##



lambda <- BoxCox.lambda(sp500_TR)
fit_sp500_BC <- ar(BoxCox(sp500_TR,lambda))
fit_sp500_BC
#Creating the predicted values for the Box Cox model for 2015
s <- autoplot(forecast(fit_sp500_BC,h=12,lambda=lambda, level = 95), 
              ts.colour = "turquoise4", size=1) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Box Cox Transformation Forecast Plot of S&P 500")
s
ggplotly(s)
# Conclusions: Box Cox transformations are usually done with data that is heteroskedastic so the forecast didn't perform as well as the ARIMA model, but we wanted to include it just in case
# anyone wants to use our methodology with data that has a non-constant variance!
# Here we're plotting other forecasts that aren't as good predictors for this data so we are keeping them as simple plots the same steps would be followed as done before if you wanted a 
# detailed plot of these methods
e <- autoplot(forecast(meanf(sp500_TR, h = 12, level = 95)), 
              ts.colour = "turquoise4", size=1) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Mean Forecast Plot of S&P 500")
e
ggplotly(e)

f <- autoplot(forecast(naive(sp500_TR, h = 12, level = 95)), ts.colour = "turquoise4", size=1) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Naive Forecast Plot of S&P 500") 

f
ggplotly(f)

g <- autoplot(forecast(snaive(sp500_TR, h = 12, level = 95)), ts.colour = "turquoise4", size=1) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Seasonal Naive Forecast Plot of S&P 500") 
ggplotly(g)  

h <- autoplot(forecast(ets(sp500_TR), h = 12, level = 95), ts.colour = "turquoise4", size=1) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Exponential Smoothing Forecast Plot of S&P 500") 
ggplotly(h)  

# For shits and giggles we plot 60 months ahead and see what our model predicts for the next 5 years! 
# It predicts an upward trend so we'll see in 5 years how well our prediction did...
autoplot(forecast(auto.arima(sp_500), h = 60, level = 95)) + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values",
       title = "Five Year ARIMA Forecast")
###Measuring accuracy between models 
accuracy(sp500_for)
accuracy(forecast(fit_sp500_BC, h = 12))
accuracy(meanf(sp500_TR, h = 12))
accuracy(naive(sp500_TR, h = 12))
accuracy(snaive(sp500_TR, h = 12))
accuracy(forecast(ets(sp500_TR), h = 12))

# Thus we concluded that the ARIMA model produced the best forecast!

# After concluding our basic time series analysis, upon further research I were concerned with the issue of volitaty since our data seemed to 
# to display this. So I learned about the model that is called Autoregressive Conditional Heteroskedasticity (ARCH). Upon reading online documentation 
# (listed in the resources!) there is risk of volatility clustering which is especially prevalent in financial time series. The steps required to 
# see if ARCH was necessary are outlined here!!

# ARCH Modeling
# Here we first square the residuals and plot the time series/ACF/PACF to see if there is correlation within the residuals.
# If there is we can continue adding on to our ARIMA model with a gARCH aspect that helps in the volatity of our data.
squared.resARIMA <- fit$residuals^2
sqRes <- autoplot(squared.resARIMA, main = "Plot of Squared Residuals", ts.colour = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))
sqRes
ggplotly(sqRes)
e <- autoplot(acf(squared.resARIMA, plot = FALSE), 
              conf.int.fill = '#0000FF', conf.int.value = 0.95, 
              conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  labs(title = "ACF and PACF of S&P 500 Residuals^2")
f <- autoplot(pacf(squared.resARIMA, plot = FALSE), 
              conf.int.fill = '#0000FF', conf.int.value = 0.95, 
              conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  labs(y = "PACF")
multiplot(e, f, cols = 1)
# The acf plot shows one significant lag, as does the pacf, but that isn't enough to suggest we need GARCH modeling
# The plots indicate that there is no correlation so a gARCH model might not be necessary, but I went ahead and made the model to be sure!
# Using the tseries package I fitted the basic gARCH(1, 1) model which I didn't conclude was the best, but it is the most commonly used mode. 
# So I fit the model on the residuals!! Not the original time series, not the differenced time series, but the residuals. Then I let TRACE 
# be TRUE so that I would be able to see if there was any errors in the estimation of the model, which there was!!!
# Sources online state that if there reads a line ***** FALSE CONVERGENCE ***** then we should be cautious of the statistical significance of the model
# Thus I concluded from this and the ACF/PACF of the squared residuals that a gARCH model was not need, but it was still interesting and useful to know 
# or future reference!!! 
