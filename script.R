# FORECASTING THE STOCK MARKET (R) script
# Project and visuals can be found at http://www.inertia7.com/projects/time-series-stock-market-r

#RUN THESE COMMANDS IF THESE THRIRD PARTY PACKAGES HAVE NOT BEEN DOWNLOADED YET

# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("astsa")
# install.packages("plotly")
# install.packages("ggfortify")
# install.packages("tseries")


# LOAD YOUR PACKAGES

library(ggplot2)
library(forecast)
library(astsa)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)

# LOAD DATA
dataMaster <- read.csv("/set/appropriate/workingDirectory/data_master_1.csv")
attach(dataMaster)

# EXPLORATORY ANALYSIS
sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)

# TIME SERIES PLOT OF S&P
tsSp <- autoplot(sp_500, 
                 main = "Plot of S & P 500 Time Series(1995-2015)", 
                 ts.colour = "turquoise4", size=1.0) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values")

tsSp
ggplotly(tsSp)


# Here we create the training set where we will compare the values for 2015 
sp500_TR <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)
sp500_TR



# DECOMPOSING TIME SERIES
stl <- autoplot(stl(sp500_TR, s.window = "periodic"),
                main = "Decomposition Plot of S & P 500",
                ts.colour = "turquoise4") +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

stl
ggplotly(stl)

# SEASONAL PLOT
sp <- ggseasonplot(sp500_TR, xlab="Year",
                   main="Seasonal Plot of First Difference of SP 500",
                   year.labels=TRUE, year.labels.left=TRUE, 
                   col=1:20, pch=19) +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 

sp
ggplotly(sp)

# DIAGNOSING ACF AND PACF PLOTS 
a <- autoplot(acf(sp500_TR, plot = FALSE), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  ggtitle("ACF and PACF plots of SP 500")

b <- autoplot(pacf(sp500_TR, plot = FALSE), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + labs(y="PACF") 

grid.arrange(a, b)

# TRANSFORMING OUR DATA TO ADJUST FOR NON STATIONARY
diff <- diff(sp_500)

tsDiff <- autoplot(diff, main = "Time series plot of First Difference", 
                   ts.colour = "turquoise4", size=0.75) + 
  theme(panel.background = element_rect(fill = "gray98"), 
        axis.line.y = element_line(colour="gray"), 
        axis.line.x = element_line(colour="gray")) +
  labs(x = "Year", y = "Differenced Values")

tsDiff
ggplotly(tsDiff)

# DIAGNOSING ACF AND PACF PLOTS FOR DIFFERENCED TIME SERIES OBJECT
c <- autoplot(acf(diff, plot = FALSE), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  ggtitle("ACF and PACF plots of SP 500")

d <- autoplot(pacf(diff, plot = FALSE), 
              conf.int.fill = '#0000FF', 
              conf.int.value = 0.95, conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + labs(y="PACF") 

grid.arrange(c, d)

# SEASONAL PLOT FOR DIFFERENCED TIME SERIES OBJECT
spDiff <- ggseasonplot(diff, xlab="Year", 
                       main="Seasonal Plot of First Difference of S&P 500",
                       year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) 

spDiff
ggplotly(spDiff)

# BUILD MODEL 
fit <- Arima(sp500_TR, order = c(0,1,1), include.drift = TRUE)
summary(fit)

# RESIDUAL DIAGNOSTICS

ggtsdiag(fit) + theme(panel.background = element_rect(fill = "gray98"),
                      panel.grid.minor = element_blank(),
                      axis.line.y = element_line(colour="gray"),
                      axis.line.x = element_line(colour="gray")) 

residFit <- ggplot(data=fit, aes(residuals(fit))) + 
  geom_histogram(aes(y =..density..), 
                 col="black", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of SP 500 ARIMA Model Residuals") 

residFit
ggplotly(residFit)

dataMaster_TS <- dataMaster[-c(1:240), ]
act_sp500_2015_ts <- ts(dataMaster_TS$sp_500, start = c(2015, 1), freq = 12)
act_sp500_2015_ts


# NEXT WE CREATE AN OBJECT THAT WILL GIVE US THE GGPLOT2 OBJECTS WE WANT 
#######################################################################
# HERE FOUND AT http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
# BY DREW SCHMIDT WITH SLIGHT MODIFICATIONS TO FIT OUR PLOTS
#######################################################################


autoplot.forecast <- function(forecast, ..., holdout=NaN){
  # data wrangling
  time <- attr(forecast$x, "tsp")
  time <- seq(time[1], attr(forecast$mean, "tsp")[2], by=1/time[3])
  lenx <- length(forecast$x)
  lenmn <- length(forecast$mean)
  
  df <- data.frame(time=time,
                   x=c(forecast$x, forecast$mean),
                   x2=c(forecast$x, rep(NA, lenmn-length(holdout)), holdout),
                   forecast=c(rep(NA, lenx), forecast$mean),
                   low1=c(rep(NA, lenx), forecast$lower[, 1]),
                   upp1=c(rep(NA, lenx), forecast$upper[, 1]),
                   low2=c(rep(NA, lenx), forecast$lower[, 2]),
                   upp2=c(rep(NA, lenx), forecast$upper[, 2]),
                   holdout=c(rep(NA, lenx+lenmn-length(holdout)), holdout)
  )
  
  ggplot(df, aes(time, x)) +
    geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow", na.rm=TRUE) +
    geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange", na.rm=TRUE) +
    geom_line(data=df, aes(time, x2), color="red")+
    geom_line(colour = "turquoise4", size = 1) +
    geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", na.rm=TRUE) +
    geom_line(data=df[!is.na(df$holdout), ], aes(time, holdout), color="red", na.rm=TRUE) +
    scale_x_continuous("") +
    scale_y_continuous("") 
}


#######################################################################



forSp500 <- autoplot(forecast(fit, h = 12), holdout = act_sp500_2015_ts) +
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values") +
  labs(title = "Plot of 2015 Forecast for S&P 500") 
forSp500
ggplotly(forSp500)

# OTHER TRANSFORMATIONS
lambda <- BoxCox.lambda(sp500_TR)
fit_sp500_BC <- ar(BoxCox(sp500_TR,lambda))
for_sp500_BC <- forecast(fit_sp500_BC,h=12,lambda=lambda)

s <- autoplot(forecast(fit_sp500_BC,h=12,lambda=lambda), holdout = act_sp500_2015_ts) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Box Cox Transformation Forecast Plot of S&P 500")
s
ggplotly(s)

e <- autoplot(forecast(meanf(sp500_TR, h = 12)), holdout = act_sp500_2015_ts) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Mean Forecast Plot of S&P 500")
e
ggplotly(e)

f <- autoplot(forecast(naive(sp500_TR, h = 12)), holdout = act_sp500_2015_ts) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Naive Forecast Plot of S&P 500") 
f
ggplotly(f)

g <- autoplot(forecast(snaive(sp500_TR, h = 12)), holdout = act_sp500_2015_ts) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Seasonal Naive Forecast Plot of S&P 500") 
g
ggplotly(g)  

h <- autoplot(forecast(ets(sp500_TR), h = 12), holdout=act_sp500_2015_ts) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) + 
  labs(x = "Year", y = "Closing Values", 
       title = "Exponential Smoothing Forecast Plot of S&P 500") 
h
ggplotly(h)  


accuracy(forecast(fit, h=12))
accuracy(forecast(fit_sp500_BC, h = 12))
accuracy(meanf(sp500_TR, h = 12))
accuracy(naive(sp500_TR, h = 12))
accuracy(snaive(sp500_TR, h = 12))
accuracy(forecast(ets(sp500_TR), h = 12))

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
i <- autoplot(acf(squared.resARIMA, plot = FALSE), 
              conf.int.fill = '#0000FF', conf.int.value = 0.95, 
              conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  labs(title = "ACF and PACF of S&P 500 Residuals^2")
j <- autoplot(pacf(squared.resARIMA, plot = FALSE), 
              conf.int.fill = '#0000FF', conf.int.value = 0.95, 
              conf.int.type = 'ma') + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  labs(y = "PACF")
grid.arrange(i, j)
# The acf plot shows one significant lag, as does the pacf, but that isn't enough to suggest we need GARCH modeling
gfit <- garch(fit$residuals, order = c(1,1), trace = TRUE)


