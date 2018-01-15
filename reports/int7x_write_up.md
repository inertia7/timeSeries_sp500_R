# Load Packages

First, we want to load the appropriate packages into our **R** environment.

For this we use the `library()` method and include the package names as arguments.

	# LOAD YOUR PACKAGES

	library(ggplot2)
	library(forecast)
	library(astsa)
	library(plotly)
	library(ggfortify)
	library(tseries)
	library(gridExtra)

An error message in this format may appear:

	library(_____)
	Error in library(_____) : there is no package called (_____)

If this error appears, it means that you haven't installed the packages yet. Make sure to install the packages using the `install.packages()` method.

## Installing Packages

	#RUN THESE COMMANDS IN YOUR CONSOLE

	install.packages(""ggplot2"")
	install.packages(""forecast"")
	install.packages(""astsa"")
	install.packages(""plotly"")
	install.packages(""ggfortify"")
	install.packages(""tseries"")
	install.packages(""gridExtra"")

We encourage you to do a quick search on each of these packages to gain context on what they are useful for, and why we are using them here. Pay special attention to the packages `astsa` and `forecast`.

# Get Data
Now we collect our data. We want to use reliable sources of complete and accurate data. We collected *21 years* (1995-2015) of **S&P 500 Stock Index** data at a monthly frequency (a total of 252 observations) from [Yahoo Finance](https://finance.yahoo.com/quote/%5EGSPC/history?period1=788947200&period2=1489820400&interval=1mo&filter=history&frequency=1mo). You can do the same too. We chose to use the Adjusted Closing Value for our analysis.

## Cleaning Data
We cleaned our data manually using *Google Sheets*. For this we made sure that all fields contained data (i.e. no missing values) and that the headers (i.e. column names) were labeled appropriately. This can be done programmatically with **R**, but this is outside of the scope of this project.

We also included a bunch of other variables (for exploratory analysis) such as the **NASDAQ Stock Index**, the **NYSE Stock Exchange**, the **U.S. Housing Price Index**, and the **U.S. Gross Domestic Product**. Here is our data in file [data_master_1.csv](https://github.com/inertia7/timeSeries_sp500_R/blob/master/data_master_1.csv), which is ready to be used with **R**.

## Loading Data
Then we must include our data set within our working R environment. For this we use:

	dataMaster <- read.csv(""/set/appopriate/working/directory/data_master_1.csv"")
	attach(dataMaster)

Now we can call our **S&P 500 Stock Index** data by typing `dataMaster$sp_500` into our terminal.

# Exploratory Analysis
Now we want to get a feel for our data to get an intuition about the models that may be appropriate for our forecast. For this, we plot our data and diagnose for *trend*, *seasonality*, *heteroskedasticity*, and *stationarity*. We go over these concepts in further detail in this section.

## Creating time-series data object
Our **S&P 500 Stock Index** data is in the form of time-series; this means that our data exists over a continuous time interval with equal spacing between every two consecutive measurements. In **R** we are able to create time-series objects for our data vectors using the `ts()` method. For this, we select the vector we would like to use as the first argument, and tune the `start` and `freq` (frequency) parameters. Then we output the time-series data to the terminal by calling our newly-created time-series object.

	sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)

Before we do anything, we need to plot our time series object `sp_500`. We do this using the `autoplot()` function in `ggplot2` which we will be using extensively in this project

	tsSp <- autoplot(sp_500,
    	main = ""Plot of S & P 500 Time Series(1995-2015)"",
    	ts.colour = ""turquoise4"", size=1.25) +
	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line   = element_line(colour=""black""),
        	axis.line.x = element_line(colour=""gray""),
        	axis.line.y = element_line(colour=""gray"")) +
    	labs(x = ""Year"", y = ""Closing Values"")

	tsSp
	ggplotly(tsSp)


<iframe width="100%" height=415  frameborder="0" scrolling="no" src=https://plot.ly/~raviolli77/55.embed?autosize=True&width=90%&height=100%></iframe>

However, we are going to remove the year 2015 in order to compare actual values and predicted values.

	# Here we create the training set where we will compare the values for 2015
	sp500_TR <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)
	sp500_TR

### Terminal Output
<img src="http://i.imgur.com/IVIGxNF.png"">

## Plotting our Time Series


Plotting the data is arguably the most critical step in the exploratory analysis phase (We chose to emphasize on the time series object that has intervals from *1995* to *2014*, which we will explain later!). This enables us to make inferences about important components of the time-series data, such as *trend*, *seasonality*, *heteroskedasticity*, and *stationarity*. Here is a quick summary of each:

+ **Trend**: we say that a dataset has a trend when it has either a *long-term increase* or *decrease*.
+ **Seasonality**: we say that a dataset has seasonality when it has patterns that repeat over known, fixed periods of time (e.g. monthly, quarterly, yearly).
+ **Heteroskedasticity**: we say that a data is *heteroskedastic* when its variability is not constant (i.e. its variance increases or decreases as a function of the explanatory variable).
+ **Stationarity**: a stochastic process is called *stationary* if the mean and variance are constant (i.e. their joint distribution does not change over time).


We can plot our `sp_500` data object in **R** using the `autoplot()` method. So again we will be using the `sp500_TR` for our analysis.

Therefore we start our analysis by plotting our time series object to give us a visual basis to start our modeling.


	ts <- autoplot(sp500_TR,
        	main = ""Plot of S & P 500 Time Series(1995-2014)"",
        	ts.colour = ""turquoise4"", size=1) +
	theme(panel.background = element_rect(fill = ""gray98""),
            	axis.ticks  = element_blank(),
            	axis.line   = element_line(colour=""black""),
            	axis.line.x = element_line(colour=""gray""),
            	axis.line.y = element_line(colour=""gray"")) +
	labs(x = ""Year"", y = ""Closing Values"")

	ts
	ggplotly(ts)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/31.embed"></iframe>

We can quickly see that our time-series has instances of both positive and negative trend. Overall, it is very volatile, which tells us that we will have transform the data in order for the **Box-Jenkins Methodology** to predict with better accuracy.

## Testing for Stationarity
We will utilize a few statistical tests to test for stationarity.

	Box.test(sp_500, lag = 20, type = 'Ljung-Box')

### Terminal Output

	> Box.test(sp500_TR, lag = 20, type = 'Ljung-Box')

		Box-Ljung test

	data:  sp500_TR
	X-squared = 2024.8, df = 20, p-value < 2.2e-16

Now we will utilize the **Augmented Dickey-Fuller Test** for stationarity.

	adf.test(sp_500)

### Terminal Output

	> adf.test(sp500_TR)

		Augmented Dickey-Fuller Test

	data:  sp500_TR
	Dickey-Fuller = -1.7877, Lag order = 6, p-value = 0.6652
	alternative hypothesis: stationary

We can see our p-value for the ADF test is relatively high, so we'll do some further visual inspection. But we know we will most likely have to difference our time series for stationarity.


## Decomposing our time-series

Beyond understanding the *trend* of our time-series, we want to further understand the anatomy of our data. For this reason we break-down our time-series into its *seasonal component*, *trend*, and *residuals*.

	stl <- autoplot(stl(sp500_TR, s.window = ""periodic""),
    	main = ""Decomposition Plot of S & P 500"",
    	ts.colour = ""turquoise4"") +
	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y   = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray""))

	stl
	ggplotly(stl)

<iframe width="100%" height=600  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/35.embed?autosize=True&width=90%&height=100%"></iframe>

## Seasonal Plot
Although not as relevant for this data set, which will be ironically explained through this graph, the seasonal plot can provide a good visual of seasonality for time series objects. This can be best illustrated through a commonly used time series dataset: [AirPassenger](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/AirPassengers.html) (which requires knowledge of *stationary* and *heteroskadasicty* to transform the data set appropriately).

But if you run the seasonal plot for this function, you can see it follows a certain season pattern. I provided the code right here but will not provide visuals since its not apart of this project. You can run it yourself to get a picture of what I'm saying

## Sample code for AirPassenger data set:

	tsAir <- diff(log(AirPassengers))
	sp <- ggseasonplot(tsAir,
    	xlab=""Year"",
    	main=""Seasonal Plot of First Difference of S & P 500"",
    	year.labels=TRUE,
    	year.labels.left=TRUE, col=1:20, pch=19) +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray""))


Going back to our data set we run the same function to grab a visual diagnosis for the seasonality in our data set.

	sp <- ggseasonplot(sp500_TR, xlab=""Year"",
    	main=""Seasonal Plot of First Difference of S&P 500"",
    	year.labels=TRUE, year.labels.left=TRUE,
    	col=1:20, pch=19) +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray""))

	sp
	ggplotly(sp)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/53.embed?autosize=True&width=90%&height=100%"></iframe>

However upon inspection there is no indication of any seasonal patterns outside of the later years showing upward trend, thus indicating *non-stationarity*.

# Model Estimation
## Diagnosing the ACF and PACF Plots of our Time-Series Object

*ACF* stands for ""autocorrelation function"" and *PACF* stands for ""partial autocorrelation function"". The *ACF* and *PACF* diagnosis is employed over a time-series to determine the order for which we are going to create our model using *ARIMA* modeling. Loosely speaking, a time-series is *stationary* when its mean, variance, and *autocorrelation* remain constant over time.

These functions help us understand the correlation component of different data points at different time *lags*. *Lag* refers to the time difference between one observation and a previous observation in a dataset. When there is large autocorrelation within our lagged values, we see geomtric decay in our plots, which is a huge indicator that we will have to take the difference of our time series object. Let's examine our plots!

To carry out our *ACF* and *PACF* diagnosis in R we use the the `grid.arrange()` method, which we referenced from the `gridExtra` package in **R**. We include our time-series object as the argument.

	a <- autoplot(acf(sp500_TR, plot = FALSE),
    	conf.int.fill = '#0000FF',
    	conf.int.value = 0.95, conf.int.type = 'ma') +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y   = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray"")) +
	ggtitle(""ACF and PACF plots of S&P 500"")

	b <- autoplot(pacf(sp500_TR plot = FALSE),
        	conf.int.fill = '#0000FF',
        	conf.int.value = 0.95, conf.int.type = 'ma') +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y   = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray"")) + labs(y=""PACF"")

	grid.arrange(a, b)

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf.png">

## Transforming our data to adjust for non-stationary

From visual inspection of the time series object and the other graphs used for exploratory purposes we decided it is appropriate to difference our time series object to account for the *non-stationarity* and see how that fares!

A way to make a time-series *stationary* is to find the difference across its consecutive values. This helps stabilize the mean, thereby making the time-series object stationary.

For this we use the `diff()` method.

	diff <- diff(sp_500)


Next we plot our transformed time-series using the `autoplot()` method.

	tsDiff <- autoplot(diff,
    	main = ""Time series plot of First Difference"",
    	ts.colour = ""turquoise4"", size=0.75) +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray"")) +
	labs(x = ""Year"", y = ""Differenced Values"")

	tsDiff
	ggplotly(tsDiff)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/37.embed?autosize=True&width=90%&height=100%"></iframe>

This plot suggests that our working data is stationary. We want to confirm this running an *ACF* and *PACF* diagnostics over this data to find our if we can proceed to estimating a model.

## Testing for Stationarity
We apply the same tests to our differenced time series object.

	> Box.test(diff, lag = 20, type = 'Ljung-Box')

	Box-Ljung test

	data:  diff
	X-squared = 58.2, df = 20, p-value = 1.347e-05

Now let's use the ADF Test

	> adf.test(diff)

	Augmented Dickey-Fuller Test

	data:  diff
	Dickey-Fuller = -4.9552, Lag order = 6, p-value = 0.01
	alternative hypothesis: stationary

	Warning message:
	In adf.test(diff) : p-value smaller than printed p-value

Upon reading this [stackoverflow](https://stats.stackexchange.com/questions/142003/adf-test-results-confusion) post over the cryptic warning message, we can see that the result yields a small p-value which makes us reject the null suggestion stationarity.

## Diagnosing the acf and pacf of our transformed time-series object

The plot below helps us confirm that we have stationarity and also helps us deduce which model we will use. It is important to keep in mind that we have a difference parameter equal to one (i.e. *d = 1*) because of the previous transformation we carried out.

	c <- autoplot(acf(diff, plot = FALSE),
    	conf.int.fill = '#0000FF',
    	conf.int.value = 0.95, conf.int.type = 'ma') +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y   = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray"")) +
	ggtitle(""ACF and PACF plots of S&P 500"")

	d <- autoplot(pacf(diff, plot = FALSE),
    	conf.int.fill = '#0000FF',
    	conf.int.value = 0.95, conf.int.type = 'ma') +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y   = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray"")) + labs(y=""PACF"")

	grid.arrange(c, d)

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf_diff.png">

From the above plots we deduce that an *MA(1)* model (where *MA* stands for **moving average**) best fits our data because the *ACF* cuts off at one significant lag and the *PACF* shows geometric decay.

Recall that we are examining the differenced time-series so we have to use the combined model *ARIMA* (**Autoregressive integrated moving average**), thus our model so far is *ARIMA(0, 1, 1)*.

## Seasonal Plot for Transformed Time Series Object

Now that we have a stationary time series object, we again make a seasonal plot which this time shows us that there is no indicative visual seasonal pattern provided by the following code:


	spDiff <- ggseasonplot(diff, xlab=""Year"",
    	main=""Seasonal Plot of First Difference of S & P 500"",
    	year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray""))

	spDiff
	ggplotly(spDiff)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/57.embed?autosize=True&width=90%&height=100%"></iframe>

Just to reiterate, this plot shows us there is no clear seasonal pattern. Therefore, we can continue and assume that our differenced time series object meets the criteria for the *Box-Jenkins model* estimation.

# Build Model
Our findings in the exploratory analysis phase suggest that model *ARIMA(0, 1, 1)* might be best fit. Fortunately, there is a function in **R** that we can use to test our findings.

The `auto.arima()` method, found within the `forecast` package, yields the best model for a time-series based on **Akaike-Information-Criterion** (*AIC*). The *AIC* is a measurement of quality used across various models to find the best fit. After running our original and differenced data sets through the `auto.arima()` method we confirmed that the *ARIMA(0, 1, 1)* is our best fit model.

We use the `Arima()` method to fit our model and include our training data set `sp500_TR` as the first argument.


	fit <- Arima(sp500_TR, order = c(0,1,1),
    	include.drift = TRUE)
	summary(fit)

Here's the summary of our model (using the `summary()` method):

### Terminal Output

	> summary(fit)
	Series: sp500_TR
	ARIMA(0,1,1) with drift


	Coefficients:
         	ma1   drift
      	0.5666  6.4975
	s.e.  0.0551  3.4326


	sigma^2 estimated as 1161:  log likelihood=-1181.58
	AIC=2369.17   AICc=2369.27   BIC=2379.59


	Training set error measures:
                      	ME     RMSE      MAE
	Training set -0.00911296 33.85289 24.84955
                      	MPE     MAPE      MASE
	Training set -0.00840343 2.141218 0.1310854
                    	ACF1
	Training set -0.01137429

Our next step is to run a residual diagnostics to ensure our residuals are white noise under our initial assumptions. For this we use the `ggtsdiplay()` method.

	ggtsdisplay(fit) +
	theme(panel.background = element_rect(fill = ""gray98""),
      	axis.line.y   = element_line(colour=""gray""),
      	axis.line.x = element_line(colour=""gray""))

<img src="http://imgur.com/Uv1EEO1.png"">

A histogram gives us another visualization of the normal distribution of our residuals.

	residFit <- ggplot(data=fit, aes(residuals(fit))) +
    	geom_histogram(aes(y =..density..),
    	col=""black"", fill=""white"") +
	geom_density(col=1) +
	theme(panel.background = element_rect(fill = ""gray98""),
    	panel.grid.minor = element_blank(),
    	axis.line   = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray"")) +
	ggtitle(""Plot of S&P 500 ARIMA Model Residuals"")

	residFit
	ggplotly(residFit)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/39.embed?autosize=True&width=90%&height=100%"></iframe>

Although not perfect we can see that the residuals do display a *normal distribution*. The outliers may be explained by the *2008 financial crisis*.

This model appears to confirm all of our assumptions, which means we can continue to the forecasting phase!

# Forecasting
We proceed to forecasting now that we believe we found the appropriate model!

So there are still limitations with respect to the newest updates of the `forecast` package with respect to `ggplot2` objects. We utilized the `autoplot()` function quite heavily on this iteration of our project, since we couldn't find a way of adding the actual values to the plot we used a workaround by borrowing [Drew Schmidt's](http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/) work around to including the actual 2015 values. The code can be found in the github repository, so you should run the `autoplot.forecast()` inorder to get the plots we have here. For now we create a time series object that will include the actual 2015 values, where in our function it will be called up by the parameter `holdout`.

	dataMaster_TS <- dataMaster[-c(1:240), ]
	act_sp500_2015_ts <- ts(dataMaster_TS$sp_500, start = c(2015, 1),
    	freq = 12)

	act_sp500_2015_ts

Next we use the `forecast` function (that we updated thanks to *Drew Schmidt*), `ggplot2` and `plotly` to visualize the predictions for the year 2015! Here within the plots the forecasted values are **BLUE**, the actual 2015 values are in **RED**, the 80% Confidence Intervals are encompassed in the **YELLOW** bands and 95% *Confidence Intervals* are encompassed in the **ORANGE** bands respectively.

	for_sp500_all <- forecast(fit, h = 32)

Next we create the autoplot visualization.

	forSp500 <- autoplot(forecast(fit, h = 32),
    	holdout = act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y   = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"") +
  	labs(title = ""Plot of 2015 Forecast for S & P 500"")
	forSp500

	ggplotly(forSp500)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/41.embed?autosize=True&width=90%&height=100%"></iframe>

We can see that the model performs well and within the 80% and 95% confidence intervals. You can forecast values even further into the future by tuning the appropriate parameters. Please not that this forecast project is for educational purposes and **we do not recommend investing by using these predictions** - remember that the stock market is very volatile.

# Other Forecasting Methods

So in this more interactive iteration of our project we included other forecasting methods to show the versatility of forecasting methods and we love how the `ggplotly()` graphs look!

## Box-Cox Forecast


*Box-Cox transformations* are generally used to transform non-normally distributed data to become approximately normal! Although we do not think this an appropriate transformation for our data set, we still included it because it's a useful transformation to do especially since most real time data is not approximately *normally distributed*.

	lambda <- BoxCox.lambda(sp500_TR)
	fit_sp500_BC <- ar(BoxCox(sp500_TR,lambda))
	for_sp500_BC <- forecast(fit_sp500_BC,h=32,lambda=lambda)

Now that we have created the forecast object we plot the prediction!

	s <- autoplot(forecast(fit_sp500_BC,h=32,lambda=lambda),
        	holdout = act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"",
       	title = ""Box Cox Transformation Forecast Plot of S & P 500"")
	s
	ggplotly(s)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/51.embed?autosize=True&width=90%&height=100%"></iframe>


## Neural Networks
More later

	fit_net <- nnetar(sp500_TR, lambda = lambda) # Using BC lambda

Now we plot

	n <- autoplot(forecast(fit_net,h=32,lambda=lambda, PI = TRUE),
        	holdout = act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"",
       	title = ""Neural Network Transformation Forecast Plot of S & P 500"")
	n
	ggplotly(n)


<iframe width="100%" height=415 frameborder="0" scrolling="no" src="//plot.ly/~raviolli77/130.embed?autosize=True&width=90%&height=100%"></iframe>

## Mean Forecast
For most of these forecasting methods they are best explained by Rob J. Hydnman (the man who created most of these time series packages) I will just be iterating what he has already said for simple forecasting methods found [here](https://www.otexts.org/fpp/2/3).

The forecasting methods are useful to keep in mind because you might conclude that your time series object might not even require some complex algorithm. We begin with the average method; with the `meanf()` function we are essenntially forecasting values based upon the mean of the historical data!

	e <- autoplot(forecast(meanf(sp500_TR, h = 32)),
        	holdout = act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"",
        	title = ""Mean Forecast Plot of S & P 500"")
	e
	ggplotly(e)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/43.embed?autosize=True&width=90%&height=100%"></iframe>

## Naive Forecast

From the naive description I gathered that the *naive forecasting* method returns an *ARIMA(0, 1, 0) with random walk* model that is applied to our time series object. Important to note that Hydman described this forecasting method as being effective in financial time series objects!

	f <- autoplot(forecast(naive(sp500_TR, h = 32)),
        	holdout = act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"",
        	title = ""Naive Forecast Plot of S & P 500"")
	f
	ggploty(f)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/45.embed?autosize=True&width=90%&height=100%"></iframe>

## Seasonal Naive Forecast

For the `snaive()` method it follows the same principles as the *naive* method, but works better for very seasonal data!


	g <- autoplot(forecast(snaive(sp500_TR, h = 32)),
        	holdout = act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"",
       	title = ""Seasonal Naive Forecast Plot of S & P 500"")
	g
	ggplotly(g)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="//plot.ly/~raviolli77/47.embed?autosize=True&width=90%&height=100%"></iframe>

## Exponential Smoothing Forecast

The following forecasting method is far more complex than the previous methods. This forecasting method relies on weighted averages of past observations where the most recent observations hold higher weight! Fortunately for us if we usethe `ets` function it outputs the method that best fits (much like the `auto.arima()` function)

For those interested when outputting the summary for the `ets` model we receive that our model is *ETS(A, Ad, N)* which reading more of Hyndman's blog we see that it is equivalent to an *ARIMA(1, 1, 2)* interesting to know. Definitely something I will research more in detail later!

	h <- autoplot(forecast(ets(sp500_TR), h = 32),
        	holdout=act_sp500_2015_ts) +
  	theme(panel.background = element_rect(fill = ""gray98""),
        	axis.line.y = element_line(colour=""gray""),
        	axis.line.x = element_line(colour=""gray"")) +
  	labs(x = ""Year"", y = ""Closing Values"",
       	title = ""Exponential Smoothing Forecast Plot of S & P 500"")
	h
	ggplotly(h)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/49.embed?autosize=True&width=90%&height=100%"></iframe>

# Conclusions

The forecasting method we use to find the best model is recieving the lowest *MAE* and *MAPE* as described by **Rob J. Hyndman** [here](https://www.otexts.org/fpp/2/5)

We run the accuracy function on all the forecast methods and we check which performed best!

	accuracy(for_sp500_all)
	accuracy(for_sp500_BC)
	accuracy(nnetar(sp500_TR, h = 32))
	accuracy(meanf(sp500_TR, h = 32))
	accuracy(naive(sp500_TR, h = 32))
	accuracy(snaive(sp500_TR, h = 32))
	accuracy(forecast(ets(sp500_TR), h = 32))

### Terminal Output

| Model | ME | RMSE | MAE | MPE | MAPE | MASE | ACF1 |
|-----------|--------------|----------|----------|-------------|----------|-----------|--------------|
| ARIMA Foreacst |  -0.00911296 | 33.85289 | 24.84955 | -0.00840343 | 2.141218 | 0.1310854 |  -0.01137429 |
| Box-Cox Forecast    |  6.808873    | 39.28348 | 30.16598 | 0.282006    | 2.567669 | 0.1591304 |   0.4091459  |
| Neural Networks | 0.1500122 | 34.94519 | 26.72383 | -0.08139134 | 2.280222 | 0.1409725 | 0.07412086 |
| Mean Forecast |-9.668655e-15 | 319.3598 | 244.9373 | -9.110565   | 24.74398 | 1.292084  |   0.9666459  |
| Naive Forecast |  6.624059    | 39.30052 | 30.14866 | 0.5581768   |  2.66548 | 0.1590391 |   0.4170651  |
| Seasonal Naive Forecast |  73.06769 | 219.6302 | 189.5676 | 4.738094 | 16.6731 |    1  | 0.9647997 |
| Exponential Smoothing Forecast   |  2.648054    | 36.65711 | 27.74495 | 0.2519063   | 2.409392 | 0.1463591 |   0.1592457  |

# GARCH Modeling


So as we stated before the data set we used has very volatile observations which is a cause for concern when it comes to predicting!

Upon further research I heard about **Generalized Autoregressive Conditional Heteroskedasticity** as stated by [Pat](https://www.r-bloggers.com/author/pat/) on [R-blogger](https://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/):

""*Volatility clustering — the phenomenon of there being periods of relative calm and periods of high volatility — is a seemingly universal attribute of market data*""

From the articles I've read online the best way to see if *GARCH* models are appropriate is to take the residuals and square them.

Once we do this any values that are volatile will visually appear, but heads up since I am learning this outside academia it's been hard to find documentation that is at an undergraduate level. But here is the plot of the squared residuals which seem to show two spikes of high volatility.

	squared.resARIMA <- fit$residuals^2
	sqRes <- autoplot(squared.resARIMA, main = ""Squared Residuals"",
    	ts.colour = ""turquoise4"") +
	theme(panel.background = element_rect(fill = ""gray98""),
    	axis.line.y = element_line(colour=""gray""),
    	axis.line.x = element_line(colour=""gray""))
	sqRes

	ggplotly(sqRes)

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/59.embed?autosize=True&width=90%&height=100%"></iframe>

So the next step is to plot the *ACF* and *PACF* of the squared residuals to see if *GARCH* modeling is appropriate!

	e <- autoplot(acf(squared.resARIMA, plot = FALSE),
            	conf.int.fill = '#0000FF', conf.int.value = 0.95,
            	conf.int.type = 'ma') +
	theme(panel.background = element_rect(fill = ""gray98""),
            	panel.grid.minor = element_blank(),
            	axis.line.y = element_line(colour=""gray""),
            	axis.line.x = element_line(colour=""gray""))

	f <- autoplot(pacf(squared.resARIMA, plot = FALSE),
            	conf.int.fill = '#0000FF', conf.int.value = 0.95,
            	conf.int.type = 'ma') +
	theme(panel.background = element_rect(fill = ""gray98""),
            	panel.grid.minor = element_blank(),
            	axis.line.y = element_line(colour=""gray""),
            	axis.line.x = element_line(colour=""gray""))

	grid.arrange(e, f)

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf_res_sq.png">

These plots suggest that there are not significant lags when doing the residuals squared, so more research has to be done on our part with respect to this part since its new material to us. But this concludes our time series analysis!

# Sources Cited
Here we include all the resources that helped us and that we would highly recommend reading! They are resources that we believed covered the material very well without getting to technical. Although I would recommend **Time Series Analysis and Its Application with R Example** if you would like to learn more in depth. I would also like to acknowledge [Rob J. Hyndman](http://robjhyndman.com) for his major contributions to Time Series Analysis and the r community with the creation of the `forecast` package among other contributions. As well as [Hadley Wickham](http://hadley.nz/) for his contribution to both **Rstudio** and `ggplot2`, which without these none of this would have been possible. Thank you.

+ Hyndman, Rob J., and George Athanasopoulos. [""Forecasting: Principles and Practice""](https://www.otexts.org/fpp) Otexts. N.p., May 2012. Web.
+ [NIST/SEMATECH e-Handbook of Statistical Methods](http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4.htm), ""Introduction to Time Series Analysis"". June, 2016.
+ Schmidt, Drew. [Autoplot: Graphical Methods with ggplot2](http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/) Wrathematics, my stack runneth over. June, 2012. Web.
+ [""Stack Exchange""](http://stats.stackexchange.com) To all the contributors when we looked for answers on Stack Exchange we can't thank you enough

## Citations for Packages Used in Project

Citations created using the function (in **R**):

	citation(""packageName"")

+ A. Trapletti and K. Hornik (2016). **tseries: Time Series Analysis and Computational Finance**. R package version 0.10-35.
+ B. Auguie (2016). **gridExtra: Miscellaneous Functions for ""Grid"" Graphics**. R package version 2.2.1. https://CRAN.R-project.org/package=gridExtra
+ C. Sievert, C. Parmer, T. Hocking, S. Chamberlain,
K. Ram, M. Corvellec and P. Despouy (NA). **plotly: Create Interactive Web Graphics via 'plotly.js'**.
https://plot.ly/r, https://cpsievert.github.io/plotly_book/, https://github.com/ropensci/plotly.
+ D. Stoffer (2016). **astsa: Applied Statistical Time Series Analysis**. R package version 1.6. https://CRAN.R-project.org/package=astsa
+ H. Wickham. **ggplot2: Elegant Graphics for Data Analysis**. Springer-Verlag New York, 2009.
+ H. Wickham and W. Chang (2016). **devtools: Tools to Make Developing R Packages Easier**. R package version 1.12.0. https://CRAN.R-project.org/package=devtools
+ M. Horikoshi and Y. Tang (2016). **ggfortify: Data Visualization Tools for Statistical Analysis Results**. R package version 0.2.0. https://CRAN.R-project.org/package=ggfortify
+ R. J. Hyndman(2016). **forecast: Forecasting functions for time series and linear models** . R package version 7.2, http://github.com/robjhyndman/forecast>.
