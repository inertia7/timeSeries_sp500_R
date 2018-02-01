# Load Packages

This project has changed since its creation, for the current structure we include the additional packages inside the `helper_functions` script.

Along with the use of the `packrat` package as a version controller, which removes the need to manually install each package used in this project.

Within the `helper_functions` script we call `library()` method and include the package names as arguments.

	# LOAD YOUR PACKAGES

	library(ggplot2)
	library(forecast)
	library(plotly)
	library(ggfortify)
	library(tseries)
	library(gridExtra)
	library(docstring)
	library(readr)

## Here Package

Utilizing the `here` package, the `here()` function will make the main directory accessible to easily navigate the project directory.


	library(here)
	here()
	# Console Output
	# > here() starts at /home/myProjects/timeSeries_sp500_R
	# You should get something similar

We encourage you to do a quick search on each of these packages to gain context on what they are useful for, and why we are using them here. Pay special attention to the packages `forecast`.

# Get Data
Now we collect our data. We want to use reliable sources of complete and accurate data. We collected *21 years* (1995-2015) of **S&P 500 Stock Index** data at a monthly frequency (a total of 252 observations) from [Yahoo Finance](https://finance.yahoo.com/quote/%5EGSPC/history?period1=788947200&period2=1489820400&interval=1mo&filter=history&frequency=1mo). You can do the same too. We chose to use the Adjusted Closing Value for our analysis.

## Cleaning Data
We cleaned our data manually using *Google Sheets*. For this we made sure that all fields contained data (i.e. no missing values) and that the headers (i.e. column names) were labeled appropriately. This can be done programmatically with **R**, but this is outside of the scope of this project.

We also included a bunch of other variables (for exploratory analysis) such as the **NASDAQ Stock Index**, the **NYSE Stock Exchange**, the **U.S. Housing Price Index**, and the **U.S. Gross Domestic Product**. Here is our data in file [data_master_1.csv](https://github.com/inertia7/timeSeries_sp500_R/blob/master/data_master_1.csv), which is ready to be used with **R**.

## Loading Data
Then we must include our data set within our working R environment. For this we use:

	data_master <- read.csv(here("data", "data_master_1.csv"))
	# Within the github repo the file `data_master_1`
	# is inside the data folder
	# Using `here` will notify R to go to:
	# `/home/myProjects/timeSeries_sp500_R/data`

Now we can call our **S&P 500 Stock Index** data by typing `dataMaster$sp_500` into our terminal.

# Exploratory Analysis
Now we want to get a feel for our data to get an intuition about the models that may be appropriate for our forecast. For this, we plot our data and diagnose for *trend*, *seasonality*, *heteroskedasticity*, and *stationarity*. We go over these concepts in further detail in this section.

## Creating time-series data object
Our **S&P 500 Stock Index** data is in the form of time-series; this means that our data exists over a continuous time interval with equal spacing between every two consecutive measurements. In **R** we are able to create time-series objects for our data vectors using the `ts()` method. For this, we select the vector we would like to use as the first argument, and tune the `start` and `freq` (frequency) parameters. Then we output the time-series data to the terminal by calling our newly-created time-series object.

	sp_500 <- ts(data_master$sp_500, start=c(1995, 1), freq=12)

Here we use our function called `plot_time_series`, which does as its name suggests:

	plot_time_series(sp_500, 'S&P 500')


<iframe width="100%" height=500  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_full.html"></iframe>

Before we begin any analysis, we will be splitting the data to remove 2015 to use as our test set.

	sp500_training <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)

### Terminal Output
<img src="http://i.imgur.com/IVIGxNF.png">

## Plotting our Time Series


Plotting the data is arguably the most critical step in the exploratory analysis phase (We chose to emphasize on the time series object that has intervals from *1995* to *2014*, which we will explain later!). This enables us to make inferences about important components of the time-series data, such as *trend*, *seasonality*, *heteroskedasticity*, and *stationarity*. Here is a quick summary of each:

+ **Trend**: we say that a dataset has a trend when it has either a *long-term increase* or *decrease*.
+ **Seasonality**: we say that a dataset has seasonality when it has patterns that repeat over known, fixed periods of time (e.g. monthly, quarterly, yearly).
+ **Heteroskedasticity**: we say that a data is *heteroskedastic* when its variability is not constant (i.e. its variance increases or decreases as a function of the explanatory variable).
+ **Stationarity**: a stochastic process is called *stationary* if the mean and variance are constant (i.e. their joint distribution does not change over time).


we start our analysis by plotting our time series object to give us a visual basis to start our modeling.

	 plot_time_series(sp500_training, 'S&P 500')

<iframe width="100%" height=500  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_training.html"></iframe>

We can quickly see that our time-series has instances of both positive and negative trend. Overall, it is very volatile, which tells us that we will have transform the data in order for the **Box-Jenkins Methodology** to predict with better accuracy.

## Testing for Stationarity

We will utilize a few statistical tests to test for stationarity. We must be weary of our model having a *unit root*, this will lead to non-stationary processes.

	Box.test(sp500_training, lag = 20, type = 'Ljung-Box')

### Terminal Output

	> Box.test(sp500_training, lag = 20, type = 'Ljung-Box')

		Box-Ljung test

	data:  sp500_training
	X-squared = 2024.8, df = 20, p-value < 2.2e-16

Now we will utilize the **Augmented Dickey-Fuller Test** for stationarity. The null hypothesis states that large p-values indicate non-stationarity and smaller p values indicate stationarity (We will be using *0.05* as our alpha value).

	adf.test(sp500_training)

### Terminal Output

	> adf.test(sp500_training)

		Augmented Dickey-Fuller Test

	data:  sp500_training
	Dickey-Fuller = -1.7877, Lag order = 6, p-value = 0.6652
	alternative hypothesis: stationary

We can see our p-value for the ADF test is relatively high, so we'll do some further visual inspection. But we know we will most likely have to difference our time series for stationarity.

## Decomposing our time-series

Beyond understanding the *trend* of our time-series, we want to further understand the anatomy of our data. For this reason we break-down our time-series into its *seasonal component*, *trend*, and *residuals*.

	plot_decomp(sp500_training, 'S&P 500')

<iframe width="100%" height=700  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_decomp.html"></iframe>

The trend line already shows us what we know and we can see that there might be some seasonality in our time series object.

## Seasonal Plot

We will investigate if there was enough seasonality to adjust our time series object for seasonality. The seasonal plot can provide a good visual of seasonality for time series objects. This can be best illustrated through a commonly used time series dataset: [AirPassenger](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/AirPassengers.html) (which requires knowledge of *stationary* and *heteroskadasicty* to transform the data set appropriately).

But if you run the seasonal plot for this function, you can see it follows a certain season pattern. I provided the code right here but will not provide visuals since its not apart of this project. You can run it yourself to get a picture of what I'm saying

## Sample code for AirPassenger data set:

	tsAir <- diff(log(AirPassengers))
	plot_seasonal(tsAir, 'Air Passengers')


Going back to our data set we run the same function to grab a visual diagnosis for the seasonality in our data set.

	plot_seasonal(sp500_training, 'S&P')

<iframe width="100%" height=700  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_seasonal.html"></iframe>

However upon inspection there is no indication of any seasonal patterns outside of the later years showing upward trend, thus another indication towards *non-stationarity*.

# Model Estimation
## Diagnosing the ACF and PACF Plots of our Time-Series Object

*ACF* stands for "autocorrelation function" and *PACF* stands for "partial autocorrelation function". The *ACF* and *PACF* diagnosis is employed over a time-series to determine the order for which we are going to create our model using *ARIMA* modeling. Loosely speaking, a time-series is *stationary* when its mean, variance, and *autocorrelation* remain constant over time.

These functions help us understand the correlation component of different data points at different time *lags*. *Lag* refers to the time difference between one observation and a previous observation in a dataset. Let's examine our plots!


    # DIAGNOSING ACF AND PACF PLOTS
    plot_acf_pacf(sp500_training, 'S&P 500')

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf.png">

When there is large autocorrelation within our lagged values, we see geometric decay in our plots, which is a huge indicator that we will have to take the difference of our time series object.

## Transforming our data to adjust for non-stationary

From visual inspection of the time series object and the other graphs used for exploratory purposes we decided it is appropriate to difference our time series object to account for the *non-stationarity* and see how that fares!

A way to make a time-series *stationary* is to find the difference across its consecutive values. This helps stabilize the mean, thereby making the time-series object stationary.

For this we use the `diff()` method.

	tsDiff <- diff(sp500_training)


Next we plot our transformed time-series.

	plot_time_series(tsDiff, 'First Difference')

<iframe width="100%" height=500  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_diff.html"></iframe>

This plot suggests that our working data is stationary. We want to confirm this running an *ACF* and *PACF* diagnostics over this data to find our if we can proceed to estimating a model.

## Testing for Stationarity
We apply the same tests to our differenced time series object.

	> Box.test(tsDiff, lag = 20, type = 'Ljung-Box')

	Box-Ljung test

	data:  tsDiff
	X-squared = 58.2, df = 20, p-value = 1.347e-05

Now let's use the ADF Test

	> adf.test(tsDiff)

	Augmented Dickey-Fuller Test

	data:  tsDiff
	Dickey-Fuller = -4.9552, Lag order = 6, p-value = 0.01
	alternative hypothesis: stationary

	Warning message:
	In adf.test(tsDiff) : p-value smaller than printed p-value

Upon reading this [stackoverflow](https://stats.stackexchange.com/questions/142003/adf-test-results-confusion) post over the cryptic warning message, we can see that the result yields a small p-value which makes us reject the null suggestion stationarity.

## Diagnosing the acf and pacf of our transformed time-series object

The plot below helps us confirm that we have stationarity and also helps us deduce which model we will use. It is important to keep in mind that we have a difference parameter equal to one (i.e. *d = 1*) because of the previous transformation we carried out.

	plot_acf_pacf(tsDiff, 'First Difference Time Series Object')

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf_diff.png">

From the above plots we deduce that an *MA(1)* model (where *MA* stands for **moving average**) best fits our data because the *ACF* cuts off at one significant lag and the *PACF* shows geometric decay.

Recall that we are examining the differenced time-series so we have to use the combined model *ARIMA* (**Autoregressive integrated moving average**), thus our model so far is *ARIMA(0, 1, 1)*.

## Seasonal Plot for Transformed Time Series Object

Now that we have a stationary time series object, we again make a seasonal plot which this time shows us that there is no indicative visual seasonal pattern provided by the following code:


	plot_seasonal(tsDiff, 'First Difference of S&P 500')

<iframez width="100%" height=700  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_diff_seasonal.html"></iframe>

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

	# RESIDUAL DIAGNOSTICS
	ggtsdiag_custom(fit) +
	theme(panel.background = element_rect(fill = "gray98"),
			panel.grid.minor = element_blank(),
			axis.line.y = element_line(colour="gray"),
			axis.line.x = element_line(colour="gray"))

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/resid_diag.png">

A histogram gives us another visualization of the normal distribution of our residuals.

	residFit <- ggplot(data=fit, aes(residuals(fit))) +
    	geom_histogram(aes(y =..density..),
    	col="black", fill="white") +
	geom_density(col=1) +
	theme(panel.background = element_rect(fill = "gray98"),
    	panel.grid.minor = element_blank(),
    	axis.line   = element_line(colour="gray"),
    	axis.line.x = element_line(colour="gray")) +
	ggtitle("Plot of S&P 500 ARIMA Model Residuals")

	residFit
	ggplotly(residFit)

<img src="https://i.imgur.com/fDwVTzn.png">

Although not perfect we can see that the residuals do display a *normal distribution*. The outliers may be explained by the *2008 financial crisis*.

This model appears to confirm all of our assumptions, which means we can continue to the forecasting phase!

# Forecasting
We proceed to forecasting now that we believe we found the appropriate model!

We utilized the `autoplot()` function quite heavily on this iteration of our project, since we couldn't find a way of adding the actual values to the plot we used a workaround by borrowing [Drew Schmidt's](http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/) work to include the actual 2015 values.  

**UPDATE**: For the current iteration on inertia7, we decided to iteratively update the test set, so for this demonstration we included 2015-2017. In the shiny dashboard we only included the original forecast range (only 2015). 

For this we downloaded data for 2015-2017 and created our test set accordingly. 

	sp500_test <- read.csv(here("data", "test_data.csv"))
	sp500_test <- ts(sp500_test$Adj.Close, 
		start = c(2015, 1), 
		frequency = 12)
	# original: sp500_test <- window(sp_500, 2015, c(2015, 12))

Next we use the `forecast` function, `ggplot2` and `plotly` to visualize the predictions for the year 2015! Here within the plots the forecasted values are **BLUE**, the actual 2015 values are in **RED**, the 80% Confidence Intervals are encompassed in the **YELLOW** bands and 95% *Confidence Intervals* are encompassed in the **ORANGE** bands respectively.

	for_sp500_all <- forecast(fit, h = 36)

Next we create the autoplot visualization.

	autoplot(fit_arima,
		holdout = sp500_test, 
		forc_name = 'ARIMA', 
		ts_object_name = 'S&P 500')

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_arima.html"></iframe>

We can see that the model performs well and within the 80% and 95% confidence intervals. You can forecast values even further into the future by tuning the appropriate parameters. Please not that this forecast project is for educational purposes and **we do not recommend investing by using these predictions** - remember that the stock market is very volatile.

# Other Forecasting Methods

So in this more interactive iteration of our project we included other forecasting methods to show the versatility of forecasting methods and to use as comparisons!

## Box-Cox Forecast


*Box-Cox transformations* are generally used to transform non-normally distributed data to become approximately normal! Although we do not think this an appropriate transformation for our data set, it is still included in our analysis because it's a useful transformation to do especially since most real time data is not approximately *normally distributed*.

	lambda <- BoxCox.lambda(sp500_TR)
	fit_sp500_BC <- ar(BoxCox(sp500_TR,lambda))
	fit_BC <- forecast(fit_sp500_BC,h=36,lambda=lambda)

Now that we have created the forecast object we plot the prediction!


	autoplot(fit_BC, 
		holdout = sp500_test,
		forc_name = 'Box-Cox Transformation', 
		ts_object_name = 'S&P 500')
	

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_box.html"></iframe>

The prediction shows a downward trend whereas the actual values show upward trend.  

## Exponential Smoothing Forecast

The following forecasting method is far more complex than the previous methods. This forecasting method relies on weighted averages of past observations where the most recent observations hold higher weight! Fortunately for us if we usethe `ets` function it outputs the method that best fits (much like the `auto.arima()` function)

For those interested when outputting the summary for the `ets` model we receive that our model is *ETS(A, Ad, N)* which reading more of Hyndman's blog we see that it is equivalent to an *ARIMA(1, 1, 2)* interesting to know. 

	fit_ets <- forecast(ets(sp500_training), h = 36)
	autoplot(fit_ets, 
		holdout=sp500_test,
		forc_name = 'Exponential Smoothing',
		ts_object_name = 'S&P 500')

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_ets.html"></iframe>

Interesting that *Exponential Smoothing's* prediction is still within both prediction intervals, although the bands are noticeably larger than our *ARIMA*, it will be interesting to see more future predictions for this promising model. 

## Mean Forecast
For most of these forecasting methods they are best explained by Rob J. Hydnman (the man who created most of these time series packages) I will just be iterating what he has already said for simple forecasting methods found [here](https://www.otexts.org/fpp/2/3).

The forecasting methods are useful to keep in mind because you might conclude that your time series object might not even require some complex algorithm. We begin with the average method; with the `meanf()` function we are essenntially forecasting values based upon the mean of the historical data!

	fit_meanf <- meanf(sp500_training, h = 36)
	autoplot(fit_meanf, 
		holdout = sp500_test,
		forc_name = 'Mean',
		ts_object_name = 'S&P 500') 

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_meanf.html"></iframe>

As we can see due to the non-stationarity and volatility of our data this model performs very poorly. 

## Naive Forecast

The *naive forecasting* method returns an *ARIMA(0, 1, 0) with random walk* model that is applied to our time series object. Important to note that Hyndman described this forecasting method as being effective in financial time series objects, and that the forecasting method "... all [...] values are set to be $$y_T$$, where $$y_T$$ is the last observed value"

	fit_naive <- naive(sp500_training, h = 36)
	autoplot(fit_naive, 
		holdout = sp500_test,
		forc_name = 'Naive Forecast',
		ts_object_name = 'S&P 500') 

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_naive.html"></iframe>

## Seasonal Naive Forecast

For the `snaive()` method it follows the same principles as the *naive* method, but works better for very seasonal data!


	fit_snaive <- snaive(sp500_training, h = 36)
	autoplot(fit_snaive, 
		holdout = sp500_test,
		forc_name = 'Seasonal Naive',
		ts_object_name = 'S&P 500')

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_snaive.html"></iframe>


## Neural Networks
For neural networks in the context of time series, each lagged value can be thought of as an input for the network (more specifically a *feedforward neural network*). Our model produced was a **NNAR(2, 1, 2)[12]**, which has 3 inputs including a seasonal input with 2 hidden layers. Important to note that models can sometimes be translated to ARIMA equivalents, but don't have restrictions on parameters to ensure stationarity (for more information [see here](https://www.otexts.org/fpp/9/3)). 

Now we plot:

	fit_sp500_net <- nnetar(sp500_training, lambda = lambda) # Using BC lambda
	fit_net <- forecast(fit_sp500_net, h = 36, PI = TRUE)
	autoplot(fit_net, 
		holdout = sp500_test,
		forc_name = 'Neural Networks',
		ts_object_name = 'S&P 500')


<iframe width="100%" height=550 frameborder="0" scrolling="no" src="https://raviolli77.github.io/sp_forc_nn.html"></iframe>


# Conclusions

The forecasting method we use to find the best model is recieving the lowest *MAE* and *MAPE* as described by **Rob J. Hyndman** [here](https://www.otexts.org/fpp/2/5)

We run the accuracy function on all the forecast methods and we check which performed best!

	round(accuracy(fit_arima, sp500_test), 3)
	round(accuracy(fit_BC, sp500_test), 3)
	round(accuracy(fit_ets, sp500_test), 3)
	round(accuracy(fit_meanf, sp500_test), 3)
	round(accuracy(fit_naive, sp500_test), 3)
	round(accuracy(fit_snaive, sp500_test), 3)
	round(accuracy(fit_net, sp500_test), 3)

### Terminal Output

<iframe width="100%" src="https://cdn.rawgit.com/raviolli77/bbef474cb6a88f715635ce790bd6f662/raw/de7a39633e447da38c03df1d0a78992c5780ba95/accuracy_metrics_time_series.html" width="800" height="350" frameborder="0"></iframe>

As we can see from our metrics relating to the 3 year test set, the *ARIMA* modeled performed better with *Exponential Smoothing* peforming well. Through the forecast plots however we saw that *Exponential Smoothing* is still within the prediction intervals, so its a close call. 

We conclude that the *ARIMA* models performs best given that it's still inside the 95% prediction intervals and the accuracy metrics performed better than all other models. 


# GARCH Modeling


So as we stated before the data set we used has very volatile observations which is a cause for concern when it comes to predicting!

Upon further research I heard about **Generalized Autoregressive Conditional Heteroskedasticity** as stated by [Pat](https://www.r-bloggers.com/author/pat/) on [R-blogger](https://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/):

"*Volatility clustering — the phenomenon of there being periods of relative calm and periods of high volatility — is a seemingly universal attribute of market data*"

From the articles I've read online the best way to see if *GARCH* models are appropriate is to take the residuals and square them.

Once we do this any values that are volatile will visually appear, but heads up since I am learning this outside academia it's been hard to find documentation that is at an undergraduate level. But here is the plot of the squared residuals which seem to show two spikes of high volatility.

	squared_res_fit <- fit$residuals^2
	plot_time_series(squared_res_fit, "Squared Residuals")

<iframe width="100%" height=550  frameborder="0" scrolling="no" src="https://raviolli77.github.io/garch_resid.html"></iframe>

So the next step is to plot the *ACF* and *PACF* of the squared residuals to see if *GARCH* modeling is appropriate!

	plot_acf_pacf(squared_res_fit, 'S&P 500 Residuals^2')

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf_res_sq.png">

These plots suggest that there are not significant lags when doing the residuals squared, so more research has to be done on our part with respect to this part since its new material to us. But this concludes our time series analysis!

# Sources Cited
Here we include all the resources that helped us and that we would highly recommend reading! They are resources that we believed covered the material very well without getting to technical. Although I would recommend **Time Series Analysis and Its Application with R Example** if you would like to learn more in depth. I would also like to acknowledge [Rob J. Hyndman](http://robjhyndman.com) for his major contributions to Time Series Analysis and the r community with the creation of the `forecast` package among other contributions. As well as [Hadley Wickham](http://hadley.nz/) for his contribution to both **Rstudio** and `ggplot2`, which without these none of this would have been possible. Thank you.

+ Hyndman, Rob J., and George Athanasopoulos. ["Forecasting: Principles and Practice"](https://www.otexts.org/fpp) Otexts. N.p., May 2012. Web.
+ [NIST/SEMATECH e-Handbook of Statistical Methods](http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4.htm), "Introduction to Time Series Analysis". June, 2016.
+ Schmidt, Drew. [Autoplot: Graphical Methods with ggplot2](http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/) Wrathematics, my stack runneth over. June, 2012. Web.
+ ["Stack Exchange"](http://stats.stackexchange.com) To all the contributors when we looked for answers on Stack Exchange we can't thank you enough

## Citations for Packages Used in Project

Citations created using the function (in **R**):

	citation("packageName")

+ A. Trapletti and K. Hornik (2016). **tseries: Time Series Analysis and Computational Finance**. R package version 0.10-35.
+ B. Auguie (2016). **gridExtra: Miscellaneous Functions for "Grid" Graphics**. R package version 2.2.1. https://CRAN.R-project.org/package=gridExtra
+ C. Sievert, C. Parmer, T. Hocking, S. Chamberlain,
K. Ram, M. Corvellec and P. Despouy (NA). **plotly: Create Interactive Web Graphics via 'plotly.js'**.
https://plot.ly/r, https://cpsievert.github.io/plotly_book/, https://github.com/ropensci/plotly.
+ D. Stoffer (2016). **astsa: Applied Statistical Time Series Analysis**. R package version 1.6. https://CRAN.R-project.org/package=astsa
+ H. Wickham. **ggplot2: Elegant Graphics for Data Analysis**. Springer-Verlag New York, 2009.
+ H. Wickham and W. Chang (2016). **devtools: Tools to Make Developing R Packages Easier**. R package version 1.12.0. https://CRAN.R-project.org/package=devtools
+ M. Horikoshi and Y. Tang (2016). **ggfortify: Data Visualization Tools for Statistical Analysis Results**. R package version 0.2.0. https://CRAN.R-project.org/package=ggfortify
+ R. J. Hyndman(2016). **forecast: Forecasting functions for time series and linear models** . R package version 7.2, http://github.com/robjhyndman/forecast>.