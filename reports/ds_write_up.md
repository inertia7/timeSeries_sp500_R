# Univariate Time Series Analysis on S&P 500 Stock Index

This project focuses on finding the best statistical-learning time series model to predict future values for the **S&amp;P 500 Stock Index**.

Time-series analysis is a basic concept within the field of statistical-learning, which is appropriate for the analysis of the **S&amp;P 500 Stock Index**. Although important to note that using this time series analysis to invest in stocks is highly discouraged.   

For this project we leverage the horse-power of RStudio and deliver, where appropriate, gorgeous interactive data visualizations using **ggplot2** and **plotly**

# Load Packages

First, in order for all the packages to be loaded correctly into your *Rstudio* console, select in *File* - *Open Project in new Sesssion...*, then clicking on the pre-existing *Rproject* from the repo. With the use of the `packrat` package, the `.Rprofile` script will automatically run files in the `packrat` directory and load the appropriate packages into the **R** environment.

Once this is completed, you should be opened up to a new Rstudio sesion. Here we will be referencing the `helper_functions` script which contains all the necessary packages. We use

	source(here("src",'helper_functions.R'))

For this demonstration we will load them in manually.

```
# LOAD YOUR PACKAGES
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
here() # Should output current work directory
```

Using the `here` package, (as recommended by [Jenny Bryan](https://community.rstudio.com/u/jennybryan) in [this thread](https://community.rstudio.com/t/project-oriented-workflow-setwd-rm-list-ls-and-computer-fires/3549)), we are going to set the working directory and will be able to reference different subdirectories to make the workflow easier.

# Get Data
Now we collect our data. We want to use reliable sources of complete and accurate data. We collected *21 years* (1995-2015) of **S&P 500 Stock Index** data at a monthly frequency (a total of 252 observations) from [Yahoo Finance](https://finance.yahoo.com/quote/%5EGSPC/history?period1=788947200&period2=1489820400&interval=1mo&filter=history&frequency=1mo). You can do the same too. We chose to use the Adjusted Closing Value for our analysis.

## Loading Data
Then we must include our data set within our working R environment. For this we use:

	data_master <- read.csv(here("data", "data_master_1.csv"))

	attach(data_master)

Now we can call our **S&P 500 Stock Index** data by typing `data_master$sp_500` into our terminal.

# Helper Functions

As the project grew, our knowledge of object oriented programming grew as well. We created a helper script which contains functions that automate many of our plots.

In order to provide well written documentation for these functions, we utilized the `docstrings` package which allows you to view them on *Rstudio* as such:


    ?function_name

# Exploratory Analysis

Now we want to get a feel for our data to get an intuition about the models that may be appropriate for our forecast. For this, we plot our data and diagnose for *trend*, *seasonality*, *heteroskedasticity*, and *stationarity*. We go over these concepts in further detail in this section.

## Creating time-series data object

Our **S&P 500 Stock Index** data is in the form of time-series; this means that our data exists over a continuous time interval with equal spacing between every two consecutive measurements.

In **R** we are able to create time-series objects for our data vectors using the `ts()` method. For this, we select the vector we would like to use as the first argument, and tune the `start` and `freq` (frequency) parameters. Then we output the time-series data to the terminal by calling our newly-created time-series object.

  	sp_500 <- ts(data_master$sp_500, start=c(1995, 1), freq=12)

Here we use our own function called `plot_time_series`, which does as its name suggests:

    plot_time_series(sp_500, 'S&P 500')


<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/55.embed?autosize=True&width=90%&height=100%"></iframe>

Before we begin any analysis, we will be splitting the data to remove 2015 to use as our test set.

    sp500_training <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)


## Plotting our Time Series


Plotting the data is arguably the most critical step in the exploratory analysis phase (We chose to emphasize on the time series object that has intervals from *1995* to *2014*, which we will explain later!). This enables us to make inferences about important components of the time-series data, such as *trend*, *seasonality*, *heteroskedasticity*, and *stationarity*. Here is a quick summary of each:

+ **Trend**: we say that a dataset has a trend when it has either a *long-term increase* or *decrease*.
+ **Seasonality**: we say that a dataset has seasonality when it has patterns that repeat over known, fixed periods of time (e.g. monthly, quarterly, yearly).
+ **Heteroskedasticity**: we say that a data is *heteroskedastic* when its variability is not constant (i.e. its variance increases or decreases as a function of the explanatory variable).
+ **Stationarity**: a stochastic process is called *stationary* if the mean and variance are constant (i.e. their joint distribution does not change over time).

we start our analysis by plotting our time series object to give us a visual basis to start our modeling.


	 plot_time_series(sp500_training, 'S&P 500')

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/31.embed"></iframe>

We can quickly see that our time-series has instances of both positive and negative trend. Overall, it is very volatile, which tells us that we will have transform the data in order for the **Box-Jenkins Methodology** to predict with better accuracy.

## Testing for Stationarity
We will utilize a few statistical tests to test for stationarity. We must be weary of our model having a *unit root*, this will lead to non-stationary processes.

	Box.test(sp_500, lag = 20, type = 'Ljung-Box')

### Terminal Output

	> Box.test(sp500_training, lag = 20, type = 'Ljung-Box')

		Box-Ljung test

	data:  sp500_training
	X-squared = 2024.8, df = 20, p-value < 2.2e-16

Now we will utilize the **Augmented Dickey-Fuller Test** for stationarity. The null hypothesis states that large p-values indicate non-stationarity and smaller p values indicate stationarity (We will be using 0.05 as our alpha value).

	adf.test(sp_500)

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

<iframe width="100%" height=600  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/35.embed?autosize=True&width=90%&height=100%"></iframe>

The trend line already shows us what we know and we can see that there might be some seasonality in our time series object.

# Model Estimation
## Diagnosing the ACF and PACF Plots of our Time-Series Object

*ACF* stands for "autocorrelation function" and *PACF* stands for "partial autocorrelation function". The *ACF* and *PACF* diagnosis is employed over a time-series to determine the order for which we are going to create our model using *ARIMA* modeling. Loosely speaking, a time-series is *stationary* when its mean, variance, and *autocorrelation* remain constant over time.

These functions help us understand the correlation component of different data points at different time *lags*. *Lag* refers to the time difference between one observation and a previous observation in a dataset. Let's examine our plots!


    # DIAGNOSING ACF AND PACF PLOTS
    plot_acf_pacf(sp500_training, 'S&P 500')

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/acf_pacf.png">

When there is large autocorrelation within our lagged values, we see geometric decay in our plots, which is a huge indicator that we will have to take the difference of our time series object.

## Transforming our data to adjust for non-stationary

From visual inspection of the time series object and the previously mentioned statistical tests used for exploratory analysis, we decided it is appropriate to difference our time series object to account for the *non-stationarity* and see how that fares!

A way to make a time-series *stationary* is to find the difference across its consecutive values. This helps stabilize the mean, thereby making the time-series object stationary.

For this we use the `diff()` method.

	tsDiff <- diff(sp500_training)


Next we plot our transformed time-series:

    plot_time_series(tsDiff, 'First Difference')

<iframe width="100%" height=415  frameborder="0" scrolling="no" src="https://plot.ly/~raviolli77/37.embed?autosize=True&width=90%&height=100%"></iframe>

This plot suggests that our working data is stationary. We want to confirm this running the same tests, and looking at the  *ACF* and *PACF* diagnostics over the differenced data to find our if we can proceed to estimating a model.

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

# Build Model
Our findings in the exploratory analysis phase suggest that model *ARIMA(0, 1, 1)* might be best fit. Fortunately, there is a function in **R** that we can use to test our findings.

The `auto.arima()` method, found within the `forecast` package, yields the best model for a time-series based on **Akaike-Information-Criterion** (*AIC*). The *AIC* is a measurement of quality used across various models to find the best fit. After running our original and differenced data sets through the `auto.arima()` method we confirmed that the *ARIMA(0, 1, 1)* is our best fit model.

We use the `Arima()` method to fit our model and include our training data set `sp500_training` as the first argument.


	fit <- Arima(sp500_training, order = c(0,1,1),
    	include.drift = TRUE)
	summary(fit)

Here's the summary of our model (using the `summary()` method):

### Terminal Output

	> summary(fit)
	Series: sp500_training
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

```
# RESIDUAL DIAGNOSTICS
ggtsdiag_custom(fit) +
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
```

<img src="https://raw.githubusercontent.com/inertia7/timeSeries_sp500_R/master/reports/figures/resid_diag.png">

Based on our diagnostic plot, the residuals to seem to display a normal distribution.

This model appears to confirm all of our assumptions, which means we can continue to the forecasting phase!

# Forecasting
We proceed to forecasting now that we believe we found the appropriate model!

We utilized the `autoplot()` function quite heavily on this iteration of our project, since we couldn't find a way of adding the actual values to the plot we used a workaround by borrowing [Drew Schmidt's](http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/) work around to including the actual 2015 values.

The code can be found in the github repository, so you should run the `autoplot.forecast()` in order to get the plots we have here. For now we create a time series object that will include the actual 2015 values, where in our function it will be called up by the parameter `holdout`.

Next we use the `forecast` function (that we updated thanks to *Drew Schmidt*), `ggplot2` and `plotly` to visualize the predictions for the year 2015! Here within the plots the forecasted values are **BLUE**, the actual 2015 values are in **RED**, the 80% Confidence Intervals are encompassed in the **YELLOW** bands and 95% *Confidence Intervals* are encompassed in the **ORANGE** bands respectively.

	for_sp500_all <- forecast(fit, h = 12)


Next let's create the test set:

    sp500_test <- window(sp_500, 2015, c(2015, 12))


# Shiny Dashboard

For this demonstration we primarily focuses on *ARIMA* modeling, a shiny dashboard will be created to give flexible filtering options between different forecasting methods used in the full analysis.

We used 7 different methods which we won't go into detail (for more read the forecasting section on [Inertia7](https://www.inertia7.com/projects/8)).


<iframe width="100%" height="950px" frameborder="0" scrolling="no" src="https://raviolli77.shinyapps.io/forecast_dashboard/"></iframe>

# Conclusions

The forecasting method we use to find the best model is recieving the lowest *MAE* and *MAPE* as described by **Rob J. Hyndman** [here](https://www.otexts.org/fpp/2/5)

Here we use the accuracy method including the test set to give us metrics for all models. The functions is called as follows:

    # Using round function to make it more readable
    round(accuracy(fit, sp500_test), 3)

Here are the results for the test set (the function will return both training and test set metrics, but we're only concerned with the test set metrics).

<iframe src="https://cdn.rawgit.com/raviolli77/bbef474cb6a88f715635ce790bd6f662/raw/de7a39633e447da38c03df1d0a78992c5780ba95/accuracy_metrics_time_series.html" width="800" height="350" frameborder="0"></iframe>

Surprisingly other models performed better on the test set than the ARIMA model. This could be a result of overfitting since our test set was really small, but upon iteratively working on this project, we have been able to expand the test set to include data up until 2017 on the repo and inertia7 project.

For the sake of the dashboard we decided to leave it in the original state, although we do encourage to explore the models with a larger test set.  

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
+ R. J. Hyndman(2016). **forecast: Forecasting functions for time series and linear models** . R package version 7.2, http://github.com/robjhyndman/forecast>."
