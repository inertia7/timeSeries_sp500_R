# Time Series Analysis on S&P 500 Stock Prices (1995-2015)

# ABSTRACT
This project focuses on using univariate time series forecasting methods for the stock market index, Standard & Poor's 500 (abbreviated commonly as S&P 500, which is the notation we will use in this project) emphasizing on Box-Jenkins AutoRegressive Integrated Moving Average (ARIMA) modeling. We went about the time series analysis was through using R and R studio to both predict and visualize our predictions. Along with the interactivity of plotly through the [ggplot2 package](https://github.com/tidyverse/ggplot2) we were able to create stunning visuals that help in understanding which time series forecasting method is most appropriate for your own time series analysis. 
## Packages Required
Here are the required packages which will ensure all the code will run properly. 

	ggplot2
	forecast 
	astsa
	plotly 
	ggfortify
	tseries
	gridExtra

To make sure you have the packages we use in this project use the command(you will only have to use this once): 

	install.packages("packageName") 

You will have now downloaded the package so within your script you run: 

	require(packageName)

This must be done before each Rstudio session, and written at the start of every script to ensure your code will be easily reproducible!
## Steps Required 

### Create plotly Account (Optional)	
If you would like to have the images you create (using plotly and ggplot2) published so that you can customise the plots to your liking or brag about the interactivety of your visuals simply create a [plolty account](https://plot.ly/). Once you do so you will have access to your username and more importantly your API key, these will be necessary to publishing your plots (If you do not wish to publish your plots skip this step). 

### Using Plotly account in Rstudio session
Important to note, when posting on GitHub never publish API keys (this is a common mistake I see people do). Once you gain access to your API key, have plotly in your current working directory, you run:

	Sys.setenv("plotly_username"="userName")
	Sys.setenv("plotly_api_key"="d1X4Hrmbe")

From here you will be able to publish your ggplotly visuals by running (our ggplot2 object is called timeSeriesPlot for this example):

	plotly_POST(timeSeriesPlot, filename = "timeSeriesPlot")

If ran correctly this line of code should open up a browser with your newly published plotly graph!
### Create appropriate working directory
Once the preliminary process of ensure your Rstudio has all parameters to ensure the code will run smoothly we suggest create an appropriate directory. For those using git we recommend using the  following line on a terminal:

	git clone git@github.com:wH4teVr folder-name

But if you are doing it manually you choose the "Clone or download" button and choose "Download ZIP". From here you must take note of where the file is downloaded, once you are able to find the file location you must set the appropriate working directory. 

For this example we set the file into "/user/home/myProjects/timeSeriesR" so recall we have to set the working directory in Rstudio or you will receive errors especially when trying to read in the csv file. Therefore you run at the stop of your script:
For linux:

	setwd("/user/home/myProjects/timeSeriesR")

For windows(Important when finding directorys you will have):

	C:\home\myDocuments\myProjects\timeSeriesR

Which will give you an error (since R considers "\" as an escape code, the correct form is:

	setwd("C:/home/myDocuments/myProjects/timeSeriesR")

Once you have done this you can read the csv file which contains the S&P 500 closing values for which we did our analysis on, and you can proceed to the time series analysis done through R!

# Methodology 
For our time series analysis, we chose to focus on the [Box-Jenkins](https://en.wikipedia.org/wiki/Box%E2%80%93Jenkins#Box-Jenkins_model_identification) methodology which incorporates a series of steps to ensure we  produce the best model to forecasting.

But before we outline the steps we would like to outline some  necessary assumptions for univariate time series analysis:

- The Box-Jenkins Model assumes weakly stationarity process. 
- The residuals are white noise (independently and identically distributed random variables) and homoscedastic


We won't go into detail since there is already a plethora of online resources outlining these assumptions, but we did feel that it was important to state these assumptions.

## ARIMA Model 
For this project we will be using the Autoregressive Integrated Moving Average model and its variations to forecast the S&P 500. For each component we have a corresponding variable for which we model if there is sign of these components. Here we roughly outline the parts that make an ARIMA model 
- **Autoregressive [AR(p)]** - a stochastic process where future values are dependent on past values signifying that past values have a linear effect on the future values.
- **Integration [I(d)]** - when differencing is done to make a process stationary, we include the differenced value(i.e. if we took the first difference it would be I(d=1))
- **Moving Average [MA(q)]** - a prcoess where the current value is linearly regressed on current and past white noise terms (residuals)

Next we outline the steps to ensure we fit the appropriate ARIMA model!

### Stationary process and Seasonality
The first step is checking to see if the time series object is stationary, this can be done in various methods which can also be explained as exploratory analysis since we are in essence "getting a feel" for our data. Here we include some of the processes:

-  Plot the time series object: sometimes simply plotting the time series object can tell you if a process is stationary or not. As well as telling you if there are strong seasonal patterns!
- Plot Decomposed time series object: decomposing allows us to view a time series object in components (four components see website for more information). Further discussion can be seen in the project, but when we decompose our time series objects we get a glimpse of its seasonal and trend components independently. 

- Seasonal Plot: The name speaks for itself but this plot is a great way to check for seasonal components which is something common when dealing with yearly, quarterly and monthly data. 

These plots will help us in our Box-Jenkins Model estimation, as well as doing transformations such as differencing (and taking the log if necessary) of our time series objects to take into consideration non-stationarity and heteroskedasticity respectively. 

### Autocorrelation and Partial Autocorrelation Plots
These plots play a crucial role in time series analysis, because we can estimate our ARIMA model based on the behaviour of these plots or justify the need to do an appropriate transformation.  

We won't go into too much detail since we outlined the process in the project, but through the use of our ACF and PACF plots for our original time series we were able to make the deduction to take the first difference of our time series. Once we did that we saw that the ACF and PACF plot showed characteristics of a MA(1) model, but since we took the first difference it becomes a mixed model; ARIMA(0, 1, 1)

From here we do residual diagnostics to see if our model displays residuals that are white noise. 

### Residual Diagnostics 






