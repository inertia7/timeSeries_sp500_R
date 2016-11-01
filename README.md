# Time Series Analysis on S&P 500 Stock Prices (1995-2015)

## ABSTRACT
This project focuses on using univariate time series forecasting methods for the stock market index, Standard & Poor's 500 (abbreviated commonly as S&P 500, which is the notation we will use in this project). We went about the time series analysis was through using R and R studio to both predict and visualize our predictions. Along with the interactivity of plotly through the [ggplot2 package](https://github.com/tidyverse/ggplot2) we were able to create stunning visuals that help in understanding which time series forecasting method is most appropriate for your own time series analysis. 
# Packages Required
Here are the required packages which will ensure all the code will run properly. The process to ensure you have the respective package in your system is to run: 

	install.packages("packageName") 

You will have now downloaded the package so within your script you run: 

	require(packageName)

This must be done before each Rstudio session, and written at the start of every script to ensure your code will be easily reproducible!
## Steps Required 

### Create plotly Account (Optional)	
If you would like to have the images you create (using plotly and ggplot2) published so that you can customise the plots to your liking or brag about the interactivety of your visuals simply create a [plolty account](https://plot.ly/). Once you do so you will have access to your username and more importantly your API key, these will be necessary to publishing your plots (If you do not wish to publish your plots skip this step). 

Important to note, when posting on GitHub never publish API keys (this is a common mistake I see people do). Once you gain access to your API key, have plotly in your current working directory, you run:

	Sys.setenv("plotly_username"="userName")
	Sys.setenv("plotly_api_key"="d1X4Hrmbe")

From here you will be able to publish your ggplotly visuals by running (for example our ggplot2 object is called timeSeriesPlot):

	plotly_POST(timeSeriesPlot, filename = "timeSeriesPlot")
