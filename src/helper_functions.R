# Helper Function Script

#RUN THESE COMMANDS IF THESE THIRD PARTY PACKAGES HAVE NOT BEEN DOWNLOADED YET

# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("plotly")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("gridExtra")
# install.packages("docstring")
# install.packages("here")

library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)


# NOTE: For more information on helper functions use ?function_name
# LOAD DATA
data_master <- read.csv(here("data", "data_master_1.csv"))

data_master$year <- NULL
data_master$month <- NULL
data_master$year <- NULL
data_master$trillion <- NULL
data_master$billion <- NULL

create_ts <- function(x){
  ts(x, start = c(1995, 1), frequency = 12)
}

data_master <- lapply(data_master, create_ts)

# Contains functions that create ggplot2 plots of integral time series plots
# use ?function_name for more details.

plot_time_series <- function(ts_object, ts_object_name){
  #' Plot Time Series Object
  #'
  #' Creates time series plot utilizing \code{ggplot2} utlizing
  #' custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_time_series(air_pass_ts, 'Air Passengers')
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      startYear <- start(ts_object) # Grabs start date
      endYear <- end(ts_object) # Grabs end date
      tsPlot <- autoplot(ts_object,
                         ts.colour = 'turquoise4',
                         size = 1,
                         main = sprintf("Plot of %s Time Series (%s - %s)",
                                        ts_object_name, startYear[1], endYear[1])) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1),
              panel.background = element_rect(fill = "gray98"),
              axis.line.x = element_line(colour="gray"),
              axis.line.y = element_line(colour="gray")) +
        labs(x = "Year", y = "Closing Values") 
      return(tsPlot)
    }
  }
  else {
    warning('Make sure object entered is time-series object!')
  }
}

# FUNCTION FOR ACF AND PACF PLOTS
plot_acf_pacf <- function(ts_object, ts_object_name){
  #' Plot ACF and PACF for Time Series Object
  #'
  #' Creates \emph{Autocorrelation} and \emph{Partial Autocorrelation} plot
  #' utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_acf_pacf(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      a <- autoplot(acf(ts_object, plot = FALSE),
                    colour = 'turquoise4',
                    conf.int.fill = '#4C4CFF',
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) +
        ggtitle(sprintf("ACF plot of %s", ts_object_name))

      b <- autoplot(pacf(ts_object, plot = FALSE),
                    colour = 'turquoise4',
                    conf.int.fill = '#4C4CFF',
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) + labs(y="PACF") +
        ggtitle(sprintf("PACF plot of %s", ts_object_name))

      grid.arrange(a, b)
    }
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

# Decomposed Plot
plot_decomp <- function(ts_object, ts_object_name){
  #' Plots Seasonal Decomposition for Time Series Object
  #'
  #' Decomposes time series object to \emph{Seasonal},
  #' \emph{Remainder}, and \emph{Trend}.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_decomp(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    autoplot(stl(ts_object, s.window = "periodic"),
             main = sprintf("Decomposition Plot of %s", ts_object_name),
             ts.colour = "turquoise4") +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y   = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

# Seasonal Plot
plot_seasonal <- function(ts_object, ts_object_name){

  #' Plots Seasonal Component for Time Series Object
  #'
  #' Plots \emph{Seasonal} aspect of time series object.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_seasonal(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    ggseasonplot(ts_object, xlab="Year",
                 main=sprintf("Seasonal Plot of %s", ts_object_name),
                 year.labels=TRUE, year.labels.left=TRUE,
                 col=1:20, pch=19) +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

ggtsdiag_custom <- function(object, ts_object_name, gof.lag = 10,
                            conf.int = TRUE,
                            conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                            conf.int.fill = NULL, conf.int.alpha = 0.3,
                            ad.colour = '#888888', ad.linetype = 'dashed', ad.size = .2,
                            nrow = NULL, ncol = 1, ...) {
  rs <- stats::residuals(object)
  if (is.null(rs)) {
    rs <- object$residuals
  }
  if (is.null(rs)) {
    rs <- object$resid
  }

  p.std <- ggplot2::autoplot(rs, na.action = stats::na.pass,
                             ts.colour = 'turquoise4', size = 1.05) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = ad.linetype, size = ad.size,
                        colour = ad.colour) +
    labs(subtitle = '') +
    ggplot2::ggtitle(sprintf("Residual Diagnostics for %s \nNon-Standardized Residuals",
                             ts_object_name))

  acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
  p.acf <- autoplot(acfobj, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill,
                    conf.int.alpha = conf.int.alpha,
                    colour = 'turquoise4', size = 1.25)
  p.acf <- p.acf + ggplot2::ggtitle('ACF of Residuals')

  nlag <- gof.lag
  pval <- numeric(nlag)
  for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
  lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval,
                      lower = -0.05, upper = 0.05)
  # Unnable to create column with space by above expression
  colnames(lb.df) <- c('Lag', 'p value', 'lower', 'upper')
  p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(y = '`p value`'), na.rm = TRUE,
                        colour = 'turquoise4') +
    ggplot2::scale_y_continuous(limits=c(-0.1, 1)) +
    ggplot2::ggtitle('p values for Ljung-Box statistic')

  p.lb <- ggfortify:::plot_confint(p = p.lb, data = lb.df, conf.int = conf.int,
                                   conf.int.colour = conf.int.colour,
                                   conf.int.linetype = conf.int.linetype,
                                   conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)

  if (is.null(ncol)) { ncol <- 0 }
  if (is.null(nrow)) { nrow <- 0 }
  new('ggmultiplot', plots = list(p.std, p.acf, p.lb), nrow = nrow, ncol = ncol)
}


###########################################################################################
# HERE FOUND AT http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/ #
# BY DREW SCHMIDT WITH SLIGHT MODIFICATIONS TO FIT OUR PLOTS                              #
###########################################################################################


autoplot.forecast <- function(forecast, forc_name, ts_object_name, 
                              ..., holdout=NaN){
  #' Plots Forecasted values for Time Series Models
  #'
  #' Borrowed from Drew Schmidt, but modified to fit our aesthetic appeal
  #' we create a dataframe with all the appropriate sections (i.e. upper and
  #' lower 95% CI bands, forecasted, actual values, the training time series
  #' object, and upper and lower 80% CI), the we create a \code{ggplot2} object that
  #' reflects the forecasted plot
  #'
  #' @param forecast forecasted values created using \code{forecast} function
  #' @param forc_name name of forecasted method included in title
  #' @param ts_object_name time series name included in title
  #' @param holdout time series object that contains actual values that can be
  #' compared to the forecasted values

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
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
    scale_y_continuous("")  +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          panel.background = element_rect(fill = "gray98"),
          axis.line.y   = element_line(colour="gray"),
          axis.line.x = element_line(colour="gray")) +
    labs(x = "Year", y = "Closing Values") +
    ggtitle(sprintf('%s Forecast Plot of %s', forc_name, ts_object_name))
}

###########################################################################################
