#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(here)

source(here('src', 'helper_functions.R'))
here()

data_master <- read_csv(here("data", "data_master_1.csv"))

# Create initial time series object 
sp500 <- ts(data_master$sp_500, start = c(1995, 1), frequency = 12)

# Create test set
sp500_test <- window(sp500, 2015, c(2015, 12))

fit_arima <- readRDS(here("models", 'arima.rds'))
fit_BC <- readRDS(here("models", 'box_cox.rds'))
fit_net <- readRDS(here("models", 'neural_net.rds'))
fit_meanf <- readRDS(here("models", 'meanf.rds'))
fit_naive <- readRDS(here("models", 'naive.rds'))
fit_snaive <- readRDS(here("models", 'snaive.rds'))
fit_ets <- readRDS(here("models", 'ets.rds'))

## app.R ##

ui <- dashboardPage(
  dashboardHeader(title = "Inertia7 - Time Series Dashboard",
                  titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('
    /* logo */
        .skin-blue .main-header .logo {
                              background-color: #006b6f;
                              font-family: Courier;
                              }
    /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #00868B;
                              }'))),
    fluidRow(
      column(width = 8,
             box(plotlyOutput("forecast_plots"),
                 width = NULL),
             box(plotOutput("diag_plots"),
                 width = NULL)),
      column(width = 4,
             box(selectInput("forecast", "Choose Forecast Method:",
                             c("ARIMA" = "fit_arima",
                               "Box-Cox Transformation" = "fit_BC",
                               "Exponential Smoothing" = "fit_ets",
                               "Mean Forecasting" = "fit_meanf",
                               "Naive Forecasting" = "fit_naive",
                               "Seasonal Naive Forecasting" = "fit_snaive",
                               "Neural Networks" = "fit_net")),
                 width=NULL),
             box(DT::dataTableOutput("accuracy_table"),
                 width=NULL),
             box(verbatimTextOutput('test_print'),
                 width=NULL))
    )
  )
)

server <- function(input, output) {
  output$forecast_plots <- renderPlotly({
    if (input$forecast == "fit_arima") {
      autoplot(fit_arima,
                            holdout = sp500_test,
                            ts_object_name = 'ARIMA')
    } else if (input$forecast == "fit_BC") {
      autoplot(fit_BC,
               holdout = sp500_test,
               ts_object_name = 'Box-Cox Transformation')
    } else if (input$forecast == "fit_ets") {
      autoplot(fit_ets,
               holdout=sp500_test,
               ts_object_name = "Exponential Smoothing")
    } else if (input$forecast == "fit_meanf") {
      autoplot(fit_meanf,
               holdout = sp500_test,
               ts_object_name = 'Mean')
    } else if (input$forecast == "fit_naive") {
      autoplot(fit_naive,
               holdout = sp500_test,
               ts_object_name = "Naive")
    } else if (input$forecast == "fit_snaive") {
      autoplot(fit_snaive,
               holdout = sp500_test,
               ts_object_name = "Seasonal Naive")
    } else if (input$forecast == "fit_net") {
      autoplot(fit_net,
               holdout = sp500_test,
               ts_object_name = 'Neural Networks')
    }
  })
  output$diag_plots <- renderPlot({
    if (input$forecast == "fit_arima") {
      ggtsdiag_custom(fit_arima$model, ts_object_name = "ARIMA Model") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "fit_BC") {
      ggtsdiag_custom(fit_BC$model, ts_object_name = "Box-Cox Transformation Model") +
                     theme(panel.background = element_rect(fill = "gray98"),
                           panel.grid.minor = element_blank(),
                           axis.line.y = element_line(colour="gray"),
                           axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "fit_ets") {
      ggtsdiag_custom(fit_ets$model, ts_object_name = "Exponential Smoothing Model") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "fit_meanf") {
      ggtsdiag_custom(fit_meanf, ts_object_name = "Mean Forecast Model") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "fit_naive") {
      ggtsdiag_custom(fit_naive, ts_object_name = "Naive Forecast Model") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "fit_snaive") {
      ggtsdiag_custom(fit_snaive, ts_object_name = "Seasonal Naive Forecast Model") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    } else if (input$forecast == "fit_net") {
      ggtsdiag_custom(fit_net, ts_object_name = "Neural Networks Model") +
        theme(panel.background = element_rect(fill = "gray98"),
              panel.grid.minor = element_blank(),
              axis.line.y = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray"))
    }
  })
  output$accuracy_table = DT::renderDataTable({
    if (input$forecast == "fit_arima") {
      acc_arima <- round(accuracy(fit_arima, sp500_test), 4)
      t(acc_arima)
    } else if (input$forecast == "fit_BC") {
      acc_box <- round(accuracy(fit_BC, sp500_test), 4)
      t(acc_box)
    } else if (input$forecast == "fit_ets") {
      acc_ets <- round(accuracy(fit_ets, sp500_test), 4)
      t(acc_ets)
    } else if (input$forecast == "fit_meanf") {
      acc_meanf <- round(accuracy(fit_meanf, sp500_test), 4)
      t(acc_meanf)
    } else if (input$forecast == "fit_naive") {
      acc_naive <- round(accuracy(fit_naive, sp500_test), 4)
      t(acc_naive)
    } else if (input$forecast == "fit_snaive") {
      acc_snaive <- round(accuracy(fit_snaive, sp500_test), 4)
      t(acc_snaive)
    } else if (input$forecast == "fit_net") {
      acc_net <- round(accuracy(fit_net, sp500_test), 4)
      t(acc_net)
    }
  })
  output$test_print = renderPrint({
    if (input$forecast == "fit_arima") {
      fit_arima$model
    } else if (input$forecast == "fit_BC") {
      print(fit_BC$method)
      print(fit_BC$model)
    } else if (input$forecast == "fit_ets") {
      fit_ets$model
    } else if (input$forecast == "fit_meanf") {
      print(fit_meanf$model)
    } else if (input$forecast == "fit_naive") {
      print(fit_naive$method)
      print(fit_naive$model)
    } else if (input$forecast == "fit_snaive") {
      print(fit_snaive$method)
      print(fit_snaive$model)
    } else if (input$forecast == "fit_net") {
      print(fit_net$method)
      print(fit_net$model)
    }
  })
}

shinyApp(ui, server)
