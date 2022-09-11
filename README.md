# timeSeries_Shiny
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(xts)
library(plotly)
library(fpp2)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forecast)
library(tseries)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  navbarPage("Time Series Forecasting",
             tabPanel("Data",sidebarLayout(
               sidebarPanel(
                 h4("Import and Build Dataset"),
                 fileInput("file1", "Choose a File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                           
                 ),
                 radioButtons("per","Choose periodicity ",c("Daily","Monthly","Yearly"))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("View",DT::dataTableOutput("dt")),
                   tabPanel("Description",verbatimTextOutput("des")),
                   tabPanel("Summary",DT::dataTableOutput("sum"))
                 )
                 
               )
             )
             ),
             tabPanel("Visualization",sidebarLayout(
               sidebarPanel(h4("Take a look at your data"),radioButtons("type","Choose the function",c("Normal","log","diff","log_diff")),br(),selectInput("gg","Choose a variable","not_selected"))
               ,
               mainPanel(
                 plotlyOutput("plot1"),
                 plotlyOutput("plot2")
               )
             )),
             tabPanel("Statistics",tabsetPanel(
               tabPanel("ACF",sidebarLayout(
                 sidebarPanel(h4("Analyse your data"),radioButtons("type1","Choose the function",c("Normal","log","diff","log_diff")),selectInput("cc","Choose a variable","not_selected")),
                 mainPanel(plotlyOutput("plot3"))
               )),
               tabPanel("PACF",sidebarLayout(
                 sidebarPanel(h4("Analyse your data"),radioButtons("type2","Choose the function",c("Normal","log","diff","log_diff")),selectInput("pp","Choose a variable","not_selected")),
                 mainPanel(plotlyOutput("plot4"))
               ))
               ,
               tabPanel("Ljung-Box-test",h4("Determine whether a series of observations over time are random and independent,Autocorrelation can affect the accuracy of a time-based forecasting model, such as a time series plot, and lead to misinterpretation of data."),sidebarLayout(
                 sidebarPanel(selectInput("bb","Choose a variable","not_selected")),
                 mainPanel(verbatimTextOutput("boxt"))
               )
               ))
               
             ),
             navbarMenu("Models",
                        tabPanel("ARIMA",sidebarLayout(
                          sidebarPanel(h4("ARIMA MODEL"),radioButtons("type3","Choose the function",c("Normal","log","diff","log_diff")),sliderInput("train", "Choose the train set",
                                                                                                                                                     min = 0.5,max = 0.9,value = 0.75),
                                       selectInput("s1","Choose a variable","not_selected")),
                          mainPanel(tabsetPanel(
                            tabPanel("CheckResiduals",plotOutput("plot5")),
                            tabPanel("Prediction",sidebarLayout(
                              sidebarPanel("Forecast",numericInput("n1","h = ",value ="1")),
                              mainPanel(
                                plotOutput("pre1")))
                            ),
                            tabPanel("Summary", 
                                     plotOutput("sum2"),
                                     plotOutput("sum3"),
                                     verbatimTextOutput("sum1"))
                          )))),
                        tabPanel("Exponential smoothing",sidebarLayout(sidebarPanel(h4("Exponential smoothing"),radioButtons("type4","Choose the function",c("Normal","log","diff","log_diff")),sliderInput("train1", "Choose the train set",
                                                                                                                                                                                                            min = 0.5,max = 0.9,value = 0.75),selectInput("s2","Choose a variable","not_selected")),
                                                                       mainPanel(tabsetPanel(
                                                                         tabPanel("CheckResiduals",h4("Check if residuals are White noise or not"),plotOutput("plot6")),
                                                                         tabPanel("Prediction",sidebarLayout(
                                                                           sidebarPanel("Forecast",numericInput("n2","h = ",value ="1")),
                                                                           
                                                                           mainPanel(
                                                                             plotOutput("pre2"), 
                                                                             DT::dataTableOutput("acc2")))
                                                                         ))))),
                        tabPanel("TBATS",sidebarLayout(sidebarPanel(h4("TBATS: Trigonometric seasonality, Box-Cox transformation, ARMA errors, Trend and Seasonal components. The final model will be chosen using Akaike information criterion (AIC)"),radioButtons("type5","Choose the function",c("Normal","log","diff","log_diff")),sliderInput("train2", "Choose the train set",
                                                                                                                                                                                                                                                                                                                                                    min = 0.5,max = 0.9,value = 0.75),selectInput("s3","Choose a variable","not_selected")),
                                                       mainPanel(tabsetPanel(
                                                         tabPanel("CheckResiduals",h4("Check if residuals are White noise or not"),plotOutput("plot7")),
                                                         tabPanel("Prediction",sidebarLayout(
                                                           sidebarPanel("Forecast",numericInput("n3","h = ",value ="1")),
                                                           
                                                           mainPanel(
                                                             plotOutput("for3")))
                                                         )))))
             )
  ),
  
  theme=shinytheme("superhero")
)


# Define server logic required to draw a histogram
server <- function(session,input, output) {
  my_data<-reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tbl <- read.csv(inFile$datapath)
    dates<- as.Date(tbl$Date, format = "%Y-%m-%d")
    tbl<-tbl[,sapply(tbl, is.numeric)]
    return(tbl)
  })
  output$dt<-DT::renderDataTable({
    df <- my_data()
    DT::datatable(df)
  })
  
  output$sum<-DT::renderDataTable({
    switch(input$per,
           Daily=summary(my_data()),
           Monthly=summary(to.monthly(my_data())),
           Yearly=summary(to.yearly(my_data()))
    )
  })
  output$des<-renderPrint({
    switch(input$per,
           Daily=str(my_data()),
           Monthly=str(to.monthly(my_data())),
           Yearly=str(to.yearly(my_data()))
    )
  })
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       'gg', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  output$plot1<-renderPlotly({
    dft<-ts(my_data())
    dfts<-dft[,input$gg]
    lg<-log(dfts)
    dff<-diff(dfts)
    lgd<-log(dff)
    switch(input$type,
           Normal=autoplot(dfts,main = "Analyse your data") + geom_area(alpha=0.15),
           log=autoplot(lg,main = "Analyse your data") + geom_area(alpha=0.15),
           diff=autoplot(dff,main = "Analyse your data") + geom_area(alpha=0.15),
           log_diff=autoplot(lgd,main = "Analyse your data") + geom_area(alpha=0.15)
           
    )
  })
  output$plot2<-renderPlotly({
    dfts<-ts(my_data())
    lg<-log(dfts)
    dff<-diff(dfts)
    lgd<-log(dff)
    switch(input$type,
           Normal=autoplot(dfts,main = "Analyse your data") + geom_area(alpha=0.15),
           log=autoplot(lg,main = "Analyse your data") + geom_area(alpha=0.15),
           diff=autoplot(dff,main = "Analyse your data") + geom_area(alpha=0.15),
           log_diff=autoplot(lgd,main = "Analyse your data") + geom_area(alpha=0.15)
           
    )
  })
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       'cc', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  
  output$plot3<-renderPlotly({
    dft<-ts(my_data())
    dfts<-dft[,input$cc]
    lg<-log(dfts)
    dff<-diff(dfts)
    lgd<-log(dff)
    switch (input$type1,
            Normal =ggAcf(dfts,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            log =ggAcf(lg,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            diff=ggAcf(dff,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            log_diff=ggAcf(lgd,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE)
    )
  })
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       'pp', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  
  output$plot4<-renderPlotly({
    dft<-ts(my_data())
    dfts<-dft[,input$pp]
    lg<-log(dfts)
    dff<-diff(dfts)
    lgd<-log(dff)
    switch (input$type2,
            Normal =ggAcf(dfts,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            log =ggAcf(lg,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            diff=ggAcf(dff,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE),
            log_diff=ggAcf(lgd,lag.max = NULL,type = c("correlation", "covariance", "partial"),plot = TRUE)
    )
  })
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       'bb', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  output$boxt<-renderPrint({
    dft<-ts(my_data())
    dfts<-dft[,input$bb]
    Box.test(dfts,lag=24,fitdf=0,type="Ljung-Box")
  })
  output$periodicity<-renderPrint({
    periodicity(my_data())
  })
  output$nquar<-renderPrint({
    nquarters(my_data())
  })
  output$nyear<-renderPrint({
    nyears(my_data())
  })
  
  ###Models:
  ##Train and test set:
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       's1', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  sar <- reactive({
    dft<-my_data()
    dfts<-dft[,input$s1]
    train<-ts(head(dfts, round(length(dfts)*input$train)))
    switch(input$type3,
           Normal = auto.arima( train ),
           log = auto.arima(log(train)),
           diff = auto.arima(diff(train)),
           log_diff = auto.arima(log(diff(train))))
    
    
    
  })
  
  output$plot5<-renderPlot({
    plot(sar()$residuals)
    
  })
  output$sum1 <- renderPrint({
    summary(sar())
  })
  output$sum2 <- renderPlot({
    acf(sar()$residuals )
    
  })
  output$sum3 <- renderPlot({
    pacf(sar()$residuals)
  })
  output$pre1 <- renderPlot({
    plot(forecast(sar(), h = input$n1, shadecols = "oldstyle"))
  })
  
  
  #exp
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       's2', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  exp <- reactive({
    dft<-my_data()
    dfts<-dft[,input$s2]
    train<-ts(head(dfts, round(length(dfts)*input$train1)))
    switch(input$type4,
           Normal = HoltWinters(train,alpha=NULL,beta=NULL,gamma=FALSE,seasonal = c("additive", "multiplicative")),
           log = HoltWinters(log(train),alpha=NULL,beta=NULL,gamma=FALSE,seasonal = c("additive", "multiplicative")),
           diff = HoltWinters(diff(train),alpha=NULL,beta=NULL,gamma=FALSE,seasonal = c("additive", "multiplicative")),
           log_diff = HoltWinters(log(diff(train)),alpha=NULL,beta=NULL,gamma=FALSE,seasonal = c("additive", "multiplicative")))
  })
  output$plot6<-renderPlot({
    checkresiduals(exp(), lag, df = NULL, test=FALSE, plot = TRUE)
    
  })
  output$pre2 <- renderPlot({
    plot(forecast(exp(),h =input$n2))
  })
  
  #tbats
  observeEvent(my_data(), {
    
    mySeries <- my_data()
    
    updateSelectInput( session,
                       's3', 
                       label = 'Select Variable :',
                       choices = names(mySeries)
    )
    
  })
  tbatss <- reactive({
    dft<-my_data()
    dfts<-dft[,input$s3]
    train<-ts(head(dfts, round(length(dfts)*input$train2)))
    switch (input$type5,
            Normal =tbats(train,use.box.cox = TRUE,use.trend = NULL,use.damped.trend = NULL,use.arma.errors=TRUE) ,
            log=tbats(log(train),use.box.cox = TRUE,use.trend = NULL,use.damped.trend = NULL,use.arma.errors=TRUE),
            diff=tbats(diff(train),use.box.cox = TRUE,use.trend = NULL,use.damped.trend = NULL,use.arma.errors=TRUE),
            log_diff=tbats(log(diff(train)),use.box.cox = TRUE,use.trend = NULL,use.damped.trend = NULL,use.arma.errors=TRUE)
    )
  })
  
  output$plot7<-renderPlot({
    checkresiduals(tbatss(), lag, df = NULL, test=FALSE, plot = TRUE)
  })
  
  
  output$for3<-renderPlot({
    plot(forecast(tbatss(), h = input$n3))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
