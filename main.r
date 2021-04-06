library(shiny)
shinyServer(
  pageWithSidebar(
    headerPanel("A hybrid model for Rainfall Prediction"),
    sidebarPanel(

      selectInput("Algorithm", "Select the Algorithm",
        choices = c("Select", "ARIMA Model", "HoltWinters Model")),

      sliderInput("year", "Select the Year ",
        min = 2015, max = 2050, value = 2015, step = 1),
      selectInput("month", "Select the Month",
        choices = c("Select", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) #selectInput("month", "Select the Year to be predicted",
        # choices = c(2015: 2050)) 
        #textInput("year", "Select the Year to be predicted", 2015)
    ),
    mainPanel(
      plotOutput("myPlot")
    )
  )
)
