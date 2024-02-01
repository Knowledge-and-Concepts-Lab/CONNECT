library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Testing a stopwatch"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(12, switchInput(inputId="goflag", value = FALSE)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(6, textOutput("flagstate")),
        column(6, textOutput("counter"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cval <- reactiveVal(0)
  output$counter <- renderText({
    if(input$goflag) invalidateLater(100)
    isolate(cval(cval() + 1))
    paste("Counter value is:", cval())
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
