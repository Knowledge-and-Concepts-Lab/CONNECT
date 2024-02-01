library(shiny)
library(shinyWidgets)

#Set global structures:
room.wts <- as.matrix(read.csv("./data/room_weights.csv", header = T, row.names = 1))
unames <- row.names(room.wts)
fromnames <-colnames(room.wts)

# UI definition
ui <- fluidPage(

    # Application title
    #titlePanel("Room Schema Model"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          h3("Room schema"),
          fluidRow(
            column(6,
              checkboxGroupInput("sela", NULL,
                choices = unames[c(0:19)*2 + 1]
                )
            ),
            column(6,
              checkboxGroupInput("selb", NULL,
                choices = unames[c(1:20)*2]
              )
            )
          ),
          fluidRow(
            column(6, numericInput("ts", "timesteps", value=20, min=1)),
            column(6, numericInput("dt", "dt", value=0.2, min=0.0, max=1.0))
          ),
          fluidRow(
            column(6, actionButton("run","Run", width="100%")),
            column(6, actionButton("reset","Reset", width="100%"))
          ),
          br(),
          fluidRow(
            column(12, switchInput("wtflag","View weights", labelWidth="200px")),
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            column(12,align = "left", textOutput("unit_info"))
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("modelPlot", NULL, width = '100%', height = '800px', hover="plot_hover"),
           fluidRow(column(12, align='center',
                      textOutput("lastgval")
                      )
            )
        )
    )
)

#Server logic:
server <- function(input, output) {
  #Get functions
  source("./rsapp_functions.r")
  
  #Define reactive values
  dt <- reactiveVal(value=0.2)
  ts <- reactiveVal(value=20)
  gvals <-reactiveVal(rep(0, times = 20))
  ext.inputs <- reactiveVal(rep(0, times = 40))
  
  #Initialize activation matrix:
  tmp <- update.acts(rep(0, times = 40), room.wts)
  tmp[,] <- 0
  amat <- reactiveVal(tmp)

  #Hover function:
  output$unit_info <- renderPrint({
    x <- as.numeric(input$plot_hover$x)
    y <- as.numeric(input$plot_hover$y)
    
    if(input$wtflag){ #If weights are showing:
      to <- floor(x * 39-.5) + 2
      from <- 40 - floor(y * 40 - .5)

      if(length(x)==0 | length(y)==0){
        o <- "no connection selected"
      } else{
        o <- paste0("from ", fromnames[from], " to ", unames[to], ": ", round(room.wts[to, from],3))
      }
    } else{ #If network is showing
      #If cursor not on plot set x and y to -1
      if(length(x)==0 | length(y)==0){
        x <- -1
        y <- -1
      }
      x <- floor(x)
      y <- floor(y) + 1
      if(x < 1 | x > ts() | y < 1 | y > 40 | length(x)==0 | length(y) ==0){ 
        o <- "no unit selected"
      } else{
        o <- paste(unames[41 - y], ": ", round(amat()[41 - y,x], 3))
        #o <- c(length(x), length(y))
      }
    }
    cat(o)
  }
  )
  #Click run button:
  observeEvent(input$run,{
    #render the network    
    selected <- c(input$sela, input$selb)
    tmp <- rep(0, times = 40)
    tmp[match(selected, unames)] <- 1
    ext.inputs(tmp)
    amat(update.acts(ext.inputs(), room.wts, dt = dt(), timesteps=ts()))
    tmp <- rep(0, times = ts())
    for(i in c(1:ts())) tmp[i] <- goodness(ext.inputs(), amat()[,i], room.wts)
    gvals(tmp)
  })
  
  #Click reset button
  observeEvent(input$reset,{
    #render the network with null values
    ext.inputs(rep(0, times = 40))
    nullmat <- matrix(0, 40, ts()) #Matrix of zeros
    row.names(nullmat) <- unames   #Unit names
    amat(nullmat)
  })


  #Set values when numeric input changes
  observeEvent(input$dt,{
    #Set dt
    dt(input$dt)
  })
  
  observeEvent(input$ts,{
    #Set timesteps
    ts(input$ts)
  })
  
  #Render model plot
  output$modelPlot <- renderPlot({
    if(input$wtflag){
      plot.wts(room.wts)
      output$lastgval <- renderText(c(""))
    } else{
      render.network(ext.inputs(), amat())
      if(sum(amat()) != 0){
        ostr <- paste("Final G =", round(gvals()[ts()], 3)) 
        mtext(side = 1, line = -1, at = c(1:ts()) + .5, text = round(gvals()), cex = 30/ts())
      } else{
        mtext(side = 1, line = -1, at = c(1:ts()) + .5, text = "", cex = 30/ts())
        ostr <- c("")
      }
      output$lastgval <- renderText(ostr)
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
