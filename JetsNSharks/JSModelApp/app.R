library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)

js.wts <- as.matrix(read.csv("./data/JS_wts.csv", header = T, row.names = 1))
jscoords <- read.csv("./data/JS_unit_coords.csv", header = T, row.names = 1)
unames <- row.names(js.wts)



# UI definition
ui <- fluidPage(
  
# Application title
#titlePanel("Jets N Sharks"),

# Top panel:
  wellPanel(
    fluidRow(
      column(12, 
          checkboxGroupButtons(
            inputId="sel_id", 
            label = "Identity node", 
            choices=unames[28:54],
            size="sm",
            justified=TRUE,
            direction="horizontal"
          )
      )
    ),
    fluidRow(
      column(12, 
         checkboxGroupButtons(
           inputId="sel_name", 
           label = "Name", 
           choices=unames[1:27], 
           size="sm",
           justified=TRUE,
           direction="horizontal")
      )
    ),
    fluidRow(
      column(2, 
             checkboxGroupButtons(
               inputId="sel_gang", 
               label = "Gang", 
               choices=unames[55:56],
               size="sm",
               justified=TRUE,
               direction="horizontal")
             ),
      column(2, 
            checkboxGroupButtons(
              inputId="sel_age", 
              label = "Age", 
              choices=unames[57:59], 
              size="sm",
              justified=TRUE,
              direction="horizontal")
            ),
      column(2, 
             checkboxGroupButtons(
               inputId="sel_ed", 
               label = "Education", 
               choices=unames[60:62], 
               size="sm",
               justified=TRUE,
               direction="horizontal")
             ),
      column(3, 
            checkboxGroupButtons(
              inputId="sel_rel", 
              label = "Relationship", 
              choices=unames[63:65], 
              size="sm",
              justified=TRUE,
              direction="horizontal")
            ),
      column(3, 
             checkboxGroupButtons(
               inputId="sel_job", 
               label = "Job", 
               choices=unames[66:68], 
               size="sm",
               justified=TRUE,
               direction="horizontal")
      )
     )
    ),
    
  # Plot panel:
  fluidRow(
    column(2,
       wellPanel(
         fluidRow(
           column(6, numericInput("ts", "timesteps", value=20, min=1)),
           column(6, numericInput("dt", "dt", value=0.1, min=0.0, max=1.0))
         ),
         fluidRow(
           column(6, actionBttn("run","Run", width="100%", style = "unite", color="primary", block=TRUE, size = "sm")),
           column(6, actionBttn("reset","Reset", width="100%", style = "unite", color="primary", block=TRUE, size = "sm"))
         ),
         br(),
       fluidRow(
         column(2, actionBttn("stback", label="<", style = "unite", size = "sm", color = "primary")),
         column(2, actionBttn("stfor", label = ">", style = "unite", size = "sm",color = "primary")),
         column(4, actionBttn("jmpback", label = "<<", style = "unite", size = "sm", block = TRUE,color = "primary")),
         column(4, actionBttn("jmpfor", label = ">>", style = "unite", size = "sm", block = TRUE,color = "primary")),
       ),
       br(),
       fluidRow(
         column(12, switchInput("loopflag", label="Loop", labelWidth="100%"))
       ),
       br(),
       fluidRow(
         column(12, align="center", textOutput("stepcounter"))
       ),
       hr(style = "border-top: 1px solid #000000;"),
       fluidRow(
         column(12,align = "left", textOutput("unit_info"))
        ),
      hr(style = "border-top: 1px solid #000000;"),
      fluidRow(
        column(12, switchInput("wtflag", label = "Show weights", labelWidth = "100px", size = "sm"))
        )
       )
      ),
    column(10,
      wellPanel(
        plotOutput("modelPlot", NULL, width = '100%', height = '600px', 
                   hover="plot_hover", click="plot_click")
      )
    )
  )
)

#Server logic:
server <- function(input, output) {
  #Get functions
  source("./js_functions.r")
  
  #Fixed values
  blank.input <- as.data.frame(matrix(0,68,1))
  row.names(blank.input) <- unames
  
  #Reactive values:
  dt <- reactiveVal(value=0.2)
  ts <- reactiveVal(value=20)
  currtime <- reactiveVal(value = 1)
  amat <- reactiveVal(update.acts(rep(0, times = 68), js.wts))
  wmat <- reactiveVal(js.wts) #Editable weights to use / display
  
  #Hover function:
  output$unit_info <- renderPrint({
    
    if(input$wtflag){
      x <- as.numeric(input$plot_hover$x)
      y <- as.numeric(input$plot_hover$y)
      to <- floor(x * 67-.5) + 2
      from <- 67 - floor(y * 67 - .5)
      
      if(length(x)==0 | length(y)==0){
        o <- "no connection selected"
      } else{
        o <- paste0("from ", unames[from], " to ", unames[to], ": ", wmat()[from, to])
#        o <- c(from, to)
      }
    } else{
      dx <- abs((input$plot_hover$x - 1) - jscoords$X + .5)
      dy <- abs((input$plot_hover$y - 1) - jscoords$Y + .5)
      s <- (dx < 0.5) & (dy < 0.5)
      if(sum(s)!=1){ 
        o <- "no unit selected"
      } else{
      #un <- unames[s]
      o <- paste(unames[s], ": ", round(amat()[s,currtime()], 3))
      if(currtime()==0) o[1,1] <- "0"
      }
    }
    cat(o)
  }
  )
  
  #Click weights to lesion/restore:
  observeEvent(input$plot_click,{
    if(input$wtflag){
      x <- as.numeric(input$plot_hover$x)
      y <- as.numeric(input$plot_hover$y)
      to <- floor(x * 67-.5) + 2
      from <- 67 - floor(y * 67 - .5)
      
      if(length(x)==0 | length(y)==0){
        o <- "no connection selected"
      } else{
        tmp <- wmat() #Copy current weight matrix
        if(tmp[from, to] == 0) tmp[from, to] <- jswts[from, to] else tmp[from, to] <- 0
        if(tmp[to, from] == 0) tmp[to, from] <- jswts[to, from] else tmp[to, from] <- 0
        wmat(tmp)
      }
    } 
  })
  
  #Click the run button
  observeEvent(input$run,{
    #Compute input vector
    ext.inputs <- rep(0, times = 68)
    selected <- c(input$sel_id, input$sel_name, input$sel_gang, input$sel_age, input$sel_ed, input$sel_rel, input$sel_job)
    ext.inputs[match(selected, unames)] <- 1
    
    #Update activation matrix:
    amat(update.acts(ext.inputs, wmat(), dt = dt(), timesteps=ts()))
    currtime(1) #Set current time to 1

  })
  
  #Step forward button:
  observeEvent(input$stfor,{
    #increment current time
    currtime(currtime()+1) 
    if(currtime() > ts()) currtime(ts())
  })

  #Step back button:
  observeEvent(input$stback,{
    #decrement current time
    currtime(currtime()-1) 
    if(currtime() < 1) currtime(1)
})
  
  #Jump forward button:
  observeEvent(input$jmpfor,{
    #increment current time
    currtime(ts()) 
  })
  
  #Jump back button:
  observeEvent(input$jmpback,{
    #decrement current time
    currtime(1) 
  })

  #Reset button:
  observeEvent(input$reset,{
    
    currtime() #reset current time
    #Update with blank input
    amat(update.acts(blank.input[,1], wmat(), dt(), ts()))
  })
  
  #Set values when numeric input changes
  observeEvent(input$dt,{
    #Set dt
    dt(input$dt)
  })
  
  observeEvent(input$ts,{
    #Set timesteps
    ts(input$ts)

    #Compute input vector
    ext.inputs <- rep(0, times = 68)
    selected <- c(input$sel_id, input$sel_name, input$sel_gang, input$sel_age, input$sel_ed, input$sel_rel, input$sel_job)
    ext.inputs[match(selected, unames)] <- 1
    
    #Update activation matrix:
    amat(update.acts(ext.inputs, wmat(), dt = dt(), timesteps=ts()))
    currtime(1) #Set current time to 1
    
  })
  
  #Looping behavior:
  observe({
    if(input$loopflag) invalidateLater(200)
    isolate({
      currtime((currtime()+1) %% ts())
    })
  })
  
  output$modelPlot <- renderPlot({
    if(input$wtflag){
      plot.wts(wmat())
    } else{
      if(currtime()!=0) inpat <- amat()[,currtime()] else inpat <- blank.input[,1]
      render.network(inpat, jscoords)
    }
    })
  
    #Update counter display:
    output$stepcounter <- renderText({
      o <- "  "
      if(currtime()!=0 & !input$wtflag) o <- paste("Showing step", currtime(), "of", ts())
      o
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
