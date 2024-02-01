#Shiny app implementing simple pattern association
#with Hebb and delta rules.
library(shiny)
library(plotrix)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pattern associator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("infile", "Input pattern csv file:"),
          fileInput("targfile", "Target pattern csv file:"),
          strong("Network viewer"),
          br(),
          actionButton("make_pa", "Create"),
          actionButton("back", "<"),
          actionButton("forward", ">"),
          actionButton("reset", "Reset"),
          textOutput("patno"),
          br(),
          fluidRow(
            column(4, numericInput("lrate", "LR", value=0.0, min=0.0, max=1.0)),
            column(4, numericInput("bprop", "BP", value=0, min=0.0, max=1.0)),
            column(4, numericInput("nepochs", "NE", value=1, min=1))
          ),
          fluidRow(
            column(4, verbatimTextOutput("currentlr", placeholder=TRUE)),
            column(4, verbatimTextOutput("currentbp", placeholder=TRUE)),
            column(4, verbatimTextOutput("currentne", placeholder=TRUE))
          ),
          fluidRow(
            column(6, actionButton("hebb","Hebb", width="100%")),
            column(6, actionButton("delta","Delta", width="100%"))
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          #h3(textOutput("currpat")),
          fluidRow(column(12, plotOutput("paPlot", height = "600px"))),
          fluidRow(column(12, textOutput("currpat")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #Functions needed for pattern associator
    source("./pa_functions.r")
    
    #Set reactive values:
    innum <- reactiveVal()    #Number of input units
    targnum <- reactiveVal()  #Number of target units
    numpats <- reactiveVal(0)   #Number of patterns

    currindex <- reactiveVal(0) #Index for current pattern
    
    lrate <- reactiveVal(0.1)    #Learning rate
    bprop <- reactiveVal(1)    #Proportion batch size
    nepochs <- reactiveVal(1)  #Number of epochs
    
    inputs <- reactiveVal(read.csv("./data/inputs.csv", header = 1, row.names = 1))   #input patterns
    targets <- reactiveVal(read.csv("./data/targets.csv", header = 1, row.names = 1))   #target patterns
    m <- reactiveVal(0)        #Weight matrix
    
    #output$currpat <- renderText("no input pattern")
  
    #########Events here#####################
    #####Network build and view functions####
    
    #Creates the initial network with zero weights:
    observeEvent(input$make_pa,{
      #Load input file
      file <- input$infile #Get file object from input field
      ext <- tools::file_ext(file$datapath) #Get the file extension
      
      req(file) #Make sure this exists
      validate(need(ext == "csv", "Please upload a csv file")) #Check it's a csv file
      
      tmp <- read.csv(file$datapath, header = T, row.names = 1) #Read file to data frame
      tmp <- as.matrix(tmp) #convert to matrix object
      inputs(tmp)
      
      #Load target file, same steps as above
      file <- input$targfile
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      tmp <- read.csv(file$datapath, header = T, row.names = 1)
      tmp <- as.matrix(tmp)
      targets(tmp)
      
      #Create matrix of weights:
      #m <- make.pa(inputs, targets)
      m(make.pa(inputs(), targets()))
  
      #Set number of units globally
      innum(dim(m())[1])
      targnum(dim(m())[1])
      
      #Render network with zero inputs and targets     
      currin <- rep(0, times = innum()) #Zero inputs
      currtarg <- rep(0, times = targnum()) #zero targets
      numpats(dim(inputs())[1]) #Set total number of patterns
      output$currpat <- renderText("no input pattern")
      output$patno <- renderText(paste("Showing pattern 0 of", numpats()))
      
      output$paPlot <- renderPlot({
        # generate network plot
        plot.pa(m(), currin, currtarg)
      })
    })
    
    #Click on pattern forward button
    observeEvent(input$forward,{
      
      if(currindex() < numpats()) currindex(currindex() + 1)
      currin <- inputs()[currindex(),]
      currtarg <- targets()[currindex(),]
      
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
      
      output$paPlot <- renderPlot({
          # generate network plot
          
          #Create initial network
          #m <- make.pa(inputs, targets)
          plot.pa(m(), currin, currtarg)
          
      })
    })

    #Click on pattern backward button
    observeEvent(input$back,{
      
      currindex(currindex() - 1)
      if(currindex() < 1) currindex(1)
      currin <- inputs()[currindex(),]
      currtarg <- targets()[currindex(),]
      
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
      
      output$paPlot <- renderPlot({
        # generate network plot
        plot.pa(m(), currin, currtarg)
        
      })
    })
    
    #Click on reset button
    observeEvent(input$reset,{
      
      currindex(0) #reset current pattern index to zero
      currin <- rep(0, times = innum()) #reset input pattern to zeros
      currtarg <- rep(0, times = targnum()) #reset target pattern to zeros
      m(make.pa(inputs(), targets())) #reset weight matrix to zeros
      
      output$patno <- renderText(paste("Showing pattern 0 of", numpats()))
      output$currpat <- renderText("no input pattern")
      
      output$paPlot <- renderPlot({
        # generate network plot
        plot.pa(m(), currin, currtarg)
        
      })
    })
   
    ##########Training functions#########
    
    #Set values when numeric input changes
    observeEvent(input$lrate,{
      #Set lrate
      lrate(input$lrate)
      output$currentlr <- renderText({lrate()})
    })
    observeEvent(input$bprop,{
      #Set batch proportion
      bprop(input$bprop)
      output$currentbp <- renderText({bprop()})
    })
    observeEvent(input$nepochs,{
      #Set number of epochs
      nepochs(input$nepochs)
      output$currentne <- renderText({nepochs()})
    })
    
    #Click on Hebb train button
    observeEvent(input$hebb,{
      if(bprop()==0){ #If we are stepping one pattern at a time
        currindex(currindex() + 1)
        if(currindex() > numpats()) currindex(1)
        currin <- inputs()[currindex(),]
        currtarg <- targets()[currindex(),]
        m(hebb.update(m(), currin, currtarg, lr=lrate()))
      } else{ #If training on a batch
        bsize <- floor(numpats() * bprop()) #Number of patterns in batch
        bindex <- sample(c(1:numpats()), bsize) #Index of sampled items
        currin <- inputs()[bindex,] #Get sample of input patterns
        currtarg <- targets()[bindex,] #Get sample of target patterns
        for(i in c(1:nepochs())){
               m(hebb.update(m(), currin, currtarg, lr=lrate())) #Update matrix by Hebb rule
          }
      }
      #Reset index to 1 if it is outside of pattern range
      if(currindex() < 1 | currindex() > numpats()) currindex(1)
      #Update plot
      output$paPlot <- renderPlot({
        # generate network plot
        currin <- inputs()[currindex(),]
        currtarg <- targets()[currindex(),]
        #Create initial network
        #m <- make.pa(inputs, targets)
        plot.pa(m(), currin, currtarg)
      })
      #Update count:
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
    })

    #Click on Delta train button
    observeEvent(input$delta,{
       if(bprop()==0){
        currindex(currindex() + 1)
        if(currindex() > numpats()) currindex(1)
        currin <- inputs()[currindex(),]
        currtarg <- targets()[currindex(),]
        m(delta.update(m(), currin, currtarg, lr = lrate()))
      } else{
        bsize <- floor(numpats() * bprop()) #Number of patterns in batch
        bindex <- sample(c(1:numpats()), bsize) #Index of sampled items
        currin <- inputs()[bindex,] #Get sample of input patterns
        currtarg <- targets()[bindex,] #Get sample of target patterns
        for(i in c(1:nepochs())){
          m(delta.update(m(), currin, currtarg, lr=lrate())) #Update matrix by Hebb rule          }
        }
      }

      if(currindex() < 1 | currindex() > numpats()) currindex(1)
      output$paPlot <- renderPlot({
        # generate network plot
        currin <- inputs()[1,]
        currtarg <- targets()[1,]
        plot.pa(m(), currin, currtarg)
      })
      #Update count:
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
