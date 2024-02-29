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
          fileInput("infile", "Patterns csv file:"),
#          fileInput("targfile", "Target pattern csv file:"),
          strong("Network viewer"),
          br(),
          actionButton("make_pa", "Create"),
          actionButton("back", "<"),
          actionButton("forward", ">"),
          actionButton("reset", "Reset"),
          textOutput("patno"),
          br(),
          fluidRow(
            column(4, numericInput("lrate", "LR", value=0.05, min=0.0, max=1.0)),
            column(4, numericInput("bprop", "BP", value=1.0, min=0.0, max=1.0)),
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
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            column(12,align = "left", textOutput("weight_info"))
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(column(12, plotOutput("paPlot", height = "500px", hover="plot_hover"))),
          fluidRow(column(6, plotOutput("errPlot", height = "300px")),
                   column(6, plotOutput("terrPlot", height = "300px"))),
          fluidRow(column(12, textOutput("currpat")))
        )
    )
)

# Define server functions
server <- function(input, output) {
    #Functions needed for pattern associator
    source("./pa_functions.r")
    
    #Set reactive values:
    innum <- reactiveVal()    #Number of input units
    targnum <- reactiveVal()  #Number of target units
    numpats <- reactiveVal(0)   #Number of patterns
    wtflag <- reactiveVal(FALSE) #Are weights showing in display

    currindex <- reactiveVal(0) #Index for current pattern
    
    lrate <- reactiveVal(0.1)    #Learning rate
    bprop <- reactiveVal(1)    #Proportion batch size
    nepochs <- reactiveVal(1)  #Number of epochs
    
    inputs <- reactiveVal(read.csv("./data/demo_patterns.csv", header = 1, row.names = 1))   #input patterns
    targets <- reactiveVal(read.csv("./data/demo_patterns.csv", header = 1, row.names = 1))   #target patterns
    filename <- reactiveVal("empty")
    m <- reactiveVal(0)        #Weight matrix
    errvec <- reactiveVal(c())    #Vector of error values for linear activation
    terrvec <- reactiveVal(c())   #Vector of error values for thresholded activation
    
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
      filename(gsub(".csv", "", file$name)) #Set global file name string
      
      tmp <- read.csv(file$datapath, header = T, row.names = 1) #Read file to data frame
      tmp <- as.matrix(tmp) #convert to matrix object
      tmpin <- tmp[,grep("in_", colnames(tmp))] #Extract input patterns
      inputs(tmpin)
      tmpout <- tmp[,grep("out_", colnames(tmp))]
      targets(tmpout)
      
      #Create matrix of weights:
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
        title(filename())
        wtflag(TRUE)
      })
    })
    
    #Hover function:
    output$weight_info <- renderPrint({
      x <- round(as.numeric(input$plot_hover$x), 0)
      y <- round(as.numeric(input$plot_hover$y),0)

      
      if(wtflag()){ #If weights are showing:
        currmat <- m()
        from <- innum() - y + 1
        to <- x
        
        
        if(!is.null(row.names(currmat))) fname <- row.names(currmat)[from] else fname <- paste0("in_", from)
        if(!is.null(colnames(currmat))) toname <- colnames(currmat)[to] else toname <- paste0("out_", to)

        if(length(x)==0 | length(y)==0){
          o <- "no connection selected"
        } else if(from < 0 | from > innum() | to < 0 | to > targnum()){
          o <- "no connection selected"
        } else{
          w <- round(currmat[from, to], 3)
          o <- paste0(fname, " \U2192 ", toname, ": ", w)
        }
      } else{
        o <- "no network loaded"
      } 
      cat(o)
    }
    )
    #Click on pattern forward button
    observeEvent(input$forward,{
      #file <- input$infile #Get file object from input field
      
      if(currindex() < numpats()) currindex(currindex() + 1)
      currin <- inputs()[currindex(),]
      currtarg <- targets()[currindex(),]
      
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
      
      output$paPlot <- renderPlot({
        plot.pa(m(), currin, currtarg)
        title(filename())
      })
    })

    #Click on pattern backward button
    observeEvent(input$back,{
      #file <- input$infile #Get file object from input field
      
      currindex(currindex() - 1)
      if(currindex() < 1) currindex(1)
      currin <- inputs()[currindex(),]
      currtarg <- targets()[currindex(),]
      
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
      
      output$paPlot <- renderPlot({
        # generate network plot
        plot.pa(m(), currin, currtarg)
        title(filename())
      })
    })
    
    #Click on reset button
    observeEvent(input$reset,{
      #file <- input$infile #Get file object from input field
      
      currindex(0) #reset current pattern index to zero
      currin <- rep(0, times = innum()) #reset input pattern to zeros
      currtarg <- rep(0, times = targnum()) #reset target pattern to zeros
      m(make.pa(inputs(), targets())) #reset weight matrix to zeros
      
      output$patno <- renderText(paste("Showing pattern 0 of", numpats()))
      output$currpat <- renderText("no input pattern")
      
      output$paPlot <- renderPlot({
        # generate network plot
        plot.pa(m(), currin, currtarg)
        title(filename())
      })
      errvec(c())
      terrvec(c())
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
      #file <- input$infile #Get file object from input field
      curerr <- errvec() #get global error vector
      curterr <- terrvec() #get global thresholded error vec
      ne <- nepochs() #get number of epochs
      for(i in c(1:ne)){
        if(bprop()==0){ #If we are stepping one pattern at a time
          currindex(currindex() + 1)
          if(currindex() > numpats()) currindex(1)
          currin <- inputs()[currindex(),]
          currtarg <- targets()[currindex(),]
        } else{ #If training on a batch
          bsize <- floor(numpats() * bprop()) #Number of patterns in batch
          if(bsize < 1) bsize <- 1
          bindex <- sample(c(1:numpats()), bsize) #Index of sampled items
          currin <- inputs()[bindex,] #Get sample of input patterns
          currtarg <- targets()[bindex,] #Get sample of target patterns
        }
        m(hebb.update(m(), currin, currtarg, lr=lrate())) #Update matrix by Hebb rule
        curerr <- append(curerr, get.err(m(), currin, currtarg))
        curterr <- append(curterr, get.terr(m(), currin, currtarg, min(targets()), max(targets())))
      }
      errvec(curerr) #update global error vector
      terrvec(curterr) #update global thresholded error vector
      
      #Reset index to 1 if it is outside of pattern range
      if(currindex() < 1 | currindex() > numpats()) currindex(1)
      #Update plot
      output$paPlot <- renderPlot({
        # generate network plot
        currin <- inputs()[currindex(),]
        currtarg <- targets()[currindex(),]
        plot.pa(m(), currin, currtarg)
        title(filename())
      })
      #Update count:
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
    })

    #Click on Delta train button
    observeEvent(input$delta,{
      #file <- input$infile #Get file object from input field
      curerr <- errvec() #current error
      curterr <- terrvec() #current thresholded error vector
      ne <- nepochs()    #number of epochs
        for(i in c(1:ne)){
          if(bprop()==0){
            currindex(currindex() + 1)
            if(currindex() > numpats()) currindex(1)
            currin <- inputs()[currindex(),]
            currtarg <- targets()[currindex(),]
          } else{
            bsize <- floor(numpats() * bprop()) #Number of patterns in batch
            if(bsize < 1) bsize <- 1
            bindex <- sample(c(1:numpats()), bsize) #Index of sampled items
            currin <- inputs()[bindex,] #Get sample of input patterns
            currtarg <- targets()[bindex,] #Get sample of target patterns
        }
        m(delta.update(m(), currin, currtarg, lr=lrate())) #Update matrix by Hebb rule          }
        curerr <- append(curerr, get.err(m(), currin, currtarg))
        curterr <- append(curterr, get.terr(m(), currin, currtarg, min(targets()), max(targets())))
        }
      errvec(curerr) #Update global error vector
      terrvec(curterr) #update global thresholded error vector
    
      if(currindex() < 1 | currindex() > numpats()) currindex(1)
      output$paPlot <- renderPlot({
        # generate network plot
        currin <- inputs()[1,]
        currtarg <- targets()[1,]
        plot.pa(m(), currin, currtarg)
        title(filename())
      })
      #Update count:
      output$currpat <- renderText(row.names(inputs())[currindex()])
      output$patno <- renderText(paste("Showing pattern", currindex(), "of", numpats()))
    })
    
    #Update linear error plot
    output$errPlot <- renderPlot({
      currerr <- errvec()
      if(length(currerr) ==0){
        p <- plot(0,0,type = "n", xlab = "epochs", ylab = "error")
      } else{
        p <- plot(1:length(currerr), currerr, type = "l", lwd = 2, col = 2,
                  xlab = "epochs", ylab = "error")
        title(paste("final linear error:", round(currerr[length(currerr)],4)))
      }
      p
    })

    #Update threshold error plot
    output$terrPlot <- renderPlot({
      curterr <- terrvec()
      if(length(curterr) ==0){
        p <- plot(0,0,type = "n", xlab = "epochs", ylab = "error", ylim = c(0,1))
      } else{
        p <- plot(1:length(curterr), curterr, type = "l", lwd = 2, col = 2,
                  xlab = "epochs", ylab = "error", ylim = c(0,1))
        title(paste("final thresholded error:", round(curterr[length(curterr)],4)))
      }
      p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
