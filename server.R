# Define server logic required 
server <- function(input, output) {
  
  cols <- reactive({
    as.numeric(c(input$var,12))
    
  })
  mylabel <- reactive({
    if(input$TMInput=='a'){
      lable <- "Plot for automatic"
    }
    if(input$TMInput=='m'){
      lable <- "Plot for manual"
    }
    lable
  })
  
  
  finalData <- reactive({
    #------------------------------------------------------------------
    # Select data according to selection of radio button
    if(input$TMInput=='a'){
      mydata <- df[df$am == 0,]
      
    }
    
    if(input$TMInput=='m'){
      mydata <- df[df$am == 1,]
    }
    
    
    #------------------------------------------------------------------
    # Get data rows for selected horsepower
    mydata1 <- mydata[mydata$hp >= input$HorsePower[1], ] # From
    mydata1 <- mydata1[mydata1$hp <= input$HorsePower[2], ] # To
    mydata2<- mydata1[, c(1, sort(cols()))]
    data.frame(mydata2)
    #------------------------------------------------------------------
    
  })
  
  # Prepare Data tab
  output$fdata <- renderTable({
    finalData()
  })
  
  # Prepare Structure Tab
  renderstr <- reactive({ str(finalData())})
  output$dataStructure <- renderPrint({
    renderstr()
  })
  
  # Prepare Summary Tab
  rendersummary <- reactive({ summary(finalData())})
  output$dataSummary <- renderPrint({
    rendersummary()
  })
  
  # Prepare Plot Tab
  output$dataplot <- renderPlot({
    fd <- finalData()
    manuffreq <- table(fd[ncol(fd)])
    lbls <- names(manuffreq)
    pct <- round(manuffreq/sum(manuffreq)*100)
    lbls <- paste(lbls, pct)
    lbls <- paste(lbls,"%",sep="")
    pie(manuffreq,labels = lbls,col=rainbow(length(lbls)), main="Manufacturers Distribution")
    
  })
  
}