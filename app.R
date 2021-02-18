#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Read Data
data(mtcars)
df<-mtcars
df$manuf <- sapply(strsplit(rownames(df), " "), "[[", 1)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel(title = h3("Motor Trend Car Road Tests", align="center")),
    br(),   br(),
    sidebarLayout(
        sidebarPanel(
            #------------------------------------------------------------------
            # Add radio button to choice for automatic or manual transmission
            radioButtons("TMInput", 
                         label = "Select Transmission: ",
                         choices = list("automatic" = 'a', "manual" = 'm'),
                         selected = 'a'),
            br(),   br(),
            #------------------------------------------------------------------
            # Add Variable for Year Selection
            sliderInput("HorsePower", "Select Horse Power Range : ", min=52, max=335, value=c(0, 400), step=1
                        
            ),
            
            br(),   br(),
            #------------------------------------------------------------------
            # Add Variables selection option 
            selectInput("var", "Select Variable from Dataset", 
                        choices=c("mpg"=1, "cyl"=2, "disp"=3, "hp"=4, 
                                  "drat"=5, "wt"=6, "qsec"=7, "vs"=8, 
                                   "am"=9, "gear"=10, "carb"=11),
                        multiple=TRUE, selected = "mpg"
            ),
            
            br(),   br()
            #------------------------------------------------------------------
            # Change background color for body
            #tags$style("body{background-color:lightyellow; color:brown}")
        ),
        
        mainPanel(
            #------------------------------------------------------------------
            # Create tab panes
            tabsetPanel(type="tab",
                        tabPanel("Data", tableOutput("fdata")),
                        tabPanel("Structure", verbatimTextOutput("dataStructure")),
                        tabPanel("Summary",verbatimTextOutput("dataSummary")),
                        tabPanel("Plot", plotOutput("dataplot"))
            )
            
            #------------------------------------------------------------------
        )
    )
    
)

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

# Run the application 
shinyApp(ui = ui, server = server)
