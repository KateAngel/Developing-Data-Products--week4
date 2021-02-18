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