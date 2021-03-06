# Patent Density Explorer user interface file
library(plotly)


Kgroups <- read.delim("data/Kgroup colors.txt", sep="\t", stringsAsFactors = F)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
  titlePanel("US Patent Technology Class Explorer"),
  
  sidebarLayout(
    mainPanel(
      h1("Instructions"),
      p("1. Select a Technology Group"),
      p("2. Select a Patent Technology (USPTO) Class"),
      p("The number of patents in that class and the top assignees will display below"),
      p("3. Select one or more assignees in the table below to compare their timelines of patent grants in the Count and Density graphs.")
    ),
    sidebarPanel(
      fluidRow(
        selectInput("Kgroup", label="Select Tech Group", choices=Kgroups$Kgroup)  
      ),
      fluidRow(
        uiOutput("USPTOclass")
      )
    )
  ),
  
  wellPanel(
    textOutput("selection"),
    textOutput("stats")
  ),
  
  fluidRow(
    splitLayout(
      plotOutput("patcount"),
      plotOutput("patdensity")
    )
  ),
  
  splitLayout(
    plotlyOutput("assigneehist"),
    DT::dataTableOutput('assignees')
    #plotOutput("assigneetype")
  ),
  
  fluidRow(
    #textOutput("testtxt")
  )
  
))

