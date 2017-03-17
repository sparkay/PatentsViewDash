# Patent Density Explorer server script

### Put setup code here -runs only once
library(plotly)

classes_to_groups <- read.delim("data/patent class groups.txt", sep="\t", stringsAsFactors = F)
USPTOclasses <- read.delim("data/primary_classes_index2.txt", sep="\t", stringsAsFactors = F)

source("patentsview.r") #load API functions
source("patplots.R") #load plotting functions

server <- shinyServer(function(input, output) {
  
  #filter USPTO main class selector based on Kgroup
  output$USPTOclass <- renderUI({
    selectInput("USPTOclass", label="Select USPTO Class", choices=as.character(USPTOclasses[USPTOclasses$class %in% classes_to_groups[classes_to_groups$Kgroup == input$Kgroup,"class"],"shortname"]))
  })
  
  #capture mainclass code selection
  mainclasscode <- reactive({
    USPTOclasses[USPTOclasses$shortname == input$USPTOclass, "class"] 
  })
  
  #send description of class selection to input
  output$selection <- renderText({ 
    sprintf("You have selected USPTO class #%s - %s.", mainclasscode(), USPTOclasses[USPTOclasses$shortname == input$USPTOclass, "title"])
  })
  
  #pull count of patents per year for classcode
  patdata_bkgrd <- reactive({
    patsperyear(mainclasscode())
  })
  
  #generate background density plot
  output$patdensity <- renderPlot({
    plot_patbkgrd(patdata_bkgrd())
  })
  
  #pull assignees for classcode
  #may want to make the numorgs parameter user-controlled
  class_assignees <- reactive({
    assignees(mainclasscode())
  })
  
  #display statistics on assignee patent counts
  output$assigneehist <- renderPlot({
    plot_assigneescatter(class_assignees())
  })
  output$assigneetype <- renderPlot({
    plot_assigneetype(class_assignees())
  })
  
  #quick note with basic stats
  output$stats <- renderText(
    sprintf("Total Patents = %d. Total Assignees = %d.", sum(patdata_bkgrd()$count, na.rm=T), nrow(class_assignees()))
  )
    
  #display assignee table
  output$assignees <- renderDataTable(class_assignees())
  
})

