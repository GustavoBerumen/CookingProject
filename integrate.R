# central organizer of code. It calls functions and perfom data analysis

# clear workspace
rm(list = ls())

# load libraries
source("./R/load-libraries.R")

# load all the data files and assign them to variables
source("./R/load-files.R")

# get descriptive stats of items
source("./R/stats-items.R")

# get items usage in sections
source("./R/session-slices.R")

# get concurrent items used around an item of interest
source("./R/process-items.R")

# get descriptive stats of items
source("./R/video-instances.R")


### test 
items.long <- c("stove", "oven", "extractorFan", "kettle", "microwave", "coffeeMachine", "riceCooker", "dishWasher",
               "blender", "speaker", "phone", "tablet", "smartAssistant", "radio")

# compare items.long to items.participants
items.match <- items.long %in% reg.list[[1]]$items

# get items.long preesent in a participant 
items.present <- items.long[items.match]



### shiny app 

library(shiny)
ui <- fluidPage(sliderInput(inputId = "num",
                            label = "Choose a number",
                            value = 25, min = 1, max = 100),
                            plotOutput("hist"))

server <- function(input, output) {
  output$hist <- renderPlot({hist(rnorm(input$num))})
}

shinyApp(ui = ui, server = server)