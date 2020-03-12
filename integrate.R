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



### show on Friday
path_test <- "C:/Users/LPXJGB/Desktop/CHI-2020/CookingProject/outputs/test/test-trashB.xlsx"

test <- as.data.frame(read_excel(path_test, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0))

len.test <- length(test$info)

for (i in 1:len.test){
  cat("\n", i, ". ", test$info[i], ":", "\n", test$data[i])
}


fileName = paste("/", "_stats_.csv", sep = '')
fileOutput = paste(path_output, fileName, sep = '')
write.csv(test, fileOutput)





### trying shiny app 

library(shiny)
ui <- fluidPage(sliderInput(inputId = "num",
                            label = "Choose a number",
                            value = 25, min = 1, max = 100),
                            plotOutput("hist"))

server <- function(input, output) {
  output$hist <- renderPlot({hist(rnorm(input$num))})
}

shinyApp(ui = ui, server = server)