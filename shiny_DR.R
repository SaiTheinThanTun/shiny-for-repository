library(shiny)
library(zoo)
library(reshape2)
setwd("~/")

ui <- fluidPage()

server <- function(input, output) {
  rdt <- read.csv("rdt.csv")
  rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
  
}

shinyApp(ui = ui, server = server)
