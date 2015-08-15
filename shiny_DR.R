library(shiny)
library(zoo)
library(reshape2)
setwd("~/")

ui <- fluidPage(
  radioButtons(inputId = "scope", label = "Scope", choices = c("Nation-wide","State/Region","Township"), selected = "Nation-wide"),
  checkboxGroupInput(inputId = "year", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013"),
  radioButtons(inputId = "type", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All"),
  plotOutput(outputId = "graph"),
  textOutput(outputId = "no")
)

server <- function(input, output) {
  rdt <- read.csv("rdt.csv")
  rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
  
  
  output$graph <- renderPlot({
    #subsetting by 2 reactives
    rdt <- rdt[rdt$Yr %in% input$year,]
    year_in <- paste(input$year,sep=",")
    if(input$type=="chw"){
      rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
      typep <- "Community Health Worker"
      #y_limits <- c(300,2000)
    }
    if(input$type=="hf"){
      rdt <- rdt[rdt$Expr1=="HF",]
      typep <- "Health Facility"
      #y_limits <- c(500,2000)
    }
    if(input$type=="all") {
      typep <- "Health facility and CHW"
      #y_limits <- c(500,3500)
    }
    
    
    combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
    #comb2013 <- combined[combined$Yr==2013,]
    
    
    combined$yrmth <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
    #combined$pf_npf <- combined$Pf+combined$`Non-Pf`
    #combined$tested <- combined$Pf+combined$`Non-Pf`+combined$Neg
    y_limits <- c(min(combined$`Non-Pf`),max(combined$Pf))
    #Plotting
    plot(combined$Pf ~ combined$yrmth, type="l", col="coral1",ylim=y_limits, main=paste("Malaria incidence (",typep,")\nMARC region,",year_in,sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
    lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
    legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
    grid()
  })
  output$no <- renderText(input$year)
  
}

shinyApp(ui = ui, server = server)
