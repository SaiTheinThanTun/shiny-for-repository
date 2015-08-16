library(shiny)
library(zoo)
library(reshape2)
setwd("~/")

uni_ts <- read.csv("uniq_ts.csv")

ui <- shinyUI(fluidPage(
  titlePanel("Malaria Data Repository, Myanmar"),
  navlistPanel(
    "Malaria Incidence",
    tabPanel("Whole country",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "year", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph"))
             )
             ),
    tabPanel("State/Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sr", "State/Region",choices= levels(uni_ts[,2]), selected="KACHIN"),
                 checkboxGroupInput(inputId = "year_sr", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type_sr", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_sr"))
               )
            ),
    tabPanel("Township",
             sidebarLayout(
               sidebarPanel(
                 selectInput("tsp", "Township",choices= levels(uni_ts[,3]), selected="BAGO"),
                 checkboxGroupInput(inputId = "year_tsp", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type_tsp", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_tsp"))
             )
    ),
    "Outcomes",
    tabPanel("Whole country",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "year_oc", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type_oc", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T),
                 radioButtons(inputId = "perc", label="Percentage: ", choices= c("Yes"="y","No"="n"), selected="No", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_oc"))
             )
    ),
    tabPanel("State/Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("ocsr", "State/Region",choices= levels(uni_ts[,2]), selected="KACHIN"),
                 checkboxGroupInput(inputId = "year_ocsr", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type_ocsr", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T),
                 radioButtons(inputId = "perc", label="Percentage: ", choices= c("Yes"="y","No"="n"), selected="No", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_ocsr"))
             )
    ),
    tabPanel("Township",
             sidebarLayout(
               sidebarPanel(
                 selectInput("octsp", "Township",choices= levels(uni_ts[,3]), selected="BAGO"),
                 checkboxGroupInput(inputId = "year_octsp", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type_octsp", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T),
                 radioButtons(inputId = "perc", label="Percentage: ", choices= c("Yes"="y","No"="n"), selected="No", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_octsp"))
             )
             ),
    widths = c(2,8)
    )
)
)

server <- function(input, output) {
  rdt <- read.csv("rdt.csv")
  rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
  
  output$graph <- renderPlot({
    #subsetting by 2 reactives
    rdt <- rdt[rdt$Yr %in% input$year,]
    if(input$type=="chw"){
      rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
      typep <- "Community Health Worker"
    }
    if(input$type=="hf"){
      rdt <- rdt[rdt$Expr1=="HF",]
      typep <- "Health Facility"
    }
    if(input$type=="all") {
      typep <- "Health facility and CHW"
    }
    
    
    combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
    
    combined$yrmth <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
    y_limits <- c(min(c(min(combined$`Non-Pf`),min(combined$Pf))),max(c(max(combined$`Non-Pf`),max(combined$Pf)))) #c(min(combined$`Non-Pf`),max(combined$Pf))
    #Plotting
    plot(combined$Pf ~ combined$yrmth, type="l", col="coral1",ylim=y_limits, main=paste("Malaria incidence (",typep,")\nMARC region ",paste(input$year, collapse = ", "),sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
    lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
    legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
    grid()
  })
  
  output$graph_sr <- renderPlot({
    #graph by state/region
    rdt <- rdt[rdt$State_Region %in% input$sr,]
    rdt <- rdt[rdt$Yr %in% input$year_sr,]
    if(input$type_sr=="chw"){
      rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
      typep <- "Community Health Worker"
    }
    if(input$type_sr=="hf"){
      rdt <- rdt[rdt$Expr1=="HF",]
      typep <- "Health Facility"
    }
    if(input$type_sr=="all") {
      typep <- "Health facility and CHW"
    }
    
    
    combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
    
    combined$yrmth <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
    y_limits <- c(min(c(min(combined$`Non-Pf`),min(combined$Pf))),max(c(max(combined$`Non-Pf`),max(combined$Pf))))
    #Plotting
    plot(combined$Pf ~ combined$yrmth, type="l", col="coral1",ylim=y_limits, main=paste("Malaria incidence (",typep,")\n", input$sr," ",paste(input$year_sr, collapse = ", "),sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
    lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
    legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
    grid()
  })
  
  output$graph_tsp <- renderPlot({
    #graph by township
    rdt <- rdt[rdt$Township %in% input$tsp,]
    rdt <- rdt[rdt$Yr %in% input$year_tsp,]
    if(input$type_tsp=="chw"){
      rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
      typep <- "Community Health Worker"
    }
    if(input$type_tsp=="hf"){
      rdt <- rdt[rdt$Expr1=="HF",]
      typep <- "Health Facility"
    }
    if(input$type_tsp=="all") {
      typep <- "Health facility and CHW"
    }
    
    
    combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
    
    combined$yrmth <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
    y_limits <- c(min(c(min(combined$`Non-Pf`),min(combined$Pf))),max(c(max(combined$`Non-Pf`),max(combined$Pf))))
    #Plotting
    plot(combined$Pf ~ combined$yrmth, type="l", col="coral1",ylim=y_limits, main=paste("Malaria incidence (",typep,")\n", input$tsp," ",paste(input$year_tsp, collapse = ", "),sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
    lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
    legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
    grid()
  })
}

shinyApp(ui = ui, server = server)
