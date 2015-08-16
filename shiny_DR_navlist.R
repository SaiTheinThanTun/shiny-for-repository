library(shiny)

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
                 checkboxGroupInput(inputId = "year", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_sr"))
               )
            ),
    tabPanel("Township",
             sidebarLayout(
               sidebarPanel(
                 selectInput("tsp", "Township",choices= levels(uni_ts[,3]), selected="BAGO"),
                 checkboxGroupInput(inputId = "year", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "type", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_tsp"))
             )
    ),
    "Outcomes",
    tabPanel("Whole Country"
    ),
    widths = c(2,8)
    )
)
)

server <- function(input, output) {
  rdt_org <- read.csv("rdt.csv")
  rdt_org$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
  
  
  output$graph <- renderPlot({
    #subsetting by 2 reactives
    rdt <- rdt_org[rdt_org$Yr %in% input$year,]
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
    rdt <- rdt_org[rdt_org$State_Region %in% input$sr,]
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
    y_limits <- c(min(c(min(combined$`Non-Pf`),min(combined$Pf))),max(c(max(combined$`Non-Pf`),max(combined$Pf))))
    #Plotting
    plot(combined$Pf ~ combined$yrmth, type="l", col="coral1",ylim=y_limits, main=paste("Malaria incidence (",typep,")\n", input$sr," ",paste(input$year, collapse = ", "),sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
    lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
    legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
    grid()
  })
}

shinyApp(ui = ui, server = server)
