library(shiny)
library(reshape2)

uni_ts <- read.csv("uniq_ts.csv")

ui <- shinyUI(fluidPage(
  titlePanel("Malaria Testing Rates"),
  navlistPanel(
    "Average malaria testing rates of CHW",
    tabPanel("Whole country",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "year", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "size", label = "Size of each bar: ", choices = c("10"= "10","base2"="2"), selected="10", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph"))
             )
    ),
    tabPanel("State/Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sr", "State/Region",choices= levels(uni_ts[,2]), selected="KACHIN"),
                 checkboxGroupInput(inputId = "year_sr", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "size_sr", label = "Size of each bar: ", choices = c("10"= "10","base2"="2"), selected="10", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_sr"))
             )
    ),
    tabPanel("Township",
             sidebarLayout(
               sidebarPanel(
                 selectInput("tsp", "Township",choices= levels(uni_ts[,3]), selected="BAGO"),
                 checkboxGroupInput(inputId = "year_tsp", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "size_tsp", label = "Size of each bar: ", choices = c("10"= "10","base2"="2"), selected="10", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_tsp"))
             )
    ),
    "Boxplots",
    tabPanel("By State/Region",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "year_bpsr", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "out_bpsr", label="Outlier: ", choices= c("Yes"="T","No"="F"), selected="F", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_bpsr"))
             )
    ),
    tabPanel("By Implementer",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "year_bpip", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 radioButtons(inputId = "out_bpip", label="Outlier: ", choices= c("Yes"="T","No"="F"), selected="F", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_bpip"))
             )
    ),
    widths = c(2,8)
  )
)
)

server <- function(input, output) {
  m_q2 <- read.csv("m_q2.csv")
  
  
  output$graph <- renderPlot({
      #subsetting by 2 reactives
      m_q2 <- m_q2[m_q2$Year %in% input$year,]
      
      uniq_villages <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source ~ variable, mean, na.rm=TRUE)
      med_rdt <- median(uniq_villages$CountOfOutcome) #4.75
      uniq_villages_ym <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source+Year+Month ~ variable, mean, na.rm=TRUE)
      
      if(input$size == "10")
      {
        uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,10,20,30,40,50,250), labels=c("<=10","11-20","21-30","31-40","41-50",">50"))
        barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in MARC area ",paste(input$year,collapse=", ")," \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
        
      }
      if(input$size == "2")
      {
        uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,2^(0:6)[-1],250), labels=c("<=2","3-4","5-8","9-16","17-32","33-64",">64"))
        barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in MARC area ",paste(input$year,collapse=", ")," \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
      }

  })
  
  output$graph_sr <- renderPlot({
      #graph by state/region
      m_q2 <- m_q2[m_q2$MaxOfState..Division %in% input$sr,]
      m_q2 <- m_q2[m_q2$Year %in% input$year_sr,]
      
      uniq_villages <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source ~ variable, mean, na.rm=TRUE)
      med_rdt <- median(uniq_villages$CountOfOutcome) #4.75
      uniq_villages_ym <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source+Year+Month ~ variable, mean, na.rm=TRUE)
      
      if(input$size_sr == "10")
      {
        uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,10,20,30,40,50,250), labels=c("<=10","11-20","21-30","31-40","41-50",">50"))
        barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in ",paste(input$sr),", ",paste(input$year_sr,collapse=", ")," \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
        
      }
      if(input$size_sr == "2")
      {
        uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,2^(0:6)[-1],250), labels=c("<=2","3-4","5-8","9-16","17-32","33-64",">64"))
        barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in ",paste(input$sr),", ",paste(input$year_sr,collapse=", ")," \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
      }
  })
  
  output$graph_tsp <- renderPlot({
      #graph by township
      m_q2 <- m_q2[m_q2$MaxOfTownship %in% input$tsp,]
      m_q2 <- m_q2[m_q2$Year %in% input$year_tsp,]
      
      uniq_villages <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source ~ variable, mean, na.rm=TRUE)
      med_rdt <- median(uniq_villages$CountOfOutcome) #4.75
      uniq_villages_ym <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source+Year+Month ~ variable, mean, na.rm=TRUE)
      
      if(input$size_tsp == "10")
      {
        uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,10,20,30,40,50,250), labels=c("<=10","11-20","21-30","31-40","41-50",">50"))
        barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in ",paste(input$sr),", ",paste(input$year_tsp,collapse=", ")," \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
        
      }
      if(input$size_tsp == "2")
      {
        uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,2^(0:6)[-1],250), labels=c("<=2","3-4","5-8","9-16","17-32","33-64",">64"))
        barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in ",paste(input$sr),", ",paste(input$year_tsp,collapse=", ")," \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
      }
  })
  
  output$graph_bpsr <- renderPlot({
      #outcome plot by state/region
      m_q2 <- m_q2[m_q2$Year %in% input$year_bpsr,]
      
      uniq_villages <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source ~ variable, mean, na.rm=TRUE)
      uniq_villages_ym <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source+Year+Month ~ variable, mean, na.rm=TRUE)
      
      boxplot(CountOfOutcome ~ MaxOfState..Division,uniq_villages, xlab="States/Regions", ylab="Malaria tests", main=paste("Malaria testing rates \n across States and Regions \n MARC area ",paste(input$year_bpsr,collapse=", ")), outline=as.logical(input$out_bpsr), cex.names=.8) 
  })
  
  output$graph_bpip <- renderPlot({
      m_q2 <- m_q2[m_q2$Year %in% input$year_bpip,]
      
      uniq_villages <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source ~ variable, mean, na.rm=TRUE)
      uniq_villages_ym <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source+Year+Month ~ variable, mean, na.rm=TRUE)
      
      boxplot(CountOfOutcome ~ Source,uniq_villages, outline=as.logical(input$out_bpip), xlab="Implementing partners", ylab="Malaria tests", main=paste("Malaria testing rates \n across Implementing Partners \n MARC area ",paste(input$year_bpip,collapse=", "))) 
  })
}

shinyApp(ui = ui, server = server)
