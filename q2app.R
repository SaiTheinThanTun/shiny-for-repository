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
    "Outcomes",
    tabPanel("Whole country",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(inputId = "year_oc", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 #radioButtons(inputId = "type_oc", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T),
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
                 #radioButtons(inputId = "type_ocsr", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T),
                 radioButtons(inputId = "percsr", label="Percentage: ", choices= c("Yes"="y","No"="n"), selected="No", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_ocsr"))
             )
    ),
    tabPanel("Township",
             sidebarLayout(
               sidebarPanel(
                 selectInput("octsp", "Township",choices= levels(uni_ts[,3]), selected="BAGO"),
                 checkboxGroupInput(inputId = "year_octsp", label = "Years to include: ", choices = c("2012" ,"2013","2014"), selected = "2013", inline=T),
                 #radioButtons(inputId = "type_octsp", label = "Type: ", choices = c("CHW"= "chw","HF"="hf", "All"="all"), selected="All", inline=T),
                 radioButtons(inputId = "perctsp", label="Percentage: ", choices= c("Yes"="y","No"="n"), selected="No", inline=T)
               ),
               mainPanel(plotOutput(outputId = "graph_octsp"))
             )
    ),
    tabPanel("Password",
             sidebarLayout(
               sidebarPanel(
                 passwordInput("psw", "Enter Password:")
               ),
               mainPanel ()  
             )
    ),
    widths = c(2,8)
  )
)
)

server <- function(input, output) {
  m_q2 <- read.csv("m_q2.csv")
  
  
  output$graph <- renderPlot({
    if(input$psw == "mocru711"){
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

      }
  })
  
  output$graph_sr <- renderPlot({
    if(input$psw == "mocru711"){
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
      }
  })
  
  output$graph_tsp <- renderPlot({
    if(input$psw == "mocru711"){
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
      }
  })
  
  output$graph_oc <- renderPlot({
    if(input$psw == "mocru711"){
      #outcome plot for whole country
      rdt <- rdt[rdt$Yr %in% input$year_oc,]
      
      combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
      combined[,1] <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
      combined <- combined[,-2]
      YearMonth <- combined$Yr
      
      if(input$perc == "n")
      {
        #Stacked
        tcomb <- t(combined)
        colnames(tcomb) <- tcomb[1,]
        tcomb <- tcomb[-1,]
        barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", main=paste("Malaria Outcomes per Month\n MARC region ",paste(input$year_oc,collapse=", ")), ylab="No. of Malaria Outcomes")
        legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
      }
      if(input$perc=="y")
      {
        #Percentage Stacked Outcome plot per month
        tcomb_prop <- t(prop.table(as.matrix(combined[,2:4]),1))
        colnames(tcomb_prop) <- as.character(YearMonth)
        barplot(tcomb_prop, col=c("cornflowerblue","orange","coral1"), border="white", main=paste("Percentage of Malaria Outcomes per Month\n MARC region ", paste(input$year_oc,collapse=", ")), ylab="Percentage of Malaria Outcomes per Month")
        legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
      }
    }
  })
  
  output$graph_ocsr <- renderPlot({
    if(input$psw == "mocru711"){
      #outcome plot by state/region
      rdt <- rdt[rdt$State_Region %in% input$ocsr,]
      rdt <- rdt[rdt$Yr %in% input$year_ocsr,]
      
      combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
      combined[,1] <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
      combined <- combined[,-2]
      YearMonth <- combined$Yr
      
      if(input$percsr == "n")
      {
        #Stacked
        tcomb <- t(combined)
        colnames(tcomb) <- tcomb[1,]
        tcomb <- tcomb[-1,]
        barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", main=paste("Malaria Outcomes per Month\n",input$ocsr,paste(input$year_ocsr,collapse=", ")), ylab="No. of Malaria Outcomes")
        legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
      }
      if(input$percsr=="y")
      {
        #Percentage Stacked Outcome plot per month
        tcomb_prop <- t(prop.table(as.matrix(combined[,2:4]),1))
        colnames(tcomb_prop) <- as.character(YearMonth)
        barplot(tcomb_prop, col=c("cornflowerblue","orange","coral1"), border="white", main=paste("Percentage of Malaria Outcomes per Month\n",input$ocsr, paste(input$year_ocsr,collapse=", ")), ylab="Percentage of Malaria Outcomes per Month")
        legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
      }
    }
  })
  
  output$graph_octsp <- renderPlot({
    if(input$psw == "mocru711"){
      #outcome plot by state/region
      rdt <- rdt[rdt$Township %in% input$octsp,]
      rdt <- rdt[rdt$Yr %in% input$year_octsp,]
      
      combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
      combined[,1] <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
      combined <- combined[,-2]
      YearMonth <- combined$Yr
      
      if(input$perctsp == "n")
      {
        #Stacked
        tcomb <- t(combined)
        colnames(tcomb) <- tcomb[1,]
        tcomb <- tcomb[-1,]
        barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", main=paste("Malaria Outcomes per Month\n",input$octsp,paste(input$year_octsp,collapse=", ")), ylab="No. of Malaria Outcomes")
        legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
      }
      if(input$perctsp=="y")
      {
        #Percentage Stacked Outcome plot per month
        tcomb_prop <- t(prop.table(as.matrix(combined[,2:4]),1))
        colnames(tcomb_prop) <- as.character(YearMonth)
        barplot(tcomb_prop, col=c("cornflowerblue","orange","coral1"), border="white", main=paste("Percentage of Malaria Outcomes per Month\n",input$octsp, paste(input$year_octsp,collapse=", ")), ylab="Percentage of Malaria Outcomes per Month")
        legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
      }
    }
  })
}

shinyApp(ui = ui, server = server)
