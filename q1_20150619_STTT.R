#setwd("C:/WorkingDirectoryName") #The name and path can be anywhere in your computer
#another example: setwd("C:/Users/Sai Thein Than Tun/Documents/R Working Directory")
#Copy the following files into your working directory:
#1. Q1.csv #Query 1 produced from Access, and then converted to CSV
#2. MARC PCodes.csv ##Link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1

#The outputs will also be produced in the same directory.
#Outputs are for 2013 MARC area: 
#1. Table1: Outcomes per IP
#2. Table2: Outcomes per States, Regions
#3. Table3: Outcomes per Townships
#4. Table4: Xtab Townships against IP (Total tests)
#5. Table5: Xtab Townships against IP (Pf+Pmix)
#6. Plot1: Malaria incidence (CHW)
#7. Plot2: Malaria incidence (Health Facility)
#8. Plot3: Malaria incidence (CHW + Health Facility)
#9. Plot4: Stacked Outcome plot per month
#10. Plot5: Percentage Stacked Outcome plot per month
#11. Plot6: Stacked Outcome plot per States/Regions
#12. Plot7: Percentage Stacked Outcome plot per States/Regions
#13. Plot8: Stacked Outcome plot per Township
#14. Plot9: Percentage Stacked Outcome plot per Township

setwd("C:/Users/Tom/Dropbox/1. MAEMOD/QGIS/2014 MARC Data")

#q1_1_cleaning
#Setting up name vectors for Outcome & Type categories
pf <- c("Mix","pf","Pf","PF")
npf <- c("Non-Pf","pv","Pv")
neg <- "Neg"
type2include <- c("Clinic","HF","Volunteer") #There's no Mobile and Screening points in 2013 data

rdt <- read.csv("Q1.csv")

#Changing the name of "SumOfNumber" variable into "Number"
if(sum(names(rdt) %in% "SumOfNumber") >0){
  names(rdt)[names(rdt)=="SumOfNumber"] <- "Number"
}
#checking names
sum(names(rdt) %in% c("Expr1", "SR_Pcode","State_Region","Tsp_Code","Township","Yr","Mth", "Age_Group", "Gender", "Outcome","Number", "Source","Type"))==13



#Box link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1
#Dropbox link: https://www.dropbox.com/s/wq1rdkdin9a5c4g/MARC%20PCodes.csv?dl=0
marc_p <- readLines("MARC PCodes.csv")
rdt <- rdt[rdt$Tsp_Code %in% marc_p[-1],]

#cleaning up
rdt <- rdt[rdt$Outcome %in% c(pf,npf,neg),] #removing unknown outcomes
rdt <- rdt[rdt$Mth!="",] #removing records with no month information
rdt <- rdt[!is.na(rdt$Mth),] #removing recodes with NA
rdt <- rdt[rdt$Yr=="2014",] #subsetting for 2013
rdt <- rdt[rdt$Type %in% type2include,]

rdt$Mth[rdt$Mth=="April"] <- "Apr"#This will be removed when Wynn has cleaned the month
rdt$Mth[rdt$Mth=="July"] <- "Jul"

#Standardizing
rdt$Mth <- toupper(rdt$Mth)

sum(!(levels(factor(rdt$Mth)) %in% c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")))
#if the above value is more than 0, there's something wrong with the month values

rdt$Mth <- factor(rdt$Mth, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
rdt$State_Region <- toupper(rdt$State_Region)
rdt$Township <- toupper(rdt$Township)
#table(rdt$Mth)
#rdt$Outcome <- factor(rdt$Outcome)

#Recoding the Outcome variable
rdt$Outcome[rdt$Outcome %in% pf] <- "Pf"
rdt$Outcome[rdt$Outcome %in% npf] <- "Non-Pf"
#rdt$Outcome[rdt$Outcome %in% neg] <- "Neg"
rdt$Outcome <- factor(rdt$Outcome)

#Recoding IP names
levels(rdt$Source)[levels(rdt$Source)=="WHO"] <- "WHO/NMCP"
levels(rdt$Source)[levels(rdt$Source)=="NMCP"] <- "BHS"

#q1_2_table.R
library(reshape2)
#1. Table1: Outcomes per IP
#Per Source (IP)
perSource2014 <- dcast(rdt, Source ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perSource2014$Total <- perSource2014$Neg+perSource2014$`Non-Pf`+perSource2014$Pf
write.csv(perSource2014, paste("OCperIP2014_",Sys.Date(),".csv",sep=""))

#2. Table2: Outcomes per States, Regions
#Per Townships
perTsp2014 <- dcast(rdt, Tsp_Code+Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perTsp2014$Total <- perTsp2014$Neg+perTsp2014$`Non-Pf`+perTsp2014$Pf
write.csv(perTsp2014, paste("OCperTsp2014_",Sys.Date(),".csv",sep=""))


#3. Table3: Outcomes per Townships
#Per States/Divisions
perStates2014 <- dcast(rdt, State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number")
perStates2014$Total <- perStates2014$Neg+perStates2014$`Non-Pf`+perStates2014$Pf
write.csv(perStates2014, paste("OCperStates2014_",Sys.Date(),".csv",sep=""))

#4. Table4: Xtab Townships against IP (Total tests)
#Townships vs IP
tsp_ip_totaltests <- dcast(rdt, Tsp_Code+Township ~ Source, sum, na.rm=TRUE, value.var="Number")
write.csv(tsp_ip_totaltests, paste("Townships_IP_TotalTests_", Sys.Date(),".csv",sep=""))
#5. Table5: Xtab Townships against IP (Pf+Pmix)
rdt_tmp <- rdt[rdt$Outcome=="Pf",]
tsp_ip_pf <- dcast(rdt_tmp, Tsp_Code+Township ~ Source, sum, na.rm=TRUE, value.var="Number")
write.csv(tsp_ip_pf, paste("Townships_IP_Pf_", Sys.Date(),".csv",sep=""))
rdt_tmp <- NULL
#q1_3_trends_graph
#Plot1-3
#Required libraries for loading the function
library(zoo)
library(reshape2)

trendplot <- function(rdt=rdt, type){
  if(type=="chw"){
    rdt <- rdt[rdt$Expr1=="CHW"|rdt$Expr1=="Village",]
    typep <- "Community Health Worker"
    y_limits <- c(300,2000)
  }
  if(type=="hf"){
    rdt <- rdt[rdt$Expr1=="HF",]
    typep <- "Health Facility"
    y_limits <- c(500,2000)
  }
  if(type=="all") {
    typep <- "Health facility and CHW"
    y_limits <- c(500,3500)
  }
  
  combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
  #comb2013 <- combined[combined$Yr==2013,]
  
  
  combined$yrmth <- as.yearmon(paste(combined$Yr,combined$Mth), "%Y %b")
  combined$pf_npf <- combined$Pf+combined$`Non-Pf`
  combined$tested <- combined$Pf+combined$`Non-Pf`+combined$Neg
  
  #Plotting
  png(file=paste(typep,Sys.Date(),".png",sep=""), width=960, height=960)
  plot(combined$Pf ~ combined$yrmth, type="l", col="coral1", ylim=y_limits, main=paste("Malaria incidence (",typep,")\nMARC region, 2014",sep=""), xlab="Months", ylab="No. of Malaria Cases",lwd=3)
  lines(combined$`Non-Pf` ~ combined$yrmth, type="l", col="orange", lwd=3)
  legend("topright", legend=c("Pf+Pmix","Non-Pf"),lty=1, lwd=3,col=c("coral1","orange"))
  grid()
  dev.off()
}
trendplot(rdt=rdt, type="chw")+ trendplot(rdt=rdt, type="hf")+ trendplot(rdt=rdt, type="all")

#q1_4_bargraphs

library(reshape2)
combined <- dcast(rdt, Yr+Mth ~ Outcome, sum, na.rm=TRUE, value.var="Number") #To graph testing per month graphs
#comb2013 <- combined[combined$Yr==2013,]

#9. Plot4: Stacked Outcome plot per month
tcomb <- t(combined[,-1])
colnames(tcomb) <- tcomb[1,]
tcomb <- tcomb[-1,]
png(file=paste("stacked_oc_mnth_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(tcomb, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per Month\n MARC region, 2014", ylab="No. of Malaria Outcomes")
legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
dev.off()

#10. Plot5: Percentage Stacked Outcome plot per month
tcomb_prop <- t(prop.table(as.matrix(combined[,3:5]),1))
colnames(tcomb_prop) <- combined$Mth
png(file=paste("stacked_oc_mnth_percent_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(tcomb_prop, col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes per Month\n MARC region, 2014", ylab="Percentage of Malaria Outcomes per Month")
legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
dev.off()

#11. Plot6: Stacked Outcome plot per States/Regions
#pf npf comparison between states..divisons
states <- dcast(rdt, State_Region ~ Outcome, sum, na.rm=TRUE, value.var="Number") 
tstates <- t(states)
colnames(tstates) <- states$State_Region
tstates <- tstates[-1,]
png(file=paste("stacked_oc_states_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(tstates, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per State/Division\n in MARC region, 2014", ylab="No. of Malaria Outcomes")
legend("topleft",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
dev.off()

#12. Plot7: Percentage Stacked Outcome plot per States/Regions
#pf npf comparison between states..divisons
prop <- t(prop.table(as.matrix(states[,2:4]),1))
colnames(prop) <- states$State_Region
png(file=paste("stacked_oc_states_percent_",Sys.Date(),".png",sep=""), width=850, height=480)
barplot(prop,col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes in different States/Divisions\n MARC region, 2014", ylab="Percentage of Malaria Outcomes per State/Division")
legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
dev.off()

#13. Plot8: Stacked Outcome plot per Township
#pf npf comparison between townships
tsp <- dcast(rdt, Township ~ Outcome, sum, na.rm=TRUE, value.var="Number") 
tsp <- tsp[order(tsp$Neg+tsp$`Non-Pf`+tsp$Pf, decreasing=TRUE),]
ttsp <- t(tsp)
colnames(ttsp) <- tsp$Township
ttsp <- ttsp[-1,]
png(file=paste("stacked_oc_tsp_",Sys.Date(),".png",sep=""), width=1000, height=800)
par(mar=c(8,6,4,2))
barplot(ttsp, col=c("cornflowerblue","orange","coral1"), border="white", main="Malaria Outcomes per Township\n in MARC region, 2014", ylab="No. of Malaria Outcomes", las=2, cex.names=.8, mgp=c(4,1,0))
legend("topright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"), horiz=TRUE)
dev.off()

#14. Plot9: Percentage Stacked Outcome plot per Township
#pf npf percentage comparison between townships
tsp <- dcast(rdt, Township ~ Outcome, sum, na.rm=TRUE, value.var="Number")
prop_tsp <- t(prop.table(as.matrix(tsp[,2:4]),1))
colnames(prop_tsp) <- tsp$Township
prop_tsp_df <- as.data.frame(prop_tsp)

png(file=paste("stacked_oc_tsp_percent_",Sys.Date(),".png",sep=""), width=1000, height=500)
par(mar=c(8,4,4,2))
barplot(as.matrix(prop_tsp_df), cex.names=.8, las=2, col=c("cornflowerblue","orange","coral1"), border="white", main="Percentage of Malaria Outcomes in different Townships\n MARC region, 2014", ylab="Percentage of Malaria Outcomes per Township")
legend("bottomright",legend=c("Negative","Non-Pf","Pf+Pmix"),fill=c("cornflowerblue","orange","coral1"))
dev.off()
