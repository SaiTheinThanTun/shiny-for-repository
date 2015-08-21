setwd("C:/WorkingDirectoryName") #The name and path can be anywhere in your computer
#another example: setwd("C:/Users/Sai Thein Than Tun/Documents/R Working Directory")
#Copy the following files into your working directory:
#1. Q2.csv #Query 2 produced from Access, and then converted to CSV
#2. MARC PCodes.csv ##Link: https://app.box.com/s/4jwka23zsbsukxtng1c8t6jq9vpxayy1

#The outputs will also be produced in the same directory.
#Outputs are for 2014 MARC area: 
#1. Plot1: CHW histogram for average RDTs: Normal
#2. Plot2: CHW histogram for average RDTs:base2
#3. Plot3: Testing per IP, boxplot with outliers
#4. Plot4: Testing per IP, boxplot no outliers
#5. Plot5: Testing per State/Division, boxplot with outliers
#6. Plot6: Testing per State/Division, boxplot no outliers
#7. Table1: CHW count per [Township,IP] #Assuming that each village has a volunteer

library(reshape2)

setwd("C:/Users/Tom/Dropbox/1. MAEMOD/20. Myanmar Malaria Data Repository/2012-14 Data")

q2 <- read.csv("Q2.csv")

q2$Month[q2$Month=="April"] <- "Apr" #remove after being cleaned
q2$Month[q2$Month=="July"] <- "Jul"

#Subsetting for 2014 MARC, #and outcomes
q2 <- q2[q2$Year %in% c("2014","2013","2012"),]
marc_p <- readLines("MARC PCodes.csv")
q2 <- q2[q2$TS_Pcode %in% marc_p[-1],]
q2 <- q2[q2$Volunteer.Villages!="",]

#pf <- c("Mix","pf","Pf","PF")
#npf <- c("Non-Pf","pv","Pv")
#neg <- "Neg"

#q2 <- q2[q2$Outcome %in% c(pf,npf,neg),]
#q2$Outcome[q2$Outcome %in% pf] <- "Pf"
#q2$Outcome[q2$Outcome %in% npf] <- "Non-Pf"
#q2$Outcome[q2$Outcome %in% neg] <- "Neg"
#q2$Outcome <- factor(q2$Outcome)
q2 <- q2[,-9] #dropping the outcome categories


#Standardizing
q2$Month <- toupper(q2$Month) #Changing the months into ALLCAPS
q2$Month <- factor(q2$Month, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
q2 <- q2[!is.na(q2$Month),]
q2$MaxOfState..Division <- toupper(q2$MaxOfState..Division)
q2$MaxOfTownship <- toupper(q2$MaxOfTownship)

library(reshape2)

m_q2 <- melt(q2, names(q2)[-9]) #To place "CountOfOutcome" as value
m_q2 <- m_q2[m_q2$value!=0,]
m_q2 <- m_q2[!is.na(m_q2$value),] #to remove 3 NA's in 20150525 data query

###for shiny app
write.csv(m_q2,"m_q2.csv",row.names = F)

#uniq_villages <- dcast(q2, Volunteer.Villages+TS_Pcode+Source ~ Outcome, mean, na.rm=TRUE, value.var="CountOfOutcome")
uniq_villages <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source ~ variable, mean, na.rm=TRUE)
med_rdt <- median(uniq_villages$CountOfOutcome) #4.75
mean_rdt <- mean(uniq_villages$CountOfOutcome) #8.28854

uniq_villages_ym <- dcast(m_q2, MaxOfState..Division+MaxOfTownship+TS_Pcode+Volunteer.Villages+Source+Year+Month ~ variable, mean, na.rm=TRUE)

#write.csv(uniq_villages_time2,"CHWovertime.csv")


#1. Plot1: CHW histogram for average RDTs: Normal
uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,10,20,30,40,50,250), labels=c("<=10","11-20","21-30","31-40","41-50",">50"))
png(file=paste("chw_hist_normal_",Sys.Date(),".png",sep=""))
barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in MARC area, 2014 \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
dev.off()

#2. Plot2: CHW histogram for average RDTs:base2
uniq_villages$f <- cut(uniq_villages$CountOfOutcome, c(0,2^(0:6)[-1],250), labels=c("<=2","3-4","5-8","9-16","17-32","33-64",">64"))
png(file=paste("chw_hist_base2_",Sys.Date(),".png",sep=""))
barplot(table(uniq_villages$f), main=paste("Average malaria testing rates of \nCommunity Health Workers per month in MARC area, 2014 \n(median=",round(med_rdt,1),")",sep=""), xlab= "No. of malaria tests", ylab= "No. of Community Health Workers")
dev.off()

#3. Plot3: Testing per IP, boxplot with outliers
png(file=paste("boxplot_IP_outliers_",Sys.Date(),".png",sep=""),width=960, height=960)
boxplot(CountOfOutcome ~ Source,uniq_villages, xlab="Implementing partners", ylab="Malaria tests", main="Malaria testing rates \n among Implementing Partners, MARC area (2014)") 
dev.off()
#4. Plot4: Testing per IP, boxplot no outliers
png(file=paste("boxplot_IP_",Sys.Date(),".png",sep=""),width=960, height=960)
boxplot(CountOfOutcome ~ Source,uniq_villages, outline=FALSE, xlab="Implementing partners", ylab="Malaria tests", main="Malaria testing rates \n among Implementing Partners, MARC area (2014)") 
dev.off()

#5. Plot5: Testing per State/Division, boxplot with outliers
png(file=paste("boxplot_states_outliers_",Sys.Date(),".png",sep=""), width=640)
boxplot(CountOfOutcome ~ MaxOfState..Division,uniq_villages, xlab="States/Regions", ylab="Malaria tests", main="Malaria testing rates \n across States and Regions \n MARC area (2014)", cex.names=.8) 
dev.off()
#6. Plot6: Testing per State/Division, boxplot no outliers
png(file=paste("boxplot_states_",Sys.Date(),".png",sep=""), width=640)
boxplot(CountOfOutcome ~ MaxOfState..Division,uniq_villages, xlab="States/Regions", ylab="Malaria tests", main="Malaria testing rates \n across States and Regions \n MARC area (2014)", outline=FALSE, cex.names=.8) 
dev.off()

#7. Table1: CHW count per [Township,IP] #Assuming that each village has a volunteer
chw_tsp_ip <- dcast(uniq_villages, TS_Pcode+MaxOfTownship ~ Source, length, value.var="TS_Pcode")
chw_tsp_ip$total <- rowSums(chw_tsp_ip[,3:12])
write.csv(chw_tsp_ip, paste("CHW_count_tsp_IP_", Sys.Date(),".csv",sep=""))

#8. Table2: Number of CHW test per [Township, IP] #Assuming that each village has a volunteer
chwtst_tsp_ip <- dcast(uniq_villages, TS_Pcode+MaxOfTownship ~ Source, sum, value.var="CountOfOutcome")
chwtst_tsp_ip[,3:13] <- round(chwtst_tsp_ip[,3:13], digits=0)
write.csv(chwtst_tsp_ip, paste("CHW_test_tsp_IP_", Sys.Date(),".csv",sep=""))

#9. Table3: Number of CHW over time
chw_time <- dcast(uniq_villages_ym, Year+Month ~ TS_Pcode, length, value.var="TS_Pcode")
#chw_time["NA",]<-NULL
#chw_time <- chw_time[-37,]
chw_time$total <- rowSums(chw_time[,3:52])
write.csv(chw_time, paste("CHW_time_", Sys.Date(),".csv",sep=""))

