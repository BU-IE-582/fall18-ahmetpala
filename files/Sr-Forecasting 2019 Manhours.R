
### Extracting Manhours Data From ERP ###

library(data.table)
library(readxl)
library(dplyr)
library(kableExtra)
library(tidyr)

file="C:/Users/ahmet.pala/Desktop/2017-2018 Taþeron Adam Saat Deðerleri (ay Bazýnda)/Taþeron Adam Saatler (ProjBz)/Reports (R)"
files1 <- sprintf('%s/%d.csv', file, 1:36) 
zz<-list.files(file, pattern=NULL, all.files=FALSE,
               full.names=TRUE)
Monthly_Mh=data.frame(matrix(0,nrow=length(zz), ncol=6))
Monthly_Mh$Month<-as.factor(rep(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"),3))
Monthly_Mh$Year<-(as.factor(rep(c("2016","2017","2018"),each=12)))
Monthly_Mh<-Monthly_Mh[,c(7,8,1,2,3,4,5,6)]
colnames(Monthly_Mh)<-c("Month","Year","Tuzla Approved","Tuzla Subcon","Yalova Approved","Yalova Subcon","OSB Approved","OSB Subcon")

# A particular algorithm for extracting monthly manhours from ERP reports
getvalues<-function(data){
  manhours<-data.frame(matrix(0,nrow =3,ncol = 3 ))
  manhours[,1]<-c("Yalova","Tuzla","OSB")
  colnames(manhours)<-c("Location","Approved Mh","Subcon. Mh")
  data<-data[,c(3,13,16)]
  colnames(data)<-c("Location","Approved","Subcon")
  data <- data[!is.na(data[,1]),]
  data$Approved<-as.numeric(data$Approved)
  data$Subcon<-as.numeric(data$Subcon)
  Yalova <- data[data$Location == "ALY" | data$Location =="AHB"| data$Location =="YBE" | data$Location =="YBP"| data$Location =="YHB" |data$Location =="YPP" | data$Location =="YSP" ,]
  Tuzla <- data[data$Location == "TBE" | data$Location =="TLY"| data$Location =="TPP",]
  OSB <- data[data$Location == "OBP" ,]
  manhours[1,-1]<-colSums(Tuzla[,-1])
  manhours[2,-1]<-colSums(Yalova[,-1])
  manhours[3,-1]<-colSums(OSB[,-1])
  return(manhours)
}


for(i in 1:36){
  data<-read_xlsx(zz[i])
  inter<-getvalues(data)
  Monthly_Mh[i,3]=inter[1,2]
  Monthly_Mh[i,4]=inter[1,3]
  Monthly_Mh[i,5]=inter[2,2]
  Monthly_Mh[i,6]=inter[2,3]
  Monthly_Mh[i,7]=inter[3,2]
  Monthly_Mh[i,8]=inter[3,3]
}


# Monthly Manhours Table
print("Monthly Manhours")

kable(Monthly_Mh) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


### Subcon. Manhour Graphs for each Location ###



# Defining Labels and Total Manhours Column
Monthly_Mh$Labels<-with(Monthly_Mh,paste0(Month,Year))
Monthly_Mh$Total_Subcon<-(Monthly_Mh$`Tuzla Subcon`+Monthly_Mh$`Yalova Subcon`+Monthly_Mh$`OSB Subcon`)


# Plotting Manhours For All Locations
plot(Monthly_Mh$`Tuzla Subcon`,type = "l",xaxt="n",xlab = "",ylab = "",las=1,main = "Subcon. Manhours",col="black",lwd=4)
title(ylab = "Manhour",line = 3.35)
axis(1, at=1:36, labels=Monthly_Mh$Labels,las=2)
par(new=TRUE)
plot(Monthly_Mh$`Yalova Subcon`,type ="l",axes = FALSE,col="red",xlab = "",ylab = "",lwd=3)
par(new=TRUE)
plot(Monthly_Mh$`OSB Subcon`,type ="l",axes = FALSE,col="blue",xlab = "",ylab = "",lwd=2)

legend(x=25,y=15000,c("Tuzla Subcon","Yalova Subcon","OSB Subcon"),cex=1.35,col=c("black","red","blue"),lwd =3)

#Printing Total Manhours
df_totalmh<-Monthly_Mh[,9:10]
print("Total Suncon. Manhours")
df_totalmh

# Plotting Total Subcon. Manhours
plot( Monthly_Mh$`Total_Subcon`,type = "l",xaxt="n",xlab = "",ylab = "",las=2,main = "Total Subcon. Manhours",col="black" ,lwd=3 )
title(ylab = "Manhour",line = 3.45)
axis(1, at=1:36, labels=Monthly_Mh$Labels,las=2)

#Forecasting 2019
datanew <- ts(Monthly_Mh$`Total_Subcon`, start = c(2016,01), end = c(2018,12), frequency = 12)

#plot(datanew)

hw <- HoltWinters(datanew)
plot(hw,main = "Holt Winters - Fitted Values",lwd=3)


forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast,main = "Forecasted Manhours for 2019",lwd=3,ylab = "Observed/Fitted Manhours")
legend("topright",c("Observed Mh","Fitted/Forecasted Mh","Upper & Lower Bounds for Forecasting"),cex=1.35,col=c("black","red","blue"),lwd =3)

#Printing Forecasted Manhours

print("Forecasted Manhours")
forecast




