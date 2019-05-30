library(ggplot2)
library(readxl)
library(knitr)
library(tidyr)

#Reading the Datasets for Each Project Plan
H169 <- read_excel("C:/Users/ahmet.pala/Desktop/H169.xlsx")
H189 <- read_excel("C:/Users/ahmet.pala/Desktop/H189.xlsx")
H195 <- read_excel("C:/Users/ahmet.pala/Desktop/H195.xlsx")
H120 <- read_excel("C:/Users/ahmet.pala/Desktop/H120.xlsx")
H122 <- read_excel("C:/Users/ahmet.pala/Desktop/H122.xlsx")
H95  <- read_excel("C:/Users/ahmet.pala/Desktop/H95.xlsx")
H217 <- read_excel("C:/Users/ahmet.pala/Desktop/H217.xlsx")
H146 <- read_excel("C:/Users/ahmet.pala/Desktop/H146.xlsx")

launchingfunction<-function(dataframe){
  dataframe<-dataframe[complete.cases(dataframe$`Avtivity Code`),]
  dataframe<-dataframe[complete.cases(dataframe$`Responsible`),]
  dataframe<-separate(dataframe,`Avtivity Code`,into = c("L","MW","Activity Type","SP"),   sep="-",remove = FALSE)
  launching<-dataframe[dataframe$`Activity Type`=="PL",]
  launching<-launching[,-c(2,3,4,5,7)]
  return(launching)
}


completing<-function(dataframe){
  dataframe<-dataframe[complete.cases(dataframe$`Avtivity Code`),]
  dataframe<-dataframe[complete.cases(dataframe$`Responsible`),]
  dataframe<-separate(dataframe,`Avtivity Code`,into = c("L","MW","Activity Type","SP"),   sep="-",remove = FALSE)
  dataframe<-dataframe[dataframe$`Activity Type`!="PL",]
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="PD","4-Piping Drawing")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="WD" | 
                                         dataframe$`Activity Type`=="OD","1-Block & Outfitting Drawing")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="BE" | 
                                         dataframe$`Activity Type`=="CN" | 
                                         dataframe$`Activity Type`=="CT" | 
                                         dataframe$`Activity Type`=="PR" | 
                                         dataframe$`Activity Type`=="TS", "2-Steel Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="EI","7-Equipment Installation")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="ES" | 
                                         dataframe$`Activity Type`=="EW","7-Electrical Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="CS","3-Quality Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="OT" | 
                                         dataframe$`Activity Type`=="OW","5-Outfitting")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="IS" | 
                                         dataframe$`Activity Type`=="FR","9-Insulation & HVAC")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="PW" | 
                                         dataframe$`Activity Type`=="PS","6-Piping Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="PT","10-Painting")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="ST" | 
                                         dataframe$`Activity Type`=="DW","12-Delivery Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="CO" |
                                         dataframe$`Activity Type`=="SS","11-Startup & Commissioning")
  dataframe<-dataframe[,-c(2,3,5)]
  
  category <- data.frame(table(dataframe$`Activity Type`))
  reportmatrix<-data.frame(matrix(nrow = nrow(category),ncol = 6))
  reportmatrix[,1]<-category[,1]
  
  for (i in 1:nrow(reportmatrix)){
    reportmatrix[i,2]<-nrow(dataframe[dataframe$Status=="Complete" & dataframe$`Activity Type`==reportmatrix[i,1],])
    reportmatrix[i,3]<-nrow(dataframe[dataframe$Status=="On Schedule" & dataframe$`Activity Type`==reportmatrix[i,1],])
    reportmatrix[i,4]<-nrow(dataframe[dataframe$Status=="Late" & dataframe$`Activity Type`==reportmatrix[i,1],])
    reportmatrix[i,5]<-nrow(dataframe[dataframe$Status=="Future Task" & dataframe$`Activity Type`==reportmatrix[i,1],])
  }
  
  for(i in 1:nrow(reportmatrix)){
    reportmatrix[i,6]<-reportmatrix[i,2]/rowSums(reportmatrix[i,-c(1,6)])}
  
  reportmatrix[,6]<-100*(reportmatrix[,6])
  colnames(reportmatrix)<-c("Category","Completed Tasks", "On Scheduled Tasks" , "Late Tasks" ,"Future Tasks", "Completed Tasks Rate %")
  
  reportmatrix<-separate(reportmatrix,Category,into = c("Number","Category"),   sep="-",remove = TRUE)
  reportmatrix$Number<-as.numeric(reportmatrix$Number)
  reportmatrix<-reportmatrix[order(reportmatrix$Number),]
  reportmatrix$Category <- factor(reportmatrix$Category, levels =    reportmatrix$Category[order(reportmatrix$Number)])
  reportmatrix$`Completed Tasks Rate %`<-round(reportmatrix$`Completed Tasks Rate %`,2)
  return(reportmatrix[,-1])
}

# Detailed Tasks Reports and Completed Tasks Graphs


# H169
H169_launching<-launchingfunction(H169)
kable(H169_launching)
H169_reportmatrix<-completing(H169)
kable(H169_reportmatrix)
ggplot(H169_reportmatrix, aes(x = H169_reportmatrix$Category, 
                              y = H169_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H169") +
  xlab("Category") + ylab("Completed Tasks Rate %") 

#H189
H189_launching<-launchingfunction(H189)
kable(H189_launching)
H189_reportmatrix<-completing(H189)
kable(H189_reportmatrix)
ggplot(H189_reportmatrix, aes(x = H189_reportmatrix$Category, 
                              y = H189_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H189") +
  xlab("Category") + ylab("Completed Tasks Rate %") 


# H195
H195_launching<-launchingfunction(H195)
kable(H195_launching)
H195_reportmatrix<-completing(H195)
kable(H195_reportmatrix)
ggplot(H195_reportmatrix, aes(x = H195_reportmatrix$Category, 
                              y = H195_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H195") +
  xlab("Category") + ylab("Completed Tasks Rate %") 

# H120
H120_launching<-launchingfunction(H120)
kable(H120_launching)
H120_reportmatrix<-completing(H120)
kable(H120_reportmatrix)
ggplot(H120_reportmatrix, aes(x = H120_reportmatrix$Category, 
                              y = H120_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H120") +
  xlab("Category") + ylab("Completed Tasks Rate %") 

# H122
H122_launching<-launchingfunction(H122)
kable(H122_launching)
H122_reportmatrix<-completing(H122)
kable(H122_reportmatrix)
ggplot(H122_reportmatrix, aes(x = H122_reportmatrix$Category, 
                              y = H122_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H122") +
  xlab("Category") + ylab("Completed Tasks Rate %")

# H95
H95_launching<-launchingfunction(H95)
kable(H95_launching)
H95_reportmatrix<-completing(H95)
kable(H95_reportmatrix)
ggplot(H95_reportmatrix, aes(x = H95_reportmatrix$Category, 
                             y = H95_reportmatrix$`Completed Tasks Rate %`,
                             ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H95") +
  xlab("Category") + ylab("Completed Tasks Rate %") 

# H217
H217_launching<-launchingfunction(H217)
kable(H217_launching)
H217_reportmatrix<-completing(H217)
kable(H217_reportmatrix)
ggplot(H217_reportmatrix, aes(x = H217_reportmatrix$Category, 
                              y = H217_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H217") +
  xlab("Category") + ylab("Completed Tasks Rate %")

# H146
H146_launching<-launchingfunction(H146)
kable(H146_launching)
H146_reportmatrix<-completing(H146)
kable(H146_reportmatrix)
ggplot(H146_reportmatrix, aes(x = H146_reportmatrix$Category, 
                              y = H146_reportmatrix$`Completed Tasks Rate %`,
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity",color="black",fill="brown") + ggtitle("H146") +
  xlab("Category") + ylab("Completed Tasks Rate %")






