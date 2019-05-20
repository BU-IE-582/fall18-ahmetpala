library(ggplot2)
library(readxl)
library(knitr)
library(tidyr)

#Reading the Datasets for Each Project Plan
H95  <- read_excel("C:/Users/ahmet.pala/Desktop/H95.xlsx")
H195 <- read_excel("C:/Users/ahmet.pala/Desktop/H195.xlsx")
H169 <- read_excel("C:/Users/ahmet.pala/Desktop/H169.xlsx")
H189 <- read_excel("C:/Users/ahmet.pala/Desktop/H189.xlsx")
H120 <- read_excel("C:/Users/ahmet.pala/Desktop/H120.xlsx")
H122 <- read_excel("C:/Users/ahmet.pala/Desktop/H122.xlsx")
H217 <- read_excel("C:/Users/ahmet.pala/Desktop/H217.xlsx")
H146 <- read_excel("C:/Users/ahmet.pala/Desktop/H146.xlsx")


# A Particular Function for Determining the Performance Metrics for Each Category
reporting <- function(dataframe){
  dataframe_future<-dataframe[dataframe$Status == 'Future Task',]
  dataframe<-dataframe[dataframe$Status != 'Future Task',]
  dataframe<-dataframe[complete.cases(dataframe$`Responsible`),]
  dataframe<-separate(dataframe,`Avtivity Code`,into = c("L","MW","Activity Type","SP"),   sep="-",remove = FALSE)

  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="PD","Piping Drawing")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="WD" | 
                                         dataframe$`Activity Type`=="OD","Block & Outfitting Drawing")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="BE" | 
                                         dataframe$`Activity Type`=="CN" | 
                                         dataframe$`Activity Type`=="CT" | 
                                         dataframe$`Activity Type`=="PR" | 
                                         dataframe$`Activity Type`=="TS", "Steel Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="EI","Equipment Installation")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="ES" | 
                                         dataframe$`Activity Type`=="EW","Electrical Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="CS","Quality Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="OT" | 
                                         dataframe$`Activity Type`=="OW","Outfitting")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="IS" | 
                                         dataframe$`Activity Type`=="FR","Insulation & HVAC")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="PW" | 
                                         dataframe$`Activity Type`=="PS","Piping Works")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="PT","Painting")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="ST" | 
                                         dataframe$`Activity Type`=="DW" | 
                                         dataframe$`Activity Type`=="PL","Extra Works & Milestones")
  dataframe$`Activity Type` <- replace(dataframe$`Activity Type`,dataframe$`Activity Type`=="CO" | 
                                         dataframe$`Activity Type`=="SS","Startup & Commissioning")
  dataframe<-dataframe[,-c(2,3,5)] 
  
  
  dataframe_future<-dataframe_future[complete.cases(dataframe_future$`Responsible`),]
  dataframe_future<-separate(dataframe_future,`Avtivity Code`,into = c("L","MW","Activity Type","SP"),   sep="-",remove = FALSE)
  
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="PD","Piping Drawing")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="WD" | 
                                                dataframe_future$`Activity Type`=="OD","Block & Outfitting Drawing")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="BE" | 
                                                dataframe_future$`Activity Type`=="CN" | 
                                                dataframe_future$`Activity Type`=="CT" | 
                                                dataframe_future$`Activity Type`=="PR" | 
                                                dataframe_future$`Activity Type`=="TS", "Steel Works")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="EI","Equipment Installation")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="ES" | 
                                                dataframe_future$`Activity Type`=="EW","Electrical Works")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="CS","Quality Works")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="OT" | 
                                                dataframe_future$`Activity Type`=="OW","Outfitting")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="IS" | 
                                                dataframe_future$`Activity Type`=="FR","Insulation & HVAC")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="PW" | 
                                                dataframe_future$`Activity Type`=="PS","Piping Works")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="PT","Painting")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="ST" | 
                                                dataframe_future$`Activity Type`=="DW" | 
                                                dataframe_future$`Activity Type`=="PL","Extra Works & Milestones")
  dataframe_future$`Activity Type` <- replace(dataframe_future$`Activity Type`,dataframe_future$`Activity Type`=="CO" | 
                                                dataframe_future$`Activity Type`=="SS","Startup & Commissioning")
  dataframe_future<-dataframe_future[,-c(2,3,5)]
  
  
  dataframe$Status <- replace(dataframe$Status,dataframe$Status == "Complete" | dataframe$Status == "On Schedule",0)
  dataframe$Status <- replace(dataframe$Status,dataframe$Status == "Late",1)
  colnames(dataframe)[7] <- "Delayed?"
  
  category <- data.frame(table(dataframe$`Activity Type`))
  reportmatrix<-data.frame(matrix(nrow = nrow(category),ncol = 5))
  reportmatrix[,1]<-category[,1]
  
  for (i in 1:nrow(reportmatrix)){
    reportmatrix[i,2]<-nrow(dataframe[dataframe$`Delayed?`==0 & dataframe$`Activity Type`==reportmatrix[i,1],])
    reportmatrix[i,3]<-nrow(dataframe[dataframe$`Delayed?`==1 & dataframe$`Activity Type`==reportmatrix[i,1],])
    reportmatrix[i,4]<-nrow(dataframe_future[dataframe_future$`Activity Type`==reportmatrix[i,1],])
  }
  
  dataframe$`Delayed?`<-as.numeric(dataframe$`Delayed?`)
  
  for(i in 1:nrow(reportmatrix)){
    reportmatrix[i,5]<-sum(dataframe[dataframe$`Activity Type` == reportmatrix[i,1],7])/nrow(dataframe[dataframe$`Activity Type`==reportmatrix[i,1],]) 
  }
  
  reportmatrix[,5]<-100*(1-reportmatrix[,5])
  colnames(reportmatrix)<-c("Category","Scheduled on Time or Completed Tasks", "Delayed Tasks" ,"Future Tasks", "Scheduled on Time or Completed Tasks Rate %")
  return(reportmatrix)
}

# Performance Reports and Graphs

# H95
H95_reportmatrix<-reporting(H95)
kable(H95_reportmatrix)
ggplot(H95_reportmatrix, aes(x = H95_reportmatrix$Category, 
                                  y = H95_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                                  ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H95") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)") 

# H169
H169_reportmatrix<-reporting(H169)
kable(H169_reportmatrix)
ggplot(H169_reportmatrix, aes(x = H169_reportmatrix$Category, 
                         y = H169_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                         ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H169") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

#H189
H189_reportmatrix<-reporting(H189)
kable(H189_reportmatrix)
ggplot(H189_reportmatrix, aes(x = H189_reportmatrix$Category, 
                              y = H189_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H189") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H195
H195_reportmatrix<-reporting(H195)
kable(H195_reportmatrix)
ggplot(H195_reportmatrix, aes(x = H195_reportmatrix$Category, 
                              y = H195_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H195") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H120
H120_reportmatrix<-reporting(H120)
kable(H120_reportmatrix)
ggplot(H120_reportmatrix, aes(x = H120_reportmatrix$Category, 
                              y = H120_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H120") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H122
H122_reportmatrix<-reporting(H122)
kable(H122_reportmatrix)
ggplot(H122_reportmatrix, aes(x = H122_reportmatrix$Category, 
                              y = H122_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H122") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H217
H217_reportmatrix<-reporting(H217)
kable(H217_reportmatrix)
ggplot(H217_reportmatrix, aes(x = H217_reportmatrix$Category, 
                              y = H217_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H217") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H146
H146_reportmatrix<-reporting(H146)
kable(H146_reportmatrix)
ggplot(H146_reportmatrix, aes(x = H146_reportmatrix$Category, 
                              y = H146_reportmatrix$'Scheduled on Time or Completed Tasks Rate %',
                              ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H146") + 
  xlab("Category") + ylab("Scheduled on Time or Completed Tasks Rate (%)")







