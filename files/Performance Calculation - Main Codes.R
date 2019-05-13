library(ggplot2)
library(readxl)
library(knitr)

#Reading the Datasets for Each Project Plan
H195 <- read_excel("C:/Users/ahmet.pala/Desktop/H195.xlsx")
H169 <- read_excel("C:/Users/ahmet.pala/Desktop/H169.xlsx")
H189 <- read_excel("C:/Users/ahmet.pala/Desktop/H189.xlsx")
H120 <- read_excel("C:/Users/ahmet.pala/Desktop/H120.xlsx")
H122 <- read_excel("C:/Users/ahmet.pala/Desktop/H122.xlsx")
H95  <- read_excel("C:/Users/ahmet.pala/Desktop/H95.xlsx")
H217 <- read_excel("C:/Users/ahmet.pala/Desktop/H217.xlsx")
H146 <- read_excel("C:/Users/ahmet.pala/Desktop/H146.xlsx")


# A Particular Function for Determining the Performance Metrics for Each Responsible
performance<-function(dataframe){
  
  dataframe<-dataframe[complete.cases(dataframe$`Delayed?`),]
  dataframe<-dataframe[complete.cases(dataframe$`Responsible`),]
  
  personnel<-data.frame(table(dataframe$`Responsible`))
  
  performancematrix<-data.frame(matrix(nrow = nrow(personnel),ncol = 4))
  performancematrix[,1]<-personnel[,1]
  
  for (i in 1:nrow(performancematrix)){
    performancematrix[i,2]<-nrow(dataframe[dataframe$`Delayed?`==0 & dataframe$`Responsible`==performancematrix[i,1],])
    performancematrix[i,3]<-nrow(dataframe[dataframe$`Delayed?`==1 & dataframe$`Responsible`==performancematrix[i,1],])
  }
  
  for(i in 1:nrow(performancematrix)){
    performancematrix[i,4]<-sum(dataframe[dataframe$`Responsible` == personnel[i,1],6])/nrow(dataframe[dataframe$`Responsible`==personnel[i,1],]) 
  }
  
  performancematrix[,4]<-100*(1-performancematrix[,4])
  colnames(performancematrix)<-c("Responsibles","Scheduled on Time or Completed Tasks", "Delayed Tasks" ,"Scheduled on Time or Completed Tasks Rate %")
  return(performancematrix)
}

# Performance Reports and Graphs

# H169
H169_PerformanceMatrix<-performance(H169)
kable(H169_PerformanceMatrix)
ggplot(H169_PerformanceMatrix, aes(x = H169_PerformanceMatrix$'Responsibles', y = H169_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',ymin=100,ymax=100)) +
  geom_bar(stat = "identity") + ggtitle("H169") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

#H189
H189_PerformanceMatrix<-performance(H189)
kable(H189_PerformanceMatrix)
ggplot(H189_PerformanceMatrix, aes(x = H189_PerformanceMatrix$'Responsibles', 
                                   y = H189_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                                   ymin=100,ymax=100)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H189") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H195
H195_PerformanceMatrix<-performance(H195)
kable(H195_PerformanceMatrix)
ggplot(H195_PerformanceMatrix, aes(x = H195_PerformanceMatrix$'Responsibles', 
                               y = H195_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                               ymin=100,ymax=100)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H195") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)") 

# H120
H120_PerformanceMatrix<-performance(H120)
kable(H120_PerformanceMatrix)
ggplot(H120_PerformanceMatrix, aes(x = H120_PerformanceMatrix$'Responsibles', 
                               y = H120_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                               ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H120") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)") 

# H122
H122_PerformanceMatrix<-performance(H122)
kable(H122_PerformanceMatrix)
ggplot(H122_PerformanceMatrix, aes(x = H122_PerformanceMatrix$'Responsibles', 
                                   y = H122_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                                   ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H122") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)") 

# H95
H95_PerformanceMatrix<-performance(H95)
kable(H95_PerformanceMatrix)
ggplot(H95_PerformanceMatrix, aes(x = H95_PerformanceMatrix$'Responsibles', 
                                   y = H95_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                                   ymin=100,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H95") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)") 

# H217
H217_PerformanceMatrix<-performance(H217)
kable(H217_PerformanceMatrix)
ggplot(H217_PerformanceMatrix, aes(x = H217_PerformanceMatrix$'Responsibles', 
                                   y = H217_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                                   ymin=0,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H217") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)")

# H146
H146_PerformanceMatrix<-performance(H146)
kable(H146_PerformanceMatrix)
ggplot(H146_PerformanceMatrix, aes(x = H146_PerformanceMatrix$'Responsibles', 
                                   y = H146_PerformanceMatrix$'Scheduled on Time or Completed Tasks Rate %',
                                   ymin=0,ymax=100)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 100))+
  geom_bar(stat = "identity") + ggtitle("H146") + 
  xlab("Responsibles") + ylab("Scheduled on Time or Completed Tasks Rate (%)")







