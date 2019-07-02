library(ggplot2)
library(readxl)
library(knitr)
library(tidyr)
library(plotly)

#Reading the Datasets for Each Project Plan
H169 <- read_excel("C:/Users/ahmet.pala/Desktop/H169.xlsx")
H189 <- read_excel("C:/Users/ahmet.pala/Desktop/H189.xlsx")
H195 <- read_excel("C:/Users/ahmet.pala/Desktop/H195.xlsx")
H119 <- read_excel("C:/Users/ahmet.pala/Desktop/H119.xlsx")
H120 <- read_excel("C:/Users/ahmet.pala/Desktop/H120.xlsx")
H122 <- read_excel("C:/Users/ahmet.pala/Desktop/H122.xlsx")
H123 <- read_excel("C:/Users/ahmet.pala/Desktop/H123.xlsx")
H95  <- read_excel("C:/Users/ahmet.pala/Desktop/H95.xlsx")
H217 <- read_excel("C:/Users/ahmet.pala/Desktop/H217.xlsx")
H146 <- read_excel("C:/Users/ahmet.pala/Desktop/H146.xlsx")

launchingfunction<-function(dataframe){
  dataframe<-dataframe[complete.cases(dataframe$`Avtivity Code`),]
  dataframe<-dataframe[complete.cases(dataframe$`Responsible`),]
  dataframe<-separate(dataframe,`Avtivity Code`,into = c("L","MW","Activity Type","SP"),   sep="-",remove = FALSE)
  launching<-dataframe[dataframe$`Activity Type`=="PL",]
  launching<-launching[,-c(2,3,4,5,7)]
  return(launching[,-5])
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



#H169
H169_launching<-launchingfunction(H169)
kable(H169_launching)
H169_reportmatrix<-completing(H169)
kable(H169_reportmatrix) 
plot_ly(y = H169_reportmatrix$`Completed Tasks Rate %`, x = H169_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H169",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear"))


#H189
H189_launching<-launchingfunction(H189)
kable(H189_launching)
H189_reportmatrix<-completing(H189)
kable(H189_reportmatrix)
plot_ly(y = H189_reportmatrix$`Completed Tasks Rate %`, x = H189_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H189",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear"))


# H195
H195_launching<-launchingfunction(H195)
kable(H195_launching)
H195_reportmatrix<-completing(H195)
kable(H195_reportmatrix)
plot_ly(y = H195_reportmatrix$`Completed Tasks Rate %`, x = H195_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H195",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear"))


# H119
H119_launching<-launchingfunction(H119)
kable(H119_launching)
H120_reportmatrix<-completing(H119)
kable(H119_reportmatrix)
plot_ly(y = H119_reportmatrix$`Completed Tasks Rate %`, x = H119_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H119",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear"))


# H120
H120_launching<-launchingfunction(H120)
kable(H120_launching)
H120_reportmatrix<-completing(H120)
kable(H120_reportmatrix)
plot_ly(y = H120_reportmatrix$`Completed Tasks Rate %`, x = H120_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H120",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear")) 


# H122
H122_launching<-launchingfunction(H122)
kable(H122_launching)
H122_reportmatrix<-completing(H122)
kable(H122_reportmatrix)
plot_ly(y = H122_reportmatrix$`Completed Tasks Rate %`, x = H122_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H122",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear")) 


# H123
H123_launching<-launchingfunction(H123)
kable(H123_launching)
H123_reportmatrix<-completing(H123)
kable(H123_reportmatrix)
plot_ly(y = H123_reportmatrix$`Completed Tasks Rate %`, x = H123_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H123",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear")) 


# H95
H95_launching<-launchingfunction(H95)
kable(H95_launching)
H95_reportmatrix<-completing(H95)
kable(H95_reportmatrix)
plot_ly(y = H95_reportmatrix$`Completed Tasks Rate %`, x = H95_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H95",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear")) 



# H217
H217_launching<-launchingfunction(H217)
kable(H217_launching)
H217_reportmatrix<-completing(H217)
kable(H217_reportmatrix)
plot_ly(y = H217_reportmatrix$`Completed Tasks Rate %`, x = H217_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H217",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear")) 


# H146
H146_launching<-launchingfunction(H146)
kable(H146_launching)
H146_reportmatrix<-completing(H146)
kable(H146_reportmatrix)
plot_ly(y = H146_reportmatrix$`Completed Tasks Rate %`, x = H146_reportmatrix$Category, type = "bar",color = I("brown")) %>%
  layout(
    title = "H146",
    xaxis = list(title = "Category",tickangle = -65),
    yaxis = list(title = "Completed Tasks Rate %",type="linear")) 

