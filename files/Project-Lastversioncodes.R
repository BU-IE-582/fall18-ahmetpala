require(data.table)
require(anytime)

matches_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Data 201.11.2018/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Data 201.11.2018/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)
matches[,match_date:=anydate(date)]
matches$Weekday=wday(matches$match_date)
matches$Weekday=as.factor(matches$Weekday)
matches[,Month:=month(match_date)]
matches$Month=as.factor(matches$Month)
matches[,match_date:=NULL]


# Data preprocessing function for the matches data. This gives some particular features of this dataset.
matches_data_preprocessing <- function(data){
  
  temp = copy(data)
  temp = unique(temp,by="matchId") 
  setnames(temp,c("home","away","score","date"),c("Home","Away","Score","Match_Date"))
  temp[,Home:=tolower(Home)]
  temp[,Away:=tolower(Away)]
  temp[,Match_DateTime:=as.POSIXct(Match_Date,tz="UTC",origin = as.POSIXct("1970-01-01",tz="UTC"))]
  temp[,Match_Hour := format(strptime(Match_DateTime,"%Y-%m-%d %H:%M:%OS"),'%H')]
  temp[,Match_Hour := as.numeric(Match_Hour)]
  temp[,Match_Date := as.Date(Match_DateTime,format="%Y-%m-%d")]
  
  temp[,AWA_FLAG:=0]
  temp[(Score %like% "AWA."),`:=`(Score=gsub("AWA.","",Score),AWA_FLAG=1)]
  temp[,POSTP_FLAG:=0]
  temp[(Score == "POSTP."),`:=`(Score=gsub("POSTP.","",Score),POSTP_FLAG=1)]
  temp[,CAN_FLAG:=0]
  temp[(Score == "CAN."),`:=`(Score=gsub("CAN.","",Score),CAN_FLAG=1)]
  latestDateTimeofKnownScore = max(temp[Score!=""]$Match_DateTime)
  POSTP_toberemoved = temp[which(temp$POSTP_FLAG==1 & temp$Match_DateTime <= latestDateTimeofKnownScore)]$matchId
  if(length(POSTP_toberemoved)!=0){
    cat("Following postponed matches are REMOVED during data_preprocessing:\n")
    print(data[matchId%in%POSTP_toberemoved,])
    temp=temp[!matchId%in%POSTP_toberemoved,]
  }
  POSTP_tobekept = temp[which(temp$POSTP_FLAG==1 & temp$Match_DateTime > latestDateTimeofKnownScore)]$matchId
  if(length(POSTP_tobekept)!=0){
    cat("Following postponed matches are KEPT during data_preprocessing:\n")
    print(data[matchId%in%POSTP_tobekept,])
  }
  
  temp[,c("Home_Score","Away_Score") := lapply(tstrsplit(Score,":",fixed=T),as.integer)]
  temp[,`:=`(Match_Result = ifelse(Home_Score == Away_Score, "Tie" , ifelse(Home_Score > Away_Score, 'Home' , 'Away'))
             ,Total_Score = Home_Score + Away_Score)]
  temp[,`:=`(Result_Home = ifelse(Match_Result=="Home",1,0)
             ,Result_Tie = ifelse(Match_Result=="Tie",1,0)
             ,Result_Away = ifelse(Match_Result=="Away",1,0))]
  
  temp[,c('Score','Match_DateTime','AWA_FLAG','POSTP_FLAG','CAN_FLAG'):=NULL]
  gc()
  return(temp)
}

matches_new<-matches_data_preprocessing(matches)
#matches_new$Match_Hour=as.factor(matches_new$Match_Hour)


# Data preprocessing function for odd data. This gives open and close odds for oddtype 1x2 from the bookmakers.
details_data_preprocessing <- function(data,matches,which_bets=c('1x2'),remove_bookmaker=c('BetfairExchange','PaddyPower'),removeOlderThan=30){
  # data manipulation for historical odd data
  details = copy(data)
  
  #remove duplicate entries
  details = unique(details)
  
  details = details[betType %in% which_bets]
  details[,totalhandicap:=NULL]
  
  details = merge(details,matches[,list(matchId,Match_Date)],by="matchId",all.x=T)
  setnames(details,"date","OddChangeDateTime")
  details[,OddChangeDateTime:=as.POSIXct(OddChangeDateTime,tz="UTC",origin = as.POSIXct("1970-01-01",tz="UTC"))]
  details = details[difftime(Match_Date,OddChangeDateTime, units = "days") <= removeOlderThan] #remove odds seen earlier than 10 days from the match date
  details[, odd := as.numeric(odd)]
  
  details[,bookmaker:=gsub(" |-","",bookmaker)]
  if(!is.null(remove_bookmaker)){
    details = details[!(bookmaker %in% remove_bookmaker)]
  }
  
  gc()
  return(details)
}

odds_new<-details_data_preprocessing(odds,matches_new)

#' Feature Extraction

extract_features.openclose <- function(matches,odd_details,pMissThreshold=0.01,trainStart,testStart){
  
  details = copy(odd_details)
  matches = copy(matches)
  
  details=details[order(OddChangeDateTime)]
  feature_odd_details=details[,list(Odd_Open=odd[1],Odd_Close=odd[.N]),list(matchId,betType,oddtype,bookmaker)]
  
  feature_odd_details = merge(matches[,list(matchId,Match_Date)], feature_odd_details,by="matchId")
  
  
  #HANDLE MISSINGS
  details_temp = dcast(feature_odd_details, matchId+betType ~ paste0("Odd_Close_",bookmaker)+oddtype, value.var = c("Odd_Close"))
  details_melt = melt(details_temp, id.vars = c("matchId","betType"), measure.vars = names(details_temp)[names(details_temp) %like% "Odd_Close"], value.name = "odd")
  details_melt[,c("OpenClose","bookmaker","oddtype"):=tstrsplit(variable,split="_",keep=c(2:4))]
  details_melt[,variable:=NULL]
  details_melt = merge(matches[,list(matchId,Match_Date)], details_melt,by="matchId",all=T)
  
  bookieMissingness = details_melt[Match_Date >= trainStart,list(.N,percMiss=sum(is.na(odd))/.N),by=list(bookmaker,betType)]
  bookiesToKeep = unique(bookieMissingness[percMiss <= pMissThreshold]$bookmaker)
  cat("Number of bookmakers with proportion of missings below",pMissThreshold,"since",as.character(trainStart),":",length(bookiesToKeep),"\n")
  
  nonmissingBookmakers_sinceTestStart = unique(details_melt[Match_Date >= testStart, list(.N,NA_SUM=sum(is.na(odd))),by=list(bookmaker,betType)][NA_SUM==0]$bookmaker)
  bookiesToKeep = intersect(bookiesToKeep,nonmissingBookmakers_sinceTestStart)
  cat("Number of bookmakers with no missings since testStart", as.character(testStart), ":", length(bookiesToKeep), "\n")
  
  details = dcast(feature_odd_details,matchId~oddtype+bookmaker,value.var = c("Odd_Open","Odd_Close"))
  columnsToKeep = grep(paste(bookiesToKeep,collapse="|"),names(details),value=T)
  details = details[,c('matchId',columnsToKeep),with=F]
  #HANDLE MISSINGS END
  
  
  details = merge(matches[,-c('Home','Away','Home_Score','Away_Score','Total_Score','Result_Home','Result_Tie','Result_Away','type'),with=F],
                  details,by="matchId",all=T)
  
  
  return(features = details)
}

testStart=as.Date('2018-11-30')
trainStart=as.Date('2012-07-15')

# matches1<-matches
# matches1[,leagueId:=NULL]
# matches1[,score:=NULL]
# matches1[,date:=NULL]
# matches1[,type:=NULL]

features<-extract_features.openclose(matches_new,odds_new,trainStart =  trainStart,testStart = testStart)
# features<-merge(features,matches1,by="matchId")
# features<-unique(features,by="matchId")
# features$home=as.factor(features$home)
features$Match_Hour=as.factor(features$Match_Hour)
# features<-features[complete.cases(features)]


# Performance_metrics.r

#' Performance Metric: Ranked Probability Score

RPS_single<- function(probs,outcomes){
  probs = cumsum(probs)
  outcomes = cumsum(outcomes)
  RPS = sum((probs-outcomes )^2) / (length(probs)-1)
  return(RPS)
}

RPS_matrix<- function(probs,outcomes){
  probs=as.matrix(probs)
  outcomes=as.matrix(outcomes)
  probs=t(apply(t(probs), 2, cumsum))
  outcomes=t(apply(t(outcomes), 2, cumsum))
  RPS = apply((probs-outcomes)^2,1,sum) / (ncol(probs)-1)
  return(RPS)
}


# Multinomial Penalized (LASSO) Logistic Regression Function.
# This function also makes cross validation in order to find the optimum lambda value.
library(TunePareto)
library(glmnet)
train_glmnet <- function(train_features, test_features,not_included_feature_indices=c(1,2,3,7), alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=3,nFolds=10,trace=T){
  
  set.seed(5)
  
  # glmnet works with complete data
  glm_features=train_features[complete.cases(train_features)]
  train_class=glm_features$Match_Result
  glm_train_data=glm_features[,-not_included_feature_indices,with=F]
  glm_test_data=test_features[,-not_included_feature_indices,with=F]
  if(tune_lambda){
    # to set lambda parameter, cross-validation will be performed and lambda is selected based on RPS performance
    
    cvindices=generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
    
    # first get lambda sequence for all data
    glmnet_alldata = glmnet(data.matrix(glm_train_data), as.factor(train_class), family="multinomial", alpha = alpha, nlambda=nlambda)
    lambda_sequence = glmnet_alldata$lambda
    
    cvresult=vector('list',nofReplications*nFolds)
    iter=1
    for(i in 1:nofReplications) {
      thisReplication=cvindices[[i]]
      for(j in 1:nFolds){
        if(trace){
          cat(sprintf('Iteration %d: Fold %d of Replication %d\n',iter,j,i))
        }
        testindices=order(thisReplication[[j]])
        
        cvtrain=glm_train_data[-testindices]    
        cvtrainclass=train_class[-testindices]   
        cvtest=glm_train_data[testindices]
        cvtestclass=train_class[testindices] 
        
        inner_cv_glmnet_fit = glmnet(data.matrix(cvtrain),as.factor(cvtrainclass),family="multinomial", alpha = alpha,lambda=lambda_sequence)
        valid_pred = predict(inner_cv_glmnet_fit, data.matrix(cvtest), s = lambda_sequence, type = "response")
        
        #check order of predictions
        order_of_class=attr(valid_pred,'dimnames')[[2]]
        new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
        foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,new_order,x],result=cvtestclass)}))
        cvresult[[iter]]=foldresult
        iter=iter+1
      }
    }
    
    cvresult=rbindlist(cvresult)
    
    # creating actual targets for rps calculations
    cvresult[,pred_id:=1:.N]
    outcome_for_rps=data.table::dcast(cvresult,pred_id~result,value.var='pred_id')
    outcome_for_rps[,pred_id:=NULL]
    outcome_for_rps[is.na(outcome_for_rps)]=0
    outcome_for_rps[outcome_for_rps>0]=1
    setcolorder(outcome_for_rps,c('Home','Tie','Away'))
    
    
    # calculate RPS
    overall_results=data.table(cvresult[,list(repl,fold,lambda)],RPS=RPS_matrix(cvresult[,list(Home,Tie,Away)],outcome_for_rps))
    
    # summarize performance for each lambda
    overall_results_summary=overall_results[,list(RPS=mean(RPS)),list(repl,fold,lambda)]
    
    # find best lambdas as in glmnet based on RPS
    overall_results_summary=overall_results_summary[,list(meanRPS=mean(RPS),sdRPS=sd(RPS)),list(lambda)]
    overall_results_summary[,RPS_mean_lb := meanRPS - sdRPS]
    overall_results_summary[,RPS_mean_ub := meanRPS + sdRPS]
    
    cv_lambda_min=overall_results_summary[which.min(meanRPS)]$lambda
    
    semin=overall_results_summary[lambda==cv_lambda_min]$RPS_mean_ub
    cv_lambda.1se=max(overall_results_summary[meanRPS<semin]$lambda)
    
    cvResultsSummary = list(lambda.min =cv_lambda_min, lambda.1se = cv_lambda.1se,
                            meanRPS_min=overall_results_summary[lambda==cv_lambda_min]$meanRPS,
                            meanRPS_1se=overall_results_summary[lambda==cv_lambda.1se]$meanRPS)
    
  }
  
  # fit final glmnet model with the lambda with minimum error
  final_glmnet_fit = glmnet(data.matrix(glm_train_data),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
  # obtain predictions
  predicted_probabilities=predict(final_glmnet_fit, data.matrix(glm_test_data), type = "response")
  
  #check order of predictions
  order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
  new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
  
  final_result=data.table(test_features[,list(matchId,Match_Result)],predicted_probabilities[,new_order,1])
  
  return(list(predictions=final_result,cv_stats=cvResultsSummary))
}

# Calcultaions for Round 14
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
train_features<-na.omit(train_features) # Removing nonplayed matches
test_features=features[Match_Date>=testStart]

# Multinomial Penalized Logistic Regression
aa<-train_glmnet(train_features = train_features,test_features = test_features)

# RPS for Train Data
bb<-train_glmnet(train_features = train_features,test_features = train_features)
pred_train_glm<-data.frame(bb$predictions)

rps_train_glm<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-1)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-0)^2+0)
  
  }
}

average_rps_train_glm<-mean(rps_train_glm$V1)
print(c("Average RPS for train data (multinomial penalized logistic regression)", average_rps_train_glm))

# Predictions for Round 14

predictions_testdata_glm=data.frame(aa$predictions)
predictions_testdata_glm<-merge(matches,predictions_testdata_glm,by="matchId")
predictions_testdata_glm<-predictions_testdata_glm[,-c(2,5,6,7,8,9,10)]
predictions_testdata_glm <- predictions_testdata_glm[which(predictions_testdata_glm$matchId=="8pSKbJs5" 
                                                           | predictions_testdata_glm$matchId=="ja8YPGUS"
                                                           | predictions_testdata_glm$matchId=="Qia0Lzwp"
                                                           | predictions_testdata_glm$matchId=="8Gl5Kfhj"
                                                           | predictions_testdata_glm$matchId=="zNtirbpc"
                                                           | predictions_testdata_glm$matchId=="EDuesIa3"
                                                           | predictions_testdata_glm$matchId=="dxVatxF9"
                                                           | predictions_testdata_glm$matchId=="dx9UQdFM"
                                                           | predictions_testdata_glm$matchId=="IoAQRx0G"
                                                           | predictions_testdata_glm$matchId=="n7m9JE7d"), ] 

print("Predictions for round 14 (multinomial penalized logistic regression)")
predictions_testdata_glm

#Stochastic Gradient Boosting 
library(caret)
require(gbm)
trcontrol<-trainControl(method = "repeatedcv",number=5)
set.seed(5)

gbm_tree<-gbm(Match_Result~.-matchId -leagueId -Match_Date,data = train_features,distribution="multinomial",cv.folds = 10)
print("Relative influences of the features")
summary(gbm_tree)

predictions_traindata<-as.data.frame(predict(gbm_tree,type = "response",n.trees = 100))
rps_traindata<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-1)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-0)^2+0)
  
  }
}

average_rps_train_sgb<-mean(rps_traindata$V1)
print(c("Average RPS for traindata (stochastic gradient boosting)",average_rps_train_sgb))

predictions_testdata_sgb<-as.data.frame(predict(gbm_tree,test_features, type = "response",n.trees = 100))
predictions_testdata_sgb<-cbind(test_features$matchId,predictions_testdata_sgb)
colnames(predictions_testdata_sgb)[colnames(predictions_testdata_sgb)=="test_features$matchId"] <- "matchId"
predictions_testdata_sgb<-merge(matches,predictions_testdata_sgb,by="matchId")
predictions_testdata_sgb<-predictions_testdata_sgb[,-c(2,5,6,7,8,9)]
predictions_testdata_sgb <- predictions_testdata_sgb[which(predictions_testdata_sgb$matchId=="8pSKbJs5" 
                                                           | predictions_testdata_sgb$matchId=="ja8YPGUS"
                                                           | predictions_testdata_sgb$matchId=="Qia0Lzwp"
                                                           | predictions_testdata_sgb$matchId=="8Gl5Kfhj"
                                                           | predictions_testdata_sgb$matchId=="zNtirbpc"
                                                           | predictions_testdata_sgb$matchId=="EDuesIa3"
                                                           | predictions_testdata_sgb$matchId=="dxVatxF9"
                                                           | predictions_testdata_sgb$matchId=="dx9UQdFM"
                                                           | predictions_testdata_sgb$matchId=="IoAQRx0G"
                                                           | predictions_testdata_sgb$matchId=="n7m9JE7d"), ] 

predictions_testdata_sgb<-predictions_testdata_sgb[,c(1,2,3,5,6,4)]
colnames(predictions_testdata_sgb)<-c("matchId","home","away","Home","Tie","Away")
print("Predictions for round 14 (stochastic gradient boosting)")
predictions_testdata_sgb

# RPS for test data glm

predictions_testdata_glm$Result <- "Home"
predictions_testdata_glm[,7] = c("Home","Home","Away","Home","Away","Home","Tie","Home","Home","Home")

rps_test_glm<-as.data.frame(matrix(nrow = nrow(predictions_testdata_glm),ncol = 1))
for (i in 1:nrow(predictions_testdata_glm)){
  if (predictions_testdata_glm[i,7]=="Home") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,4]-1)^2+(predictions_testdata_glm[i,4]+predictions_testdata_glm[i,5]-1)^2+0)
  }
  else if (predictions_testdata_glm[i,7]=="Tie") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,4]-0)^2+(predictions_testdata_glm[i,4]+predictions_testdata_glm[i,5]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_glm[i,7]=="Away") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,4]-0)^2+(predictions_testdata_glm[i,4]+predictions_testdata_glm[i,5]-0)^2+0)
  
  }
}
average_rps_test_glm<-mean(rps_test_glm$V1)
print(c("Average RPS for test data (multinomial penalized logistic regression)",average_rps_test_glm))

# RPS for test data sgb

Results<-read.csv("C:/Users/AHMET/Desktop/IE 582/Project/Submission 1 (Round 14)/Results.csv"
                  ,sep = ";")
colnames(Results)<-c("matchId","Result")

predictions_testdata_sgb<-merge(Results,predictions_testdata_sgb,by="matchId")

rps_test_sgb<-as.data.frame(matrix(nrow = nrow(predictions_testdata_sgb),ncol = 1))
for (i in 1:nrow(predictions_testdata_sgb)){
  if (predictions_testdata_sgb[i,2]=="Home") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-1)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }
  else if (predictions_testdata_sgb[i,2]=="Tie") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_sgb[i,2]=="Away") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-0)^2+0)
  
  }
}
average_rps_test_sgb<-mean(rps_test_sgb$V1)
print(c("Average RPS for testdata (stochastic gradient boosting)",average_rps_test_sgb))

# Calcultaions for Round 16

matches_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 3 (Round 16)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 3 (Round 16)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)
matches[,match_date:=anydate(date)]
matches$Weekday=wday(matches$match_date)
matches$Weekday=as.factor(matches$Weekday)
matches[,Month:=month(match_date)]
matches$Month=as.factor(matches$Month)
matches[,match_date:=NULL]

matches_new<-matches_data_preprocessing(matches)
odds_new<-details_data_preprocessing(odds,matches_new)

testStart=as.Date('2018-12-08')
trainStart=as.Date('2012-07-15')

features<-extract_features.openclose(matches_new,odds_new,trainStart =  trainStart,testStart = testStart)
features$Match_Hour=as.factor(features$Match_Hour)

train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
train_features<-na.omit(train_features) # Removing nonplayed matches
test_features=features[Match_Date>=testStart]

# Multinomial Penalized Logistic Regression
aa<-train_glmnet(train_features = train_features,test_features = test_features)

# RPS for Train Data
bb<-train_glmnet(train_features = train_features,test_features = train_features)
pred_train_glm<-data.frame(bb$predictions)

rps_train_glm<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-1)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-0)^2+0)
  
  }
}

average_rps_train_glm<-mean(rps_train_glm$V1)
print(c("Average RPS for train data (multinomial penalized logistic regression)", average_rps_train_glm))

# Predictions for Round 16

predictions_testdata_glm=data.frame(aa$predictions)
predictions_testdata_glm<-merge(matches,predictions_testdata_glm,by="matchId")
predictions_testdata_glm<-predictions_testdata_glm[,-c(2,5,6,7,8,9,10)]
predictions_testdata_glm <- predictions_testdata_glm[which(predictions_testdata_glm$matchId=="8AZxhZDL" 
                                                           | predictions_testdata_glm$matchId=="UJzZhFbF"
                                                           | predictions_testdata_glm$matchId=="jZOsigTR"
                                                           | predictions_testdata_glm$matchId=="tbQSdcCH"
                                                           | predictions_testdata_glm$matchId=="GG8MqBL1"
                                                           | predictions_testdata_glm$matchId=="8Ae87ED8"
                                                           | predictions_testdata_glm$matchId=="QRF9nDyq"
                                                           | predictions_testdata_glm$matchId=="buEHpi6e"
                                                           | predictions_testdata_glm$matchId=="f77QrVz8"
                                                           | predictions_testdata_glm$matchId=="KlFDoXjk"), ] 

print("Predictions for round 16 (multinomial penalized logistic regression)")
predictions_testdata_glm

#Stochastic Gradient Boosting 
library(caret)
require(gbm)
trcontrol<-trainControl(method = "repeatedcv",number=5)
set.seed(5)

gbm_tree<-gbm(Match_Result~.-matchId -leagueId -Match_Date,data = train_features,distribution="multinomial",cv.folds = 10)
print("Relative influences of the features")
summary(gbm_tree)

predictions_traindata<-as.data.frame(predict(gbm_tree,type = "response",n.trees = 100))
rps_traindata<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-1)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-0)^2+0)
  
  }
}

average_rps_train_sgb<-mean(rps_traindata$V1)
print(c("Average RPS for traindata (stochastic gradient boosting)",average_rps_train_sgb))

predictions_testdata_sgb<-as.data.frame(predict(gbm_tree,test_features, type = "response",n.trees = 100))
predictions_testdata_sgb<-cbind(test_features$matchId,predictions_testdata_sgb)
colnames(predictions_testdata_sgb)[colnames(predictions_testdata_sgb)=="test_features$matchId"] <- "matchId"
predictions_testdata_sgb<-merge(matches,predictions_testdata_sgb,by="matchId")
predictions_testdata_sgb<-predictions_testdata_sgb[,-c(2,5,6,7,8,9)]
predictions_testdata_sgb <- predictions_testdata_sgb[which(predictions_testdata_sgb$matchId=="8AZxhZDL" 
                                                           | predictions_testdata_sgb$matchId=="UJzZhFbF"
                                                           | predictions_testdata_sgb$matchId=="jZOsigTR"
                                                           | predictions_testdata_sgb$matchId=="tbQSdcCH"
                                                           | predictions_testdata_sgb$matchId=="GG8MqBL1"
                                                           | predictions_testdata_sgb$matchId=="8Ae87ED8"
                                                           | predictions_testdata_sgb$matchId=="QRF9nDyq"
                                                           | predictions_testdata_sgb$matchId=="buEHpi6e"
                                                           | predictions_testdata_sgb$matchId=="f77QrVz8"
                                                           | predictions_testdata_sgb$matchId=="KlFDoXjk"), ] 

predictions_testdata_sgb<-predictions_testdata_sgb[,c(1,2,3,5,6,4)]
colnames(predictions_testdata_sgb)<-c("matchId","home","away","Home","Tie","Away")
print("Predictions for round 16 (stochastic gradient boosting)")
predictions_testdata_sgb

# RPS for test data glm

Results<-read.csv("C:/Users/AHMET/Desktop/IE 582/Project/Submission 3 (Round 16)/Results.csv"
                  ,sep = ";")
colnames(Results)<-c("matchId","Result")

predictions_testdata_glm<-merge(Results,predictions_testdata_glm,by="matchId")

rps_test_glm<-as.data.frame(matrix(nrow = nrow(predictions_testdata_glm),ncol = 1))
for (i in 1:nrow(predictions_testdata_glm)){
  if (predictions_testdata_glm[i,2]=="Home") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-1)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }
  else if (predictions_testdata_glm[i,2]=="Tie") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_glm[i,2]=="Away") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-0)^2+0)
  
  }
}
average_rps_test_glm<-mean(rps_test_glm$V1)
print(c("Average RPS for test data (multinomial penalized logistic regression)",average_rps_test_glm))

# RPS for test data sgb

predictions_testdata_sgb<-merge(Results,predictions_testdata_sgb,by="matchId")

rps_test_sgb<-as.data.frame(matrix(nrow = nrow(predictions_testdata_sgb),ncol = 1))
for (i in 1:nrow(predictions_testdata_sgb)){
  if (predictions_testdata_sgb[i,2]=="Home") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-1)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }
  else if (predictions_testdata_sgb[i,2]=="Tie") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_sgb[i,2]=="Away") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-0)^2+0)
  
  }
}
average_rps_test_sgb<-mean(rps_test_sgb$V1)
print(c("Average RPS for testdata (stochastic gradient boosting)",average_rps_test_sgb))

# Calcultaions for Round 17

require(data.table)
require(anytime)

matches_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 4 (Round 17)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 4 (Round 17)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)
matches[,match_date:=anydate(date)]
matches$Weekday=wday(matches$match_date)
matches$Weekday=as.factor(matches$Weekday)
matches[,Month:=month(match_date)]
matches$Month=as.factor(matches$Month)
matches[,match_date:=NULL]

matches_new<-matches_data_preprocessing(matches)
odds_new<-details_data_preprocessing(odds,matches_new)

testStart=as.Date('2018-12-15')
trainStart=as.Date('2012-07-15')

features<-extract_features.openclose(matches_new,odds_new,trainStart =  trainStart,testStart = testStart)
features$Match_Hour=as.factor(features$Match_Hour)

train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
train_features<-na.omit(train_features) # Removing matches not played
test_features=features[Match_Date>=testStart]

# Multinomial Penalized Logistic Regression
aa<-train_glmnet(train_features = train_features,test_features = test_features)

# RPS for Train Data
bb<-train_glmnet(train_features = train_features,test_features = train_features)
pred_train_glm<-data.frame(bb$predictions)

rps_train_glm<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-1)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-0)^2+0)
  
  }
}

average_rps_train_glm<-mean(rps_train_glm$V1)
print(c("Average RPS for train data (multinomial penalized logistic regression)", average_rps_train_glm))

# Predictions for Round 17

predictions_testdata_glm=data.frame(aa$predictions)
predictions_testdata_glm<-merge(matches,predictions_testdata_glm,by="matchId")
predictions_testdata_glm<-predictions_testdata_glm[,-c(2,5,6,7,8,9,10)]
predictions_testdata_glm <- predictions_testdata_glm[which(predictions_testdata_glm$matchId=="zROebAze" 
                                                           | predictions_testdata_glm$matchId=="4xkH5hrL"
                                                           | predictions_testdata_glm$matchId=="EBzm0W5r"
                                                           | predictions_testdata_glm$matchId=="riS3dl57"
                                                           | predictions_testdata_glm$matchId=="hzgduVtp"
                                                           | predictions_testdata_glm$matchId=="MsR7e8KD"
                                                           | predictions_testdata_glm$matchId=="rTmL4CcR"
                                                           | predictions_testdata_glm$matchId=="MDaC6YSE"
                                                           | predictions_testdata_glm$matchId=="lUSacUk1"
                                                           | predictions_testdata_glm$matchId=="h2ZjajLl"), ] 

print("Predictions for round 17 (multinomial penalized logistic regression)")
predictions_testdata_glm

#Stochastic Gradient Boosting 
library(caret)
require(gbm)
trcontrol<-trainControl(method = "repeatedcv",number=5)
set.seed(5)

gbm_tree<-gbm(Match_Result~.-matchId -leagueId -Match_Date,data = train_features,distribution="multinomial",cv.folds = 10)
print("Relative influences of the features")
summary(gbm_tree)

predictions_traindata<-as.data.frame(predict(gbm_tree,type = "response",n.trees = 100))
rps_traindata<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-1)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-0)^2+0)
  
  }
}

average_rps_train_sgb<-mean(rps_traindata$V1)
print(c("Average RPS for traindata (stochastic gradient boosting)",average_rps_train_sgb))

predictions_testdata_sgb<-as.data.frame(predict(gbm_tree,test_features, type = "response",n.trees = 100))
predictions_testdata_sgb<-cbind(test_features$matchId,predictions_testdata_sgb)
colnames(predictions_testdata_sgb)[colnames(predictions_testdata_sgb)=="test_features$matchId"] <- "matchId"
predictions_testdata_sgb<-merge(matches,predictions_testdata_sgb,by="matchId")
predictions_testdata_sgb<-predictions_testdata_sgb[,-c(2,5,6,7,8,9)]
predictions_testdata_sgb <- predictions_testdata_sgb[which(predictions_testdata_sgb$matchId=="zROebAze" 
                                                           | predictions_testdata_sgb$matchId=="4xkH5hrL"
                                                           | predictions_testdata_sgb$matchId=="EBzm0W5r"
                                                           | predictions_testdata_sgb$matchId=="riS3dl57"
                                                           | predictions_testdata_sgb$matchId=="hzgduVtp"
                                                           | predictions_testdata_sgb$matchId=="MsR7e8KD"
                                                           | predictions_testdata_sgb$matchId=="rTmL4CcR"
                                                           | predictions_testdata_sgb$matchId=="MDaC6YSE"
                                                           | predictions_testdata_sgb$matchId=="lUSacUk1"
                                                           | predictions_testdata_sgb$matchId=="h2ZjajLl"), ] 

predictions_testdata_sgb<-predictions_testdata_sgb[,c(1,2,3,5,6,4)]
colnames(predictions_testdata_sgb)<-c("matchId","home","away","Home","Tie","Away")
print("Predictions for round 17 (stochastic gradient boosting)")
predictions_testdata_sgb

# RPS for test data glm

Results<-read.csv("C:/Users/AHMET/Desktop/IE 582/Project/Submission 4 (Round 17)/Results.csv"
                  ,sep = ";")
colnames(Results)<-c("matchId","Result")

predictions_testdata_glm<-merge(Results,predictions_testdata_glm,by="matchId")

rps_test_glm<-as.data.frame(matrix(nrow = nrow(predictions_testdata_glm),ncol = 1))
for (i in 1:nrow(predictions_testdata_glm)){
  if (predictions_testdata_glm[i,2]=="Home") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-1)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }
  else if (predictions_testdata_glm[i,2]=="Tie") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_glm[i,2]=="Away") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-0)^2+0)
  
  }
}
average_rps_test_glm<-mean(rps_test_glm$V1)
print(c("Average RPS for test data (multinomial penalized logistic regression)",average_rps_test_glm))

# RPS for test data sgb

predictions_testdata_sgb<-merge(Results,predictions_testdata_sgb,by="matchId")

rps_test_sgb<-as.data.frame(matrix(nrow = nrow(predictions_testdata_sgb),ncol = 1))
for (i in 1:nrow(predictions_testdata_sgb)){
  if (predictions_testdata_sgb[i,2]=="Home") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-1)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }
  else if (predictions_testdata_sgb[i,2]=="Tie") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_sgb[i,2]=="Away") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-0)^2+0)
  
  }
}
average_rps_test_sgb<-mean(rps_test_sgb$V1)
print(c("Average RPS for testdata (stochastic gradient boosting)",average_rps_test_sgb))


# Calcultaions for Round 18

matches_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 5 (Round 18)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 5 (Round 18)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)
matches[,match_date:=anydate(date)]
matches$Weekday=wday(matches$match_date)
matches$Weekday=as.factor(matches$Weekday)
matches[,Month:=month(match_date)]
matches$Month=as.factor(matches$Month)
matches[,match_date:=NULL]

matches_new<-matches_data_preprocessing(matches)
odds_new<-details_data_preprocessing(odds,matches_new)

testStart=as.Date('2018-12-21')
trainStart=as.Date('2012-07-15')

features<-extract_features.openclose(matches_new,odds_new,trainStart =  trainStart,testStart = testStart)
features$Match_Hour=as.factor(features$Match_Hour)

train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
train_features<-na.omit(train_features) # Removing matches not played
test_features=features[Match_Date>=testStart]

# Multinomial Penalized Logistic Regression
aa<-train_glmnet(train_features = train_features,test_features = test_features)

# RPS for Train Data
bb<-train_glmnet(train_features = train_features,test_features = train_features)
pred_train_glm<-data.frame(bb$predictions)

rps_train_glm<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-1)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-0)^2+0)
  
  }
}

average_rps_train_glm<-mean(rps_train_glm$V1)
print(c("Average RPS for train data (multinomial penalized logistic regression)", average_rps_train_glm))

# Predictions for Round 18

predictions_testdata_glm=data.frame(aa$predictions)
predictions_testdata_glm<-merge(matches,predictions_testdata_glm,by="matchId")
predictions_testdata_glm<-predictions_testdata_glm[,-c(2,5,6,7,8,9,10)]
predictions_testdata_glm <- predictions_testdata_glm[which(predictions_testdata_glm$matchId=="z7UdYJYN" 
                                                           | predictions_testdata_glm$matchId=="fV70dWKD"
                                                           | predictions_testdata_glm$matchId=="WfVTv4kt"
                                                           | predictions_testdata_glm$matchId=="QHUXwO4n"
                                                           | predictions_testdata_glm$matchId=="6Rxtx2Zb"
                                                           | predictions_testdata_glm$matchId=="pIypyMl5"
                                                           | predictions_testdata_glm$matchId=="Eitlzt4B"
                                                           | predictions_testdata_glm$matchId=="hruhZaJH"
                                                           | predictions_testdata_glm$matchId=="zar1vkdj"
                                                           | predictions_testdata_glm$matchId=="O2mywrKh"), ] 

print("Predictions for round 18 (multinomial penalized logistic regression)")
predictions_testdata_glm

#Stochastic Gradient Boosting 
library(caret)
require(gbm)
trcontrol<-trainControl(method = "repeatedcv",number=5)
set.seed(5)

gbm_tree<-gbm(Match_Result~.-matchId -leagueId -Match_Date,data = train_features,distribution="multinomial",cv.folds = 10)
print("Relative influences of the features")
summary(gbm_tree)

predictions_traindata<-as.data.frame(predict(gbm_tree,type = "response",n.trees = 100))
rps_traindata<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-1)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-0)^2+0)
  
  }
}

average_rps_train_sgb<-mean(rps_traindata$V1)
print(c("Average RPS for traindata (stochastic gradient boosting)",average_rps_train_sgb))

predictions_testdata_sgb<-as.data.frame(predict(gbm_tree,test_features, type = "response",n.trees = 100))
predictions_testdata_sgb<-cbind(test_features$matchId,predictions_testdata_sgb)
colnames(predictions_testdata_sgb)[colnames(predictions_testdata_sgb)=="test_features$matchId"] <- "matchId"
predictions_testdata_sgb<-merge(matches,predictions_testdata_sgb,by="matchId")
predictions_testdata_sgb<-predictions_testdata_sgb[,-c(2,5,6,7,8,9)]
predictions_testdata_sgb <- predictions_testdata_sgb[which(predictions_testdata_sgb$matchId=="z7UdYJYN" 
                                                           | predictions_testdata_sgb$matchId=="fV70dWKD"
                                                           | predictions_testdata_sgb$matchId=="WfVTv4kt"
                                                           | predictions_testdata_sgb$matchId=="QHUXwO4n"
                                                           | predictions_testdata_sgb$matchId=="6Rxtx2Zb"
                                                           | predictions_testdata_sgb$matchId=="pIypyMl5"
                                                           | predictions_testdata_sgb$matchId=="Eitlzt4B"
                                                           | predictions_testdata_sgb$matchId=="hruhZaJH"
                                                           | predictions_testdata_sgb$matchId=="zar1vkdj"
                                                           | predictions_testdata_sgb$matchId=="O2mywrKh"), ] 

predictions_testdata_sgb<-predictions_testdata_sgb[,c(1,2,3,5,6,4)]
colnames(predictions_testdata_sgb)<-c("matchId","home","away","Home","Tie","Away")
print("Predictions for round 18 (stochastic gradient boosting)")
predictions_testdata_sgb

# RPS for test data glm

Results<-read.csv("C:/Users/AHMET/Desktop/IE 582/Project/Submission 5 (Round 18)/Results.csv"
                  ,sep = ";")
colnames(Results)<-c("matchId","Result")

predictions_testdata_glm<-merge(Results,predictions_testdata_glm,by="matchId")

rps_test_glm<-as.data.frame(matrix(nrow = nrow(predictions_testdata_glm),ncol = 1))
for (i in 1:nrow(predictions_testdata_glm)){
  if (predictions_testdata_glm[i,2]=="Home") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-1)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }
  else if (predictions_testdata_glm[i,2]=="Tie") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_glm[i,2]=="Away") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-0)^2+0)
  
  }
}
average_rps_test_glm<-mean(rps_test_glm$V1)
print(c("Average RPS for test data (multinomial penalized logistic regression)",average_rps_test_glm))

# RPS for test data sgb

predictions_testdata_sgb<-merge(Results,predictions_testdata_sgb,by="matchId")

rps_test_sgb<-as.data.frame(matrix(nrow = nrow(predictions_testdata_sgb),ncol = 1))
for (i in 1:nrow(predictions_testdata_sgb)){
  if (predictions_testdata_sgb[i,2]=="Home") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-1)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }
  else if (predictions_testdata_sgb[i,2]=="Tie") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_sgb[i,2]=="Away") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-0)^2+0)
  
  }
}
average_rps_test_sgb<-mean(rps_test_sgb$V1)
print(c("Average RPS for testdata (stochastic gradient boosting)",average_rps_test_sgb))

# Calcultaions for Round 20

matches_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 7 (Round 20)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 7 (Round 20)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)
matches[,match_date:=anydate(date)]
matches$Weekday=wday(matches$match_date)
matches$Weekday=as.factor(matches$Weekday)
matches[,Month:=month(match_date)]
matches$Month=as.factor(matches$Month)
matches[,match_date:=NULL]

matches_new<-matches_data_preprocessing(matches)
odds_new<-details_data_preprocessing(odds,matches_new)

testStart=as.Date('2018-12-29')
trainStart=as.Date('2012-07-15')

features<-extract_features.openclose(matches_new,odds_new,trainStart =  trainStart,testStart = testStart)
features$Match_Hour=as.factor(features$Match_Hour)

train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
train_features<-na.omit(train_features) # Removing matches not played
test_features=features[Match_Date>=testStart]

#Multinomial Penalized Logistic Regression
aa<-train_glmnet(train_features = train_features,test_features = test_features)

# RPS for Train Data
bb<-train_glmnet(train_features = train_features,test_features = train_features)
pred_train_glm<-data.frame(bb$predictions)

rps_train_glm<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-1)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-0)^2+0)
  
  }
}

average_rps_train_glm<-mean(rps_train_glm$V1)
print(c("Average RPS for train data (multinomial penalized logistic regression)", average_rps_train_glm))

# Predictions for Round 20

predictions_testdata_glm=data.frame(aa$predictions)
predictions_testdata_glm<-merge(matches,predictions_testdata_glm,by="matchId")
predictions_testdata_glm<-predictions_testdata_glm[,-c(2,5,6,7,8,9,10)]
predictions_testdata_glm <- predictions_testdata_glm[which(predictions_testdata_glm$matchId=="MwLWpcIT" 
                                                           | predictions_testdata_glm$matchId=="xSlo4KIi"
                                                           | predictions_testdata_glm$matchId=="25zm2hlc"
                                                           | predictions_testdata_glm$matchId=="67t20xIG"
                                                           | predictions_testdata_glm$matchId=="0Uu6adXM"
                                                           | predictions_testdata_glm$matchId=="Wllk3vYc"
                                                           | predictions_testdata_glm$matchId=="Qios503o"
                                                           | predictions_testdata_glm$matchId=="On7btHQp"
                                                           | predictions_testdata_glm$matchId=="SGsb1I2A"
                                                           | predictions_testdata_glm$matchId=="Aumg2bm4"), ] 

print("Predictions for round 20 (multinomial penalized logistic regression)")
predictions_testdata_glm

#Stochastic Gradient Boosting 
library(caret)
require(gbm)
trcontrol<-trainControl(method = "repeatedcv",number=5)
set.seed(5)

gbm_tree<-gbm(Match_Result~.-matchId -leagueId -Match_Date,data = train_features,distribution="multinomial",cv.folds = 10)
print("Relative influences of the features")
summary(gbm_tree)

predictions_traindata<-as.data.frame(predict(gbm_tree,type = "response",n.trees = 100))
rps_traindata<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-1)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-0)^2+0)
  
  }
}

average_rps_train_sgb<-mean(rps_traindata$V1)
print(c("Average RPS for traindata (stochastic gradient boosting)",average_rps_train_sgb))

predictions_testdata_sgb<-as.data.frame(predict(gbm_tree,test_features, type = "response",n.trees = 100))
predictions_testdata_sgb<-cbind(test_features$matchId,predictions_testdata_sgb)
colnames(predictions_testdata_sgb)[colnames(predictions_testdata_sgb)=="test_features$matchId"] <- "matchId"
predictions_testdata_sgb<-merge(matches,predictions_testdata_sgb,by="matchId")
predictions_testdata_sgb<-predictions_testdata_sgb[,-c(2,5,6,7,8,9)]
predictions_testdata_sgb <- predictions_testdata_sgb[which(predictions_testdata_sgb$matchId=="MwLWpcIT" 
                                                           | predictions_testdata_sgb$matchId=="xSlo4KIi"
                                                           | predictions_testdata_sgb$matchId=="25zm2hlc"
                                                           | predictions_testdata_sgb$matchId=="67t20xIG"
                                                           | predictions_testdata_sgb$matchId=="0Uu6adXM"
                                                           | predictions_testdata_sgb$matchId=="Wllk3vYc"
                                                           | predictions_testdata_sgb$matchId=="Qios503o"
                                                           | predictions_testdata_sgb$matchId=="On7btHQp"
                                                           | predictions_testdata_sgb$matchId=="SGsb1I2A"
                                                           | predictions_testdata_sgb$matchId=="Aumg2bm4"), ] 

predictions_testdata_sgb<-predictions_testdata_sgb[,c(1,2,3,5,6,4)]
colnames(predictions_testdata_sgb)<-c("matchId","home","away","Home","Tie","Away")
print("Predictions for round 20 (stochastic gradient boosting)")
predictions_testdata_sgb

# RPS for test data glm

Results<-read.csv("C:/Users/AHMET/Desktop/IE 582/Project/Submission 7 (Round 20)/Results.csv"
                  ,sep = ";")
colnames(Results)<-c("matchId","Result")

predictions_testdata_glm<-merge(Results,predictions_testdata_glm,by="matchId")

rps_test_glm<-as.data.frame(matrix(nrow = nrow(predictions_testdata_glm),ncol = 1))
for (i in 1:nrow(predictions_testdata_glm)){
  if (predictions_testdata_glm[i,2]=="Home") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-1)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }
  else if (predictions_testdata_glm[i,2]=="Tie") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_glm[i,2]=="Away") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-0)^2+0)
  
  }
}
average_rps_test_glm<-mean(rps_test_glm$V1)
print(c("Average RPS for test data (multinomial penalized logistic regression)",average_rps_test_glm))

# RPS for test data sgb

predictions_testdata_sgb<-merge(Results,predictions_testdata_sgb,by="matchId")

rps_test_sgb<-as.data.frame(matrix(nrow = nrow(predictions_testdata_sgb),ncol = 1))
for (i in 1:nrow(predictions_testdata_sgb)){
  if (predictions_testdata_sgb[i,2]=="Home") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-1)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }
  else if (predictions_testdata_sgb[i,2]=="Tie") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_sgb[i,2]=="Away") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-0)^2+0)
  
  }
}
average_rps_test_sgb<-mean(rps_test_sgb$V1)
print(c("Average RPS for testdata (stochastic gradient boosting)",average_rps_test_sgb))

#Calculations for Round 21

matches_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 8 (Round 21)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/AHMET/Desktop/IE 582/Project/Submission 8 (Round 21)/Data/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)
matches[,match_date:=anydate(date)]
matches$Weekday=wday(matches$match_date)
matches$Weekday=as.factor(matches$Weekday)
matches[,Month:=month(match_date)]
matches$Month=as.factor(matches$Month)
matches[,match_date:=NULL]

matches_new<-matches_data_preprocessing(matches)
odds_new<-details_data_preprocessing(odds,matches_new)

testStart=as.Date('2018-12-31')
trainStart=as.Date('2012-07-15')

features<-extract_features.openclose(matches_new,odds_new,trainStart =  trainStart,testStart = testStart)
features$Match_Hour=as.factor(features$Match_Hour)

train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
train_features<-na.omit(train_features) # Removing matches not played
test_features=features[Match_Date>=testStart]

# Multinomial Penalized Logistic Regression
aa<-train_glmnet(train_features = train_features,test_features = test_features)

# RPS for Train Data
bb<-train_glmnet(train_features = train_features,test_features = train_features)
pred_train_glm<-data.frame(bb$predictions)

rps_train_glm<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-1)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_train_glm[i,1]=0.5*((pred_train_glm[i,3]-0)^2+(pred_train_glm[i,3]+pred_train_glm[i,4]-0)^2+0)
  
  }
}

average_rps_train_glm<-mean(rps_train_glm$V1)
print(c("Average RPS for train data (multinomial penalized logistic regression)", average_rps_train_glm))

# Predictions for Round 21

predictions_testdata_glm=data.frame(aa$predictions)
predictions_testdata_glm<-merge(matches,predictions_testdata_glm,by="matchId")
predictions_testdata_glm<-predictions_testdata_glm[,-c(2,5,6,7,8,9,10)]
predictions_testdata_glm <- predictions_testdata_glm[which(predictions_testdata_glm$matchId=="CMONHwmi" 
                                                           | predictions_testdata_glm$matchId=="EXqAbGnT"
                                                           | predictions_testdata_glm$matchId=="tdZj1C33"
                                                           | predictions_testdata_glm$matchId=="KEMxfzep"
                                                           | predictions_testdata_glm$matchId=="b5LtgfAj"
                                                           | predictions_testdata_glm$matchId=="jDNRGc2c"
                                                           | predictions_testdata_glm$matchId=="t2GvEenG"
                                                           | predictions_testdata_glm$matchId=="KhFrDF1M"
                                                           | predictions_testdata_glm$matchId=="nuHzFyX9"
                                                           | predictions_testdata_glm$matchId=="YyDWFHH3"), ] 

print("Predictions for round 21 (multinomial penalized logistic regression)")
predictions_testdata_glm

#Stochastic Gradient Boosting 
library(caret)
require(gbm)
trcontrol<-trainControl(method = "repeatedcv",number=5)
set.seed(5)

gbm_tree<-gbm(Match_Result~.-matchId -leagueId -Match_Date,data = train_features,distribution="multinomial",cv.folds = 10)
print("Relative influences of the features")
summary(gbm_tree)

predictions_traindata<-as.data.frame(predict(gbm_tree,type = "response",n.trees = 100))
rps_traindata<-as.data.frame(matrix(nrow = nrow(train_features),ncol = 1))

for (i in 1:nrow(train_features)){
  if (train_features[i,7]=="Home") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-1)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }
  else if (train_features[i,7]=="Tie") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-1)^2+0)
  }                                                                                           
  else if (train_features[i,7]=="Away") {rps_traindata[i,1]=0.5*((predictions_traindata[i,2]-0)^2+(predictions_traindata[i,2]+predictions_traindata[i,3]-0)^2+0)
  
  }
}

average_rps_train_sgb<-mean(rps_traindata$V1)
print(c("Average RPS for traindata (stochastic gradient boosting)",average_rps_train_sgb))

predictions_testdata_sgb<-as.data.frame(predict(gbm_tree,test_features, type = "response",n.trees = 100))
predictions_testdata_sgb<-cbind(test_features$matchId,predictions_testdata_sgb)
colnames(predictions_testdata_sgb)[colnames(predictions_testdata_sgb)=="test_features$matchId"] <- "matchId"
predictions_testdata_sgb<-merge(matches,predictions_testdata_sgb,by="matchId")
predictions_testdata_sgb<-predictions_testdata_sgb[,-c(2,5,6,7,8,9)]
predictions_testdata_sgb <- predictions_testdata_sgb[which(predictions_testdata_sgb$matchId=="CMONHwmi" 
                                                           | predictions_testdata_sgb$matchId=="EXqAbGnT"
                                                           | predictions_testdata_sgb$matchId=="tdZj1C33"
                                                           | predictions_testdata_sgb$matchId=="KEMxfzep"
                                                           | predictions_testdata_sgb$matchId=="b5LtgfAj"
                                                           | predictions_testdata_sgb$matchId=="jDNRGc2c"
                                                           | predictions_testdata_sgb$matchId=="t2GvEenG"
                                                           | predictions_testdata_sgb$matchId=="KhFrDF1M"
                                                           | predictions_testdata_sgb$matchId=="nuHzFyX9"
                                                           | predictions_testdata_sgb$matchId=="YyDWFHH3"), ] 

predictions_testdata_sgb<-predictions_testdata_sgb[,c(1,2,3,5,6,4)]
colnames(predictions_testdata_sgb)<-c("matchId","home","away","Home","Tie","Away")
print("Predictions for round 21 (stochastic gradient boosting)")
predictions_testdata_sgb

# RPS for test data glm

Results<-read.csv("C:/Users/AHMET/Desktop/IE 582/Project/Submission 8 (Round 21)/Results.csv"
                  ,sep = ";")
colnames(Results)<-c("matchId","Result")

predictions_testdata_glm<-merge(Results,predictions_testdata_glm,by="matchId")

rps_test_glm<-as.data.frame(matrix(nrow = nrow(predictions_testdata_glm),ncol = 1))
for (i in 1:nrow(predictions_testdata_glm)){
  if (predictions_testdata_glm[i,2]=="Home") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-1)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }
  else if (predictions_testdata_glm[i,2]=="Tie") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_glm[i,2]=="Away") {rps_test_glm[i,1]=0.5*((predictions_testdata_glm[i,5]-0)^2+(predictions_testdata_glm[i,5]+predictions_testdata_glm[i,6]-0)^2+0)
  
  }
}
average_rps_test_glm<-mean(rps_test_glm$V1)
print(c("Average RPS for test data (multinomial penalized logistic regression)",average_rps_test_glm))

# RPS for test data sgb

predictions_testdata_sgb<-merge(Results,predictions_testdata_sgb,by="matchId")

rps_test_sgb<-as.data.frame(matrix(nrow = nrow(predictions_testdata_sgb),ncol = 1))
for (i in 1:nrow(predictions_testdata_sgb)){
  if (predictions_testdata_sgb[i,2]=="Home") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-1)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }
  else if (predictions_testdata_sgb[i,2]=="Tie") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-1)^2+0)
  }                                                                                           
  else if (predictions_testdata_sgb[i,2]=="Away") {rps_test_sgb[i,1]=0.5*((predictions_testdata_sgb[i,5]-0)^2+(predictions_testdata_sgb[i,5]+predictions_testdata_sgb[i,6]-0)^2+0)
  
  }
}
average_rps_test_sgb<-mean(rps_test_sgb$V1)
print(c("Average RPS for testdata (stochastic gradient boosting)",average_rps_test_sgb))








