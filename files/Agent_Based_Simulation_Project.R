
### Sensitivity for Uncertain Parameters ###

# Analysis for Initial_Skill
initial_skill<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 2 - Initial_skill Analysis-table.csv",sep = ";")
initial_skill<-initial_skill[,-c(2:11)]
initial_skill_10<-initial_skill[1:100,]
initial_skill_30<-initial_skill[101:200,]
initial_skill_50<-initial_skill[201:300,]
initial_skill_70<-initial_skill[301:400,]
# Results Matrix for Initial Skill
results_initial_skill<-data.frame(matrix(nrow = 4,ncol = 4))
colnames(results_initial_skill)<-c("Always-Helps","Never Helps","Shy-Never-Helps","Tricky-Sometimes")
for (i in 1:4){
  results_initial_skill[1,i]<-mean(initial_skill_10[,i+1])
  results_initial_skill[2,i]<-mean(initial_skill_30[,i+1])
  results_initial_skill[3,i]<-mean(initial_skill_50[,i+1])
  results_initial_skill[4,i]<-mean(initial_skill_70[,i+1])
}
results_initial_skill$Initial_Skill<-c(10,30,50,70)
results_initial_skill<-results_initial_skill[,c(5,1,2,3,4)]

print("Analysis for Different Initial Skill Values")
results_initial_skill

# Analysis for High Difference
high_difference<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 3 - High_difference Analysis-table.csv",sep = ";")
high_difference<-high_difference[,-c(2:11)]
high_difference_5<-high_difference[1:100,]
high_difference_10<-high_difference[101:200,]
high_difference_20<-high_difference[201:300,]
high_difference_30<-high_difference[301:400,]
# Results Matrix for High Difference
results_high_difference<-data.frame(matrix(nrow = 4,ncol = 4))
colnames(results_high_difference)<-c("Always-Helps","Never Helps","Shy-Never-Helps","Tricky-Sometimes")
for (i in 1:4){
  results_high_difference[1,i]<-mean(high_difference_5[,i+1])
  results_high_difference[2,i]<-mean(high_difference_10[,i+1])
  results_high_difference[3,i]<-mean(high_difference_20[,i+1])
  results_high_difference[4,i]<-mean(high_difference_30[,i+1])
}
results_high_difference$High_Difference<-c(5,10,20,30)
results_high_difference<-results_high_difference[,c(5,1,2,3,4)]

print("Analysis for Different High Difference Values")
results_high_difference

# Analysis for Remember Rate
remember_rate<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 4 - Remember_rate Analysis-table.csv",sep = ";")
remember_rate<-remember_rate[,-c(2:11)]
remember_rate_0.980<-remember_rate[1:100,]
remember_rate_0.985<-remember_rate[101:200,]
remember_rate_0.990<-remember_rate[201:300,]
remember_rate_0.995<-remember_rate[301:400,]
# Results Matrix for High Difference
results_remember_rate<-data.frame(matrix(nrow = 4,ncol = 4))
colnames(results_remember_rate)<-c("Always-Helps","Never Helps","Shy-Never-Helps","Tricky-Sometimes")
for (i in 1:4){
  results_remember_rate[1,i]<-mean(remember_rate_0.980[,i+1])
  results_remember_rate[2,i]<-mean(remember_rate_0.985[,i+1])
  results_remember_rate[3,i]<-mean(remember_rate_0.990[,i+1])
  results_remember_rate[4,i]<-mean(remember_rate_0.995[,i+1])
}
results_remember_rate$Remember_Rate<-c(0.980,0.985,0.990,0.995)
results_remember_rate<-results_remember_rate[,c(5,1,2,3,4)]

print("Analysis for Different Remember Rate Values")
results_remember_rate


### Validation Analysis ###

# Analysis for Friendly Class (Friendship level 1:5) (ah=10,nh=5,snh=5,tsh=5)
friendly_1<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 5 - Analysis for Validation (Friendly Class)-table.csv",sep = ";")
friendly_1<-friendly_1[,c(1,12)]

results_friendly<-data.frame(matrix(nrow = 1,ncol = 5))
for (i in 1:5){
  results_friendly[1,i]<-mean(friendly_1[(100*(i-1)+1):(100*i),2])
}
# Analysis for Shy Class (Friendship Level 1:5) (ah=5,nh=5,snh=10,tsh=5)
shy_nevers_1<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 6 - Analysis for Validation (Shy Class)-table.csv",sep = ";")
shy_nevers_1<-shy_nevers_1[,c(1,12)]
results_shy_nevers<-data.frame(matrix(nrow = 1,ncol = 5))
for (i in 1:5){
  results_shy_nevers[1,i]<-mean(shy_nevers_1[(100*(i-1)+1):(100*i),2])
}
# Analysis for Tricky Class (Friendship Level 1:5) (ah=5,nh=5,snh=5,tsh=10)
tricky_1<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 7 - Analysis for Validation (Tricky Class)-table.csv",sep = ";")
tricky_1<-tricky_1[,c(1,12)]
results_tricky<-data.frame(matrix(nrow = 1,ncol = 5))
for (i in 1:5){
  results_tricky[1,i]<-mean(tricky_1[(100*(i-1)+1):(100*i),2])
}
# Analysis for Unfriendly Class (Friendship Level 1:5) (ah=5,nh=10,snh=5,tsh=5)
unfriendly_1<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 8 - Analysis for Validation (Unfriendly Class)-table.csv",sep = ";")
unfriendly_1<-unfriendly_1[,c(1,12)]
results_unfriendly<-data.frame(matrix(nrow = 1,ncol = 5))
for (i in 1:5){
  results_unfriendly[1,i]<-mean(unfriendly_1[(100*(i-1)+1):(100*i),2])
}
results_friendship<-rbind(results_friendly,results_tricky,results_unfriendly,results_shy_nevers)
results_friendship$Class_Type<-c("Friendly","Tricky","Unfriendly","Shy-Nevers")
colnames(results_friendship)<-c("f_l=1","f_l=2","f_l=3","f_l=4","f_l=5","Class_Type")


print("Analysis for Different type of Classes (Class Average Skills)")
results_friendship[,-5]


### RESULTS ###

# 500 Run Results for Default Parameters (ah=5,nh=10,snh=5,tsh=5, high_dif=10, rem_rate=0.995, fr_level=2, initial_skill=50)
default_500<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 1- Default Parameters-table.csv",sep = ";")
default_500<-default_500[,-c(2:11)]

print("Scaled Skills of Student Types")
par(mfrow=c(2,2))
hist_ah<-hist(default_500$mean...skill..of.always.helps.,main = "Always-Helps",xlab = "Mean Scaled Skill")
hist_nh<-hist(default_500$mean...skill..of.never.helps.,main = "Never-Helps",xlab = "Mean Scaled Skill")
hist_snh<-hist(default_500$mean...skill..of.shy.nevers.,main = "Shy-Never-Helps",xlab = "Mean Scaled Skill")
hist_tsh<-hist(default_500$mean...skill..of.tricky.sometimes.helps.,main = "Tricky-Sometimes-Helps",xlab = "Mean Scaled Skill")


ah<-data.frame(default_500[order(default_500$mean...skill..of.always.helps....mean...skill..of.turtles.),2])
nh<-data.frame(default_500[order(default_500$mean...skill..of.never.helps....mean...skill..of.turtles.),3])
snh<-data.frame(default_500[order(default_500$mean...skill..of.shy.nevers....mean...skill..of.turtles.),4])
tsh<-data.frame(default_500[order(default_500$mean...skill..of.tricky.sometimes.helps....mean...skill..of.turtles.),5])

print("Mean Scaled Skills of Different Type of Students
      (From Minimum to Maximum for each Student Type")
par(mfrow=c(1,1))
plot(ah$default_500.order.default_500.mean...skill..of.always.helps....mean...skill..of.turtles....,main ="Mean Scaled Skills of Different Type of Students
      (From Minimum to Maximum for each Student Type)",type = "l",col="blue",ylab = "Mean Scaled Skill",xlab = "Run Number",ylim = c(0.65,1.35))
par(new=TRUE)
plot(nh$default_500.order.default_500.mean...skill..of.never.helps....mean...skill..of.turtles....,ylim = c(0.65,1.35),col="red",type = "l",ylab = "Mean Scaled Skill",xlab = "Run Number")
par(new=TRUE)
plot(snh$default_500.order.default_500.mean...skill..of.shy.nevers....mean...skill..of.turtles....,ylim = c(0.65,1.35),col="orange",type="l",ylab = "Mean Scaled Skill",xlab = "Run Number")
par(new=TRUE)
plot(tsh$default_500.order.default_500.mean...skill..of.tricky.sometimes.helps....mean...skill..of.turtles....,type="l",ylim = c(0.65,1.35),col="green",ylab = "Mean Scaled Skill",xlab = "Run Number")


# T-test Between Always-Helps and Tricky-Sometimes-Helps
print("T-test Between Always-Helps and Tricky-Sometimes-Helps")
t.test(default_500$mean...skill..of.always.helps....mean...skill..of.turtles. ,
       default_500$mean...skill..of.tricky.sometimes.helps....mean...skill..of.turtles.,
       alternative = "greater")

# T-test Between Tricky-Sometimes-Helps and Never Helps
print("T-test Between Tricky-Sometimes-Helps and Never Helps")
t.test(default_500$mean...skill..of.tricky.sometimes.helps....mean...skill..of.turtles.,
       default_500$mean...skill..of.never.helps....mean...skill..of.turtles.,
       alternative = "greater")

# T-test Between Never Helps and Shy-Never Helps
print("T-test Between Never Helps and Shy-Never Helps")
t.test(default_500$mean...skill..of.never.helps....mean...skill..of.turtles.,
       default_500$mean...skill..of.shy.nevers....mean...skill..of.turtles.,
       alternative = "greater")


## Global Sensitivity Analysis ##

gsa_homogenous<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 9 - GSA -Homogenous_fl_1-table.csv",sep = ";")
gsa_friendly<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 10 - GSA -Friendly_fl_1-table.csv",sep = ";")
gsa_unfriendly<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 11 - GSA -Unfriendly_fl_1-table.csv",sep = ";")
gsa_shy<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 12 - GSA -Shy_fl_1-table.csv",sep = ";")
gsa_tricky<-read.csv("C:/Users/AHMET/Desktop/IE 588/Project/Results for RStudio/Project 13 - GSA -Tricky_fl_1-table.csv",sep = ";")

extraction<-function(data){     # A particular function for extraction
  data<-data[,-c(2:11)]
  gsa_vector<-data.frame(matrix(nrow = 3,ncol = 4))
  for(i in 1:3){
    gsa_vector[i,1]<-mean(data[c((((i-1)*100)+1):(100*i)),2])
    gsa_vector[i,2]<-mean(data[c((((i-1)*100)+1):(100*i)),3])
    gsa_vector[i,3]<-mean(data[c((((i-1)*100)+1):(100*i)),4])
    gsa_vector[i,4]<-mean(data[c((((i-1)*100)+1):(100*i)),5])
  }
  colnames(gsa_vector)<-c("Always-Helps","Never-Helps","Shy-Never-Helps","Tricky-St-Helps")
  gsa_vector$Friendship_Level<-c(1,2,3)
  gsa_vector<-gsa_vector[c(5,1,2,3,4)]
  return(gsa_vector)
}

vector_homogenous<-extraction(gsa_homogenous)
vector_homogenous$Class_Type<-"Homogenous"
vector_friendly<-extraction(gsa_friendly)
vector_friendly$Class_Type<-"Friendly"
vector_unfriendly<-extraction(gsa_unfriendly)
vector_unfriendly$Class_Type<-"Unfriendly"
vector_shy<-extraction(gsa_shy)
vector_shy$Class_Type<-"Shy"
vector_tricky<-extraction(gsa_tricky)
vector_tricky$Class_Type<-"Tricky"


GSA_RESULTS<-rbind(vector_homogenous,vector_friendly,vector_unfriendly,vector_shy,vector_tricky)
GSA_RESULTS<-GSA_RESULTS[,c(6,1,2,3,4,5)]
GSA_RESULTS<-GSA_RESULTS[order(GSA_RESULTS$Friendship_Level),]
GSA_RESULTS[,c(3,4,5,6)]<-round(GSA_RESULTS[,c(3,4,5,6)],digits = 4)

GSA_RESULTS

