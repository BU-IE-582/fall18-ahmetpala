# Reading the dataset
data<-read.csv("C:/Users/AHMET/Desktop/IE 582/Homework 5/Data Files/Musk1.csv")

# Defining column names and splitting the dataset
bag_info<-data[,1:2]
colnames(bag_info)<-c("Bag_Class","Bag_Id")

features<-data[,-c(1,2)] # Deifining features in a particular dataset

data<-cbind(bag_info,features) # Defining the original data with the column names
features<-data.frame(scale(features)) # Scaling the features before calculating distances

require(cluster)
GetBagLevelInformation<-function(features,bag_info,numberofClusters){
  set.seed(5) 
  ## STEP 1 ##
  # K-MEDOIDS
  # k-medoid for euclidean distance
  k_medoids_euclidean<-pam(features, numberofClusters, metric = "euclidean")
  instance_labels_kmed_euc<-data.frame(k_medoids_euclidean$clustering)
  
  labeled_instances_km_euc<-cbind(instance_labels_kmed_euc,features)
  
  # k-medoid for manhattan distance
  k_medoids_manhattan<-pam(features, numberofClusters, metric = "manhattan")
  instance_labels_kmed_man<-data.frame(k_medoids_manhattan$clustering)
  
  labeled_instances_km_man<-cbind(instance_labels_kmed_man,features)
  
  # HIERARCHICAL CLUSTERING
  euclidean_dist<-dist(features,method = "euclidean") # Calculting the distance based on two different measures
  manhattan_dist<-dist(features,method = "manhattan")
  
  # Hierarchical clustering for Euclidean Distance
  clusters_euc<-hclust(euclidean_dist)
  
  instance_labels_hc_euc <- data.frame(cutree(clusters_euc, numberofClusters))
  
  labeled_instances_hc_euc<-cbind(instance_labels_hc_euc,features)
  
  # Hierarchical clustering for Manhattan Distance
  clusters_man<-hclust(manhattan_dist)
  
  instance_labels_hc_man <- data.frame(cutree(clusters_man, numberofClusters))
  
  labeled_instances_hc_man<-cbind(instance_labels_hc_man,features)
  
  ## STEP 2 ##
  # Calculating Centroids
  # Centroids for k-medoids (Euclidean Distance)
  centroids_km_Euc<-data.frame(matrix(nrow=numberofClusters,ncol = 167))
  for(i in 1:numberofClusters){
    data1_1<-labeled_instances_km_euc[labeled_instances_km_euc[,1] ==i,]
    centroids_km_Euc[i,]<-data.frame(t(colMeans(data1_1)))
  }
  # Centroids for k-medoids (Manhattan Distance)
  centroids_km_Man<-data.frame(matrix(nrow=numberofClusters,ncol = 167))
  for(i in 1:numberofClusters){
    data2_1<-labeled_instances_km_man[labeled_instances_km_man[,1] ==i,]
    centroids_km_Man[i,]<-data.frame(t(colMeans(data2_1)))
  }
  # Centroids for hierarchial clustering (Euclidean Distance)
  centroids_hc_Euc<-data.frame(matrix(nrow=numberofClusters,ncol = 167))
  for(i in 1:numberofClusters){
    data3_1<-labeled_instances_hc_euc[labeled_instances_hc_euc[,1] ==i,]
    centroids_hc_Euc[i,]<-data.frame(t(colMeans(data3_1)))
  }
  # Centroids for hierarchial clustering (Manhattan Distance)
  centroids_hc_Man<-data.frame(matrix(nrow=numberofClusters,ncol = 167))
  for(i in 1:numberofClusters){
    data4_1<-labeled_instances_hc_man[labeled_instances_hc_man[,1] ==i,]
    centroids_hc_Man[i,]<-data.frame(t(colMeans(data4_1)))
  }
  # Calculating Distances to the centroids
  # k-medoids - Euclidean Distance
  colnames(labeled_instances_km_euc)<-colnames(centroids_km_Euc)
  km_euc_new<-rbind(labeled_instances_km_euc[,-1],centroids_km_Euc[,-1])
  a1<-data.frame(as.matrix(dist(km_euc_new,method = "euclidean")))
  dist_km_euc<-a1[-c(476:(475+numberofClusters)),c(476:(475+numberofClusters))]
  bags_km_euc<-cbind(bag_info,dist_km_euc)
  
  # k-medoids - Manhattan Distance
  colnames(labeled_instances_km_man)<-colnames(centroids_km_Man)
  km_man_new<-rbind(labeled_instances_km_man[,-1],centroids_km_Man[,-1])
  a2<-data.frame(as.matrix(dist(km_man_new,method = "manhattan")))
  dist_km_man<-a2[-c(476:(475+numberofClusters)),c(476:(475+numberofClusters))]
  bags_km_man<-cbind(bag_info,dist_km_man)
  
  # Hierarchical Clustering - Euclidean Distance
  colnames(labeled_instances_hc_euc)<-colnames(centroids_hc_Euc)
  hc_euc_new<-rbind(labeled_instances_hc_euc[,-1],centroids_hc_Euc[,-1])
  a3<-data.frame(as.matrix(dist(hc_euc_new,method = "euclidean")))
  dist_hc_euc<-a3[-c(476:(475+numberofClusters)),c(476:(475+numberofClusters))]
  bags_hc_euc<-cbind(bag_info,dist_hc_euc)
  
  # Hierarchical Clustering - Manhattan Distance
  colnames(labeled_instances_hc_man)<-colnames(centroids_hc_Man)
  hc_man_new<-rbind(labeled_instances_hc_man[,-1],centroids_hc_Man[,-1])
  a4<-data.frame(as.matrix(dist(hc_man_new,method = "manhattan")))
  dist_hc_man<-a4[-c(476:(475+numberofClusters)),c(476:(475+numberofClusters))]
  bags_hc_man<-cbind(bag_info,dist_hc_man)
  
  ## STEP 3 ## Representing the bags as the average of their instance distances to the cluster centroids
    # k-medoids (Euclidean Distance)
  km_euc_last<-data.frame(matrix(nrow = 92,ncol = 2+numberofClusters))
  
  for (i in 1:92){
    km_euc_last[i,2]<-i
    km_euc_last[i,1]<-mean(bag_info[bag_info$Bag_Id==i,1])
    for(j in 1:numberofClusters){
      km_euc_last[i,2+j]<-mean(bags_km_euc[bags_km_euc$Bag_Id==i,2+j])
    }
  }
  
  # k-medoids (Manhattan Distance)
  km_man_last<-data.frame(matrix(nrow = 92,ncol = 2+numberofClusters))
  
  for (i in 1:92){
    km_man_last[i,2]<-i
    km_man_last[i,1]<-mean(bag_info[bag_info$Bag_Id==i,1])
    for(j in 1:numberofClusters){
      km_man_last[i,2+j]<-mean(bags_km_man[bags_km_man$Bag_Id==i,2+j])
    }
  }
  
  # Hierarchical Clustering (Euclidean Distance)
  hc_euc_last<-data.frame(matrix(nrow = 92,ncol = 2+numberofClusters))
  
  for (i in 1:92){
    hc_euc_last[i,2]<-i
    hc_euc_last[i,1]<-mean(bag_info[bag_info$Bag_Id==i,1])
    for(j in 1:numberofClusters){
      hc_euc_last[i,2+j]<-mean(bags_hc_euc[bags_hc_euc$Bag_Id==i,2+j])
    }
  }
  
  # Hierarchical Clustering (Manhattan Distance)
  hc_man_last<-data.frame(matrix(nrow = 92,ncol = 2+numberofClusters))
  
  for (i in 1:92){
    hc_man_last[i,2]<-i
    hc_man_last[i,1]<-mean(bag_info[bag_info$Bag_Id==i,1])
    for(j in 1:numberofClusters){
      hc_man_last[i,2+j]<-mean(bags_hc_man[bags_hc_man$Bag_Id==i,2+j])
    }
  }
  
  ## Penalized Logistic Regression ##
  require(glmnet)
  results<-data.frame(matrix(nrow=4,ncol=5))
  colnames(results)<-c("nofClusters","ClusteringMethod","DistanceMethod","mincvm","minlambda")
  
  # k-medoids (Euclidean Distance)
  cv_km_euc <- cv.glmnet(x = as.matrix(km_euc_last[,-c(1,2)]), y =km_euc_last[,1] , 
                         nfolds = 9, type.measure = "auc", family = "binomial")
  results[1,]=c(numberofClusters,"k-medoids","Euclidean",min(cv_km_euc$cvm),cv_km_euc$lambda.min)
  
  # k-medoids (Manhattan Distance)
  cv_km_man <- cv.glmnet(x = as.matrix(km_man_last[,-c(1,2)]), y =km_man_last[,1] , 
                         nfolds = 9, type.measure = "auc", family = "binomial")
  results[2,]=c(numberofClusters,"k-medoids","Manhattan",min(cv_km_man$cvm),cv_km_man$lambda.min)
  
  #Hierarchical Clustering (Euclidean Distance)
  cv_hc_euc <- cv.glmnet(x = as.matrix(hc_euc_last[,-c(1,2)]), y =hc_euc_last[,1] , 
                         nfolds = 9, type.measure = "auc", family = "binomial")
  results[3,]=c(numberofClusters,"HierarchicalClust","Euclidean",min(cv_hc_euc$cvm),cv_hc_euc$lambda.min)
  
  #Hierarchical Clustering (Manhattan Distance)
  cv_hc_man <- cv.glmnet(x = as.matrix(hc_man_last[,-c(1,2)]), y =hc_man_last[,1] , 
                         nfolds = 9, type.measure = "auc", family = "binomial")
  results[4,]=c(numberofClusters,"HierarchicalClust","Manhattan",min(cv_hc_man$cvm),cv_hc_man$lambda.min)
  
  return(results)
}


Results<-data.frame(matrix(ncol=5,nrow=0))
colnames(Results)<-c("nofClust","ClusterMethod","DistanceMethod","mincvm","minlambda")

for (i in 2:30) {
  Output<-GetBagLevelInformation(numberofClusters = i,features = features,bag_info = bag_info) 
  Results<-rbind(Results,Output)
}

Results$mincvm<-as.numeric(Results$mincvm)
Results$minlambda<-as.numeric(Results$minlambda)
Results$mincvm<-round(Results$mincvm,4)
Results$minlambda<-round(Results$minlambda,4)
Results

Results[which.min(Results$mincvm),] # Finding the best parameters which give the minimum cross-validated error




  
  