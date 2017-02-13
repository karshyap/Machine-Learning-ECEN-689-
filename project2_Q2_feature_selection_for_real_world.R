rm(list=ls())

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}

library(MASS)
library(ggplot2)
library(class)


setwd("/home/kashyap/Desktop/TAMU_first_semester/MTLS_informatics/Project/Project_2")
SFE_train_dataset=read.csv("SFE_train_dataset.csv") #reading the csv file
SFE_test_dataset=read.csv("SFE_test_dataset.csv") #reading the csv file


SFE_train_dataset_original=SFE_train_dataset

##Arranging in alphabetical(dictionary order)
SFE_train_dataset=SFE_train_dataset[c("C","Cr","Fe","Mn","N","Ni","Si","SFE")]
SFE_test_dataset=SFE_test_dataset[c("C","Cr","Fe","Mn","N","Ni","Si","SFE")]



SFE_train_dataset$SFE=factor(SFE_train_dataset$SFE)
SFE_test_dataset$SFE=factor(SFE_test_dataset$SFE)

ftrs=c(1,2,3,4,5,6,7)
lda_appr_exh_err_vec=numeric(5)
knn_appr_exh_err_vec=numeric(5)

lda_appr_sfs_err_vec=numeric(5)
knn_appr_sfs_err_vec=numeric(5)
####Exhaustive Feature Set#####

combns_of_features_5=t(combn(ftrs,5))
combns_of_features_4=t(combn(ftrs,4))
combns_of_features_3=t(combn(ftrs,3))
combns_of_features_2=t(combn(ftrs,2))
combns_of_features_1=t(combn(ftrs,1))

app_err_lda_1=numeric(length(combns_of_features_1[,1]))
app_err_lda_2=numeric(length(combns_of_features_2[,1]))
app_err_lda_3=numeric(length(combns_of_features_3[,1]))
app_err_lda_4=numeric(length(combns_of_features_4[,1]))
app_err_lda_5=numeric(length(combns_of_features_5[,1]))

app_err_knn_1=numeric(length(combns_of_features_1[,1]))
app_err_knn_2=numeric(length(combns_of_features_2[,1]))
app_err_knn_3=numeric(length(combns_of_features_3[,1]))
app_err_knn_4=numeric(length(combns_of_features_4[,1]))
app_err_knn_5=numeric(length(combns_of_features_5[,1]))

lda_exh_ftr_vec=numeric(5)
knn_exh_ftr_vec=numeric(5)



####for 1 feature####

for(i in 1:length(combns_of_features_1[,1])){
  trn_set=SFE_train_dataset[combns_of_features_1[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set$SFE=factor(trn_set$SFE)
  trn_set=data.frame(trn_set)
  training_labels=trn_set$SFE
  lda_trained<-lda(SFE ~ .,data=trn_set,prior=c(0.5,0.5))
  new_data=data.frame(trn_set[,1])
  colnames(new_data)=colnames(trn_set)[1]
  predictions_lda=predict(lda_trained,newdata = new_data)$class
  conf_matrix_lda=table(predictions_lda,trn_set[,2])
  #cat(conf_matrix_lda);cat("\n")
  lda_error=(conf_matrix_lda[2]+conf_matrix_lda[3])/sum(conf_matrix_lda)
  predictions_knn<-knn(train = new_data, test = new_data, cl = training_labels, k=3)
  conf_matrix_knn=table(predictions_knn,trn_set[,2])
  knn_error=(conf_matrix_knn[2]+conf_matrix_knn[3])/sum(conf_matrix_knn)
  
  
  app_err_lda_1[i]=lda_error
  app_err_knn_1[i]=knn_error
  
}

lda_exh_ftr_vec[1]=match(min(app_err_lda_1),app_err_lda_1)
knn_exh_ftr_vec[1]=match(min(app_err_knn_1),app_err_knn_1)



lda_appr_exh_err_vec[1]=min(app_err_lda_1)
knn_appr_exh_err_vec[1]=min(app_err_knn_1)


lda_true_exh_err_vec=numeric(5)
knn_true_exh_err_vec=numeric(5)

####1 feature exh test set verification####
exh_lda_min_1=match(min(app_err_lda_1),app_err_lda_1)
exh_knn_min_1=match(min(app_err_lda_1),app_err_lda_1)
trn_set_exh_1=SFE_train_dataset[exh_lda_min_1]
trn_set_exh_1$SFE=SFE_train_dataset$SFE
test_set_exh_1=SFE_test_dataset[exh_lda_min_1]
test_set_exh_1$SFE=SFE_test_dataset$SFE

lda_trained_exh_1<-lda(SFE ~ .,data=trn_set_exh_1,prior=c(0.5,0.5))

new_data_exh_1=data.frame(test_set_exh_1[,1])
colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]

predictions_lda_exh_1=predict(lda_trained_exh_1,newdata = new_data_exh_1)$class
conf_matrix_lda_exh_1=table(predictions_lda_exh_1,test_set_exh_1[,2])
lda_true_exh_err_vec[1]=(conf_matrix_lda_exh_1[2]+conf_matrix_lda_exh_1[3])/sum(conf_matrix_lda_exh_1)

training_labels_exh_1=trn_set_exh_1$SFE

predictions_knn_exh_1<-knn(train = trn_set_exh_1[,1], test = new_data_exh_1, cl = training_labels_exh_1, k=3)

conf_matrix_knn_exh_1=table(predictions_knn_exh_1,test_set_exh_1[,2])
knn_true_exh_err_vec[1]=(conf_matrix_knn_exh_1[2]+conf_matrix_knn_exh_1[3])/sum(conf_matrix_knn_exh_1)





####for 2 features####

for(i in 1:length(combns_of_features_2[,1])){
  trn_set=SFE_train_dataset[combns_of_features_2[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set$SFE=factor(trn_set$SFE)
  training_labels=trn_set$SFE
  lda_trained<-lda(SFE ~ .,data=trn_set,prior=c(0.5,0.5))
  predictions_lda=predict(lda_trained,trn_set[,c(1,2)])$class
  conf_matrix_lda=table(predictions_lda,trn_set[,3])
  #cat(conf_matrix_lda);cat("\n")
  lda_error=(conf_matrix_lda[2]+conf_matrix_lda[3])/sum(conf_matrix_lda)
  predictions_knn<-knn(train = trn_set[,1:2], test = trn_set[,1:2], cl = training_labels, k=3)
  conf_matrix_knn=table(predictions_knn,trn_set[,3])
  knn_error=(conf_matrix_knn[2]+conf_matrix_knn[3])/sum(conf_matrix_knn)
  
  
  app_err_lda_2[i]=lda_error
  app_err_knn_2[i]=knn_error
  
}
print(app_err_knn_2)

lda_appr_exh_err_vec[2]=min(app_err_lda_2)
knn_appr_exh_err_vec[2]=min(app_err_knn_2)

lda_exh_ftr_vec[2]=match(min(app_err_lda_2),app_err_lda_2)
knn_exh_ftr_vec[2]=match(min(app_err_knn_2),app_err_knn_2)


####2 feature exh test set verification####
exh_lda_min_2=match(min(app_err_lda_2),app_err_lda_2)
exh_knn_min_2=match(min(app_err_lda_2),app_err_lda_2)
trn_set_exh_2=SFE_train_dataset[combns_of_features_2[exh_lda_min_2,]]
trn_set_exh_2$SFE=SFE_train_dataset$SFE
test_set_exh_2=SFE_test_dataset[combns_of_features_2[exh_lda_min_2,]]
test_set_exh_2$SFE=SFE_test_dataset$SFE


lda_trained_exh_2<-lda(SFE ~ .,data=trn_set_exh_2,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]

predictions_lda_exh_2=predict(lda_trained_exh_2, test_set_exh_2[,c(1,2)])$class
conf_matrix_lda_exh_2=table(predictions_lda_exh_2,test_set_exh_2[,3])
lda_true_exh_err_vec[2]=(conf_matrix_lda_exh_2[2]+conf_matrix_lda_exh_2[3])/sum(conf_matrix_lda_exh_2)

training_labels_exh_2=trn_set_exh_2$SFE

predictions_knn_exh_2<-knn(train = trn_set_exh_2[,c(1,2)], test = test_set_exh_2[,c(1,2)] , cl = training_labels_exh_2, k=3)

conf_matrix_knn_exh_2=table(predictions_knn_exh_2,test_set_exh_2[,3])
knn_true_exh_err_vec[2]=(conf_matrix_knn_exh_2[2]+conf_matrix_knn_exh_2[3])/sum(conf_matrix_knn_exh_2)


####For 3 features#####
for(i in 1:length(combns_of_features_3[,1])){
  trn_set=SFE_train_dataset[combns_of_features_3[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set$SFE=factor(trn_set$SFE)
  training_labels=trn_set$SFE
  lda_trained<-lda(SFE ~ .,data=trn_set,prior=c(0.5,0.5))
  predictions_lda=predict(lda_trained,trn_set[,c(1,2,3)])$class
  conf_matrix_lda=table(predictions_lda,trn_set[,4])
  ##cat(conf_matrix_lda);cat("\n")
  #cat("hello")
  lda_error=(conf_matrix_lda[2]+conf_matrix_lda[3])/sum(conf_matrix_lda)
  predictions_knn<-knn(train = trn_set[,1:3], test = trn_set[,1:3], cl = training_labels, k=3)
  conf_matrix_knn=table(predictions_knn,trn_set[,4])
  knn_error=(conf_matrix_knn[2]+conf_matrix_knn[3])/sum(conf_matrix_knn)
  
  
  app_err_lda_3[i]=lda_error
  app_err_knn_3[i]=knn_error
  
}

print(app_err_lda_3)

lda_appr_exh_err_vec[3]=min(app_err_lda_3)
knn_appr_exh_err_vec[3]=min(app_err_knn_3)

lda_exh_ftr_vec[3]=match(min(app_err_lda_3),app_err_lda_3)
knn_exh_ftr_vec[3]=match(min(app_err_knn_3),app_err_knn_3)

####3 feature exh test set verification####
exh_lda_min_3=match(min(app_err_lda_3),app_err_lda_3)
exh_knn_min_3=match(min(app_err_lda_3),app_err_lda_3)
trn_set_exh_3=SFE_train_dataset[combns_of_features_3[exh_lda_min_3,]]
trn_set_exh_3$SFE=SFE_train_dataset$SFE
test_set_exh_3=SFE_test_dataset[combns_of_features_3[exh_lda_min_3,]]
test_set_exh_3$SFE=SFE_test_dataset$SFE


lda_trained_exh_3<-lda(SFE ~ .,data=trn_set_exh_3,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]
cat("hello-gund")

predictions_lda_exh_3=predict(lda_trained_exh_3, test_set_exh_3[,c(1,2,3)])$class
conf_matrix_lda_exh_3=table(predictions_lda_exh_3,test_set_exh_3[,4])
lda_true_exh_err_vec[3]=(conf_matrix_lda_exh_3[2]+conf_matrix_lda_exh_3[3])/sum(conf_matrix_lda_exh_3)

training_labels_exh_3=trn_set_exh_3$SFE

predictions_knn_exh_3<-knn(train = trn_set_exh_3[,c(1,2,3)], test = test_set_exh_3[,c(1,2,3)] , cl = training_labels_exh_3, k=3)

conf_matrix_knn_exh_3=table(predictions_knn_exh_3,test_set_exh_3[,4])
knn_true_exh_err_vec[3]=(conf_matrix_knn_exh_3[2]+conf_matrix_knn_exh_3[3])/sum(conf_matrix_knn_exh_3)


####For 4 features####
for(i in 1:length(combns_of_features_4[,1])){
  trn_set=SFE_train_dataset[combns_of_features_4[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set$SFE=factor(trn_set$SFE)
  training_labels=trn_set$SFE
  lda_trained<-lda(SFE ~ .,data=trn_set,prior=c(0.5,0.5))
  predictions_lda=predict(lda_trained,trn_set[,c(1,2,3,4)])$class
  conf_matrix_lda=table(predictions_lda,trn_set[,5])
  ##cat(conf_matrix_lda);cat("\n")
 # cat("hello")
  lda_error=(conf_matrix_lda[2]+conf_matrix_lda[3])/sum(conf_matrix_lda)
  predictions_knn<-knn(train = trn_set[,1:4], test = trn_set[,1:4], cl = training_labels, k=3)
  conf_matrix_knn=table(predictions_knn,trn_set[,5])
  knn_error=(conf_matrix_knn[2]+conf_matrix_knn[3])/sum(conf_matrix_knn)
  
  
  app_err_lda_4[i]=lda_error
  app_err_knn_4[i]=knn_error
  
}

lda_appr_exh_err_vec[4]=min(app_err_lda_4)
knn_appr_exh_err_vec[4]=min(app_err_knn_4)

lda_exh_ftr_vec[4]=match(min(app_err_lda_4),app_err_lda_4)
knn_exh_ftr_vec[4]=match(min(app_err_knn_4),app_err_knn_4)

#### 4 feature exh test set verification####
exh_lda_min_4=match(min(app_err_lda_4),app_err_lda_4)
exh_knn_min_4=match(min(app_err_lda_4),app_err_lda_4)
trn_set_exh_4=SFE_train_dataset[combns_of_features_4[exh_lda_min_4,]]
trn_set_exh_4$SFE=SFE_train_dataset$SFE
test_set_exh_4=SFE_test_dataset[combns_of_features_4[exh_lda_min_4,]]
test_set_exh_4$SFE=SFE_test_dataset$SFE


lda_trained_exh_4<-lda(SFE ~ .,data=trn_set_exh_4,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]
cat("hello-gund")

predictions_lda_exh_4=predict(lda_trained_exh_4, test_set_exh_4[,c(1,2,3,4)])$class
conf_matrix_lda_exh_4=table(predictions_lda_exh_4,test_set_exh_4[,5])
lda_true_exh_err_vec[4]=(conf_matrix_lda_exh_4[2]+conf_matrix_lda_exh_4[3])/sum(conf_matrix_lda_exh_4)

training_labels_exh_4=trn_set_exh_4$SFE

predictions_knn_exh_4<-knn(train = trn_set_exh_4[,c(1,2,3,4)], test = test_set_exh_4[,c(1,2,3,4)] , cl = training_labels_exh_4, k=3)

conf_matrix_knn_exh_4=table(predictions_knn_exh_4,test_set_exh_4[,5])
knn_true_exh_err_vec[4]=(conf_matrix_knn_exh_4[2]+conf_matrix_knn_exh_4[3])/sum(conf_matrix_knn_exh_4)


####For 5 features####
for(i in 1:length(combns_of_features_5[,1])){
  trn_set=SFE_train_dataset[combns_of_features_5[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set$SFE=factor(trn_set$SFE)
  training_labels=trn_set$SFE
  lda_trained<-lda(SFE ~ .,data=trn_set,prior=c(0.5,0.5))
  predictions_lda=predict(lda_trained,trn_set[,c(1,2,3,4,5)])$class
  conf_matrix_lda=table(predictions_lda,trn_set[,6])
  ##cat(conf_matrix_lda);cat("\n")
  # cat("hello")
  lda_error=(conf_matrix_lda[2]+conf_matrix_lda[3])/sum(conf_matrix_lda)
  predictions_knn<-knn(train = trn_set[,1:5], test = trn_set[,1:5], cl = training_labels, k=3)
  conf_matrix_knn=table(predictions_knn,trn_set[,6])
  knn_error=(conf_matrix_knn[2]+conf_matrix_knn[3])/sum(conf_matrix_knn)
  
  
  app_err_lda_5[i]=lda_error
  app_err_knn_5[i]=knn_error
  
}

print(app_err_lda_5)

lda_appr_exh_err_vec[5]=min(app_err_lda_5)
knn_appr_exh_err_vec[5]=min(app_err_knn_5)

lda_exh_ftr_vec[5]=match(min(app_err_lda_5),app_err_lda_5)
knn_exh_ftr_vec[5]=match(min(app_err_knn_5),app_err_knn_5)

#### 5 feature exh test set verification####
exh_lda_min_5=match(min(app_err_lda_5),app_err_lda_5)
exh_knn_min_5=match(min(app_err_lda_5),app_err_lda_5)
trn_set_exh_5=SFE_train_dataset[combns_of_features_5[exh_lda_min_5,]]
trn_set_exh_5$SFE=SFE_train_dataset$SFE
test_set_exh_5=SFE_test_dataset[combns_of_features_5[exh_lda_min_5,]]
test_set_exh_5$SFE=SFE_test_dataset$SFE


lda_trained_exh_5<-lda(SFE ~ .,data=trn_set_exh_5,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]
#cat("hello-gund")

predictions_lda_exh_5=predict(lda_trained_exh_5, test_set_exh_5[,c(1,2,3,4,5)])$class
conf_matrix_lda_exh_5=table(predictions_lda_exh_5,test_set_exh_5[,6])
lda_true_exh_err_vec[5]=(conf_matrix_lda_exh_5[2]+conf_matrix_lda_exh_5[3])/sum(conf_matrix_lda_exh_5)

training_labels_exh_5=trn_set_exh_5$SFE

predictions_knn_exh_5<-knn(train = trn_set_exh_5[,c(1,2,3,4,5)], test = test_set_exh_5[,c(1,2,3,4,5)] , cl = training_labels_exh_5, k=3)

conf_matrix_knn_exh_5=table(predictions_knn_exh_5,test_set_exh_5[,6])
knn_true_exh_err_vec[5]=(conf_matrix_knn_exh_5[2]+conf_matrix_knn_exh_5[3])/sum(conf_matrix_knn_exh_5)


######Sequential Forward search####
lda_ftr_vec=numeric(5)
knn_ftr_vec=numeric(5)
##For first feature###
knn_min_1=match(min(app_err_knn_1),app_err_knn_1)
lda_min_1=match(min(app_err_lda_1),app_err_lda_1)

lda_ftr_vec[1]=lda_min_1
knn_ftr_vec[1]=knn_min_1


trn_set_fs_lda=SFE_train_dataset[lda_min_1]
trn_set_fs_knn=SFE_train_dataset[knn_min_1]

test_set_fs_lda=SFE_test_dataset[lda_min_1]
test_set_fs_knn=SFE_test_dataset[knn_min_1]

knn_trn_list=1:7
lda_trn_list=1:7

knn_trn_list=setdiff(knn_trn_list,knn_min_1)
lda_trn_list=setdiff(lda_trn_list,lda_min_1)

lda_sfs_error_2=numeric(length(lda_trn_list))
knn_sfs_error_2=numeric(length(knn_trn_list))



####For getting the second feature#####
for(i in 1:length(lda_trn_list)){
  sfs_lda=trn_set_fs_lda
  sfs_knn=trn_set_fs_knn
  sfs_lda[,2]=SFE_train_dataset[,lda_trn_list[i]]
  sfs_lda$SFE=SFE_train_dataset$SFE
  #print(head(sfs_lda),2)
  lda_trained_2<-lda(SFE ~ .,data=sfs_lda,prior=c(0.5,0.5))
  predictions_lda_2=predict(lda_trained_2,sfs_lda[,c(1,2)])$class
  conf_matrix_lda_2=table(predictions_lda_2,sfs_lda[,3])
  #print(conf_matrix_lda_2)
  ##cat(conf_matrix_lda);cat("\n")
  lda_error_2=(conf_matrix_lda_2[2]+conf_matrix_lda_2[3])/sum(conf_matrix_lda_2)
  lda_sfs_error_2[i]=lda_error_2
  
  sfs_knn[,2]=SFE_train_dataset[,knn_trn_list[i]]
  sfs_knn$SFE=SFE_train_dataset$SFE
  training_labels=sfs_knn$SFE
  predictions_knn_2<-knn(train = sfs_knn[,1:2], test = sfs_knn[,1:2], cl = training_labels, k=3)
  conf_matrix_knn_2=table(predictions_knn_2,sfs_knn[,3])
  knn_error_2=(conf_matrix_knn_2[2]+conf_matrix_knn_2[3])/sum(conf_matrix_knn_2)
  knn_sfs_error_2[i]=knn_error_2
}

lda_appr_sfs_err_vec[2]=min(lda_sfs_error_2)
knn_appr_sfs_err_vec[2]=min(knn_sfs_error_2)

lda_min_2=match(min(lda_sfs_error_2),lda_sfs_error_2)
knn_min_2=match(min(knn_sfs_error_2),knn_sfs_error_2)

knn_trn_list=setdiff(knn_trn_list,knn_min_2)
lda_trn_list=setdiff(lda_trn_list,lda_min_2)

lda_ftr_vec[2]=lda_min_2
knn_ftr_vec[2]=knn_min_2

p1=trn_set_fs_knn
trn_set_fs_lda[,2]=SFE_train_dataset[lda_min_2]
trn_set_fs_knn[,2]=SFE_train_dataset[knn_min_2]

test_set_fs_lda[,2]=SFE_test_dataset[lda_min_2]
test_set_fs_knn[,2]=SFE_test_dataset[knn_min_2]

lda_true_sfs_err_vec=numeric(5)
lda_true_sfs_err_vec[1]=lda_true_exh_err_vec[1]

knn_true_sfs_err_vec=numeric(5)
knn_true_sfs_err_vec[1]=knn_true_exh_err_vec[1]


####2 feature sfs test set verification####
sfs_lda_min_2=lda_ftr_vec[2]
sfs_knn_min_2=knn_ftr_vec[2]
trn_set_sfs_2=trn_set_fs_lda
trn_set_sfs_2$SFE=SFE_train_dataset$SFE
test_set_sfs_2=test_set_fs_lda
test_set_sfs_2$SFE=SFE_test_dataset$SFE


lda_trained_sfs_2<-lda(SFE ~ .,data=trn_set_sfs_2,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]

predictions_lda_sfs_2=predict(lda_trained_sfs_2, test_set_sfs_2[,c(1,2)])$class
conf_matrix_lda_sfs_2=table(predictions_lda_sfs_2,test_set_sfs_2[,3])
lda_true_sfs_err_vec[2]=(conf_matrix_lda_sfs_2[2]+conf_matrix_lda_sfs_2[3])/sum(conf_matrix_lda_sfs_2)

##knn related###
trn_set_sfs_knn_2=trn_set_fs_knn
trn_set_sfs_knn_2$SFE=SFE_train_dataset$SFE
test_set_sfs_knn_2=test_set_fs_knn
test_set_sfs_knn_2$SFE=SFE_test_dataset$SFE


training_labels_sfs_2=trn_set_sfs_knn_2$SFE
predictions_knn_sfs_2<-knn(train = trn_set_sfs_knn_2[,c(1,2)], test = test_set_sfs_knn_2[,c(1,2)] , cl = training_labels_sfs_2, k=3)
conf_matrix_knn_sfs_2=table(predictions_knn_sfs_2,test_set_sfs_knn_2[,3])
knn_true_sfs_err_vec[2]=(conf_matrix_knn_sfs_2[2]+conf_matrix_knn_sfs_2[3])/sum(conf_matrix_knn_sfs_2)

  
####For getting the third feature#####
lda_sfs_error_3=numeric(length(lda_trn_list))
knn_sfs_error_3=numeric(length(knn_trn_list))


for(i in 1:length(lda_trn_list)){
  sfs_lda=trn_set_fs_lda
  sfs_knn=trn_set_fs_knn
  sfs_lda[,3]=SFE_train_dataset[,lda_trn_list[i]]
  sfs_lda$SFE=SFE_train_dataset$SFE
  #print(head(sfs_lda),2)
  #print(head(sfs_lda,1))
  #sfs_lda$V2=NULL
  lda_trained_3<-lda(SFE ~ .,data=sfs_lda,prior=c(0.5,0.5))
  predictions_lda_3=predict(lda_trained_3,sfs_lda[,c(1,2,3)])$class
  conf_matrix_lda_3=table(predictions_lda_3,sfs_lda[,4])
  #print(conf_matrix_lda_3)
  ##cat(conf_matrix_lda);cat("\n")
  lda_error_3=(conf_matrix_lda_3[2]+conf_matrix_lda_3[3])/sum(conf_matrix_lda_3)
  lda_sfs_error_3[i]=lda_error_3
  
  sfs_knn[,3]=SFE_train_dataset[,knn_trn_list[i]]
  sfs_knn$SFE=SFE_train_dataset$SFE
  training_labels=sfs_knn$SFE
  predictions_knn_3<-knn(train = sfs_knn[,1:3], test = sfs_knn[,1:3], cl = training_labels, k=3)
  conf_matrix_knn_3=table(predictions_knn_3,sfs_knn[,4])
  knn_error_3=(conf_matrix_knn_3[2]+conf_matrix_knn_3[3])/sum(conf_matrix_knn_3)
  knn_sfs_error_3[i]=knn_error_3
}

lda_appr_sfs_err_vec[3]=min(lda_sfs_error_3)
knn_appr_sfs_err_vec[3]=min(knn_sfs_error_3)

lda_min_3=match(min(lda_sfs_error_3),lda_sfs_error_3)
knn_min_3=match(min(knn_sfs_error_3),knn_sfs_error_3)

lda_ftr_vec[3]=lda_trn_list[lda_min_3]
knn_ftr_vec[3]=knn_trn_list[knn_min_3]

knn_trn_list=setdiff(knn_trn_list,knn_trn_list[knn_min_3])
lda_trn_list=setdiff(lda_trn_list,lda_trn_list[lda_min_3])

trn_set_fs_lda[,3]=SFE_train_dataset[lda_ftr_vec[3]]
trn_set_fs_knn[,3]=SFE_train_dataset[knn_ftr_vec[3]]

test_set_fs_lda[,3]=SFE_test_dataset[lda_ftr_vec[3]]
test_set_fs_knn[,3]=SFE_test_dataset[knn_ftr_vec[3]]



####3 feature sfs test set verification####
sfs_lda_min_3=lda_ftr_vec[3]
sfs_knn_min_3=knn_ftr_vec[3]
trn_set_sfs_3=trn_set_fs_lda
trn_set_sfs_3$SFE=SFE_train_dataset$SFE
test_set_sfs_3=test_set_fs_lda
test_set_sfs_3$SFE=SFE_test_dataset$SFE


lda_trained_sfs_3<-lda(SFE ~ .,data=trn_set_sfs_3,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]

predictions_lda_sfs_3=predict(lda_trained_sfs_3, test_set_sfs_3[,c(1,2,3)])$class
conf_matrix_lda_sfs_3=table(predictions_lda_sfs_3,test_set_sfs_3[,4])
lda_true_sfs_err_vec[3]=(conf_matrix_lda_sfs_3[2]+conf_matrix_lda_sfs_3[3])/sum(conf_matrix_lda_sfs_3)


##knn related###
trn_set_sfs_knn_3=trn_set_fs_knn
trn_set_sfs_knn_3$SFE=SFE_train_dataset$SFE
test_set_sfs_knn_3=test_set_fs_knn
test_set_sfs_knn_3$SFE=SFE_test_dataset$SFE


training_labels_sfs_3=trn_set_sfs_knn_3$SFE

predictions_knn_sfs_3<-knn(train = trn_set_sfs_knn_3[,c(1,2,3)], test = test_set_sfs_knn_3[,c(1,2,3)] , cl = training_labels_sfs_3, k=3)

conf_matrix_knn_sfs_3=table(predictions_knn_sfs_3,test_set_sfs_knn_3[,4])
knn_true_sfs_err_vec[3]=(conf_matrix_knn_sfs_3[2]+conf_matrix_knn_sfs_3[3])/sum(conf_matrix_knn_sfs_3)


####For getting the fourth feature#####
lda_sfs_error_4=numeric(length(lda_trn_list))
knn_sfs_error_4=numeric(length(knn_trn_list))


for(i in 1:length(lda_trn_list)){
  sfs_lda=trn_set_fs_lda
  sfs_knn=trn_set_fs_knn
  sfs_lda[,4]=SFE_train_dataset[,lda_trn_list[i]]
  sfs_lda$SFE=SFE_train_dataset$SFE
  #print(head(sfs_lda),2)
  #print(head(sfs_lda,1))
  #sfs_lda$V2=NULL
  lda_trained_4<-lda(SFE ~ .,data=sfs_lda,prior=c(0.5,0.5))
  predictions_lda_4=predict(lda_trained_4,sfs_lda[,c(1,2,3,4)])$class
  conf_matrix_lda_4=table(predictions_lda_4,sfs_lda[,5])
  #print(conf_matrix_lda_4)
  ##cat(conf_matrix_lda);cat("\n")
  lda_error_4=(conf_matrix_lda_4[2]+conf_matrix_lda_4[3])/sum(conf_matrix_lda_4)
  lda_sfs_error_4[i]=lda_error_4
  
  sfs_knn[,4]=SFE_train_dataset[,knn_trn_list[i]]
  sfs_knn$SFE=SFE_train_dataset$SFE
  training_labels=sfs_knn$SFE
  predictions_knn_4<-knn(train = sfs_knn[,1:4], test = sfs_knn[,1:4], cl = training_labels, k=3)
  conf_matrix_knn_4=table(predictions_knn_4,sfs_knn[,5])
  knn_error_4=(conf_matrix_knn_4[2]+conf_matrix_knn_4[3])/sum(conf_matrix_knn_4)
  knn_sfs_error_4[i]=knn_error_4
}
#####golu###
lda_appr_sfs_err_vec[4]=min(lda_sfs_error_4)
knn_appr_sfs_err_vec[4]=min(knn_sfs_error_4)

lda_min_4=match(min(lda_sfs_error_4),lda_sfs_error_4)
knn_min_4=match(min(knn_sfs_error_4),knn_sfs_error_4)

lda_ftr_vec[4]=lda_trn_list[lda_min_4]
knn_ftr_vec[4]=knn_trn_list[knn_min_4]

knn_trn_list=setdiff(knn_trn_list,knn_trn_list[knn_min_4])
lda_trn_list=setdiff(lda_trn_list,lda_trn_list[lda_min_4])

trn_set_fs_lda[,4]=SFE_train_dataset[lda_ftr_vec[4]]
trn_set_fs_knn[,4]=SFE_train_dataset[knn_ftr_vec[4]]


test_set_fs_lda[,4]=SFE_test_dataset[lda_ftr_vec[4]]
test_set_fs_knn[,4]=SFE_test_dataset[knn_ftr_vec[4]]

###bolu####

#### 4 feature sfs test set verification####
sfs_lda_min_4=lda_ftr_vec[4]
sfs_knn_min_4=knn_ftr_vec[4]
trn_set_sfs_4=trn_set_fs_lda
trn_set_sfs_4$SFE=SFE_train_dataset$SFE
test_set_sfs_4=test_set_fs_lda
test_set_sfs_4$SFE=SFE_test_dataset$SFE


lda_trained_sfs_4<-lda(SFE ~ .,data=trn_set_sfs_4,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]

predictions_lda_sfs_4=predict(lda_trained_sfs_4, test_set_sfs_4[,c(1,2,3,4)])$class
conf_matrix_lda_sfs_4=table(predictions_lda_sfs_4,test_set_sfs_4[,5])
lda_true_sfs_err_vec[4]=(conf_matrix_lda_sfs_4[2]+conf_matrix_lda_sfs_4[3])/sum(conf_matrix_lda_sfs_4)

##knn related###
trn_set_sfs_knn_4=trn_set_fs_knn
trn_set_sfs_knn_4$SFE=SFE_train_dataset$SFE
test_set_sfs_knn_4=test_set_fs_knn
test_set_sfs_knn_4$SFE=SFE_test_dataset$SFE

##knn related###
trn_set_sfs_knn_4=trn_set_fs_knn
trn_set_sfs_knn_4$SFE=SFE_train_dataset$SFE
test_set_sfs_knn_4=test_set_fs_knn
test_set_sfs_knn_4$SFE=SFE_test_dataset$SFE


training_labels_sfs_4=trn_set_sfs_knn_4$SFE

predictions_knn_sfs_4<-knn(train = trn_set_sfs_knn_4[,c(1,2,3,4)], test = test_set_sfs_knn_4[,c(1,2,3,4)] , cl = training_labels_sfs_4, k=3)

conf_matrix_knn_sfs_4=table(predictions_knn_sfs_4,test_set_sfs_knn_4[,5])
knn_true_sfs_err_vec[4]=(conf_matrix_knn_sfs_4[2]+conf_matrix_knn_sfs_4[3])/sum(conf_matrix_knn_sfs_4)


####For getting the fifth feature#####
lda_sfs_error_5=numeric(length(lda_trn_list))
knn_sfs_error_5=numeric(length(knn_trn_list))


for(i in 1:length(lda_trn_list)){
  sfs_lda=trn_set_fs_lda
  sfs_knn=trn_set_fs_knn
  sfs_lda[,5]=SFE_train_dataset[,lda_trn_list[i]]
  sfs_lda$SFE=SFE_train_dataset$SFE
  #print(head(sfs_lda),2)
  #print(head(sfs_lda,1))
  #sfs_lda$V2=NULL
  lda_trained_5<-lda(SFE ~ .,data=sfs_lda,prior=c(0.5,0.5))
  predictions_lda_5=predict(lda_trained_5,sfs_lda[,c(1,2,3,4,5)])$class
  conf_matrix_lda_5=table(predictions_lda_5,sfs_lda[,6])
  #print(conf_matrix_lda_5)
  ##cat(conf_matrix_lda);cat("\n")
  lda_error_5=(conf_matrix_lda_5[2]+conf_matrix_lda_5[3])/sum(conf_matrix_lda_5)
  lda_sfs_error_5[i]=lda_error_5
  
  sfs_knn[,5]=SFE_train_dataset[,knn_trn_list[i]]
  sfs_knn$SFE=SFE_train_dataset$SFE
  training_labels=sfs_knn$SFE
  predictions_knn_5<-knn(train = sfs_knn[,1:5], test = sfs_knn[,1:5], cl = training_labels, k=3)
  conf_matrix_knn_5=table(predictions_knn_5,sfs_knn[,6])
  knn_error_5=(conf_matrix_knn_5[2]+conf_matrix_knn_5[3])/sum(conf_matrix_knn_5)
  knn_sfs_error_5[i]=knn_error_5
}

lda_appr_sfs_err_vec[5]=min(lda_sfs_error_5)
knn_appr_sfs_err_vec[5]=min(knn_sfs_error_5)

lda_min_5=match(min(lda_sfs_error_5),lda_sfs_error_5)
knn_min_5=match(min(knn_sfs_error_5),knn_sfs_error_5)

lda_ftr_vec[5]=lda_trn_list[lda_min_5]
knn_ftr_vec[5]=knn_trn_list[knn_min_5]

knn_trn_list=setdiff(knn_trn_list,knn_trn_list[knn_min_5])
lda_trn_list=setdiff(lda_trn_list,lda_trn_list[lda_min_5])

trn_set_fs_lda[,5]=SFE_train_dataset[lda_ftr_vec[5]]
trn_set_fs_knn[,5]=SFE_train_dataset[knn_ftr_vec[5]]


test_set_fs_lda[,5]=SFE_test_dataset[lda_ftr_vec[5]]
test_set_fs_knn[,5]=SFE_test_dataset[knn_ftr_vec[5]]




#### 5 feature sfs test set verification####
sfs_lda_min_5=lda_ftr_vec[5]
sfs_knn_min_5=knn_ftr_vec[5]
trn_set_sfs_5=trn_set_fs_lda
trn_set_sfs_5$SFE=SFE_train_dataset$SFE
test_set_sfs_5=test_set_fs_lda
test_set_sfs_5$SFE=SFE_test_dataset$SFE


lda_trained_sfs_5<-lda(SFE ~ .,data=trn_set_sfs_5,prior=c(0.5,0.5))

#new_data_exh_1=data.frame(test_set_exh_2[,c(1,2)])
#colnames(new_data_exh_1)=colnames(test_set_exh_1)[1]

predictions_lda_sfs_5=predict(lda_trained_sfs_5, test_set_sfs_5[,c(1,2,3,4,5)])$class
conf_matrix_lda_sfs_5=table(predictions_lda_sfs_5,test_set_sfs_5[,6])
lda_true_sfs_err_vec[5]=(conf_matrix_lda_sfs_5[2]+conf_matrix_lda_sfs_5[3])/sum(conf_matrix_lda_sfs_5)


##knn related###
trn_set_sfs_knn_5=trn_set_fs_knn
trn_set_sfs_knn_5$SFE=SFE_train_dataset$SFE
test_set_sfs_knn_5=test_set_fs_knn
test_set_sfs_knn_5$SFE=SFE_test_dataset$SFE


training_labels_sfs_5=trn_set_sfs_knn_5$SFE

predictions_knn_sfs_5<-knn(train = trn_set_sfs_knn_5[,c(1,2,3,4,5)], test = test_set_sfs_knn_5[,c(1,2,3,4,5)] , cl = training_labels_sfs_5, k=3)

conf_matrix_knn_sfs_5=table(predictions_knn_sfs_5,test_set_sfs_knn_5[,6])
knn_true_sfs_err_vec[5]=(conf_matrix_knn_sfs_5[2]+conf_matrix_knn_sfs_5[3])/sum(conf_matrix_knn_sfs_5)

##should have put at start..####
lda_appr_sfs_err_vec[1]=lda_appr_exh_err_vec[1]
knn_appr_sfs_err_vec[1]=knn_appr_exh_err_vec[1]

lda_sfs_ftr_vec=lda_ftr_vec
knn_sfs_ftr_vec=knn_ftr_vec


#****************#

####Information###

####These are the variables which are useful

###1,2,3,4,5 variable exhaustive search##


###1,2,3,4,5 variable sequential forward search
#lda_sfs_ftr_vec
#knn_sfs_ftr_vec

######plotting part#####

###these are the variables to be plotted####

####Apparent errors for exhaustive search##
##lda_appr_exh_err_vec
##knn_appr_exh_err_vec

####Apparent errors for Sequential forward search##
##lda_appr_sfs_err_vec
##knn_appr_sfs_err_vec


####Test set errors for exhaustive search##
#lda_true_exh_err_vec
#knn_true_exh_err_vec

####Test set errors for sequential forward search##
#lda_true_sfs_err_vec
#knn_true_sfs_err_vec


