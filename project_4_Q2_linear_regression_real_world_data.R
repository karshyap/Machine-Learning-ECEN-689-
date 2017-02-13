rm(list=ls())

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}

library(MASS)
library(ggplot2)
library(GGally)

setwd("/home/kashyap/Desktop/TAMU_first_semester/MTLS_informatics/Project/Project_1")
SFE_dataset=read.csv("SFE_Dataset.csv") #reading the csv file
SFE_dataset_original=SFE_dataset
#removing the non-informative columns

#preprocessing the data
del_vctr=c()
line_cntr1=0  
i=0

for (i in 1:length(SFE_dataset$SFE)){
  #cat(i) 
  if(SFE_dataset$SFE[i]<=35){
    SFE_dataset$SFE_level[i]="low"
    #cat("goku")
  }
  else if(SFE_dataset$SFE[i]>35 && SFE_dataset$SFE[i]<45){
    SFE_dataset$SFE_level[i]="medium"
    #  print(i)
  }
  else if(SFE_dataset$SFE[i]>=45){
    SFE_dataset$SFE_level[i]="high"
  }
  #else {
  #  line_cntr1=line_cntr1+1
  #  del_vctr[line_cntr1]=i
  # }
}
#SFE_dataset=SFE_dataset[-del_vctr,]
#######problem 2(a)##########

clean_function<-function(dataset,thresh){
  colnames_dataset=colnames(dataset)
  count_col=0
  count_vec<-c()
  count_vec1<-c()
  for(j in 1:length(colnames_dataset)){
    count_col=count_col+1
    # cat((colnames_dataset));cat("\n")
    count_row=0
    for(i in 1:length(dataset[,j]))
    {
      if((dataset[i,j])!=0){
        count_row=count_row+1
        #cat(count_row)
        #cat("\n")
        #cat(j); cat("-");cat(i); cat("\n")
      }
    }
    #cat(j);cat("-");cat(count_row);cat("\n")
    if(count_row/length(dataset[,j])>thresh)
    {
      #cat(lengt)
      count_vec[j]=1
    }
    else{
      count_vec[j]=0
    }
    count_vec1[j]=count_row
    #cat(count_vec)
    
  }
  #return(sort(unique(count_col),FALSE))
  #cat(count_vec)
  return(count_vec)
}

non_zero_cols=clean_function(SFE_dataset,0.6)
SFE_dataset_clean=SFE_dataset[non_zero_cols==1]

#####Removing any remaining zeros
line_cntr2=0
del_vctr2=c()

for(i in 1:length(SFE_dataset_clean$SFE)){
  zero_flag=0
  for (j in 1:length(SFE_dataset_clean)){
    if(SFE_dataset_clean[i,j]==0){
      zero_flag=1
    }
    
    
  }
  if(zero_flag){
    line_cntr2=line_cntr2+1
    del_vctr2[line_cntr2]=i
  }
  
}
p11=1
flag=1


SFE_dataset_clean=SFE_dataset_clean[-del_vctr2,]
SFE_dataset_clean$SFE_level=factor(SFE_dataset_clean$SFE_level)

SFE_dataset_reg=SFE_dataset_clean
SFE_dataset_reg$SFE_level=NULL

SFE_dataset_reg=SFE_dataset_reg[c("C","Cr","Fe","Mn","N","Ni","Si","SFE")]

#####Question 2-a###

###Exhaustive search#### 

ftrs=c(1,2,3,4,5,6,7)

combns_of_features_5=t(combn(ftrs,5))
combns_of_features_4=t(combn(ftrs,4))
combns_of_features_3=t(combn(ftrs,3))
combns_of_features_2=t(combn(ftrs,2))
combns_of_features_1=t(combn(ftrs,1))


rss_err_exh_1=numeric(length(combns_of_features_1[,1]))
rss_err_exh_2=numeric(length(combns_of_features_2[,1]))
rss_err_exh_3=numeric(length(combns_of_features_3[,1]))
rss_err_exh_4=numeric(length(combns_of_features_4[,1]))
rss_err_exh_5=numeric(length(combns_of_features_5[,1]))


rsq_err_exh_1=numeric(length(combns_of_features_1[,1]))
rsq_err_exh_2=numeric(length(combns_of_features_2[,1]))
rsq_err_exh_3=numeric(length(combns_of_features_3[,1]))
rsq_err_exh_4=numeric(length(combns_of_features_4[,1]))
rsq_err_exh_5=numeric(length(combns_of_features_5[,1]))

adj_rsq_err_exh_1=numeric(length(combns_of_features_1[,1]))
adj_rsq_err_exh_2=numeric(length(combns_of_features_2[,1]))
adj_rsq_err_exh_3=numeric(length(combns_of_features_3[,1]))
adj_rsq_err_exh_4=numeric(length(combns_of_features_4[,1]))
adj_rsq_err_exh_5=numeric(length(combns_of_features_5[,1]))

rss_exh_ftr_vec=numeric(5)
rsq_exh_ftr_vec=numeric(5)
adj_rsq_exh_ftr_vec=numeric(5)

rss_exh_vec=numeric(5)
rsq_exh_vec=numeric(5)
adj_rsq_exh_vec=numeric(5)

SFE_train_dataset=SFE_dataset_reg

##For 1 feature ###

for(i in 1:length(combns_of_features_1[,1])){
  trn_set=SFE_train_dataset[combns_of_features_1[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set=data.frame(trn_set)
  lin_reg_1<-lm(trn_set$SFE~trn_set[,1])
  RSS_mean_1<-(sum((lin_reg_1$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_1=summary(lin_reg_1)$r.squared
  adj_r_sq_1=summary(lin_reg_1)$adj.r.squared
  
  
  rss_err_exh_1[i]=RSS_mean_1
  rsq_err_exh_1[i]=R_sq_1
  adj_rsq_err_exh_1[i]=adj_r_sq_1
}

rsq_exh_ftr_vec[1]=match(max(rsq_err_exh_1),rsq_err_exh_1)
rss_exh_ftr_vec[1]=match(min(rss_err_exh_1),rss_err_exh_1)
adj_rsq_exh_ftr_vec[1]=match(max(adj_rsq_err_exh_1),adj_rsq_err_exh_1)

rsq_exh_vec[1]=max(rsq_err_exh_1)
rss_exh_vec[1]=min(rss_err_exh_1)
adj_rsq_exh_vec[1]=max(adj_rsq_err_exh_1)

##For 2 features ###

for(i in 1:length(combns_of_features_2[,1])){
  trn_set=SFE_train_dataset[combns_of_features_2[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set=data.frame(trn_set)
  lin_reg_2<-lm(trn_set$SFE~trn_set[,1]+trn_set[,2])
  RSS_mean_2<-(sum((lin_reg_2$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_2=summary(lin_reg_2)$r.squared
  adj_r_sq_2=summary(lin_reg_2)$adj.r.squared
  
  
  rss_err_exh_2[i]=RSS_mean_2
  rsq_err_exh_2[i]=R_sq_2
  adj_rsq_err_exh_2[i]=adj_r_sq_2
}

rsq_exh_ftr_vec[2]=match(max(rsq_err_exh_2),rsq_err_exh_2)
rss_exh_ftr_vec[2]=match(min(rss_err_exh_2),rss_err_exh_2)
adj_rsq_exh_ftr_vec[2]=match(max(adj_rsq_err_exh_2),adj_rsq_err_exh_2)

rsq_exh_vec[2]=max(rsq_err_exh_2)
rss_exh_vec[2]=min(rss_err_exh_2)
adj_rsq_exh_vec[2]=max(adj_rsq_err_exh_2)

##For 3 features ###

for(i in 1:length(combns_of_features_3[,1])){
  trn_set=SFE_train_dataset[combns_of_features_3[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set=data.frame(trn_set)
  lin_reg_3<-lm(trn_set$SFE~trn_set[,1]+trn_set[,2]+trn_set[,3])
  RSS_mean_3<-(sum((lin_reg_3$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_3=summary(lin_reg_3)$r.squared
  adj_r_sq_3=summary(lin_reg_3)$adj.r.squared
  
  
  rss_err_exh_3[i]=RSS_mean_3
  rsq_err_exh_3[i]=R_sq_3
  adj_rsq_err_exh_3[i]=adj_r_sq_3
}

rsq_exh_ftr_vec[3]=match(max(rsq_err_exh_3),rsq_err_exh_3)
rss_exh_ftr_vec[3]=match(min(rss_err_exh_3),rss_err_exh_3)
adj_rsq_exh_ftr_vec[3]=match(max(adj_rsq_err_exh_3),adj_rsq_err_exh_3)

rsq_exh_vec[3]=max(rsq_err_exh_3)
rss_exh_vec[3]=min(rss_err_exh_3)
adj_rsq_exh_vec[3]=max(adj_rsq_err_exh_3)

##For 4 features ###

for(i in 1:length(combns_of_features_4[,1])){
  trn_set=SFE_train_dataset[combns_of_features_4[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set=data.frame(trn_set)
  lin_reg_4<-lm(trn_set$SFE~trn_set[,1]+trn_set[,2]+trn_set[,3]+trn_set[,4])
  RSS_mean_4<-(sum((lin_reg_4$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_4=summary(lin_reg_4)$r.squared
  adj_r_sq_4=summary(lin_reg_4)$adj.r.squared
  
  
  rss_err_exh_4[i]=RSS_mean_4
  rsq_err_exh_4[i]=R_sq_4
  adj_rsq_err_exh_4[i]=adj_r_sq_4
}

rsq_exh_ftr_vec[4]=match(max(rsq_err_exh_4),rsq_err_exh_4)
rss_exh_ftr_vec[4]=match(min(rss_err_exh_4),rss_err_exh_4)
adj_rsq_exh_ftr_vec[4]=match(max(adj_rsq_err_exh_4),adj_rsq_err_exh_4)

rsq_exh_vec[4]=max(rsq_err_exh_4)
rss_exh_vec[4]=min(rss_err_exh_4)
adj_rsq_exh_vec[4]=max(adj_rsq_err_exh_4)

##For 5 features ###

for(i in 1:length(combns_of_features_5[,1])){
  trn_set=SFE_train_dataset[combns_of_features_5[i,]]
  trn_set$SFE=SFE_train_dataset$SFE
  trn_set=data.frame(trn_set)
  lin_reg_5<-lm(trn_set$SFE~trn_set[,1]+trn_set[,2]+trn_set[,3]+trn_set[,4]+trn_set[,5])
  RSS_mean_5<-(sum((lin_reg_5$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_5=summary(lin_reg_5)$r.squared
  adj_r_sq_5=summary(lin_reg_5)$adj.r.squared
  
  
  rss_err_exh_5[i]=RSS_mean_5
  rsq_err_exh_5[i]=R_sq_5
  adj_rsq_err_exh_5[i]=adj_r_sq_5
}

rsq_exh_ftr_vec[5]=match(max(rsq_err_exh_5),rsq_err_exh_5)
rss_exh_ftr_vec[5]=match(min(rss_err_exh_5),rss_err_exh_5)
adj_rsq_exh_ftr_vec[5]=match(max(adj_rsq_err_exh_5),adj_rsq_err_exh_5)

rsq_exh_vec[5]=max(rsq_err_exh_5)
rss_exh_vec[5]=min(rss_err_exh_5)
adj_rsq_exh_vec[5]=max(adj_rsq_err_exh_5)

####Exhaustive search done#####


#####Forward sequential search###

####For the first feature###
rss_sfs_ftr_vec=numeric(5)
rsq_sfs_ftr_vec=numeric(5)
adj_rsq_sfs_ftr_vec=numeric(5)

rss_sfs_vec=numeric(5)
rsq_sfs_vec=numeric(5)
adj_rsq_sfs_vec=numeric(5)

rsq_sfs_ftr_vec[1]=match(max(rsq_err_exh_1),rsq_err_exh_1)
rss_sfs_ftr_vec[1]=match(min(rss_err_exh_1),rss_err_exh_1)
adj_rsq_sfs_ftr_vec[1]=match(max(adj_rsq_err_exh_1),adj_rsq_err_exh_1)

rsq_sfs_vec[1]=max(rsq_err_exh_1)
rss_sfs_vec[1]=min(rss_err_exh_1)
adj_rsq_sfs_vec[1]=max(adj_rsq_err_exh_1)

trn_set_list=1:7
trn_set_fs_reg=SFE_train_dataset[rsq_sfs_ftr_vec[1]]

trn_set_list=setdiff(trn_set_list,rsq_sfs_ftr_vec[1])

####For the second feature###
reg_sfs_error_2=numeric(length(trn_set_list))

rss_err_sfs_2=numeric(length(trn_set_list))
rsq_err_sfs_2=numeric(length(trn_set_list))
adj_rsq_err_sfs_2=numeric(length(trn_set_list))

for(i in 1:length(trn_set_list)){
  sfs_reg=trn_set_fs_reg
  sfs_reg[,2]=SFE_dataset_reg[,trn_set_list[i]]
  sfs_reg$SFE=SFE_dataset_reg$SFE
  lin_reg_sfs_2<-lm(sfs_reg$SFE~sfs_reg[,1]+sfs_reg[,2])
  
  RSS_mean_sfs_2<-(sum((lin_reg_sfs_2$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_sfs_2=summary(lin_reg_sfs_2)$r.squared
  adj_r_sq_sfs_2=summary(lin_reg_sfs_2)$adj.r.squared
  
  
  rss_err_sfs_2[i]=RSS_mean_sfs_2
  rsq_err_sfs_2[i]=R_sq_sfs_2
  adj_rsq_err_sfs_2[i]=adj_r_sq_sfs_2
  
  
}

rsq_sfs_vec[2]=max(rsq_err_sfs_2)
rss_sfs_vec[2]=min(rss_err_sfs_2)
adj_rsq_sfs_vec[2]=max(adj_rsq_err_sfs_2)

rsq_sfs_ftr_vec[2]=match(max(rsq_err_sfs_2),rsq_err_sfs_2)
rss_sfs_ftr_vec[2]=match(min(rss_err_sfs_2),rss_err_sfs_2)
adj_rsq_sfs_ftr_vec[2]=match(max(adj_rsq_err_sfs_2),adj_rsq_err_sfs_2)

trn_set_fs_reg[,2]=SFE_train_dataset[rsq_sfs_ftr_vec[2]]


trn_set_list=setdiff(trn_set_list,rsq_sfs_ftr_vec[2])


####For the third feature###
reg_sfs_error_3=numeric(length(trn_set_list))


rss_err_sfs_3=numeric(length(trn_set_list))
rsq_err_sfs_3=numeric(length(trn_set_list))
adj_rsq_err_sfs_3=numeric(length(trn_set_list))

for(i in 1:length(trn_set_list)){
  sfs_reg=trn_set_fs_reg
  sfs_reg[,3]=SFE_dataset_reg[,trn_set_list[i]]
  sfs_reg$SFE=SFE_dataset_reg$SFE
  lin_reg_sfs_3<-lm(sfs_reg$SFE~sfs_reg[,1]+sfs_reg[,3])
  
  RSS_mean_sfs_3<-(sum((lin_reg_sfs_3$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_sfs_3=summary(lin_reg_sfs_3)$r.squared
  adj_r_sq_sfs_3=summary(lin_reg_sfs_3)$adj.r.squared
  
  
  rss_err_sfs_3[i]=RSS_mean_sfs_3
  rsq_err_sfs_3[i]=R_sq_sfs_3
  adj_rsq_err_sfs_3[i]=adj_r_sq_sfs_3
  
  
}

rsq_sfs_vec[3]=max(rsq_err_sfs_3)
rss_sfs_vec[3]=min(rss_err_sfs_3)
adj_rsq_sfs_vec[3]=max(adj_rsq_err_sfs_3)
#print(trn_set_list)
rsq_sfs_ftr_vec[3]=trn_set_list[match(max(rsq_err_sfs_3),rsq_err_sfs_3)]
rss_sfs_ftr_vec[3]=trn_set_list[match(min(rss_err_sfs_3),rss_err_sfs_3)]
adj_rsq_sfs_ftr_vec[3]=trn_set_list[match(max(adj_rsq_err_sfs_3),adj_rsq_err_sfs_3)]

trn_set_fs_reg[,3]=SFE_train_dataset[rsq_sfs_ftr_vec[3]]


trn_set_list=setdiff(trn_set_list,rsq_sfs_ftr_vec[3])

print(trn_set_list)
####For the fourth feature###
reg_sfs_error_4=numeric(length(trn_set_list))


rss_err_sfs_4=numeric(length(trn_set_list))
rsq_err_sfs_4=numeric(length(trn_set_list))
adj_rsq_err_sfs_4=numeric(length(trn_set_list))

for(i in 1:length(trn_set_list)){
  sfs_reg=trn_set_fs_reg
  sfs_reg[,4]=SFE_dataset_reg[,trn_set_list[i]]
  sfs_reg$SFE=SFE_dataset_reg$SFE
  lin_reg_sfs_4<-lm(sfs_reg$SFE~sfs_reg[,1]+sfs_reg[,4])
  
  RSS_mean_sfs_4<-(sum((lin_reg_sfs_4$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_sfs_4=summary(lin_reg_sfs_4)$r.squared
  adj_r_sq_sfs_4=summary(lin_reg_sfs_4)$adj.r.squared
  
  
  rss_err_sfs_4[i]=RSS_mean_sfs_4
  rsq_err_sfs_4[i]=R_sq_sfs_4
  adj_rsq_err_sfs_4[i]=adj_r_sq_sfs_4
  
  
}

rsq_sfs_vec[4]=max(rsq_err_sfs_4)
rss_sfs_vec[4]=min(rss_err_sfs_4)
adj_rsq_sfs_vec[4]=max(adj_rsq_err_sfs_4)

rsq_sfs_ftr_vec[4]=trn_set_list[match(max(rsq_err_sfs_4),rsq_err_sfs_4)]
rss_sfs_ftr_vec[4]=trn_set_list[match(min(rss_err_sfs_4),rss_err_sfs_4)]
adj_rsq_sfs_ftr_vec[4]=trn_set_list[match(max(adj_rsq_err_sfs_4),adj_rsq_err_sfs_4)]
trn_set_fs_reg[,4]=SFE_train_dataset[rsq_sfs_ftr_vec[4]]


trn_set_list=setdiff(trn_set_list,rsq_sfs_ftr_vec[4])

####For the fifth feature###
reg_sfs_error_5=numeric(length(trn_set_list))


rss_err_sfs_5=numeric(length(trn_set_list))
rsq_err_sfs_5=numeric(length(trn_set_list))
adj_rsq_err_sfs_5=numeric(length(trn_set_list))

for(i in 1:length(trn_set_list)){
  sfs_reg=trn_set_fs_reg
  sfs_reg[,5]=SFE_dataset_reg[,trn_set_list[i]]
  sfs_reg$SFE=SFE_dataset_reg$SFE
  lin_reg_sfs_5<-lm(sfs_reg$SFE~sfs_reg[,1]+sfs_reg[,5])
  
  RSS_mean_sfs_5<-(sum((lin_reg_sfs_5$residuals)^2))/length(SFE_dataset_reg[,1])
  R_sq_sfs_5=summary(lin_reg_sfs_5)$r.squared
  adj_r_sq_sfs_5=summary(lin_reg_sfs_5)$adj.r.squared
  
  
  rss_err_sfs_5[i]=RSS_mean_sfs_5
  rsq_err_sfs_5[i]=R_sq_sfs_5
  adj_rsq_err_sfs_5[i]=adj_r_sq_sfs_5
  
  
}

rsq_sfs_vec[5]=max(rsq_err_sfs_5)
rss_sfs_vec[5]=min(rss_err_sfs_5)
adj_rsq_sfs_vec[5]=max(adj_rsq_err_sfs_5)

rsq_sfs_ftr_vec[5]=trn_set_list[match(max(rsq_err_sfs_5),rsq_err_sfs_5)]
rss_sfs_ftr_vec[5]=trn_set_list[match(min(rss_err_sfs_5),rss_err_sfs_5)]
adj_rsq_sfs_ftr_vec[5]=trn_set_list[match(max(adj_rsq_err_sfs_5),adj_rsq_err_sfs_5)]
trn_set_fs_reg[,5]=SFE_train_dataset[rsq_sfs_ftr_vec[5]]


trn_set_list=setdiff(trn_set_list,rsq_sfs_ftr_vec[5])
