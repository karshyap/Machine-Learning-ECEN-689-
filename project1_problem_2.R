rm(list=ls())

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}

library(MASS)
library(ggplot2)

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
  else if(SFE_dataset$SFE[i]>=45){
    SFE_dataset$SFE_level[i]="high"
  }
  else {
    line_cntr1=line_cntr1+1
    del_vctr[line_cntr1]=i
  }
}
SFE_dataset=SFE_dataset[-del_vctr,]
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
#while(p11<0.45 | flag==1){
flag=0
SFE_dataset_clean=SFE_dataset_clean[-del_vctr2,]
SFE_dataset_clean$SFE_level=factor(SFE_dataset_clean$SFE_level)

trn_set_len=0.2*length(SFE_dataset_clean[,1])
test_set_len=0.8*length(SFE_dataset_clean[,1])

SFE_dataset_temp=SFE_dataset_clean
SFE_dataset_clean <- SFE_dataset_temp[sample(nrow(SFE_dataset_temp)),]

#Getting the required data
SFE_dataset_trn=SFE_dataset_clean[1:floor(trn_set_len),]
SFE_dataset_test=SFE_dataset_clean[floor(trn_set_len)+1:floor(test_set_len),]

p1=SFE_dataset_trn$SFE_level=="high"
p12=length(p1[p1==TRUE])
p11=p12/trn_set_len
#}

###############Question 2-b################


a1=t.test(SFE_dataset_trn[,1],SFE_dataset_trn$SFE)
a2=t.test(SFE_dataset_trn[,2],SFE_dataset_trn$SFE)
a3=t.test(SFE_dataset_trn[,3],SFE_dataset_trn$SFE)
a4=t.test(SFE_dataset_trn[,4],SFE_dataset_trn$SFE)
a5=t.test(SFE_dataset_trn[,5],SFE_dataset_trn$SFE)

t_test_array=c(a1$statistic,a2$statistic,a3$statistic,a4$statistic,a5$statistic)
p_value_array=c(a1$p.value,a2$p.value,a3$p.value,a4$p.value,a5$p.value)

###############Question 2-c################


data_for_LDA_2=SFE_dataset_trn
data_for_LDA_2$Ni=NULL
data_for_LDA_2$Fe=NULL
data_for_LDA_2$Cr=NULL
data_for_LDA_2$SFE=NULL


data_for_LDA_test_2=SFE_dataset_test
data_for_LDA_test_2$Ni=NULL
data_for_LDA_test_2$Fe=NULL
data_for_LDA_test_2$Cr=NULL
data_for_LDA_test_2$SFE=NULL

#z <- lda(SFE_level ~ ., data_for_LDA_2,prior=c(1,1)/2)
z_2<-lda(SFE_level ~ .,data=data_for_LDA_2,prior=c(0.5,0.5))
#z<-lda(SFE_level ~ .,data=data_for_LDA_2)

predictions_2=predict(z_2,data_for_LDA_test_2[,c(1,2)])$class
table_data2=table(predictions_2,data_for_LDA_test_2[,3])
p1=z_2$scaling
#plot(data_for_LDA_2[,c(1,2)],col=data_for_LDA_2[,3])
#grid()
#legend(0.2,70,c("low","high"))
plot1=ggplot(data_for_LDA_2, aes(x = C, y = Mn, colour = SFE_level)) +
  geom_point(position = position_jitter(),size=3)

gmean <- z_2$prior%*%z_2$means
const <- drop(gmean%*%z_2$scaling)

slope_of_lda_line=-p1[1]/p1[2]
intercept_of_lda_line <- const/z_2$scaling[2]

total_plot_train=plot1+geom_abline(intercept = intercept_of_lda_line,slope = slope_of_lda_line,color="pink",size=2)


plot2=ggplot(data_for_LDA_test_2, aes(x = C, y = Mn, colour = SFE_level)) +
  geom_point(position = position_jitter(),size=3)

total_plot_test=plot2+geom_abline(intercept = intercept_of_lda_line,slope = slope_of_lda_line,color="pink",size=2)

###############Question 2-d##################

data_for_LDA_3=SFE_dataset_trn
data_for_LDA_3$Ni=NULL
data_for_LDA_3$Cr=NULL
data_for_LDA_3$SFE=NULL

data_for_LDA_test_3=SFE_dataset_test
data_for_LDA_test_3$Ni=NULL
data_for_LDA_test_3$Cr=NULL
data_for_LDA_test_3$SFE=NULL
##################################################
data_for_LDA_4=SFE_dataset_trn
data_for_LDA_4$Cr=NULL
data_for_LDA_4$SFE=NULL

data_for_LDA_test_4=SFE_dataset_test
data_for_LDA_test_4$Cr=NULL
data_for_LDA_test_4$SFE=NULL
######################################################
data_for_LDA_5=SFE_dataset_trn
data_for_LDA_5$SFE=NULL

data_for_LDA_test_5=SFE_dataset_test
data_for_LDA_test_5$SFE=NULL

z_3<-lda(SFE_level ~ .,data=data_for_LDA_3,prior=c(0.5,0.5))
z_3_np<-lda(SFE_level ~ .,data=data_for_LDA_3)

predictions_3=predict(z_3,data_for_LDA_test_3[,c(1,2,3)])$class
predictions_3_np=predict(z_3_np,data_for_LDA_test_3[,c(1,2,3)])$class


table_data3=table(predictions_3,data_for_LDA_test_3[,4])
table_data3_np=table(predictions_3_np,data_for_LDA_test_3[,4])


z_4<-lda(SFE_level ~ .,data=data_for_LDA_4,prior=c(0.5,0.5))
z_4_np<-lda(SFE_level ~ .,data=data_for_LDA_4)

predictions_4=predict(z_4,data_for_LDA_test_4[,c(1,2,3,4)])$class
predictions_4_np=predict(z_4_np,data_for_LDA_test_4[,c(1,2,3,4)])$class

table_data4=table(predictions_4,data_for_LDA_test_4[,5])
table_data4_np=table(predictions_4_np,data_for_LDA_test_4[,5])


z_5<-lda(SFE_level ~ .,data=data_for_LDA_5,prior=c(0.5,0.5))
z_5_np<-lda(SFE_level ~ .,data=data_for_LDA_5)


predictions_5=predict(z_5,data_for_LDA_test_5[,c(1,2,3,4,5)])$class
predictions_5_np=predict(z_5_np,data_for_LDA_test_5[,c(1,2,3,4,5)])$class

table_data5=table(predictions_5,data_for_LDA_test_5[,6])
table_data5_np=table(predictions_5_np,data_for_LDA_test_5[,6])

#########################################################################

