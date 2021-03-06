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


####Question 1-a####
SFE_dataset_numeric=SFE_dataset_clean[,1:7]
prin_comp_SFE=prcomp(SFE_dataset_numeric,scale. = TRUE)
prin_comp_SFE_new=princomp(SFE_dataset_numeric)


####Question 1-b####
prin_comp_SFE_var=prin_comp_SFE$sdev*prin_comp_SFE$sdev
prin_comp_SFE_var_per=100*prin_comp_SFE_var/sum(prin_comp_SFE_var)
prin_comp_SFE_var_df=data.frame(prin_comp_SFE_var_per)
prin_comp_SFE_var_df$pc_num=c(1,2,3,4,5,6,7)

plot_q2=ggplot(prin_comp_SFE_var_df, aes(y = prin_comp_SFE_var_per , x = pc_num ))+theme(axis.title = element_text(size=12))
plot_q2=plot_q2+geom_line(size=2.5,color="pink")+geom_point(size=2.5,color="blue")
plot_q2=plot_q2+xlab("PC_index")+ylab("Percentage_of_variance")
#plot_q2=plot_q2+title("Amount of variance expalined by different PCs")



###Question 1-c######

prin_comp_SFE1=prin_comp_SFE$x[,1:7]
cols <- character(nrow(prin_comp_SFE1))
#cols[]="red"

for(i in 1:length(SFE_dataset_clean[,1])){
  if(SFE_dataset_clean$SFE_level[i]=="high"){
    cols[i]="red"
    #print("red")
  }
  else if(SFE_dataset_clean$SFE_level[i]=="medium"){
    cols[i]="blue"
  }
  else{
    cols[i]="green"
  }
}

pairs(prin_comp_SFE1,col=cols)
prin_comp_SFE1=data.frame(prin_comp_SFE1)
prin_comp_SFE1$colors=SFE_dataset_clean$SFE_level
#plot_q3=ggplot(data=prin_comp_SFE1)+ggpairs()

plot_q3=ggpairs(data = prin_comp_SFE1,columns = 1:7, ggplot2::aes(colour=colors))


####Question-1-d ####

####Use the rotation matrix######

