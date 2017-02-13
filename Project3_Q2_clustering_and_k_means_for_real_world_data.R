rm(list=ls())

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}

library(MASS)
library(ggplot2)
library(GGally)
library(ggdendro)

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
    SFE_dataset$SFE_level[i]="l"
    #cat("goku")
  }
  else if(SFE_dataset$SFE[i]>35 && SFE_dataset$SFE[i]<45){
    SFE_dataset$SFE_level[i]="m"
    #  print(i)
  }
  else if(SFE_dataset$SFE[i]>=45){
    SFE_dataset$SFE_level[i]="h"
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

####Question 2-a###
a1 = SFE_dataset_clean$SFE_level
hc_avg <- hclust(dist(SFE_dataset_clean[,1:7]), "average")
hc_complete <- hclust(dist(SFE_dataset_clean[,1:7]), "complete")
hc_single <- hclust(dist(SFE_dataset_clean[,1:7]), "single")

dh_avg <- as.dendrogram(hc_avg)
ddata <- dendro_data(dh_avg, type = "rectangle")

p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))
#+  scale_y_reverse(expand = c(0.2, 0))
par(bg="light grey")
plot(hc_complete,labels = a1)
grid(col="white")
cutree_complete=cutree(hc_complete,k=3)
r_complete<-rect.hclust(hc_complete, k=3, border="red")

plot(hc_avg,labels = a1)
grid(col="white")
cutree_avg=cutree(hc_avg,k=3)
r_avg<-rect.hclust(hc_avg, k=3, border="blue")


plot(hc_single,labels = a1)
grid(col="white")
cutree_single=cutree(hc_single,k=3)
r_single<-rect.hclust(hc_single, k=3, border="brown") 

#http://stackoverflow.com/questions/14118033/horizontal-dendrogram-in-r-with-labels







