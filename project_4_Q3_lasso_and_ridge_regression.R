#####Question-3#####
###Ridge Regression and Lasso
library(MASS)
library(glmnet)

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

###3-a##
lambda_val<-(c(50,30,15,7,3,1,0.3,0.1,0.03,0.01))
ridge_reg<-glmnet(as.matrix(SFE_dataset_reg[,1:7]), SFE_dataset_reg[,8], alpha =0,lambda = lambda_val, standardize = FALSE)
lasso_reg<-glmnet(as.matrix(SFE_dataset_reg[,1:7]), SFE_dataset_reg[,8], alpha =1,family="gaussian",lambda = lambda_val, standardize = FALSE)
lin_reg_Fe<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$Fe)
X=as.matrix(SFE_dataset_reg[,1:7])
Y=as.matrix(SFE_dataset_reg[,8])
#ridge_reg<-cv.glmnet(X, Y, alpha=0,lambda = lambda_val) # ridge
#lasso_reg<-cv.glmnet(X, Y, alpha=1,lambda = lambda_val) # lasso
lasso_coeffs=coef(lasso_reg,s=lambda_val)
ridge_coeffs=coef(ridge_reg,s=lambda_val)
###Use coef(lasso_reg,)
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- (fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  #text(x, y, labels=labs, ...)
  legend('topright', legend=labs, col=1:length(labs), lty=1) 
}


###3-b###
par(bg="white")

plot(ridge_reg, xvar = "lambda" ,lwd=4)
lbs_fun(ridge_reg)
grid(col="black")
plot(lasso_reg,xvar = "lambda" , lwd=4)
lbs_fun(lasso_reg)
grid(col="black")
#grid(col="white")

###plotting lasso regressed line


lasso_line=ggplot(data = SFE_dataset_reg, aes(x=Fe,SFE))+geom_point(color="green")+theme(text = element_text(size=18))
lasso_line=lasso_line+geom_abline(colour="red",size=2,intercept = 134.10,slope = -1.44)
lasso_line=lasso_line+geom_abline(colour="pink",label="log",size=2,intercept = 74.01,slope = -0.529)