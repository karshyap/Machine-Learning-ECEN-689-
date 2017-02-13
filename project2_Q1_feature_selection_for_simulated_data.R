rm(list=ls())

library(class)
library(boot)
library(caret)
library(ISLR)

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}

library(MASS) 
library(plyr)
library(ellipse)
library(ggplot2)

rms<-function(x){
  return(sqrt(mean(x^2)))
}

# Multiple plot function
# To plot mutiple items in a ggplot

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




########Main function for simulation###############
lda_knn_gaussian<-function(num_of_sim,n_tr,n_test,sig_const,n_folds){
#num_of_sim- number of runs of the expt
#n_tr<- number of training samples
#n_ts<- number of test samples

avg_error_lda_array=numeric(num_of_sim)
avg_error_knn_array=numeric(num_of_sim)

avg_apparent_error_lda_array=numeric(num_of_sim)
avg_apparent_error_knn_array=numeric(num_of_sim)

avg_error_lda_array_cv_whole=numeric(num_of_sim)
avg_error_knn_array_cv_whole=numeric(num_of_sim)

avg_error_lda_array_loo_whole=numeric(num_of_sim)
avg_error_knn_array_loo_whole=numeric(num_of_sim)







  
mu1=c(0,0)
mu2=c(1,1)
sig=sig_const*matrix(c(1,0.2,0.2,1),2,2)
  
for (q in 1:num_of_sim){
  #cat("carrying-sim",q)
  #cat("\n")
  
k=sample((floor(0.4*n_tr)):(floor(0.6*n_tr)),1)
kt=sample((floor(0.4*n_test)):(floor(0.6*n_test)),1) #for testing




gs_tr_0=mvrnorm(n = k, mu1, sig, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
gs_tr_1=mvrnorm(n = n_tr-k, mu2, sig, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)

gs_ts_0=mvrnorm(n = kt, mu1, sig, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
gs_ts_1=mvrnorm(n = n_test-kt, mu2, sig, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)


gs_tr_0=data.frame(gs_tr_0)
gs_ts_0=data.frame(gs_ts_0)

gs_tr_1=data.frame(gs_tr_1)
gs_ts_1=data.frame(gs_ts_1)

gs_tr_0$Y=0
gs_tr_0$Y=factor(gs_tr_0$Y)
gs_ts_0$Y=0
gs_ts_0$Y=factor(gs_ts_0$Y)

gs_tr_1$Y=1
gs_tr_1$Y=factor(gs_tr_1$Y)
gs_ts_1$Y=1
gs_ts_1$Y=factor(gs_ts_1$Y)

total_gs_tr <- rbind(gs_tr_0, gs_tr_1)
total_gs_ts <- rbind(gs_ts_0, gs_ts_1)


training_labels=total_gs_tr$Y
#cat(head(total_gs_ts))
lda_trained<-lda(Y ~ .,data=total_gs_tr,prior=c(0.5,0.5))
knn_trained<-knn(train = total_gs_tr[,1:2], test = total_gs_ts[,1:2], cl = training_labels, k=3)

predictions_lda=predict(lda_trained,total_gs_ts[,c(1,2)])$class
predictions_knn<-knn(train = total_gs_tr[,1:2], test = total_gs_ts[,1:2], cl = training_labels, k=3)

predictions_lda_apparent=predict(lda_trained,total_gs_tr[,c(1,2)])$class
predictions_knn_apparent<-knn(train = total_gs_tr[,1:2], test = total_gs_tr[,1:2], cl = training_labels, k=3)
predictions_knn_apparent1<-knn(train = total_gs_tr[,1:2], test = total_gs_tr[,1:2], cl = training_labels, k=3)


conf_matrix_lda=table(predictions_lda,total_gs_ts[,3])
conf_matrix_knn=table(predictions_knn,total_gs_ts[,3])


conf_matrix_lda_apparent=table(predictions_lda_apparent,total_gs_tr[,3])
conf_matrix_knn_apparent=table(predictions_knn_apparent,total_gs_tr[,3])
conf_matrix_knn_apparent1=table(predictions_knn_apparent1,total_gs_tr[,3])


lda_error=(conf_matrix_lda[2]+conf_matrix_lda[3])/sum(conf_matrix_lda)
avg_error_lda_array[q]=lda_error
knn_error=(conf_matrix_knn[2]+conf_matrix_knn[3])/sum(conf_matrix_knn)
avg_error_knn_array[q]=knn_error


lda_appaerent_error=(conf_matrix_lda_apparent[2]+conf_matrix_lda_apparent[3])/sum(conf_matrix_lda_apparent)
avg_apparent_error_lda_array[q]=lda_appaerent_error
knn_apparent_error=(conf_matrix_knn_apparent[2]+conf_matrix_knn_apparent[3])/sum(conf_matrix_knn_apparent)
avg_apparent_error_knn_array[q]=knn_apparent_error


#####Doing Cross- validation for k=5####
folds <- cut(seq(1,nrow(total_gs_tr)),breaks=n_folds,labels=FALSE)
for(i in 1:n_folds){
  avg_error_lda_array_cv=numeric(n_folds)
  avg_error_knn_array_cv=numeric(n_folds)
  
  #Segementing the data using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  total_gs_ts_cv <- total_gs_tr[testIndexes, ]  #cross validation test set
  total_gs_tr_cv <- total_gs_tr[-testIndexes, ] #cross validation training set
  
  training_labels_cv=total_gs_tr_cv$Y
  #cat(head(total_gs_ts))
  lda_trained_cv<-lda(Y ~ .,data=total_gs_tr_cv,prior=c(0.5,0.5))
  knn_trained_cv<-knn(train = total_gs_tr_cv[,1:2], test = total_gs_ts_cv[,1:2], cl = training_labels_cv, k=3)
  
  predictions_lda_cv=predict(lda_trained_cv,total_gs_ts_cv[,c(1,2)])$class
  predictions_knn_cv<-knn(train = total_gs_tr_cv[,1:2], test = total_gs_ts_cv[,1:2], cl = training_labels_cv, k=3)
  
  conf_matrix_lda_cv=table(predictions_lda_cv,total_gs_ts_cv[,3])
  conf_matrix_knn_cv=table(predictions_knn_cv,total_gs_ts_cv[,3])
  
  lda_error_cv=(conf_matrix_lda_cv[2]+conf_matrix_lda_cv[3])/sum(conf_matrix_lda_cv)
  avg_error_lda_array_cv[i]=lda_error_cv
  knn_error_cv=(conf_matrix_knn_cv[2]+conf_matrix_knn_cv[3])/sum(conf_matrix_knn_cv)
  avg_error_knn_array_cv[i]=knn_error_cv

  

}

####5-fold cross validation over############

##################################################

#####Doing Leave one-out cross validation####

n_folds_loo=nrow(total_gs_tr)-1
folds_loo <- cut(seq(1,nrow(total_gs_tr)),breaks=n_folds_loo,labels=FALSE)
for(i in 1:n_folds_loo){
  avg_error_lda_array_loo=numeric(n_folds_loo)
  avg_error_knn_array_loo=numeric(n_folds_loo)
  
  #Segementing the data using the which() function 
  testIndexes_loo <- which(folds_loo==i,arr.ind=TRUE)
  total_gs_ts_loo <- total_gs_tr[testIndexes_loo, ]  #cross validation test set
  total_gs_tr_loo <- total_gs_tr[-testIndexes_loo, ] #cross validation training set
  
  training_labels_loo=total_gs_tr_loo$Y
  #cat(head(total_gs_ts))
  lda_trained_loo<-lda(Y ~ .,data=total_gs_tr_loo,prior=c(0.5,0.5))
  knn_trained_loo<-knn(train = total_gs_tr_loo[,1:2], test = total_gs_ts_loo[,1:2], cl = training_labels_loo, k=3)
  
  predictions_lda_loo=predict(lda_trained_loo,total_gs_ts_loo[,c(1,2)])$class
  predictions_knn_loo<-knn(train = total_gs_tr_loo[,1:2], test = total_gs_ts_loo[,1:2], cl = training_labels_loo, k=3)
  
  conf_matrix_lda_loo=table(predictions_lda_loo,total_gs_ts_loo[,3])
  conf_matrix_knn_loo=table(predictions_knn_loo,total_gs_ts_loo[,3])
  
  lda_error_loo=(conf_matrix_lda_loo[2]+conf_matrix_lda_loo[3])/sum(conf_matrix_lda_loo)
  avg_error_lda_array_loo[i]=lda_error_loo
  knn_error_loo=(conf_matrix_knn_loo[2]+conf_matrix_knn_loo[3])/sum(conf_matrix_knn_loo)
  avg_error_knn_array_loo[i]=knn_error_loo
  
  
  
}
avg_error_lda_array_cv_whole[q]=mean(avg_error_lda_array_cv)
avg_error_knn_array_cv_whole[q]=mean(avg_error_knn_array_cv)

avg_error_lda_array_loo_whole[q]=mean(avg_error_lda_array_loo)
avg_error_knn_array_loo_whole[q]=mean(avg_error_knn_array_loo)


}


avg_error=c(0,0)
avg_apparent_error=c(0,0)
avg_error_cv=c(0,0)
avg_error_loo=c(0,0)
var_error_apparent=c(0,0)
var_error_cv=c(0,0)
var_error_loo=c(0,0)
rms_error_apparent=c(0,0)
rms_error_cv=c(0,0)
rms_error_loo=c(0,0)

avg_error[1]=mean(avg_error_lda_array) #true error frm test set
avg_error[2]=mean(avg_error_knn_array)

avg_apparent_error[1]=mean(avg_apparent_error_lda_array)
avg_apparent_error[2]=mean(avg_apparent_error_knn_array)

avg_error_cv[1]=mean(avg_error_lda_array_cv_whole)
avg_error_cv[2]=mean(avg_error_knn_array_cv_whole)

avg_error_loo[1]=mean(avg_error_lda_array_loo_whole)
avg_error_loo[2]=mean(avg_error_knn_array_loo_whole)

#####Calculating stuff for question 1-c######
dif_vec_lda_apparent=avg_apparent_error_lda_array-avg_error_lda_array
dif_vec_knn_apparent=avg_apparent_error_knn_array-avg_error_knn_array

dif_vec_lda_cv=avg_error_lda_array_cv_whole-avg_error_lda_array
dif_vec_knn_cv=avg_error_knn_array_cv_whole-avg_error_knn_array

dif_vec_lda_loo=avg_error_lda_array_loo_whole-avg_error_lda_array
dif_vec_knn_loo=avg_error_knn_array_loo_whole-avg_error_knn_array

var_error_apparent[1]=var(dif_vec_lda_apparent)
var_error_apparent[2]=var(dif_vec_knn_apparent)

var_error_cv[1]=var(dif_vec_lda_cv)
var_error_cv[2]=var(dif_vec_knn_cv)

var_error_loo[1]=var(dif_vec_lda_loo)
var_error_loo[2]=var(dif_vec_knn_loo)

rms_error_apparent[1]=rms(dif_vec_lda_apparent)
rms_error_apparent[2]=rms(dif_vec_knn_apparent)

rms_error_cv[1]=rms(dif_vec_lda_cv)
rms_error_cv[2]=rms(dif_vec_knn_cv)

rms_error_loo[1]=rms(dif_vec_lda_loo)
rms_error_loo[2]=rms(dif_vec_knn_loo)


return(list(avg_error,avg_apparent_error,avg_error_cv,avg_error_loo,var_error_apparent,var_error_cv,var_error_loo,rms_error_apparent,rms_error_cv,rms_error_loo,conf_matrix_knn_apparent,conf_matrix_knn_apparent1))

}

####Return elements#####
#1. Average errors for LDA and 3NN
#2. 

##############Function over###########

n_values=c(20,30,40,50,60)

avg_errors_lda_sig_1=c(0,0,0,0,0)
avg_errors_lda_sig_2=c(0,0,0,0,0)
avg_errors_knn_sig_1=c(0,0,0,0,0)
avg_errors_knn_sig_2=c(0,0,0,0,0)

avg_apparent_errors_lda_sig_1=c(0,0,0,0,0)
avg_apparent_errors_lda_sig_2=c(0,0,0,0,0)
avg_apparent_errors_knn_sig_1=c(0,0,0,0,0)
avg_apparent_errors_knn_sig_2=c(0,0,0,0,0)

avg_cv_errors_lda_sig_1=c(0,0,0,0,0)
avg_cv_errors_lda_sig_2=c(0,0,0,0,0)
avg_cv_errors_knn_sig_1=c(0,0,0,0,0)
avg_cv_errors_knn_sig_2=c(0,0,0,0,0)

avg_loo_errors_lda_sig_1=c(0,0,0,0,0)
avg_loo_errors_lda_sig_2=c(0,0,0,0,0)
avg_loo_errors_knn_sig_1=c(0,0,0,0,0)
avg_loo_errors_knn_sig_2=c(0,0,0,0,0)

var_apparent_errors_lda_sig_1=c(0,0,0,0,0)
var_apparent_errors_lda_sig_2=c(0,0,0,0,0)
var_apparent_errors_knn_sig_1=c(0,0,0,0,0)
var_apparent_errors_knn_sig_2=c(0,0,0,0,0)

var_cv_errors_lda_sig_1=c(0,0,0,0,0)
var_cv_errors_lda_sig_2=c(0,0,0,0,0)
var_cv_errors_knn_sig_1=c(0,0,0,0,0)
var_cv_errors_knn_sig_2=c(0,0,0,0,0)

var_loo_errors_lda_sig_1=c(0,0,0,0,0)
var_loo_errors_lda_sig_2=c(0,0,0,0,0)
var_loo_errors_knn_sig_1=c(0,0,0,0,0)
var_loo_errors_knn_sig_2=c(0,0,0,0,0)

rms_apparent_errors_lda_sig_1=c(0,0,0,0,0)
rms_apparent_errors_lda_sig_2=c(0,0,0,0,0)
rms_apparent_errors_knn_sig_1=c(0,0,0,0,0)
rms_apparent_errors_knn_sig_2=c(0,0,0,0,0)

rms_cv_errors_lda_sig_1=c(0,0,0,0,0)
rms_cv_errors_lda_sig_2=c(0,0,0,0,0)
rms_cv_errors_knn_sig_1=c(0,0,0,0,0)
rms_cv_errors_knn_sig_2=c(0,0,0,0,0)

rms_loo_errors_lda_sig_1=c(0,0,0,0,0)
rms_loo_errors_lda_sig_2=c(0,0,0,0,0)
rms_loo_errors_knn_sig_1=c(0,0,0,0,0)
rms_loo_errors_knn_sig_2=c(0,0,0,0,0)

if(1) ###Knob
{
for(i in 1:length(n_values)){
  
  ###Getting answers for 1-a#########
  avg_all_errors_sig_1=lda_knn_gaussian(n_tr=n_values[i],n_test = 500, num_of_sim = 10,sig_const = 1,n_folds = 5)
  avg_all_errors_sig_2=lda_knn_gaussian(n_tr=n_values[i],n_test = 500, num_of_sim = 10,sig_const = 2,n_folds = 5)
  
  avg_errors_lda_sig_1[i]=avg_all_errors_sig_1[[1]][1]
  avg_errors_lda_sig_2[i]=avg_all_errors_sig_2[[1]][1]
  avg_errors_knn_sig_1[i]=avg_all_errors_sig_1[[1]][2]
  avg_errors_knn_sig_2[i]=avg_all_errors_sig_2[[1]][2]
  
  #####Getting answers for 1-b########
  
  avg_apparent_errors_lda_sig_1[i]=avg_all_errors_sig_1[[2]][1]
  avg_apparent_errors_lda_sig_2[i]=avg_all_errors_sig_2[[2]][1]
  avg_apparent_errors_knn_sig_1[i]=avg_all_errors_sig_1[[2]][2]
  avg_apparent_errors_knn_sig_2[i]=avg_all_errors_sig_2[[2]][2]
  
  #####Cross-validation answers######
  
  avg_cv_errors_lda_sig_1[i]=avg_all_errors_sig_1[[3]][1]
  avg_cv_errors_lda_sig_2[i]=avg_all_errors_sig_2[[3]][1]
  avg_cv_errors_knn_sig_1[i]=avg_all_errors_sig_1[[3]][2]
  avg_cv_errors_knn_sig_2[i]=avg_all_errors_sig_2[[3]][2]
  
  ####Leave out one answers##########
  
  avg_loo_errors_lda_sig_1[i]=avg_all_errors_sig_1[[4]][1]
  avg_loo_errors_lda_sig_2[i]=avg_all_errors_sig_2[[4]][1]
  avg_loo_errors_knn_sig_1[i]=avg_all_errors_sig_1[[4]][2]
  avg_loo_errors_knn_sig_2[i]=avg_all_errors_sig_2[[4]][2]
  
  ######Answers for variance of question-1-c####
  
  ####Finding variances for 1-c########
  var_apparent_errors_lda_sig_1[i]=avg_all_errors_sig_1[[5]][1]
  var_apparent_errors_lda_sig_2[i]=avg_all_errors_sig_2[[5]][1]
  var_apparent_errors_knn_sig_1[i]=avg_all_errors_sig_1[[5]][2]
  var_apparent_errors_knn_sig_2[i]=avg_all_errors_sig_2[[5]][2]
  
  var_cv_errors_lda_sig_1[i]=avg_all_errors_sig_1[[6]][1]
  var_cv_errors_lda_sig_2[i]=avg_all_errors_sig_2[[6]][1]
  var_cv_errors_knn_sig_1[i]=avg_all_errors_sig_1[[6]][2]
  var_cv_errors_knn_sig_2[i]=avg_all_errors_sig_2[[6]][2]
  
  var_loo_errors_lda_sig_1[i]=avg_all_errors_sig_1[[7]][1]
  var_loo_errors_lda_sig_2[i]=avg_all_errors_sig_2[[7]][1]
  var_loo_errors_knn_sig_1[i]=avg_all_errors_sig_1[[7]][2]
  var_loo_errors_knn_sig_2[i]=avg_all_errors_sig_2[[7]][2]
  
  ######Finding rms errors for 1-c#####
  
  rms_apparent_errors_lda_sig_1[i]=avg_all_errors_sig_1[[8]][1]
  rms_apparent_errors_lda_sig_2[i]=avg_all_errors_sig_2[[8]][1]
  rms_apparent_errors_knn_sig_1[i]=avg_all_errors_sig_1[[8]][2]
  rms_apparent_errors_knn_sig_2[i]=avg_all_errors_sig_2[[8]][2]
  
  rms_cv_errors_lda_sig_1[i]=avg_all_errors_sig_1[[9]][1]
  rms_cv_errors_lda_sig_2[i]=avg_all_errors_sig_2[[9]][1]
  rms_cv_errors_knn_sig_1[i]=avg_all_errors_sig_1[[9]][2]
  rms_cv_errors_knn_sig_2[i]=avg_all_errors_sig_2[[9]][2]
  
  rms_loo_errors_lda_sig_1[i]=avg_all_errors_sig_1[[10]][1]
  rms_loo_errors_lda_sig_2[i]=avg_all_errors_sig_2[[10]][1]
  rms_loo_errors_knn_sig_1[i]=avg_all_errors_sig_1[[10]][2]
  rms_loo_errors_knn_sig_2[i]=avg_all_errors_sig_2[[10]][2]
  
  
  }
}

###These are the variables to be plotted

###for 1-a####

###For the test error asked##
#avg_errors_lda_sig_1
#avg_errors_lda_sig_2
#avg_errors_knn_sig_1
#avg_errors_knn_sig_1

###for 1-b#####

##The below stuff should be plotted along with the variables in 1-a##

###For the apparent errors asked
#avg_apparent_errors_lda_sig_1
#avg_apparent_errors_lda_sig_2
#avg_apparent_errors_knn_sig_1
#avg_apparent_errors_knn_sig_2


###For the cv errors asked
#avg_cv_errors_lda_sig_1
#avg_cv_errors_lda_sig_2
#avg_cv_errors_knn_sig_1
#avg_cv_errors_knn_sig_2

###For the loo errors asked
#avg_loo_errors_lda_sig_1
#avg_loo_errors_lda_sig_2
#avg_loo_errors_knn_sig_1
#avg_loo_errors_knn_sig_2

###for 1-c####

####Variances of lda###

#var_apparent_errors_lda_sig_1
#var_apparent_errors_lda_sig_2
#var_apparent_errors_knn_sig_1
#var_apparent_errors_knn_sig_2

#var_cv_errors_lda_sig_1
#var_cv_errors_lda_sig_2
#var_cv_errors_knn_sig_1
#var_cv_errors_knn_sig_2

#var_loo_errors_lda_sig_1
#var_loo_errors_lda_sig_2
#var_loo_errors_knn_sig_1
#var_loo_errors_knn_sig_2

####RMSes of lda###

#rms_apparent_errors_lda_sig_1
#rms_apparent_errors_lda_sig_2
#rms_apparent_errors_knn_sig_1
#rms_apparent_errors_knn_sig_2

#rms_cv_errors_lda_sig_1
#rms_cv_errors_lda_sig_2
#rms_cv_errors_knn_sig_1
#rms_cv_errors_knn_sig_2

#rms_loo_errors_lda_sig_1
#rms_loo_errors_lda_sig_2
#rms_loo_errors_knn_sig_1
#rms_loo_errors_knn_sig_2

####Plotting the errors####

###plot for question 1-a###




