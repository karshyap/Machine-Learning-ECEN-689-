rm(list=ls())

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}

library(MASS)
library(ggplot2)
library(GGally)

###Function for multiple ggplots####
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

#####Question 1-a####


lin_reg_C<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$C)
lin_reg_N<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$N)
lin_reg_Ni<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$Ni)
lin_reg_Fe<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$Fe)
lin_reg_Mn<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$Mn)
lin_reg_Si<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$Si)
lin_reg_Cr<-lm(SFE_dataset_reg$SFE~SFE_dataset_reg$Cr)


###Mean RSS####
RSS_mean_C<-(sum((lin_reg_C$residuals)^2))/length(SFE_dataset_reg[,1])
RSS_mean_N<-(sum((lin_reg_N$residuals)^2))/length(SFE_dataset_reg[,1])
RSS_mean_Ni<-(sum((lin_reg_Ni$residuals)^2))/length(SFE_dataset_reg[,1])
RSS_mean_Fe<-(sum((lin_reg_Fe$residuals)^2))/length(SFE_dataset_reg[,1])
RSS_mean_Mn<-(sum((lin_reg_Mn$residuals)^2))/length(SFE_dataset_reg[,1])
RSS_mean_Si<-(sum((lin_reg_Si$residuals)^2))/length(SFE_dataset_reg[,1])
RSS_mean_Cr<-(sum((lin_reg_Cr$residuals)^2))/length(SFE_dataset_reg[,1])


####R-sq values##
R_sq_C=summary(lin_reg_C)$r.squared
R_sq_N=summary(lin_reg_N)$r.squared
R_sq_Ni=summary(lin_reg_Ni)$r.squared
R_sq_Fe=summary(lin_reg_Fe)$r.squared
R_sq_Mn=summary(lin_reg_Mn)$r.squared
R_sq_Si=summary(lin_reg_Si)$r.squared
R_sq_Cr=summary(lin_reg_Cr)$r.squared

####1-b###

plot_C=ggplot(data = SFE_dataset_reg,aes(x=C,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))
plot_N=ggplot(data = SFE_dataset_reg,aes(x=N,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))
plot_Ni=ggplot(data = SFE_dataset_reg,aes(x=Ni,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))
plot_Fe=ggplot(data = SFE_dataset_reg,aes(x=Fe,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))
plot_Mn=ggplot(data = SFE_dataset_reg,aes(x=Mn,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))
plot_Si=ggplot(data = SFE_dataset_reg,aes(x=Si,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))
plot_Cr=ggplot(data = SFE_dataset_reg,aes(x=Cr,SFE))+geom_point(color="green")+stat_smooth(method = "lm", col = "red")+theme(text = element_text(size=18))



###Interpretation###
