setwd("D:/Purdue/2nd Sem/Predictive Modeling(IE 590)/Final") 
electricity<-read.csv("2012_public_use_data_aug2016.csv",header=TRUE)

Final<-subset(electricity,CENDIV==2)
Final$Electric<-Final$ELWTBTU+Final$ELHTBTU+Final$ELCLBTU
Final$Electric<-Final$Electric/1000


#Variable selection is done manually
#The most important variables are selected manually by removing variables that maximum N/A values 

Final<-Final[,c(2:18,85,86,91,95:98,104,116,126,136,137,139,140,142,143,144,146,152,155:166,174:178,184:200,209:211,214:216,217:225,227:238,255:260,269,398:399,1049,1050,1056,1120)]
Final<-Final[-c(1,2,4,6,14,16,19,21,23,24,25,29,35,37,38:63,64:69,77:84,86:97,106)]

Final1<-Final[complete.cases(Final),]

quantiles <- quantile(Final1$Electric, probs = c(0.85,0.90,0.95))
Final2<-Final1[which(Final1$Electric<=8042.401),]
Final2<-subset(Final2,Electric!=0)


#Some variables are converted to factor and used in the different models
Final10<-Final2
Final10$PBA<-as.factor(Final10$PBA)
Final10$WLCNS<-as.factor(Final10$WLCNS)
Final10$RFCNS<-as.factor(Final10$RFCNS)
Final10$RFCOOL<-as.factor(Final10$RFCOOL)
Final10$RFTILT<-as.factor(Final10$RFTILT)
Final10$BLDSHP<-as.factor(Final10$BLDSHP)
Final10$GLSSPC<-as.factor(Final10$GLSSPC)
Final10$EQGLSS<-as.factor(Final10$EQGLSS)
Final10$NFLOOR<-as.factor(Final10$NFLOOR)
Final10$BASEMNT<-as.factor(Final10$BASEMNT)
Final10$FLCEILHT<-as.factor(Final10$FLCEILHT)
Final10$MONUSE<-as.factor(Final10$MONUSE)
Final10$HT1<-as.factor(Final10$HT1)
Final10$HT2<-as.factor(Final10$HT2)
Final10$ELHT1<-as.factor(Final10$ELHT1)
Final10$ELHT2<-as.factor(Final10$ELHT2)
Final10$PKGHT<-as.factor(Final10$PKGHT)
Final10$BOILER<-as.factor(Final10$BOILER)
Final10$HTPMPH<-as.factor(Final10$HTPMPH)
Final10$SLFCON<-as.factor(Final10$SLFCON)
Final10$OTHTEQ<-as.factor(Final10$OTHTEQ)
Final10$MAINHT<-as.factor(Final10$MAINHT)
Final10$ELCOOL<-as.factor(Final10$ELCOOL)
Final10$RCAC<-as.factor(Final10$RCAC)
Final10$PKGCL<-as.factor(Final10$PKGCL)
Final10$CHILLR<-as.factor(Final10$CHILLR)
Final10$ACWNWL<-as.factor(Final10$ACWNWL)
Final10$EVAPCL<-as.factor(Final10$EVAPCL)
Final10$OTCLEQ<-as.factor(Final10$OTCLEQ)
Final10$MAINCL<-as.factor(Final10$MAINCL)
Final10$CLVCAV<-as.factor(Final10$CLVCAV)
Final10$CLVVAV<-as.factor(Final10$CLVVAV)
Final10$CLVFLR<-as.factor(Final10$CLVFLR)
Final10$CLVOAS<-as.factor(Final10$CLVOAS)
Final10$CLVDEM<-as.factor(Final10$CLVDEM)
Final10$CLVNON<-as.factor(Final10$CLVNON)
Final10$ELWATR<-as.factor(Final10$ELWATR)
Final10$DATACNTR<-as.factor(Final10$DATACNTR)


# # Dummy variable encoding
#In order to run models with categorical data I tried creating dummy variables as well but
#there was not much change in the model performance
# traindum<-Final2
#  library(ade4)
#  library(data.table)
# ohe_feats = c('PBA','WLCNS','RFCNS','RFCOOL','RFTILT','BLDSHP','GLSSPC','EQGLSS','NFLOOR',
#              'FLCEILHT','MONUSE','HT1','HT2','COOL','WATR','ELUSED','ELHT1','ELHT2',
#               'PKGHT','BOILER','HTPMPH','SLFCON','OTHTEQ','MAINHT','ELCOOL','RCAC','PKGCL','CHILLR','ACWNWL','EVAPCL','OTCLEQ','MAINCL',
#               'CLVCAV','CLVVAV','CLVFLR','CLVOAS','CLVDEM','CLVNON','ELWATR','DATACNTR')
# for (f in ohe_feats){
#   df_all_dummy = acm.disjonctif(traindum[f])
#   traindum[f] = NULL
#   traindum = cbind(traindum, df_all_dummy)
#  }

#Linear model(for all variables)------------------------------------
#For using Linear model first convert all variables to int
set.seed(42)
Final7<-Final2
folds <- 10
Final7$folds <- sample(seq(1:folds),size=nrow(Final7),replace=T)
error_linear<-data.frame(linear=numeric(folds))
for(i in (1:folds)){
  
  Final7.test<- Final7[which(Final7$folds==i),]
  Final7.train <-Final7[-which(Final7$folds==i),]
  
  Linear_model<-lm(Electric~.-folds,data=Final7.train)
  pred_linear.OS<-predict(Linear_model,newdata=Final7.test)
  pred_linear.IS<-predict(Linear_model,newdata=Final7.train)
  library(ModelMetrics)
 
  error_linear$linear.OS[i] <-rmse(actual = Final7.test$Electric,predicted = pred_linear.OS)
  error_linear$linear.IS[i] <-rmse(actual = Final7.train$Electric,predicted = pred_linear.IS)
}
mean(error_linear$linear.OS)
mean(error_linear$linear.IS)


#-----------Linear model after selecting variables---------------
#Variables are removed by sseing the summary of linear model
#Model performance improves by removing the variables
set.seed(42)
Final7<-Final2
folds <- 10
Final7$folds <- sample(seq(1:folds),size=nrow(Final7),replace=T)
error_linear_reduced<-data.frame(linear_reduced=numeric(folds))
for(i in (1:folds)){
  
  Final7.test<- Final7[which(Final7$folds==i),]
  Final7.train <-Final7[-which(Final7$folds==i),]
  
  Linear_model1<-lm(Electric~ELEXP+GLSSPC+WKHRS+PKGHT+CHILLR+ACWNWL+OTCLEQ+CLVCAV+DATACNTR,data=Final7.train)
  pred_linear1.OS<-predict(Linear_model1,newdata=Final7.test)
  pred_linear1.IS<-predict(Linear_model1,newdata=Final7.train)
  library(ModelMetrics)
  
  error_linear_reduced$linear_reduced.OS[i] <-rmse(actual = Final7.test$Electric,predicted = pred_linear1.OS)
  error_linear_reduced$linear_reduced.IS[i] <-rmse(actual = Final7.train$Electric,predicted = pred_linear1.IS)
}
mean(error_linear_reduced$linear_reduced.OS)
mean(error_linear_reduced$linear_reduced.IS)

##-------------------------------------GAM--------------------------

library(ISLR)
library(gam)
Final11<-Final2
set.seed(42)
folds <- 10
Final11$folds <- sample(seq(1:folds),size=nrow(Final11),replace=T)
error_gam<-data.frame(gam=numeric(folds))
for(i in (1:folds)){
  
  Final11.test<- Final11[which(Final11$folds==i),]
  Final11.train <-Final11[-which(Final11$folds==i),]
  gam.fit<-gam(Electric~.-folds,data=Final11.train)
  summary(gam.fit)
  gam_output<-gam(Electric~as.numeric(PBA)+SQFT+BLDSHP+NFLOOR+as.numeric(FLCEILHT)+HT2+WKHRS+OTCLEQ+DATACNTR+ELEXP+CDD65,family="gaussian",data=Final11.train)
  
  
  pred_gam.OS<-predict(gam_output,newdata=Final11.test)
  pred_gam.IS<-predict(gam_output,newdata=Final11.train)
   
  library(ModelMetrics)
  error_gam$gam.OS[i] <-rmse(actual = Final11.test$Electric,predicted = pred_gam.OS)
  error_gam$gam.IS[i] <-rmse(actual = Final11.train$Electric,predicted = pred_gam.IS)
}
mean(error_gam$gam.OS)
mean(error_gam$gam.IS)


#------------------Random Forest Full model--------------------------
#Random Forest can differentiate between factors and integers
Final3<-Final10
library(randomForest)
library(caret)
set.seed(42)
folds <- 10
Final3$folds <- sample(seq(1:folds),size=nrow(Final3),replace=T)
error<-data.frame(rf=numeric(folds))
for(i in (1:folds)){
 
  Final3.test<- Final3[which(Final3$folds==i),]
  Final3.train <-Final3[-which(Final3$folds==i),]

library(randomForest)
library(caret)
multi_rf<-randomForest(Electric~.-folds,ntree=500,data=Final3.train, parallel=TRUE,
                        nodesize=3,importance=TRUE,mtry=5)
pred_rf_multi<-predict(multi_rf,newdata=Final3.test)
pred_rf_multi.IS<-predict(multi_rf,newdata=Final3.train)
library(ModelMetrics)
y_test_randomforest<-as.matrix(Final3.test$Electric)
error$rf.OS[i] <-rmse(actual = Final3.test$Electric,predicted = pred_rf_multi)
error$rf.IS[i] <-rmse(actual = Final3.train$Electric,predicted = pred_rf_multi.IS)
Importance_plot1<-varImpPlot(multi_rf,type=2)
Importance_plot1
}
mean(error$rf.OS)
mean(error$rf.IS)


#-----------------Random Forest(Variables selected from from variable importance)----------
Final4<-Final10
set.seed(42)
folds <- 10
 Final4$folds <- sample(seq(1:folds),size=nrow(Final4),replace=T)
 error_reduced<-data.frame(rf_reduced=numeric(folds))
for(i in (1:folds)){
   
  Final4.test.v<- Final4[which(Final4$folds==i),]
  Final4.train.v <-Final4[-which(Final4$folds==i),]
  
  Finalmodel<-randomForest(Electric~ELEXP+SQFT+NFLOOR+DATACNTR+CDD65+CHILLR+HDD65+WKHRS+PBA+FLCEILHT,ntree=500,data=Final4.train.v, parallel=TRUE,
                         nodesize=3,importance=TRUE,mtry=4)
  pred_rf_multi_reduced<-predict(Finalmodel,newdata=Final4.test.v)
  pred_rf_multi_reduced.IS<-predict(Finalmodel,newdata=Final4.train.v)
  library(ModelMetrics)
 
   error_reduced$rf.OS[i] <-rmse(actual = Final4.test.v$Electric,predicted = pred_rf_multi_reduced)
  error_reduced$rf.IS[i]<-rmse(actual=Final4.train.v$Electric,predicted=pred_rf_multi_reduced.IS)
  Importance_plot2<-varImpPlot(Finalmodel,type=2)
  Importance_plot2
  
}

 mean(error_reduced$rf.OS)
 mean(error_reduced$rf.IS)
 
 #Partial dependence plots for random forest(selected variables)
 partialPlot(Finalmodel,Final4.test.v,ELEXP,main=paste("Partial dependence plot for ELEXP"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,SQFT,main=paste("Partial dependence plot for SQFT"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,NFLOOR,main=paste("Partial dependence plot for NFLOOR"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,DATACNTR,main=paste("Partial dependence plot for DATACNTR"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,CDD65,main=paste("Partial dependence plot for CDD65"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,CHILLR,main=paste("Partial dependence plot for CHILLR"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,HDD65,main=paste("Partial dependence plot for HDD65"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,WKHRS,main=paste("Partial dependence plot for WKHRS"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,PBA,main=paste("Partial dependence plot for PBA"),ylab="Electricity consumed")
 partialPlot(Finalmodel,Final4.test.v,FLCEILHT,main=paste("Partial dependence plot for FLCEIL"),ylab="Electricity consumed")


#-----------------Classification and regression Trees method-------------
#It can classify between factors and integers automatically
library(rpart)
library(rpart.plot)
library(Hmisc)
library(caret)

Final5<-Final10
set.seed(42)
folds <- 10
Final5$folds <- sample(seq(1:folds),size=nrow(Final5),replace=T)
error_full<-data.frame(rpart_full=numeric(folds))
for(i in (1:folds)){
  
  Final5.test.f<- Final5[which(Final5$folds==i),]
  Final5.train.f <-Final5[-which(Final5$folds==i),]
  
model_rpart_full<-rpart(Electric~.-folds,data=Final5.train.f)
summary(model_rpart_full)
rpart.plot(model_rpart_full)

predmodel_rpart_full<-predict(model_rpart_full,newdata =Final5.test.f) 
predmodel_rpart_full.IS<-predict(model_rpart_full,newdata=Final5.train.f)

library(ModelMetrics)
error_full$rpart_full.OS[i]<-rmse(actual=Final5.test.f$Electric,predicted=predmodel_rpart_full)
error_full$rpart_full.IS[i]<-rmse(actual=Final5.train.f$Electric,predicted=predmodel_rpart_full.IS)

#Variable selection from rpart
variable_selection_rpart<-varImp(model_rpart_full)
variable_selection_rpart
}
mean(error_full$rpart_full.OS)
mean(error_full$rpart_full.IS)



#--------------After selecting the variables from full rpart model----------------------------
library(rpart)
library(rpart.plot)
library(Hmisc)
library(caret)

Final5<-Final10
set.seed(42)
folds <- 10
Final5$folds <- sample(seq(1:folds),size=nrow(Final5),replace=T)
error_rpart_r<-data.frame(rpart_reduced=numeric(folds))
for(i in (1:folds)){
  
  Final5.test.f<- Final5[which(Final5$folds==i),]
  Final5.train.f <-Final5[-which(Final5$folds==i),]
  
  model_rpart_reduced<-rpart(Electric~PBA+ELEXP+FLCEILHT+NFLOOR+BLDSHP+DATACNTR+PKGHT+SQFT,data=Final5.train.f)
  summary(model_rpart_reduced)
  rpart.plot(model_rpart_reduced)
  
  predmodel_rpart_reduced.OS<-predict(model_rpart_reduced,newdata =Final5.test.f) 
  predmodel_rpart_reduced.IS<-predict(model_rpart_reduced,newdata=Final5.train.f)
  
  library(ModelMetrics)
  error_rpart_r$rpart_reduced.OS[i]<-rmse(actual=Final5.test.f$Electric,predicted=predmodel_rpart_reduced.OS)
  error_rpart_r$rpart_reduced.IS[i]<-rmse(actual=Final5.train.f$Electric,predicted=predmodel_rpart_reduced.IS)
}
mean(error_rpart_r$rpart_reduced.OS)
mean(error_rpart_r$rpart_reduced.IS)


#---------------------------Support vector machines-------------------------------
library(e1071)

Final6<-Final10
set.seed(42)
folds <- 10
Final6$folds <- sample(seq(1:folds),size=nrow(Final6),replace=T)
error_fullsvm<-data.frame(svm_full=numeric(folds))
for(i in (1:folds)){
  
  Final6.test.f<- Final6[which(Final6$folds==i),]
  Final6.train.f <-Final6[-which(Final6$folds==i),]
  
  modelsvm_full<-svm(Electric~.-folds,data=Final6.train.f, kernel="radial",gamma=0.1,Scale=T)
  predsvm_full<-predict(modelsvm_full,Final6.test.f)
  
  predmodel_svm_full.OS<-predict(modelsvm_full,newdata =Final6.test.f) 
  predmodel_svm_full.IS<-predict(modelsvm_full,newdata=Final6.train.f)
  
  library(ModelMetrics)
  error_fullsvm$svm_full.OS[i]<-rmse(actual=Final6.test.f$Electric,predicted=predmodel_svm_full.OS)
  error_fullsvm$svm_full.IS[i]<-rmse(actual=Final6.train.f$Electric,predicted=predmodel_svm_full.IS)
}
mean(error_fullsvm$svm_full.OS)
mean(error_fullsvm$svm_full.IS)




#-----------Bart Machine(full model)---------------
options(java.parameters = "-Xmx4g") 
library('rJava')
library("bartMachine")
set_bart_machine_num_cores(3)
Final9<-Final2

set.seed(42)
folds <- 10
Final9$folds <- sample(seq(1:folds),size=nrow(Final9),replace=T)

error_BART <- data.frame(bart=numeric(folds))
for(i in (1:folds)){
  Final9.test.f<- Final9[which(Final9$folds==i),]
  Final9.train.f <-Final9[-which(Final9$folds==i),]
  Final9.train.covariates <- Final9.train.f[,-c(43,44)]
  Final9.train.response <-Final9.train.f$Electric
  model_bart <- bartMachine(X=Final9.train.covariates,y=Final9.train.response)
  
  model3.pred.OS <- predict(model_bart,new_data = Final9.test.f[,-c(43,44)])
  model3.pred.IS <-predict(model_bart,new_data=Final9.train.f[,-c(43,44)])
  error_BART$bart.OS[i] <-rmse(actual=Final9.test.f$Electric,predicted = model3.pred.OS)
  error_BART$bart.IS[i] <-rmse(actual=Final9.train.f$Electric,predicted=model3.pred.IS)

}
mean(error_BART$bart.OS)
mean(error_BART$bart.IS)

var_selection_by_permute(model_bart,bottom_margin = 3)


#--------BART with variable selection-------------
Final9<-Final2
set.seed(42)
folds <- 10
Final9$folds <- sample(seq(1:folds),size=nrow(Final9),replace=T)

error_BART1 <- data.frame(bart1=numeric(folds))
for(i in (1:folds)){
  Final9.test.v<- Final9[which(Final9$folds==i),]
  Final9.train.v <-Final9[-which(Final9$folds==i),]
  Final9.train.covariates1 <- Final9.train.v[,c(42,39,10,2,11,18,21)]
  Final9.train.response1 <-Final9.train.v$Electric
  model_bart1 <- bartMachine(X=Final9.train.covariates1,y=Final9.train.response1)
  
  model3.pred.OS1 <- predict(model_bart1,new_data = Final9.test.v[,c(42,39,10,2,11,18,21)])
  model3.pred.IS1 <-predict(model_bart1,new_data=Final9.train.v[,c(42,39,10,2,11,18,21)])
  error_BART1$bart1.OS[i] <-rmse(actual=Final9.test.v$Electric,predicted = model3.pred.OS1)
  error_BART1$bart1.IS[i] <-rmse(actual=Final9.train.v$Electric,predicted=model3.pred.IS1)
  
}
mean(error_BART1$bart1.OS)
mean(error_BART1$bart1.IS)

par(mfrow=c(2,3))
plot4<-pd_plot(model_bart1,"SQFT")
plot5<-pd_plot(model_bart1,"NFLOOR")
plot6<-pd_plot(model_bart1,"FLCEILHT")
plot7<-pd_plot(model_bart1,"PKGHT")
plot8<-pd_plot(model_bart1,"DATACNTR")
plot9<-pd_plot(model_bart1,"ELEXP")
plot10<-pd_plot(model_bart1,"SLFCON")



#------MARS(Multivariate Additive Regression Splines)(pruned)-------

library(earth)
Final13<-Final10
set.seed(42)
folds <- 10
Final13$folds <- sample(seq(1:folds),size=nrow(Final13),replace=T)
error_MARS<-data.frame(mars=numeric(folds))
for(i in (1:folds)){
  
  Final13.test<- Final13[which(Final13$folds==i),]
  Final13.train <-Final13[-which(Final13$folds==i),]
  
 Model_MARS<-earth(Electric~.-folds,data=Final13.train)
  pred_MARS.OS<-predict(Model_MARS,newdata=Final13.test)
  pred_MARS.IS<-predict(Model_MARS,newdata=Final13.train)
  library(ModelMetrics)
  
  error_MARS$mars.OS[i] <-rmse(actual = Final13.test$Electric,predicted = pred_MARS.OS)
  error_MARS$mars.IS[i] <-rmse(actual = Final13.train$Electric,predicted = pred_MARS.IS)
  plot1<-plotmo(Model_MARS)
}
mean(error_MARS$mars.OS)
mean(error_MARS$mars.IS)

variable_importance_MARS <- evimp(Model_MARS) # estimate variable importance for MARS(pruned)
plot(variable_importance_MARS)

#pruned reduced model
library(earth)
Final13<-Final10
set.seed(42)
folds <- 10
Final13$folds <- sample(seq(1:folds),size=nrow(Final13),replace=T)
error_MARS1<-data.frame(mars1=numeric(folds))
for(i in (1:folds)){
  
  Final13.test<- Final13[which(Final13$folds==i),]
  Final13.train <-Final13[-which(Final13$folds==i),]
  
  Model_MARS1<-earth(Electric~ELEXP+DATACNTR+NFLOOR+PKGHT+SQFT+FLCEILHT+PBA+OTCLEQ+ELCOOL,data=Final13.train)
  pred_MARS1.OS<-predict(Model_MARS1,newdata=Final13.test)
  pred_MARS1.IS<-predict(Model_MARS1,newdata=Final13.train)
  library(ModelMetrics)
  
  error_MARS1$mars1.OS[i] <-rmse(actual = Final13.test$Electric,predicted = pred_MARS1.OS)
  error_MARS1$mars1.IS[i] <-rmse(actual = Final13.train$Electric,predicted = pred_MARS1.IS)
}
mean(error_MARS1$mars1.OS)
mean(error_MARS1$mars1.IS)


###MARS(unpruned)--------------------------------------------------------

library(earth)
Final16<-Final10
set.seed(42)
folds <- 10
Final16$folds <- sample(seq(1:folds),size=nrow(Final16),replace=T)
error_MARS2<-data.frame(mars2=numeric(folds))
for(i in (1:folds)){
  
  Final16.test<- Final16[which(Final16$folds==i),]
  Final16.train <-Final16[-which(Final16$folds==i),]
  
  Model_MARS2<-earth(Electric~.-folds,data=Final16.train,pmethod="none")
  pred_MARS2.OS<-predict(Model_MARS2,newdata=Final16.test)
  pred_MARS2.IS<-predict(Model_MARS2,newdata=Final16.train)
  library(ModelMetrics)
  
  error_MARS2$mars2.OS[i] <-rmse(actual = Final16.test$Electric,predicted = pred_MARS2.OS)
  error_MARS2$mars2.IS[i] <-rmse(actual = Final16.train$Electric,predicted = pred_MARS2.IS)
  
  variable_importance_MARS2 <- evimp(Model_MARS2) # estimate variable importance for MARS(unpruned)
  plot(variable_importance_MARS2)
  
}
mean(error_MARS2$mars2.OS)
mean(error_MARS2$mars2.IS)


#----Reduced model MARS unpruned--------------------
library(earth)
Final13<-Final10
set.seed(42)
folds <- 10
Final16$folds <- sample(seq(1:folds),size=nrow(Final16),replace=T)
error_MARS3<-data.frame(mars3=numeric(folds))
for(i in (1:folds)){
  
  Final16.test<- Final16[which(Final16$folds==i),]
  Final16.train <-Final16[-which(Final16$folds==i),]
  
  Model_MARS3<-earth(Electric~ELEXP+DATACNTR+NFLOOR+PKGHT+SQFT+FLCEILHT,data=Final16.train,pmethod="none")
  pred_MARS3.OS<-predict(Model_MARS3,newdata=Final16.test)
  pred_MARS3.IS<-predict(Model_MARS3,newdata=Final16.train)
  library(ModelMetrics)
  
  error_MARS3$mars3.OS[i] <-rmse(actual = Final16.test$Electric,predicted = pred_MARS3.OS)
  error_MARS3$mars3.IS[i] <-rmse(actual = Final16.train$Electric,predicted = pred_MARS3.IS)
}
mean(error_MARS3$mars3.OS)
mean(error_MARS3$mars3.IS)


#--------------Neural Networks(Full model)------------------
library(nnet)
Final15<-Final10
set.seed(42)
folds <- 10
Final15$folds <- sample(seq(1:folds),size=nrow(Final15),replace=T)
error_NN<-data.frame(nn=numeric(folds))
for(i in (1:folds)){
  
  Final15.test<- Final15[which(Final15$folds==i),]
  Final15.train <-Final15[-which(Final15$folds==i),]
  myGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1), decay = c(0.5, 0.1,0.01,0.001,0.0001))
 
Model_NN1<-nnet(Electric~.,data=Final15.train,method="nnet",tuneGrid=myGrid,size=5,maxit=1000,lineout=1)
pred_NN.OS<-predict(Model_NN1,newdata=Final15.test)
pred_NN.IS<-predict(Model_NN1,newdata=Final15.train)
library(ModelMetrics)
  
  error_NN$nn.OS[i] <-rmse(actual = Final15.test$Electric,predicted = pred_NN.OS)
  error_NN$nn.IS[i] <-rmse(actual = Final15.train$Electric,predicted = pred_NN.IS)
}
mean(error_NN$nn.OS)
mean(error_NN$nn.IS)

#Normal plot for Neural networks
qqline(resid(Model_NN1))


#--------------------------------Dataframe for all the RMSE values for different models(Full models)------------------
RMSE_OSall<-data.frame(mean(error_linear$linear.OS),mean(rmse.kfold_ridge),mean(error_gam$gam.OS),mean(error$rf.OS),mean(error_full$rpart_full.OS),mean(error_fullsvm$svm_full.OS), mean(error_BART$bart.OS),mean(error_MARS$mars.OS),mean(error_MARS2$mars2.OS),mean(error_NN$nn.OS))
RMSE_OSall
colnames(RMSE_OSall)<-c("Linear model(OS)","Ridge Regression(OS)","GAM(OS)","Random Forest(OS)","Rpart(OS)","SVM(OS)","BART(OS)","MARS_pruned(OS)","MARS_unpruned(OS)","Neural Networks(OS)")
rownames(RMSE_OSall)<-c("Mean")

RMSE_ISall<-data.frame(mean(error_linear$linear.IS),mean(error$rf.IS),mean(error_full$rpart_full.IS),mean(error_fullsvm$svm_full.IS), mean(error_BART$bart.IS),mean(error_MARS$mars.IS),mean(error_MARS2$mars2.IS),mean(error_NN$nn.IS))
RMSE_ISall
colnames(RMSE_ISall)<-c("Linear model(IS)","Ridge Regression(IS)","Random Forest(IS)","Rpart(IS)","SVM(IS)","BART(IS)","MARS_pruned(IS)","MARS_unpruned(IS)","Neural Networks(IS)")
rownames(RMSE_ISall)<-c("Mean")

#---------------------------Dataframe for all the RMSE values after variable selcetion---------------------------------
RMSE_OS_reduced<-data.frame(mean(error_linear_reduced$linear_reduced.OS),mean(error_reduced$rf.OS),mean(error_rpart_r$rpart_reduced.OS), mean(error_BART1$bart1.OS),mean(error_MARS1$mars1.OS),mean(error_MARS3$mars3.OS))
RMSE_OS_reduced
colnames(RMSE_OS_reduced)<-c("Linear model reduced(OS)","Random Forest reduced(OS)","Rpart reduced(OS)","BART reduced(OS)","MARS_pruned_reduced(OS)","MARS_unpruned_reduced(OS)")
rownames(RMSE_OS_reduced)<-c("Mean")

RMSE_IS_reduced<-data.frame(mean(error_linear_reduced$linear_reduced.IS),mean(error_reduced$rf.IS),mean(error_rpart_r$rpart_reduced.IS), mean(error_BART1$bart1.IS),mean(error_MARS3$mars3.IS))
RMSE_IS_reduced
colnames(RMSE_OS_reduced)<-c("Linear model reduced(IS)","Random Forest reduced(IS)","Rpart reduced(IS)","BART reduced(IS)","MARS_pruned_reduced(IS)","MARS_unpruned_reduced(IS)")
rownames(RMSE_OS_reduced)<-c("Mean")



save(Finalmodel, file= 'idhawan.RData')

