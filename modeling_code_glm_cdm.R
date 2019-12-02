#import csv file and drop "response id" and columns wiht "wgt"
cdm <-read.csv("d:/Modeling-4.csv",header = TRUE, strip.white = TRUE)


#explore the data
dim(cdm)
str(cdm)
summary(cdm)

#transform data set
cdm <- sapply(cdm, as.factor)
cdm <- as.data.frame(cdm)

#check the missing values
mean(!complete.cases(cdm))
Missingvalues <- colSums(is.na(cdm))
barplot(Missingvalues)
Missingratio <- data.frame(colSums(is.na(cdm))/nrow(cdm))
Missingratio

#visualize the missing value
#library(mice)
#library("VIM")
#md.pattern(cdm, plot = TRUE)
#aggr(cdm, prop=TRUE, numbers=TRUE)

##marginplot(cdm[c("NumberofChildren","Region")],pch=c(20),col=c("darkgray","red","blue"))
                         
#impute missing values
#library(DMwR)
#cdm <- centralImputation(cdm)
#mean(!complete.cases(cdm))
#Missingvalues1 <- colSums(is.na(cdm))
#Missingratio1 <- data.frame(colSums(is.na(cdm))/nrow(cdm))
#aggr(cdm, prop=TRUE, numbers=TRUE)

####validate data set
library(caret)

####################### GLM model

glmmodel <- glm(cdm$NEWBUYER2~.,family = "binomial",data = cdm)
summary(glmmodel)

#extract the coeffcient tabl from summary of GLM
##glm_coef <- summary(glmmodel)$coef
summary(glmmodel)$coefficients

#importance of decision variable
##varImp(glmmodel)

#predict with GLM model
library(caret)
glm_pred<- predict(glmmodel, newdata = cdm, type = "response")

#plot ROC and find AUC
library(ggplot2)
library(ROCR)
glm_prob = prediction(glm_pred, cdm$NEWBUYER2)#standarize prediction
glm_predroc <- performance(glm_prob,"tpr","fpr")
# Plot the graph 
plot(glm_predroc,colorized=T,print.cutoffs.at=seq(0,1,0.05))
glm_predauc <- performance(glm_prob, measure = "auc")
glm_auc <- glm_predauc@y.values[[1]]
glm_auc

#plot ROC and find AUC of test data
library(pROC)


#Accuracy of GLM
glm_pred_accu <- ifelse(glm_pred>0.5,1,0)
glm_pred_accu <- as.factor(glm_pred_accu)
confusionMatrix(glm_pred_accu,cdm$NEWBUYER2, positive = "1")  









