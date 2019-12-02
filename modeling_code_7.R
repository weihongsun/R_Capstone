#import csv file and drop "response id" and columns wiht "wgt"
cdm <-read.csv("e:/Modeling17.csv",header = TRUE, strip.white = TRUE)
##cdm <- cdm[,c(-1,-3)]
##cdm <- cdm[,-1]
##cdm <- cdm[cdm$Q407 !=17,]
####cdm <- cdm[cdm$AGE2 !=84,]



#tansform NEWBUYER2 variable
##cdm$NEWBUYER2 <- ifelse(cdm$NEWBUYER2==1,1,0)

#explore the data
dim(cdm)
str(cdm)
summary(cdm)

#transform data set
cdm <- sapply(cdm, as.factor)
cdm <- as.data.frame(cdm)



####validate data set
library(caret)
set.seed(4233)
row1= createDataPartition(cdm$NEWBUYER2,p=0.8,list=F)
prop.table(table(cdm$NEWBUYER2))#0.5197568 0.4802432  , train and test data set are balance
train_set <- cdm[row1,]
test_set <- cdm[-row1,]

####################### GLM model
glmmodel <- glm(train_set$NEWBUYER2~.,family = "binomial",data = train_set)

summary(glmmodel)

#extract the coeffcient tabl from summary of GLM
##glm_coef <- summary(glmmodel)$coef
summary(glmmodel)$coefficients

#importance of decision variable
varImp(glmmodel)

imp_glm <- varImp(glmmodel)
write.csv(imp_glm,file = "e:/imp.csv")

#predict with GLM model
library(caret)
glm_pred_train <- predict(glmmodel, newdata = train_set, type = "response")

#plot ROC and find AUC
library(ggplot2)
library(ROCR)
glm_train_prob = prediction(glm_pred_train, train_set$NEWBUYER2)#standarize prediction
glm_train_predroc <- performance(glm_train_prob,"tpr","fpr")
# Plot the graph 
plot(glm_train_predroc,colorized=T,print.cutoffs.at=seq(0,1,0.05))
glm_train_predauc <- performance(glm_train_prob, measure = "auc")
glm_train_auc <- glm_train_predauc@y.values[[1]]
glm_train_auc# 0.9366956

#plot ROC and find AUC of test data
library(pROC)
glm_pred_test <-predict(glmmodel, test_set, type = "response")
glm_test_roc <- roc(test_set$NEWBUYER2~glm_pred_test)
plot(glm_test_roc)
auc(glm_test_roc) # 0.7869

#Accuracy of GLM
glm_pred_accu <- ifelse(glm_pred_test>0.5,1,0)
glm_pred_accu <- as.factor(glm_pred_accu)
confusionMatrix(glm_pred_accu,test_set$NEWBUYER2, positive = "1")#Accuracy : 0.7293



###############Decision Tree
library(rpart)

dt_model <- rpart(train_set$NEWBUYER2~.,data = train_set,method = "class")
summary(dt_model)

library(rpart.plot)
rpart.plot(dt_model,type = 4, fallen.leaves = FALSE, tweak=2, main = "decision tree model")


plotcp(dt_model)  # no need prunerpart.plot(dt_model,type = 4, fallen.leaves = FALSE, tweak=1.5, main = "decision tree model")
text(dt_model, use.n=TRUE, all=TRUE, cex=.8)

##dt_model_prune <- prune(dt_model,cp=0.023)

printcp(dt_model)

##dt_model_prune <- prune(dt_model,cp=0.016)

# importance of decision variable
varImp(dt_model)

#prdiction on test_set
dt_pred_test <- predict(dt_model,test_set,type = "class")


#plot ROC and find AUC of test data
library(pROC)
dt_test_roc <- roc(test_set$NEWBUYER2~as.numeric(dt_pred_test))
plot(dt_test_roc)
auc(dt_test_roc)# 0.7057

#accuracy of decision tree
confusionMatrix(dt_pred_test,test_set$NEWBUYER2) #Accuracy :  0.687

########### random forest
library(randomForest)
rf_model <- randomForest(train_set$NEWBUYER2~., data = train_set,keep.forest = TRUE, ntree=30)
summary(rf_model)
print(rf_model)

#importance of variables
varImp(rf_model)
varImpPlot(rf_model, sort=TRUE, n.var=min(30, nrow(rf_model$importance)))

#prediction of test and compute accuracy
rf_pred_test <- predict(rf_model, newdata = test_set, type = "response", norm.votes = TRUE)
confusionMatrix(rf_pred_test,test_set$NEWBUYER2, positive = "1")# Accuracy : 0.7056 

#plot ROC and find AUC of test data
library(pROC)
rf_test_roc <- roc(test_set$NEWBUYER2~as.numeric(rf_pred_test))
plot(rf_test_roc)
auc(rf_test_roc)#  0.8562


######### NAIVE BAYES
install.packages("e1071")
library(e1071)

nb_model <- naiveBayes(train_set$NEWBUYER2~., data = train_set)
summary(nb_model)
print(nb_model)

#prediction of test and accuracy
nb_pred_test <- predict(nb_model,newdata = test_set)

conf_matrix <- table(nb_pred_test, test_set$NEWBUYER2)> TP_TN<-conf_matrix[1,1]+conf_matrix[2,2]
TP_TN_FP_FN<-conf_matrix[1,1]+conf_matrix[2,2]+conf_matrix[1,2]+conf_matrix[2,1]
accuracy_nb<-TP_TN/TP_TN_FP_FN
accuracy_nb # accuracy:0.7445008


#plot ROC and find AUC of test data
library(pROC)
nb_test_roc <- roc(test_set$NEWBUYER2~as.numeric(nb_pred_test))
plot(nb_test_roc, main ="Naive Byes")
auc(nb_test_roc)# 0.7765

################## SVM
library(e1071)

svm_model <- svm(train_set$NEWBUYER2~., data = train_set, cross = 10, gmma =0.5, cost =1)
summary(svm_model)

# prediction of test and accuracy
svm_pred_test <- predict(svm_model,newdata = test_set)
library(caret)
confusionMatrix(svm_pred_test,test_set$NEWBUYER2, positive = "1")#Accuracy : 0.8613  

#plot ROC and find AUC of test data
library(pROC)
svm_test_roc <- roc(test_set$NEWBUYER2~as.numeric(svm_pred_test))
plot(svm_test_roc, main = "svm")
auc(svm_test_roc)#  0.8651







