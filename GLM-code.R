cdm <-read.csv("e:/Modeling18.csv",header = TRUE, strip.white = TRUE)
cdm <- sapply(cdm, as.factor)
cdm <- as.data.frame(cdm)
library(caret)
set.seed(4233)
row1= createDataPartition(cdm$NEWBUYER2,p=0.8,list=F)
prop.table(table(cdm$NEWBUYER2))#0.5197568 0.4802432  , train and test data set are balance
train_set <- cdm[row1,]
test_set <- cdm[-row1,]

glmmodel <- glm(train_set$NEWBUYER2~.,family = "binomial",data = train_set)
summary(glmmodel)

summary(glmmodel)$coefficients

v <- varImp(glmmodel,scale=FALSE)
write.csv(v, file = "e:/imp.csv")

glm_pred_test <-predict(glmmodel, test_set, type = "response")

glm_pred_accu <- ifelse(glm_pred_test>0.4,1,0)
glm_pred_accu <- as.factor(glm_pred_accu)
confusionMatrix(glm_pred_accu,test_set$NEWBUYER2, positive = "1")

library(pROC)
glm_pred_test <-predict(glmmodel, test_set, type = "response")
glm_test_roc <- roc(test_set$NEWBUYER2~glm_pred_test)
plot(glm_test_roc)
auc(glm_test_roc)

coff <- summary(glmmodel)$coefficients
class(coff)

write.csv(coff, file = "e:/imp-pvalue.csv")


