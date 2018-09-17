#Q1
load("D:/Coding/R/HW6/DSBchurn.Rda")
set.seed(3478)
train<-sample(1:nrow(Churn),0.667*nrow(Churn))
churn.train<-Churn[train,]
churn.test<-Churn[-train,]

library(rpart)
library(caret)

#======================================================================
#Q2
fit.big<-rpart(stay~.,
               churn.train,
               method='class',
               control=rpart.control(minsplit =2,cp=0))
fit.big
nrow(fit.big$frame)
plot(fit.big,
     uniform=TRUE,
     branch=1,
     compress=TRUE,
     main = 'A large Complex Tree with 5663 nodes',
     margin=0.2)
text(fit.big,
     splits = FALSE,
     all=FALSE,
     use.n = TRUE,
     pretty = FALSE,
     cex=0.5)

stay.predict<-predict(fit.big,churn.test,type = 'class')
stay.actual<-churn.test$stay
big_tree_confusionmatrix<-confusionMatrix(table(stay.predict,stay.actual), positive='STAY')
big_tree_confusionmatrix

FPR.big<-1-big_tree_confusionmatrix$byClass['Specificity'][[1]]
FPR.big
FNR.big<-1-big_tree_confusionmatrix$byClass['Sensitivity'][[1]]
FNR.big
ACC.big<-big_tree_confusionmatrix$overall['Accuracy'][[1]]
ACC.big
#false positive rate = FP/N = 1 - Specificty = 0.4051698
#false negative rate = FN/P = 1 - Sensitivity = 0.3561356
#Accuracy:0.6193694

#======================================================================
#Q3
plotcp(fit.big,upper = 'size')
bestcp<-fit.big$cptable[which.min(fit.big$cptable[,'xerror']),'CP']
print(bestcp)
fit.small<-rpart(stay~.,
               churn.train,
               method='class',
               control=rpart.control(cp=bestcp))
fit.small
nrow(fit.small$frame)

stay.predict1<-predict(fit.small,churn.test,type='class')
stay.actual1<-churn.test$stay
small_tree_confusionmatrix<-confusionMatrix(table(stay.predict1,stay.actual1),positive = 'STAY')
small_tree_confusionmatrix

FPR.small<-1-small_tree_confusionmatrix$byClass['Specificity'][[1]]
FPR.small
FNR.small<-1-small_tree_confusionmatrix$byClass['Sensitivity'][[1]]
FNR.small
ACC.small<-small_tree_confusionmatrix$overall['Accuracy'][[1]]
ACC.small
#false positive rate = FP/N = 1 - Specificty = 1 - 0.7229 =0.2771265
#false negative rate = FN/P = 1 - Sensitivity = 1 - 0.6544 =0.3456346
#Accuracy = 0.6885886
plot(fit.small,
     uniform=TRUE,
     branch=1,
     compress=TRUE,
     main = 'A Small Tree with 19 nodes',
     margin=0.1)
text(fit.small,
     splits = TRUE,
     all=TRUE,
     use.n = TRUE,
     pretty = TRUE,
     cex=0.8)

#======================================================================
#Q4
library(ROCR)
churn.pred.test<-predict(fit.small,churn.train,type='prob')
churn.pred.score<-prediction(churn.pred.test[,2],churn.train$stay)
churn.pre.perf<-performance(churn.pred.score,'tpr','fpr')
plot(churn.pre.perf,
     colorize=TRUE,
     lwd=4)
churn.auc<-performance(churn.pred.score,'auc')@y.values[[1]]
churn.auc

churn.cost<-performance(churn.pred.score,measure = 'cost',cost.fp=51000000,cost.fn=196000000)
plot(churn.cost)
churn.pred.test<-predict(fit.small,churn.test,type='prob')
cutoffs = data.frame(cut=churn.cost@"x.values"[[1]], cost=churn.cost@"y.values"[[1]])
best.index = which.min(cutoffs$cost)
cutoffs[best.index,]
cutoff<-cutoffs[best.index,'cut']
churn.pred.test.cutoff<-ifelse(churn.pred.test[,2]<cutoff,'LEAVE','STAY')
cm<-confusionMatrix(table(churn.pred.test.cutoff,churn.test$stay),positive='STAY')
cm

FPR.cm<-1-cm$byClass['Specificity'][[1]]
FPR.cm
FNR.cm<-1-cm$byClass['Sensitivity'][[1]]
FNR.cm
ACC.cm<-cm$overall['Accuracy'][[1]]
ACC.cm

#false positive rate = FP/N = 1 - Specificty = 1 - 0.4058 =0.594229
#false negative rate = FN/P = 1 - Sensitivity = 1 - 0.8953 =0.1047105
#Accuracy = 0.6507508

#======================================================================
#Q5
#For big tree:
#false positive rate = FP/N = 1 - Specificty = 0.4051698
#false negative rate = FN/P = 1 - Sensitivity = 0.3561356
#Accuracy:0.6193694
profit1=541000000-51000000*FPR.big-196000000*FNR.big
profit1

#For small tree:
#false positive rate = FP/N = 1 - Specificty = 1 - 0.7229 =0.2771265
#false negative rate = FN/P = 1 - Sensitivity = 1 - 0.6544 =0.3456346
#Accuracy = 0.6885886
profit2=541000000-51000000*FPR.small-196000000*FNR.small
profit2

#For best threshold pruned tree
#false positive rate = FP/N = 1 - Specificty = 1 - 0.4058 =0.594229
#false negative rate = FN/P = 1 - Sensitivity = 1 - 0.8953 =0.1047105
#Accuracy = 0.6507508
profit3=541000000-51000000*FPR.cm-196000000*FNR.cm
profit3
