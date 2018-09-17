#1. Read in the CSV file using read.csv(file.choose()) and save it into churn data frame.
Churn <- read.csv("D:/Coding/R/HW 5/DSBchurn.CSV")
Churn<-read.csv(file.choose())
#2.Examine the structure of the churn data frame. It should look like the figure below
library('dplyr')
glimpse(Churn)
str(Churn)

#3.If the column types do not match, the use the conversion functions to fix it.

#4.Fix the order of levels of factors to match that in the table on the first page. 
#See http://www.r-bloggers.com/reorder-factor-levels-2/ for help.
print(levels(Churn$college))
Churn$college = factor(Churn$college,levels(Churn$college)[c(2,1)])
levels(Churn$college)

print(levels(Churn$rep_sat))
Churn$rep_sat = factor(Churn$rep_sat,levels(Chur2n$rep_sat)[c(5,3,1,2,4)])
levels(Churn$rep_sat)

print(levels(Churn$rep_usage))
Churn$rep_usage = factor(Churn$rep_usage,levels(Churn$rep_usage)[c(5,3,1,2,4)])
levels(Churn$rep_usage)

print(levels(Churn$rep_change))
Churn$rep_change = factor(Churn$rep_change,levels(Churn$rep_change)[c(3,4,2,5,1)])
levels(Churn$rep_change)

#5.Save the data frame as DSBchurn.Rda for later reuse.
save(Churn,file='DSBchurn.Rda')

#6.Create randomly sampled training and test data sets with about 66.7% and 33.3% of the observations, 
# respectively. Use the seed 3478 so that it is repeatable across the groups.

set.seed(3478)
training<-sample(nrow(Churn),0.667*nrow(Churn))
Churn.training<-Churn[training,]
Churn.testing<-Churn[-training,]

#7.Grow a tree using the training dataset to explain the stay class variable. 
#Use minsplit=100 to keep the tree small for now.
library(rpart)
fit<-rpart(stay~.,
           Churn.training,
           method = 'class',
           control = rpart.control(minsplit = 100)
           )

#8. Display fit (type fit and hit return).
fit

#9.Explain rows numbered 1, 10, and 3. Which node is the parent node. 
#What was the immediate split that created it? What is the count of stay and leave at this node? 
#Parent node: 
#1:None; 
#3:1;
#10:5;
#immediate split that created it: 3:house>=604440.5; 10: leftover>=24.5
#count of stay and leave at this node:
#1:stay=6815,leave=6525
#3:stay=3082,leave=1379
#10:stay=774,leave=1221

#10.Plot and label the tree. (save the pdf)
plot(fit,
     uniform = TRUE,
     branch=1,
     compress = TRUE,
     main = 'classification Tree for churn prediction',
     margin=0.1)

text(fit,
     all=TRUE,
     use.n = TRUE,
     fancy=FALSE,
     pretty = TRUE,
     cex=1)

#11.Print the confusion matrix for the test data set.
churn.predict1<-predict(fit,Churn.testing,type='class')
churn.actual1<-Churn.testing$stay
confusion_matrix1<-table(churn.predict1,churn.actual1)
library(caret)
confusionMatrix(confusion_matrix1,positive = 'STAY')

#12.Determine the accuracy, error rates, recall, specificity, and precision for this tree 
#and the test data set.

churn.predict<-predict(fit,Churn.training,type='class')
churn.actual<-Churn.training$stay
confusion_matrix<-table(churn.predict,churn.actual)
confusionMatrix(confusion_matrix,positive = 'STAY')

#data for the tree:
#accuracy:0.7024
#error rates: FPR: 1-Specificity = 1-0.7629=0.2371; FNR: 1-Sensitivity=1-0.6445=0.3555
#recall(Sensitivity):0.6445
#specificity:0.7629
#precision(pos pred value):0.7395

#data for the test data set:
#accuracy:0.6889
#error rates: FPR: 1-Specificity = 1-0.7364=0.2636; FNR: 1-Sensitivity=1-0.6415=0.3585
#recall(Sensitivity):0.6415 
#specificity:0.7364
#precision(pos pred value):0.7091

