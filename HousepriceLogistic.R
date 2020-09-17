dff <- read.csv("/Users/dinkargoyal/Desktop/r_programme_material/House-Price-class.csv", header = TRUE)
View(dff)
summary(dff)

#EDA
boxplot(dff$n_hot_rooms)
hist(dff$parks)
hist(dff$rainfall)
barplot(table(dff$airport))
barplot(table(dff$bus_ter))
barplot(table(dff$waterbody))
pairs(~price+n_hot_rooms+rainfall, data = dff)

#outlier treatment of hotelrooms
uv = 3*quantile(dff$n_hot_rooms, 0.99)
dff$n_hot_rooms[dff$n_hot_rooms > uv] <- uv
summary(dff$n_hot_rooms)
boxplot(dff$n_hot_rooms)
hist(dff$n_hot_rooms)


#outlier treatment of rainfall
summary(dff$rainfall)
lv = 0.3* quantile(dff$rainfall, 0.01)
dff$rainfall[dff$rainfall < lv] <- lv
summary(dff$rainfall)


#outlier treatment for age
summary(dff$age)
lv = 0.3* quantile(dff$age, 0.01)
dff$age[dff$age < lv] <-lv
summary(dff$age)

#null values treatment of hospbeds
summary(dff$n_hos_beds)
which(is.na(dff$n_hos_beds))
dff$n_hos_beds[is.na(dff$n_hos_beds)] <- mean(dff$n_hos_beds,na.rm = TRUE)
summary(dff$n_hos_beds)


#feature engineering for distance column
dff$avg_dist = (dff$dist1+dff$dist2+dff$dist3+dff$dist4)/4
dff$avg_dist
View(dff)

#removal of distance
dff <- dff[,-6:-9]
dff <- dff[,-13]
View(dff)
summary(dff)
str(dff)


#create dummy variables (one hot encoding)
library(dummies)
dff <- dummy.data.frame(dff)
View(dff)


dff <- dff[,-8]
dff <- dff[,-13]
View(dff)



#splitting of data
library(caTools)
set.seed(0)
split = sample.split(dff,SplitRatio = 0.8)
training_set = subset(dff,split == TRUE)
test_set = subset(dff, split == FALSE)
View(training_set)
View(test_set)

#training model 1
model1 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model1)

#predictions
pred1 = predict(model1, type = 'response', newdata = test_set)
y_pred1 = ifelse(pred1 > 0.5, 1, 0)
View(y_pred1)

#confusion matrix
cm_1 = table(test_set[,16], y_pred1 > 0.5)
cm_1

#accuracy
acc_1 <- sum(diag(cm_1))/sum(cm_1)
acc_1

#precision
prec1 <- diag(cm_1)/colSums(cm_1,2)
prec1

#recall
rc1 <- diag(cm_1)/rowSums(cm_1,2)
rc1

#f1 score
f1_score1 <- 2*prec1*rc1/(prec1+rc1)
f1_score1

#auc
library(pROC)
auc(test_set$Sold, pred1)
roc1 <- roc(test_set$Sold, pred1 , quiet = FALSE)

#ROCR Curve
library(ROCR)
ROCRpred1 <- prediction(pred1,test_set$Sold)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))


#bacward elimination
training_set <- training_set[,-11]
test_set <- test_set[,-11]
training_set <- training_set[,-8]
test_set <- test_set[,-8]

#training model 2
model2 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model2)

#predictions
pred2 = predict(model2, type = 'response', newdata = test_set)
y_pred2 = ifelse(pred2 > 0.5, 1, 0)

#confusion matrix
cm_2 = table(test_set[,14], y_pred2 > 0.5)
cm_2

#accuracy
acc_2 <- sum(diag(cm_2))/sum(cm_2)
acc_2

#precision
prec2 <- diag(cm_2)/colSums(cm_2,2)
prec2

#recall
rc2 <- diag(cm_2)/rowSums(cm_2,2)
rc2

#f1 score
f1_score2 <- 2*prec2*rc2/(prec2+rc2)
f1_score2

#auc
#library(pROC)
auc(test_set$Sold, pred2)
roc2 <- roc(test_set$Sold, pred2 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred2 <- prediction(pred2,test_set$Sold)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))

#bacward elimination
training_set <- training_set[,-5]
test_set <- test_set[,-5]

#training model 3
model3 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model3)

#predictions
pred3 = predict(model3, type = 'response', newdata = test_set)
y_pred3 = ifelse(pred3 > 0.5, 1, 0)

#confusion matrix
cm_3 = table(test_set[,13], y_pred3 > 0.5)
cm_3

#accuracy
acc_3 <- sum(diag(cm_3))/sum(cm_3)
acc_3

#precision
prec3 <- diag(cm_3)/colSums(cm_3,2)
prec3

#recall
rc3 <- diag(cm_3)/rowSums(cm_3,2)
rc3

#f1 score
f1_score3 <- 2*prec3*rc3/(prec3+rc3)
f1_score3

#auc
#library(pROC)
auc(test_set$Sold, pred3)
roc3 <- roc(test_set$Sold, pred3 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred3 <- prediction(pred3,test_set$Sold)
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')
plot(ROCRperf3, colorize = TRUE, text.adj = c(-0.2,1.7))

#bacward elimination
training_set <- training_set[,-10]
test_set <- test_set[,-10]

#training model 4
model4 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model4)

#predictions
pred4 = predict(model4, type = 'response', newdata = test_set)
y_pred4 = ifelse(pred4 > 0.5, 1, 0)

#confusion matrix
cm_4 = table(test_set[,12], y_pred4 > 0.5)
cm_4

#accuracy
acc_4 <- sum(diag(cm_4))/sum(cm_4)
acc_4

#precision
prec4 <- diag(cm_4)/colSums(cm_4,2)
prec4

#recall
rc4 <- diag(cm_4)/rowSums(cm_4,2)
rc4

#f1 score
f1_score4 <- 2*prec4*rc4/(prec4+rc4)
f1_score4

#auc
#library(pROC)
auc(test_set$Sold, pred4)
roc4 <- roc(test_set$Sold, pred4 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred4 <- prediction(pred4,test_set$Sold)
ROCRperf4 <- performance(ROCRpred4, 'tpr','fpr')
plot(ROCRperf4, colorize = TRUE, text.adj = c(-0.2,1.7))

#bacward elimination
training_set <- training_set[,-10]
test_set <- test_set[,-10]

#training model 5
model5 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model5)

#predictions
pred5 = predict(model5, type = 'response', newdata = test_set)
y_pred5 = ifelse(pred5 > 0.5, 1, 0)

#confusion matrix
cm_5 = table(test_set[,11], y_pred5 > 0.5)
cm_5

#accuracy
acc_5 <- sum(diag(cm_5))/sum(cm_5)
acc_5

#precision
prec5 <- diag(cm_5)/colSums(cm_5,2)
prec5

#recall
rc5 <- diag(cm_5)/rowSums(cm_5,2)
rc5

#f1 score
f1_score5 <- 2*prec5*rc5/(prec5+rc5)
f1_score5

#auc
#library(pROC)
auc(test_set$Sold, pred5)
roc5 <- roc(test_set$Sold, pred5 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred5 <- prediction(pred5,test_set$Sold)
ROCRperf5 <- performance(ROCRpred5, 'tpr','fpr')
plot(ROCRperf5, colorize = TRUE, text.adj = c(-0.2,1.7))

#backward elimination
training_set <- training_set[,-9]
test_set <- test_set[,-9]

#training model 6
model6 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model6)

#predictions
pred6 = predict(model6, type = 'response', newdata = test_set)
y_pred6 = ifelse(pred6 > 0.5, 1, 0)

#confusion matrix
cm_6 = table(test_set[,10], y_pred6 > 0.5)
cm_6

#accuracy
acc_6 <- sum(diag(cm_6))/sum(cm_6)
acc_6

#precision
prec6 <- diag(cm_6)/colSums(cm_6,2)
prec6

#recall
rc6 <- diag(cm_6)/rowSums(cm_6,2)
rc6

#f1 score
f1_score6 <- 2*prec6*rc6/(prec6+rc6)
f1_score6

#auc
#library(pROC)
auc(test_set$Sold, pred6)
roc6 <- roc(test_set$Sold, pred6 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred6 <- prediction(pred6,test_set$Sold)
ROCRperf6 <- performance(ROCRpred6, 'tpr','fpr')
plot(ROCRperf6, colorize = TRUE, text.adj = c(-0.2,1.7))

#backward elimination
training_set <- training_set[,-9]
test_set <- test_set[,-9]

#training model 7
model7 <- glm(Sold ~ ., family = 'binomial', data = training_set)
summary(model7)

#predictions
pred7 = predict(model7, type = 'response', newdata = test_set)
y_pred7 = ifelse(pred7 > 0.5, 1, 0)

#confusion matrix
cm_7 = table(test_set[,9], y_pred7 > 0.5)
cm_7

#accuracy
acc_7 <- sum(diag(cm_7))/sum(cm_7)
acc_7

#precision
prec7 <- diag(cm_7)/colSums(cm_7,2)
prec7

#recall
rc7 <- diag(cm_7)/rowSums(cm_7,2)
rc7

#f1 score
f1_score7 <- 2*prec7*rc7/(prec7+rc7)
f1_score7

#auc
#library(pROC)
auc(test_set$Sold, pred7)
roc7 <- roc(test_set$Sold, pred7 , quiet = FALSE)

#ROCR Curve
#library(ROCR)
ROCRpred7 <- prediction(pred7,test_set$Sold)
ROCRperf7 <- performance(ROCRpred7, 'tpr','fpr')
plot(ROCRperf7, colorize = TRUE, text.adj = c(-0.2,1.7))