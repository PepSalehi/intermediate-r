options(menu.graphics=FALSE)
diabetes <- read.csv("data/pima-indians-diabetes.data.txt")
colnames(diabetes) <- c("NumPreg","PlasmaGlucose","BP","SkinFoldThickness","SerumInsulin","BMI","Fam","Age","Diabetes")

replace_0_with_NA <- function(x) { 
  x[x==0] <- NA
  return(x)
}

for(cn in c("PlasmaGlucose","BP","SkinFoldThickness","SerumInsulin","BMI","Fam","Age")) {
  diabetes[,cn] <- replace_0_with_NA(diabetes[,cn])
}
isna <- apply(diabetes,1,function(x) { any(is.na(x)) })
diabetes <- diabetes[!isna,]
diabetes$Diabetes <- diabetes$Diabetes==1

diabetes.raw <- diabetes

colmeans <- apply(diabetes,2,mean)
colsds <- apply(diabetes,2,sd)

## scale
for(i in 1:(ncol(diabetes)-1)) { 
  diabetes[,i] <- scale(diabetes[,i])[,1]
}

plot(density(diabetes$SerumInsulin))

#################################################
## linear regression -- predict serum insulin
lm0 <- lm(SerumInsulin ~ 1, data=diabetes)
summary(lm0)

lm1 <- lm(SerumInsulin ~ Age,data=diabetes)
summary(lm1)
plot(diabetes$SerumInsulin,predict(lm1))

anova(lm0,lm1)

lm2 <- lm(SerumInsulin ~ Age + PlasmaGlucose, data=diabetes)
summary(lm2)
plot(diabetes$SerumInsulin,predict(lm2))

anova(lm1,lm2)

## view it on the original scale
plot(diabetes.raw$SerumInsulin,(predict(lm2) * colsds['SerumInsulin']) + colmeans['SerumInsulin'])

lm3 <- lm(SerumInsulin ~ Age + PlasmaGlucose + BMI, data=diabetes)
summary(lm3)
plot(diabetes$SerumInsulin,predict(lm3))

anova(lm2,lm3)

## what about without Age? Does Age matter?
lm4 <- lm(SerumInsulin ~ PlasmaGlucose + BMI, data=diabetes)
anova(lm4,lm3)  ## nope

#################################################
## dealing with non-linearity in predictors
library(MASS)
data(Boston)
## predict Boston suburb median value of owner occupied homes from % low-income families
lm.fit <- lm(medv ~ lstat, data=Boston)
plot(lm.fit)
plot(predict(lm.fit), rstudent(lm.fit)) ##non-linearity

## what is the residual plot trying to tell us?
plot(Boston$lstat,Boston$medv) ## aha! a non-linear relationship

## transform the predictor -- take both lstat and the square root of lstat
lm.fit2 <- lm(medv ~ lstat+I(sqrt(lstat)), data=Boston)
summary(lm.fit2)
plot(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2)) ##non-linearity taken care of

## and finally, put a p-value on how much better this fit is. (a lot)
anova(lm.fit,lm.fit2)

#################################################
## logistic -- predict diabetes
logfit1 <- glm(Diabetes ~ Age,data=diabetes,family='binomial')
summary(logfit1)
ta <- table(diabetes$Diabetes,predict(logfit1,type='response') > .5)
print(ta)
(ta[1,1] + ta[2,2]) / nrow(diabetes) ## so 71% accuracy

## but! how does that compare to just guessing "no" (the most frequent label) for every instance?
sum(!diabetes$Diabetes) / nrow(diabetes)  ## only 5% better than guessing "no"

## compare to the results of a logistic regression classifier that only knows
## the background frequency of diabetes
logfit0 <- glm(Diabetes ~ 1, data=diabetes,family='binomial')
summary(logfit0)
ta <- table(diabetes$Diabetes, predict(logfit0, type='response') > .5)
print(ta)
(ta[1,1]) / nrow(diabetes)

## add in family history
logfit2 <- glm(Diabetes ~ Age + Fam,data=diabetes,family='binomial')
summary(logfit2)
ta <- table(diabetes$Diabetes,predict(logfit2,type='response') > .5)
print(ta)
(ta[1,1] + ta[2,2]) / nrow(diabetes) ## so 70% accuracy... that didn't help...

## add in plasma glucose
logfit3 <- glm(Diabetes ~ Age + Fam + PlasmaGlucose,data=diabetes,family='binomial')
summary(logfit3)
ta <- table(diabetes$Diabetes,predict(logfit3,type='response') > .5)
print(ta)
(ta[1,1] + ta[2,2]) / nrow(diabetes) ## so 78% accuracy... getting better....

## remove Family & PlasmaGlucose
logfit4 <- glm(Diabetes ~ Age + PlasmaGlucose,data=diabetes,family='binomial')
summary(logfit4)
ta <- table(diabetes$Diabetes,predict(logfit4,type='response') > .5)
print(ta)
(ta[1,1] + ta[2,2]) / nrow(diabetes) ## so 78% accuracy... the same. Did family not help?

## but how do we compare which model is better? anova doesn't work...
anova(logfit4,logfit3)

#################################################
## There are lots of different ways to imagine comparing the relative quality of two classifiers.
## Here we take an approach based on the receiver operator characteristic curve.
## Taking the area under this curve (AUC) gives us a statistic to measure 
## how well our classifier is working. 
## Why do this instead of continuing to simply compute confusion matrices?
## Because the confusion matrix tests the bias of the logistic model without
## testing the variance, and we want a model that minimizes both bias and variance.
## See section 2.2 of ISLR for a good discussion.
## By measuring bias, we mean seeing how accurate a given model is on a test data set.
## By measuring variance, we mean quantifying how accurate our model would be on a novel dataset (Overfitting)
## By 
#################################################
## ROC Curves and AUC:
install.packages("ROCR")
library("ROCR")
## first need to cross-validate
## Leave-one-out Cross Validated Model for the Age-only model:
diabetes$CV.index <- 1:nrow(diabetes)
for(i in 1:nrow(diabetes)) {
  cat("  ",i," / ",nrow(diabetes),"  \r")
  in.fold <- diabetes$CV.index != i
  glm.fit.cv <- glm(Diabetes ~ Age,data=diabetes[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
  diabetes[!in.fold,'logfit1.prediction'] <- p
}
ta <- table(RealLabel=diabetes$Diabetes, PredictedLabel=diabetes$logfit1.prediction > .5)
(ta[1,1] + ta[2,2]) / nrow(diabetes)

pred <- prediction(diabetes$logfit1.prediction,diabetes$Diabetes)
perf <- performance(pred,measure='tpr',x.measure='fpr')
perf.auc <- performance(pred,measure='auc')
plot(perf)
abline(a=0,b=1,lty=3)
text(.5,.2,paste("AUC:",formatC(perf.auc@y.values[[1]])))

## try a new model with Age, Family History, and PlasmaGlucose
for(i in 1:nrow(diabetes)) {
  cat("  ",i," / ",nrow(diabetes),"  \r")
  in.fold <- diabetes$CV.index != i
  glm.fit.cv <- glm(Diabetes ~ Age + Fam + PlasmaGlucose,data=diabetes[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
  diabetes[!in.fold,'logfit3.prediction'] <- p
}
ta <- table(RealLabel=diabetes$Diabetes, PredictedLabel=diabetes$logfit3.prediction > .5)
(ta[1,1] + ta[2,2]) / nrow(diabetes)

pred.3 <- prediction(diabetes$logfit3.prediction,diabetes$Diabetes)
perf.3 <- performance(pred.3,measure='tpr',x.measure='fpr')
perf.auc.3 <- performance(pred.3,measure='auc')
plot(perf.3)
abline(a=0,b=1,lty=3)
text(.5,.2,paste("AUC:",formatC(perf.auc@y.values[[1]])))

## now compare the two models:
plot_multiple_ROCs <- function(pred.list,cols=palette()) { 
  plot(performance(pred.list[[1]],measure='tpr',x.measure='fpr'),col=cols[1])
  for(i in 2:length(pred.list)) {
    perf <- performance(pred.list[[i]],measure='tpr',x.measure='fpr')
    lines(perf@x.values[[1]],perf@y.values[[1]],col=cols[i])
  }
  abline(a=0,b=1,lty=3)
  aucs <- sapply(pred.list,function(x) { formatC(performance(x,measure='auc')@y.values[[1]]) })
  legend('bottomright',legend=paste(names(pred.list)," AUC=",aucs,sep=""),fill=cols[1:length(cols)])
}
plot_multiple_ROCs(list(Age=pred,AgeFamPlasmaGlucose=pred.3))

## try the model without Fam
for(i in 1:nrow(diabetes)) {
  cat("  ",i," / ",nrow(diabetes),"  \r")
  in.fold <- diabetes$CV.index != i
  glm.fit.cv <- glm(Diabetes ~ Age + PlasmaGlucose,data=diabetes[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
  diabetes[!in.fold,'logfit4.prediction'] <- p
}
ta <- table(RealLabel=diabetes$Diabetes, PredictedLabel=diabetes$logfit4.prediction > .5)
(ta[1,1] + ta[2,2]) / nrow(diabetes)

pred.4 <- prediction(diabetes$logfit4.prediction,diabetes$Diabetes)
plot_multiple_ROCs(list(Age=pred,AgeFamPlasmaGlucose=pred.3,AgePlastmaGlucose=pred.4))

#################################################
## now try again with 10-fold cross-validation
diabetes$CV.index <- rep(1:10,length=nrow(diabetes))
for(i in 1:10) {
  cat("  ",i," / ",10,"  \r")
  in.fold <- diabetes$CV.index != i
  glm.fit.cv <- glm(Diabetes ~ Age,data=diabetes[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
  diabetes[!in.fold,'logfit1.prediction'] <- p
}
ta <- table(RealLabel=diabetes$Diabetes, PredictedLabel=diabetes$logfit1.prediction > .5)
(ta[1,1] + ta[2,2]) / nrow(diabetes)

pred.1 <- prediction(diabetes$logfit1.prediction,diabetes$Diabetes)

for(i in 1:10) {
  cat("  ",i," / ",10,"  \r")
  in.fold <- diabetes$CV.index != i
  glm.fit.cv <- glm(Diabetes ~ Age + PlasmaGlucose + Fam,data=diabetes[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
  diabetes[!in.fold,'logfit1.prediction'] <- p
}
pred.3 <- prediction(diabetes$logfit3.prediction,diabetes$Diabetes)

for(i in 1:10) {
  cat("  ",i," / ",10,"  \r")
  in.fold <- diabetes$CV.index != i
  glm.fit.cv <- glm(Diabetes ~ Age + PlasmaGlucose,data=diabetes[in.fold,],family='binomial')
  p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
  diabetes[!in.fold,'logfit4.prediction'] <- p
}
pred.4 <- prediction(diabetes$logfit4.prediction,diabetes$Diabetes)
plot_multiple_ROCs(list(Age=pred.1,AgeFamPlasmaGlucose=pred.3,AgePlastmaGlucose=pred.4))

#################################################
## compare LOOCV to 10-fold CV
plot_multiple_ROCs(list(AgeLOOCV=pred,Age10CV=pred.1)) ## not bad
## higher bias but lower variance (folds look more like new data sets)

#################################################
## bootstrap estimates for logistic coefficients
boot.fn <- function (data ,index)  return(coef(lm(SerumInsulin ~ PlasmaGlucose ,data=data , subset=index)))
boot.fn(diabetes ,1:392)
set.seed(1)
boot.fn(diabetes ,sample (nrow(diabetes),nrow(diabetes), replace=T))
boot.fn(diabetes ,sample (nrow(diabetes),nrow(diabetes), replace=T))
library(boot)
boot(diabetes,boot.fn,1000)
## compare to the closed form version
summary(lm(SerumInsulin ~ PlasmaGlucose ,data=diabetes))$coef
## these are a little different, but chances are the bootstrap version is 
## more accurate, since the closed form version makes a bunch of assumptions
## that may not hold

#################################################
## bootstrap estimate for correlation between Plasma Glucose Level and Serum
plot(diabetes$SerumInsulin,diabetes$PlasmaGlucose)
COR <- cor(diabetes$SerumInsulin,diabetes$PlasmaGlucose)
COR.closedForm.SE <- sqrt((1-COR^2) / (nrow(diabetes) - 2))

boot.fn <- function(data, index) return(cor(data$SerumInsulin[index],data$PlasmaGlucose[index]))
set.seed(1)
boot(diabetes,boot.fn,1000)

#################################################
## bootstrap estimates for AUC
diabetes$CV.index <- rep(1:10,length=nrow(diabetes))
boot.fn <- function(data, index) {
  diabetes.inner <- data[index,]
  for(i in 1:10) {
    cat("  ",i," / ",10,"  \r")
    in.fold <- diabetes.inner$CV.index != i
    glm.fit.cv <- glm(Diabetes ~ Age + PlasmaGlucose,data=diabetes.inner[in.fold,],family='binomial')
    p <- predict(glm.fit.cv,type='response',newdata=diabetes.inner[!in.fold,])
    diabetes.inner[!in.fold,'prediction'] <- p
  }
  pred <- prediction(diabetes.inner$prediction,diabetes.inner$Diabetes)
  return(performance(pred,measure='auc')@y.values[[1]])
}
set.seed(1)
cv.boot <- boot(diabetes,boot.fn,1000)

cv.aucs <- vector()
for(j in 1:1000) {
  diabetes$CV.index <- sample(rep(1:10,length=nrow(diabetes)))
  for(i in 1:10) {
    cat("  ",i," / ",10,"  \r")
    in.fold <- diabetes$CV.index != i
    glm.fit.cv <- glm(Diabetes ~ Age + PlasmaGlucose,data=diabetes[in.fold,],family='binomial')
    p <- predict(glm.fit.cv,type='response',newdata=diabetes[!in.fold,])
    diabetes[!in.fold,'prediction'] <- p
  }
  pred <- prediction(diabetes$prediction,diabetes$Diabetes)
  cv.aucs[j] <- performance(pred,measure='auc')@y.values[[1]]
}

cv.stderr <- sd(cv.aucs) / sqrt(nrow(diabetes))

plot(density(cv.boot$t[,1]),ylim=c(0,225))
lines(density(cv.aucs),col='red')

