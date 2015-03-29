source("header.R")

## ============================================================================
## DATA PREPARATION
## ============================================================================
diabetes <- read.csv(paste0(datadir, "/pima-indians-diabetes.data.txt"))

colnames(diabetes) <- c("NumPreg","PlasmaGlucose","BP","SkinFoldThickness",
                        "SerumInsulin","BMI","Fam","Age","Diabetes")

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


## ============================================================================
## DATA OVERVIEW
## ============================================================================


## ============================================================================
## (1) LDA
## James et al, p. 138, 161
##
## Why LDA?
## an alternative approach to logistic regression
## models distribution of X separately in each class Y
## (logistics: models P(Y|X) directly, using logistic function)
##      is more stable in certain situations (p 138)
##
## ASSUMPTIONS of LDA
## (use Fig on p. 150 to explain)
## predictors X = (X1, X2, ..., Xp) drawn from multivariate Gaussian
## with class-specific mean vector, and COMMON covariance
## ============================================================================
library(MASS)

## (missclasiffcation function)
miss <- function(table1){
  za <- (sum(table1)-sum(diag(table1)))/sum(table1)
  return(za)
}

## predictors: Age
(lda.fit <- lda(Diabetes ~ Age, data=diabetes))
plot(lda.fit)

lda.pred <- predict(lda.fit)
(ta <- table(diabetes$Diabetes, lda.pred$class))
1-miss(ta)

## predictors: Age + Fam
(lda.fit <- lda(Diabetes ~ Age + Fam, data=diabetes))
plot(lda.fit)

lda.pred <- predict(lda.fit, data=diabetes[,7:8])
(ta <- table(diabetes$Diabetes, lda.pred$class))
1-miss(ta)

## predictors: Age + Plasma Glucose
(lda.fit <- lda(Diabetes ~ Age + PlasmaGlucose, data=diabetes))
plot(lda.fit)

lda.pred <- predict(lda.fit)
(ta <- table(diabetes$Diabetes, lda.pred$class))
1-miss(ta)

## 
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

## probability of NO diabetes
lda.pred$posterior[1:20,1]
lda.pred$class[1:20]

##
sum(lda.pred$posterior[,1]>0.9)


## =========================
## LDA: training and testing
## =========================

## obtain training (90%) and testing (10%) data
## change proportion as needed
prop <- 0.8

(len <- dim(diabetes)[1])
(split <- round(prop*len))

train <- diabetes[1:split,]
test <- diabetes[(split+1):len,]

dim(train)
dim(test)

## LDA: train and test

## predictors: Age
(lda.fit <- lda(Diabetes ~ Age, data=train))
plot(lda.fit)

lda.pred <- predict(lda.fit, newdata=test)
(ta <- table(test$Diabetes, lda.pred$class))
1-miss(ta)

## predictors: Age + Fam
(lda.fit <- lda(Diabetes ~ Age + Fam, data=train))
plot(lda.fit)

lda.pred <- predict(lda.fit, newdata=test)
(ta <- table(test$Diabetes, lda.pred$class))
1-miss(ta)

## predictors: Age + Plasma Glucose
(lda.fit <- lda(Diabetes ~ Age + PlasmaGlucose, data=train))
plot(lda.fit)

lda.pred <- predict(lda.fit, newdata=test)
(ta <- table(test$Diabetes, lda.pred$class))
1-miss(ta)

## 
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)


##
sum(lda.pred$posterior[,1]>0.9)




## ============================================================================
## (2) QDA
## James et al, p. 149, 163
##
## ASSUMPTIONS of QDA
## like LDA, except each class has unique covariance matrix (spread)
## means it's more complicated - more parameters to estimate
## but also more flexible
## use QDA if have a large training set
## ============================================================================

## predictors: Age
(qda.fit <- qda(Diabetes ~ Age, data=train))

qda.pred <- predict(qda.fit, test)
(ta <- table(test$Diabetes, qda.pred$class))
1-miss(ta)

## predictors: Age + Fam
(qda.fit <- qda(Diabetes ~ Age + Fam, data=train))

qda.pred <- predict(qda.fit, test)
(ta <- table(test$Diabetes, qda.pred$class))
1-miss(ta)

## predictors: Age + Plasma Glucose
(qda.fit <- qda(Diabetes ~ Age + PlasmaGlucose, data=diabetes))

qda.pred <- predict(qda.fit, test)
(ta <- table(test$Diabetes, qda.pred$class))
1-miss(ta)





## ============================================================================
## (3) KNN
## James et al, p. 39, 163
## completely different approach: nonparametric (no parametric assumptions)
## takes closes observations in space
## see Figure on p. 40 in James et al
## ============================================================================
library(class)

train.X <- train[,c(2,8)]
test.X <- test[,c(2,8)]

train.Y <- train$Diabetes
test.Y <- test$Diabetes

set.seed(1423)
knn.pred <- knn(train=train.X, test=test.X, cl=train.Y, k=1)
(ta <- table(knn.pred, test.Y))
1-miss(ta)

knn.pred <- knn(train.X, test.X, train.Y, k=3)
(ta <- table(knn.pred, test.Y))
1-miss(ta)

knn.pred <- knn(train.X, test.X, train.Y, k=5)
(ta <- table(knn.pred, test.Y))
1-miss(ta)

knn.pred <- knn(train.X, test.X, train.Y, k=10)
(ta <- table(knn.pred, test.Y))
1-miss(ta)


## TRAINING ERROR, TESTING ERROR, CV ERROR
tem1 <- tem2 <- err1.train <- err1.cv <- err1.test <- 0
ii <- 50

## test
for(i in 1:ii){
    knn.pred <- knn(train.X, test.X, train.Y, k=i, prob=TRUE)
    ta <- table(knn.pred, test.Y)
    err1.test[i] <- miss(ta)
}
plot(err1.test)

## train
## training error should be =0 when K=1
for(i in 1:ii){
    knn.pred <- knn(train.X, train.X, train.Y, k=i, prob=TRUE)
    ta <- table(knn.pred, train.Y)
    err1.train[i] <- miss(ta)
}
plot(err1.train)

## here: CV

for(i in 1:ii){
    knn.pred <- knn.cv(train.X, train.Y, k=i, prob=TRUE)
    ta <- table(knn.pred, train.Y)
    err1.cv[i] <- miss(ta)
}
plot(err1.cv,type='l')  



## PLOT ALL ERRORS
err1.all <- as.data.frame(c(err1.cv,err1.test,err1.train))
err1.all$index <- as.factor(rep(c(1:ii),3))
err1.all$error <- as.factor(c(rep("cv",length(err1.cv)),rep("test",length(err1.test)),rep("train",length(err1.train))))
names(err1.all) <- c("val","index","error")
#err.all[c(1:10,90:120,190:205,299,300),]

theme_set(theme_gray())
qplot(index,val,data=err1.all,geom="line",group=error,colour=error)+
    scale_x_discrete(breaks=seq(0,50,5))+labs(x="K",y="error value")

theme_set(theme_bw())
p50 <- ggplot(data=err1.all, aes(x=index,y=val)) +
    geom_point(size=2,aes(shape=error))+geom_line(aes(group=error))+
        scale_x_discrete(breaks=seq(0,50,5))+
            labs(x="K",y="error value")

p50


err1.b <- err1.all %>% filter(as.numeric(index)<16)
p15 <- ggplot(data=err1.b, aes(x=index,y=val)) +
    geom_point(size=2,aes(shape=error))+geom_line(aes(group=error))+
        scale_x_discrete(breaks=seq(0,15,1))+
            labs(x="K",y="error value")


p15




