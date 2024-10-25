library(readxl)
library(glmnet)

file_path <- "/Users/eoinhoustoun/Desktop/FYP/Transfers.xlsx"
dat <- read_excel(file_path, sheet = "Complete_dataset")
attach(dat)

dat$log_fee <- log(dat$fee)
dat$overall = scale(dat$overall)
dat$FFI <- log(dat$FFI)
dat$age_sold = scale(dat$age_sold)

N = nrow(dat)
dat = dat[sample(1:N,N,replace=FALSE),]
set.seed(4060)
lmo <- lm(dat$log_fee ~ age_sold + overall + FFI
          + factor(contract_remaining) + factor(team_importance) 
          + factor(position_category) + factor(cont_new)
          + factor(league_left) + factor(league_joined), 
          data = dat)

summary_lmo <- summary(lmo)
summary_lmo
hist(dat$log_fee,xlim=c(-2,5))

standardized_residuals_using_scale <- scale(lmo$residuals)

hist(standardized_residuals_using_scale, 
     breaks = 20, 
     col = "lightblue", 
     main = "Histogram and Q-Q Plot of Standardized Residuals", 
     xlab = "Standardized Residuals", 
     xlim = c(-4, 3))
# Q-Q plot without new plotting window
qqnorm(standardized_residuals_using_scale, pch = 3, col = "red" ,xlim=c(-4,3))
qqline(standardized_residuals_using_scale, col = "black", lwd = 2, add = TRUE, xlim=c(-4,3))

# Round the coefficients to three decimal places
summary_lmo$coefficients <- round(summary_lmo$coefficients, 3)
summary_lmo$coefficients

y = dat$log_fee
x <- data.frame(
  age_sold = dat$age_sold,
  overall = dat$overall,
  contract_remaining = factor(dat$contract_remaining),
  team_importance = factor(dat$team_importance),
  position_category = factor(dat$position_category),
  league_left = factor(dat$league_left),
  league_joined = factor(dat$league_joined),
  FFI = dat$FFI,
  cont_new = factor(dat$cont_new)
)
#############################################
############# 0.) Split-sample #############
#############################################
set.seed(4060)
i.train = sample(c(1:N),round(.67*N))
o = lm(y~., data=x, subset=i.train)
#Train rmse
yh = o$fitted.values
fit.rmse = sqrt(mean((yh-y[i.train])^2))
fit.rmse

#Test rmse
yp = predict(o, newdata=x[-i.train,])
rmse.test = sqrt(mean((yp-y[-i.train])^2))
rmse.test

#############################################
############# 1.) Bootstrapping #############
#############################################

N = nrow(x)
B = 1000
fit.b = rmse.b = numeric(B)
set.seed(4060)

for(b in 1:B){		
  i.b = sample(1:N, N, replace=TRUE)
  
  x.b = x[i.b,]
  x.test = x[-i.b,] # OOB predictor values
  
  y.b = y[i.b]
  y.test = y[-i.b] # OOB outcome values
  
  lmb = lm(y.b~.,data=x.b)
  
  yh = lmb$fitted.values
  fit.b[b] = sqrt(mean((yh-y.b)^2))
  
  yp = predict(lmb, newdata=x.test)
  rmse.b[b] = sqrt(mean((yp-y.test)^2))
}

mean(fit.b)
mean(rmse.b)
hist(rmse.b)
boxplot(fit.b, rmse.b,main=("1000 Bootstrap resamples"), names=c("Train (IN-Bag)", "Test (Out-Of-Bag)"), col=c("red", "blue"),ylab='rmse')

# # # # # # # # # # # 
#632 method
# # # # # # # # # # # 
.632*(mean(rmse.b)) +.368*(mean(fit.b))
six32 = .632*(rmse.b) +.368*(fit.b)
boxplot(six32, rmse.b)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Testing different B from 100 -> 1000
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
all_rmse_b <- list()

for(B in seq(100, 1000, by=100)){
  fit.b = rmse.b = numeric(B)
  
  for(b in 1:B){		
    i.b = sample(1:N, N, replace=TRUE)
    
    x.b = x[i.b,]
    x.test = x[-i.b,] # OOB predictor values
    
    y.b = y[i.b]
    y.test = y[-i.b] # OOB outcome values
    
    lmb = lm(y.b~., data=x.b)
    
    yh = lmb$fitted.values
    fit.b[b] = sqrt(mean((yh-y.b)^2))
    
    yp = predict(lmb, newdata=x.test)
    rmse.b[b] = sqrt(mean((yp-y.test)^2))
  }
  
  all_rmse_b[[as.character(B)]] <- rmse.b
}
# Calculate mean rmse for each B
mean_rmse_b <- sapply(all_rmse_b, mean)
se_rmse_b <- sapply(all_rmse_b, function(x) sd(x) / sqrt(length(x)))
x_values <- seq(100, 1000, by=100)
par(mfrow=c(1, 1))
plot(x_values, mean_rmse_b, type="b", pch=19, xlab="B", ylab="Mean rmse", 
     main="Mean rmse for different values of B", ylim=c(0.72, 0.74), 
     yaxp=c(0.72, 0.74, 1))
arrows(x_values, mean_rmse_b - se_rmse_b, x_values, mean_rmse_b + se_rmse_b, 
       angle=90, code=3, length=0.05, col="black")

par(mfrow=c(3, 4)) 
for(B in seq(100, 1000, by=100)){
  hist(all_rmse_b[[as.character(B)]], 
       main=paste("B =", B), 
       xlim=c(.65,.8), 
       breaks=8, 
       xlab="rmse", 
       ylab="Density",
       cex.axis=1.2,  
       cex.lab=1.0, freq=F, ylim=c(0, 20))   
  
  mean_val <- mean(all_rmse_b[[as.character(B)]])
  sd_val <- sd(all_rmse_b[[as.character(B)]])
  
  # Add a normal distribution line
  curve(dnorm(x, mean=mean_val, sd=sd_val), 
        col="blue", lwd=2, add=TRUE, xlim=c(.65, .8), ylim=c(0, 20))
}

#############################################
############# 2.) LOO CV ############## # # # 
#############################################
set.seed(120437004)

K = N
sss = fit.loo = rmse.loo = r_squared.loo = numeric(K)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)

for(k in 1:K){		
  i.train = which(folds!=k)
  o = lm(y~., data=x, subset=i.train)
  
  #Train rmse
  yh = o$fitted.values
  fit.loo[k] = sqrt(mean((yh-y[i.train])^2))
  
  #Test rmse
  yp = predict(o, newdata=x[-i.train,])
  rmse.loo[k] = sqrt(mean((yp-y[-i.train])^2))

}
mean(fit.loo)
mean(rmse.loo)
par(mfrow=c(1,1))
hist(rmse.loo)


#############################################
############# 3.) K-Fold CV #############
#############################################
# 2 fold
K = 2
l = fit.k2 = rmse.k2 = r_squared.k2=r_squared.my= numeric(K)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)


for(k in 1:K){		
  i.train = which(folds!=k)
  o = lm(y~., data=x, subset=i.train)
  #Train rmse
  yh = o$fitted.values
  fit.k2[k] = sqrt(mean((yh-y[i.train])^2))
  #Test rmse
  yp = predict(o, newdata=x[-i.train,])
  rmse.k2[k] = sqrt(mean((yp-y[-i.train])^2))
  
  
}
mean(fit.k2)
mean(rmse.k2)


# 5 fold
K = 5
fit.k5 = rmse.k5 = numeric(K)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)

for(k in 1:K){		
  i.train = which(folds!=k)
  o = lm(y~., data=x, subset=i.train)
  yh = o$fitted.values
 # train RMSE
  fit.k5[k] = sqrt(mean((yh-y[i.train])^2))
  
  # test RMSE
  yp = predict(o, newdata=x[-i.train,])
  rmse.k5[k] = sqrt(mean((yp-y[-i.train])^2))
}

mean(fit.k5)
mean(rmse.k5)

# 10-fold
K = 10
fit.k10 = rmse.k10 = numeric(K)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)

for(k in 1:K){		
  i.train = which(folds!=k)
  o = lm(y~., data=x, subset=i.train)
  yh = o$fitted.values
  #train rmse
  fit.k10[k] = sqrt(mean((yh-y[i.train])^2))
  #test rmse
  
  yp = predict(o, newdata=x[-i.train,])
  rmse.k10[k] = sqrt(mean((yp-y[-i.train])^2))
}
mean(fit.k10)
mean(rmse.k10)

# k= 10 -> 1316
K_val <- c(10,20,30,40,50,60,70,80,90, 100, 500, 1000,1316)
K_max <- length(K_val)
K=1316
rmse_matrix <- matrix(NA, nrow = K, ncol =K_max )
colnames(rmse_matrix) <- as.character(K_val)

set.seed(4061)
for (i in 1:K_max) {
  K <- K_val[i]
  N = nrow(x) 
  folds = cut(1:N, K, labels=FALSE)
  for (k in 1:K) {		
    i.train = which(folds != k)
    o = lm(y ~ ., data = x, subset = i.train)
    yh = o$fitted.values
    
    # Calculate training error
    #rmse_matrix[k, i] = sqrt(mean((yh - y[i.train])^2))
    
    # Calculate prediction error
    yp = predict(o, newdata = x[-i.train,])
    rmse_matrix[k, i] <- sqrt(mean((y[-i.train] - yp)^2))
  }
}
rmse_matrix
par(mfrow=c(1, 1))

boxplot(rmse_matrix[,], col=c('yellow'),ylim=c(0,1.5), ylab=c("Test rmse"), xlab=("Value of K"), main=c('Variability of rmse for different K'))

#############################################
############# 4.) Repeated K-Fold CV #############
#############################################

#200 resamples
R = 20
K = 10
fit.5 = rmse.5 = r_squared.my= r_squared.k2= numeric(K*R)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)
for(r in 1:R){
  dat = dat[sample(1:N,N,replace=FALSE),]
  y = dat$log_fee
  x <- data.frame(
    age_sold = dat$age_sold,
    overall = dat$overall,
    contract_remaining = factor(dat$contract_remaining),
    team_importance = factor(dat$team_importance),
    position_category = factor(dat$position_category),
    league_left = factor(dat$league_left),
    league_joined = factor(dat$league_joined),
    FFI = dat$FFI,
    cont_new = factor(dat$cont_new)
  )
  for(k in 1:K){		
    i.train = which(folds!=k)
    o = lm(y~., data=x, subset=i.train)
    #Train rmse
    yh = o$fitted.values
    fit.5[k+(r-1)*K] = sqrt(mean((yh-y[i.train])^2))
    
    #Test rmse
    yp = predict(o, newdata=x[-i.train,])
    rmse.5[k+(r-1)*K] = sqrt(mean((yp-y[-i.train])^2))
  }
}
mean(fit.5)
mean(rmse.5)

# # # # # # # # # # # 
# 999 resamples # # # 
# # # # # # # # # # # 
R = 333
K = 3
fit.10 = rmse.10 = numeric(K*R)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)

for(r in 1:R){
  dat = dat[sample(1:N,N,replace=FALSE),]
  y = dat$log_fee
  x <- data.frame(
    age_sold = dat$age_sold,
    overall = dat$overall,
    contract_remaining = factor(dat$contract_remaining),
    team_importance = factor(dat$team_importance),
    position_category = factor(dat$position_category),
    league_left = factor(dat$league_left),
    league_joined = factor(dat$league_joined),
    FFI = dat$FFI,
    cont_new = factor(dat$cont_new)
  )
  
  for(k in 1:K){		
    i.train = which(folds!=k)
    o = lm(y~., data=x, subset=i.train)
    yh = o$fitted.values
    fit.10[k+(r-1)*K] = sqrt(mean((yh-y[i.train])^2))
    yp = predict(o, newdata=x[-i.train,])
    rmse.10[k+(r-1)*K] = sqrt(mean((yp-y[-i.train])^2))
  }
}
mean(fit.10)
mean(rmse.10)

#############################################
############# 5.) Nested K-Fold CV ##########
################.     LASSO        ##########
#############################################
library(glmnet)
dat
n = nrow(dat)
R = 333
K = 3
fit = rmse = numeric(K*R)
fit.l = rmse.l = numeric(K*R)
folds = cut(1:n, K, labels=FALSE)
set.seed(4060)
reg_dat=cbind(y,x)
for(r in 1:R){
  # shuffle initial dataset
  reg_dat = reg_dat[sample(1:n,n,replace=FALSE),]
  y = reg_dat$y
  xm = model.matrix(y ~. +0, data=reg_dat)

  
  for(k in 1:K){		
    i.train = which(folds!=k)
    # Lambda = CV    
    cv.l = cv.glmnet(xm[i.train,],y[i.train],grouped=FALSE)
    lasso = glmnet(xm[i.train,],y[i.train],lambda=cv.l$lambda.min,nfolds=10)
    yh.l = predict(lasso,newx=xm[i.train,])
    fit.l[k+(r-1)*K] = sqrt(mean((yh.l-y[i.train])^2))
    yp.l = predict(lasso,newx=xm[-i.train,])
    rmse.l[k+(r-1)*K] = sqrt(mean((yp.l-y[-i.train])^2))
    
  }
}

lasso$a0
lasso$beta
mean(fit.l)
mean(rmse.l)

#############################################
############# 6.) Monte-Carlo CV #############
#############################################
N = nrow(x)
y=dat$log_fee
M = 1000
fit.mccv = rmse.mccv = numeric(M)

set.seed(4060)
for(m in 1:M){		
  i.train = sample(c(1:N),round(.7*N))
  
  x.train = x[i.train,]
  x.test = x[-i.train,]
  y.train = y[i.train]
  y.test = y[-i.train]
  
  o = lm(y.train~.,data=x.train)
  
  yh = o$fitted.values
  fit.mccv[m] = sqrt(mean((yh-y.train)^2))
  
  yp = predict(o, newdata=x.test)
  rmse.mccv[m] = sqrt(mean((yp-y.test)^2))
}

mean(fit.mccv)
mean(rmse.mccv)

#############################################
############# RESULTS #############
#############################################

#boot
round(mean(fit.b),2)
round(mean(rmse.b),2)
sd(rmse.b)

#rkcv
round(mean(fit.5),2)
round(mean(rmse.5),2)
sd(fit.5)

#split sample
round(mean(fit.rmse),2)
round(mean(rmse.test),2)
sd(rmse.test)

#MCCV
round(mean(fit.mccv),2)
round(mean(rmse.mccv),2)
sd(rmse.mccv)

#LOOCV
round(mean(fit.loo),2)
round(mean(rmse.loo),2)
sd(rmse.loo)

#10 fold
round(mean(fit.k10),2)
round(mean(rmse.k10),2)
sd(rmse.k10)

boxplot(rmse.b,rmse.10,rmse.l,main=("Reguralisation"), 
        names=c("Bootstrap (LM)","RK-fold CV (LM)","Nested CV (LASSO)"), 
        col=c("red", "yellow",'blue'),
        xlab=('Internal Validation Method (Model)'), ylab = ('Test rmse'))

par(mfrow=c(1,1))
boxplot(fit.b, fit.k10, fit.10, fit.mccv,
        names = c("Bootstrap", 
                  "K-fold CV", 
                  "R.K-fold CV",
                  "MCCV"),
        main = "RMSE - Train Sample",
        col = c('red3', "blue", "yellow", "green"),
        cex.main = 1.5,  
        cex.axis = 1.2,  
        cex.names = 1.2) 



boxplot(rmse.b, rmse.10, rmse.mccv,
        names = c("Bootstrap", "R.K-fold CV", "MC CV"),
        main = "rmse - Test set",
        col = c('red3', "blue", "yellow"),
        cex.main = 1.5,  
        cex.axis = 1.2, 
        cex.names = 1.2,
        ylab = "rmse",  
        xlab = "Internal Validation Method") 
boxplot(fit.b, rmse.b, fit.10, rmse.10, fit.mccv, rmse.mccv,
        names = c("Bootstrap In-Bag","Bootstrap OOB","R.K-fold CV Fit", "R.K-fold CV Test","MC CV Fit", "MC CV Test"),
        main = "rmse Box plot",
        col = c('red3','red3', "blue","blue", "yellow","yellow"),
        
        cex.main = 1.5,  
        cex.axis = 1.0,  
        cex.names = 1.0, 
        ylab = "rmse",   
        xlab = "Internal Validation Method") 

#############################################
############# Random Forest #############
#############################################
library(randomForest)

N <- nrow(x) 
K <- 10
fit.rf <- rmse.rf <- numeric(K)
folds <- cut(seq(1, N), breaks=K, labels=FALSE)
set.seed(4060)

for (k in 1:K) {
  i.train <- which(folds != k)
  i.test <- which(folds == k)
  o <- randomForest(y ~ ., data = x, subset = i.train)
  yh <- predict(o, x[i.train, ])
  fit.rf[k] <- sqrt(mean((yh - y[i.train])^2))
  yp <- predict(o, newdata = x[-i.train, ])
  rmse.rf[k] <- sqrt(mean((yp - y[-i.train])^2))
}

mean(fit.rf)
mean(rmse.rf)


#############################################
############# Neural Network #############
#############################################
test = x
myrecode <- function(x){
  # function recoding levels into numerical values
  if(is.factor(x)){
    levels(x)
    return(as.numeric(x)) 
  } else {
    return(x)
  }
}
myscale <- function(x){
  # function applying normalization to [0,1] scale
  minx = min(x,na.rm=TRUE)
  maxx = max(x,na.rm=TRUE)
  return((x-minx)/(maxx-minx))
}
datss = data.frame(lapply(test,myrecode))
datss = data.frame(lapply(datss,myscale))
datss = cbind(dat$log_fee,datss)

names(datss)[1] <- "log_fee"
library(nnet)
datss
K = 10
fit.k10 = rmse.k10 = numeric(K)
r_squared_k10 <- numeric(K)
folds = cut(1:N, K, labels=FALSE)
set.seed(4060)
for(k in 1:K){		
  i.train = which(folds!=k)
  nno.ss = nnet(log_fee~., data=datss, subset=i.train, size=10, decay=c(0.1), linout=1)
  fit.ss = predict(nno.ss, newdata=datss[i.train,])
  fit.k10[k] = sqrt(mean((datss$log_fee[i.train]-fit.ss)^2))
  pred.ss = predict(nno.ss, newdata=datss[-i.train,])
  rmse.k10[k] = sqrt(mean((datss$log_fee[-i.train]-pred.ss)^2))
  
}
mean(fit.k10)
mean(rmse.k10)
library(NeuralNetTools)
plotnet(nno.ss)
boxplot(fit.k10,rmse.k10)
sr = scale(residuals)
plot(sr)
boxplot(rmse.b,rmse.k10)


################################################
##### Bootstrap Coefficients ###################
################################################
################################################
set.seed(4061)
B = 100  
n = nrow(x) 
p = length(coef(lm(log_fee ~ age_sold + overall + FFI
                   + factor(contract_remaining) + factor(team_importance) 
                   + factor(position_category) + factor(cont_new)
                   + factor(league_left) + factor(league_joined), 
                   data = dat)))  
bcoefs = matrix(NA, nr = B, nc = p)

for (b in 1:B) {
  ib = sample(1:n, size = n, replace = TRUE) 
  xbdata = x[ib, ]
  ybdata = y[ib ]
  blm = lm(ybdata ~ ., data = xbdata) 
  bcoefs[b, ] = coef(blm)  
}
colnames(bcoefs) <- names(coef(lm(log_fee ~ age_sold + overall + FFI
                                  + factor(contract_remaining) + factor(team_importance) 
                                  + factor(position_category) + factor(cont_new)
                                  + factor(league_left) + factor(league_joined), 
                                  data = dat)))

coefs_mean = apply(bcoefs, 2, mean)
coefs_se = apply(bcoefs, 2, sd)

# Plots for each coefficient:
par(mfrow=c(3, 4)) 
for(j in 1:12) {
  hist(bcoefs[, j], 
       main=names(coef(blm))[j], 
       breaks=5, 
       freq=FALSE, 
       cex.main=1.5,  
       cex.axis=1.4,   
       cex.lab=1.5,    
       xlab=bquote("Bootstrapped " ~ beta[.(j-1)]))  
  lower_ci <- quantile(bcoefs[, j], probs = 0.025)
  upper_ci <- quantile(bcoefs[, j], probs = 0.975)
  
  abline(v = lower_ci, col = "red", lwd = 2, lty = 2)  # Lower CI line
  abline(v = upper_ci, col = "red", lwd = 2, lty = 2)  # Upper CI line
}

par(mfrow=c(3, 4))  

for(j in 13:24) {
  hist(bcoefs[, j], 
       main=names(coef(blm))[j], 
       breaks=6, 
       freq=FALSE, 
       cex.main=1.5,
       cex.axis=1.4,  
       cex.lab=1.5,   
       xlab=bquote("Bootstrapped " ~ beta[.(j-1)]))  
  lower_ci <- quantile(bcoefs[, j], probs = 0.025)
  upper_ci <- quantile(bcoefs[, j], probs = 0.975)
  
  # Add lines for the lower and upper confidence intervals
  abline(v = lower_ci, col = "red", lwd = 2, lty = 2)  # Lower CI line
  abline(v = upper_ci, col = "red", lwd = 2, lty = 2)  # Upper CI line
}


################################################
##### Model Comparison ###################
################################################
################################################

library(readxl)
library(glmnet)
library(randomForest)
library(tree)
library(gbm)



# Bootstrap

boot_model =  function(B, model_type, y, x){
  fit=rmse=train_rsquared=test_rsquared = numeric(B)
  N = nrow(x)
  set.seed(120437004)
  
  for(b in 1:B){		
    i.b = sample(1:N, N, replace=TRUE)
    x.b = x[i.b,]
    x.test = x[-i.b,] # OOB predictor values
    
    y.b = y[i.b]
    y.test = y[-i.b] # OOB outcome values
    
    ####### model
    if (model_type == "lm") {
      my_model <- lm(y.b ~ ., data = x.b)
    } else if (model_type == "randomForest") {
      my_model <- randomForest(y.b ~ ., data = x.b)
    } else if (model_type == "tree") {
      my_model <- tree(y.b ~ ., data = x.b)
    } else if (model_type == "gbm") {
      my_model <- gbm(y.b ~ ., data = x.b, distribution = "gaussian")
    } else {
      stop("Unsupported model type")
    }
    #Train
    yh <- predict(my_model, x.b)
    fit[b] <- sqrt(mean((yh - y.b)^2))
    SSres_train = sum((y.b - yh)^2)
    SStot_train = sum((y.b - mean(y.b))^2)
    train_rsquared[b] = 1 - (SSres_train / SStot_train)
    
    #Test
    yp = predict(my_model, newdata=x.test)
    rmse[b] = sqrt(mean((yp-y.test)^2))
    SSres_test = sum((y.test - yp)^2)
    SStot_test = sum((y.test - mean(y.test))^2)
    test_rsquared[b] = 1 - (SSres_test / SStot_test)
    
  }
  return(list(fit = fit, rmse = rmse, train_rsquared = train_rsquared, test_rsquared = test_rsquared))
  
}

lm =  boot_model(100, "lm", y, x)
tree =  boot_model(100, "tree", y, x)
rf =  boot_model(100, "randomForest", y, x)
gbm =  boot_model(100, "gbm", y, x)

lm.mean = sapply(lm,mean)
names(lm.mean)
sapply(tree,mean)
sapply(rf,mean)
sapply(gbm,mean)


rbind(sapply(lm,mean),sapply(tree,mean),sapply(rf,mean),sapply(gbm,mean))
library(ggplot2)
data_plot <- data.frame(
  Model = rep(c("Decision Tree", "GBM", "Random Forest", "LM"), times = 2), 
  MetricType = c(rep("Train", 4), rep("Test", 4)),
  Value = c(
    mean(tree$fit), mean(gbm$fit), mean(rf$fit), mean(lm$fit),
    mean(tree$rmse), mean(gbm$rmse),mean(rf$rmse), mean(lm$rmse)
  )
)

data_plot$Model <- factor(data_plot$Model, levels = c("Decision Tree", "GBM", "Random Forest", "LM"))
data_plot$MetricType <- factor(data_plot$MetricType, levels = c("Train", "Test"))

ggplot(data_plot, aes(x = Model, y = Value, shape = MetricType, color = Model)) + 
  geom_point(size = 4) + # Adjust size as needed
  scale_shape_manual(values = c(15, 17), name = "Boostrap sample:") + # 15: square, 17: triangle, adjust legend title for shapes
  scale_color_discrete(name = "Model:") + # Adjust legend title for colors
  theme_minimal() +
  labs(title = "Comparison of Model Performance",
       x = "Model",
       y = "RMSE") +
  theme(legend.position = "bottom",
        text = element_text(size = 12), # Adjusts overall text size
        plot.title = element_text(size = 16, face = "bold"), # Adjusts plot title size
        axis.title = element_text(size = 14), # Adjusts axis titles size
        axis.text = element_text(size = 12), # Adjusts axis text size
        legend.title = element_text(size = 12), # Adjusts legend title size
        legend.text = element_text(size = 10)) # Adjusts legend text size


data_plot <- data.frame(
  Model = rep(c("Decision Tree", "GBM", "Random Forest", "LM"), times = 2), # Four models, repeated twice for two metrics
  MetricType = c(rep("Train", 4), rep("Test", 4)), # Identifying metric type
  Value = c(
    mean(tree$train_rsquared), mean(gbm$train_rsquared),mean(rf$train_rsquared),mean(lm$train_rsquared), 
    mean(tree$test_rsquared), mean(gbm$test_rsquared),mean(rf$test_rsquared), mean(lm$test_rsquared) 
  )
)

data_plot$Model <- factor(data_plot$Model, levels = c("Decision Tree", "GBM", "Random Forest", "LM"))
data_plot$MetricType <- factor(data_plot$MetricType, levels = c("Train", "Test"))


library(ggplot2)

ggplot(data_plot, aes(x = Model, y = Value, shape = MetricType, color = Model)) + 
  geom_point(size = 4) + 
  scale_shape_manual(values = c(15, 17)) + 
  theme_minimal() +
  labs(title = "Comparison of Model Performance",
       x = "Model",
       y = expression(R^2), 
       shape = "Boostrap sample:",
       color = "Model:") +
  theme(legend.position = "bottom",
        text = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10)) 


### comparison to baseline
rmse = mean(lm$rmse)
std_dev = sd(y)
# Compare RMSE and Standard Deviation
cat("RMSE:", mean(lm$rmse), "\n")
cat("Standard Deviation:", std_dev, "\n")

# The RMSE is 43.85% smaller than the standard deviation of the log transfer fee
#This suggests that, on average, the errors the lm model make are significantly smaller 
#than the variability in the dataset. In practical terms, this indicates that 
#the model's predictions are considerably more consistent than a naive guess based on the 
#mean or median of the target variable




### END




