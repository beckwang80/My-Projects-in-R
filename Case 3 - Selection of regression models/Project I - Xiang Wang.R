library(lars)                              # The data is available in R package “lars” 
library(leaps)                             # The package "leaps" was used to perform best subset selection by function regsubsets() 
library(glmnet)                            # The package "glmnet" was used to perform Ridge Regression and Lasso
data(diabetes)
data.all <- data.frame(cbind(diabetes$x, y=diabetes$y))
# Partition the patients into two groups: training (75%) and test (25%)
n <- dim(data.all)[1]                      # sample size = 442
set.seed(1306)                             # set random number generator seed to enable repeatability of results
test <- sample(n, round(n/4))              # randomly sample 25% test
data.train <- data.all[-test,]
data.test <- data.all[test,]
x <- model.matrix(y~.,data=data.all)[,-1]  # define predictor matrix excl intercept col of 1s 
x.train <- x[-test,]                       # define training predictor matrix
x.test <- x[test,]                         # define test predictor matrix
y <- data.all$y                            # define response variable
y.train <- y[-test]                        # define training response variable
y.test <- y[test]                          # define test response variable
n.train <- dim(data.train)[1]              # training sample size
n.test <- dim(data.test)[1]                # test sample size

# Model 1: Least squares regression model using all ten predictors 
OLS <- lm(y~., data.train); summary(OLS)   # fit the least squres model using training data
pred.OLS <- predict(OLS, data.test)        # predict the response using test data
mean((y.test-pred.OLS)^2)                  # calculate the mean prediction error (MSE)
sd((y.test-pred.OLS)^2)/sqrt(n.test)       # calculate the standard error of MSE
par(mfrow=c(2,2)); plot(OLS)               # Check assumptions using residual and Q-Q plots

# Model 2: Best subset selection using BIC to select the number of predictors
BIC <- regsubsets(y~., data=data.train, nvmax=10) # fit the best subset model using training data
summary(BIC)                                      # summarize the best subset model 
which.min(summary(BIC)$bic)                       # determine the number of variables with the lowest BIC value
coef(BIC, id=6)                                   # show the coefficient estimates for the model above

# Create the function to use predict() with regsubsets 
predict.regsubsets=function(object,newdata,id,...){
   form=as.formula(object$call[[2]])
   mat=model.matrix(form,newdata)
   coefi=coef(object,id=id)
   xvars=names(coefi)
   mat[,xvars]%*%coefi
}

pred.BIC <- predict(BIC, data.test, id=6)         # predict the response using test data
mean((y.test-pred.BIC)^2)                         # calculate the mean prediction error (MSE)
sd((y.test-pred.BIC)^2)/sqrt(n.test)              # calculate the standard error of MSE
plot(BIC, scale='bic')

# Model 3: Best subset selection using 10-fold cross-validation to select the number of predictors 
k=10; set.seed(1306)                                              # set k fold and random seed               
folds <- sample(1:k, nrow(data.train), replace=TRUE)              # define each fold
cv.errors <- matrix(NA, k, 10, dimnames=list(NULL, paste(1:10)))  # define the matrix for CV errors
for(j in 1:k){
   best=regsubsets(y~., data=data.train[folds!=j,], nvmax=10)
   for(i in 1:10) {
     pred=predict(best,data.train[folds==j,], id=i)
     cv.errors[j, i]=mean((data.train$y[folds==j]-pred)^2)
   }
}
mean.cv.errors <- apply(cv.errors, 2, mean); mean.cv.errors      # calculate the mean CV error
which.min(mean.cv.errors) # 6                                    # determine the number of variables with the lowest CV error
best <- regsubsets(y~., data=data.train, nvmax=10)               # fit the best subset model using training data
coef(best, 6)                                                    # show the coefficient estimates for the model above
pred.best <- predict(best,data.test, 6)                          # predict the response using test data
mean((y.test-pred.best)^2)                                       # calculate the mean prediction error (MSE)
sd((y.test-pred.best)^2)/sqrt(n.test)                            # calculate the standard error of MSE
plot(mean.cv.errors, type='b')

# Model 4: Ridge regression using 10-fold cross-validation to select the largest value of λ such that the cross-validation error
# is within 1 SE of the minimum
set.seed(1306) 
cv.out <- cv.glmnet(x.train, y.train, alpha=0, nfolds=10)
bestλ <- cv.out$lambda.1se; bestλ
ridge <- glmnet(x.train, y.train,alpha=0) 
pred.ridge <- predict(ridge, s=bestλ, newx=x.test)
predict(ridge, type="coefficients", s=bestλ)[1:11, ]
mean((y.test-pred.ridge)^2)
sd((y.test-pred.ridge)^2)/sqrt(n.test)  
plot(cv.out)

# Model 5: Lasso regression using 10-fold cross-validation to select the largest value of λ such that the cross-validation error 
# is within 1 SE of the minimum
set.seed(1306)
cv.out <- cv.glmnet(x.train, y.train, alpha=1, nfolds=10)
bestλ <- cv.out$lambda.1se; bestλ
lasso <- glmnet(x.train, y.train, alpha =1)
pred.lasso <- predict(lasso, s=bestλ, newx=x.test, exact=T) 
mean((y.test-pred.lasso)^2)
sd((y.test-pred.lasso)^2)/sqrt(n.test) 
plot(cv.out)
