##  Project- Predicting Total Wealth
## Name: Kai Cheng Wu



## ---------------------------------------------------------
## Open Data Set 
data <- read.table('data_tr.txt', head = T)[,-1]
set.seed(613)

names(data)
head(data)
summary(data)

## ---------------------------------------------------------

# Separate out continuous and binary X variables
continuous_X <- subset(data, select = c(ira, nifa, inc, hmort, hval, hequity, educ, age, fsize))
binary_X <- subset(data, select = c(e401, male, twoearn, nohs, hs, smcol, col, marr))
tw <- data$tw


## ---------------------------------------------------------
summary(continuous_X)
summary(binary_X)

## ---------------------------------------------------------
plot(data$inc, data$tw, cex = 0.5)
plot(data$ira, data$tw, cex = 0.5)
plot(data$educ, data$tw, cex = 0.5)
plot(data$nifa, data$tw, cex = 0.5)
plot(data$hmort, data$tw, cex = 0.5)
plot(data$hval, data$tw, cex = 0.5)
plot(data$hequity, data$tw, cex = 0.5)
plot(data$age, data$tw, cex = 0.5)
plot(data$fsize, data$tw, cex = 0.5)


## ---------------------------------------------------------
#implying which variables is more correlates with total wealth
reg <- lm(data_no_outliers$tw ~ data_no_outliers$inc)
plot(data_no_outliers$inc, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$ira)
plot(data_no_outliers$ira, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$educ)
plot(data_no_outliers$educ, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$nifa)
plot(data_no_outliers$nifa, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$age)
plot(data_no_outliers$age, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$hmort)
plot(data_no_outliers$hmort, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$hval)
plot(data_no_outliers$hval, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$hequity)
plot(data_no_outliers$hequity, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data_no_outliers$tw ~ data_no_outliers$fsize)
plot(data_no_outliers$fsize, data_no_outliers$tw, cex = 0.5)
abline(reg)

reg <- lm(data$tw ~ data$age)
summary(reg)

reg <- lm(data$tw ~ data$nifa+data$inc+data$ira+data$hequity)

summary(reg)
#nifa, ira,inc,hequity

## ---------------------------------------------------------
hist(data$tw)
hist(data$inc)

##-----




## ---------------------------------------------------------
# Checks which observations are outliers
outliers <- data$tw > 1000000

# Makes a new dataset without outliers
data_no_outliers <- data[!outliers,]

hist(data_no_outliers$tw)

# Compare the new maximum observation of tw
max(data$tw)
max(data_no_outliers$tw)

# Count number of excluded outliers
sum(outliers)

## Simple model: OLS with all provided variables
## ---------------------------------------------------------
model0 - lm(tw ~ ., data = data_no_outliers)
summary(model0)
#ols_pred <- predict(model0, newdata = data_no_outliers) 
#MSE <- mean((data_no_outliers$tw - ols_pred)**2) 
#MSE
x <- as.matrix(data_no_outliers)
y <- data_no_outliers$tw
percent <- 0.8
train <- sample(1:n < percent * n)
test <- !train

x_train <- x[train, ]
y_train <- y[train]
x_test <- x[test,]
y_test <- y[test]
train_data <- as.data.frame(cbind(y_train = y_train, x_train))
model_0_train <- lm(y_train ~ x_train)
predictions_0 <- predict(model_0_train, newdata = data.frame(x_test))
mspe_model0 <- mean((predictions_0 - y_test)^2)
print(mspe_model0)




##Ridge Regression
lambdas.rr <- exp(seq(-5, 5, length = 100))
y <- data_no_outliers$tw
X <- as.matrix(data_no_outliers[,-1])
set.seed(613)
ridge_cv <- cv.glmnet(x = X, y = y, lambda = lambdas.rr,alpha = 0)
ridge_cv$lambda.min
ridge <- glmnet(x = X, y = y, lambda = ridge_cv$lambda.min,alpha = 0)
ridge$beta

ridge_pred <- predict(ridge, newx = X)
head(ridge_pred)


#Lasso Regression
lambdas.LASSO <- exp(seq(-5, 5, length = 100))
set.seed(613)
LASSO_cv <- cv.glmnet(x = X, y = y, lambda = lambdas.rr,alpha = 1)
LASSO_cv$lambda.min
LASSO <- glmnet(x = X, y = y, lambda = LASSO_cv$lambda.min,alpha = 1)
LASSO$beta
LASSO_pred <- predict(LASSO, newx = X)
head(LASSO_pred)


#stepwise model
library(MASS)
full <- lm(tw ~ ., data=data_no_outliers)
null <- lm(tw ~ 1, data=data_no_outliers)

# forward stepwise - AIC
a <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
# backward stepwise - AIC
b <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')
coef(a)
coef(b)

#first method in comparing MSPE between linear models
n <- length(y)
# Set seed for consistency
set.seed(613)
# percent of observations in training dataset
percent <- 0.8
# randomly assign the observations to training
train <- sample(1:n < percent * n)
test <- ! train

# Train the models on the training set

# Stepwise
full <- lm(tw ~ ., data=data_no_outliers[train,])
null <- lm(tw ~ 1, data=data_no_outliers[train,])

# Forward stepwise
forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
# Backward stepwise
backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')

# Make predictions for stepwise selection
pr.stepwise_forward <- predict(forward, newdata=data_no_outliers[test,])
pr.stepwise_backward <- predict(backward, newdata=data_no_outliers[test,])


# Do with LASSO/Ridge (we use X and y defined in part 4)
X.train <- as.matrix(X[train,])
y.train <- y[train]
X.test <- X[test,]

# glmnet chooses its own sequence if no lambda sequence is provided.
ridge.cv <- cv.glmnet(x = X.train, y = y.train, alpha = 0)
lasso.cv <- cv.glmnet(x = X.train, y = y.train, alpha = 1)

ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)

# Make predictions for Ridge/LASSO
pr.lasso <- predict(lasso, newx=X.test)
pr.ridge <- predict(ridge, newx=X.test)


# Calculate MSPE
MSPE_stepwsie_forward <- mean((y[test] - pr.stepwise_forward)^2)
MSPE_stepwsie_backward <- mean((y[test] - pr.stepwise_backward)^2)
MSPE_Lasso <- mean((y[test] - pr.lasso)^2)
MSPE_Ridge<- mean((y[test] - pr.ridge)^2)

cbind(mean(MSPE_stepwsie_forward), mean(MSPE_stepwsie_backward), mean(MSPE_Lasso), mean(MSPE_Ridge))

#choose Ridge

#K-fold Cross Validation: Ridge vs LASSO vs Stepwise
n <- length(y)
k <- 10 
set.seed(613)
id <- sample(rep(1:k, length= n))

MSPE.stepwise_backward <- MSPE.stepwise_forward <- MSPE.lasso <- MSPE.ridge <- rep(NA, k)

for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  
  # Stepwise
  full <- lm(tw ~ ., data=data_no_outliers[train,])
  null <- lm(tw ~ 1, data=data_no_outliers[train,])
  
  # Forward stepwise
  forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
  # Backward stepwise
  backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')
  
  # Make predictions for stepwise selection
  pr.stepwise_forward <- predict(forward, newdata=data_no_outliers[test,])
  pr.stepwise_backward <- predict(backward, newdata=data_no_outliers[test,])
  
  ## Do with LASSO/Ridge (we use X and y defined in part 4)
  X.train <- as.matrix(X[train,])
  y.train <- y[train]
  X.test <- X[test,]
  
  #glmnet chooses its own sequence if no lambda sequence is provided.
  ridge.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 0) 
  lasso.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 1)
  
  ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
  lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)
  
  # Make predictions for Ridge/LASSO
  pr.lasso <- predict(lasso, newx=X.test)
  pr.ridge <- predict(ridge, newx=X.test)
  
  # Calculate MSPE
  MSPE.stepwise_backward[f] <- mean((y[test] - pr.stepwise_forward)^2)
  MSPE.stepwise_forward[f] <- mean((y[test] - pr.stepwise_backward)^2)
  MSPE.lasso[f] <- mean((y[test] - pr.lasso)^2)
  MSPE.ridge[f] <- mean((y[test] - pr.ridge)^2)
}
mean(MSPE.stepwise_backward)
mean(MSPE.stepwise_forward)
mean(MSPE.lasso)
mean(MSPE.ridge)

cbind(mean(MSPE.stepwise_backward), mean(MSPE.stepwise_forward), mean(MSPE.lasso), mean(MSPE.ridge))
#choose lasso smallest mspe


#using poly on selective varaibles
set.seed(613)
k <- 10
#id <- sample(rep(1:k, length= n))
p <- 12
mspe_degree_poly1 <- rep(NA, p)

for (deg in 1:p) {
  mspe <- vector(length = k)
  for (f in 1:k) {
    test <- (id == f)
    train <- (id != f)
    
    #continuous_X_poly <- poly(as.matrix(continuous_X), deg)
    #model1_X <- as.matrix(cbind(continuous_X_poly, binary_X))
    # Polynomial fit with deg degrees
    poly_reg_two_var <- lm(tw~poly(nifa+inc+hval,deg), data=data_no_outliers[train,])
    
    
    # Make predictions
    pred <- predict(poly_reg_two_var, newdata=data_no_outliers[test,])
    
    # Calculate MSPE
    mspe[f] <- mean((y[test] - pred)^2)
  }
  mspe_degree_poly1[deg] <- mean(mspe)
}

plot(mspe_degree_poly1)
which.min(mspe_degree_poly1)
mspe_degree[which.min(mspe_degree_poly1)]

#poly OLS
continuous_X <- subset(data_no_outliers, select = c(ira, nifa, inc, hmort, hval, educ, age, fsize))
binary_X <- subset(data_no_outliers, select = c(e401, male, twoearn, nohs, hs, smcol, marr))

tw <- data_no_outliers$tw
model_poly <- lm(tw ~ model1_X) 
summary(model_poly)

y <- data_no_outliers$tw
n <- length(y)
set.seed(613)
k <- 10
#id <- sample(rep(1:k, length= n))
p <- 5
mspe_degree <- rep(NA, p)

for (deg in 1:p) {
  mspe <- vector(length = k)
  for (f in 1:k) {
    test <- (id == f)
    train <- (id != f)
    continuous_X_poly <- poly(as.matrix(continuous_X), deg)
    model1_X <- as.matrix(cbind(continuous_X_poly, binary_X))
    # Polynomial fit with deg degrees
    poly_reg <- lm(y[train]~., data=as.data.frame(model1_X)[train,])
    
    # Make predictions
    pred <- predict(poly_reg, newdata = as.data.frame(model1_X)[test,])
    
    # Calculate MSPE
    mspe[f] <- mean((y[test] - pred)^2)
  }
  mspe_degree[deg] <- mean(mspe)
}
# Plot the mspe vs degree of polynomial fit
plot(mspe_degree)

which.min(mspe_degree)
mspe_degree[which.min(mspe_degree)]


#use poly for final one

# best model: poly on all variables has the smallest mspe 





## Make predictions on for prediction challenge
## ---------------------------------------------------------
### We now want to make the predictions 
#my_model <- cv.glmnet(y = data_tr$tw, x = as.matrix(subset(data_tr, select = -tw), alpha = 1))
OLS <- lm(tw ~ ., data = data)
keep <- names(OLS$coefficients)[!is.na(OLS$coefficients)][-1]
keep <- c('tw', keep)
data <- subset(data, select = keep)

data_te <- read.table("data_for_prediction.txt", header = TRUE, sep = "\t", dec = ".")[,-1]
data_te <- subset(data_te, select = keep[-1])
continuous_X_te <- subset(data_te, select = c(ira, nifa, inc, hmort, hval, educ, age, fsize))
binary_X_te <- subset(data_te, select = c(e401, male, twoearn, nohs, hs, smcol, marr))

continuous_X_poly <- poly(as.matrix(continuous_X), 2)
model1_X <- as.matrix(cbind(continuous_X_poly, binary_X))
my_poly_model <- lm(y~., data=as.data.frame(model1_X))
continuous_X_te_poly <- poly(as.matrix(scale(continuous_X_te)), 2) 
data_te_prepared <- cbind(continuous_X_te_poly, binary_X_te)
data_te <- as.data.frame(scale(data_te))
data_te <-as.matrix(cbind(poly(as.matrix(continuous_X_te,2)),binary_X_te))
my_predictions <- predict(my_poly_model, newdata = as.data.frame(data_te_prepared))


length(my_predictions)
write.table(my_predictions, file = 'my_predictions.txt')


