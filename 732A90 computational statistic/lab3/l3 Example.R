# Assignment 2

# 1.________________________________________________________

# load the data

df<-read.csv("parkinsons.csv")

# check for missing data

any(is.na(df))

# remove nonessential features

df<-df[,-c(1,2,3,4,6)]

# rearrange columns so that motor_UPDRS stands the last

df<-as.data.frame(scale(df[,c(2:17,1)]))

# divide the data by 60/40 

n=dim(df)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.6))
train=df[id,]
test=df[-id,]


# 2.________________________________________________________

# run linear regression model

linreg<-lm(formula = motor_UPDRS ~ . - 1, data = train)

# looks like below listed variables strongly contribute to the motor_UPDRS:
# Jitter..., Shimmer, Shimmer.APQ5, Shimmer.APQ11, NHR, HNR, DFA, PPE
# RSE = 0.9366

train_fitted<-predict(linreg,train)           # fit train data
test_fitted<-predict(linreg,test)             # predict test data

mse_train<-mean((train[,17]-train_fitted)^2)

mse_test<-mean((test[,17]-test_fitted)^2)

cat("MSE values derived using Normal Equation: \n")
cat("mse train =", mse_train,"\n")
cat("mse test =", mse_test, "\n")
cat("\n")



# 3.__________________________________________________

# a)

sigma<-0.9366

Loglikelihood<-function(params){
    
    tetta<-params[1:16]
    sigma<-params[17]
    
    squared_sum<-sum((as.matrix((train[,1:16]))%*%(as.matrix(tetta))-train[,'motor_UPDRS'])^2)
    
    
    logl<-(-length(train[,'motor_UPDRS'])/2)*log(2*pi*sigma^2)-squared_sum/(2*sigma^2)
    
    return(-logl)  # it is converted to minus log-likelihood because in b) it says that
    # add lambda to minus log-likelihood (minus could be added later as well)
    
}

# b)

Ridge<-function(params,lambda){
    
    tetta<-params[1:16]
    
    return(Loglikelihood(params)+lambda*sum(tetta^2))
}

# c)

RidgeOpt<-function(lambda){
    
    optim(par = c(coefficients(linreg),0.9366), fn = Ridge, lambda=lambda, method = "BFGS")
    
}

# d)

DF<-function(lambda){
    
    X<-as.matrix(train[, -17])
    
    deg_of_free<-sum(diag( (X %*% solve(t(X) %*% X + lambda*diag(16))) %*% t(X) ) )
    
    return(deg_of_free)
    
    
}



# 4.___________________________________________________________

# test tetta and sigma optimization for different lambdas

lambda_values<-c(1,100,1000)

optim_mse_train<-c()
optim_mse_test<-c()
optim_sigma<-c()
degrees_of_freedom<-c()

for (i in lambda_values) {
    
    # run optim function iteratively
    
    ridge_opt<-RidgeOpt(i)
    
    # extract optimal tettas
    
    opt_tetta<-ridge_opt$par[1:16]
    
    opt_tetta<-as.matrix(opt_tetta)
    
    # calculate mse for training data using extracted tettas
    
    train_x<-as.matrix((train[,1:16]))
    
    train_y<-train_x%*%opt_tetta
    
    omse_train<-mean((as.matrix(train[,17])-train_y)^2)
    
    optim_mse_train<-c(optim_mse_train, omse_train)
    
    # calculate mse for test data using extracted tettas
    
    test_x<-as.matrix((test[,1:16]))
    
    test_y<-test_x%*%opt_tetta
    
    omse_test<-mean((as.matrix(test[,17])-test_y)^2)
    
    optim_mse_test<-c(optim_mse_test, omse_test)
    
    # extract and save optimal sigma values as a list
    
    optim_sigma<-c(optim_sigma, ridge_opt$par[17])
    
    # calculate degrees of freedom for each lambda
    
    degrees_of_freedom<-c(degrees_of_freedom, DF(i))
    
}

# data frame that contains sigma, test, train responses and degrees of freedom

final_df<-data.frame(lambda_values, optim_mse_train, 
                     optim_mse_test, optim_sigma, degrees_of_freedom)

cat("MSE values derived using Optimization: \n")
print(final_df)