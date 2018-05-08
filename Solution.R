# Set Working directory for R

train.set<-read.csv('C:/Users/Milind-PC/Box Sync/Study/01 Spring/MGS662 - ML/Assignmnet/Project2/BlogFeedback/blogData_train.csv',stringsAsFactors = F,header=FALSE, sep=",",)
#train.set <- train.set[1:5000,:]

#create X train 
train_X <- train.set[1:7000,c(51:60,281)]

train_sample <- sample(seq_len(nrow(train_X)),size=(floor(0.75*nrow(train_X))))

blog_train <- train_X[train_sample,]
blog_test <- train_X[-train_sample,]
nrow(blog_train)
nrow(blog_test)

require(Matrix)
require(Rmosek)
# matrix for Training Set
train_X=blog_train[,1:ncol(blog_train)-1]
train_Y=blog_train[,ncol(blog_train)]
train_X=as.matrix(train_X)
train_Y=as.matrix(train_Y)

# matrix for Test Set
test_X=blog_test[,1:ncol(blog_test)-1]
test_Y=blog_test[,ncol(blog_test)]
test_Y=as.matrix(test_Y)
test_X=as.matrix(test_X)

solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]
  
  xx<-crossprod(X) 
  
  c<--crossprod(X,y) 
  xx2<-xx
  xx2[upper.tri(xx)]<-0 
  idx <- which(xx2 != 0, arr.ind=TRUE)    

  qo1<-list() 
  qo1$sense<-"min"  
  qo1$c<-as.vector(c) 
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) 
  qo1$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE)
  qo1$bc<-rbind(blc=-Inf, buc= Inf)
  qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p))
  r<-mosek(qo1, opts = list(verbose = verb)) 
  return(r)
}

# LR
colnames(blog_train) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")
colnames(blog_test) <- c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eight","Ninth","Tenth","Dependent")

library(caTools)
mse_total=0
linear_regressor =lm(formula = Dependent ~ .,data = blog_train)
mse_test_value <- mean((blog_test$Dependent - predict(linear_regressor,newdata = blog_test))^2)
mse_total=mse_total+mse_test_value
print(paste("Average mean square error is ", mse_total))
summary(linear_regressor)


rf=solve.ols(train_X, train_Y)

yHat=test_X[,1]*0.057110 + test_X[,2]*0.197636 +
  test_X[,3]*(-0.011036) + test_X[,4]*(-0.079532)  + 
  test_X[,6]*(-0.728572) + test_X[,7]*(0.034236) + 
  test_X[,8]*(0.127589) + test_X[,9]*(-0.338546)

YMinusYHat = test_Y-yHat
mse <- function(error)
{
  
  mean(error^2)
  
}
MSE_Mosek=mse(YMinusYHat)
RSS_Mosek=sum(YMinusYHat^2)

