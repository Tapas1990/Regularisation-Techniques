#Setup
library(glmnet)
library(caret)
options(scipen=999)
'%ni%'=Negate('%in%')
#Create training and test Data
prostate=read.csv('https://goo.gl/qmrDcY')
#glmnet doesnt use formula interface.x and y have to be explicitly set also
#also training and test can only be matrix object.
set.seed(100)
train_rows=createDataPartition(prostate$lpsa,p=0.7,list=FALSE)
train_data=prostate[train_rows,]
test_data=prostate[-train_rows,]

train_x=as.matrix(train_data[,colnames(train_data)%ni%'lpsa'])
train_y=as.matrix(train_data[,'lpsa'])

test_x=as.matrix(test_data[,colnames(train_data)%ni%'lpsa'])
test_y=as.matrix(test_data[,'lpsa'])

#Step 1
grid=10^seq(10,-2,length=100)
grid

#Step 2
ridgeMod=glmnet(train_x,train_y,alpha = 0,
                lambda = grid,thresh = 1e-12)
ridgeMod

#Step 3
set.seed(100)
cv.out=cv.glmnet(train_x,train_y,alpha=0)#Alpha is the elastic net,mixing parameter
#alpha=0:RIDGE and alpha=1:LASSO
plot(cv.out)#2 vertical lines:1st points to lowest mean square error ,2nd one points to max variance within 1 sd 
bestlam=cv.out$lambda.min
bestlam

#Step 4
pred=predict(ridgeMod,s=bestlam,newx=test_x)
DMwR::regr.eval(test_y,pred)#mape=33.5%

#plot values of coefficients against log lambda in X axis
coefs_ridge=predict(ridgeMod,type='coefficients',s=bestlam)
coefs_ridge
plot(ridgeMod,xvar='lambda')
#None of the coefficients are zero until lambda is too large.

#Lasso
set.seed(100)
#Step 1
grid=10^seq(10,-2,length=100)
grid

#Step 2
lassoMod=glmnet(train_x,train_y,alpha = 1,
                lambda = grid,thresh = 1e-12)
lassoMod

#Step 3

cv.out=cv.glmnet(train_x,train_y,alpha=1)#Alpha is the elastic net,mixing parameter
#alpha=0:RIDGE and alpha=1:LASSO
plot(cv.out)#2 vertical lines:1st points to lowest mean square error ,2nd one points to max variance within 1 sd 
bestlam=cv.out$lambda.min
bestlam

#Step 4
pred=predict(lassoMod,s=bestlam,newx=test_x)
DMwR::regr.eval(test_y,pred)#mape:32.6%

coefs_ridge=predict(lassoMod,type='coefficients',s=bestlam)
coefs_ridge
plot(lassoMod,xvar='lambda')
#Mxing parameter alpha:mix of ridge and lasso
#for prostate dataset:Find mixing parameter alpha that minimises
#MAPE,for glmnet().Let step value for alpha search be 0.01.
#Note:The 'foldid'needs to be fixed for comparing MAPE for different alphas in cv.glmnet func so that same no of rows are considered always.

library(glmnet)
library(caret)
options(scipen=999)

'%ni%'=Negate('%in%')
#Create training and test Data
prostate=read.csv('https://goo.gl/qmrDcY')
#glmnet doesnt use formula interface.x and y have to be explicitly set also
#also training and test can only be matrix object.
set.seed(100)
train_rows=createDataPartition(prostate$lpsa,p=0.7,list=FALSE)
train_data=prostate[train_rows,]
test_data=prostate[-train_rows,]
train_x=as.matrix(train_data[,colnames(train_data)%ni%'lpsa'])
train_y=as.matrix(train_data[,'lpsa'])

test_x=as.matrix(test_data[,colnames(train_data)%ni%'lpsa'])
test_y=as.matrix(test_data[,'lpsa'])

alphas=seq(0,1,by=0.01)
alphas

set.seed(100)
foldid=sample(1:10,size=length(train_y),replace=TRUE)#define the foldid.
grid=10^seq(10,-2,length=100)#start 10 end -2 :# to the power:10,7,4,1,-2...
grid
mapes=numeric(length(alphas))#initialize output
mapes
i=1#loop counter.
for(a in alphas){
  bestlam=cv.glmnet(train_x,train_y,alpha=a,lambda= grid,foldid=foldid)$lambda.min#get best lambda for given alpha'a'
  enetMod=glmnet(train_x,train_y,alpha=a,lambda = bestlam)#fit glmnet model
  pred=predict(enetMod,s=bestlam,newx=test_x)#predict
  mapes[i]=DMwR::regr.eval(test_y,pred)[4]#get MAPE
  i=i+1#increment loop counter
}  
out=cbind(alphas,mapes)#final alphas and best MAPE
out
#Plot
plot(out,type='l',col='blue')
alpha=out[which.min(out[,2]),1]
alpha
mape=out[which.min(out[,2]),2]
mape
points(x=alpha,y=mape,cex=2,col='red',pch='*')
alpha



