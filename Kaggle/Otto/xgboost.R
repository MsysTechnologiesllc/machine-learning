require(xgboost)
require(methods)

#setwd("~/Kaggle/Otto")
train = read.csv('train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('test.csv',header=TRUE,stringsAsFactors = F)
for(i in 2:94){
  train[,i] <- as.numeric(train[,i])
}
for(i in 2:94){
  test[,i] <- as.numeric(test[,i])
}
train = train[,-1]
test = test[,-1]

y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-ncol(train)],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

# Set necessary parameter
param <- list("objective" = "multi:softprob", 
              "eval_metric" = "mlogloss", 
              "num_class" = 9, 
              "nthread" = 8, 
              "eta"=0.1, 
              "max_depth"=27, 
              "gamma"=2, 
              "min_child_weight"=3, 
              "subsample" = 0.75, 
              "max_delta_step" = 4, 
              "colsample_bytree" = 0.85) 
# Run Cross Validation
#cv.nround = 5000
#bst.cv = xgb.cv(param=param, data = x[trind,], label = y, 
 #               nfold = 10, nrounds=cv.nround)
#print(bst.cv)
# Train the model
nround = 5000
bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)

# Make prediction
pred = predict(bst,x[teind,])
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='xgboost.csv', quote=FALSE,row.names=FALSE)
