library(dplyr)
library(tree)
bk=read.csv("C:/Users/ujjwa/Downloads/bike_sharing_hours.csv",stringsAsFactors = F)
glimpse(bk)
names(bk)[sapply(bk,function(x) is.character(x))]
table(bk$dteday)
bk = bk %>% select(-dteday)
lapply(bk, function(x) length(unique(x)))
library(kknn)
library(gbm)
library(randomForest)
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
bk=bk %>% select(-yr,-instant,-temp,-casual,-registered)
for(var in c("season","mnth","hr","weekday"))
{
  bk=CreateDummies(bk,var,500)
  
}
glimpse(bk)
set.seed(2)
s=sample(1:nrow(bk),0.8*nrow(bk))
train=bk[s,]
test=bk[-s,]
## simple lineare model
fit=lm(cnt~.-weekday_6,data = train)
summary(fit)
test.pred=predict(fit,newdata=test)
error=(test.pred-test$cnt)**2 %>% mean() %>% sqrt()
error
library(cvTools)
## stacking
mykfolds=function(nobs,nfold=5){
  
  t=cvFolds(nobs,K=nfold,type='random')
  
  folds=list()
  
  for(i in 1:nfold){
    
    test=t$subsets[t$which==i]
    train=t$subsets[t$which!=i]
    
    folds[[i]]=list('train'=train,'test'=test)
  }
  
  return(folds)
}
cvFolds(6,3,type='random')

mykfolds(6,3)
# stack layer 1
myfolds=mykfolds(nrow(train),10)
train_layer1=data.frame(rf_var=numeric(nrow(train)),
                        gbm_var=numeric(nrow(train),
                        dtree_var=numeric(nrow(train)))
for(i in 1:10)
{
  print(c(i))
  fold=myfolds[[i]]
  train_data=train[fold$train,]
  test_data=train[fold$test,]
  print('rf')
  rf.fit=randomForest(cnt~.-weekday_6,data = train_data,ntree=100,mtry=10)
  rf_score=predict(rf.fit,newdata=test_data)
  print('gbm')
  gbm.fit=gbm(cnt~.-weekday_6,data = train_data,distribution = "gaussian",
              n.trees = 100,interaction.depth = 3)
  gbm_score=predict(gbm.fit,newdata=test_data,n.trees=100)
  print("dt")
  dtree.fit=tree(cnt~.-weekday_6,data = train_data)
  dtree_score=predict(dtree.fit,newdata=test_data)
  
  train_layer1$rf_var[fold$test]=rf_score
  train_layer1$gbm_var[fold$test]=gbm_score
  train_layer1$dtree_var[fold$test]=dtree_score
  
}
## stack layer 2 data for test and model
test_layer2=data.frame(rf_var=numeric(nrow(test)),
                        gbm_var=numeric(nrow(test),
                        dtree_var=numeric(nrow(test)))
full.rf=randomForest(cnt~.-weekday_6,data = train,ntree=100,mtry=10)
full.dtree=dtree(cnt~.-weekday_6,data = train)
full.gbm=gbm(cnt~.-weekday_6,data = train_data,distribution = "gaussian",
             n.trees = 100,interaction.depth = 3)
test_layer2$rf_var=predict(full.rf,newdata=test)
test_layer2$gbm_var=predict(full.gbm,newdata=test,n.trees=100)
test_layer2$dtree_var=predict(full.dtree,newdata=test)

# linear model
train_layer1$cnt=train$cnt
test_layer2$cnt=test$cnt

lin.model=lm(cnt~.,data=train_layer1)

test.pred=predict(lin.model,newdata=test_layer2)
(test.pred-test_layer2$cnt)**2 %>% mean() %>% sqrt()

