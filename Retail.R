setwd("C:/Users/Admin/Desktop/Preeti IIT Kanpur/R_language/Projects")

s_train = read.csv("store_train.csv")
s_test = read.csv("store_test.csv")

View(s_train)

s_test$store = NA

s_train$data = "train"
s_test$data = "test"

s_all = rbind(s_train,s_test)
View(s_all)

## data processing

library(dplyr)

glimpse(s_all)


names(s_all)[sapply(s_all, function(x) is.character(x))]

sort(table(s_all$store_Type),decreasing = T)

sort(table(s_train$countyname),decreasing = T)

sort(table(s_train$Areaname),decreasing = T)

sort(table(s_train$countytownname),decreasing = T)

sort(table(s_train$state_alpha),decreasing = T)

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

s_all$Id = NULL

s_all = CreateDummies(s_all,"countyname",50)

s_all = CreateDummies(s_all,"Areaname",50)

s_all = CreateDummies(s_all,"countytownname",10)

s_all = CreateDummies(s_all,"state_alpha",50)

s_all = CreateDummies(s_all,"store_Type",100)

### splitting storecode column

library(tidyr)

s_all = s_all %>%
  separate(storecode, 
           into = c("text", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )

s_all$num = NULL

#s_all$storecode = NULL

## renaming new column name text to storecode

names(s_all)[9] = c("storecode")

#or

#colnames(s_all)[9l] = c("storecode")


sort(table(s_all$storecode),decreasing = T)

s_all = CreateDummies(s_all,"storecode",500)

names(s_all)

### finding NA's

lapply(s_all,function(x) sum(is.na(x)))

### removing "NA" rows since having less observations
s_all = s_all[!is.na(s_all$population),]

s_all = s_all[!is.na(s_all$country),]

library(randomForest)
library(ggplot2)
library(tree)
library(cvTools)

## separating train and test data

s_train = s_all %>% 
  filter(data == "train") %>% 
  select(-data)

s_test = s_all %>% 
  filter(data == "test") %>% 
  select(-data,-store)

View(s_test)

### separating validation set from train set

s = sample(0.7*nrow(s_train))
s_train1 = s_train[s,]
s_train2 = s_train[-s,]

s_t1 = tree(store~.,data = s_train1)

## validation set
s_t2_pred = predict(s_t1,newdata=s_train2,type = "vector")[,2]
s_t2_real = s_train2$store

auc(roc(s_t2_real,s_t2_pred))

## In classification converting outcome into factor type.
s_train$store=as.factor(s_train$store)

names(s_train) = gsub("`","", names(s_train))
s_train$`Areaname_PenobscotCountyME(part)HUDMetroFMRArea` = NULL

##### building dtree 

s_tree_train = tree(store~.,data = s_train)

plot(s_tree_train)
text(s_tree_train)

lapply(s_train,function(x) sum(is.na(x)))

glimpse(s_train)

s_train_real = s_train$store
s_train_pred = predict(s_tree_train,newdata = s_train,type = "vector")[,2]

library(pROC)
auc(roc(s_train_real,s_train_pred))

### prediction on test data

s_tree_test = predict(s_tree_train,newdata = s_test,type = "vector")[,2]


### Randomforest model
s_t1_rf = randomForest(store~.,data = s_train1)

s_t2_real = s_train2$store
s_t2_pred = predict(s_t1_rf,newdata = s_train2,type = "prob")[,2]
auc(roc(s_t2_real,s_t2_pred))

s_train_rf = randomForest(store~.,data = s_train)

s_train_real = s_train$store
s_train_rf_pred = predict(s_train_rf,newdata = s_train,type = "prob")[,2]

auc(roc(s_train_real,s_train_rf_pred))

##### prediction on test data

s_test_rf_pred = predict(s_train_rf,newdata = s_test,type = "prob")

## Variable IMportance

d = importance(s_train_rf)

d = data.frame(d)

d %>% arrange(desc(MeanDecreaseGini)),

## Varimp Plot

varImpPlot(s_train_rf)
