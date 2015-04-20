#Libraries required 

library(rpart)
library(rpart.plot)
library(caTools)
library(lubridate)
library(tm)
library(caret)
library(e1071)
library(randomForest)
library(gbm)


#set the working directory and read the files.
setwd('D:/Userfiles/asarapure/Documents/RStudio/Projects/Analytics_Edge/Kaggle')
train = read.csv('NYTimesBlogTrain.csv')
test = read.csv('NYTimesBlogTest.csv')
sub = read.csv('SampleSubmission.csv')


#Preprocessing the data and converting it into a DocumentTermMatrix
corpusdtm = function(textCol, sparsity , letter)
{
  corpus = Corpus(VectorSource(textCol))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus , removeWords , stopwords("english"))
  corpus = tm_map(corpus, stemDocument)
  
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, sparsity)
  colnames(dtm) = paste0(letter, colnames(dtm))
  return(as.data.frame(as.matrix(dtm)))
}

#Creating dtm and adding other variables to it.
dtma = corpusdtm(train$Abstract , 0.945 , 'a')
dtmh = corpusdtm(train$Headline, 0.95, 'h')
dtms = corpusdtm(train$Snippet, 0.945, 's')

#with 0.945 the names are "acompani" "anew"     
#"atime"    "aweek"    "awill"    "ayork". Removing last.
dtma = dtma[,-6]
dtma = dtma[,-4]
#dtmh with 0.95 gives hnew for both train and test.

#dtms two extra. 
dtms = dtms[,-6]
dtms = dtms[,-4]

dtmTrain = cbind(dtma, dtms, dtmh)

dtmTrain$Pop = train$Popular
#dtmTrain$Pop = as.factor(dtmTrain$Pop) # Converting to factor.
dtmTrain$NewsDesk = train$NewsDesk
dtmTrain$secName = train$SectionName
dtmTrain$subSName = train$SubsectionName
dtmTrain$wrdCount = train$WordCount
date = as.Date(train$PubDate)
dtmTrain$weekday = as.factor(wday(date))
dtmTrain$isSaturday = as.factor(wday(date) == 6)
dtmTrain$isSunday  = as.factor(wday(date) == 7)
dtmTrain$hour = as.factor(hour(train$PubDate))


#dtmTrain$isHoliday = 
#creating a isHoliday variable using conditional expression is difficult. 




#Doing the same for the test data. 

test.dtms = corpusdtm(test$Snippet , 0.945 , 's')
test.dtmh = corpusdtm(test$Headline , 0.95, 'h')
test.dtma = corpusdtm(test$Abstract, 0.945, 'a')

#test.dtms has one extra.
test.dtms = test.dtms[,-5]
test.dtma = test.dtma[,-5]

dtmTest = cbind(test.dtma, test.dtms, test.dtmh)


dtmTest$NewsDesk = test$NewsDesk
dtmTest$secName = test$SectionName
dtmTest$subSName = test$SubsectionName
dtmTest$wrdCount = test$WordCount
datetest = as.Date(test$PubDate)
dtmTest$weekday = as.factor(wday(datetest))
dtmTest$isSaturday = as.factor(wday(datetest) == 6)
dtmTest$isSunday = as.factor(wday(datetest) == 7)
dtmTest$hour = as.factor(hour(test$PubDate))






#splitting the training test for crossvalidation
spl = sample.split(dtmTrain$Pop , SplitRatio =0.7)
dtm.cv.train = subset(dtmTrain , spl == TRUE)
dtm.cv.test = subset(dtmTrain , spl == FALSE)


#CrossValidation
numFolds = trainControl( method = "cv", number = 20 )
cpGrid = expand.grid( .cp = seq(0.0001,0.01,0.0005)) 

train(Pop ~ NewsDesk + secName + subSName + wrdCount + weekday + isSaturday + isSunday, data = dtmTrain , method = 'rpart' , trControl = numFolds , tuneGrid = cpGrid)



#Random Forest. Tune RF
dtm.rf.train = dtmTrain[,seq(73,80)]
tuneRF(dtm.rf.train[,-1], dtm.rf.train[,1] , ntreeTry = 1000, stepFactor = 2  ,
       trace = TRUE , plot = TRUE , doBest = FALSE)
#get mtry 2 as the best.


#Let's test for number of trees and nodesize. 

for (j in 0:2){
for( i in 0:3){
  numTrees = 100 + ( 10 * i)
  ndSize = 1 + (1 * j)
  spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
  train.data = subset(dtmTrain, spl == TRUE)
  test.data = subset(dtmTrain , spl == FALSE)

rftrees = randomForest(Pop ~ NewsDesk + secName + subSName + wrdCount + weekday + isSaturday + isSunday + hour, mtry = 2,
             ntree = numTrees , data = train.data , nodesize = ndSize)
predTrees = predict(rftrees , newdata = test.data)
RMSES_sum = sum(sqrt((predTrees - test.data$Pop)^2))
print(paste('trees are ' , numTrees , 'nodesize is ' , ndSize , 'RMSE is ', RMSES_sum))
}
}

#Number of trees being too doesn't really help that much. Plus, the results are random each. 
#



#correcting the levels of both train and test.
c = rbind(dtmTrain[,seq(74,81)] , dtmTest[,seq(22, 29)])
l.test = c[6533:8402,] #Matching the levels
#train and predict on the test set. 
rfMod = randomForest(Pop ~ NewsDesk + secName + subSName + wrdCount + weekday + isSaturday + isSunday + hour, mtry = 2, ntree = 119, nodesize = 3, data = dtmTrain)
predRF = predict(rfMod , newdata = l.test)










cart = rpart(Pop ~ ., data = dtrn )
prp(cart)
pred = predict(cart, newdata = dtst)
summary(pred)

svMd = svm(Pop ~ . , data = dtrn)
predSvm = predict(svMd, newdata = dtst)
summary(predSvm)



rem = c(1,3,4, 5, 6, 7, 11, 14, 16)
dtrain = dtmTrain [, -rem]
dtrain$Pop = train$Popular


#even less variables.
newTrain = dtrain[, seq(7,15)]
newTest = dtmTest[,seq(9,16)]



cartM = rpart(Pop ~ . , data = dtrain )
prp(cartM)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 
train(Pop ~ . , data = newTrain , method = 'rpart', trControl = numFolds , tuneGrid = cpGrid)


treeCV = rpart(Pop ~ . , data = newTrain , cp = 0.001, minbucket = 10)
prp(treeCV)
predTree = predict(treeCV , newdata = newTest)


predM = predict(cartM , newdata = dtmTest)
summary(predM)




glmM = glm(Pop ~ . , data = newTrain )
predG = predict(glmM , newdata = newTest)





