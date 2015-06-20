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
library(MASS)
library(ggplot2)


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








cart = rpart(Pop ~ ., data = dtrn )
prp(cart)
pred = predict(cart, newdata = dtst)
summary(pred)


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





