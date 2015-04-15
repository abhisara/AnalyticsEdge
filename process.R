#Libraries required 

library(rpart)
library(rpart.plot)
library(caTools)




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
  colnames(dtm) = paste(letter, colnames(dtm))
  return(as.data.frame(as.matrix(dtm)))
}


corAbs = corpusdtm(train$Abstract , 0.96 , 'a')
corHeadline = corpusdtm(train$Headline, 0.96, 'h')
corSnippet = corpusdtm(train$Snippet, 0.96, 's')



dtmTrain = cbind(dtma, dtms, dtmh)

dtmTrain$Pop = train$Popular
dtmTrain$NewsDesk = train$NewsDesk
dtmTrain$secName = train$SectionName
dtmTrain$subSName = train$SubsectionName
dtmTrain$wrdCount = train$WordCount
date = as.Date(train$PubDate)
dtmTrain$weekday = as.factor(wday(date))
dtmTrain$isSaturday = as.factor(wday(date) == 6)
dtmTrain$isSunday  = as.factor(wday(date) == 7)


#dtmTrain$isHoliday = 
#creating a isHoliday variable using conditional expression is difficult. 


spl = sample.split(dtmTrain$Pop , SplitRatio =0.7)
dtrn = subset(dtmTrain , spl == TRUE)
dtst = subset(dtmTrain , spl == FALSE)



cart = rpart(Pop ~ ., data = dtrn )
prp(cart)
pred = predict(cart, newdata = dtst)
summary(pred)

svMd = svm(Pop ~ . , data = dtrn)
predSvm = predict(svMd, newdata = dtst)
summary(predSvm)



corTestSnip = corpusdtm(test$Snippet , 0.96 , 's')
corTestHead = corpusdtm(test$Headline , 0.95, 'h')
corTestAbs = corpusdtm(test$Abstract, 0.95, 'a')


dtmTest = cbind(Tdtma, Tdtms, Tdtmh)


dtmTest$NewsDesk = test$NewsDesk
dtmTest$secName = test$SectionName
dtmTest$subSName = test$SubsectionName
dtmTest$wrdCount = test$WordCount
datetest = as.Date(test$PubDate)
dtmTest$weekday = as.factor(wday(datetest))
dtmTest$isSaturday = as.factor(wday(datetest) == 6)
dtmTest$isSunday = as.factor(wday(datetest) == 7)



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



library(randomForest)
forest = randomForest(Pop ~ . , data = newTrain[,-1] , ntree = 500 , nodesize = 5)
predForest = predict(forest, newdata = newTest[,-1])



