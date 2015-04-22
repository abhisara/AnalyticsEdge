#RandomForest regression will be explored. 




#Random Forest. Tune RF
#dtm.rf.train = dtmTrain[,seq(73,80)]
tuneRF(dtmTrain[,-10], dtmTrain[,10] , ntreeTry = 501, stepFactor = 1.5  ,
       trace = TRUE , plot = TRUE , doBest = FALSE)
#get mtry 2 as the best.


#Let's test for number of trees and nodesize. 
dtmTrain$Pop = as.integer(dtmTrain$Pop) #Doesn't work for factor Pop.

for (j in 0:1){
  for( i in 0:3){
    numTrees = 100 + ( 100 * i)
    ndSize = 5 + (3 * j)
    spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
    train.data = subset(dtmTrain, spl == TRUE)
    test.data = subset(dtmTrain , spl == FALSE)
    
    rftrees = randomForest(Pop ~ ., mtry = 6,
                           ntree = numTrees , data = train.data , nodesize = ndSize)
    predTrees = predict(rftrees , newdata = test.data)
    RMSES_sum = sum(sqrt((predTrees - test.data$Pop)^2))
    print(paste('trees are ' , numTrees , 'nodesize is ' , ndSize , 'RMSE is ', RMSES_sum))
  }
}

#Number of trees being too doesn't really help that much. Plus, the results are random each. 
#



#correcting the levels of both train and test.
c = rbind(dtmTrain[,seq(11,18)] , dtmTest[,seq(10, 17)])
l.test = c[6533:8402,] #Matching the levels
l.t = cbind(dtmTest[,seq(1,9)], l.test)
#train and predict on the test set. 
rfMod = randomForest(Pop ~ ., mtry = 7, ntree = 501, nodesize = 7, data = dtmTrain)
#predRF = predict(rfMod , newdata = l.t , type = 'prob')
predRF = predict(rfMod , newdata = l.t )
predRF = specify_decimal(predRF, 7)

#predRF = signif( predRF , digits = 6)
#options(digits = 6) # to not convert it into scientific notation.
sub$Probability1 = predRF
View(sub)

write.csv(sub, 'sub3.csv', row.names = FALSE, quote = FALSE)


specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
