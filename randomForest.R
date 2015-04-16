#RandomForest regression will be explored. 




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

