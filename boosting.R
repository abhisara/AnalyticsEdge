#boosting Algorithm and logistic Regression

boost.train = dtmTrain[,seq(73,81)]
boost.test = dtmTest[,seq(22,29)]
boost.Model = gbm(Pop ~ . , data = boost.train, n.trees = 5000,distribution = 'binomial', shrinkage = 0.01,
                  interaction.depth = 4)
boost.Pred = predict( boost.Model , newdata = boost.test, n.trees = 5000)







for( i in 0:4){
  
  spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
  train.data = subset(dtmTrain, spl == TRUE)
  test.data = subset(dtmTrain , spl == FALSE)
  
  log.mdl = glm(Pop ~ . , data = train.data , family = 'binomial')
  pred.log = predict(log.mdl , newdata = test.data , type = 'response')
  
  RMSES_sum = sum(sqrt((pred.log - test.data$Pop)^2))
  print(paste( 'Cost is ' , cost , 'RMSE is ', RMSES_sum))
}


for( i in 0:4){
  
  spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
  train.data = subset(dtmTrain, spl == TRUE)
  test.data = subset(dtmTrain , spl == FALSE)
  
  lda.mdl = lda(Pop ~ . , data = train.data )
  pred.lda = predict(lda.mdl , newdata = test.data , type = 'response')
  
  RMSES_sum = sum(sqrt((pred.log - test.data$Pop)^2))
  print(paste( 'Cost is ' , cost , 'RMSE is ', RMSES_sum))
}


for( i in 0:4){
  
  spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
  train.data = subset(dtmTrain, spl == TRUE)
  test.data = subset(dtmTrain , spl == FALSE)
  
  tree.mdl = rpart(Pop ~ . , data = train.data , minbucket = 50)
  pred.tree = predict(tree.mdl , newdata = test.data )
  
  RMSES_sum = sum(sqrt((pred.log - test.data$Pop)^2))
  print(paste( 'Cost is ' , cost , 'RMSE is ', RMSES_sum))
}
