#ensemble of RandomForest and SVM. Create predictions for both RF and SVM. 

ensemble.pred = ifelse(svm.pred <= 0, predRF, svm.pred)
ensemble.pred = ifelse(ensemble.pred > 1 , predRF , ensemble.pred)


ensem.pred = (svm.pred + predRF * 6) / 7
ensem.pred = ifelse(ensem.pred < 0 , predRF, ensem.pred)
ensem.pred = ifelse(ensem.pred > 1 , predRF, ensem.pred)


sub$Probability1 = ensem.pred
ensem.pred = specify_decimal(ensem.pred, 7)



for( i in seq(1,15,3)){
  
  spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
  train.data = subset(dtmTrain, spl == TRUE)
  test.data = subset(dtmTrain , spl == FALSE)
  
  rftrees = randomForest(Pop ~ ., mtry = 7,
                         ntree = 501 , data = train.data , nodesize = 7)
  svm.mdl = svm( Pop ~ . , data = dtmTrain , kernel = 'radial'  ,cost = 10)
  svm.pred = predict(svm.mdl , newdata = test.data)
  
  predTrees = predict(rftrees , newdata = test.data)
  
  ensem.pred = (svm.pred + predTrees * i)/ (2+ (i- 1))
  ensem.pred = ifelse(ensem.pred < 0 , predRF, ensem.pred)
  ensem.pred = ifelse(ensem.pred > 1 , predRF, ensem.pred)
  
  
  RMSES_sum = sum(sqrt((ensem.pred - test.data$Pop)^2))
  print(paste('Rf contribution  ' ,i ,' times. RMSE is ', RMSES_sum))
}