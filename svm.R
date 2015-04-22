#SVM model for the data. 


#Don't use cost 100. Takes forever. 
  for( i in seq(6,18,3)){
    cost = 5 * (i)
    kernel = 'radial'
    spl = sample.split(dtmTrain$Pop , SplitRatio = 0.7)
    train.data = subset(dtmTrain, spl == TRUE)
    test.data = subset(dtmTrain , spl == FALSE)
    
    svm.mdl = svm(Pop ~ ., data = train.data , kernel = kernel , cost = cost)
    svm.pred = predict(svm.mdl , newdata = test.data)
    svm.pred = ifelse(svm.pred < 0, 0, svm.pred)
    svm.pred = ifelse(svm.pred > 1, 1, svm.pred)
    RMSES_sum = sum(sqrt((svm.pred - test.data$Pop)^2))
    print(paste( 'Cost is ' , cost , 'RMSE is ', RMSES_sum))
  }



#crossvalidation for svm 

set.seed(1)
tune.out = tune(svm, Pop ~ . , data = dtmTrain , kernel = 'radial' , ranges = list(cost = c(0.001, 
            0.01, 0.1, 1, 5, 10 , 20 )))


  
  
  
  c = rbind(dtmTrain[,seq(11,18)] , dtmTest[,seq(10, 17)])
  l.test = c[6533:8402,] #Matching the levels
  l.t = cbind(dtmTest[,seq(1,9)], l.test)  
  
svm.mdl = svm( Pop ~ . , data = dtmTrain , kernel = 'radial'  ,cost = 37)
svm.pred = predict(svm.mdl , newdata = l.t )  
svm.pred = ifelse(svm.pred < 0 , 0, svm.pred)

sub$Probability1 = svm.pred
write.csv(sub, 'sub3.csv', row.names = FALSE, quote = FALSE)
