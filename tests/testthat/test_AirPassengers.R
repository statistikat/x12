library(x12)
test_that("AirPassenger ts x12 run",{
xts <- x12(AirPassengers)
s <- summary(xts)
expect_true(class(xts)=="x12Output")
expect_true(s[12,2]=="0.26")
})

test_that("AirPassenger x12Single x12 run",{
xs <- x12(new("x12Single",ts=AirPassengers))
s <- summary(xs)
expect_true(class(xs)=="x12Single")
expect_true(s[12,2]=="0.26")
})

test_that("AirPassenger x12Batch x12 run",{
  xb <- x12(new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers)))
  xbs <- summary(xb)
  
  expect_true(class(xb)=="x12Batch")
  expect_true(all(sapply(xb@x12List,class)=="x12Single"))
  expect_true(all(xbs[12,2:4]=="0.26"))
  #Create new batch object with 4 time series
  xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))
  # change the automdl to FALSE in all 4 elements
  xb <- setP(xb,list(automdl=FALSE))
  #change the arima.model and arima.smodel setting for the first ts object
  xb <- setP(xb,list(arima.model=c(1,1,0),arima.smodel=c(1,1,0)),1)
  #change the arima.model and arima.smodel setting for the second ts object
  xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),2)
  #change the arima.model and arima.smodel setting for the third ts object
  xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(1,1,1)),3)
  #change the arima.model and arima.smodel setting for the fourth ts object
  xb <- setP(xb,list(arima.model=c(1,1,1),arima.smodel=c(1,1,1)),4)
  #run x12 on all series
  xb <- x12(xb)
  xbs <- summary(xb)
  expect_true(class(xb)=="x12Batch")
  expect_true(all(sapply(xb@x12List,class)=="x12Single"))
  expect_true(all(xbs[12,2:5]=="0.26"))
  #Set automdl=TRUE for the first ts
  xb <- setP(xb,list(automdl=TRUE),1)
  #rerun x12 on all series (the binaries will only run on the first one)
  xb <- x12(xb)
  
  #summary with oldOutput
  xbs <- summary(xb,oldOutput=10)
  
  #Change the parameter and output of the first series back to the first run
  xb <- prev(xb,index=1,n=1)
  
  #summary with oldOutput (--- No valid previous runs. ---)
  summary(xb,oldOutput=10)
  
})