#Prepare test set data for prediction
rawtestdata <- read.csv('E:/R/Projects/JHU_R/Week4 Assignment_08/SourceData/pml-testing.csv')
names(rawtestdata)
summary(rawtestdata)

#convert date time
testthedatetime <- as.POSIXlt(as.character(rawtestdata$cvtd_timestamp), format = '%d/%m/%Y %H:%M')
#userdata
testusersData <- as.character(rawtestdata$user_name)
testusersData <- rawtestdata$user_name

#device data
testgyrosData <- rawtestdata[grepl("^gyros_", names(rawtestdata))]
testmagnetData <- rawtestdata[grepl("^magnet_", names(rawtestdata))]
testaccelData <- rawtestdata[grepl("^accel_", names(rawtestdata))]
#devicesData<-as.data.frame(cbind(thedatetime,usersData,gyrosData,magnetData,accelData))
testdevicesData <- as.data.frame(cbind(testgyrosData, testmagnetData, testaccelData))

#sites data
testforearmData <- rawtestdata[grepl("_forearm_", names(rawtestdata))]
testdumbbellData <- rawtestdata[grepl("_dumbbell_", names(rawtestdata))]
testbeltData <- rawtestdata[grepl("_belt_", names(rawtestdata))]
testarmData <- rawtestdata[grepl("_arm_", names(rawtestdata))]
testsitesData <- as.data.frame(cbind(testforearmData, testdumbbellData, testbeltData, testarmData))

#metric data
testyawData <- rawtestdata[grepl("^yaw_", names(rawtestdata))]
testrollData <- rawtestdata[grepl("^roll_", names(rawtestdata))]
testpitchData <- rawtestdata[grepl("^pitch_", names(rawtestdata))]

testmetricsData <- as.data.frame(cbind(testyawData, testrollData, testpitchData))

obs<-3
#predict on test data sets
#predict devices
devicesPredTest <- predict(devicesThetaTrain, testdevicesData[obs,])

#predict sites
sitesPredTest <- predict(sitesThetaTrain, testsitesData[obs,])

#predict metrics
metricsPredTest <- predict(metricsThetaTrain, testmetricsData[obs,])


#predict ensemble
TestENSpredDF <- data.frame(devicesPredTrain = devicesPredTest, sitesPredTrain = sitesPredTest, metricsPredTrain = metricsPredTest)
ENSpredTest <- predict(ENSthetaTrain, TestENSpredDF)

#use the getPredict_da() function to retrieve prediction one test observation at a time
ENSpredTest <- getPrediction_da(3)
ENSpredTest
