###End data preparation, training and cross validation prediction code
library(caret)
library(ggplot2)
library(e1071)
library(plyr)
library(randomForest)
library(fastAdaboost)

#read data into R
rawdata <- read.csv('E:/R/Projects/JHU_R/Week4 Assignment_08/SourceData/pml-training.csv')

#set datetime to date timeobject
thedatetime <- as.POSIXlt(as.character(rawdata$cvtd_timestamp), format = '%d/%m/%Y %H:%M')


#create validation and train sets
inTrain = createDataPartition(rawdata$X, p = 0.75, list = FALSE)
Vrawdata <- rawdata[-inTrain,]
Trawdata <- rawdata[inTrain,]

#split data into components:
#devices
#sites
#metrics

#userdata
usersData <- as.character(rawdata$user_name)
usersData <- rawdata$user_name
#classe data
Tclasses <- as.factor(Trawdata$classe)
Vclasses <- as.factor(Vrawdata$classe)

#device data for training
TgyrosData <- Trawdata[grepl("^gyros_", names(Trawdata))]
TmagnetData <- Trawdata[grepl("^magnet_", names(Trawdata))]
TaccelData <- Trawdata[grepl("^accel_", names(Trawdata))]
TdevicesData <- as.data.frame(cbind(TgyrosData, TmagnetData, TaccelData))

#device data for validation
VgyrosData <- Vrawdata[grepl("^gyros_", names(Vrawdata))]
VmagnetData <- Vrawdata[grepl("^magnet_", names(Vrawdata))]
VaccelData <- Vrawdata[grepl("^accel_", names(Vrawdata))]
VdevicesData <- as.data.frame(cbind(VgyrosData, VmagnetData, VaccelData))

#sites data for training
TforearmData <- Trawdata[grepl("_forearm_", names(Trawdata))]
TdumbbellData <- Trawdata[grepl("_dumbbell_", names(Trawdata))]
TbeltData <- Trawdata[grepl("_belt_", names(Trawdata))]
TarmData <- Trawdata[grepl("_arm_", names(Trawdata))]
TsitesData <- as.data.frame(cbind(TforearmData, TdumbbellData, TbeltData, TarmData))

#sites data for validation
VforearmData <- Vrawdata[grepl("_forearm_", names(Vrawdata))]
VdumbbellData <- Vrawdata[grepl("_dumbbell_", names(Vrawdata))]
VbeltData <- Vrawdata[grepl("_belt_", names(Vrawdata))]
VarmData <- Vrawdata[grepl("_arm_", names(Vrawdata))]
VsitesData <- as.data.frame(cbind(VforearmData, VdumbbellData, VbeltData, VarmData))

#metric data for training
TyawData <- Trawdata[grepl("^yaw_", names(Trawdata))]
TrollData <- Trawdata[grepl("^roll_", names(Trawdata))]
TpitchData <- Trawdata[grepl("^pitch_", names(Trawdata))]
TmetricsData <- as.data.frame(cbind(TyawData, TrollData, TpitchData))

#metric data for validation
VyawData <- Vrawdata[grepl("^yaw_", names(Vrawdata))]
VrollData <- Vrawdata[grepl("^roll_", names(Vrawdata))]
VpitchData <- Vrawdata[grepl("^pitch_", names(Vrawdata))]
VmetricsData <- as.data.frame(cbind(VyawData, VrollData, VpitchData))

#train random forest for devices
devicesThetaTrain <- randomForest(x = TdevicesData, y = Tclasses, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
#predict random forest for devices
devicesPredTrain <- predict(devicesThetaTrain, TdevicesData)

#train random forest for sites
sitesThetaTrain <- randomForest(x = TsitesData, y = Tclasses, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
#predict random forest for sites
sitesPredTrain <- predict(sitesThetaTrain, TsitesData)

#train random forest for metrics
metricsThetaTrain <- randomForest(x = TmetricsData, y = Tclasses, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
#predict random forest for sites
metricsPredTrain <- predict(metricsThetaTrain, TmetricsData)

#build ensemble data set
TrainENSpredDF <- data.frame(devicesPredTrain, sitesPredTrain, metricsPredTrain) #, classe = Tclasses)
tries <- length(TrainENSpredDF[1,])
#train ensemble model
ENSthetaTrain <- randomForest(x = TrainENSpredDF, y = Tclasses, mtry = tries, importance = TRUE, proximity = TRUE, keep.forest = TRUE)

#predict on ensemble model
ENSpredTrain <- predict(ENSthetaTrain, TrainENSpredDF)

#cross validate on V data sets
#predict devices
devicesPredVal <- predict(devicesThetaTrain, VdevicesData)
#predict sites
sitesPredVal <- predict(sitesThetaTrain, VsitesData)
#predict metrics
metricsPredVal <- predict(metricsThetaTrain, VmetricsData)
#predict ensemble
ValENSpredDF <- data.frame(devicesPredTrain = devicesPredVal, sitesPredTrain = sitesPredVal, metricsPredTrain = metricsPredVal)
ENSpredVal <- predict(ENSthetaTrain, ValENSpredDF)

###End data preparation, training and cross validation prediction code

###Start exploration code

#analyse results for devices data set
devicesThetaTrain

impRF <- varImp(devicesThetaTrain)
impRF
order(impRF)
imp <- apply(impRF, 1, function(x) mean(x))
order(imp)

#shows which of the features in the dataset is most important in predicting which classe
varImpPlot(devicesThetaTrain, sort = TRUE, n.var = min(30, nrow(devicesThetaTrain$importance)),
type = NULL, class = NULL, scale = TRUE,
main = deparse(substitute(devicesThetaTrain)))

#analyse results for sites data set
sitesThetaTrain$

impRF <- varImp(sitesThetaTrain)
imp <- apply(impRF, 1, function(x) mean(x))
order(imp)

sum(sitesPredTrain == Tclasses)
sum(sitesPredTrain != Tclasses)

#shows which of the features in the dataset is most important in predicting which classe
varImpPlot(sitesThetaTrain, sort = TRUE, n.var = min(30, nrow(sitesThetaTrain$importance)),
type = NULL, class = NULL, scale = TRUE,
main = deparse(substitute(sitesThetaTrain)))

#analyse results for metric data set
metricsThetaTrain

varImpPlot(metricsThetaTrain, sort = TRUE, n.var = min(30, nrow(metricsThetaTrain$importance)),
type = NULL, class = NULL, scale = TRUE,
main = deparse(substitute(metricsThetaTrain)))

sum(metricsPredTrain == Tclasses)
sum(metricsPredTrain != Tclasses)

#shows which of the features in the dataset is most important in predicting which classe
par(mfrow = c(2, 2))
	for (i in 1:5){
		plot(sort(metricsThetaTrain$importance[, i], dec = TRUE),
	type = "h", main = paste("Measure", i))
	}

#analyse ensemble vs each data set prediction
ENSthetaTrain

varImpPlot(ENSthetaTrain, sort = TRUE, n.var = min(30, nrow(ENSthetaTrain$importance)),
type = NULL, class = NULL, scale = TRUE,
main = deparse(substitute(ENSthetaTrain)))

impRF <- varImp(ENSthetaTrain) #[,1:2]
imp <- apply(impRF, 1, function(x) mean(x))
order(imp)

#shows which of the predictors in the ensemble is most important in predicting which classe
par(mfrow = c(2, 2))
for (i in 1:5) {
	plot(sort(ENSthetaTrain$importance[, i], dec = TRUE),
	type = "h", main = paste("Measure", i))
}



qplot(y=order(Trawdata$cvtd_timestamp), x=tapply(Trawdata$user_name, Trawdata$X), colour = ENSpredTrain, xlab = 'tapply User x $X', ylab = 'Ordered time spans', main = 'Repitions per time event')
qplot(seq_along(rawdata$X), tapply(rawdata$X, order(rawdata$cvtd_timestamp)), colour = rawdata$user_name, xlab = 'Repitions', ylab = 'Ordered time spans', main = 'Repitions per time event')
t(ENSpredTrain)
sum(devicesPredTrain == Tclasses) / length(devicesPredTrain)
sum(sitesPredTrain == Tclasses) / length(sitesPredTrain)
sum(metricsPredTrain == Tclasses) / length(metricsPredTrain)
sum(ENSpredTrain == Tclasses) / length(ENSpredTrain)

#analyse ensemble vs each data set prediction
sum(devicesPredVal == Vclasses) / length(devicesPredVal)
sum(sitesPredVal == Vclasses) / length(sitesPredVal)
sum(metricsPredVal == Vclasses) / length(metricsPredVal)
sum(ENSpredVal == Vclasses) / length(ENSpredVal)

#analyse the variables used in the randomForest
hist(t(varUsed(metricsThetaTrain, by.tree = FALSE, count = TRUE)))

plot(devicesThetaTrain, type = "l", main = deparse(substitute(devicesThetaTrain)))

max(devicesThetaTrain$err.rate[, 1])
min(devicesThetaTrain$err.rate[, 1])
mean(devicesThetaTrain$err.rate[, 1])
sd(devicesThetaTrain$err.rate[, 1])

max(sitesThetaTrain$err.rate[, 1])
min(sitesThetaTrain$err.rate[, 1])
mean(sitesThetaTrain$err.rate[, 1])
sd(sitesThetaTrain$err.rate[, 1])

max(metricsThetaTrain$err.rate[, 1])
min(metricsThetaTrain$err.rate[, 1])
mean(metricsThetaTrain$err.rate[, 1])
sd(metricsThetaTrain$err.rate[, 1])

max(ENSthetaTrain$err.rate[, 1])
min(ENSthetaTrain$err.rate[, 1])
mean(ENSthetaTrain$err.rate[, 1])
sd(ENSthetaTrain$err.rate[, 1])

ENSthetaTrain$err.rate

#explore rawdata
##summaries
names(rawdata)
summary(rawdata)
class(rawdata)
class(rawdata$X)
class(rawdata$classe)
class(rawdata$new_window)
class(rawdata$user_name)
class(rawdata$raw_timestamp_part_1)
class(rawdata$raw_timestamp_part_2)
class(rawdata$cvtd_timestamp)
class(rawdata$num_window)

#summarise the datetime object
print(unique(thedatetime))

#plot vs timestamp
qplot(rawdata$X, order(rawdata$cvtd_timestamp), colour = rawdata$user_name, xlab = 'Repitions', ylab = 'Ordered time spans', main = 'Repitions per time event')
qplot(rawdata$X, order(rawdata$raw_timestamp_part_1), colour = rawdata$user_name, xlab = 'Repitions', ylab = 'Ordered time spans', main = 'Repitions per time event')
qplot(seq_along(rawdata$X), tapply(rawdata$X, order(rawdata$cvtd_timestamp)), colour = rawdata$user_name, xlab = 'Repitions', ylab = 'Ordered time spans', main = 'Repitions per time event')
plot(seq_along(rawdata$X), tapply(rawdata$X, order(rawdata$cvtd_timestamp)))




###End exploration code