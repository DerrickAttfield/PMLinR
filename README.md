---
title: "Week 4 Project Assignment"
author:	"Derrick Attfield"
date:	2017/03/09
output: html_document
---

##Data Management
The data was loaded from the provided .csv  files and explored using the:
summary(), head(), names(), plot() and qplot() R functions.
From this initial exploration 5 sub-classes of data were identified in the data set:
1.	Dates
	Describing the date and time on which the measurements were sourced -> the date data is represented in 3 separate columns:
	a.	Date in character atomic class,
	b.	Datepart1 in integer atomic class,
	c.	Datepart2 in integer atomic class.
3.	Descriptive data
	a.	Username -> in character atomic class, containing the names of the participants.
	b.	X -> in integer atomic class which as far as can be established is a sequential identify number for the observation.
	c.	New Window -> in character atomic class containing yes and no -> the meaning of this variable could not be established.
	d.	Window number -> in integer atomic class, the meaning of this variable could not be established.
4.	Classification data
	a.	Classe -> in integer atomic class.  This variable indicates in which of the 5 ways of doing the barbell lift task the             measures were taken
5.	Measures
    A large number of measurements were recorded during the data collection period.  I elected to separate the data set into 4               subsets based on (a) the device (named deviceData), (b) the site from where the measure was recorded (named sitesData), (c) the         type of metric that was recorded (named metricsData) and finally (d) the statistics (this data set was not used).  All                   variables related to totals and "amplitude" excluded.
Next I explored the data through various plots to develop an overall intuition for the data.  Furthermore, I was able to         identify what appeared to be a correlation between the date and time on which the measures were recorded and the class for which it was labelled (fig. 1).

[Link](https://cloud.githubusercontent.com/assets/22258974/23790574/653abd18-0588-11e7-90ed-3c7aaba7472f.png)
Fig.1

##Analysis and prediction
The goal is to predict to which classe (sic) dumbbell lifts were labelled in an unseen test set.  
Based on the fact that this was clearly a classification problem I elected to use random forests to build a model for prediction.  
The raw data was partitioned into a Train set (75%) and a Validate set (25%) using the sample() function. As mentioned above the 
raw data (for both the Train and Validaet partitions were divided into 3 subsets deviceData set, sitesData set and metricsData set.  
The variables included in the deviceData and sitesData is the same data subset.  A separate random forest using the randomForest() function was built for each of the data sets.  The predictions from these 3 random forests were then again combined to create a ENSData set and applied to a 4 th random forest.  The code to implement the 4 random forests is shown below in code sample 1. 

![Alt text](/temp/Project Report Figures_files/image015.png)
Code sample 1.

Each random forest grew a maximum of 500 trees with mtry at each node ranging from 2 to 5. mtry was explicitly set to 5 on the ensemble random forest. The resultant mean OOB (out-of-bag) error rate for the prediction to identify a specific set of metrics of an observation in the training set as belonging to 1 of the 4 class labels in the raw data set was <0.02 in each of the 3 subsets created and 0 in the ensembled data set (Table.1).

![Alt text](/temp/Project Report Figures_files/image003.png)
Table 1

The overall predictive accuracy was better than 98% from the ensemble random forest. I did not use the tuneRF() function because of the observed 0 mean OOB error rate but would have, should the OOB error not have been satisfactory.  Furthermore, the deviceData and sitesData set contained the same observations in the 1st build of the complete model which yielded the 0 OOB error rate.  If the OOB error rate was not acceptable the deviceData set would have been recreated combining all measures of each device and the sitesData set would have been recreated combining all the measures of each site through a covariate generation algorithm.

##Variable importance
Different variables had higher importance in predicting each of the classes, the first 5 variables for each class on the devicesData set are presented below as illustration of this observation.  The varying degrees to which these variables are important can also be seen in the graphs for each class (fig. 2) Overall variation in variable importance is shown the figures 3 through 6 Each figure identifies the variable importance in prediction for each of the data sets  including the final ensembled data set.

![Alt text](/temp/Project Report Figures_files/image005.png)
Fig. 2

![Alt text](/temp/Project Report Figures_files/image007.png)
Fig. 3

![Alt text](/temp/Project Report Figures_files/image009.png)
Fig. 4

![Alt text](/temp/Project Report Figures_files/image011.png)
Fig. 5

![Alt text](/temp/Project Report Figures_files/image013.png)
Fig. 6

##Test set prediction
The test set data was loaded and pre-processed in the same manner as described for the training data set. A separate function to execute the prediction on the 4 predictors was written and sourced to R, which allowed an interactive call of a function with the number of the test set observation that was required to be predicted, this made the completion of the assignment quiz simple.  The solution predicted 20 of 20 test examples correctly on submission of the quiz.  The submission grid is intentionally not displayed.
