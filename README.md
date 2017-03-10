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
Next I explored the data through various plots to develop an overall intuition for the data.  Furthermore, I was able to identify what appeared to be a correlation between the date and time on which the measures were recorded and the class for which it is labelled 
(fig. 1).

![Image](https://cloud.githubusercontent.com/assets/22258974/23790574/653abd18-0588-11e7-90ed-3c7aaba7472f.png)
Fig.1
