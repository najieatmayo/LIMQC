## Start to use `Longitudinal and Interactive Monitoring of Quality Characteristics (LIM-QC)`

### Overview

This interactive application allows you to explore a longitudinal data across a period of time. Data exploration, QC, trending, correlation, distribution comparison, etc. can be visualized in this app.

### Date range input

Enter date in format yyy-mm-dd or select from calendar.


### Raw Data

#### Choose RData

##### Example data

An example data from a bioinformatics pipeline was compiled and preloaded to the app.

Click on `Raw Data` tab on the menu to overview the data, then go through the rest of the tabs for other visual exploration of the data.


##### Upload your own data 

If you need to upload a customized data, click on **Browse** under **Choose RData:** while still on the `Raw Data` tab. Browse your local folder for a **.RData** file to be uploaded. Once the uploading is complete, all the data summary and visulization QC plots will be updated based on the uploaded data.  

This app requires the following format for your uploaded data.

1. The data is in RData format, with only one data frame named **test** saved in the data.

2. Data frame **test** has to have the following variables:
  + __pdate__ : processing date in date format;
  + __run.ID__: name of each run;
  + __sample.ID__: name of sample;
  + __sample_type__: Sample type (could be clinical patient, lab control, or other controls)
  + __sample.QC__: QC status of each sample "Pass" or "Fail" or other warnings;
  
  
3. Other variables including eg. grouping, other metrics. 

#### Sample Type

Choose Sample Type for further summary.

#### Cols to display
 
Select columns in the data to show in the bottom Sample info table. 

#### Number of runs

Summary number of runs within date range and selected sample type and other filters.

#### Number of samples

Summary number of samples within date range and selected sample type and other filters.

#### Sample info

Sample information of selected columns to display are shown as data table. All the tables can be copied, printed, downloaded (in format of CSV, EXCEL, PDF), or sorted by different columns, with searching ability. 

#### Cont Filter

Filtering on Continuous variables 

#### Cat Filter

Filtering on Categorical variables 

### Data Descr

Tab shows stats of variables that in the data. Start with continuous variables followed by categorical variables. Basic description is given for user to have the genearl idea of the data.

### QC stats

Tab shows stats of samples of selected Sample Type in selected date range with QC warnings in three ways: Dash board, Monthly summary, tabulates of QC warning types in most recent day run, most recent week runs, most recent month runs, most recent year runs and since pipeline went live or the earliest date of the samples. QC warning is a predefined variable with different types output from pipeline data. It can be defined as “Pass” if user data does not have the QC warning output from their bioinformatics pipeline. 

### Graphs

`Trending` Tab shows time series plots per sample, boxplot per run, median per run, moving window average (default 50) for selected samples.  Distribution of the selected metric is shown as histogram and basic summary statistics on the side.

`Correlation` Tab shows scatter plot of any two user selected metrics for user selected sample type. Filtering is available on the metrics. Tooltip annotation (point labeling) can be customized.  Summary of linear regression is printed below plot, if add trend line is toggled. X and Y axis could be log transformed.  

`Comparison` Tab shows Comparison between distributions for selected metric of selected Sample Type. The histograms by “Group by” variables are shown in one plot. Groups could be turned on or off by user to see any combination of the comparison. 

`Multi panel` Tab shows two scatter plots of any three user selected metrics for user selected sample type. Key features include: 
  1) the two plots are interactively bilaterally linked, which means user can select a point or points with lasso or box selection on either plot, the linked sample(s) will be highlighted in red on both plots; 
  2) primary information of the selected samples is listed at the bottom of the plots in format of data table; 
  3) Grouping is available; 
  and 4) Tooltip annotation (point labeling). This is a good tool for data exploration for example outlier identification.  

### Report

To be developed
