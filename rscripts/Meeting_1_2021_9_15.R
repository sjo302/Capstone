library(data.table)
setwd("C:/Users/Zoran Gaijc/Desktop/NYU CDS")
source('Monet.R')

## Reading in the Colecotmy Data
data = readRDS('NSQIP_Merged_Tar_Colectomy_2012_2019.rds')

dim(data)

## Removing on of the Duplicate Columns, note that these will likely need to be merged
data$CaseID = NULL

## Launching the data cleaner
mn = Monet$new(raw_data = data[,c(1:10)], experiment_name = 'My First Experiment', verbose = TRUE)

## The Data cleaner object
mn

## Saving the Cleaned Data Object
mn$save_schema('test_schema.xlsx')

## Re-loading in the data from the cleaned object
mn = Monet$new(raw_data = data[,c(1:10)], experiment_name = 'My First Experiment', verbose = TRUE,
               schema = 'test_schema.xlsx')

## Extracting the cleaned data
mn$data

## Extracting the cleaned data (one hot encoded)
mn$one_hot_data

## Example for saving data to file
saveRDS(mn$data, file = 'data_test.rds')

## Create html report for the data
mn$report(output_file = 'report.html', y = 'TRANST', report_title = 'test_report')
