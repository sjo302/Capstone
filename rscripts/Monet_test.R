setwd('D:/NYU/Classes/capstone/project')

source('code/Monet.R')
library(data.table)
ecol = readRDS('data/Emergent_Colectomy.rds')

mn = Monet$new(raw_data = ecol, experiment_name = 'My First Experiment', verbose = TRUE)

mn$save_schema('Schema_Testing_Meeting.xlsx')


mn


mn = Monet$new(raw_data = as.data.table(1),  verbose = TRUE)




colnames(ecol)[which(sapply(ecol, function(x) length(unique(x))) == nrow(ecol))]

## add in check for not full lenght input

####################################
### case_id tests
####################################
source('Monet.R')


## case_id is NULL, autosearch with only one item
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE)
mn
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_id = NULL)
mn

## case_id is NULL, autosearch with two valid columns
ecol2 = ecol
ecol2$case_id2 = ecol2$CASEID
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE)
mn

## case_id is column names
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = 'CASEID')
mn

## case_id is wrong column name
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = 'CASEIDs')

## case_id is a valid column name that fails to validate
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = 'APPROACH_SIMP')

## case_id is non-character vector
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = 1)

## case_id is non-character vector
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = c(1,2,3,4))

## case_id is a vector in raw_data
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = as.character(ecol$CASEID))
mn
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_id = as.character(ecol$CASEID))
mn

## case_id is a vector not in raw_data
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = rev(as.character(ecol$CASEID)))
mn


## case_id is not a vector in raw_data and has dupes
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_id = as.character(rep(ecol$CASEID[1], nrow(ecol))))

## Supplying name that has duplicate values
ecol2$case_dupe = 'test'
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_id = 'case_dupe')

## case_id is a full length vector in raw_data and has dupes
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_id = ecol2$case_dupe)

mn$case_id

####################################
### case_time tests
####################################

source('Monet.R')

## case_time is NULL, autosearch with no valid columns
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_id = NULL)


## case_time is NULL, autosearch with only one item
ecol$date = as.Date(Sys.Date())
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_time = NULL)
mn
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE)
mn

## case_time is NULL, autosearch with two valid columns
ecol2 = ecol
ecol2$date2 = ecol2$date
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE)
mn

## case_time is in column names
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = 'date')
mn

## case_time is wrong column name
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = 'APPROACH_SIMP')
mn

## case_time is non-character vector
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = 1)

## case_time is non-character vector
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = c(1,2,3,4))

## case_time is a vector in raw_data
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = ecol$date)
mn
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_time = ecol$date)
mn

## case_time is a full length vector not in raw_data
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = rep(Sys.Date()+1, nrow(ecol2)))
mn

## case_time is a non-date vector not in raw_data
mn = Monet$new(raw_data = ecol2, experiment_name = 'debug', verbose = TRUE, case_time = c(1:nrow(ecol2)))
mn


mn$case_time

####################################
### treatment tests
####################################

source('Monet.R')

## treatment is NULL, set all patients to control
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = NULL)
mn
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE)
mn


## treatment is in column names
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = 'APPROACH_SIMP')
mn

## treatment is wrong column name
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = 'CASEIDs')

## treatment is non-character vector
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = 2)
mn

## treatment is a vector in raw_data
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = ecol$APPROACH_SIMP)
mn

## treatment is a full length vector not in raw_data
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = rep(Sys.Date()+1, nrow(ecol2)))
mn

mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, treatment = rep('OPEN', nrow(ecol2)))
mn


mn$treatment

#########################################
#### Variable Testing
#########################################

source("Monet.R")
var = Variable$new(raw_data = ecol, var_name = 'SEX', keep = T, class = 'character')
var

var = Variable$new(raw_data = ecol, var_name = 'PRALBUM', keep = T, class = 'numeric')
var



vars = lapply(c(1:10), function(x) Variable$new(raw_data = ecol, var_name = colnames(ecol)[x]))




#########################################
#### Variable Mapping Testing
#########################################

source("Monet.R")
var = Variable$new(raw_data = ecol, var_name = 'SEX', keep = T, class = 'character')
var
#var$raw_values
var$mapping = NULL

source("Monet.R")
var = Variable$new(raw_data = ecol, var_name = 'SEX', keep = T, class = 'logical')
var
var$raw_values
var$values
var$mapping = NULL

cat(paste0('Select Value to Map:\n', paste0('[', index_vals, ']', uni_vals, '(n = ', val_amounts, ', ', val_props, '%)\n', collapse = ''),'[q] quit\n'))

source("Monet.R")
var = Variable$new(raw_data = ecol, var_name = 'SEX')

source("Monet.R")
var = Variable$new(raw_data = ecol, var_name = 'CPT')

source("Monet.R")
var = Variable$new(raw_data = ecol, var_name = 'CASEID')

source("Monet.R")
schema = Schema$new(raw_data = ecol[,1:5])

schema$keeps
schema$classes
schema$names
schema$validate()

schema$mapping_table

schema$variable_table

schema$save()

## merge select value and quit
## if no keep, set class to unknown and move on
## after map, show variables

source("Monet.R")
schema = Schema$new(file = 'zg_test2.xlsx')
schema$data
schema$save()

file = 'Schema_2021-08-17_11-18-44.xlsx'
as.data.table(read_excel(path = file, sheet = "Raw_Data"))


source("Monet.R")
mn = Monet$new(raw_data = ecol[,49:52], experiment_name = 'debug', verbose = TRUE, case_id = as.character(ecol$CASEID))

source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'debug', verbose = TRUE, case_id = as.character(ecol$CASEID))


source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'debug', verbose = TRUE, schema = 'Schema_2021-08-17_11-18-44.xlsx')
mn
mn$data

mn$save_schema('test.xlsx')



source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:50], experiment_name = 'debug', verbose = TRUE, case_id = 'CASEID')
mn
mn$data
mn$save_schema('Schema_Testing_Meeting.xlsx')

source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:50], experiment_name = 'debug', verbose = TRUE, schema = 'Schema_Testing_Meeting.xlsx')
mn
mn$data

source("Monet.R")
mn = Monet$new(raw_data = ecol, experiment_name = 'debug', verbose = TRUE, case_id = 'CASEID')
mn
mn$data

numeric_NA_values = 1

for (col in colnames(x)){
  print(col)
  temp = x[[col]]
  if(class(temp) == 'numeric'){
    temp[temp %in% numeric_NA_values] = NA
    x[[col]] = temp
  }
}


source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:50], experiment_name = 'debug', verbose = TRUE, schema = 'Schema_Testing_Meeting.xlsx', numeric_NA_values = c(31, 40))
mn
mn$data

clean_data = mn$data

source("Monet.R")
e2 = as.data.table(list('CaseID' = ecol$CASEID, 'CASEID' = ecol$CASEID))
mn = Monet$new(raw_data = e2, verbose = T)



source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:50], experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'Schema_Testing_Meeting.xlsx', numeric_NA_values = c(74))
mn
mn$data
clean_data = mn$data

mn$report(y = 'SMOKE')



library(AEDA)
clean_data$DIABETES = as.factor(clean_data$DIABETES)
fastReport(data = clean_data, target = "DIABETES")


##################################################
## Testing disallow duplicate variable names
###################################################


ecol$caseid = ecol$CASEID

source("Monet.R")
mn = Monet$new(raw_data = ecol[,c(1,391)], experiment_name = 'My First Report', verbose = TRUE)

coln = toupper(colnames(ecol[,c(1,391)]))
dups = duplicated(coln)
any(dups)
coln[dups]
dup_id = which(coln %in% coln[dups])
nms = colnames(ecol)[dup_id]
groups = lapply(coln[dups], function(x) nms[toupper(nms) == x])
mes = paste(sapply(groups, function(x) paste(x, collapse = ' - ')), collapse = '\n\t')
mes = paste('Duplicate Columns:\n\t', mes, sep = '')
cat(mes)

##################################################
## Testing adding in notes
###################################################

source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'debug', verbose = TRUE, case_id = as.character(ecol$CASEID))

#mn$save_schema('notes_test.xlsx')

mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'notes_test.xlsx')
mn

mn$save_schema('notes2.x')

mn$schema$vars


source("Monet.R")
mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'debug', verbose = TRUE, case_id = as.character(ecol$CASEID), notes = T)

mn$save_schema('notes2_test.xlsx')

mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'notes2_test.xlsx')
mn
mn$schema$vars


mn = Monet$new(raw_data = ecol[,1:5], experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'notes2_test.xlsx', notes = T)
mn
mn$schema$vars
mn$schema$variable_table



##################################################
## Testing Stat tester
##################################################

ecol_test = ecol[,c('CASEID', 'SEX', 'RACE', 'ANESTHES', 'SMOKE', 'WEIGHT', 'HXCHF', 'RENAFAIL', 'PRBUN', 'APPROACH_SIMP')]

source("Monet.R")
mn = Monet$new(raw_data = ecol_test, experiment_name = 'debug', verbose = TRUE, treatment = 'APPROACH_SIMP')

mn$save_schema('notes2_test.xlsx')

mn = Monet$new(raw_data = ecol_test, experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'notes2_test.xlsx', treatment = 'APPROACH_SIMP')

mn

ttest_fisher = mn$compare_groups(treatment_col = 'APPROACH_SIMP', treatment_indicator1 = 'MIS', treatment_indicator2 = 'OPEN', 
                  file_name = 'CRGMeeting.html', num_test_indicator = 'ttest', pretty = T)

ttest_fisher = mn$compare_groups(treatment_indicator1 = 'MIS', treatment_indicator2 = 'OPEN', 
                                 file_name = 'compare3.html', num_test_indicator = 'ttest', pretty = T)


ttest_fisher = mn$compare_groups(file_name = 'compare4.html', num_test_indicator = 'ttest', pretty = T)


ttest_fisher = mn$compare_groups(file_name = 'compare5', num_test_indicator = 'ttest', pretty = T)


ttest_fisher = mn$compare_groups(file_name = c('compare6.html', 'compare5'), num_test_indicator = 'ttest', pretty = T)


#ttest_fisher = mn$compare_groups(file_name = c(1,3,4,5), num_test_indicator = 'ttest', pretty = T)

##################################################
## Testing Volcano Plot
##################################################


source("Monet.R")

mn = Monet$new(raw_data = ecol_test, experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'notes2_test.xlsx', treatment = 'APPROACH_SIMP')

mn

ttest_fisher = mn$compare_groups( num_test_indicator = 'ttest', pretty = T)


p1 = mn$volcano_plots(ttest_fisher, file = 'plot.pdf')

##################################################
## Testing one hot encoding
##################################################


source("Monet.R")

mn = Monet$new(raw_data = ecol_test, experiment_name = 'My First Report', verbose = TRUE, 
               schema = 'notes2_test.xlsx', treatment = 'APPROACH_SIMP')

mn

mn$data

mn$one_hot_data


##################################################
## Testing Date formating
##################################################


source("Monet.R")

mn = Monet$new(raw_data = ecol[,c('YRDEATH', 'CASEID')], experiment_name = 'Timing', verbose = TRUE)

mn$save_schema('date_test.xlsx')

mn2 = Monet$new(raw_data = ecol[,c('YRDEATH', 'CASEID')], experiment_name = 'My First Report', verbose = TRUE, schema = 'date_test.xlsx')
mn2$schema$vars[['case_time']]$meta = '%Y-%m-%d'
mn2$schema$vars[['case_time']]

mn2$save_schema('date2_test.xlsx')

mn3 = Monet$new(raw_data = ecol[,c('YRDEATH', 'CASEID')], experiment_name = 'My First Report', verbose = TRUE, schema = 'date2_test.xlsx')

