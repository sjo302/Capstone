
#' Blank datatable setup
#'
#' Creates a blank data table with named columns for each of the outputs provided by the stat tester depending on the test type performed.
#'
#' @param test_indicator A string indicating which stat test is being performed. Options are "t", "w", "c", and "f" which stand for ttest, wilcox test, chi-squared test, and fishers test.
#'
#' @return A blank data table with named columns.
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export
blank_setup <-
  function(test_indicator){
    require(data.table)
    if(test_indicator == "t"){
      #Creating the blank data table to populate with data from t.test
      output <- data.table(Var = character(),
                           Bonferroni_Significant = logical(),
                           Bonferroni_Adjusted_Pval = numeric(),
                           qval_Significant = logical(),
                           qval = numeric(),
                           FDR_Significant = logical(),
                           FDR_Adjusted_Pval = numeric(),
                           Nominal_pval = numeric(),
                           meanTreat1 = numeric(),
                           meanTreat2 = numeric(),
                           Difference_In_Means = numeric(),
                           Ci_95_Lower = numeric(),
                           Ci_95_Upper = numeric(),
                           Hedges_g = numeric(),
                           Hedges_g_Ci_95_Lower = numeric(),
                           Hedges_g_Ci_95_Upper = numeric(),
                           t_Stat = numeric(),
                           Std_Error = numeric(),
                           Degrees_of_Freedom = numeric(),
                           Null_Val = numeric(),
                           Alt = character(),
                           Method = character(), 
                           stringsAsFactors = FALSE)
    }
    else if(test_indicator == "w"){
      #Creating the blank data table to populate with data from wilcox.test
      output <- data.table(Var = character(),
                           Bonferroni_Significant = logical(),
                           Bonferroni_Adjusted_Pval = numeric(),
                           qval_Significant = logical(),
                           qval = numeric(),
                           FDR_Significant = logical(),
                           FDR_Adjusted_Pval = numeric(),
                           Nominal_pval = numeric(),
                           Ci_95_Lower = numeric(),
                           Ci_95_Upper = numeric(),
                           Hedges_g = numeric(),
                           Hedges_g_Ci_95_Lower = numeric(),
                           Hedges_g_Ci_95_Upper = numeric(),
                           w_Stat = numeric(),
                           #This is the estimate
                           Location_Shift = numeric(),
                           Null_Val = numeric(),
                           Alt = character(),
                           Method = character(), 
                           stringsAsFactors = FALSE)
    }
    else if(test_indicator == "c"){
      #Creating the blank data table to populate with data from chi-squared test
      output <- data.table(Column_of_Interest = character(),
                           Var_Indicator = character(),
                           Bonferroni_Significant = logical(),
                           Bonferroni_Adjusted_Pval = numeric(),
                           qval_Significant = logical(),
                           qval = numeric(),
                           FDR_Significant = logical(),
                           FDR_Adjusted_Pval = numeric(),
                           Nominal_pval = numeric(),
                           indicatorObserved1 = numeric(),
                           indicatorObserved2 = numeric(),
                           otherObserved1 = numeric(),
                           otherObserved2 = numeric(),
                           indicatorExpected1 = numeric(),
                           indicatorExpected2 = numeric(),
                           otherExpected1 = numeric(),
                           otherExpected2 = numeric(),
                           Odds_Ratio = numeric(),
                           chi_Statistic = numeric(),
                           Degrees_of_Freedom = numeric(),
                           Method = character(),
                           stringsAsFactors = FALSE)
    }
    else if(test_indicator == "f"){
      #Creating the blank data table to populate with data from fisher test
      output <- data.table(Column_of_Interest = character(),
                           Var_Indicator = character(),
                           Bonferroni_Significant = logical(),
                           Bonferroni_Adjusted_Pval = numeric(),
                           qval_Significant = logical(),
                           qval = numeric(),
                           FDR_Significant = logical(),
                           FDR_Adjusted_Pval = numeric(),
                           Nominal_pval = numeric(),
                           sum1has = numeric(),
                           sum1hasnot = numeric(),
                           sum2has = numeric(),
                           sum2hasnot = numeric(),
                           Odds_Ratio = numeric(),
                           Odds_Ratio_Ci_95_Lower = numeric(),
                           Odds_Ratio_Ci_95_Upper = numeric(),
                           Degrees_of_Freedom = numeric(),
                           Null_Val = numeric(),
                           Alt = character(),
                           Method = character(),
                           stringsAsFactors = FALSE)
    }
    return(output)
  }

#' Numeric results
#'
#' Executes either a two sample t.test or a two sample Wilcoxon Ranked Sum Test as well as Hedges G for effect size on a given data set. Returns a vector with relevant results that should ideally be bound into the data table from blank setup. Should only be used with stat_tester. Welch's t-test is performed by default for ttest which accounts for unequal variance between samples. If ignore_wilcox_duplicate is FALSE and the column being tested has repeated values, a small amount of "noise" is added to each observation and then both tests are performed. The "noise" is a randomly sampled value from a uniform distribution with a minimum of 0 and a maximum of the minimum value in the column divided by 1,000,000. This is done so the Wilcox test is able to correctly rank each value without omitting large amounts of duplicate observations.
#'
#' @param data A data table
#' @param test_indicator A string indicating which stat test to perform. Options are "t" and "w" which stand for ttest and wilcox test respectively.
#' @param var_exec A string specifying a column name from data that the test and Hedges G will be performed on.
#' @param treatment_col A string specifying a column name from data that contains the treatment indicators.
#' @param treatment_indicator1 A string or integer specifying the first sample's treatment. Must be an observation in treatment_col.
#' @param treatment_indicator2 A string or integer specifying the second sample's treatment. Must be an observation in treatment_col.
#' @param digits An optional integer specifying the number of digits to return, 3 by default.
#' @param modify_wilcox_duplicates An optional logical specifying whether or not to make each observation unique so the wilcox test can correctly rank observations. FALSE by default.
#'
#' @return A vector containing the results of either ttest or wilcox test and Hedges G alongside other relevant information.
#'
#' @examples
#' numeric_results(data = data, test_indicator = "t", var_exec = "AGE", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN")
#'
#' numeric_results(data = data, test_indicator = "t", var_exec = "AGE", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", digits = 4, modify_wilcox_duplicates = T)
#'
#' @import data.table
#' @import effectsize
#'
#' @author Ethan Assouline
#' @export

numeric_results <-
  function(data, test_indicator, var_exec, treatment_col, treatment_indicator1, treatment_indicator2, 
           digits = 3, modify_wilcox_duplicates = F){
    require(data.table)
    require(effectsize)
    if (test_indicator == "t"){
      #Performing the t-test. The Welch t-test is performed by default which accounts for unequal variance between samples
      tresult <- t.test(x = as.numeric(data[data[[treatment_col]] == treatment_indicator1][[var_exec]]),
                        y = as.numeric(data[data[[treatment_col]] == treatment_indicator2][[var_exec]]))
      #Hedges G for effect size
      hedgesG <- hedges_g(x = as.numeric(data[data[[treatment_col]] == treatment_indicator1][[var_exec]]), 
                          y = as.numeric(data[data[[treatment_col]] == treatment_indicator2][[var_exec]]))
      result <- vector(mode = "list", length = 22) #Initializing an empty list to store results
      result[1] <- var_exec
      result[2] <- "bonferroni significant"
      result[3] <- "bonferroni adj pval"
      result[4] <- "qval significant"
      result[5] <- "qval"
      result[6] <- "fdr significant"
      result[7] <- "fdr adj pval"
      result[8] <- format(tresult$p.value, digits = digits, scientific = T)
      result[9] <- format(tresult$estimate[1], digits = digits, scientific = F) #Means of each group
      result[10] <- format(tresult$estimate[2], digits = digits, scientific = F)
      result[11] <- format(tresult$estimate[1] - tresult$estimate[2], digits = digits, scientific = F) #Difference in means
      result[12] <- format(tresult$conf.int[1], digits = digits, scientific = F) #Diff-Mean Confidence Interval
      result[13] <- format(tresult$conf.int[2], digits = digits, scientific = F)
      result[14] <- format(hedgesG$Hedges_g, digits = digits) #Hedges G
      result[15] <- format(hedgesG$CI_low, digits = digits) #Hedges G confidence interval
      result[16] <- format(hedgesG$CI_high, digits = digits)
      result[17] <- format(tresult$statistic, digits = digits) #T-Stat
      result[18] <- format(tresult$stderr, digits = digits) #Standard Error
      result[19] <- format(tresult$parameter, digits = T) #Degrees of freedom
      result[20] <- tresult$null.value
      result[21] <- tresult$alternative
      result[22] <- tresult$method
    }
    else if (test_indicator == "w"){
      set.seed(42)
      if (modify_wilcox_duplicates){ #Checking if duplicates should be ignored
        #Adding an extremely small amount of "noise" to the observations to perform the ranked sums test without dropping duplicate data
        data[["NOISE"]] <- runif(nrow(data), min = 0, max = min(data[[var_exec]], na.rm = T)/1000000)
        data[["RESULTWNOISE"]] <- as.numeric(data[[var_exec]]) + data[["NOISE"]]
        wilcoxresult <- wilcox.test(x = as.numeric(data[data[[treatment_col]] == treatment_indicator1][["RESULTWNOISE"]]), 
                                    y = as.numeric(data[data[[treatment_col]] == treatment_indicator2][["RESULTWNOISE"]]),
                                    conf.int = T)
      }
      else{
        if (length(unique(data[["var_exec"]])) != nrow(data)){ #Checking if each value is unique
          warning("Data has duplicate observations which might affect the result of the Wilcoxon Ranked Sum Test.")
        }
        wilcoxresult <- wilcox.test(x = as.numeric(data[data[[treatment_col]] == treatment_indicator1][[var_exec]]), 
                                    y = as.numeric(data[data[[treatment_col]] == treatment_indicator2][[var_exec]]),
                                    conf.int = T)
      }
      #Calculating Hedges G
      hedgesG <- hedges_g(x = as.numeric(data[data[[treatment_col]] == treatment_indicator1][[var_exec]]), 
                          y = as.numeric(data[data[[treatment_col]] == treatment_indicator2][[var_exec]]))
      result <- vector(mode = "list", length = 18)
      result[1] <- var_exec
      result[2] <- "bonferroni significant"
      result[3] <- "bonferroni adj pval"
      result[4] <- "qval significant"
      result[5] <- "qval"
      result[6] <- "fdr significant"
      result[7] <- "fdr adj pval"
      result[8] <- format(wilcoxresult$p.value, digits = digits) #P-value
      result[9] <- format(wilcoxresult$conf.int[1], digits = digits, scientific = F) #Confidence interval
      result[10] <- format(wilcoxresult$conf.int[2], digits = digits, scientific = F)
      result[11] <- format(hedgesG$Hedges_g, digits = digits) #Hedges G
      result[12] <- format(hedgesG$CI_low, digits = digits) #Hedges G confidence interval
      result[13] <- format(hedgesG$CI_high, digits = digits)
      result[14] <- wilcoxresult$statistic #Wilcox stat
      result[15] <- format(wilcoxresult$estimate, digits = digits, scientific = F) #Estimate of location shift
      result[16] <- wilcoxresult$null.value
      result[17] <- wilcoxresult$alternative
      result[18] <- wilcoxresult$method
    }
    else{
      return("Please specify a valid test")
    }
    return(result)
  }


#' Numeric Statistical Tester
#'
#' Executes either a two sample T-test or a two sample Wilcoxon Ranked Sum Test alongside Hedges G for effect size on the given data and returns a data table with relevant results.
#'
#' @param data A data table
#' @param vars_of_interest A vector of strings specifying which columns from data to perform the numerical statistical test on.
#' @param treatment_col A string specifying a column name from data that contains the treatment indicators.
#' @param treatment_indicator1 A string or integer specifying the first treatment. Must be an observation in treatment_col.
#' @param treatment_indicator2 A string or integer specifying the second treatment. Must be an observation in treatment_col.
#' @param test_indicator A string specifying which statistical test to execute. Either "ttest" or "wilcoxtest".
#' @param digits An optional integer specifying the number of digits to return, 3 by default.
#' @param modify_wilcox_duplicates An optional logical specifying whether or not to ignore duplicate values for the Wilcoxon Ranked Sum Test, FALSE by default.
#'
#' @return A data table with the results from either ttest_results or wilcox_results.
#'
#' @examples
#' numeric_stat_tester(data = data, vars_of_interest = c("AGE", "WEIGHT"), treatment_col = "APPROACH_SIMP", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", test_indicator = "ttest")
#'
#' numeric_stat_tester(data = data, vars_of_interest = c("AGE", "WEIGHT"), treatment_col = "APPROACH_SIMP", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", test_indicator = "ttest", digits = 4)
#'
#' numeric_stat_tester(data = data, vars_of_interest = c("AGE", "WEIGHT"), treatment_col = "APPROACH_SIMP", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", test_indicator = "wilcoxtest", digits = 5, modify_wilcox_duplicates = TRUE)
#'
#' @import data.table
#' @import effectsize
#'
#' @author Ethan Assouline
#' @export

numeric_stat_tester <-
  function(data, test_indicator, vars_of_interest, treatment_col, treatment_indicator1, 
           treatment_indicator2, digits = 3, modify_wilcox_duplicates = F){
    require(data.table)
    require(effectsize)
    test_indicator <- tolower(strsplit(test_indicator,"")[[1]][1]) #Checking user specified test
    #Checking if valid test
    if ((test_indicator != "t") & (test_indicator != "w")){
      return ("Please specify a valid test")
    } 
    output <- blank_setup(test_indicator)
    #For every variable specified in vars_of_interest
    output <- rbindlist(lapply(vars_of_interest, function(var_exec){
      #Calculating the results of the test depending on the indicator
      result <- numeric_results(data, test_indicator, var_exec, treatment_col, treatment_indicator1, 
                                treatment_indicator2, digits, modify_wilcox_duplicates)
      #Binding the result to the dataframe
      output <- rbind(output, result, use.names = F)
    }))
    return(output)
  }

#' Categorical Results
#'
#' Executes either a 2x2 Chi-Squared Test of Independence with continuity correction or a 2x2 Fisher's Exact Test for the count of an observation against the count of all other observations in a given column. Also calculates the odds ratio.
#'
#' @param var_exec A string specifying a column name that the chi squared is being performed on.
#' @param var_indicator_index An integer specifying an index to get the observation name from var_count_table.
#' @param var_count_table A table of the counts of all possible observations in the specified column.
#' @param cat_matrix A 2x2 matrix comparing the counts of an observation against all other observations in that column.
#' @param digits An optional integer specifying the number of digits to return, 3 by default.
#'
#' @return A vector containing the results of either chisquared or fishers test and the odds ratio alongside other relevant information.
#'
#' @examples
#' categorical_results(test_indicator = "f", var_exec = "DIABETES", var_indicator_index = 1, var_count_table = categorical_count_table, cat_matrix = categorical_var_matrix, digits = 5)
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

categorical_results <-
  function(test_indicator, var_exec, var_indicator_index, var_count_table, cat_matrix, digits = 3){
    require(data.table)
    if (test_indicator == "c"){
      chiresult <- chisq.test(cat_matrix, correct = TRUE) #Performing the chi-squared test with continuity correction
      result <- vector(mode = "list", length = 21)
      result[1] <- var_exec
      result[2] <- var_count_table[var_indicator_index,1] #Indicator
      result[3] <- "bonferroni significant"
      result[4] <- "bonferroni adj pval"
      result[5] <- "qval significant"
      result[6] <- "qval"
      result[7] <- "fdr significant"
      result[8] <- "fdr adj pval"
      result[9] <- format(chiresult$p.value, digits = digits, scientific = T)
      result[10] <- cat_matrix[1,1] #Observed values
      result[11] <- cat_matrix[1,2]
      result[12] <- cat_matrix[2,1]
      result[13] <- cat_matrix[2,2]
      result[14] <- format(chiresult$expected[1,1], digits = digits) #Expected values from the chi-squared
      result[15] <- format(chiresult$expected[1,2], digits = digits)
      result[16] <- format(chiresult$expected[2,1], digits = digits)
      result[17] <- format(chiresult$expected[2,2], digits = digits)
      result[18] <- (cat_matrix[1]/cat_matrix[2])/(cat_matrix[3]/cat_matrix[4]) #Odds ratio
      result[19] <- chiresult$statistic #Chisq stat
      result[20] <- chiresult$parameter #Degrees of Freedom
      result[21] <- chiresult$method
    }
    else if (test_indicator == "f"){
      fisherresult <- fisher.test(cat_matrix)
      result <- vector(mode = "list", length = 20)
      result[1] <- var_exec #Column
      result[2] <- var_count_table[var_indicator_index,1] #Variable
      result[3] <- "bonferroni significant"
      result[4] <- "bonferroni adj pval"
      result[5] <- "qval significant"
      result[6] <- "qval"
      result[7] <- "fdr significant"
      result[8] <- "fdr adj pval"
      result[9] <- format(fisherresult$p.value, digits = digits, scientific = T)
      result[10] <- cat_matrix[1,1]
      result[11] <- cat_matrix[1,2]
      result[12] <- cat_matrix[2,1]
      result[13] <- cat_matrix[2,2]
      result[14] <- format(fisherresult$estimate, digits = digits) #Odds ratio
      result[15] <- format(fisherresult$conf.int[1], digits = digits)
      result[16] <- format(fisherresult$conf.int[2], digits = digits)
      result[17] <- ncol(cat_matrix) - 1 #Degrees of Freedom
      result[18] <- fisherresult$null.value
      result[19] <- fisherresult$alternative
      result[20] <- fisherresult$method
    }
    else{
      return("Please specify a valid test")
    }
    return(result)
  }


#' Output Class Format
#'
#' Formats the column classes of the output of stat_tester.
#'
#' @param output A data table with the results from stat_tester
#' @param test_indicator A character specifying which statistical test was executed. Must be "t", "w", "c", or "f".
#'
#' @return A data table with the same shape and results as output with correct column classes.
#'
#' @examples
#' output_class_format(output = output, test_indicator = "t")
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

output_class_format <-
  function(output, test_indicator){
    if (test_indicator == "t"){
      numeric_columns <- c("Bonferroni_Adjusted_Pval", "qval", "FDR_Adjusted_Pval", "Nominal_pval", 
                           "meanTreat1","meanTreat2", "Difference_In_Means", "Ci_95_Lower", "Ci_95_Upper", 
                           "Hedges_g", "Hedges_g_Ci_95_Lower", "Hedges_g_Ci_95_Upper", "t_Stat", "Std_Error", 
                           "Degrees_of_Freedom")
    }
    else if (test_indicator == "w"){
      numeric_columns <- c("Bonferroni_Adjusted_Pval", "qval", "FDR_Adjusted_Pval", "Nominal_pval", 
                           "Ci_95_Lower", "Ci_95_Upper", "Hedges_g", "Hedges_g_Ci_95_Lower", 
                           "Hedges_g_Ci_95_Upper", "w_Stat", "Location_Shift")
    }
    else if (test_indicator == "c"){
      numeric_columns <- c("Bonferroni_Adjusted_Pval", "qval", "FDR_Adjusted_Pval", "Nominal_pval", 
                           "indicatorObserved1", "indicatorObserved2", "otherObserved1", "otherObserved2", 
                           "indicatorExpected1", "indicatorExpected2", "otherExpected1", "otherExpected2", 
                           "Odds_Ratio","chi_Statistic", "Degrees_of_Freedom")
    }
    else{
      numeric_columns <- c("Bonferroni_Adjusted_Pval", "qval", "FDR_Adjusted_Pval", "Nominal_pval", 
                           "sum1has","sum1hasnot","sum2has","sum2hasnot", "Odds_Ratio",
                           "Odds_Ratio_Ci_95_Lower", "Odds_Ratio_Ci_95_Upper", "Degrees_of_Freedom")
    }
    #Changing specified columns to numeric
    output[, numeric_columns] <- lapply(numeric_columns, function(x) as.numeric(output[[x]])) 
    return(output)
  }

#' Output Name Format
#'
#' Formats the column classes of the output of stat_tester
#'
#' @param output A data table with the results from stat_tester.
#' @param test_indicator A character specifying which statistical test was executed. Must be "t", "w", "c", or "f".
#' @param treatment_indicator1 A string or integer specifying the first treatment.
#' @param treatment_indicator2 A string or integer specifying the second treatment.
#' @param padjmethod A string specifying what method was used when calculating the p-adjusted value.
#'
#' @return A data table with the same shape and results as output with correct column classes.
#'
#' @examples
#' output_class_format(output = output, test_indicator = "t", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", padjmethod = "bonferroni")
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

output_name_format <-
  function(output, test_indicator, treatment_indicator1, treatment_indicator2){
    if (test_indicator == "t"){
      setnames(output,c("meanTreat1", "meanTreat2"), 
               c(paste("Mean", treatment_indicator1, sep = "_"), 
                 paste("Mean", treatment_indicator2, sep = "_")))
    }
    else if (test_indicator == "c"){
      setnames(output, c("indicatorObserved1","indicatorObserved2","otherObserved1","otherObserved2",
                         "indicatorExpected1", "indicatorExpected2", "otherExpected1", "otherExpected2"), 
               c(paste("Indicator Observed",treatment_indicator1, sep = "_"),
                 paste("Indicator Observed",treatment_indicator2, sep = "_"), 
                 paste("Other Observed", treatment_indicator1, sep = "_"),
                 paste("Other Observed",treatment_indicator2, sep = "_"),
                 paste("Indicator Expected", treatment_indicator1, sep = "_"),
                 paste("Indicator Expected", treatment_indicator2, sep = "_"),
                 paste("Other Expected", treatment_indicator1, sep = "_"),
                 paste("Other Expected", treatment_indicator2, sep = "_")))
    }
    else if (test_indicator == "f"){
      setnames(output,c("sum1has","sum1hasnot","sum2has","sum2hasnot"), 
               c(paste("Indicator",treatment_indicator1, sep = "_"),
                 paste("Indicator",treatment_indicator2, sep = "_"), 
                 paste("Other",treatment_indicator1, sep = "_"), 
                 paste("Other",treatment_indicator2, sep = "_")))
    }
    return(output)
  }


#' Categorical Count Table
#'
#' Given a data frame or data table and a column name, counts the number of observations in the column subsetted on specified observations from another column.
#'
#' @param data A data frame or data table.
#' @param var_exec A string specifying a column name from data that the counts will be performed on.
#' @param treatment_col A string specifying a column name from data that contains the treatment indicators.
#' @param treatment_indicator1 A string or integer specifying the first sample's treatment. Must be an observation in treatment_col.
#' @param treatment_indicator2 A string or integer specifying the second sample's treatment. Must be an observation in treatment_col.
#'
#' @return A data table with 3 columns containing the observation names and the subsetted counts for both treatment indicators.
#'
#' @examples
#' categorical_count_table(data = data, var_exec = "DIABETES", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN")
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

categorical_count_table <-
  function(data, var_exec, treatment_col, treatment_indicator1, treatment_indicator2){
    require(data.table)
    #Creating the counts for each observations
    var_count_table <- data.frame(unclass(table(data[[var_exec]], data[[treatment_col]])))
    var_count_table[["COL VAR INDICATOR"]] <- rownames(var_count_table) #Creating a new column with the variable names
    var_count_table <- var_count_table[c("COL VAR INDICATOR", treatment_indicator1, treatment_indicator2)] #Renaming columns
    var_count_table <- as.data.table(var_count_table) #Output as a data table
    return(var_count_table)
  }


#' Categorical Variable Matrix
#'
#' Changes a data table from categorical_count_table into a 2x2 matrix of the counts of one observation against all other observations subsetted on the specified treatment indicators.
#'
#' @param var_indicator_index An integer specifying an index to get the data from var_count_table.
#' @param var_count_table A data table of each observation and their counts subsetted by treatment indicators.
#' @param treatment_indicator1 A string or integer specifying the first treatment column. Must be a column name from var_count_table.
#' @param treatment_indicator2 A string or integer specifying the second treatment column. Must be a column name from var_count_table.
#'
#' @return A 2x2 matrix of the counts of one observation against all other observations subsetted on the specified treatment indicators with rows column names of treatment indicators.
#'
#' @examples
#' categorical_var_matrix(var_indicator_index = 1, var_count_table = var_count_table, treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN")
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

categorical_var_matrix <-
  function(var_indicator_index, var_count_table, treatment_indicator1, treatment_indicator2){
    require(data.table)
    count_table <- var_count_table[FALSE, ] #Blank table with the same shape as var_count_table
    count_table <- rbind(count_table, var_count_table[var_indicator_index, ]) #Adding names 
    count_table <- rbind(count_table, #Calculating the sums of the observations
                         list("OTHER", sum(var_count_table[[treatment_indicator1]][var_count_table[["COL VAR INDICATOR"]] != var_count_table[["COL VAR INDICATOR"]][var_indicator_index]]),
                              sum(var_count_table[[treatment_indicator2]][var_count_table[["COL VAR INDICATOR"]] != var_count_table[["COL VAR INDICATOR"]][var_indicator_index]])))
    count_matrix <- matrix(c(as.numeric(count_table[1,2]),as.numeric(count_table[1,3]),
                             as.numeric(count_table[2,2]),as.numeric(count_table[2,3])), ncol=2, byrow=TRUE)
    colnames(count_matrix) <- c(count_table[1,1],"OTHER") #Naming the columns and rows
    rownames(count_matrix) <- c(treatment_indicator1, treatment_indicator2)
    return(count_matrix)
  }

#' Categorical Statistical Tester
#'
#' Executes either a Chi-Squared Test of Independence or a Fisher's Exact Test for Count Data on specified columns from a data table.
#'
#' @param data A data table.
#' @param vars_of_interest A vector of strings specifying which columns from data to perform the statistical test on.
#' @param treatment_col A string specifying a column name from data that contains the treatment indicators.
#' @param treatment_indicator1 A string or integer specifying the first treatment. Must be an observation in treatment_col.
#' @param treatment_indicator2 A string or integer specifying the second treatment. Must be an observation in treatment_col.
#' @param test_indicator A string specifying which statistical test to execute. Either "chisquared" or "fishertest".
#' @param digits An optional integer specifying the number of digits to return, 3 by default.
#'
#' @return A data table with the results from either chi_results or fisher_results.
#'
#' @examples
#' categorical_stat_tester(data = data, vars_of_interest = c("DIABETES", "COL_INDICATION"), treatment_col = "APPROACH_SIMP",treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", test_indicator = "chisquared")
#'
#' categorical_stat_tester(date = data, vars_of_interest = c("DIABETES", "COL_INDICATION"), treatment_col = "APPROACH_SIMP",treatment_indicator1 = "MIS", treatment_indicator2 = "OPEN", test_indicator = "fishertest", digits = 4)
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

categorical_stat_tester <-
  function(data, test_indicator, vars_of_interest, treatment_col, treatment_indicator1, 
           treatment_indicator2, digits = 3){
    require(data.table)
    test_indicator <- tolower(strsplit(test_indicator,"")[[1]][1]) #Checking the test specified
    output <- blank_setup(test_indicator)
    if ((test_indicator != "c") & (test_indicator != "f")){
      return ("Please specify a valid test")
    }
    output <- rbindlist(lapply(vars_of_interest, function(var_exec){ #For each column in the specified columns
      #Creating the total counts for each indicator
      var_count_table <- categorical_count_table(data, var_exec, treatment_col, treatment_indicator1, treatment_indicator2) 
      #For each index of the indicator in the column of interest
      for(var_indicator_index in 1:length(var_count_table[["COL VAR INDICATOR"]])){
        
        #Creating the matrix that the test will be executed upon
        count_matrix <- categorical_var_matrix(var_indicator_index, 
                                               var_count_table, treatment_indicator1, treatment_indicator2) 
        result <- categorical_results(test_indicator, var_exec, var_indicator_index, var_count_table, count_matrix)
        output <- rbind(output, result, use.names = F)
      }
      return(output)
    }))
    return(output)
  }

#' Remove Duplicate Categoricals
#'
#' Removes FALSE observations from the test result table provided from stat_tester when only TRUE/FALSE are present. Used because categorical tests that only have TRUE/FALSE observations just have inverse observations.
#'
#' @param data A data table of the data the test was run on.
#' @param test_result A data table of test results from stat_tester. Must be a categorical test.
#'
#' @return A data table of test results with FALSE observations removed.
#'
#' @examples
#' remove_duplicate_cat(data = data, test_result = fisher)
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

remove_duplicate_cat <-
  function(data, test_result){
    require(data.table)
    test_result$index = c(1:nrow(test_result))
    #browser()
    logi_cols = colnames(data)[which(sapply(data, 'class') %in% c('logical', 'character'))]
    check = test_result[Column_of_Interest %in% logi_cols]
    split_check = split(check, by = 'Column_of_Interest')
    index_remove = lapply(split_check, function(var){
      if(nrow(var) == 2 & length(unique(var$Nominal_pval)) == 1){
        if(FALSE %in% var$Var_Indicator){
          rem = var[Var_Indicator == FALSE]$index
        } else if (0 %in% var$Var_Indicator){
          rem = var[Var_Indicator == 0]$index
        }
        else{
          rem = var[2]$index
        }
        return(rem)
      }
      return(NA)
    })
    output <- test_result[!index %in% index_remove]
    output$index <- NULL
    return(output)
  }


#' Col Class Separator
#'
#' Separates the names of the columns of a data set into numeric column names and non numeric column names.
#'
#' @param data A data table.
#' @param vec_return A string specifying whether to return the numeric or non-numeric column names. Must either be "numeric" or "non-numeric".
#' @param both An optional logical specifying whether to return both lists of numeric and non-numeric names in a single list. FALSE by default.
#'
#' @return 
#'
#' @examples
#' col_class_separator(data = data, vec_return = "numeric")
#' col_class_separator(data = data, vec_return = "numeric", both = TRUE )
#'
#' @import data.table
#'
#' @author Ethan Assouline
#' @export

col_class_separator <-
  function(data, vec_return, both = F){
    require(data.table)
    num_list <- vector() #Initializing empty vector
    num_counter <- 1 #Counter
    non_num_list <- vector()
    non_num_counter <- 1
    class_list <- lapply(data, 'class')
    for (i in 1:ncol(data)){
      ifelse(class_list[i] == "numeric",
             num_list[num_counter] <- names(class_list)[i], #Adding the names to the vectors
             non_num_list[non_num_counter] <- names(class_list)[i])
      ifelse(class_list[i] == "numeric",
             num_counter <- num_counter + 1,
             non_num_counter <- non_num_counter + 1)
    }
    ifelse(both == F, 
           ifelse (vec_return == 'numeric', return(num_list), return(non_num_list)), 
           return(list("numeric" = num_list, "non_numeric" = non_num_list)))
  }

#' Complete Statistical Tester
#'
#' Executes two specified statistical tests on the numeric and non-numeric data in a data table based on the column classes. Options are: a Two Sample T.Test, Two Sample Wilcoxon Ranked Sum Test, Chi-Squared Test of Independence or a Fisher's Exact Test for Count Data. Nominal p-value, Bonferroni adjusted p-value, Fdr adjusted p-value, and q-values are calculated. Numeric statistical tests (T.test & Wilcoxon Test) also calculate Hedges G for effect size. Has functionality to save a formatted table if given a file name or path within the working directory.
#'
#' @param data A data table.
#' @param num_test_indicator A string specifying which numeric statistical test to execute. Must be "ttest" or "wilcoxtest". 
#' @param cat_test_indicator A string specifying which numeric statistical test to execute. Must be "chisquared" or "fishertest".
#' @param treatment_col A string specifying a column name from data that contains the treatment indicators.
#' @param treatment_indicator1 A string or integer specifying the first sample's treatment. Must be an observation in treatment_col.
#' @param treatment_indicator2 A string or integer specifying the second sample's treatment. Must be an observation in treatment_col.
#' @param digits An optional integer specifying the number of digits to return, 3 by default.
#' @param var_specify An optional vector of column names as strings used to omit or keep columns in data. Used in conjunction with remove_var. NA by default.
#' @param remove_var An optional logical specifying whether or not var_specify is a list of columns to remove from the total list of columns of data. FALSE by default.
#' @param modify_wilcox_duplicates An optional logical specifying whether or not to ignore duplicate values for the Wilcoxon Ranked Sum Test, FALSE by default.
#' @param remove_duplicate_logi An optional logical specifying whether to remove duplicate logical variables from the categorical tables. Used when only TRUE and FALSE are present as observations in the column. TRUE by default.
#' @param nComp An optional parameter allowing the user to change the number of comparisons used when calculating the bonferroni and fdr adjusted p-values. Input must be an integer and must be greater than or equal to the base number of comparisons which is the sum of the numeric and non-numeric tests performed.
#' @param bonferroni_threshold An optional double from 0.0-1.0 specifying the threshold for significance for the bonferroni adjusted p-value. 0.05 by default.
#' @param qval_threshold An optional double from 0.0-1.0 specifying the threshold for significance for q-values. 0.1 by default.
#' @param FDR_threshold An optional double from 0.0-1.0 specifying the threshold for significance for the FDR adjusted p-value. 0.1 by default.
#' @param file_name An optional list of strings of size 2 specifying the paths or file names to save the test tables as. If giving only file names, extension must be specified. Saved within the working directory if a full path is not provided. NA by default.
#' @param remove_print An optional vector of the output column names as strings to be omitted from the final printed table. Can only be used when a path is provided. NA by default.
#' @param remove_duplicate_logi An optional logical specifying whether to remove duplicate logical variables from the categorical tables. Used when only TRUE and FALSE are present as observations in the column. TRUE by default.
#' @param titles An optional list of strings of length 2 specifying the title for each test.
#'
#' @return A list of two data tables with the results of the chosen numeric and non-numeric statistical tests. Optionally saves formatted tables if given a file path.
#'
#' @examples
#' stat_tester(data = data, num_test_indicator = "ttest", cat_test_indicator = "fishertest", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "OPEN", treatment_indicator2 = "MIS")
#'
#' stat_tester(data = data, num_test_indicator = "ttest", cat_test_indicator = "fishertest", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "OPEN", treatment_indicator2 = "MIS", digits = 5, modify_wilcox_duplicates = TRUE, nComp = 500)
#'
#' stat_tester(data = data, num_test_indicator = "wilcoxtest", cat_test_indicator = "chisquared", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "OPEN", treatment_indicator2 = "MIS", digits = 5, var_specify = ("CASEID", "DIABETES"), remove_var = TRUE)
#'
#'stat_tester(data = data, num_test_indicator = "wilcoxtest", cat_test_indicator = "chisquared", treatment_col = "APPROACH_SIMP", treatment_indicator1 = "OPEN", treatment_indicator2 = "MIS", digits = 4, bonferroni_threshold = 0.01, qval_threshold = 0.01, fdr_threshold = 0.05 file_name = c("wilcox.html", "chisquared.html"), remove_print = c("nullVal"), remove_duplicate_logi = FALSE)
#'
#' @import data.table
#' @import effectsize
#' @import sjPlot
#' @import BiocManager
#' @import qvalue
#'
#' @author Ethan Assouline
#' @export

stat_tester <-
  function(data, num_test_indicator, cat_test_indicator, treatment_col, treatment_indicator1, treatment_indicator2, 
           digits = 3, var_specify = NA, remove_var = FALSE, modify_wilcox_duplicates = FALSE, 
           remove_duplicate_logi = TRUE, nComp = NA, bonferroni_threshold = 0.05, qval_threshold = 0.1, 
           fdr_threshold = 0.1, file_name, remove_print = NA, titles){
    require(qvalue)
    require(data.table)
    require(effectsize)
    require(sjPlot)
    #Getting the columns to perform the test on
    #Determined by the class of the column
    num_vars_of_interest <- col_class_separator(data, "numeric")
    cat_vars_of_interest <- col_class_separator(data, "non_numeric")
    #Keeping or removing specified columns
    ifelse(is.na(var_specify), num_vars_of_interest <- num_vars_of_interest [!num_vars_of_interest %in% var_specify],
           ifelse(remove_var == TRUE, num_vars_of_interest <- num_vars_of_interest [!num_vars_of_interest %in% var_specify],
                  num_vars_of_interest <- num_vars_of_interest [num_vars_of_interest %in% var_specify]))
    ifelse(is.na(var_specify), cat_vars_of_interest <- cat_vars_of_interest [!cat_vars_of_interest %in% var_specify],
           ifelse(remove_var == TRUE, cat_vars_of_interest <- cat_vars_of_interest [!cat_vars_of_interest %in% var_specify],
                  cat_vars_of_interest <- cat_vars_of_interest [cat_vars_of_interest %in% var_specify]))
    #Executing the tests
    num_test <- numeric_stat_tester(data, num_test_indicator, num_vars_of_interest, treatment_col, 
                                    treatment_indicator1, treatment_indicator2, digits, modify_wilcox_duplicates)
    cat_test <- categorical_stat_tester(data, cat_test_indicator, cat_vars_of_interest, 
                                        treatment_col, treatment_indicator1, treatment_indicator2, digits)
    #Removing duplicate from categoricals
    if (remove_duplicate_logi){
      cat_test <- remove_duplicate_cat(data, cat_test)
    }
    #Calculating q values
    all_pvals <- c(num_test[["Nominal_pval"]], cat_test[["Nominal_pval"]])
    qvals_obj <- qvalue(p = as.numeric(all_pvals))#, pi0 = 1)
    #Calculating q-value object
    qvals <- qvals_obj$qvalues
    #Splitting the numeric and categorical values
    num_qval <- qvals[1:length(num_test[["Nominal_pval"]])]
    cat_qval <- qvals[(length(num_test[["Nominal_pval"]]) + 1):length(qvals)]
    all_qvals <- list(num_qval, cat_qval)
    #Checking number of comparisons
    nComp <- ifelse(is.na(nComp), nrow(num_test) + nrow(cat_test), nComp)
    #Number of comparisons is the sum of the rows of both tests by default
    if (nComp < nrow(num_test) + nrow(cat_test)){
      warning(paste(nComp, "is less than the required minimum number of comparisons which is the sum the number of numeric and categorical tests performed. Changing nComp to the minimum of", nrow(num_test) + nrow(cat_test) , sep = " "))
      nComp <- nrow(num_test) + nrow(cat_test)
    }
    all_test <- list(num_test, cat_test)
    all_test <- lapply(1:length(all_test), function(i, all_test, all_qvals){
      #Calculating all adjusted p-values
      df <- as.data.table(all_test[i])
      df[["Bonferroni_Adjusted_Pval"]] <- 
        format(p.adjust(df[['Nominal_pval']], method = "bonferroni", n = nComp), digits = digits, scientific = T)
      df[["FDR_Adjusted_Pval"]] <- 
        format(p.adjust(df[['Nominal_pval']], method = "fdr", n = nComp), digits = digits, scientific = T)
      df[["qval"]] <- format(unlist(all_qvals[i]), digits = digits, scientific = T)
      #Adding whether or not each value is significant at either p = 0.05 or p = 0.01
      df[["Bonferroni_Significant"]] <- ifelse(as.numeric(df[["Bonferroni_Adjusted_Pval"]]) < bonferroni_threshold, 1, 0)
      df[["qval_Significant"]] <- ifelse(as.numeric(df[["qval"]]) < qval_threshold, 1, 0)
      df[["FDR_Significant"]] <- ifelse(as.numeric(df[["FDR_Adjusted_Pval"]]) < fdr_threshold, 1, 0)
      return(df)
    }, all_test = all_test, all_qvals = all_qvals)
    df_names <- c(num_test_indicator, cat_test_indicator)
    #Writing the output
    all_test_output <- lapply(1:length(all_test), function(i, all_test, treatment_indicator1, 
                                                           treatment_indicator2, df_names, file_name, titles){
      output <- as.data.table(all_test[i])
      test_indicator <- tolower(strsplit(df_names[i],"")[[1]][1])
      if(!missing(file_name)){ #Checking if a file name was provided
        exportoutput <- as.data.table(output)
        exportoutput <- output_name_format(exportoutput, test_indicator, 
                                           treatment_indicator1, treatment_indicator2)
        #Removing variables from the printed output
        if (is.na(remove_print)){
          exportTab <- exportoutput
        }
        else{
          exportTab <- exportoutput[, (remove_print) := NULL]
        }
        #Titles
        if (missing(titles)){
          test_name <- output$Method[1]
          odds_desc <- ifelse (test_indicator == "t" | test_indicator == "w",  "", 
                               paste("Odds Ratio > 1 is", treatment_indicator2))
          output_title <- paste(test_name, treatment_indicator1, "V.S.", treatment_indicator2, odds_desc)
        }
        else{
          output_title <- titles[i]
        }
        #Printing the table so it saves to the working directory
        #browser()
        print(tab_df(exportTab, title = output_title, 
                     alternate.rows = T, digits = digits, file = file_name[i], 
                     footnote = paste("Bonferroni Significant indicates a p < ", bonferroni_threshold, 
                                      ", FDR Significant indicate a p < ", fdr_threshold, 
                                      ", qval Significant indicates q < ", qval_threshold), show.footnote = T))
      }
      #Formatting column classes
      output <- output_class_format(output, test_indicator)
      #Formatting names
      output <- output_name_format(output, test_indicator, treatment_indicator1, treatment_indicator2)
      return(output)
    }, all_test = all_test, treatment_indicator1 = treatment_indicator1, 
    treatment_indicator2 = treatment_indicator2, 
    df_names = df_names, file_name = file_name, titles = titles)
    return(all_test_output)
  }



pretty_table = function(ttest_fisher, file_name = NULL, title = '', min_p = 0.001, round_digits = 2, 
                       treatment_indicator1 = NULL, treatment_indicator2 = NULL){
  
  if(is.null(treatment_indicator1) | is.null(treatment_indicator2)){
    stop("Error, in paper_table, treatment_indicator1 and treatment_indicator 2 must not be 'NULL'")
  }
  
  ttest = ttest_fisher[[1]]
  fisher = ttest_fisher[[2]]
  
  t1_mean = paste('Mean', treatment_indicator1, sep = '_')
  t2_mean = paste('Mean', treatment_indicator2, sep = '_')
  ind1 = paste('Indicator', treatment_indicator1, sep = '_')
  ind2 = paste('Indicator', treatment_indicator2, sep = '_')
  other1 = paste('Other', treatment_indicator1, sep = '_')
  other2 = paste('Other', treatment_indicator2, sep = '_')
  
  dt_t = as.data.table(data.frame('Variable' = ttest$Var, 'Open' = round(ttest[[t1_mean]],round_digits), 
                                  'MIS' = round(ttest[[t2_mean]],round_digits), 
                                  'Difference' = round(ttest$Difference_In_Means,round_digits),
                                  'Effect Size or Odds Ratio' = ttest$Hedges_g, 
                                  'Unadjusted p-value' = as.character(ttest$Nominal_pval),
                                  'Adjusted p-value' = as.character(ttest$Bonferroni_Adjusted_Pval),
                                  'Variable Type' = 'Numeric'))
  
  open_pct = round(100*fisher[[ind1]]/(fisher[[ind1]]+fisher[[other1]]),round_digits)
  mis_pct = round(100*fisher[[ind2]]/(fisher[[ind2]]+fisher[[other2]]),round_digits)
  dt_fisher = as.data.table(
    data.frame('Variable' = paste0(fisher$Column_of_Interest, '-', fisher$Var_Indicator),
               'Open' = paste0(fisher[[ind1]], ' (', open_pct,'%)'),
               'MIS' = paste0(fisher[[ind2]], ' (', mis_pct, '%)'),
               'Difference' = mis_pct-open_pct,
               'Effect Size or Odds Ratio' = fisher$Odds_Ratio,
               'Unadjusted p-value' = as.character(fisher$Nominal_pval),
               'Adjusted p-value' = as.character(fisher$Bonferroni_Adjusted_Pval),
               'Variable Type' = 'Categoric'))
  
  dt_all = rbind(dt_t, dt_fisher)
  dt_all$Adjusted.p.value[as.numeric(dt_all$Adjusted.p.value) < min_p] = paste0('< ', as.character(min_p))
  dt_all$Unadjusted.p.value[as.numeric(dt_all$Unadjusted.p.value) < min_p] = paste0('< ', as.character(min_p))
  if(!is.null(file_name)){
    #browser()
    print(tab_df(dt_all, title = title, file = file_name, alternate.rows = T))
  }
  return(dt_all)
}




#' Effect Compare Volcano Grapher
#'
#' Creates volcano plots of the p-value against effect size. Uses Hedges G for numeric and Odds Ratio for categorical variables. −log10(pval_col) and log2(Odds Ratio) is used. 
#'
#' @param data A data table with the pvalue and the effect size.
#' @param num_effect_col A string of the column name from data that contains the effect size (most likely will be "Hedges_g"). Not required if cat_effect_col is used.
#' @param num_var_col A string of the column name from data that contains the variable names. "Var" by default. Not used if cat_effect_col is used. "Var" by default. Used in point labeling.
#' @param cat_effect_col A string of the column name from data that contains the effect size (most likely will be "Odds_Ratio". Not required if cat_effect_col is used.
#' @param cat_var_col1 A string of the column name from data that contains the variable names. "Column_of_Interest" by default. Not used if num_effect_col is used.
#' @param cat_var_col2 A second string of the column name from data that contains the individual observations found in cat_var_col1. "Var_Indicator" by default. Not used if num_effect_col is used.
#' @param pval_col A string of the column name of pvalues present in both data1 and data2. If not provided, it is assumed that effect size is being compared and num_effect_col or cat_effect_col will be provided.
#' @param pval_threshold A double from 0.0-1.0 specifying the significance cutoff for the pvalues or qvalues provided in pval_col.
#' @param treatment_indicator A string indicating the treatment from treatment_col.
#' @param point_label An optional string specifying whether to include point labels. Options are "significant" or "all". If significant 
#' @param point_color An optional list of 2 strings specifying the colors for values making the pvalue cutoff. c("red", "black") by default.
#' @param max_overlap An optional integer specifying the maximum number of overlaps for ggtextrepel. Number of rows in data by default
#' @param title An otional string indicating the plot title. If not provided, an automated title in the format "y v.s. x is provided"
#' @param xlab An optional string indicating the x-axis label.
#' @param ylab An optional string indicating the y-axis label.
#' @param legend_title An optional parameter indicating the legend title. If not provided, "pval_col, "< ", pval_threshold" is used.
#' @param print_width An optional integer or double specifying the width of the plot when saving. Used in conjunction with parameter "file_name". 10 by default.
#' @param print_height An optional integer or double specifying the height of the plot when saving. Used in conjunction with parameter "file_name". 10 by default.
#' @param file_name An optional string indicating a file name to save the plot to. File extension must be included. If missing, the graph is not saved.
#'
#' @return A ggplot volcano plot comparing the −log10(pvalue) against either Hedges_g or log2(Odds_Ratio) for effect size.
#'
#' @examples
#' effect_compare_grapher(data = ttest, title = "T−test: −log10(bonferroni AdjPval) v.s. HedgesG", point_label = T, pval_col = "Nominal_Pval" pval_threshold = 0.05, treatment_indicator = "MIS")
#'
#' effect_compare_grapher(data = fisher, file_name = "Categorical_Effect_Compare_Volc.pdf", title = "Fisher Test: −log10(bonferroni AdjPval) v.s. log2(Odds Ratio", treatment_indicator = "MIS", point_color = ("blue", "red"), legend_title = "Significant", print_width = 8, print_height = 7, file_name = "effect_compare.pdf")
#'
#' @import data.table
#' @import ggplot2
#' @import ggrepel
#'
#' @author Ethan Assouline
#' @export

effect_compare_volcano_grapher <-
  function(data, num_effect_col, num_var_col = "Var",
           cat_effect_col, cat_var_col1 = "Column_of_Interest", cat_var_col2 = "Var_Indicator", 
           pval_col, pval_threshold = 0.05, treatment_indicator, method_col = "Method", 
           point_label = NA, point_color = c("red", "black"), max_overlap, 
           title, xlab, ylab, legend_title, 
           print_width = 10, print_height = 10, file_name){
    require(data.table)
    require(ggplot2)
    require(ggrepel)
    #Checking for hedges G
    if (!missing(num_effect_col) && num_effect_col %in% colnames(data)){
      #Setting graph thresholds
      x_thresh_num <- max(abs(min(data[[num_effect_col]])), abs(max(data[[num_effect_col]])))
      effect <- data[[num_effect_col]] #Setting effect as hedges G
      p_vals <- -log10(data[[pval_col]]) #Adjusting the p-value
      #Setting up axis labels
      xlabel = ifelse(missing(xlab), paste(data[[method_col]][1], ' - Hedges G', ifelse(missing(treatment_indicator), "", 
                                                                                        paste(", > 0 is", treatment_indicator)), sep = ''), xlab)
      ylabel = ifelse(missing(ylab), paste("-log10(", data[[method_col]], ' - p-value)', sep = ''), ylab)
      title <- ifelse(missing(title), paste(paste("-log10(", data[[method_col]], ' - p-value)', sep = ''), 
                                            "v.s.", paste(data[[method_col]], ' - Hedges G', sep = '')), title)
      #Setting up labels for the points
      if (!is.na(point_label)){
        if (point_label == "significant"){
          data$lab <-  ifelse(data[[pval_col]] < pval_threshold, data[[num_var_col]], "")
        }
        else if (point_label == "all"){
          data$lab <- data[[num_var_col]]
        }
        else{
          stop("Please choose a valid point_label argument. Options are 'some', 'all', or NA.")
        }
      }
    }
    else if (!missing(cat_effect_col) && cat_effect_col %in% colnames(data)){ #Checking for odds ratio
      odds_remove <- sum(data[[cat_effect_col]] == 0 | is.infinite(data[[cat_effect_col]]))
      if(odds_remove > 0){ #If there are odds ratio equal to 0 or infinity, return warning
        warning(paste(odds_remove, 
                      "odds ratio values are equal to 0 or infinity, removing those from the graph"), sep = '')
      }
      pval_remove <- sum(data[[pval_col]] == 0)
      if(pval_remove > 0){
        warning(paste(pval_remove, 
                      "p-values are equal to 0, removing those from the graph"), sep = '')
      }
      #Removing odds ratio of infinity or 0, or p-value of 0
      data = data[data[[cat_effect_col]] != 0 & !is.infinite(data[[cat_effect_col]]) & data[[pval_col]] != 0]
      #Setting graph thresholds
      x_thresh_num = max(abs(min(log2(data[[cat_effect_col]]))), abs(max(log2(data[[cat_effect_col]]))))
      #data = data[Var_Indicator != 'FALSE']
      effect <- log2(data[[cat_effect_col]]) #Taking the log2 of the odds ratio
      p_vals <- -log10(data[[pval_col]]) #Taking -log10 of the adjusted p-values
      #Setting axis labeling
      xlabel <- ifelse(missing(xlab), paste("log2(", data[[method_col]][1], ' - Odds Ratio)', 
                                            ifelse(missing(treatment_indicator), "", 
                                                   paste(", > 0 is", treatment_indicator)), sep = ''), xlab)
      ylabel <- ifelse(missing(ylab), paste("-log10(", data[[method_col]][1], ' - p-value)', sep = ''), ylab)
      title <- ifelse(missing(title), paste(paste("-log10(", data[[method_col]][1], ' - p-value)', sep = ''), 
                                            "v.s.", paste("log2(", data[[method_col]][1], ' - Odds Ratio)',sep = '')), title)
      #Setting up labels for the points
      if (!is.na(point_label)){
        if (point_label == "all"){
          data$lab <- paste(data[[cat_var_col1]], data[[cat_var_col2]], sep = ' ')
        }
        else if (point_label == "significant"){
          data$lab = ifelse(data[[pval_col]] < pval_threshold, 
                            paste(data[[cat_var_col1]], data[[cat_var_col2]], sep = ' '), "")
        }
        else{
          stop("Please choose a valid point_label argument. Options are 'all', 'significant', or NA.")
        }
      }
    }
    else{
      #Error handling
      stop("Please provide a dataset with a valid numeric or categorical effect size column")
    }
    #Creating the ggplot object
    effect_compare_graph <- ggplot(data = data, aes(x = effect, y = p_vals, colour = (data[[pval_col]] < pval_threshold))) +
      geom_point() + 
      scale_color_manual(values = setNames(point_color, c(T,F))) + 
      scale_x_continuous(limits = c(-x_thresh_num, x_thresh_num)) + 
      theme_bw() +
      labs(title = title, x = xlabel, y = ylabel, color = ifelse(missing(legend_title), 
                                                                 paste(pval_col, "< ", pval_threshold), 
                                                                 legend_title))
    #Putting in labels if specified
    if (!is.na(point_label)){
      max_overlap <- ifelse(missing(max_overlap), nrow(data), max_overlap)
      effect_compare_graph = effect_compare_graph + 
        geom_text_repel(aes(label = data$lab), max.overlaps = max_overlap)
    }
    #Saving the file
    if (!missing(file_name)){
      ggsave(file_name, width = print_width, height = print_height)
    }
    return(effect_compare_graph)
  }
