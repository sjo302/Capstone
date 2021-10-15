library(R6)



Reporter = R6Class("Reporter",
  public = list(
    
    initialize = function(verbose = TRUE){
      self$verbose = verbose
    },
    
    report = function(message, hard = FALSE) {
      if(hard | self$verbose){
        cat(paste0(message, '\n'))
        return(message)
      }
    },
    
    verbose = NULL
  )
)


type_check = function(var, type){
  if(class(type) != 'character'){
    stop('type must be of class character')
  }
  if(!any(class(var) %in% type)){
    stop(paste0(substitute(var), ' must be of type ', paste(type, collapse = ', or ')))
  }
  return(var)
}

Variable = R6Class(
  "Variable",
  public = list(
    initialize = function(raw_data,
                          var_name,
                          auto = F,
                          keep = NULL,
                          class = NULL,
                          mapping = NULL,
                          note = NULL,
                          ask_note = F, 
                          meta = NA) {
      require('plyr')
      require('data.table')
      ## Make sure raw_data is a data.table
      raw_data = type_check(raw_data, c('data.table'))
      ## Make sure var_name is a character
      var_name = type_check(var_name, c('character'))
      
      cat('###################################\n')
      cat(paste0('      ',var_name, '\n'))
      cat('###################################\n')
      
      ## Make sure var_name only contains one colname
      if (length(var_name) != 1) {
        stop("Error, variable name (var_name) must be of length 1")
      }
      
      ## Make sure var_name is actually in raw_data
      if (!var_name %in% colnames(raw_data)) {
        stop("Error, variable name (var_name) must be a column withing raw_data")
      }
      
      ## Extract the variable info from the raw_data
      entry = raw_data[, var_name, with = F]
      
      
      cat(paste0('Number of missing entries: ', as.character(sum(is.na(entry[[1]]))), '(',as.character(round(sum(is.na(entry[[1]]))/length(entry[[1]])*100,3)), '%)\n'))
      cat(paste0('\nFirst 10 entries:\n', paste0(entry[[1]][1:10],collapse = ','), '\n\n'))
      
      ## Check if we can table, if not, then summary
      if(length(unique(entry[[1]])) <= 30){
        cat(paste0('Unique entries:', paste0(unique(entry[[1]]), collapse = ', '), '\n\n'))
      } else{
        cat('Summary: \n')
        print(summary(entry[[1]]))
        cat('\n\n')
      }
      
      ## These two variables are user independant
      private$praw_data = raw_data
      private$pvar_name = var_name
      
      #browser()
      ## auto is the default settings, keep everything, class stays the same, no change to variables (mapping)
      if (auto) {
        private$pvar_class = class(entry[[1]])
        private$pvar_keep = TRUE
        private$pvar_mapping = NULL
      }
      else{
        if(is.null(keep) & is.null(class)){ ## Run duo UI assigner
          self$assign_keep_class()
        } else{ # Run Solo UI Assigner
          self$keep = keep
          self$class = class
          self$meta = meta
        }
        self$mapping = mapping
      }
      
      if(ask_note){## User wants to provide note
        self$ask_note()
      }else{ ##No note will be asked for
        self$note = type_check(note, c('character', 'NULL'))
      }
      
    },
    
    assign_keep_class = function(){
      cat('Select a class and then press enter:\n[1] character\n[2] logical\n[3] numeric\n[4] Date\n[d] discard\n[enter] unknown\n[z] quit\n')
      input = readline()
      if (input == 'z'){ ## user wants to quit variable creation, ask again to make sure
        cat('\n\nYou have selected to quit variable creation, note that all inputs from this session will **NOT** be saved, are you sure you want to quit?\n\n')
        cat('[1] character\n[2] logical\n[3] numeric\n[4] Date\n[d] discard\n[enter] unknown\n[z] quit\n')
        input = readline()
        if(input == 'z'){ ## Run the quiting code
          cat('\n\n\nStopping Variable Creation\n\n\n')
          ## below 3 lines pulled from: https://stat.ethz.ch/pipermail/r-help/2008-November/179475.html
          opt <- options(show.error.messages=FALSE)
          on.exit(options(opt))
          stop()
        }
      }
      if (input %in% c('1', '2', '3', '4', '')) {
        ## User entered a valid number, we will keep
        if (input == '1') {
          private$pvar_class = 'character'
        } else if (input == '2') {
          private$pvar_class = 'logical'
        } else if (input == '3') {
          private$pvar_class = 'numeric'
        } else if (input == '4') {
          private$pvar_class = 'Date'
          ## ask user to provide date format
          cat('Please enter a valid Date format:\n')
          private$pmeta = readline()
        } else if (input == ''){
          private$pvar_class = 'unknown'
        }
        private$pvar_keep = TRUE
      } else if (input == 'd'){ ## user wants to discard variable, set class to unknown
        private$pvar_class = 'unknown'
        private$pvar_keep = FALSE
        
      } 
      else{ ## User entered something invalid throw warning and default to 'unknown' and keep
        warning('Unknown entry, skipping this variable for now, but keeping.')
        private$pvar_class = 'unknown'
        private$pvar_keep = TRUE
      }
    },
    
    ask_note = function(){
      cat('Please enter note:\n')
      note = readline()
      self$note = note
    },
    
    map_logical = function(vals, var_name){
      map_attempt = self$map_character(vals, var_name)
      print(map_attempt)
      fail_var = 0
      while(!self$validate_logical(mapvalues(vals, from = map_attempt$From, to = map_attempt$To))){
        if(fail_var > 10){
          warning('Too many failed attempts to create logical variable, skipping for now')
          return(map_attempt)
        }
        cat(paste0('Invalid logical schema for variable: ', var_name, ' please make sure that logical ',
            'variables only have 1 and 0s\n'))
        map_attempt = self$map_character(vals, var_name)
        fail_var = fail_var + 1
      }
      return(map_attempt)
    },
    
    
    variable_table = function(){
      dt_empty = data.table("Variable" = character(), "Class" = character(), "Keep" = logical())
      ret = rbind(dt_empty, data.table("Variable" = private$pvar_name, "Class" = private$pvar_class, "Keep" = private$pvar_keep, 'Notes' = private$pnote, 'Meta'= private$pmeta), fill = T)
      return(ret)
    },
    
    map_character = function(vals, var_name){
      cat(paste0('Mapping: ', var_name, '\n'))
      in1 = 'x'
      dt_ret = data.table('Variable' = character(), 'From' = character(), 'To' = character())
      while(in1 != ''){ ##User wants to map
        uni_vals = unique(vals)
        uni_vals = uni_vals[!is.na(uni_vals)]
        index_vals = c(1:length(uni_vals))
        val_amounts = table(vals)
        val_props = round(100*val_amounts/length(vals),2)
        over_message = length(uni_vals) > 50
        #browser()
        if(over_message){
          uni_vals = uni_vals[1:50]
          index_vals = index_vals[1:50]
          val_amounts = val_amounts[1:50]
          val_props = val_props[1:50]
        }
        dists = lapply(c(1:length(uni_vals)), function(x) stringdist(uni_vals[x],uni_vals,method="dl"))
        min_dists_index = sapply(dists, function(x) which(x == min(x[x != 0])))
        min_dists_item = sapply(c(1:length(dists)), function(x) paste0(' - ', paste0(uni_vals[min_dists_index[[x]]], collapse = ', ')))
        min_dists_val = sapply(dists, function(x) min(x[x!=0]))
        min_dists_item[min_dists_val >10] = ''
        min_dists_text = paste0('MD: ',min_dists_val)#, ' (', min_dists_item, ')\n')
        uni_vals_map = mapvalues(uni_vals, from = dt_ret$From, to = dt_ret$To)
        text_map = paste(' -> ', uni_vals_map)
        text_map[uni_vals_map == uni_vals] = ''
        vars_text = paste0('[', index_vals, '] ', paste0(uni_vals, text_map), ' (n = ', val_amounts, ', ', val_props, '% ', min_dists_text,')',min_dists_item,'\n', collapse = '')
        #vars_text = paste0('[', index_vals, '] ', paste0(uni_vals, text_map), ' (n = ', val_amounts, ', ', val_props, '% ',') - ','\n', collapse = '')
        cat(paste0('Select Value to Map:\n', vars_text,'[ ] continue\n'))
        if(over_message){
          cat('Only showing the first 50 values.\n')
        }
        sel = readline()
        
        if(sel == ''){## User wants to quit
          in1 = ''
        }else{ ## user selected a value
          if(sel %in% index_vals){ ## user entered a valid entry, allow user to enter mapping
            sel_val = uni_vals[as.numeric(sel)]
            if(sel_val %in% dt_ret$From){ ## duplicate value, prompt for overwrite/undo
              cat('You have selected a variable that already exists in the mapping, would you like to overwrite?\n[enter] No\n [*][enter] Yes\n')
              ov = readline()
              if(ov != ''){
                dt_ret = dt_ret[From != sel_val]
              }
              else{
                next
              }
            }
            cat(paste0('You have selected: ', sel_val, '\nPlease enter mapping:'))
            mp = readline()

            dt_new = data.table('Variable' = var_name, 'From' = sel_val, 'To' = mp)
            dt_ret = rbind(dt_ret, dt_new)
          }
          else{ ##user entry is invalid
            cat('Invalid entry\n')
            next
          }
        }
      }
      
      return(dt_ret)

    },
    
    character_summary = function(vals){
      tab_vals = table(vals)
      if(length(tab_vals) > 20){
        print('Only showing first 20 values:')
      }
      print(tab_vals[1:min(20,length(tab_vals))])
    },
    
    validate_keep = function(){
      if(class(private$pvar_keep) != 'logical'){
        warning(paste0('Variable "', as.character(private$pvar_name), '" keep status must be of class "logical"'))
        return(FALSE)
      }
      return(self$validate_base())

    },
    
    validate_class = function(){
      if(!private$pvar_class %in% c('character', 'numeric', 'logical', 'Date', 'unknown')){
        warning(paste0('Variable "', as.character(private$pvar_name), '" class must be one of: "character", "numeric", "logical", "Date", "unknown"'))
        return(FALSE)
      }
      return(self$validate_base())

    },
    validate_mapping = function(){
      if(!class(private$pvar_mapping)[1] %in% c('data.table', 'NULL')){
        warning(paste0('Variable "', as.character(private$pvar_name), '" mapping must be of class "data.table" or "NULL"'))
        return(FALSE)
      }
      rv = self$raw_values
      mp = private$pvar_mapping
      ## Checking that From values and raw_values overlap
      if(!all(mp$From %in% rv)){
        warning(paste0('Variable "', as.character(private$pvar_name), ' "From" mapping contains values not present in the raw_data"'))
        return(FALSE)
      }
      return(self$validate_base())
    },
    validate_note = function(){
      if(!class(private$pnote) %in% c('character', 'NULL')){
        warning(paste0('Variable "', as.character(private$pvar_name), ' "note" field must be of class "character" or "NULL"'))
        return(FALSE)
      }
      return(TRUE)
    },
    
    validate_meta = function(){
      if(!class(private$pmeta) %in% c('character', 'NULL')){
        warning(paste0('Variable "', as.character(private$pvar_name), ' "meta" field must be of class "character" or "NULL"'))
        return(FALSE)
      }
      return(TRUE)
    },
    
    
    validate_logical = function(logi = NULL){
      if(is.null(logi)){
        logi = self$values 
      }
      logi = logi[logi != 'NA']
      logi = logi[!is.na(logi)]
      tab_logi = table(logi)
      if(length(unique(logi)) != 2){
        warning(paste0('Logical variable:', private$pvar_name, ' must have at most two final values and NA'))
        return(FALSE)
      }
      if(!all(c(0,1) %in% logi)){
        warning(paste0('Logical variable: ', private$pvar_name, ' must have the values 0/1'))
        return(FALSE)
      }
      return(TRUE)
    },
    
    validate_base = function(){
      if(class(private$pvar_name) != 'character'){
        warning('Inconsistent state: Variable "', as.character(private$pvar_name), '" must be of class character')
        return(FALSE)
      }
      if(class(private$pvar_raw_data)[1] %in% c('data.table')){
        warning(paste0('Variable "', as.character(private$pvar_name), '" raw_data must be of class "logical"'))
        return(FALSE)
      }
      return(TRUE)
      
    },
    
    validate_date = function(){
      #browser()
      date = self$values
      if(all(is.na(date))){
        warning(paste0('Date variable:', private$pvar_name, ' appears to have an incorrect formating string\n,',
                       ' please refer to : https://www.r-bloggers.com/2013/08/date-formats-in-r/\n,',
                       ' and change the date formating string accordingly.'))
        return(FALSE)
      }
      return(TRUE)
    },
    
    validate = function(){
      valid = self$validate_class() & self$validate_mapping() & self$validate_keep() & self$validate_note()
      if(private$pvar_class == 'logical'){
        valid = valid & self$validate_logical()
      }
      if(private$pvar_class == 'Date'){
        valid = valid & self$validate_date()
      }
      return(valid)

    },
    
    


    
    print = function() {
      vals = self$values
      
      cat('###################################\n')
      cat(paste0('      ',private$pvar_name, '\n'))
      cat('###################################\n')
      
      if(!is.null(self$note)){## There is a note, add it in here
        cat('\n')
        cat(self$note)
        cat('\n\n')
      }
      
      cat(paste0(
        'class: ',
        private$pvar_class,
        '\n',
        'Summary:\n'
      ))
      
      
      if (private$pvar_class %in% c('logical', 'character')) {
        ## we'll use table for summary
        self$character_summary(vals)
      } else{
        ## numeric or unknown so we'll just use summary
        cat(summary(vals))
      }
      
      cat(
        paste0(
          'A Variable object derived from raw_data with\n',
          as.character(ncol(private$praw_data)),
          ' Columns (Features)\n',
          as.character(nrow(private$praw_data)),
          ' Entries (Patients)\n',
          'That is to be: '
        )
      )
      
      if (private$pvar_keep) {
        cat('kept\n')
      } else{
        cat('discarded\n')
      }
      if (!is.null(private$pvar_mapping)) {
        cat('With the following mapping schema:\n')
        print(private$pvar_mapping)
      }
      
      cat(paste0('Number of missing entries: ', as.character(sum(is.na(vals))), '(',as.character(round(100*sum(is.na(vals))/length(vals),3)), '%)\n'))

      
      cat('###################################')
      
    }
    
  ),
  
  private = list(
    praw_data = NULL,
    pvar_name = NULL,
    pvar_class = NULL,
    pvar_keep = NULL,
    pvar_mapping = NULL,
    pnote = NULL,
    pmeta = NA,
    
    to_class = function(val){
      if(private$pvar_class == 'character'){
        return(as.character(val))
      } else if (private$pvar_class == 'logical'){
        return(as.logical(as.numeric(val)))
      } else if (private$pvar_class == 'numeric'){
        return(as.numeric(val))
      } else if (private$pvar_class == 'Date'){
        return(as.Date(as.character(val), private$pmeta))
      } else{
        return(val)
      }
    }
    
  ),
  
  active = list(
    
    values = function(){
      val = mapvalues(self$raw_values,
                      from = private$pvar_mapping$From,
                      to = private$pvar_mapping$To)
      return(suppressWarnings(private$to_class(val)))
      
    },
    
    raw_values = function(){
      val = private$praw_data[, private$pvar_name, with = F][[1]]
      return(val)
    },
    
    name = function(name){
      if(!missing(name)){
          warning('Unable to update variable name, please remake variable')
      }
      else{
        return(private$pvar_name)
      }
    },
    
    keep = function(keep){
      if(!missing(keep)){
        ## we need user input, either from the function call or from the command line
        ## need to ask the user as to whether to keep the variable
        if (is.null(keep)) {
          cat('Press [enter] to keep variable\ntype anything and then press [enter] to discard variable\n')
          input = readline()
          if (input == '') {
            ## User decided to keep the variable
            private$pvar_keep = TRUE
          } else{
            private$pvar_keep = FALSE
          }
        } else{
          ## keep is defined in function call
          private$pvar_keep = keep
        }
        self$validate_keep()
      }
      else{
        self$validate_keep()
        return(private$pvar_keep)
      }
    },
    
    class = function(class){
      if(!missing(class)){
        ## need to ask the user as to what class the variable is 
        if (is.null(class)) {
          cat('Select a class and then press enter:\n[enter] unknown\n[1] character\n[2] logical\n[3] numeric\n')
          input = readline()
          if (input %in% c('1', '2', '3')) {
            ## User entered a valid number
            if (input == '1') {
              private$pvar_class = 'character'
            } else if (input == '2') {
              private$pvar_class = 'logical'
            } else if (input == '3') {
              private$pvar_class = 'numeric'
            } else if (input == '4') {
              private$pvar_class = 'Date'
            }
          } else{
            ## User entered something invalid throw warning and default to 'unknown'
            warning('Unknown entry, skipping this variable for now.')
            private$pvar_class = 'unknown'
          }
        } else{
          ## User entered a class, make sure its valid if not throw warning
          if(!class %in% c('logical', 'numeric', 'character', 'Date', 'unknown')){
            warning(paste0("User supplied non-standard class for variable: ", var_name))
          }
          private$pvar_class = class
        }
        self$validate_class()
      }
      else{
        self$validate_class()
        return(private$pvar_class)
      }
    },
    
    mapping = function(mapping){
      if(!missing(mapping)){
        if(private$pvar_class %in% c('logical', 'character')){
          if(is.null(mapping)){
            if(private$pvar_class == 'logical'){
              private$pvar_mapping = self$map_logical(self$raw_values, private$pvar_name)
              self$validate_logical()
            }
            else{ ## class is character
              private$pvar_mapping = self$map_character(self$raw_values, private$pvar_name)
            }
          } else{
            private$pvar_mapping = type_check(mapping, 'data.table')
          }
          ## if map is of length 0, set to empty datatable 
          if(nrow(private$pvar_mapping) == 0){
            private$pvar_mapping = data.table('Variable' = character(), 'From' = character(), 'To' = character())
          }
          self$validate_mapping()
        }
        else{
          private$pvar_mapping = NULL
        }
      }
      else{
        self$validate_mapping()
        if(private$pvar_class == 'character'){
          self$validate_mapping()
        } else if (private$pvar_class == 'logical'){
          self$validate_logical()
        }
        return(private$pvar_mapping)
      }
    },
    note = function(note){
      if(!missing(note)){
        if(class(note) %in% c('character', 'NULL')){
          private$pnote = note
        }
        else{
          warning(paste0('Variable "', as.character(private$pvar_name), ' "note" field must be of class "character" or NULL".'))
        }
      }
      else{
        self$validate_note()
        return(private$pnote)
      }
    },
    
    meta = function(meta){
      if(!missing(meta)){
        if(class(meta) %in% c('character', 'NULL')){
          private$pmeta = meta
        }
        else{
          warning(paste0('Variable "', as.character(private$pvar_name), ' "meta" field must be of class "character" or NULL".'))
        }
      }
      else{
        self$validate_meta()
        return(private$pmeta)
      }
    },
    raw_data = function(){
      return(private$praw_data)
    }
    
    
  )
)


Schema = R6Class(
  "Schema",
  public = list(
    initialize = function(raw_data = NULL, vars = NULL, file = NULL, numeric_NA_values = NULL, notes = F){
      require(stringdist)
      require(openxlsx)
      require(data.table)
      require(plyr)
      private$pnotes = notes
      ## User is loading in data from file
      if(!is.null(file)){
        require(readxl)
        self$vars = type_check(file, c("character"))
        private$praw_data = private$raw_data_from_file(type_check(file, c("character")))
        self$validate_raw_data()
        
      }else{ ##user is supplying data from R or a fread compatible object
        ## User is supplying raw_data and vars in R
        raw_data = type_check(raw_data, c('character', 'data.table'))
        if(class(raw_data)[1] == 'character'){ ## if user provided a character, assume its a path
          raw_data = fread(raw_data)
        }
        private$praw_data = raw_data
        self$validate_raw_data()
        
        self$vars = type_check(vars, c("NULL", "list"))
      }

      self$numeric_NA_values = numeric_NA_values
      self$validate()

      
    },
    
    save = function(file = NULL){
      #browser()
      if(is.null(file)){
        file = paste('Schema_', gsub(':', '-', gsub(' ', '_', Sys.time())), '.xlsx', sep = '')
      }
      cat('Saving Schema:\n')
      self$print()
      cat('Writing Variable Information\n')
      ## Save the variable information first
      wb = createWorkbook()
      addWorksheet(wb, 'Variable_Information')
      addWorksheet(wb, 'Variable_Mapping')
      addWorksheet(wb, 'Raw_Data')
      writeDataTable(wb, sheet = 1, x = self$variable_table, na.string = 'NA')
      writeDataTable(wb, sheet = 2, x = self$mapping_table, na.string = 'NA')
      writeDataTable(wb, sheet = 3, x = private$praw_data, na.string = 'NA')
      saveWorkbook(wb, file, overwrite = TRUE)
      cat(paste('\n Saving schema is complete and has been save to file: ', file, sep = ''))
    },
    
    
    
    print = function(){
      cat(paste0('A schema consisting of: ', length(private$pvars), ' (keep: ', sum(self$keeps), ') variables\n'))
      cat(paste0('With the following classes: '))
      print(table(self$classes))
    },
    
    validate = function(){
      valid_vars = sapply(self$vars, function(x) x$validate())
      ret = self$names[!valid_vars]
      coln_valid = self$validate_raw_data()
      if(!coln_valid){
        return(coln_valid)
      }
      if(length(ret) == 0){
      }
      else{
        warning('Some Schema Variables Invalid:')
        cat(ret)
      }
      return(ret)
    },
    
    validate_raw_data = function(){
      coln_raw = colnames(private$praw_data)
      coln = toupper(coln_raw)
      dups = duplicated(coln)
      if(any(dups)){
        dup_id = which(coln %in% coln[dups])
        nms = coln_raw[dup_id]
        groups = lapply(coln[dups], function(x) nms[toupper(nms) == x])
        mes = paste(sapply(groups, function(x) paste(x, collapse = ' - ')), collapse = '\n\t')
        mes = paste('Duplicate Columns:\n\t', mes,'\n\n', sep = '')
        stop(paste('\n\nError: Monet does not allow duplicate column names.\n Note: colnames are NOT case sensitive.\n\n', mes, sep = ''))
        return(FALSE)
      }
      return(TRUE)
      
    },
    
    update_schema = function(vars){
      print('updating schema')
      ## re-runs var creation on specific vars, then validates
    }
    
  ),
  private = list(
    praw_data = NULL,
    pvars = list(),
    pnumeric_NA_values = NULL,
    pnotes = F,
    
    autoschema = function(){
      
      data = private$praw_data
      var_names = colnames(data)
      var_iter = 1
      fail_iter = 0
      while(var_iter <= length(var_names)){
        cat(paste0('\nCurrently on Variable: ', as.character(var_iter), '/',as.character(length(var_names)), '\n\n'))
        var = Variable$new(raw_data = data, var_name = var_names[var_iter], ask_note = private$pnotes)
        if(!var$validate()){ ## there is a problem with the variable, repeat the iteration
          warning(paste0('Invalid variable state for variable ', var_names[var_iter], ' please redo variable creation below:'))
          fail_iter = fail_iter + 1
          print(fail_iter)
          if(fail_iter >= 10){
            stop("Error, failed schema more than 10 times.")
          }
        } else{ ## var is valid and good
          private$pvars = c(private$pvars, var)
          var_iter = var_iter + 1
          fail_iter = 0
        }
        

      }
      
      
      
    },
    schema_from_file = function(file){
      print(paste('loading schema from file: ', file, sep = ''))
      
      ## reading variable information
      cat('Reading Variable Information\n')
      variable_table = as.data.table(read_excel(path = file,sheet="Variable_Information")) 
      
      ## reading variable mapping
      cat('Reading Variable Mappings\n')
      mapping_table = as.data.table(read_excel(path = file,sheet="Variable_Mapping"))
      
      ## reading the raw data
      cat('Reading Raw Data\n')
      raw_data = private$raw_data_from_file(file = file, classes = variable_table$Class)
      
      cat('Generating Variables\n')
      vars = generate_variables(raw_data = raw_data, variable_table = variable_table, mapping_table = mapping_table)
      return(vars)
    },
    
    raw_data_from_file = function(file, classes = NULL){
      #browser()
      if(is.null(classes)){
        classes = self$classes
      }
      excel_class = mapvalues(classes, 
                              from = c('character', 'numeric', 'logical', 'unknown', 'Date'),
                              to = c('text', 'numeric', 'text', 'text', 'date'))
      raw_data = suppressWarnings(as.data.table(read_excel(path = file, sheet = "Raw_Data", 
                                                           col_types = excel_class)))
      return(raw_data)
    }
  ),
  active = list(
    vars = function(schema){
      if(!missing(schema)){
        if(is.null(schema)){ ## run autoschema
          private$autoschema()
        } else if (class(schema) == 'character'){ ## generate schema from file
          private$pvars = private$schema_from_file(schema)
        } else if (class(schema) == 'list'){ ##user supplied vars
          private$pvars = schema
        }
      }else{
        ret = private$pvars
        names(ret) = sapply(ret, function(x) x$name)
        return(ret)
      }
    },
    raw_data = function(raw_data){
      if(!missing(raw_data)){
        stop('Functionality not implemented 1')
        self$validate_raw_data()
        return(NULL)
        ## update raw data for all vars, validate, and run schema on all non-valid vars
      } else{
        self$validate_raw_data()
        return(private$praw_data)
      }
    },
    classes = function(){
      ret = sapply(private$pvars, function(x) x$class)
      names(ret) = sapply(private$pvars, function(x) x$name)
      return(ret)
    },
    keeps = function(){
      ret = sapply(private$pvars, function(x) x$keep)
      names(ret) = sapply(private$pvars, function(x) x$name)
      return(ret)
    },
    names = function(){
      ret = sapply(private$pvars, function(x) x$name)
      return(ret)
    },
    variable_table = function(){
      return(rbindlist(lapply(private$pvars, function(x) x$variable_table()), fill = TRUE))
    },
    mapping_table = function(){
      return(rbindlist(lapply(private$pvars, function(x) x$mapping)))
    },
    data = function(){
      ## Extracting variables to keep
      keep = self$keeps
      vars_to_keep = names(keep[keep == T])
      keep_data = self$raw_data[,vars_to_keep, with = F]
      ## Mapping variables
      for(x in 1:length(self$vars)){
        var = self$vars[[x]]
        if(!is.null(var$mapping)){ ## mapping exists, lets do some mapping
          keep_data[[var$name]] = mapvalues(keep_data[[var$name]], from = var$mapping$From, to = var$mapping$To)
        }
      }
      ## converting columns to correct classes
      #browser()
      ret = suppressWarnings(col_class_changer(keep_data, self$variable_table[Variable %in% vars_to_keep]))
      ##removing NA values from numeric columns
      if(!is.null(private$pnumeric_NA_values)){
        for (col in colnames(ret)){
          temp = ret[[col]]
          if(class(temp) == 'numeric'){
            temp[temp %in% self$numeric_NA_values] = NA
            ret[[col]] = temp
          }
        }
      }
      return(ret)
    },
    numeric_NA_values = function(numeric_NA_values){
      if(!missing(numeric_NA_values)){
        private$pnumeric_NA_values = type_check(numeric_NA_values, c('numeric', 'character', 'complex', 'Date', 'integer', 'logical', 'NULL'))
        invisible(private$pnumeric_NA_values)
      }
      else{
        return(private$pnumeric_NA_values)
      }
    }
    
  ),

)


Monet = R6Class("Monet",
                public = list(
                  initialize = function(raw_data = NULL, experiment_name = NULL, verbose = TRUE, 
                                        case_id = NULL, case_time = NULL, case_features = NULL,
                                        case_outcomes = NULL, schema = NULL, treatment = NULL,
                                        numeric_NA_values = NULL, notes = F){
                    require(data.table)
                    require(DataExplorer)
                    require(ggpubr)
                    require(mltools)
                    require(stringdist)
                    source('Monet_Stat_Tester.R')
                    
                    self$raw_data = raw_data
                    self$experiment_name = experiment_name
                    self$verbose = verbose
                    self$case_id = case_id
                    self$case_time = case_time
                    self$treatment = treatment
                    private$pnotes = notes
                    self$schema = type_check(schema, c('character', 'NULL', 'list'))
                    self$numeric_NA_values = numeric_NA_values

                  },
                  print = function(){
                    base = paste0('An object of class Monet\n', nrow(self$raw_data), ' Patients with ',
                                  ncol(self$raw_data), ' variables.')
                    if(!is.null(private$pcase_id)){
                      base = paste0(base, '\nCaseID Variable: ', private$pcase_id)
                    }
                    if(!is.null(private$pcase_time)){
                      base = paste0(base, '\nTime Variable: ', private$pcase_time)
                    }
                    if(!is.null(private$ptreatment)){
                      base = paste0(base, '\nTreatments: ', paste(unique(self$raw_data[,private$ptreatment, with = F][[1]]),'\n',  collapse = ', '))
                    }
                    cat(base)
                    if(!is.null(private$pschema)){
                      cat('\nSchema:\n')
                      private$pschema$print()
                    }
                    
                  },
                  report = function(output_file = NULL, y = NULL, report_title = NULL){
                    if(is.null(output_file)){ ##Use Default
                      output_file = paste0(self$experiment_name, '.html')
                    } else{ ## User specified, check HTML
                      if(!grepl('.html', output_file, fixed = T)){ ## check if file is .html, if not throw error
                        stop('Error: report output_file must have a .html extension')
                      }
                    }
                    
                    if(is.null(report_title)){ ## if using default title, add on experiment name
                      report_title = paste0("Data Profiling Report for: ", self$experiment_name)
                      
                    }
                    create_report(self$data, output_file = output_file, y = y, report_title = report_title)
                  },
                  
                  save_schema = function(file = NULL){
                    private$pschema$validate()
                    private$pschema$save(file)
                  },
                  validate_case_id = function(){
                    if(class(private$pcase_id) != 'character'){
                      warning('Inconsistent state, case_id must be a character')
                    }
                    if(length(private$pcase_id) > 1){
                      warning('Inconsistent state, case_id name must be of length 1')
                    }
                    if(!private$pcase_id %in% colnames(private$praw_data)){
                      warning(paste0('Inconsistant state caseid: , ', private$pcase_id, ' is not in colnames raw_data'))
                    }
                    if(any(duplicated(private$praw_data[,private$pcase_id]))){
                      warning('Inconsistant state, case_id is not unique for all patients')
                    } 
                  },
                  
                  validate_treatment = function(){
                    if(class(private$ptreatment) != 'character'){
                      warning('Inconsistent state, treatment must be a character')
                    }
                    if(length(private$ptreatment) > 1){
                      warning('Inconsistent state, treatment name must be of length 1')
                    }
                    if(!private$pcase_id %in% colnames(private$praw_data)){
                      warning(paste0('Inconsistant state treatment: , ', private$pctreatment, ' is not in colnames raw_data'))
                    }
                  },
                  
                  validate_case_time = function(){
                    if(class(private$pcase_time) != 'character'){
                      warning('Inconsistent state, case_time must be a character')
                    }
                    if(length(private$pcase_time) > 1){
                      warning('Inconsistent state, case_time name must be of length 1')
                    }
                    if(!private$pcase_time %in% colnames(private$praw_data)){
                      warning(paste0('Inconsistant state case_time: , ', private$pcase_time, ' is not in colnames raw_data'))
                    }
                    if(class(private$praw_data[,private$pcase_time, with = F][[1]]) != 'Date'){
                      warning('Inconsistant state, case_time must correspond to a column of type "Date"')
                    } 
                  },
                  
                  validate_schema = function(){
                    return(private$pschema$validate())
                  },
                  
                  compare_groups = function(num_test_indicator = 'wilcoxtest', cat_test_indicator = 'fishertest', 
                                            treatment_col = private$ptreatment, treatment_indicator1 = NULL, 
                                            treatment_indicator2 = NULL, digits = 3, var_specify = NA,
                                            remove_var = FALSE, bonferroni_threshold = 0.05, qval_threshold = 0.1, fdr_threshold = 0.1,
                                            file_name = NULL, remove_print = NA, title = NULL, pretty = T, min_p = 0.001,
                                            round_digits = 2){
                    
                    if(length(file_name) > 1){
                      warning("file_name should be a character of length 1, using first value")
                      file_name = type_check(file_name[1], 'character')
                    }
                    
                    data = self$data
                    if(is.null(title)){
                      title = self$experiment_name
                    }
                    
                    if(is.null(treatment_indicator1) | is.null(treatment_indicator2)){  
                      treatments = unique(data[[treatment_col]])
                      if(length(treatments) < 2){
                        stop("Selected treatment_col must have at least 2 unique values")
                      }
                      treatment_indicator1 = treatments[1]
                      treatment_indicator2 = treatments[2]
                      cat('\n\nfull set of treatment_indicators(1/2) not provided, defaulting to:\n')
                      cat(paste0('treatment_indicator1: ', treatment_indicator1, '\ntreatment_indicator2: ', treatment_indicator2, '\n\n'))
                      
                    }

                    ## Check if file name exists, if so then create names for the three tables
                    if(!is.null(file_name)){
                      if(!grepl('.html', file_name, fixed = T)){ ## check if file_name contains .html, if not, throw warning
                        warning('file_name should have the .html extension')
                      }
                      file_name_type = paste(c('numeric', 'categoric'), file_name, sep = '_')
                      file_name_pretty = paste('pretty', file_name, sep = '_')
                    } else{
                      file_name_type = NULL
                      file_name_pretty = NULL
                    }
                    
                    
                    
                    ret = stat_tester(data = data[,-c(private$pcase_id,private$pcase_time), with = F], 
                                      num_test_indicator = num_test_indicator, cat_test_indicator = cat_test_indicator,
                                      treatment_indicator1 = treatment_indicator1, treatment_indicator2 = treatment_indicator2,
                                      digits = digits, var_specify = var_specify, remove_var = remove_var, title = title,
                                      bonferroni_threshold = bonferroni_threshold, qval_threshold = qval_threshold, 
                                      fdr_threshold = fdr_threshold, file_name = file_name_type, remove_print = remove_print,
                                      treatment_col = treatment_col)
                    names(ret) = c('Numeric', 'Character')
                    
                    if(pretty){
                      pretty = pretty_table(ttest_fisher = ret, file_name = file_name_pretty, title = title, min_p = min_p, 
                                            round_digits = round_digits, treatment_indicator1 = treatment_indicator1, 
                                            treatment_indicator2 = treatment_indicator2)
                      ret = list(ret[[1]],ret[[2]], pretty)
                      names(ret) = c('Numeric', 'Character', 'Combined')
                    }
                    
                    return(ret)
                    
                    
                  },
                  volcano_plots = function(compare_results = NULL, file = NULL, width = 20, height = 7.5, pval_col = 'Bonferroni_Adjusted_Pval',
                                           pval_threshold = 0.05,point_label = 'significant', point_color = c("red", "black"), title = NULL,
                                           xlab, ylab, legend_title){
                    if(is.null(title)){
                      title = self$experiment_name
                    }
                    
                    if(is.null(compare_results)){
                      stop('Please set the param "compare_results" to a list output from the "compare_groups" function.')
                    }
                    
                    vol_num = effect_compare_volcano_grapher(compare_results[[1]], num_effect_col = 'Hedges_g', pval_col = pval_col,
                                                             pval_threshold = pval_threshold, point_label = point_label, point_color = point_color,
                                                             title = title, xlab = xlab, ylab = ylab, legend_title = legend_title)
                    
                    vol_cat = effect_compare_volcano_grapher(compare_results[[2]], cat_effect_col = 'Odds_Ratio', pval_col = pval_col,
                                                             pval_threshold = pval_threshold,point_label = point_label, point_color = point_color,
                                                             title = title, xlab = xlab, ylab = ylab, legend_title = legend_title)
                    
                    ret = ggarrange(vol_num, vol_cat, labels = c("A", "B"),ncol = 2)
                    
                    if(!is.null(file)){
                      ggsave(file, width = width, height = height)
                    }
                    return(ret)
                  }
                ),
                private = list(
                  preporter = NULL,
                  pseed = 42,
                  pexperiment_name = NULL,
                  praw_data = NULL,
                  pmatched_data_cases = NULL,
                  pmatched_data = NULL,
                  pfeature_data = NULL,
                  poutcome_data = NULL,
                  pverbose = NULL,
                  pcase_features = NULL,
                  pcase_outcomes = NULL,
                  pcase_time = NULL,
                  pcase_id = NULL,
                  ptreatment = NULL,
                  pschema = NULL,
                  pnotes = NULL,
                  
                  update_known_feature = function(input){
                    input_name = substitute(input)
                    if(class(input) == 'character' & length(input) == 1 ){ ## They've supplied a column name
                      if(input %in% colnames(self$raw_data)){
                        private$preporter$report(paste0("You've supplied a single column name for ", input_name ))
                        return(list(known_feature_value = input, new_column = NULL))
                      }
                      else{
                        stop(paste0(input_name, ' is supplied as a 1 length character implying it is a column name, unable to find the column name in the raw_data'))
                      }
                    }
                    else{ ## They've supplied a vector that is either in the data or not
                      ## check if vector is in the data
                      in_data = sapply(self$raw_data, function(x) {
                        tryCatch({all(x == input)
                        }, error = function(cond){
                            return(FALSE)
                        })})
                      if(any(in_data, na.rm = T)){ ## If we have a match
                        private$preporter$report(paste0("You've supplied a vector present in the raw_data for " , input_name))
                        if(sum(in_data, na.rm = T) > 1){ ## value matches mutliple columns, throw warning and use the first
                          warning("The ",input_name, " vector you've supplied matches multiple columns ", paste(colnames(self$raw_data)[which(in_data)], collapse = ', '), " using only the first match")
                        }
                        return(list(known_feature_value = colnames(self$raw_data[,which(in_data)[1], with = F]), new_column = NULL))
                      } else{ ## We don't have a match, add a new column
                        private$preporter$report(paste0("You've supplied a vector not in the raw_data for ", input_name))
                        ## Make sure vector is same length as the data, if not throw warning
                        if(length(input) != nrow(self$raw_data)){
                          warning('Your ', input_name, ' vector is not of the same length as your raw_data.')
                        }
                        return(list(known_feature_value = as.character(input_name), new_column = input))
                      }
                    }
                  }
                ),
                active = list(
                  ##seed = NULL
                  
                  ## experiment_name must either be NULL (default) or a user supplied character.
                  ## If default will name it "Experiment: Sys.time()"
                  experiment_name = function(value){
                    if(!missing(value)){
                      private$pexperiment_name = type_check(value, c('character', 'NULL'))
                      if(is.null(private$pexperiment_name)){
                        private$pexperiment_name = paste0('Experiment: ', Sys.time())
                      }
                    }
                    else{
                      return(private$pexperiment_name)
                    }
                  },
                  
                  ## raw_data may only be data.frame or data.table, if it is data.frame, convert to data.table
                  raw_data = function(value){
                    if(!missing(value)){
                      private$praw_data = type_check(value, c('data.table', 'data.frame'))
                      if(class(private$praw_data)[1] == 'data.frame'){
                        private$praw_data = as.data.table(private$praw_data)
                      }
                    } else{
                      return(private$praw_data)
                    }
                  },
                  #matched_data_cases = NULL,
                  #matched_data = NULL,
                  #feature_data = NULL,
                  #outcome_data = NULL,
                  
                  ## Verbose must be of type logical, if this checks out, setup the reporter
                  verbose = function(value){
                    if(!missing(value)){
                      private$pverbose = type_check(value, 'logical')
                      if(is.null(private$preporter)){
                        private$preporter = Reporter$new(private$pverbose)
                      }
                      else{
                        private$preporter
                      }
                    }
                    else{
                      return(private$pverbose)
                    }
                    
                  },
                  
                  #case_features = NULL,
                  #case_outcomes = NULL,
                  
                  ##case time should either be supplied as a column name ('char') or a vector ('Date or char convertable to date')
                  ##Or if null we'll look for a column that is a date or can be coerced to a date
                  case_time = function(case_time){
                    if(!missing(case_time)){
                      case_time = type_check(case_time, c('character', 'NULL', 'Date'))
                      if(!is.null(case_time)){
                        if(class(case_time) == 'character' & length(case_time) != 1){## we're converting a char to date
                          case_time = as.Date(case_time)
                        }
                        valid_input = private$update_known_feature(case_time) ## running validity check for case_time
                        private$pcase_time = valid_input$known_feature_value
                        if(!is.null(valid_input$new_column)){ ## we're adding in a new column, enforce date
                          new_column = valid_input$new_column
                          if(class(new_column) != 'Date'){
                            warning(paste0('Provided column is not of class "Date", attempting to coerce provided values to class "Date"'))
                            new_column = as.Date(new_column)
                          }
                          self$raw_data$case_time = new_column
                        }
                        else{ ## we are using an existing column, enforce date
                          if(class(self$raw_data[,private$pcase_time, with = F][[1]]) != 'Date'){
                            warning(paste0('Provided column is not of class "Date", attempting to coerce provided values to class "Date"'))
                            set(self$raw_data, j = private$pcase_time, value = as.Date(self$raw_data[,private$pcase_time, with = F][[1]]))
                          }
                        }
                      } else{ ## case_time is null lets search for a date
                        private$preporter$report('Running automatic case_time search: looking for a column that is of type "Date"')
                        dates = sapply(self$raw_data, function(x) class(x) == 'Date')
                        if(any(dates, na.rm = T)){## we've found a date
                          private$preporter$report("Completed searching for case_time columns")
                          private$preporter$report(paste("Found the following potential columns: ", paste(colnames(self$raw_data)[which(dates)], collapse = ', ')))
                          ## check to see if there is only one
                          if(sum(dates, na.rm = T) > 1){
                            warning(paste0('Automatically found multiple case_time columns that are of class "Date": ',paste(colnames(self$raw_data)[which(dates)], collapse = ', '), ' will use the first.'))
                          }
                          private$pcase_time = colnames(self$raw_data)[which(dates)[1]]
                        } else{ ## No case_times can be found
                          warning('Unable to automatically find a case_time column that is of class "Date", creating single time date column called "case_time"')
                          private$pcase_time = 'case_time'
                          self$raw_data$case_time = as.Date(Sys.Date())
                        }
                        
                      }
                      self$validate_case_time()
                      invisible(private$praw_data[,private$pcase_time, with = F])
                    }
                    else{
                      self$validate_case_time()
                      return(private$praw_data[,private$pcase_time, with = F][[1]])
                    }
                  },
                  
                  ## CaseID should either be supplied as a column name ('char') or a vector ('char') 
                  ## or if null we'll look for a column that is unique for all patients
                  case_id = function(case_id){
                    if(!missing(case_id)){
                      case_id = type_check(case_id, c('character', 'NULL'))
                      if(!is.null(case_id)){ ## The user either supplied a column name or new vector
                        valid_input = private$update_known_feature(case_id) ## running validity checks on input
                        private$pcase_id = valid_input$known_feature_value ## Assigning the column name of case_id to pcase_id
                        if(!is.null(valid_input$new_column)){## The user supplied a valid vector so add it in as a column
                          self$raw_data$case_id = valid_input$new_column ## adding in the new column
                          private$pcase_id = 'case_id'
                        }
                      } else { ## User did not supply variable, Lets search for a vector that is unique for all patients
                        private$preporter$report("Running automatic case_id search: looking for a column with no duplicate values for case_id")
                        uni_all = sapply(self$raw_data, function(x) length(unique(x)) == nrow(self$raw_data))
                        if(any(uni_all, na.rm = T)){ ## we've found one
                          private$preporter$report("Completed searching for case_id columns")
                          private$preporter$report(paste("Found the following potential columns: ", paste(colnames(self$raw_data)[which(uni_all)], collapse = ', ')))
                          ## check to see if there is only one
                          if(sum(uni_all, na.rm = T) > 1){
                            warning(paste0('Automatically found multiple case_id columns that are unique for all patients: ',paste(colnames(self$raw_data)[which(uni_all)], collapse = ', '), ' will use the first.'))
                          }
                          private$pcase_id = colnames(self$raw_data)[which(uni_all)[1]]
                        } else{ ## No unique case_id can be found
                          stop('Unable to automatically find a case_id column with unique values for each case')
                        }
                
                      } 
                      ## Wrap up
                      if(any(duplicated(self$raw_data[,private$pcase_id, with = F]))){## Make sure there are no duplicate values in case_id
                        private$pcase_id = NULL ## undo the var assignment before throwing an error
                        stop('case_id cannot contain duplicate values')
                      }
                      self$validate_case_id()
                      invisible(private$praw_data[,private$pcase_id, with = F])
                      
                    }
                    else{
                      self$validate_case_id()
                      return(private$praw_data[,private$pcase_id, with = F][[1]])
                    }
                    
                  },
                  
                  ##treatment should either be supplied as a column name ('char') or a vector ('character or  convertible to character')
                  ##Or if null we'll create a column with all controls
                  treatment = function(treatment){
                    if(!missing(treatment)){
                      if(!is.null(treatment)){
                        if(class(treatment) != 'character' & length(treatment) != 1){## User supplied vector, try to convert to character
                          private$preporter$report(paste0('User supplied a non-character vector for treatment, trying to coerce vector of type: ', 
                                                   class(treatment), ' to character.'))
                          treatment = as.character(treatment)
                        }
                        valid_input = private$update_known_feature(treatment) ## running validity check for case_time
                        private$ptreatment = valid_input$known_feature_value
                        if(!is.null(valid_input$new_column)){
                          self$raw_data$treatment = valid_input$new_column
                        }
                      } else{ ## treatment is null, we'll set everything to controls for now
                        private$preporter$report('No treatment variables specified, defaulting all patients to control')
                        private$ptreatment = 'treatment'
                        self$raw_data$treatment = 'control'
                      }
                        
                      self$validate_treatment()
                      invisible(private$praw_data[,private$ptreatment, with = F][[1]])
                    }
                    else{
                      self$validate_treatment()
                      return(private$praw_data[,private$ptreatment, with = F][[1]])
                    }
                  },
                  
                  ##Schema is either a list of Variables, or a path to a .xlsx containing schema information
                  ##Or if null we'll run the autogenerator
                  schema = function(schema){
                    if(!missing(schema)){ ##User provided NULL, list of variables or character.xlsx
                      if(!is.null(schema)){ ## user provided list of variables or character.xlsx
                        if(class(schema) == 'character'){## User supplied .xlsx character path
                          private$pschema = Schema$new(file = schema)
                        } else if(class(schema) == 'list'){ ## User is supplying a variable list, make sure its a variable list
                          if(all(sapply(schema, function(x) class(x)[1]) == 'Variable')){ ## Variable list is good to go
                            private$pschema = Schema$new(raw_data = private$praw_data, vars = schema)
                          }
                        }
                      } else{ ## schema is null, run autoschema, here is the only place to add notes manually
                        private$pschema = Schema$new(raw_data = private$praw_data, notes = private$pnotes)
                      }
                      
                      self$validate_schema()
                      invisible(private$pschema)
                    }
                    else{
                      self$validate_schema()
                      return(private$pschema)
                    }
                  },
                  
                  ## data is a non-settable variable that applies the schema to raw_data
                  data = function(schema){
                    return(self$schema$data)
                  },
                  
                  numeric_NA_values = function(numeric_NA_values){
                    if(!missing(numeric_NA_values)){
                      private$pschema$numeric_NA_values = type_check(numeric_NA_values, c('numeric', 'character', 'complex', 'Date', 'integer', 'logical', 'NULL'))
                      invisible(private$pschema$numeric_NA_values)
                    }
                    else{
                      return(private$pschema$numeric_NA_values)
                    }
                  },
                  
                  one_hot_data = function(){
                    
                    cls = self$schema$classes
                    cls = cls[self$schema$keeps]
                    data = self$data
                    
                    for (i in 1:ncol(data)){
                      if(cls[i] == 'character' & names(cls[i]) != 'CASEID'){
                        data[[i]] = as.factor(data[[i]])
                      } else if(cls[i] == 'logical' | cls[i] == 'Date'){
                        data[[i]] = as.numeric(data[[i]])
                      }
                    }
                    
                    return(one_hot(data))
                    
                  }


                )
)


#' Column Class Changer
#'
#' Changes the class of specified columns from a data frame.
#'
#' @param data A data frame or data table.
#' @param corrections A csv or xlsx containing the column names of the data and the class type that each column should be converted to. Columns must be respectively named: "data_col_name" and "new_class". "new_class" should only be filled in with "numerical", "character", or "logical".
#'
#' @return A data table of the original data with columns changed to the specified class type.
#'
#' @examples
#' col_class_changer(data = data,corrections = corrections)
#' col_class_changer(data = data,corrections = "corrections.csv")
#' col_class_changer(data = data,corrections = "corrections.xlsx")
#'
#' @import data.table
#' @import tools
#' @import readxl
#'
#' @author Ethan Assouline
#' @export
col_class_changer <- 
  function(data, corrections){
    require(data.table)
    require(tools)
    require(readxl)
    if (is.character(corrections)){ #Checking if the corrections input is passed as a path
      corrections <- file_reader(corrections)
    }
    vars_to_clean = corrections[["Variable"]]
    if(any(!vars_to_clean %in% colnames(data))){
      missing_vars = paste(vars_to_clean[!vars_to_clean %in% colnames(data)], sep = '', collapse = ', ')
      warning(paste('Variable map contains entries not present in the main dataset, this will be the case if you have composite variables in the main dataset. Variables: ', missing_vars, '\n',  sep = ''))
      vars_to_clean = vars_to_clean[vars_to_clean %in% colnames(data)]
    }
    for (row in 1:length(vars_to_clean)){ #For each row in the mapping spreadsheet
      indicator <- corrections[[row, "Class"]] #Checking the specified column class
      var_to_change = as.character(corrections[row,"Variable"])
      meta = as.character(corrections[row,"Meta"])
      if (indicator == "numeric"){
        data[[var_to_change]] <- as.numeric(data[[var_to_change]]) #Changing column to numeric
      }
      else if (indicator == "character"){
        data[[var_to_change]] <- as.character(data[[var_to_change]]) #Changing column to character
      }
      else if (indicator == "logical"){
        data[[var_to_change]] <- as.logical(as.integer(data[[var_to_change]])) #Changing column to logical
      }
      else if (indicator == "Date"){
        if(!is.na(meta)){
          data[[var_to_change]] <- tryCatch(
            {
              as.Date(as.character(data[[var_to_change]]), meta) #Changing column to date with meta
            },
            error = function(cond){
              warning(cond)
              warning(paste0('Variable: ', var_to_change, ' has malformed date format, please set the value to a valid date format.\n',
                             'For now will just set the date to a character in output.'))
              return(as.character(data[[var_to_change]]))
            })
        } else{
          warning(paste0('Date variables (', var_to_change, ') must contain a formatting string, returning as character')) #Changing column to date without meta
          data[[var_to_change]] <- as.character(data[[var_to_change]])
          }

      }
    }
    return(as.data.table(data)) #Returns the data as a data table
  }

generate_variables = function(raw_data, variable_table, mapping_table){
  vars = lapply(c(1:nrow(variable_table)), function(x){
    meta = variable_table$Meta[x]
    ## Check if there are any notes
    if('Notes' %in% colnames(variable_table)){ ## if so, then run creator with notes
      note = variable_table$Notes[x]
      if(is.na(note)){ ## convert NAs to NULL as .xlsx defaults missing values to NA
        note = NULL
      }
      v1 = Variable$new(raw_data = raw_data, var_name = variable_table$Variable[x], keep = as.logical(variable_table$Keep[x]), 
                        class = variable_table$Class[x], mapping = mapping_table[Variable == variable_table$Variable[x]],
                        note = note, meta = meta)
    } else{ ##default no notes
      v1 = Variable$new(raw_data = raw_data, var_name = variable_table$Variable[x], keep = as.logical(variable_table$Keep[x]), 
                        class = variable_table$Class[x], mapping = mapping_table[Variable == variable_table$Variable[x]],
                        meta = meta)
    }

    return(v1)
  })
  return(vars)
}

#`$.Monet` = function(obj,val){
#  if(!val %in% names(x)) {
#    
#  }
#  NextMethod()
#}