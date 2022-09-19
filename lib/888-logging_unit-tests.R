#devtools::install_github("r-lib/testthat")
library(testthat)
library(futile.logger)


# Set the logger threshold to only show warnings OR weeors
flog.threshold(WARN)# INFO # ERROR
flog.appender(appender.file("progress_V2.log"))
flog.info("Key table created", name = "quiet")

################################################################################
# Checking number of unique ISO codes matches
################################################################################

check_convert_iso = function(DF, COL1, COL2, SCRIPT) {
  # Filter for the rows where COL2 is NA (i.e. converted gecodes did not work)
  tmp = DF %>% filter(is.na({
    {
      COL2
    }
  }))
  tmp = tmp %>% distinct({
    {
      COL1
    }
  })
  
  # does what i want
  tmp = gsub(", ", "\n", tmp)
  tmp = paste0(tmp)
  
  # Get the number of the distinct values in original column
  X = DF %>% distinct({
    {
      COL1
    }
  })
  X = dim(X)[1]
  
  # Get the number of the distinct values in the column
  Y = DF %>% distinct({
    {
      COL2
    }
  })
  Y = dim(Y)[1]
  
  # Checks if the dimensions are equal
  if (X == Y) {
    flog.info("All geocodes converted correctly")
  } else{
    flog.warn(
      paste0(
        "---------------------------------------------------------------------------------------------------"
      )
    )
    flog.warn(paste0("WARNING in script : ", SCRIPT))
    flog.warn(paste0("The following geocodes could not be converted : \n", tmp)) # print out the duplicate iso year pairs
  }
  
}

# example of the script (that writes out to the lof giles)
#check_convert_iso(dat_long, iso3c, iso3c_CONVERT, "SCRIPT")


################################################################################
# Checking for duplicate country year pairs
################################################################################

check_duplicates = function(df,
                            c1 = "iso3c",
                            c2 = "year",
                            c3 = NULL,
                            SCRIPT = "") {
  df = as.data.frame(df)
  ind = duplicated(df[, c(c1, c2, c3)])
  tmp_df = df[ind, ]
  
  # Get the desired columns only to paste output
  tmp = tmp_df[, (c(c1, c2, c3))]
  tmp = tmp %>% unite("combined", c1, c2, c3, sep = " ")
  tmp = gsub(", ", "\n", tmp)
  tmp = paste0(tmp)
  
  # Generate teh country year pairs and the values to export to excel
  group_cols = c(c1, c2, c3)
  Z = df %>%
    #group_by({c1},{c2}) %>%
    group_by(!!!syms(group_cols)) %>%
    filter(n() > 1)
  
  # Conditional to save the output if there are duplicates
  if (dim(Z)[1] > 0) {
    Z = Z %>%
      mutate(country = country.code.name(iso3c))
    
    rio::export(Z, paste0(SCRIPT, "-duplicates.csv"))
  }
  
  # Z = Z
  # Z$year = unique(maxyr$year)
  #Z$variablename = unique(df$variablename)
  # EXPERIMENTING
  
  # Checks whether or not the duplicate dataframe is empty or not
  if (dim(tmp_df)[1] == 0) {
    flog.info("There are no duplicate country-year pairs")
  } else{
    flog.warn(
      paste0(
        "---------------------------------------------------------------------------------------------------"
      )
    )
    flog.warn(paste0("WARNING in script : ", SCRIPT))
    flog.warn(paste0("The following geocode-year pairs are duplicated : \n", tmp)) # print out the duplicate iso year pairs
  }
  
  return(tmp)
  
}

#XXX = check_duplicates(df,"Col1", "Col2", "TESTING SCRIPT")
#XXX = check_duplicates(gni_clean,"iso3c", "year","variablename", "TESTING SCRIPT")


################################################################################
# Checking for country coverage
################################################################################
#
# check_coverage = function(df, c1="iso3c",c2="year", SCRIPT=""){
#
#   # convert to dataframe
#   df = as.data.frame(df)
#
#   # Get the distinct geocodes across the entire dataset
#   X = df %>% distinct({{c1}})
#
#   # Get the distinct geocodes for the latest eyar
#   Y = df %>% filter(year == max({{c2}}))
#   Y = Y %>% distinct({{c1}})
#   #Y = as.data.frame(Y)
#
#   # Check which geocodes appear in the overall dataset but ot the latesr year
#   tmp = setdiff(X,Y)
#   tmp = gsub(", ","\n", tmp)
#   tmp = paste0(tmp)
#
#   # Checks whether or not the duplicate dataframe is empty or not
#   if(dim(X)[1]==dim(Y)[1]){
#     flog.info("Country coverage for overall data is same as latest year")
#   } else{
#     flog.warn(paste0("WARNING in script : ", SCRIPT))
#     flog.warn(paste0("The following geocodes are missing for the latest year : \n",tmp)) # print out the duplicate iso year pairs
#   }
#
#   return(tmp)
#
# }


# version 2 since it doesnt work for exchange rate volatility script
check_coverage = function(df,
                          c1 = "iso3c",
                          c2 = "year",
                          SCRIPT = "") {
  # convert to dataframe
  df = as.data.frame(df)
  
  # Get the distinct geocodes across the entire dataset
  X = df
  X[, {
    {
      c2
    }
  }] = as.numeric(as.character((X[, {
    {
      c2
    }
  }])))
  X = X %>% filter(year < max(year))
  X = X %>% dplyr::select({
    {
      c1
    }
  }) %>% distinct()
  
  # Get the distinct geocodes for the latest eyar
  # removes the factors because they mess things up
  Y = df
  Y[, {
    {
      c2
    }
  }] = as.numeric(as.character((Y[, {
    {
      c2
    }
  }])))
  Y = Y %>% filter(year == max(year))
  #Y = Y %>% filter(year == max({{c2}}))
  Y =  Y %>% dplyr::select({
    {
      c1
    }
  }) %>% distinct()
  
  #Y = as.data.frame(Y)
  # get the max year to print out
  maxyr = df
  maxyr[, {
    {
      c2
    }
  }] = as.numeric(as.character((maxyr[, {
    {
      c2
    }
  }])))
  maxyr = maxyr %>% filter(year == max(year)) %>% select(year) %>% distinct()
  
  
  # Check which geocodes appear in the overall dataset but ot the latesr year
  tmp = setdiff(X, Y)
  tmp = gsub(", ", paste0(maxyr, " in \n"), tmp)
  tmp = paste0(tmp)
  
  
  # EXPERIMENTING
  
  # dataframe to export to excel sheet
  Z = tmp = setdiff(X, Y)
  
  if (dim(Z)[1] > 0) {
    Z$year = unique(maxyr$year)
    
    # if(length(unique(df$variablename) == 1)){
    #   Z$variablename = unique(df$variablename)
    # } else{
    #   Z$variablename = "multiple"
    # }
    # mutate("max_year" = maxyr,
    #        "variablename" = unique(vname))
    Z = as.data.frame(Z)
    
    # mutate conditionally if it is the iso code to become the country name
    if (min(nchar(Z$iso3c)) == 3) {
      Z = Z %>% mutate(iso3c = country.code.name(iso3c))
      
    }
    
    rio::export(Z, paste0(SCRIPT, "latest_year", ".csv"))
  }
  # EXPERIMENTING
  
  
  # Checks whether or not the duplicate dataframe is empty or not
  #if(dim(X)[1]==dim(Y)[1]){
  if (dim(Z)[1] == 0) {
    flog.info("Country coverage for overall data is same as latest year")
  } else{
    flog.warn(
      paste0(
        "---------------------------------------------------------------------------------------------------"
      )
    )
    flog.warn(paste0("WARNING in script : ", SCRIPT))
    flog.warn(paste0(
      "The following geocodes are missing for the latest year : \n",
      tmp
    )) # print out the duplicate iso year pairs
  }
  
  return(tmp)
  
}

################################################################################
# Checking for NA values (type conversion issues for some rows)
################################################################################

check_nas = function(df, SCRIPT = "") {
  # Checks the sum opf NA's in each column and makes into a dataframe
  tmp = data.frame("NAs" = colSums(is.na(df)))
  
  # THis is jsut for testing
  #tmp[c(2,3),1]=999
  
  tmp = tmp %>% filter(NAs != 0)
  tmp <- rownames_to_column(tmp, "rownames")
  tmp = tmp %>% mutate("combined" = paste0(rownames, " has ", NAs, " NA values"))
  
  # Get the information you want to print
  # Columns where the NAs are !=0
  tmp = tmp$combined
  tmp = gsub(", ", "\n", tmp)
  tmp = paste0(tmp, collapse = "\n")
  
  # Checks whether or not the duplicate dataframe is empty or not
  # change is_empty to dim[1]
  if (nchar(tmp) == 0) {
    flog.info("There are no NA's in any column")
  } else{
    flog.warn(
      paste0(
        "---------------------------------------------------------------------------------------------------"
      )
    )
    flog.warn(paste0("WARNING in script : ", SCRIPT))
    flog.warn(paste0("Some columns have NA values : \n", tmp)) # print out the duplicate iso year pairs
  }
  
  
  return(tmp)
  
}

#XXX = check_nas(gni_clean, "TESTING SCRIPT")






# df = data.frame("Col1" = c("A","A","A","A","B","B","B","B","C","C","C","C"),
#                 "Col2" = c(1,1,2,3,1,2,3,4,1,2,3,3),
#                 "COl3" = c(1,2,3,4,5,6,7,8,9,10,11,12))
#
# # Works
# ind = duplicated(df[,1:2])
# my_duplicates = df[ind,]
#
# # Works
# ind = duplicated(df[,c("Col1","Col2")])
# my_duplicates = df[ind,]
#





#
#
#
#
#
# check_dupe_iso = function(DF1,DF2, SCRIPT){
#   if(DF1==DF2){
#     flog.info("Testing if DF1==DF2 PASS SHOULD NOT BE FLAGGED")
#   } else{
#     flog.warn(paste0("WARNING in script", SCRIPT))
#     flog.warn("The number of unique iso codes do not match")
#     flog.warn(paste0("")) # print out the duplicate iso year pairs
#   }
# }
#
#
#
#
#
#
#
#
# # This only loggs issues when the expectation is NOT met
# # largely depends on the threshold and whether it is warn or info etc
# log_testing = function(DF1,DF2){
#   if(DF1==DF2){
#     flog.info("Testing if DF1==DF2 PASS SHOULD NOT BE FLAGGED")
#     } else{
#     flog.warn("WARNING in script ABCD")
#     flog.warn("Testing if DF1==DF2 FAIL SHOULD BE FLAGGED")
#     }
# }
#
# # TEsting the logging to see if it flags correctly
# log_testing(B,C) # PASS
# log_testing(A,B) # FAIL
# log_testing(A,C) # FAIL
# log_testing(B,C) # PASS
#
#
# # identical to the normal one
# log_geocodes = function(DF1,DF2){
#
#   DF1=A
#   DF2=B
#   # This only loggs issues when the expectation is NOT met
#   test_that("INFO is not logged",{
#     expect_silent(flog.info("This should NOT appear"))
#     expect_silent(flog.warn("This should appear"))
#     expect_equal(DF1,DF2,flog.warn("Testing if A==B WRONG"))
#     expect_equal(B,C,flog.warn("Testing if A==B RIGHT"))
#
#   })
#
#   }
#
# # Things to test:
# # Duplicates
# # existence of duplicate country/years/variables (NOT VALUES)
# # # ind = duplicated(gni_clean[,1:2])
# # test = gni_clean[ind,]
# # can also use something like : data <- data[which(duplicated(data[,c('T.N','ID')])==TRUE),]
#
# # Check total number of geocodes
# # likely involving a setdiff between original and final geocodes
# # and then prinnting out which ones appear in orignal but not the final (or which rwos have NA in geocode once
# # converted ?)
#
# # Be aware that the length(unique(geocodes) can match, even when there is a set difference
# # This should be flagged as a warning (see example in 14-criminal networks) with the clean$iso3c and the dat_raw$country code
# #
#
#
#
# # Checking geocode coverage in latest year :
# # NUmber of countries in latest year equals number of countries across all years
# # if not, output a set difference (what is in the overall, but NOT the latest years)
#
# # Checking year coverage
# # Does the number of years match wht is expected?
# # Particularly when using rolling eman or SD is used as this decreases the years
#
# # Check data types
# # Check that the text is converted to integer correctly
# # by looking fr the number of NA's in the column
# # SHould be equal to zero, otherwise throw a warning
# # indicating the conversion might have had issues with some rows
#
# # Check existence of NA
# # used for final dataset before appending
#
# # CHeckl data types after final (for every single script)
# # to ensure types are correct and there aren't any issues there.
# #
# # sum(is.na(dat_clean$iso3c))
# # sum(is.na(dat_clean$year))
# # sum(is.na(dat_clean$value))
# # sum(is.na(dat_clean$variablename))
#
#
# # TO DO
# # FUnctionize them as well to allow for variable inputs
# # based on the differnet dataframe and names
#
