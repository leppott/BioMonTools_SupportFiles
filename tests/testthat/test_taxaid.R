# Check taxaid for each file
# Ensure each OTU is in the match column
# 2022-12-13
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# duplicate names ----
test_that("trans, dup1", {
  
})## Test ~ dup, trans

test_that("attr, dups1", {
  
})## Test ~ dup, trans

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trans, taxaid ----
test_that("trans, taxaid1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1
  
 # for (i in seq_len(length(fn_all))) {
    fn_i <- df_pickfiles[i , "filename"]
    df_i <- read.csv(file.path("data", "taxa_official", fn_i))
    i_taxaid <- df_pickfiles[i, "taxaid"]
    i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
    

    n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
    n_match_QC <- length(unique(df_i[, i_taxaid_match]))
    
    # show mismatches
    print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
    i1 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                           df_i[, i_taxaid]]
    i1
    
    # test, match
    testthat::expect_equivalent(n_match_calc, n_match_QC)
    
    # test, dup
    n_taxaid <- sum(length(unique(df_i[, i_taxaid])))
    n_rows <- nrow(df_i)
    df_freq <- as.data.frame(table(df_i[, i_taxaid]))
    df_freq[df_freq$Freq > 1, "Var1"]
    
    testthat::expect_equivalent(n_rows, n_taxaid)

 # }## FOR ~ i
  
})## Test ~ taxaid
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("trans, taxaid2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]

  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
  i2 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  i2
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("trans, taxaid3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
  i3 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  i3
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("trans, taxaid4", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]

  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
  i4 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  i4
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All missing taxa
# sort(unique(c(i1, i2, i3, i4)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# attr, taxaid ----
test_that("attr, taxaid1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  j1 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  j1
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # test, dup
  n_taxaid <- sum(length(unique(df_j[, j_taxaid])))
  n_rows <- nrow(df_j)
  df_freq <- as.data.frame(table(df_j[, j_taxaid]))
  df_freq[df_freq$Freq > 1, "Var1"]
  
  testthat::expect_equivalent(n_rows, n_taxaid)
  
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attr, taxaid2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  j2 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  j2
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attr, taxaid3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  j3 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  j3
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attr, taxaid4", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  j4 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  j4
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# test_that("attr, taxaid_orig", {
#   # data files
#   df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
#   fn_all <- df_pickfiles$filename
#   
#   i <- 1
#   
#   # for (i in seq_len(length(fn_all))) {
#   fn_i <- df_pickfiles[i , "filename"]
#   df_i <- read.csv(file.path("data", "taxa_official", fn_i))
#   i_taxaid <- df_pickfiles[i, "taxaid"]
#   i_taxaid_match <- i_taxaid #df_pickfiles[i, "calc_taxaid"]
#   
#   # attribute file
#   fn_j <- df_pickfiles[i , "attributes_filename"]
#   df_j <- read.csv(file.path("data", "taxa_official", fn_j))
#   j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
#   
#   n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
#   n_match_QC <- length(unique(df_i[, i_taxaid_match]))
#   
#   # show mismatches
#   print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
#   j5 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
#                                          df_j[, j_taxaid]]
#   j5
#   
#   # test
#   testthat::expect_equivalent(n_match_calc, n_match_QC)
#   
#   # }## FOR ~ i
#   
# })## Test ~ taxaid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All missing taxa
# sort(unique(c(j1, j2, j3, j4)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

