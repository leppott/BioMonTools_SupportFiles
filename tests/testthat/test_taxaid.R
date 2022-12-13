# Check taxaid for each file
# Ensure each OTU is in the match column
# 2022-12-13
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# taxaid ----
test_that("taxaid1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1
  
 # for (i in seq_len(length(fn_all))) {
    fn_i <- df_pickfiles[i , "filename"]
    df_i <- read.csv(file.path("data", "taxa_official", fn_i))
    i_taxaid <- df_pickfiles[i, "taxaid"]
    i_taxaid_match <- df_pickfiles[i, "calc_type_taxaid"]
    

    n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
    n_match_QC <- length(unique(df_i[, i_taxaid_match]))
    
    # show mismatches
    i_taxaid_match
    x1 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid]]
    x1
    
    # test
    testthat::expect_equivalent(n_match_calc, n_match_QC)

 # }## FOR ~ i
  
})## Test ~ taxaid
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("taxaid2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_type_taxaid"]

  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  i_taxaid_match
  x2 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid]]
  x2
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("taxaid3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_type_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  i_taxaid_match
  x3 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid]]
  x3
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("taxaid4", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_type_taxaid"]

  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  i_taxaid_match
  x4 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid]]
  x4
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All missing taxa
# sort(unique(c(x1, x2, x3, x4)))

