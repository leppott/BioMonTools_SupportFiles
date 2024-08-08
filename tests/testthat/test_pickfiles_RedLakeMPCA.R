# Check pickfiles
# files names and taxa id columns
# 2024-06-11
# Erik.Leppo@tetratech.com
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Red Lake, MPCA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# not reading files if "run tests"
# run as code and works
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20240808, update tests for MN sub dir
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FileNames ----
test_that("RedLake, pickfiles, filenames", {
  
  dn_data <- file.path("data", "taxa_official", "MN")
  # data files
  fn_pickfiles <- "_pick_files_MN.csv"
  df_pickfiles <- read.csv(file.path(dn_data, fn_pickfiles))
  
  fn_trans <- df_pickfiles$filename
  fn_trans_meta <- df_pickfiles$metadata_filename
  fn_attr <- df_pickfiles$attributes_filename
  fn_attr_meta <- df_pickfiles$attributes_metadata_filename
  fn_all <- c(fn_trans, fn_trans_meta, fn_attr, fn_attr_meta)
  fn_dir <- list.files(file.path(dn_data))
 
  # QC
  n_fn_all <- length(fn_all)
  n_fn_match <- sum(fn_all %in% fn_dir)  
  
  # show mismatches
  fn_nonmatch <- fn_all[!fn_all %in% fn_dir]
  msg <- paste0("File names in Pick Files not matching files in directory\n"
                , "File = ",  fn_pickfiles
                , "\n\n"
                , paste(fn_nonmatch, collapse = "\n")
                , "\n"
  )
  message(msg)
  
  #test
  testthat::expect_equal(n_fn_match, n_fn_all)
  
})## Test ~ pickfiles - filenames


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TaxaID ----
# one for each row in file

test_that("RedLake, taxaid cols, 1", {
  filenum <- 1
  
  dn_data <- file.path("data", "taxa_official", "MN")
  # data files
  fn_pickfiles <- "_pick_files_MN.csv"
  df_pickfiles <- read.csv(file.path(dn_data, fn_pickfiles))
  
  fn_trans <- df_pickfiles[filenum, "filename"]
  taxaid_trans_1 <- df_pickfiles[filenum, "taxaid"]
  taxaid_trans_2 <- df_pickfiles[filenum, "calc_taxaid"]
  # not checking for col_drop
  fn_attr <- df_pickfiles[filenum, "attributes_filename"]
  taxaid_attr <- df_pickfiles[filenum, "attributes_taxaid"]
  # files
  df_trans <- read.csv(file.path(dn_data, fn_trans))
  df_attr <- read.csv(file.path(dn_data, fn_attr))
  
  # QC Show
  c(taxaid_trans_1, taxaid_trans_2) %in% names(df_trans)
  taxaid_attr %in% names(df_attr)
  
  # test
  testthat::expect_contains(names(df_trans), taxaid_trans_1)
  testthat::expect_contains(names(df_trans), taxaid_trans_2)
  testthat::expect_contains(names(df_attr), taxaid_attr)

})## Test ~ taxaid cols, 1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("RedLake, taxaid cols, 2", {
  filenum <- 2
  
  dn_data <- file.path("data", "taxa_official", "MN")
  # data files
  fn_pickfiles <- "_pick_files_MN.csv"
  df_pickfiles <- read.csv(file.path(dn_data, fn_pickfiles))
  
  fn_trans <- df_pickfiles[filenum, "filename"]
  taxaid_trans_1 <- df_pickfiles[filenum, "taxaid"]
  taxaid_trans_2 <- df_pickfiles[filenum, "calc_taxaid"]
  # not checking for col_drop
  fn_attr <- df_pickfiles[filenum, "attributes_filename"]
  taxaid_attr <- df_pickfiles[filenum, "attributes_taxaid"]
  # files
  df_trans <- read.csv(file.path(dn_data, fn_trans))
  df_attr <- read.csv(file.path(dn_data, fn_attr))
  
  # QC Show
  c(taxaid_trans_1, taxaid_trans_2) %in% names(df_trans)
  taxaid_attr %in% names(df_attr)
  
  # test
  testthat::expect_contains(names(df_trans), taxaid_trans_1)
  testthat::expect_contains(names(df_trans), taxaid_trans_2)
  testthat::expect_contains(names(df_attr), taxaid_attr)
  
})## Test ~ taxaid cols, 2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("RedLake, taxaid cols, 3", {
  filenum <- 3
  
  dn_data <- file.path("data", "taxa_official", "MN")
  # data files
  fn_pickfiles <- "_pick_files_MN.csv"
  df_pickfiles <- read.csv(file.path(dn_data, fn_pickfiles))
  
  fn_trans <- df_pickfiles[filenum, "filename"]
  taxaid_trans_1 <- df_pickfiles[filenum, "taxaid"]
  taxaid_trans_2 <- df_pickfiles[filenum, "calc_taxaid"]
  # not checking for col_drop
  fn_attr <- df_pickfiles[filenum, "attributes_filename"]
  taxaid_attr <- df_pickfiles[filenum, "attributes_taxaid"]
  # files
  df_trans <- read.csv(file.path(dn_data, fn_trans))
  df_attr <- read.csv(file.path(dn_data, fn_attr))
  
  # QC Show
  c(taxaid_trans_1, taxaid_trans_2) %in% names(df_trans)
  taxaid_attr %in% names(df_attr)
  
  # test
  testthat::expect_contains(names(df_trans), taxaid_trans_1)
  testthat::expect_contains(names(df_trans), taxaid_trans_2)
  testthat::expect_contains(names(df_attr), taxaid_attr)
  
})## Test ~ taxaid cols, 3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("RedLake, taxaid cols, 4", {
  filenum <- 4
  
  dn_data <- file.path("data", "taxa_official", "MN")
  # data files
  fn_pickfiles <- "_pick_files_MN.csv"
  df_pickfiles <- read.csv(file.path(dn_data, fn_pickfiles))
  
  fn_trans <- df_pickfiles[filenum, "filename"]
  taxaid_trans_1 <- df_pickfiles[filenum, "taxaid"]
  taxaid_trans_2 <- df_pickfiles[filenum, "calc_taxaid"]
  # not checking for col_drop
  fn_attr <- df_pickfiles[filenum, "attributes_filename"]
  taxaid_attr <- df_pickfiles[filenum, "attributes_taxaid"]
  # files
  df_trans <- read.csv(file.path(dn_data, fn_trans))
  df_attr <- read.csv(file.path(dn_data, fn_attr))
  
  # QC Show
  c(taxaid_trans_1, taxaid_trans_2) %in% names(df_trans)
  taxaid_attr %in% names(df_attr)
  
  # test
  testthat::expect_contains(names(df_trans), taxaid_trans_1)
  testthat::expect_contains(names(df_trans), taxaid_trans_2)
  testthat::expect_contains(names(df_attr), taxaid_attr)
  
})## Test ~ taxaid cols, 4


