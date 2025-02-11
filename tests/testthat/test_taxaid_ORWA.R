# Check taxaid for each file
# Ensure each OTU is in the match column
# 2022-12-13
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20230602, Added tests for BSTI (trans and attr)
# could probably combine trans and attr individual tests into a single test each
# OK if "DNI" is only taxon causing failure of test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20231031, removed "DNI" if present so test passes.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# only 1 test for dups since the same table for all 6
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20231206, added 6th test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20240606, DNI messes up stuff
# wrote generic "tests" in 
# BioMonTools/_tests/QC_TaxaID_betweenFiles_Project.R
# mimics the tests here
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20241121
# add pick files tests from RedLake (MN)
# Add back DNI removal for trans tests
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20250211
# update directory
# Multiple files! Not sure how to handle
# Add back DNI removal for attr tests
# Add test taxaid_7 for both trans and attr
# Add test dups_2 for both trans and attr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FileNames ----
testthat::test_that("ORWA, pickfiles, filenames", {
  dn_data <- file.path("data", "taxa_official", "ORWA")
  # data files
  fn_pickfiles <- "_pick_files.csv"
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

# duplicate names ----
testthat::test_that("ORWA, trans, dups1", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  ii <- 1# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_ii <- df_pickfiles[ii , "filename"]
  df_ii <- read.csv(file.path(dn_data, fn_ii))
  ii_taxaid <- df_pickfiles[ii, "taxaid"]
  
  # QC
  n_taxa_calc <- length(unique(df_ii[, ii_taxaid]))
  n_taxa_qc   <- length(df_ii[, ii_taxaid])
  
  # show mismatches
  print(paste0("Duplicate names '", ii_taxaid, "' from file '", fn_ii, "'"))
  n_taxa_table <- as.data.frame(table(df_ii[, ii_taxaid]))
  n_taxa_table[n_taxa_table$Freq > 1, ]
  
  #test
  testthat::expect_equal(n_taxa_calc, n_taxa_qc)
  
})## Test ~ dup, trans

testthat::test_that("ORWA, trans, dups2", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  ii <- 2# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_ii <- df_pickfiles[ii , "filename"]
  df_ii <- read.csv(file.path(dn_data, fn_ii))
  ii_taxaid <- df_pickfiles[ii, "taxaid"]
  
  # QC
  n_taxa_calc <- length(unique(df_ii[, ii_taxaid]))
  n_taxa_qc   <- length(df_ii[, ii_taxaid])
  
  # show mismatches
  print(paste0("Duplicate names '", ii_taxaid, "' from file '", fn_ii, "'"))
  n_taxa_table <- as.data.frame(table(df_ii[, ii_taxaid]))
  n_taxa_table[n_taxa_table$Freq > 1, ]
  
  #test
  testthat::expect_equal(n_taxa_calc, n_taxa_qc)
  
})## Test ~ dup, trans

testthat::test_that("ORWA, attr, dups1", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  jj <- 1 #+ 6

  # attribute file
  fn_jj <- df_pickfiles[jj , "attributes_filename"]
  df_jj <- read.csv(file.path(dn_data, fn_jj))
  jj_taxaid <- df_pickfiles[jj, "attributes_taxaid"]
  
  # QC
  n_taxa_calc <- length(unique(df_jj[, jj_taxaid]))
  n_taxa_qc   <- length(df_jj[, jj_taxaid])
  
  # show mismatches
  print(paste0("Duplicate names '", jj_taxaid, "' from file '", fn_jj, "'"))
  n_taxa_table <- as.data.frame(table(df_jj[, jj_taxaid]))
  n_taxa_table[n_taxa_table$Freq > 1, ]
  
  # test
  testthat::expect_equivalent(n_taxa_calc, n_taxa_qc)
  
})## Test ~ dup, trans

testthat::test_that("ORWA, attr, dups2", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  jj <- 2 #+ 6
  
  # attribute file
  fn_jj <- df_pickfiles[jj , "attributes_filename"]
  df_jj <- read.csv(file.path(dn_data, fn_jj))
  jj_taxaid <- df_pickfiles[jj, "attributes_taxaid"]
  
  # QC
  n_taxa_calc <- length(unique(df_jj[, jj_taxaid]))
  n_taxa_qc   <- length(df_jj[, jj_taxaid])
  
  # show mismatches
  print(paste0("Duplicate names '", jj_taxaid, "' from file '", fn_jj, "'"))
  n_taxa_table <- as.data.frame(table(df_jj[, jj_taxaid]))
  n_taxa_table[n_taxa_table$Freq > 1, ]
  
  # test
  testthat::expect_equivalent(n_taxa_calc, n_taxa_qc)
  
})## Test ~ dup, trans

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trans, taxaid ----
testthat::test_that("ORWA, trans, taxaid_1", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
 # for (i in seq_len(length(fn_all))) {
    fn_i <- df_pickfiles[i , "filename"]
    df_i <- read.csv(file.path(dn_data, fn_i))
    i_taxaid <- df_pickfiles[i, "taxaid"]
    i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
    
    # # Remove DNI
    # df_i <- df_i[!df_i[, i_taxaid_match] == "DNI", ]

    n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
    n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
    
    # show mismatches
    print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
                 "' from file '", fn_i, "'"))
    i1 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                           df_i[, i_taxaid]]
    sort(i1)
    
    # Remove DNI
    n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
    
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
testthat::test_that("ORWA, trans, taxaid_2", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2 #+ 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]

  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
               "' from file '", fn_i, "'"))
  i2 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i2)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, trans, taxaid_3", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
               "' from file '", fn_i, "'"))
  i3 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i3)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, trans, taxaid_4", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4 #+ 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]

  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
               "' from file '", fn_i, "'"))
  i4 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i4)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, trans, taxaid_5", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 5 #+ 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
               "' from file '", fn_i, "'"))
  i5 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i5)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, trans, taxaid_6", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 6 #+ 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
               "' from file '", fn_i, "'"))
  i6 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i6)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, trans, taxaid_7", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 7 
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, 
               "' from file '", fn_i, "'"))
  i7 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i7)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All missing taxa
# sort(unique(c(i1, i2, i3, i4, i5, i6, i7)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# attr, taxaid ----
testthat::test_that("ORWA, attr, taxaid1", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j1 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j1)
  
  # Remove DNI
  # n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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
testthat::test_that("ORWA, attr, taxaid_2", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j2 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j2)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, attr, taxaid_3", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3 #+ 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j3 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j3)
  
  # Remove DNI
  # n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, attr, taxaid_4", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j4 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j4)
  
  # Remove DNI
 # n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, attr, taxaid_5", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 5# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j5 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j5)
  
  # Remove DNI
  # n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, attr, taxaid_6", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 6 # + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j6 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j6)
  
  # Remove DNI
  # n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
testthat::test_that("ORWA, attr, taxaid_7", {
  # data files
  dn_data <- file.path("data", "taxa_official", "ORWA")
  df_pickfiles <- read.csv(file.path(dn_data, "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 7 # + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path(dn_data, fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path(dn_data, fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, 
               "' from file '", fn_i, "'"))
  j7 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j7)
  
  # Remove DNI
  # n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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
#   i <- 1 + 6
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
# sort(unique(c(j1, j2, j3, j4, j5, j6, j7)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
