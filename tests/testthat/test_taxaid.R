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

# duplicate names ----
test_that("trans, dups1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  
  # QC
  n_taxa_calc <- length(unique(df_i[, i_taxaid]))
  n_taxa_qc   <- length(df_i[, i_taxaid])
  
  # show mismatches
  print(paste0("Duplicate names '", i_taxaid, "' from file '", fn_i, "'"))
  n_taxa_table <- as.data.frame(table(df_i[, i_taxaid]))
  n_taxa_table[n_taxa_table$Freq > 1, ]
  
  #test
  testthat::expect_equal(n_taxa_calc, n_taxa_qc)
  
})## Test ~ dup, trans

test_that("attr, dups1", {
  
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1 #+ 6

  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  # QC
  n_taxa_calc <- length(unique(df_j[, j_taxaid]))
  n_taxa_qc   <- length(df_j[, j_taxaid])
  
  # show mismatches
  print(paste0("Duplicate names '", j_taxaid, "' from file '", fn_j, "'"))
  n_taxa_table <- as.data.frame(table(df_j[, j_taxaid]))
  n_taxa_table[n_taxa_table$Freq > 1, ]
  
  # test
  testthat::expect_equivalent(n_taxa_calc, n_taxa_qc)
  
})## Test ~ dup, trans

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trans, taxaid ----
test_that("trans, taxaid_1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
 # for (i in seq_len(length(fn_all))) {
    fn_i <- df_pickfiles[i , "filename"]
    df_i <- read.csv(file.path("data", "taxa_official", fn_i))
    i_taxaid <- df_pickfiles[i, "taxaid"]
    i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
    
    # # Remove DNI
    # df_i <- df_i[!df_i[, i_taxaid_match] == "DNI", ]

    n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
    n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
    
    # show mismatches
    print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
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
test_that("trans, taxaid_2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2 #+ 6
  
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
  sort(i2)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("trans, taxaid_3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3# + 6
  
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
  sort(i3)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("trans, taxaid_4", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4 #+ 6
  
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
  sort(i4)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("trans, taxaid_5", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 5 #+ 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
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
# All missing taxa
# sort(unique(c(i1, i2, i3, i4, i5)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# attr, taxaid ----
test_that("attr, taxaid1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
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
  sort(j1)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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
test_that("attr, taxaid_2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2# + 6
  
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
  sort(j2)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attr, taxaid_3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3 #+ 6
  
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
  sort(j3)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attr, taxaid_4", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 4# + 6
  
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
  sort(j4)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
  # test
  testthat::expect_equivalent(n_match_calc, n_match_QC)
  
  # }## FOR ~ i
  
})## Test ~ taxaid

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attr, taxaid_5", {
  # data files
  df_pickfiles <- read.csv(file.path("data", "taxa_official", "_pick_files.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 5# + 6
  
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
  j5 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_j[, j_taxaid]]
  sort(j5)
  
  # Remove DNI
  n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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
# sort(unique(c(j1, j2, j3, j4)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

