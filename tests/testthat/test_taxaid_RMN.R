# Check taxaid for each file
# Ensure each OTU is in the match column
# 2025-02-25
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2025-02-25, Three files in "_pick_filse_RMN.csv" so need 3 of each test
# some files duplicated but ok
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bugs ----
## duplicate names ----
# check for duplicates in the 'taxaid' column from pickfiles
testthat::test_that("RMN, trans, dups1", {
  # data files
  df_pickfiles <- read.csv(file.path("data"
                                     , "taxa_official"
                                     , "RMN"
                                     , "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  ii <- 1# + 4
  
  # for (i in seq_len(length(fn_all))) {
  fn_ii <- df_pickfiles[ii , "filename"]
  df_ii <- read.csv(file.path("data", "taxa_official", "RMN", fn_ii))
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

testthat::test_that("RMN, trans, dups2", {
  # data files
  df_pickfiles <- read.csv(file.path("data"
                                     , "taxa_official"
                                     , "RMN"
                                     , "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  ii <- 2# + 4
  
  # for (i in seq_len(length(fn_all))) {
  fn_ii <- df_pickfiles[ii , "filename"]
  df_ii <- read.csv(file.path("data", "taxa_official", "RMN", fn_ii))
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
  
})## Test ~ 2 dup, trans

testthat::test_that("RMN, trans, dups3", {
  # data files
  df_pickfiles <- read.csv(file.path("data"
                                     , "taxa_official"
                                     , "RMN"
                                     , "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  ii <- 3# + 4
  
  # for (i in seq_len(length(fn_all))) {
  fn_ii <- df_pickfiles[ii , "filename"]
  df_ii <- read.csv(file.path("data", "taxa_official", "RMN", fn_ii))
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
  
})## Test ~ 2 dup, trans

testthat::test_that("RMN, attr, dups1", {
  
  # data files
  df_pickfiles <- read.csv(file.path("data"
                                     , "taxa_official"
                                     , "RMN"
                                     , "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  jj <- 1 #+ 6

  # attribute file
  fn_jj <- df_pickfiles[jj , "attributes_filename"]
  df_jj <- read.csv(file.path("data", "taxa_official", "RMN", fn_jj))
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

testthat::test_that("RMN, attr, dups2", {
  
  # data files
  df_pickfiles <- read.csv(file.path("data"
                                     , "taxa_official"
                                     , "RMN"
                                     , "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  jj <- 2 #+ 6
  
  # attribute file
  fn_jj <- df_pickfiles[jj , "attributes_filename"]
  df_jj <- read.csv(file.path("data", "taxa_official", "RMN", fn_jj))
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

testthat::test_that("RMN, attr, dups3", {
  
  # data files
  df_pickfiles <- read.csv(file.path("data"
                                     , "taxa_official"
                                     , "RMN"
                                     , "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  jj <- 3 #+ 6
  
  # attribute file
  fn_jj <- df_pickfiles[jj , "attributes_filename"]
  df_jj <- read.csv(file.path("data", "taxa_official", "RMN", fn_jj))
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
## trans, taxaid ----
testthat::test_that("RMN, trans, taxaid_1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official", 
                                     "RMN",
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
 # for (i in seq_len(length(fn_all))) {
    fn_i <- df_pickfiles[i , "filename"]
    df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
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
    #n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
    
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

testthat::test_that("RMN, trans, taxaid_2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official", 
                                     "RMN",
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
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
  #n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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

testthat::test_that("RMN, trans, taxaid_3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official", 
                                     "RMN",
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
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
  #n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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

testthat::test_that("RMN, trans, taxaid_CAPS", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official", 
                                     "RMN",
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # # Remove DNI
  # df_i <- df_i[!df_i[, i_taxaid_match] == "DNI", ]
  
  # to upper for this test
  df_i[, i_taxaid_match] <- toupper(df_i[, i_taxaid_match])
  df_i[, i_taxaid] <- toupper(df_i[, i_taxaid])
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_i[, i_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  
  # show mismatches
  print(paste0("Unique '", i_taxaid_match, "' missing from '", i_taxaid, "'"))
  i1 <- unique(df_i[, i_taxaid_match])[!unique(df_i[, i_taxaid_match]) %in% 
                                         df_i[, i_taxaid]]
  sort(i1)
  
  # Remove DNI
  #n_match_QC <- n_match_QC - "DNI" %in% df_i[, i_taxaid_match]
  
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
# All missing taxa
# sort(unique(c(i1, i2, i3, i4, i5)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## attr, taxaid ----
testthat::test_that("RMN, attr, taxaid1", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official",
                                     "RMN", 
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 1# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", "RMN", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  print(fn_j)
  print(fn_i)
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

testthat::test_that("RMN, attr, taxaid2", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official",
                                     "RMN", 
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 2# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", "RMN", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  print(fn_j)
  print(fn_i)
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
testthat::test_that("RMN, attr, taxaid3", {
  # data files
  df_pickfiles <- read.csv(file.path("data", 
                                     "taxa_official",
                                     "RMN", 
                                     "_pick_files_RMN.csv"))
  fn_all <- df_pickfiles$filename
  
  i <- 3# + 6
  
  # for (i in seq_len(length(fn_all))) {
  fn_i <- df_pickfiles[i , "filename"]
  df_i <- read.csv(file.path("data", "taxa_official", "RMN", fn_i))
  i_taxaid <- df_pickfiles[i, "taxaid"]
  i_taxaid_match <- df_pickfiles[i, "calc_taxaid"]
  
  # attribute file
  fn_j <- df_pickfiles[i , "attributes_filename"]
  df_j <- read.csv(file.path("data", "taxa_official", "RMN", fn_j))
  j_taxaid <- df_pickfiles[i, "attributes_taxaid"]
  
  n_match_calc <- sum(unique(df_i[, i_taxaid_match]) %in% df_j[, j_taxaid])
  n_match_QC <- length(unique(df_i[, i_taxaid_match]))
  
  # show mismatches
  print(paste0("Unique '", j_taxaid, "' missing from '", i_taxaid_match, "'"))
  print(fn_j)
  print(fn_i)
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

