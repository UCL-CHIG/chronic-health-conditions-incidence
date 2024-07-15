# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Describes cohort demographics
# *******************************************


setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
load("chc_cumul/processed/cohort_spine.rda")
cohort_spine[is.na(multiple), multiple := 0]
cohort_spine[, multiple := as.logical(multiple)]

describe_outcome <- function(exposures, outcome = NULL, dat, n_round = 0, p_round = 1) {
  
  if (!is.null(outcome)) {
    # tmp_table <- round(table(dat[[exposures[1]]],
    #                          dat[[outcome]], useNA = "always"), n_round)
    # prop_tmp_table <- round(prop.table(tmp_table, 1) * 100, p_round)
    # output <- matrix(
    #   rep(NA, nrow(tmp_table) * 1),
    #   nrow = nrow(tmp_table),
    #   ncol = 1
    # )
    # colnames(output) <- outcome
    # rownames(output) <- paste0(exposures[1], "_", names(tmp_table[, 2]))
    # output[, 1] <- paste0(tmp_table[, 2], " (", prop_tmp_table[, 2], "%)")
    # 
    # if (length(exposures) > 1) {
    #   for (j in 2:length(exposures)) {
    #     tmp_table <- round(table(dat[[exposures[j]]],
    #                              dat[[outcome]], useNA = "always"), n_round)
    #     prop_tmp_table <- round(prop.table(tmp_table, 1) * 100, p_round)
    #     tmp_matrix <- matrix(
    #       rep(NA, nrow(tmp_table) * 1),
    #       nrow = nrow(tmp_table),
    #       ncol = 1
    #     )
    #     colnames(tmp_matrix) <- outcome
    #     rownames(tmp_matrix) <- paste0(exposures[j], "_", names(tmp_table[, 2]))
    #     tmp_matrix[, 1] <- paste0(tmp_table[, 2], " (", prop_tmp_table[, 2], "%)")
    #     output <- rbind(output, tmp_matrix)
    #   }
    # }
    # 
    # return(output)
    
  } else {
    
    tmp_table <- round(table(dat[[exposures[1]]], useNA = "always"), n_round)
    tmp_table_no_na <- tmp_table[-length(tmp_table)]
    
    prop_tmp_table <- round(prop.table(tmp_table_no_na) * 100, p_round)
    prop_tmp_table <- c(prop_tmp_table, "-")
    
    output <- matrix(
      rep(NA, nrow(tmp_table) * 1),
      nrow = nrow(tmp_table),
      ncol = 1
    )
    
    rownames(output) <- paste0(exposures[1], "_", names(tmp_table))
    output[, 1] <- paste0(tmp_table, " (", prop_tmp_table, "%)")
    
    
    if (length(exposures) > 1) {
      for (j in 2:length(exposures)) {
        
        tmp_table <- round(table(dat[[exposures[j]]], useNA = "always"), n_round)
        tmp_table_no_na <- tmp_table[-length(tmp_table)]
        
        prop_tmp_table <- round(prop.table(tmp_table_no_na) * 100, p_round)
        prop_tmp_table <- c(prop_tmp_table, "-")
        
        tmp_matrix <- matrix(
          rep(NA, nrow(tmp_table) * 1),
          nrow = nrow(tmp_table),
          ncol = 1
        )

        rownames(tmp_matrix) <- paste0(exposures[j], "_", names(tmp_table))
        tmp_matrix[, 1] <- paste0(tmp_table, " (", prop_tmp_table, "%)")
        output <- rbind(output, tmp_matrix)
        
      }
    }
    
    return(output)
    
  }
}

exposures <- c(paste0("el_scheme_", 1:8))

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2003]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2004]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2005]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2006]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2007]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2008]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2009]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2010]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2011]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2012]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine))

colnames(final_output) <- c(paste0("birth", 2003:2012), "all")

write.csv(final_output, file = "chc_cumul/outputs/demographics/eligibility.csv")

exposures <- c("female", "ethnos_clean", "imd_quintile_birth", "resgor_clean",
               "lbw", "bwt_cat", "prem", "ga_cat", "teen_mother", "multiple")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_1 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_2 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_3 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_4 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_5 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_6 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_7 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004 & el_scheme_8 == T]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth %in% 2003:2004]))

colnames(final_output) <- c(paste0("el_scheme_", 1:8), "all")
write.csv(final_output, file = "chc_cumul/outputs/demographics/demographics_b2003-b2004.csv")

final_output <- cbind(describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2003]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2004]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2005]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2006]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2007]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2008]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2009]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2010]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2011]),
                      describe_outcome(exposure = exposures, outcome = NULL, dat = cohort_spine[academicyearofbirth == 2012]))

colnames(final_output) <- paste0("b", 2003:2012)
write.csv(final_output, file = "chc_cumul/outputs/demographics/demographics_all_years.csv")

rm(list = ls()); gc()
