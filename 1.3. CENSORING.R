# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Creates censoring indicators
# *******************************************


setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
library(lubridate)

load("chc_cumul/processed/cohort_spine.rda")

master_dir <- "[path omitted]"

# applicable in all scenarios (whichever comes first)
# rule 1: death
# rule 2: first HES record where not resident in England
# rule 3: 16th birthday

# Deaths ------------------------------------------------------------------

death <- fread(paste0(master_dir, "/HES_Death_Dates.csv"),
               header = T,
               stringsAsFactors = F)

death <- death[encrypted_hesid %in% cohort_spine$encrypted_hesid]

death[, dod_approx := as.Date(paste0(dod_year, "-", dod_month, "-15"))]

# length(unique(death$encrypted_hesid))
# nrow(death)

death <- death[order(encrypted_hesid, dod_approx)]
death <- death[, c("encrypted_hesid", "dod_approx")]
death <- death[!duplicated(death$encrypted_hesid)]

# length(unique(death$encrypted_hesid))
# nrow(death)

cohort_spine <- merge(cohort_spine,
                      death[, c("encrypted_hesid", "dod_approx")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(death); gc()

# table(cohort_spine$dod_approx < cohort_spine$dob) # induced by the dob being approx - children who died same month as born
# difftime(cohort_spine[dod_approx < dob]$dod_approx, cohort_spine[dod_approx < dob]$dob, units = "days")

cohort_spine[dod_approx < dob, dod_approx := dob]

# Non-England HES activity ------------------------------------------------

# * APC ---------------------------------------------------------------------

episodes <- fread(paste0(master_dir, "/HES_APC_DIS_ADMI_EPI_combined.csv"),
                  header = T,
                  stringsAsFactors = F)

episodes <- episodes[encrypted_hesid %in% cohort_spine$encrypted_hesid, c("encrypted_hesid", "epikey", "epistart")]
gc()

area <- fread(paste0(master_dir, "/HES_APC_Area_combined.csv"),
              header = T,
              stringsAsFactors = F)

area <- area[encrypted_hesid %in% cohort_spine$encrypted_hesid & (resgor %in% c("S", "W", "X", "Z") |
                                                                    resladst %in% c("S", "W", "X", "Z") |
                                                                    rescty %in% c("S", "W", "X", "Z")),
             c("encrypted_hesid", "epistart", "resgor", "resladst", "rescty")]

# table(area$resgor, useNA = "always")
# table(area$resladst, useNA = "always")
# table(area$rescty, useNA = "always")

# get first per child
area <- area[order(encrypted_hesid, epistart)]
area[, record_idx := seq_len(.N), by = .(encrypted_hesid)]
area <- area[record_idx == 1]
# length(unique(area$encrypted_hesid)); nrow(area)

# merge
setnames(area, "epistart", "date_first_non_eng_apc")

cohort_spine <- merge(cohort_spine,
                      area[, c("encrypted_hesid", "date_first_non_eng_apc")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(area, episodes); gc()
# summary(cohort_spine$date_first_non_eng_apc)

# * OPA -------------------------------------------------------------------

area <- fread(paste0(master_dir, "/HES_OPA_Area_combined.csv"),
              header = T,
              stringsAsFactors = F)

area <- area[encrypted_hesid %in% cohort_spine$encrypted_hesid & (resgor %in% c("S", "W", "X", "Z") |
                                                                    resha %in% c("S", "W", "X", "Z") |
                                                                    rescty %in% c("S", "W", "X", "Z")),
             c("encrypted_hesid", "apptdate", "resgor", "resha", "rescty")]

# table(area$resgor, useNA = "always")
# table(area$resha, useNA = "always")
# table(area$rescty, useNA = "always")

# get first per child
area <- area[order(encrypted_hesid, apptdate)]
area[, record_idx := seq_len(.N), by = .(encrypted_hesid)]
area <- area[record_idx == 1]
# length(unique(area$encrypted_hesid)); nrow(area)

# merge
setnames(area, "apptdate", "date_first_non_eng_opa")

cohort_spine <- merge(cohort_spine,
                      area[, c("encrypted_hesid", "date_first_non_eng_opa")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(area); gc()
# summary(cohort_spine$date_first_non_eng_opa)

# * AE --------------------------------------------------------------------

area <- fread(paste0(master_dir, "/HES_AE_Area_combined.csv"),
              header = T,
              stringsAsFactors = F)

area <- area[encrypted_hesid %in% cohort_spine$encrypted_hesid & (resgor %in% c("S", "W", "X", "Z") |
                                                                    resladst %in% c("S", "W", "X", "Z") |
                                                                    rescty %in% c("S", "W", "X", "Z")),
             c("encrypted_hesid", "arrivaldate", "resgor", "resladst", "rescty")]

# table(area$resgor, useNA = "always")
# table(area$resladst, useNA = "always")
# table(area$rescty, useNA = "always")

# get first per child
area <- area[order(encrypted_hesid, arrivaldate)]
area[, record_idx := seq_len(.N), by = .(encrypted_hesid)]
area <- area[record_idx == 1]
# length(unique(area$encrypted_hesid)); nrow(area)

# merge
setnames(area, "arrivaldate", "date_first_non_eng_ae")

cohort_spine <- merge(cohort_spine,
                      area[, c("encrypted_hesid", "date_first_non_eng_ae")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(area); gc()
# summary(cohort_spine$date_first_non_eng_ae)

# * combine ---------------------------------------------------------------

cohort_spine[, date_first_non_eng_hes := min(date_first_non_eng_apc,
                                             date_first_non_eng_opa,
                                             date_first_non_eng_ae,
                                             na.rm = T),
             by = .(encrypted_hesid)]

cohort_spine[is.na(date_first_non_eng_apc) &   # Not sure why this is necessary but the apparent NAs returned in the previous line
               is.na(date_first_non_eng_opa) & # are not true NAs.
               is.na(date_first_non_eng_ae),
             date_first_non_eng_hes := NA]

# table(cohort_spine$date_first_non_eng_hes < cohort_spine$dob) # 1 - one day before dob. Set to dob.
cohort_spine[date_first_non_eng_hes < dob, date_first_non_eng_hes := dob]

# 16th Birthday -----------------------------------------------------------

cohort_spine[, bday_16 := ymd(cohort_spine$dob) + years(16)]
# table(is.na(cohort_spine$bday_16))

# Ignore later events -----------------------------------------------------

# table(cohort_spine$dod_approx > cohort_spine$bday_16)
cohort_spine[dod_approx > bday_16, dod_approx := NA]

# table(cohort_spine$date_first_non_eng_hes > cohort_spine$bday_16)
cohort_spine[date_first_non_eng_hes > bday_16, date_first_non_eng_hes := NA]

# Check duplicates --------------------------------------------------------

length(unique(cohort_spine$encrypted_hesid))
nrow(cohort_spine)

# Save --------------------------------------------------------------------

save(cohort_spine, file = "chc_cumul/processed/cohort_spine_censor.rda")
rm(list = ls())
