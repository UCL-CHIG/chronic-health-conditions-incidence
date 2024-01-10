# *******************************************
# Matthew Jay, matthew.jay@ucl.ac.uk
# *******************************************

# load cohort --------------------------------------------------------------

setwd("[]:/Working/Matt/")
assign(".lib.loc", c(.libPaths(), "[]:/Working/Matt/r"), envir = environment(.libPaths))
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)

load("chc_cumul/processed/chc_diagnoses.rda")
load("chc_cumul/processed/cohort_spine_censor.rda")

chc_diagnoses <- chc_diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]
chc_diagnoses[, age := as.integer(trunc(difftime(epistart, dob, units = "days")) / 365.25)]
chc_diagnoses <- chc_diagnoses[, c("encrypted_hesid", "epistart", "age", "diag", "type")]
gc()

# which metetc codes?
metetc <- chc_diagnoses[type == "Metabolic/endocrine/digestive/renal/genitourinary"]
metetc <- metetc[age < 8]

metetc <- merge(metetc,
                cohort_spine[, c("encrypted_hesid", "academicyearofbirth")],
                by = "encrypted_hesid",
                all.x = T)

write.csv(table(metetc$diag, metetc$academicyearofbirth), file = "chc_cumul/outputs/metetc_codes.csv")




# check whether K52.9
master_dir <- "P:/Working/Master data TEST"
diagnoses <- fread(paste0(master_dir, "/HES_APC_DIAG_combined.csv"),
                   header = T,
                   stringsAsFactors = F,
                   integer64 = "character")

diagnoses <- diagnoses[substr(diag, 1, 3) == "K52"]
diagnoses[, diag := substr(diag, 1, 4)]
diagnoses <- diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]
diagnoses <- diagnoses[epistart <= as.Date("2019-08-31")]
diagnoses <- diagnoses[startage < 8]

diagnoses[, dup := duplicated(diagnoses)]
diagnoses <- diagnoses[dup == F]
diagnoses[, dup := NULL]

diagnoses <- merge(diagnoses,
                   cohort_spine[, c("encrypted_hesid", "academicyearofbirth")],
                   by = "encrypted_hesid",
                   all.x = T)

table(diagnoses$diag, diagnoses$academicyearofbirth)
write.csv(table(diagnoses$diag, diagnoses$academicyearofbirth), file = "chc_cumul/outputs/metetc_codes_k52.csv")
