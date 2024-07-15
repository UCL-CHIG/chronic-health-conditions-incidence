# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Initial processing for open cohorts using ONS denominators
# *******************************************

options(scipen = 999)

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)

master_dir <- "[path omitted]"

# load diagnosis codes
diagnoses <- fread(paste0(master_dir, "/HES_APC_DIAG_combined.csv"),
                   header = T,
                   stringsAsFactors = F,
                   integer64 = "character")

lt <- as.POSIXlt(diagnoses$epistart)
diagnoses$epifyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

# Get month and year of birth
mydob <- fread(paste0(master_dir, "/HES_APC_mybirth_combined.csv"),
               header = T,
               stringsAsFactors = F,
               integer64 = "character")

diagnoses <- merge(diagnoses,
                   mydob[, c("encrypted_hesid", "my_dob")],
                   by = "encrypted_hesid",
                   all.x = T)

rm(mydob); gc()

lt <- as.POSIXlt(diagnoses$my_dob)
diagnoses$birthfyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

#table(diagnoses$birthfyear, useNA = "always")

diagnoses <- diagnoses[birthfyear %in% 2003:2019]

diagnoses <- diagnoses[epifyear %in% 2003:2020]
diagnoses[startage > 7000, startage := 0]
diagnoses <- diagnoses[startage < 16]

# Deduplicate per patient and code.
# We only need the first instance of each code
diagnoses <- diagnoses[order(encrypted_hesid, diag)]
diagnoses <- diagnoses[, dups := duplicated(diagnoses[, c("encrypted_hesid", "diag")])]
diagnoses <- diagnoses[dups == F]
diagnoses[, dups := NULL]

# calculate LOS using the cleaned start and end dates
gc()
all_episodes <- fread(paste0(master_dir, "/HES_APC_DIS_ADMI_EPI_combined.csv"),
                      header = T,
                      stringsAsFactors = F)

all_episodes <- all_episodes[encrypted_hesid %in% diagnoses$encrypted_hesid]
all_episodes[, epikey := as.character(epikey)]
all_episodes <- all_episodes[epikey %in% diagnoses$epikey]

lt <- as.POSIXlt(all_episodes$epistart)
all_episodes$epifyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

# missing end dates
#table(all_episodes$epifyear, is.na(all_episodes$disdate))
all_episodes[!is.na(epiend) & is.na(disdate), disdate := epiend]
#table(all_episodes$epifyear, is.na(all_episodes$disdate))

# merge
diagnoses <- merge(diagnoses,
                   all_episodes[, c("encrypted_hesid",
                                    "epikey",
                                    "admidate",
                                    "disdate")],
                   by = c("encrypted_hesid", "epikey"),
                   all.x = T)

rm(all_episodes); gc()

# drop missing admidate
sum(is.na(diagnoses$admidate))
# table(is.na(diagnoses$admidate), diagnoses$epicyear)
# diagnoses <- diagnoses[!is.na(admidate)]

diagnoses[, admi_los_nights := as.integer(difftime(disdate, admidate, units = "days"))]
# summary(diagnoses$admi_los_nights)
# table(diagnoses$admi_los_nights < 0)

diagnoses[admi_los_nights < 0, admidate := epistart]
diagnoses[admi_los_nights < 0, admi_los_nights := 0]

# NOW IMPORT HARDELID CODES
hardelid <- fread("code lists/hardelid.csv",
                  header = T,
                  stringsAsFactors = F)

hardelid[, code := gsub("\\.", "", code)]

diagnoses[, diag := substr(diag, 1, 4)]

diagnoses[, chc := F]
diagnoses[substr(diag, 1, 3) %in% hardelid[nchar(code) == 3]$code, chc := T]
diagnoses[substr(diag, 1, 3) %in% hardelid[nchar(code) == 3]$code, diag := substr(diag, 1, 3)]

diagnoses[substr(diag, 1, 4) %in% hardelid[nchar(code) == 4]$code, chc := T]
diagnoses[substr(diag, 1, 4) %in% hardelid[nchar(code) == 4]$code, diag := substr(diag, 1, 4)]

chc_diagnoses <- diagnoses[chc == T]
chc_diagnoses[, chc := NULL]
rm(diagnoses)

chc_diagnoses <- merge(chc_diagnoses,
                       hardelid[, c("code", "flag", "type", "category")],
                       by.x = "diag",
                       by.y = "code",
                       all.x = T)

rm(hardelid)

# deal with flags
chc_diagnoses[, drop := F]
chc_diagnoses[flag == "LOS3" & admi_los_nights < 3, drop := T]
chc_diagnoses[flag == "AGE10" & startage < 10, drop := T]
chc_diagnoses <- chc_diagnoses[drop == F]
chc_diagnoses[, drop := NULL]

# tidy
chc_diagnoses <- chc_diagnoses[, c("encrypted_hesid",
                                   "my_dob",
                                   "birthfyear",
                                   "epifyear",
                                   "epistart",
                                   "startage",
                                   "diag",
                                   "type",
                                   "category")]

chc_diagnoses <- chc_diagnoses[order(encrypted_hesid, epistart)]

# save
save(chc_diagnoses, file = "1_CHC_CUMUL/processed/chc_diagnoses_ons.rda")
rm(list = ls()); gc()
