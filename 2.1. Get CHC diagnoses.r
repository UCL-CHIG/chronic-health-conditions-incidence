
# *******************************************
# Matthew Jay, matthew.jay@ucl.ac.uk
# *******************************************

options(scipen = 999)

setwd("[]:/Working/Matt/")
assign(".lib.loc", c(.libPaths(), "[]:/Working/Matt/r"), envir = environment(.libPaths))
library(data.table)

master_dir <- "[]:/Working/Master data TEST"

load("chc_cumul/processed/cohort_spine_censor.rda")

# load diagnosis codes
diagnoses <- fread(paste0(master_dir, "/HES_APC_DIAG_combined.csv"),
                   header = T,
                   stringsAsFactors = F,
                   integer64 = "character")

# Subset to those in cohort
diagnoses <- diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]

# get age vars
diagnoses <- merge(diagnoses,
                   cohort_spine[, c("encrypted_hesid", "academicyearofbirth", "dob")],
                   by = "encrypted_hesid",
                   all.x = T)

rm(cohort_spine)

# check epikey (a problem with epikeys of 19 characters)
lt <- as.POSIXlt(diagnoses$epistart)
diagnoses$epifyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

#table(diagnoses$epifyear, nchar(diagnoses$epikey))



# calculate LOS using the cleaned start and end dates
# get admission dates and startage
all_episodes <- fread(paste0(master_dir, "/HES_APC_DIS_ADMI_EPI_combined.csv"),
                      header = T,
                      stringsAsFactors = F)

all_episodes <- all_episodes[encrypted_hesid %in% diagnoses$encrypted_hesid]
all_episodes[, epikey := as.character(epikey)]
# table(nchar(all_episodes$epikey))

lt <- as.POSIXlt(all_episodes$epistart)
all_episodes$epifyear <- lt$year + (lt$mo >= 3) + 1900
rm(lt)

# table(all_episodes$epifyear, nchar(all_episodes$epikey))
# 
# table(diagnoses$epikey %in% all_episodes$epikey)
# table(diagnoses$epifyear, diagnoses$epikey %in% all_episodes$epikey)

# 2021 onwards a problem - let's implicitly drop them
all_episodes <- all_episodes[epikey %in% diagnoses$epikey]
# table(all_episodes$epifyear)

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

rm(all_episodes)

# drop missing admidate
#table(is.na(diagnoses$admidate), diagnoses$epifyear)
diagnoses <- diagnoses[!is.na(admidate)]

diagnoses[, admi_los_nights := as.integer(difftime(disdate, admidate, units = "days"))]
#summary(diagnoses$admi_los_nights)
#table(diagnoses$admi_los_nights < 0, useNA = "always")

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
chc_diagnoses[flag == "AGE10" & (startage < 10 | startage >= 7001), drop := T]
chc_diagnoses <- chc_diagnoses[drop == F]
chc_diagnoses[, drop := NULL]

# make startage > 7000 = 0 as is easier in the code later
chc_diagnoses[startage >= 7001, startage := 0]

# drop any aged 16 or greater as not needed
chc_diagnoses <- chc_diagnoses[startage < 16]

# drop after September 2020
chc_diagnoses <- chc_diagnoses[epistart < as.Date("2020-09-01")]

# save
save(chc_diagnoses, file = "chc_cumul/processed/chc_diagnoses.rda")
rm(list = ls()); gc()
