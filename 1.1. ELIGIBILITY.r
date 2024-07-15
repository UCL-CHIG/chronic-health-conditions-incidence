
# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Identifies birth records, links in first activity and creates eligility flags
# *******************************************

# SET UP WORKSPACE --------------------------------------------------------

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
library(RODBC)
library(dplyr)

master_dir <- "[path omitted]"
birth_cohorts <- 2003:2012

# LOAD BIRTHS ---------------------------------------------------------------

birth_episodes <- fread(paste0(master_dir, "/HES_APC_Baby_Tail_clean_vars_combined.csv"),
                       header = T,
                       stringsAsFactors = F)

lt <- as.POSIXlt(birth_episodes$bday)
birth_episodes[, academicyearofbirth := lt$year + (lt$mo >= 8) + 1900]
rm(lt)

cohort_spine <- birth_episodes[academicyearofbirth %in% birth_cohorts, c("encrypted_hesid",
                                                                         "bday",
                                                                         "academicyearofbirth",
                                                                         "birweit",
                                                                         "gestat",
                                                                         "matage",
                                                                         "multiple",
                                                                         "stillb")]

setnames(cohort_spine, "bday", "dob")

rm(birth_episodes); gc()

# GET MBABY LINK ----------------------------------------------------------

mbaby <- fread(paste0(master_dir, "/Birth Cohort/echild_birth_cohort_major_malformation_flags_v2_20220324.csv"),
               header = T,
               stringsAsFactors = F)

mbaby <- mbaby[, c("encrypted_hesid", "matres", "matimd")]
mbaby <- mbaby[encrypted_hesid %in% cohort_spine$encrypted_hesid]
#length(unique(mbaby$encrypted_hesid))

cohort_spine <- merge(cohort_spine,
                      mbaby,
                      by = "encrypted_hesid",
                      all.x = T)

rm(mbaby); gc()

# GET REGION --------------------------------------------------------------

area_dt <- fread(paste0(master_dir, "/HES_APC_Area_combined.csv"),
                 header = T,
                 stringsAsFactors = F)

area_dt <- area_dt[encrypted_hesid %in% cohort_spine$encrypted_hesid]

area_dt <- area_dt[, c("encrypted_hesid", "epikey", "epistart", "resgor", "rescty", "resladst")]

area_dt <- merge(area_dt,
                 cohort_spine[, c("encrypted_hesid",
                                  "dob")],
                 by = "encrypted_hesid",
                 all.x = T)

area_dt[, age_at_record := as.numeric(difftime(epistart, dob, units = "days")) / 365.25]

# drop non-birth records
# table(area_dt$age_at_record == 0)
# table(area_dt$age_at_record >= 0 & area_dt$age_at_record <= 1)

area_dt <- area_dt[order(encrypted_hesid, epistart)]
area_dt[, record_idx := seq_len(.N), by = .(encrypted_hesid)]
area_dt <- area_dt[record_idx == 1]
#length(unique(area_dt$encrypted_hesid)); nrow(area_dt)

# table(area_dt[record_idx == 1]$age_at_record >= 0 & area_dt[record_idx == 1]$age_at_record <= 1)

area_dt <- area_dt[age_at_record == 0 & record_idx == 1]
#length(unique(area_dt$encrypted_hesid))



# merge into cohort_spine
cohort_spine <- merge(cohort_spine,
                      area_dt[, c("encrypted_hesid", "resgor", "rescty", "resladst")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(area_dt); gc()

cohort_spine[resgor %in% c("S", "U", "W", "Y", "Z", "X"), resgor := NA]

cohort_spine[is.na(resgor), resgor := matres]
cohort_spine[, matres := NULL]

# identify not Eng or unknown
cohort_spine[, not_england_or_unknown := resgor %in%
                c("S", "U", "W", "Y", "Z", "X") |
          rescty %in% c("S", "U", "W", "Y", "Z", "X") |
          resladst %in% c("S", "U", "W", "Y", "Z", "X")]

cohort_spine[, rescty := NULL]
cohort_spine[, resladst := NULL]

# table(cohort_spine$resgor,
#       cohort_spine$not_england_or_unknown,
#       useNA = "always")

cohort_spine[not_england_or_unknown == T, resgor := NA]

# clean
cohort_spine[, resgor_clean := factor(NA,
                                      levels = c("North East",
                                                 "North West",
                                                 "Yorkshire & The Humber",
                                                 "East Midlands",
                                                 "West Midlands",
                                                 "East of England",
                                                 "London",
                                                 "South East",
                                                 "South West"))]

cohort_spine[resgor == "A", resgor_clean := "North East"]
cohort_spine[resgor == "B", resgor_clean := "North West"]
cohort_spine[resgor == "D", resgor_clean := "Yorkshire & The Humber"]
cohort_spine[resgor == "E", resgor_clean := "East Midlands"]
cohort_spine[resgor == "F", resgor_clean := "West Midlands"]
cohort_spine[resgor == "G", resgor_clean := "East of England"]
cohort_spine[resgor == "H", resgor_clean := "London"]
cohort_spine[resgor == "J", resgor_clean := "South East"]
cohort_spine[resgor == "K", resgor_clean := "South West"]

# table(cohort_spine$resgor,
#       cohort_spine$resgor_clean,
#       useNA = "always")
# 
# table(cohort_spine$academicyearofbirth,
#       cohort_spine$resgor_clean,
#       useNA = "always")

cohort_spine[, resgor := NULL]

cohort_spine[is.na(resgor_clean), not_england_or_unknown := T]

# GET DEMOGRAPHICS --------------------------------------------------------

# gender
sex <- fread(paste0(master_dir, "/HES_APC_Sex_Combined.csv"),
             header = T,
             stringsAsFactors = F)

cohort_spine <- merge(cohort_spine,
                      sex,
                      by = "encrypted_hesid",
                      all.x = T)
rm(sex); gc()

# table(cohort_spine$sex, useNA = "always")

# ethnicity
ethnos_new <- fread(paste0(master_dir, "/HES_APC_Ethnos_New_Modal.csv"),
                    header = T,
                    stringsAsFactors = F)

cohort_spine <- merge(cohort_spine,
                      ethnos_new,
                      by = "encrypted_hesid",
                      all.x = T)

# table(is.na(cohort_spine$ethnos))
# table(cohort_spine$academicyearofbirth,
#       is.na(cohort_spine$ethnos))

ethnos_old <- fread(paste0(master_dir, "/HES_APC_Ethnos_Old_Modal.csv"),
                    header = T,
                    stringsAsFactors = F)

setnames(ethnos_old, c("ethnos", "ethnos_description"), c("ethnos_old", "ethnos_description_old"))

cohort_spine <- merge(cohort_spine,
                      ethnos_old,
                      by = "encrypted_hesid",
                      all.x = T)

# table(is.na(cohort_spine$ethnos_old))
# table(cohort_spine$academicyearofbirth,
#       is.na(cohort_spine$ethnos_old))
# table(new = is.na(cohort_spine$ethnos),
#       old = is.na(cohort_spine$ethnos_old))

rm(ethnos_new, ethnos_old); gc()

# IMD
imd <- fread(paste0(master_dir, "/HES_APC_IMD_Decile_Combined.csv"),
             header = T,
             stringsAsFactors = F)

imd <- imd[encrypted_hesid %in% cohort_spine$encrypted_hesid]

imd <- merge(imd,
             cohort_spine[, c("encrypted_hesid", "dob")],
             by = "encrypted_hesid",
             all.x = T)

imd[, age_at_epi := trunc(as.numeric(difftime(epistart, dob, units = "days") / 365.25))]
# table(imd$age_at_epi < 0)
# table(imd$age_at_epi == 0)
# table(imd$age_at_epi >= 0 & imd$age_at_epi < 6) 

imd <- imd[age_at_epi == 0] # we will use child's birth and mother's if NA
# anyNA(imd$imd04_decile)

# mode.fun <- function(x) {
#   ux <- unique(x)
#   tab <- tabulate(match(x, ux))
#   md <- ux[tab == max(tab)]
#   if (length(md) == 1) {
#     return(md)
#   } else {
#     return(md[which(rmultinom(1, 1, rep(1/length(md), length(md))) == 1)]) # modal unless multimodal, in which case it takes one at random
#   }
# }
# 
# set.seed(1)
# imd[, imd_decile_modal := mode.fun(imd04_decile), by = .(encrypted_hesid)]
# imd <- imd[, c("encrypted_hesid", "imd_decile_modal")]
# imd <- imd[!duplicated(imd)]

setnames(imd, "imd04_decile", "imd_decile_birth")

imd <- imd[order(encrypted_hesid, epistart)]
imd[, record_idx := seq_len(.N), by = .(encrypted_hesid)]
imd <- imd[record_idx == 1]
length(unique(imd$encrypted_hesid)); nrow(imd)

cohort_spine <- merge(cohort_spine,
                      imd[, c("encrypted_hesid", "imd_decile_birth")],
                      by = "encrypted_hesid",
                      all.x = T)

# table(cohort_spine$academicyearofbirth,
#       is.na(cohort_spine$imd_decile_modal))

# table(cohort_spine$academicyearofbirth,
#       is.na(cohort_spine$imd_decile_birth))

rm(imd); gc()

# table(imd_decile_birth = cohort_spine$imd_decile_birth,
#       matimd = cohort_spine$matimd,
#       useNA = "always")

# clean matimd
cohort_spine[, matimd_clean := as.integer(NA)]
cohort_spine[matimd == "Q1: Most deprived 20%", matimd_clean := 1]
cohort_spine[matimd == "Q2", matimd_clean := 2]
cohort_spine[matimd == "Q3", matimd_clean := 3]
cohort_spine[matimd == "Q4", matimd_clean := 4]
cohort_spine[matimd == "Q5: Least deprived 20%", matimd_clean := 5]

# table(matimd_clean = cohort_spine$matimd_clean,
#       matimd = cohort_spine$matimd,
#       useNA = "always")

cohort_spine[, matimd := NULL]

# convert child imd into quintile
cohort_spine[, imd_quintile_birth := as.integer(NA)]
cohort_spine[imd_decile_birth %in% 1:2, imd_quintile_birth := 1]
cohort_spine[imd_decile_birth %in% 3:4, imd_quintile_birth := 2]
cohort_spine[imd_decile_birth %in% 5:6, imd_quintile_birth := 3]
cohort_spine[imd_decile_birth %in% 7:8, imd_quintile_birth := 4]
cohort_spine[imd_decile_birth %in% 9:10, imd_quintile_birth := 5]

# table(imd_quintile_birth = cohort_spine$imd_quintile_birth,
#       imd_decile_birth = cohort_spine$imd_decile_birth,
#       useNA = "always")

cohort_spine[, imd_decile_birth := NULL]

# take mothers where child's is NA
cohort_spine[is.na(imd_quintile_birth), imd_quintile_birth := matimd_clean]

# table(cohort_spine$academicyearofbirth,
#       cohort_spine$imd_quintile_birth,
#       useNA = "always")

cohort_spine[, matimd_clean := NULL]

# CLEAN DEMOGRAPHICS ---------------------------------------------------

# ethnos
levels(factor(cohort_spine$ethnos_description))
levels(factor(cohort_spine$ethnos_description_old))

white <- c("Any other White Background",
           "British (White)",
           "Irish (White)",
           "White")

black <- c("African (Black or Black British)",
           "Any other Black Background",
           "Carribean (Black or Black British)",
           "Black - African",
           "Black - Carribean",
           "Black - Other")

mixed <- c("Any other Mixed Background",
           "White and Asian (mixed)",
           "White and Black African (mixed)",
           "White and Black Carribean (mixed)")

asian <- c("Any other Asian Background",
           "Bangladeshi (Asian or Asian British)",
           "Indian (Asian or Asian British)",
           "Pakistani (Asian or Asian British)",
           "Bangladeshi",
           "Indian",
           "Pakistani")

other <- c("Any other ethnic group",
           "Chinese (other ethnic group)",
           "Chinese")

cohort_spine[, ethnos_clean := factor(NA,
                                      levels = c("White",
                                                 "Black",
                                                 "Mixed",
                                                 "Asian",
                                                 "Other"))]

cohort_spine[ethnos_description %in% white, ethnos_clean := "White"]
cohort_spine[is.na(ethnos_clean) & ethnos_description_old %in% white, ethnos_clean := "White"]

cohort_spine[ethnos_description %in% black, ethnos_clean := "Black"]
cohort_spine[is.na(ethnos_clean) & ethnos_description_old %in% black, ethnos_clean := "Black"]

cohort_spine[ethnos_description %in% mixed, ethnos_clean := "Mixed"]
cohort_spine[is.na(ethnos_clean) & ethnos_description_old %in% mixed, ethnos_clean := "Mixed"]

cohort_spine[ethnos_description %in% asian, ethnos_clean := "Asian"]
cohort_spine[is.na(ethnos_clean) & ethnos_description_old %in% asian, ethnos_clean := "Asian"]

cohort_spine[ethnos_description %in% other, ethnos_clean := "Other"]
cohort_spine[is.na(ethnos_clean) & ethnos_description_old %in% other, ethnos_clean := "Other"]

rm(white, black, mixed, asian, other)

cohort_spine[, ethnos := NULL]
cohort_spine[, ethnos_old := NULL]
cohort_spine[, ethnos_description := NULL]
cohort_spine[, ethnos_description_old := NULL]

# table(cohort_spine$academicyearofbirth,
#       cohort_spine$ethnos_clean, useNA = "always")

# sex
# table(cohort_spine$sex, useNA = "always")
cohort_spine[is.na(sex), sex := 9]
cohort_spine[, female := sex - 1]
cohort_spine[female == 8, female := NA]
# table(cohort_spine$female, useNA = "always")
cohort_spine[, sex := NULL]

# baby vars
cohort_spine[, lbw := birweit < 2500]

cohort_spine[, bwt_cat := factor(NA,
                                 levels = c("Normal",
                                            "Low",
                                            "Very low",
                                            "Extremely low"))]

cohort_spine[birweit >= 2500, bwt_cat := "Normal"]
cohort_spine[birweit >= 1500 & birweit < 2500, bwt_cat := "Low"]
cohort_spine[birweit >= 1000 & birweit < 1500, bwt_cat := "Very low"]
cohort_spine[birweit < 1000, bwt_cat := "Extremely low"]

# table(cohort_spine$lbw,
#       cohort_spine$bwt_cat,
#       useNA = "always")

cohort_spine[, prem := gestat < 37]

cohort_spine[, ga_cat := factor(NA,
                                levels = c("Term",
                                           "Mod to late prem",
                                           "very prem",
                                           "Ex prem"))]

cohort_spine[gestat >= 37, ga_cat := "Term"]
cohort_spine[gestat >= 32 & gestat < 37, ga_cat := "Mod to late prem"]
cohort_spine[gestat >= 28 & gestat < 32, ga_cat := "very prem"]
cohort_spine[gestat < 28, ga_cat := "Ex prem"]

# table(cohort_spine$prem,
#       cohort_spine$ga_cat,
#       useNA = "always")

cohort_spine[, teen_mother := matage < 20]
# table(cohort_spine$teen_mother,
#       useNA = "always")

cohort_spine[is.na(multiple), multiple := 0]
cohort_spine[, multiple := as.logical(multiple)]

# GET LINKAGE KEY ---------------------------------------------------------------

linkage_spine <- fread(paste0(master_dir, "/NPD_HES_Linkage_Spine.csv"),
                       header = T,
                       stringsAsFactors = F)

linkage_spine <- linkage_spine[encrypted_hesid %in% cohort_spine$encrypted_hesid]

length(unique(linkage_spine$encrypted_hesid)); nrow(linkage_spine)

cohort_spine <- merge(cohort_spine,
                      linkage_spine[, c("encrypted_hesid", "PupilMatchingRefAnonymous")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(linkage_spine); gc()

# GET FIRST HES ACTIVITY ---------------------------------------------------

# * apc ---------------------------------------------------------------------

episodes <- fread(paste0(master_dir, "/HES_APC_DIS_ADMI_EPI_combined.csv"),
                  header = T,
                  stringsAsFactors = F)

episodes <- episodes[encrypted_hesid %in% cohort_spine$encrypted_hesid, c("encrypted_hesid", "epistart")]
gc()

episodes <- merge(episodes,
                  cohort_spine[, c("encrypted_hesid", "dob")],
                  by = "encrypted_hesid",
                  all.x = T)

# table(is.na(episodes$dob))
# table(is.na(episodes$epistart))

episodes[, age_at_epi := as.numeric(difftime(epistart, dob, units = "days") / 365.25)]
episodes <- episodes[age_at_epi > 0 & age_at_epi < 16]

cohort_spine[, has_subs_apc_0_4 := encrypted_hesid %in% episodes[age_at_epi < 5]$encrypted_hesid]
cohort_spine[, has_subs_apc_0_15 := encrypted_hesid %in% episodes[age_at_epi < 16]$encrypted_hesid]
cohort_spine[, has_subs_apc_5_15 := encrypted_hesid %in% episodes[age_at_epi >= 5 & age_at_epi < 16]$encrypted_hesid]

episodes <- episodes[order(encrypted_hesid, epistart)]
episodes[, epi_n := seq_len(.N), by = .(encrypted_hesid)]
episodes <- episodes[epi_n == 1]
setnames(episodes, "epistart", "date_first_subs_apc")

cohort_spine <- merge(cohort_spine,
                      episodes[, c("encrypted_hesid", "date_first_subs_apc")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(episodes); gc()

# * opa ---------------------------------------------------------------------

opa <- fread(paste0(master_dir, "/HES_OPA_Appt_combined.csv"),
             header = T,
             stringsAsFactors = F)

opa <- opa[encrypted_hesid %in% cohort_spine$encrypted_hesid, c("encrypted_hesid", "apptdate")]
gc()

opa <- merge(opa,
             cohort_spine[, c("encrypted_hesid", "dob")],
             by = "encrypted_hesid",
             all.x = T)
# 
# table(is.na(opa$dob))
# table(is.na(opa$apptdate))

opa[, age_at_appt := as.numeric(difftime(apptdate, dob, units = "days") / 365.25)]
opa <- opa[age_at_appt > 0 & age_at_appt < 16]
gc()

cohort_spine[, has_opa_0_4 := encrypted_hesid %in% opa[age_at_appt < 5]$encrypted_hesid]
cohort_spine[, has_opa_0_15 := encrypted_hesid %in% opa[age_at_appt < 16]$encrypted_hesid]
cohort_spine[, has_opa_5_15 := encrypted_hesid %in% opa[age_at_appt >= 5 & age_at_appt < 16]$encrypted_hesid]

opa <- opa[order(encrypted_hesid, apptdate)]
opa[, app_n := seq_len(.N), by = .(encrypted_hesid)]
opa <- opa[app_n == 1]
setnames(opa, "apptdate", "date_first_opa")

cohort_spine <- merge(cohort_spine,
                      opa[, c("encrypted_hesid", "date_first_opa")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(opa); gc()

# * ae ----------------------------------------------------------------------

ae <- fread(paste0(master_dir, "/HES_AE_Area_combined.csv"),
             header = T,
             stringsAsFactors = F)

ae <- ae[encrypted_hesid %in% cohort_spine$encrypted_hesid, c("encrypted_hesid", "arrivaldate")]
gc()

ae <- merge(ae,
             cohort_spine[, c("encrypted_hesid", "dob")],
             by = "encrypted_hesid",
             all.x = T)

# table(is.na(ae$dob))
# table(is.na(ae$arrivaldate))

ae[, age_at_ae := as.numeric(difftime(arrivaldate, dob, units = "days") / 365.25)]
ae <- ae[age_at_ae > 0 & age_at_ae < 16]
gc()

cohort_spine[, has_ae_0_4 := encrypted_hesid %in% ae[age_at_ae < 5]$encrypted_hesid]
cohort_spine[, has_ae_0_15 := encrypted_hesid %in% ae[age_at_ae < 16]$encrypted_hesid]
cohort_spine[, has_ae_5_15 := encrypted_hesid %in% ae[age_at_ae >= 5 & age_at_ae < 16]$encrypted_hesid]

ae <- ae[order(encrypted_hesid, arrivaldate)]
ae[, arrival_n := seq_len(.N), by = .(encrypted_hesid)]
ae <- ae[arrival_n == 1]
setnames(ae, "arrivaldate", "date_first_ae")

cohort_spine <- merge(cohort_spine,
                      ae[, c("encrypted_hesid", "date_first_ae")],
                      by = "encrypted_hesid",
                      all.x = T)

rm(ae); gc()

# any HES
cohort_spine[, has_any_hes_0_4 := has_subs_apc_0_4 | has_opa_0_4 | has_ae_0_4]
cohort_spine[, has_any_hes_0_15 := has_subs_apc_0_15 | has_opa_0_15 | has_ae_0_15]
cohort_spine[, has_any_hes_5_15 := has_subs_apc_5_15 | has_opa_5_15 | has_ae_5_15]

# date first HES
cohort_spine[, date_first_hes := as.Date(NA)]
cohort_spine[, date_first_hes := min(date_first_subs_apc, date_first_opa, date_first_ae, na.rm = T), by = encrypted_hesid]

# flag for first type of HES
cohort_spine[, first_hes_apc := 0]
cohort_spine[!is.na(date_first_subs_apc) & date_first_subs_apc == date_first_hes, first_hes_apc := 1]

cohort_spine[, first_hes_opa := 0]
cohort_spine[!is.na(date_first_opa) & date_first_opa == date_first_hes, first_hes_opa := 1]

cohort_spine[, first_hes_ae := 0]
cohort_spine[!is.na(date_first_ae) & date_first_ae == date_first_hes, first_hes_ae := 1]

# prop.table(table(cohort_spine$academicyearofbirth, cohort_spine$first_hes_apc), 1)
# prop.table(table(cohort_spine$academicyearofbirth, cohort_spine$first_hes_opa), 1)
# prop.table(table(cohort_spine$academicyearofbirth, cohort_spine$first_hes_ae), 1)

# GET FIRST NPD ACTIVITY --------------------------------------------------

# * census ----------------------------------------------------------------

dbhandle <- odbcDriverConnect('conn_str_omitted')
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Spring|Summer|Autumn", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

tables <- tables[!(tables$TABLE_NAME %in% c("Spring_Census_2011_Disability", "Spring_Census_2012_Disability")), ]

generate_npd_source <- function() {
   npd_source <- data.table()
   
   for(table_name in unique(tables$TABLE_NAME)) {
      
      gc()
      print(paste0("Now doing table: ", table_name))
      
      temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "'"))
      temp_columns <- temp$COLUMN_NAME
      temp_columns_lower <- tolower(temp_columns)
      
      pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
      year_column <- grepl("^academicyear", temp_columns_lower) # ^ = starting with as there are some other cols with AcademicYear in the name
      census_date_column <- grepl("censusdate", temp_columns_lower)
      
      pupil_column <- temp_columns[pupil_column]
      year_column <- temp_columns[year_column]
      census_date_column <- temp_columns[census_date_column]
      
      if (identical(pupil_column, character(0)) == FALSE & identical(year_column, character(0)) == FALSE & identical(census_date_column, character(0)) == FALSE) {
         temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ", pupil_column , ", ", year_column, ", ", census_date_column, " FROM ", table_name)))
         temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
         
         if (ncol(temp) == 3) {
            colnames(temp) <- c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")
            temp <- distinct(temp)
            temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))
            npd_source <- rbind(npd_source, temp)
         }
      }
      
   }
   
   return(npd_source)
}

npd_source <- generate_npd_source()

# clean up some variables and dedup as already very big
npd_source[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]

npd_source <- npd_source[order(PupilMatchingRefAnonymous, CensusDate)]
npd_source[, dup := duplicated(PupilMatchingRefAnonymous)]
npd_source <- npd_source[dup == F]
npd_source[, dup := NULL]
gc()

# length(unique(npd_source$PupilMatchingRefAnonymous))
# nrow(npd_source)

rm(tables, generate_npd_source)

# * plasc -----------------------------------------------------------------

dbhandle <- odbcDriverConnect('conn_str_omitted')
tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("PLASC", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

generate_npd_source_from_plasc <- function() {
   
   for(table_name in unique(tables$TABLE_NAME)) {
      
      npd_source_plasc <- data.table()
      
      gc()
      print(paste0("Now doing table: ", table_name))
      
      temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "'"))
      temp_columns <- temp$COLUMN_NAME
      temp_columns_lower <- tolower(temp_columns)
      
      pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
      year_column <- grepl("^academicyear", temp_columns_lower) # ^ = starting with as there are some other cols with AcademicYear in the name
      census_date_column <- grepl("censusdate", temp_columns_lower)
      
      pupil_column <- temp_columns[pupil_column]
      year_column <- temp_columns[year_column]
      census_date_column <- temp_columns[census_date_column]
      
      if (identical(pupil_column, character(0)) == FALSE & identical(year_column, character(0)) == FALSE & identical(census_date_column, character(0)) == FALSE) {
         temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ", pupil_column , ", ", year_column, ", ", census_date_column, " FROM ", table_name)))
         temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
         
         if (ncol(temp) == 3) {
            colnames(temp) <- c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")
            temp <- distinct(temp)
            temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))
            npd_source_plasc <- rbind(npd_source_plasc, temp)
         }
      }
      
   }
   
   return(npd_source_plasc)
}

npd_source_plasc <- generate_npd_source_from_plasc()

npd_source_plasc[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]

npd_source_plasc <- npd_source_plasc[order(PupilMatchingRefAnonymous, AcademicYear)]
npd_source_plasc[, dup := duplicated(PupilMatchingRefAnonymous)]
npd_source_plasc <- npd_source_plasc[dup == F]
npd_source_plasc[, dup := NULL]
gc()

npd_source_plasc <- npd_source_plasc[, c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")]
npd_source <- rbind(npd_source, npd_source_plasc)

rm(tables, generate_npd_source_from_plasc, npd_source_plasc)

npd_source[, source := "Census"]

# * ap --------------------------------------------------------------------

dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT AP_PupilMatchingRefAnonymous, AP_ACADYR FROM AP_Census_2008_to_2020")))

temp[, AcademicYear := as.integer(substr(AP_ACADYR, 6, 9))]
temp[, CensusDate := as.Date(paste0(AcademicYear, "-01-15"))]

temp <- temp[, c("AP_PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")]
setnames(temp, "AP_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")

temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]

temp <- distinct(temp)
temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))

temp[, source := "AP"]

npd_source <- rbind(npd_source, temp)
rm(temp)

# * pru -------------------------------------------------------------------

dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT PRU_PupilMatchingRefAnonymous, PRU_AcademicYear, PRU_CensusDate FROM PRU_Census_2010_to_2013")))
temp[, AcademicYear := as.integer(substr(PRU_AcademicYear, 6, 9))]

temp <- temp[, c("PRU_PupilMatchingRefAnonymous", "AcademicYear", "PRU_CensusDate")]
setnames(temp, c("PRU_PupilMatchingRefAnonymous", "PRU_CensusDate"), c("PupilMatchingRefAnonymous", "CensusDate"))

temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]

temp <- distinct(temp)
temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))

temp[, source := "PRU"]

npd_source <- rbind(npd_source, temp)
rm(temp)

# * eyc -------------------------------------------------------------------

dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT EYC_PupilMatchingRefAnonymous, EYC_ACADYR FROM EYC_2008_to_2020")))
temp[, AcademicYear := as.integer(substr(EYC_ACADYR, 6, 9))]

temp <- temp[, c("EYC_PupilMatchingRefAnonymous", "AcademicYear")]
setnames(temp, "EYC_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")

temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]

temp[, CensusDate := as.Date(paste0(AcademicYear, "-01-15"))]

temp <- distinct(temp)
temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))

temp[, source := "EYC"]

npd_source <- rbind(npd_source, temp)
rm(temp)

# * eyfsp -------------------------------------------------------------------

# last time of the year a child turns 5
dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT FSP_PupilMatchingRefAnonymous, FSP_ACADYR FROM EYFSP_2008_to_2019")))

temp <- temp[!is.na(FSP_PupilMatchingRefAnonymous)]
temp <- temp[FSP_PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
temp <- distinct(temp)

temp[, AcademicYear := as.integer(substr(FSP_ACADYR, 6, 9))]
temp[, CensusDate := as.Date(paste0(AcademicYear, "-05-31"))]

setnames(temp, "FSP_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")
temp <- temp[, c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")]

temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))

temp[, source := "EYFSP"]

npd_source <- rbind(npd_source, temp)
rm(temp)
gc()

npd_source <- npd_source[order(PupilMatchingRefAnonymous, CensusDate)]
npd_source[, dup := duplicated(PupilMatchingRefAnonymous)]
npd_source <- npd_source[dup == F]
npd_source[, dup := NULL]
gc()

# * ks2 -------------------------------------------------------------------

#  May
dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT KS2_PupilMatchingRefAnonymous, KS2_ACADYR FROM KS2Pupil_2002_to_2019")))

temp <- temp[!is.na(KS2_PupilMatchingRefAnonymous)]
temp <- temp[KS2_PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
temp <- distinct(temp)

temp[, AcademicYear := as.integer(substr(KS2_ACADYR, 6, 9))]
temp[, CensusDate := as.Date(paste0(AcademicYear, "-05-31"))]

setnames(temp, "KS2_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")
temp <- temp[, c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")]

temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))

temp[, source := "KS2"]

npd_source <- rbind(npd_source, temp)
rm(temp)
gc()

npd_source <- npd_source[order(PupilMatchingRefAnonymous, CensusDate)]
npd_source[, dup := duplicated(PupilMatchingRefAnonymous)]
npd_source <- npd_source[dup == F]
npd_source[, dup := NULL]
gc()

# * ks4 -------------------------------------------------------------------

# May/Jun
dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT KS4_PupilMatchingRefAnonymous, KS4_ACADYR FROM KS4Pupil_2011_to_2014")))

temp <- temp[!is.na(KS4_PupilMatchingRefAnonymous)]
temp <- temp[KS4_PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
temp <- distinct(temp)

temp[, AcademicYear := as.integer(substr(KS4_ACADYR, 6, 9))]
temp[, CensusDate := as.Date(paste0(AcademicYear, "-05-31"))]

setnames(temp, "KS4_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")
temp <- temp[, c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")]

temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))

temp[, source := "KS4"]

npd_source <- rbind(npd_source, temp)
rm(temp)
gc()

dbhandle <- odbcDriverConnect('conn_str_omitted')
temp <- data.table(sqlQuery(dbhandle, paste0("SELECT KS4_PupilMatchingRefAnonymous, KS4_ACADYR FROM KS4Pupil_2015_to_2019")))

temp <- temp[!is.na(KS4_PupilMatchingRefAnonymous)]
temp <- temp[KS4_PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
temp <- distinct(temp)

temp[, AcademicYear := as.integer(substr(KS4_ACADYR, 6, 9))]
temp[, CensusDate := as.Date(paste0(AcademicYear, "-05-31"))]

setnames(temp, "KS4_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")
temp <- temp[, c("PupilMatchingRefAnonymous", "AcademicYear", "CensusDate")]

temp <- subset(temp, !is.na(AcademicYear) & !is.na(CensusDate))
npd_source <- rbind(npd_source, temp)
rm(temp)
gc()

# * merge into spine first NPD contact ------------------------------------

length(unique(npd_source$PupilMatchingRefAnonymous))
nrow(npd_source)

setnames(npd_source, c("CensusDate", "source"), c("date_first_npd", "source_first_npd"))

cohort_spine <- merge(cohort_spine,
                      npd_source[, c("PupilMatchingRefAnonymous", "date_first_npd", "source_first_npd")],
                      by = "PupilMatchingRefAnonymous",
                      all.x = T)

rm(npd_source); gc()

table(is.na(cohort_spine$PupilMatchingRefAnonymous),
      is.na(cohort_spine$date_first_npd))

table(cohort_spine$academicyearofbirth,
      cohort_spine$source_first_npd, useNA = "always")

# Time to vars ------------------------------------------------------------

cohort_spine[, time_to_first_npd_mo := trunc(as.numeric(difftime(date_first_npd, dob, units = "days")) / 30.417)]
summary(cohort_spine$time_to_first_npd_mo)
cohort_spine[time_to_first_npd_mo < 0]$time_to_first_npd_mo
cohort_spine[time_to_first_npd_mo < 0, time_to_first_npd_mo := NA]

cohort_spine[, date_first_npdhes := as.Date(NA)]
cohort_spine[, date_first_npdhes := min(date_first_hes, date_first_npd, na.rm = T), by = encrypted_hesid]
cohort_spine[, time_to_first_hesnpd_mo := trunc(as.numeric(difftime(date_first_npdhes, dob, units = "days")) / 30.417)]
cohort_spine[time_to_first_hesnpd_mo == Inf, time_to_first_hesnpd_mo := NA]

# table(cohort_spine$time_to_first_hesnpd_mo < 0)
cohort_spine[time_to_first_hesnpd_mo < 0, time_to_first_hesnpd_mo := NA]

# table(is.na(cohort_spine$time_to_first_hesnpd_mo))
# table(cohort_spine$academicyearofbirth,
#       is.na(cohort_spine$time_to_first_hesnpd_mo))

# DROP WHERE FIRST RECORD BEFORE BIRTH --------------------------------

# nrow(cohort_spine[!is.na(date_first_hes) & date_first_hes < dob])
# nrow(cohort_spine[!is.na(date_first_npd) & date_first_npd < dob])
cohort_spine <- cohort_spine[is.na(date_first_npd) | (!is.na(date_first_hes) & date_first_npd >= dob)]
# nrow(cohort_spine[!is.na(date_first_npd) & date_first_npdhes < dob])

# CREATE ELIGIBILTY FLAGS ----------------------------------------------

nrow(cohort_spine[!is.na(stillb)])
cohort_spine <- cohort_spine[is.na(stillb)]

table(cohort_spine$not_england_or_unknown)
cohort_spine <- cohort_spine[!(not_england_or_unknown)]

cohort_spine[, el_scheme_1 := T]
cohort_spine[, el_scheme_2 := has_any_hes_0_4 == T]
cohort_spine[, el_scheme_3 := has_any_hes_0_15 == T]
cohort_spine[, el_scheme_4 := !is.na(PupilMatchingRefAnonymous)]
cohort_spine[, el_scheme_5 := has_any_hes_0_4 == T & !is.na(PupilMatchingRefAnonymous)]
cohort_spine[, el_scheme_6 := has_any_hes_0_4 == T & has_any_hes_5_15 == T]
cohort_spine[, el_scheme_7 := has_any_hes_0_4 == T & has_any_hes_5_15 == T & !is.na(PupilMatchingRefAnonymous)]
cohort_spine[, el_scheme_8 := has_any_hes_0_15 == T | !is.na(PupilMatchingRefAnonymous)]

# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_1, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_2, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_3, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_4, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_5, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_6, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_7, useNA = "always")
# table(cohort_spine$academicyearofbirth, cohort_spine$el_scheme_8, useNA = "always")

cohort_spine[, stillb := NULL]
cohort_spine[, not_england_or_unknown := NULL]
cohort_spine[, has_subs_apc_0_4 := NULL]
cohort_spine[, has_subs_apc_0_15 := NULL]
cohort_spine[, has_subs_apc_5_15 := NULL]
cohort_spine[, has_opa_0_4 := NULL]
cohort_spine[, has_opa_0_15 := NULL]
cohort_spine[, has_opa_5_15 := NULL]
cohort_spine[, has_ae_0_4 := NULL]
cohort_spine[, has_ae_0_15 := NULL]
cohort_spine[, has_ae_5_15 := NULL]
cohort_spine[, has_any_hes_0_4 := NULL]
cohort_spine[, has_any_hes_0_15 := NULL]
cohort_spine[, has_any_hes_5_15 := NULL]

# Deduplicate -------------------------------------------------------------

length(cohort_spine$encrypted_hesid)
nrow(cohort_spine)

# Save --------------------------------------------------------------------

save(cohort_spine, file = "chc_cumul/processed/cohort_spine.rda")
rm(list = ls()); gc()
