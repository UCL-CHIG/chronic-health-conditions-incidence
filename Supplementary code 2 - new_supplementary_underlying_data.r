# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Outputs new supplementary data following peer review
# *******************************************

setwd("[[path omitted]")
library(data.table)

# Figure 1
dt <- fread("13. km-plots-el-scheme-8-all-birth-cohorts-underlying-data.csv")

dt[, academicyearofbirth := gsub("academicyearofbirth=", "", cohort)]
dt[, cohort := NULL]
dt[, cumul_inc_pc := round(cumul_inc * 100, 1)]

dt <- dt[, c("academicyearofbirth", "time", "cumul_inc_pc")]

dt <- dt[!is.na(cumul_inc_pc)]

dt[, cumul_inc_pc := paste0(cumul_inc_pc, "%")]

dt <- dt[time %in% c((0:16)*12)]
dt[, age := time / 12]

dt[, time := NULL]

dt_wide <- dcast(dt, academicyearofbirth ~ age, value.var = "cumul_inc_pc")
write.csv(dt_wide, file = "new_supp_underlying_data/13. km-plots-el-scheme-8-all-birth-cohorts-underlying-data-summary.csv")




# Figure 2
dt <- fread("16. km-plots-chc-subtypes-el-scheme-8-all-cohorts-underlying-data.csv")

dt[, academicyearofbirth := gsub("academicyearofbirth_comb=", "", cohort)]
dt[, cohort := NULL]
dt[, cumul_inc_pc := round(cumul_inc * 100, 1)]

dt <- dt[, c("academicyearofbirth", "type", "time", "cumul_inc_pc")]

dt <- dt[!is.na(cumul_inc_pc)]

dt[, cumul_inc_pc := paste0(cumul_inc_pc, "%")]

dt <- dt[time %in% c((0:15)*12, 191)]
dt[time == 191, time := 192]
dt[, age := time / 12]
dt[, time := NULL]

type_codes <- unique(dt$type)

for (type_code in type_codes) {
  dt_wide <- dcast(dt[type == type_code], academicyearofbirth ~ age + type, value.var = "cumul_inc_pc")
  write.csv(dt_wide, file = paste0("new_supp_underlying_data/el-scheme-8-all-birth-cohorts-", type_code, "-underlying-data-summary.csv"))
}

