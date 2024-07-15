# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Outputs new supplementary data following peer review
# *******************************************

setwd("[[path omitted]")
library(data.table)

# sensitivity - any CHC
files <- list.files("STATS19497", pattern = "*.csv")


for (i in 1:length(files)) {
  
  dt <- fread(paste0("STATS19497/", files[i]))
  
  dt[, cumul_inc_pc := round(cumul_inc * 100, 1)]
  dt[, cumul_inc_pc := paste0(cumul_inc_pc, "%")]
  dt <- dt[, c("sensitivity_analysis", "time", "cumul_inc_pc")]
  
  dt <- dt[time %in% c((0:16)*12)]
  dt[, age := time / 12]
  dt[, time := NULL]
  
  coh <- as.integer(gsub("km-plots-all-el-schemes-b|-underlying-data.csv", "", files[i]))
  
  dt <- dt[sensitivity_analysis != 8]
  
  dt_wide <- dcast(dt, sensitivity_analysis ~ age, value.var = "cumul_inc_pc")
  write.csv(dt_wide,
            file = paste0("new_supp_underlying_data/suppt7/b", coh, "-sensitivity-any-CHC-data.csv"),
            row.names = F)
  
}


# sensitivity - sub-types
files <- list.files("STATS19497/sub-types/")

for (i in 1:length(files)) {
  dt <- fread(paste0("STATS19497/sub-types/", files[i]))
  
  dt[, cumul_inc_pc := round(cumul_inc * 100, 1)]
  dt[, cumul_inc_pc := paste0(cumul_inc_pc, "%")]
  dt <- dt[, c("el_scheme", "type", "time", "cumul_inc_pc")]
  
  dt <- dt[time %in% c((0:15)*12, 191)]
  dt[time == 191, time := 192]
  dt[, age := time / 12]
  dt[, time := NULL]
  
  dt <- dt[el_scheme != 8]
  
  coh <- gsub("km-plots-chc-subtypes-all-el-schemes-|-underlying-data.csv", "", files[i])
  type_codes <- unique(dt$type)
  
  for (type_code in type_codes) {
    dt_wide <- dcast(dt[type == type_code], el_scheme ~ age, value.var = "cumul_inc_pc")
    write.csv(dt_wide,
              file = paste0("new_supp_underlying_data/suppt7/subtypes/", type_code, "-", coh, "-sensitivity-data.csv"),
              row.names = F)
  }
}



