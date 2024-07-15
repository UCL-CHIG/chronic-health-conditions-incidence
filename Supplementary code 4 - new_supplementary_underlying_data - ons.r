# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Outputs new supplementary data following peer review (open cohort method)
# *******************************************

setwd("[[path omitted]")
library(data.table)

# Figure 3
dt <- fread("STATS19483/ons-any-chc-data.csv")

dt <- dt[, c("birth_cohort", "age", "cum_inc_p")]
dt[, cum_inc_p := paste0(cum_inc_p, "%")]

dt_wide <- dcast(dt, birth_cohort ~ age, value.var = "cum_inc_p")
write.csv(dt_wide, file = "new_supp_underlying_data/ons-any-chc-data-summary.csv")


# Supp figure s6

# Figure 2
dt <- fread("STATS19483/ons-sub-types-data.csv")

dt <- dt[, c("birth_cohort", "chc_type", "age", "cum_inc_p")]
dt[, cum_inc_p := paste0(cum_inc_p, "%")]
type_codes <- unique(dt$chc_type)

for (type_code in type_codes) {
  dt_wide <- dcast(dt[chc_type == type_code], birth_cohort ~ age + chc_type, value.var = "cum_inc_p")
  write.csv(dt_wide, file = paste0("new_supp_underlying_data/ons-sub-type-", type_code, "-data-summary.csv"))
}

rm(list = ls())
