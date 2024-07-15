# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Gets ONS denominators
# *******************************************

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)

master_dir <- "[path omitted]"

pop <- fread("1_CHC_CUMUL/pop_mye_ons.csv", header = T)
pop <- pop[country == "E" & age < 16]

pop_cols <- paste0("population_", 2003:2020)
keep <- c("age", pop_cols)
pop <- pop[, keep, with = F]

for (old_col in pop_cols) {
  pop[, (old_col) := sum(get(old_col)), by = .(age)]
}

pop <- pop[!duplicated(pop)]
rm(keep, old_col, pop_cols)

pop <- melt(pop, id.vars = "age")
pop[, variable := sub("population_", "", variable)]
setnames(pop, c("variable", "value"), c("mye_year", "mye_n"))

pop[, mye_year := as.integer(mye_year)]
pop[, age := as.integer(age)]

pop <- pop[(mye_year == 2003 & age == 0) |
             (mye_year == 2004 & age %in% 0:1) |
             (mye_year == 2005 & age %in% 0:2) |
             (mye_year == 2006 & age %in% 0:3) |
             (mye_year == 2007 & age %in% 0:4) |
             (mye_year == 2008 & age %in% 0:5) |
             (mye_year == 2009 & age %in% 0:6) |
             (mye_year == 2010 & age %in% 0:7) |
             (mye_year == 2011 & age %in% 0:8) |
             (mye_year == 2012 & age %in% 0:9) |
             (mye_year == 2013 & age %in% 0:10) |
             (mye_year == 2014 & age %in% 0:11) |
             (mye_year == 2015 & age %in% 0:12) |
             (mye_year == 2016 & age %in% 0:13) |
             (mye_year == 2017 & age %in% 0:14) |
             (mye_year == 2018 & age %in% 0:15) |
             (mye_year == 2019 & age %in% 0:15) |
             (mye_year == 2020 & age %in% 0:15)]

pop <- pop[, c("mye_year", "age", "mye_n")]

pop[, birth_cohort := mye_year - age]
pop <- pop[order(birth_cohort, mye_year, age)]

fwrite(pop, "1_CHC_CUMUL/processed/ons_mye.csv", row.names = F)
rm(list = ls())
