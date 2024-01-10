
# *******************************************
# Matthew Jay, matthew.jay@ucl.ac.uk
# *******************************************

# LOAD --------------------------------------------------------------------

setwd("[]:/Working/Matt/")
assign(".lib.loc", c(.libPaths(), "[]:/Working/Matt/r"), envir = environment(.libPaths))
library(data.table)
library(survival)
library(ggplot2)
library(survminer)
library(gridExtra)

library(tidycmprsk)
library(ggsurvfit)

master_dir <- "[]:/Working/Master data TEST"

load("chc_cumul/processed/chc_diagnoses.rda")
load("chc_cumul/processed/cohort_spine_censor.rda")

chc_diagnoses <- chc_diagnoses[, c("encrypted_hesid", "epistart")]

# GET FIRST CHC DIAGNOSIS PER CHILD ---------------------------------------

chc_diagnoses <- chc_diagnoses[order(encrypted_hesid, epistart)]

chc_diagnoses[, record_idx := seq_len(.N), by = .(encrypted_hesid)]
chc_diagnoses <- chc_diagnoses[record_idx == 1]
# length(unique(chc_diagnoses$encrypted_hesid)); nrow(chc_diagnoses)
chc_diagnoses[, record_idx := NULL]

setnames(chc_diagnoses, "epistart", "date_of_first_chc")

cohort_spine <- merge(cohort_spine,
                      chc_diagnoses,
                      by = "encrypted_hesid",
                      all.x = T)

# table(cohort_spine$date_of_first_chc < cohort_spine$dob)
cohort_spine[date_of_first_chc < dob, date_of_first_chc := dob]

# KM SURVIVAL -------------------------------------------------------------

# * create survival dataset -----------------------------------------------

surv_dt <- cohort_spine[, c("encrypted_hesid",
                            "dob",
                            "bday_16",
                            "date_first_non_eng_hes",
                            "dod_approx",
                            "academicyearofbirth",
                            "female",
                            "ethnos_clean",
                            "imd_quintile_birth",
                            "resgor_clean",
                            "lbw",
                            "bwt_cat",
                            "prem",
                            "ga_cat",
                            "teen_mother",
                            "el_scheme_1",
                            "el_scheme_2",
                            "el_scheme_3",
                            "el_scheme_4",
                            "el_scheme_5",
                            "el_scheme_6",
                            "el_scheme_7",
                            "el_scheme_8",
                            "date_of_first_chc")]

# * censoring indicator -----------------------------------------------------

surv_dt[, censor_date := as.Date(NA)]
surv_dt[, censor_date := min(bday_16, dod_approx, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]
# surv_dt[!is.na(dod_approx) & is.na(date_first_non_eng_hes), censor_date := min(dod_approx, bday_16), by = .(encrypted_hesid)]
# surv_dt[is.na(dod_approx) & !is.na(date_first_non_eng_hes), censor_date := min(date_first_non_eng_hes, bday_16), by = .(encrypted_hesid)]
# surv_dt[!is.na(dod_approx) & !is.na(date_first_non_eng_hes), censor_date := min(dod_approx, date_first_non_eng_hes, bday_16), by = .(encrypted_hesid)]
# table(surv_dt$censor_date < surv_dt$dob)
summary(surv_dt$censor_date)

# * calculate status date (first chc unless censored) ---------------------

surv_dt[, status_date := as.Date(NA)]
surv_dt[is.na(date_of_first_chc), status_date := censor_date]
surv_dt[!is.na(date_of_first_chc), status_date := min(censor_date, date_of_first_chc), by = .(encrypted_hesid)]

# table(is.na(surv_dt$status_date)) # should all be false
# table(surv_dt$status_date >= surv_dt$dob) # should all be true

surv_dt[, status_time := as.integer(difftime(status_date, dob, units = "days"))]
# summary(surv_dt$status_time)

surv_dt[, status_time_mo := as.integer(trunc(difftime(status_date, dob, units = "days") / 30.417))]
# summary(surv_dt$status_time_mo)

surv_dt[, status := 1] # not censored
surv_dt[is.na(date_of_first_chc), status := 0] # censored
surv_dt[!is.na(date_of_first_chc) & censor_date < date_of_first_chc, status := 0] # censored

# ALL EL SCHEMES B2004 ----------------------------------------------------

# plot
surv_list <- list(
  el_scheme_1 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_1 == T, ]),
  el_scheme_2 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_2 == T, ]),
  el_scheme_3 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_3 == T, ]),
  el_scheme_4 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_4 == T, ]),
  el_scheme_5 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_5 == T, ]),
  el_scheme_6 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_6 == T, ]),
  el_scheme_7 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_7 == T, ]),
  el_scheme_8 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$academicyearofbirth %in% 2004 & surv_dt$el_scheme_8 == T, ])
)

p1 <- ggsurvplot(surv_list,
                 combine = T,
                 fun = "event",
                 censor = F,
                 conf.int = T,
                 xlab = "Time from birth (years)",
                 ylab = "Cumulative incidence (%)",
                 legend.labs = c(paste0("Sensitivity ", 1:7), "Main analysis"),
                 legend = "bottom",
                 #title = "Births 2003/4",
                 palette = "Set1")

p1$plot <- p1$plot +
  scale_x_continuous(breaks = seq(0, 192, by = 12),
                     labels = 0:16) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3),
                     labels = c(0, 10, 20, 30)) +
  guides(fill = guide_legend(title = "Denominator definition"),
         colour = guide_legend(title = "Denominator definition"))

tiff("chc_cumul/outputs/surv/km-plots-all-el-schemes-b2004.tiff",
     height = 12, width = 8, units = "in", res = 300)
p1
dev.off()

# tables
surv_output_data <- data.table(
  sensitivity_analysis = 1,
  time = surv_list[[1]]$time,
  n.risk = surv_list[[1]]$n.risk,
  n.event = surv_list[[1]]$n.event,
  surv = surv_list[[1]]$surv,
  cumul_inc = 1 - surv_list[[1]]$surv
)

for (i in 2:8) {
  tmp <- data.table(
    sensitivity_analysis = i,
    time = surv_list[[i]]$time,
    n.risk = surv_list[[i]]$n.risk,
    n.event = surv_list[[i]]$n.event,
    surv = surv_list[[i]]$surv,
    cumul_inc = 1 - surv_list[[i]]$surv
  )
  surv_output_data <- rbind(surv_output_data, tmp)
}

rm(i, tmp)
table(surv_output_data$n.event < 10)
write.csv(surv_output_data, file = "chc_cumul/outputs/surv/km-plots-all-el-schemes-b2004-underlying-data.csv", row.names = F)

rm(surv_list, surv_output_data)

# EL SCHEME 8 ALL COHORTS ------------------------------------------------------

# plots
surv_list <- survfit(Surv(status_time_mo, status) ~ academicyearofbirth, data = surv_dt[surv_dt$el_scheme_8 == T, ])

# suppress where cohorts become unobservable
cohorts <- rep(names(surv_list$strata), surv_list$strata)
cohorts <- as.integer(gsub("[a-z].*=", "", cohorts))

surv_list$surv[surv_list$time > (192 - (12 * 1)) & cohorts == 2005] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 2)) & cohorts == 2006] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 3)) & cohorts == 2007] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 4)) & cohorts == 2008] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 5)) & cohorts == 2009] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 6)) & cohorts == 2010] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 7)) & cohorts == 2011] <- NA
surv_list$surv[surv_list$time > (192 - (12 * 8)) & cohorts == 2012] <- NA

p1 <- ggsurvplot(surv_list,
                 #combine = T,
                 fun = "event",
                 censor = F,
                 conf.int = F,
                 xlab = "Time from birth (years)",
                 ylab = "Cumulative incidence (%)",
                 legend.labs = 2003:2012,
                 legend = "right"#,
                 #title = "By birth cohort (scheme 8)"
                 )

p1$plot <- p1$plot +
  scale_x_continuous(breaks = seq(0, 192, by = 12),
                     labels = 0:16) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3),
                     labels = c(0, 10, 20, 30)) +
  guides(fill = guide_legend(title = "Birth cohort\n(academic year ending)"),
         colour = guide_legend(title = "Birth cohort\n(academic year ending)"))

tiff("chc_cumul/outputs/surv/km-plots-el-scheme-8-all-birth-cohorts.tiff",
     height = 12, width = 8, units = "in", res = 300)
p1
dev.off()

# tables
surv_output_data <- data.table(
  cohort = rep(names(surv_list$strata), surv_list$strata),
  time = surv_list$time,
  n.risk = surv_list$n.risk,
  n.event = surv_list$n.event,
  surv = surv_list$surv,
  cumul_inc = 1 - surv_list$surv
)

# set n to NA where surv is NA
surv_output_data[is.na(surv), n.risk := NA]
surv_output_data[is.na(surv), cumul_inc := NA]
surv_output_data[is.na(surv), n.event := NA]

write.csv(surv_output_data, file = "chc_cumul/outputs/surv/km-plots-el-scheme-8-all-birth-cohorts-underlying-data.csv", row.names = F)

rm(surv_dt); gc()

# COMPETING RISKS ---------------------------------------------------------

# * create survival dataset -----------------------------------------------

compete_dt <- cohort_spine[el_scheme_8 == 1, c("encrypted_hesid",
                                               "dob",
                                               "bday_16",
                                               "date_first_non_eng_hes",
                                               "academicyearofbirth",
                                               "dod_approx",
                                               "date_of_first_chc")]

# * censoring indicator -----------------------------------------------------
# censor date (16th birthday or date_first_non_eng_hes)
compete_dt[, censor_date := as.Date(NA)]
compete_dt[, censor_date := min(bday_16, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]
table(compete_dt$censor_date < compete_dt$dob)
summary(compete_dt$censor_date)

# * status indicator ------------------------------------------------------

compete_dt[, status := as.integer(NA)]

# no CHC and no death (censored)
compete_dt[is.na(date_of_first_chc) & is.na(dod_approx), status := 1]

# CHC and no death (or death > CHC)
compete_dt[!is.na(date_of_first_chc) & ( is.na(dod_approx) |
                                          (!is.na(dod_approx) & date_of_first_chc <= dod_approx)
                                        ),
           status := 2]

# death and no CHC or death < CHC
compete_dt[!is.na(dod_approx) & ( is.na(date_of_first_chc) |
                                          (!is.na(date_of_first_chc) & dod_approx < date_of_first_chc)
                                  ), status := 3] # censored

compete_dt[, status := factor(status)]
table(compete_dt$status, useNA = "always")

# * calculate status date -------------------------------------------------

compete_dt[, status_date := as.Date(NA)]

# censored - censor date
compete_dt[status == 1, status_date := censor_date]

# CHC and no death (or death > CHC)
compete_dt[status == 2, status_date := date_of_first_chc]

# death and no CHC or death < CHC
compete_dt[status == 3, status_date := dod_approx]

table(is.na(compete_dt$status_date)) # should all be false
table(compete_dt$status_date >= compete_dt$dob) # should all be true

compete_dt[, status_time := as.integer(difftime(status_date, dob, units = "days"))]
summary(compete_dt$status_time)

compete_dt[, status_time_mo := as.integer(trunc(difftime(status_date, dob, units = "days") / 30.417))]
summary(compete_dt$status_time_mo)

# * create surv object and plot ---------------------------------------------

el_scheme_8_cmp <- tidycmprsk::cuminc(Surv(status_time_mo, status) ~ 1, data = compete_dt[compete_dt$academicyearofbirth %in% 2004, ])
rm(chc_diagnoses, cohort_spine); gc()

el_scheme_8_cmp$cmprsk$`1 1`$est[length(el_scheme_8_cmp$cmprsk$`1 1`$est)]

output <- data.table(
  el_scheme = 8,
  outcome = el_scheme_8_cmp$tidy$outcome,
  time = el_scheme_8_cmp$tidy$time,
  n_risk = el_scheme_8_cmp$tidy$n.risk,
  n_event = el_scheme_8_cmp$tidy$n.event,
  cum_inc = el_scheme_8_cmp$tidy$estimate
)

# output <- output[outcome == 2 & time == 192] # use only CHC output and only at final time point

write.csv(output, file = "chc_cumul/outputs/comp/comp_risk_dt.csv")

# CLEAR -------------------------------------------------------------------

rm(list = ls()); gc()
