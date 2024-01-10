# *******************************************
# Matthew Jay, matthew.jay@ucl.ac.uk
# *******************************************

# LOAD --------------------------------------------------------------------

setwd("[]:/Working/Matt/")
assign(".lib.loc", c(.libPaths(), "[]:/Working/Matt/r"), envir = environment(.libPaths))
library(data.table)
library(survival)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(survminer)
library(dplyr)

master_dir <- "[]:/Working/Master data TEST"

load("chc_cumul/processed/cohort_spine_censor.rda")

birth_cohorts <- 2003:2012

# KAPLAN-MEIER ------------------------------------------------------------

# SET TO NA NULL VALUES
# where occurs after dod, bday_16 or first non eng
cohort_spine[, censor_date := as.Date(NA)]
cohort_spine[, censor_date := min(dod_approx, bday_16, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]
cohort_spine[date_first_hes > censor_date, date_first_hes := NA]

# TIME TO
cohort_spine[, time_to_first_hes_mo := trunc(as.numeric(difftime(date_first_hes, dob, units = "days")) / 30.417)]
summary(cohort_spine$time_to_first_hes_mo)
# cohort_spine[time_to_first_hes_mo < 0]$time_to_first_hes_mo
# cohort_spine[time_to_first_hes_mo < 0, time_to_first_hes_mo := NA]

# * create survival dataset -----------------------------------------------

surv_dt <- cohort_spine[, c("encrypted_hesid",
                            "dob",
                            "bday_16",
                            "date_first_non_eng_hes",
                            "dod_approx",
                            "academicyearofbirth",
                            "date_first_npdhes",
                            
                            "date_first_hes",
                            "date_first_subs_apc",
                            "date_first_opa",
                            "date_first_ae",
                            
                            "date_first_npd",
                            
                            "time_to_first_hes_mo",
                            "time_to_first_npd_mo",
                            "time_to_first_hesnpd_mo")]

# * censoring indicator -----------------------------------------------------

surv_dt[, censor_date := as.Date(NA)]
surv_dt[, censor_date := min(bday_16, dod_approx, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]
table(surv_dt$censor_date < surv_dt$dob)
summary(surv_dt$censor_date)

# * calculate status date (first hesnpd unless censored) ---------------------

# ** HES or NPD -----------------------------------------------------------

# hesnpd
surv_dt[, status_date_hesnpd := as.Date(NA)]
surv_dt[is.na(date_first_npdhes), status_date_hesnpd := censor_date]
surv_dt[!is.na(date_first_npdhes), status_date_hesnpd := min(censor_date, date_first_npdhes), by = .(encrypted_hesid)]

table(is.na(surv_dt$status_date_hesnpd)) # should all be false
table(surv_dt$status_date_hesnpd >= surv_dt$dob) # should all be true

surv_dt[, status_time_mo_hesnpd := as.integer(trunc(difftime(status_date_hesnpd, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_hesnpd)

surv_dt[, status_hesnpd := 1] # not censored
surv_dt[is.na(date_first_npdhes), status_hesnpd := 0] # censored
surv_dt[!is.na(date_first_npdhes) & censor_date < date_first_npdhes, status_hesnpd := 0] # censored

# ** HES ------------------------------------------------------------------

# hes
surv_dt[, status_date_hes := as.Date(NA)]
surv_dt[is.na(date_first_hes), status_date_hes := censor_date]
surv_dt[!is.na(date_first_hes), status_date_hes := min(censor_date, date_first_hes), by = .(encrypted_hesid)]

table(is.na(surv_dt$status_date_hes)) # should all be false
table(surv_dt$status_date_hes >= surv_dt$dob) # should all be true

surv_dt[, status_time_mo_hes := as.integer(trunc(difftime(status_date_hes, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_hes)

surv_dt[, status_hes := 1] # not censored
surv_dt[is.na(date_first_hes), status_hes := 0] # censored
surv_dt[!is.na(date_first_hes) & censor_date < date_first_hes, status_hes := 0] # censored

# apc
surv_dt[, status_date_hes_apc := as.Date(NA)]
surv_dt[is.na(date_first_subs_apc), status_date_hes_apc := censor_date]
surv_dt[!is.na(date_first_subs_apc), status_date_hes_apc := min(censor_date, date_first_subs_apc), by = .(encrypted_hesid)]

table(is.na(surv_dt$status_date_hes_apc)) # should all be false
table(surv_dt$status_date_hes_apc >= surv_dt$dob) # should all be true

surv_dt[, status_time_mo_hes_apc := as.integer(trunc(difftime(status_date_hes_apc, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_hes_apc)

surv_dt[, status_hes_apc := 1] # not censored
surv_dt[is.na(date_first_subs_apc), status_hes_apc := 0] # censored
surv_dt[!is.na(date_first_subs_apc) & censor_date < date_first_subs_apc, status_hes_apc := 0] # censored

# opa
surv_dt[, status_date_hes_opa := as.Date(NA)]
surv_dt[is.na(date_first_opa), status_date_hes_opa := censor_date]
surv_dt[!is.na(date_first_opa), status_date_hes_opa := min(censor_date, date_first_opa), by = .(encrypted_hesid)]

table(is.na(surv_dt$status_date_hes_opa)) # should all be false
table(surv_dt$status_date_hes_opa >= surv_dt$dob) # should all be true

surv_dt[, status_time_mo_hes_opa := as.integer(trunc(difftime(status_date_hes_opa, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_hes_opa)

surv_dt[, status_hes_opa := 1] # not censored
surv_dt[is.na(date_first_opa), status_hes_opa := 0] # censored
surv_dt[!is.na(date_first_opa) & censor_date < date_first_opa, status_hes_opa := 0] # censored

# ae
surv_dt[, status_date_hes_ae := as.Date(NA)]
surv_dt[is.na(date_first_ae), status_date_hes_ae := censor_date]
surv_dt[!is.na(date_first_ae), status_date_hes_ae := min(censor_date, date_first_ae), by = .(encrypted_hesid)]

table(is.na(surv_dt$status_date_hes_ae)) # should all be false
table(surv_dt$status_date_hes_ae >= surv_dt$dob) # should all be true

surv_dt[, status_time_mo_hes_ae := as.integer(trunc(difftime(status_date_hes_ae, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_hes_ae)

surv_dt[, status_hes_ae := 1] # not censored
surv_dt[is.na(date_first_ae), status_hes_ae := 0] # censored
surv_dt[!is.na(date_first_ae) & censor_date < date_first_ae, status_hes_ae := 0] # censored

# ** NPD ------------------------------------------------------------------

# npd
surv_dt[, status_date_npd := as.Date(NA)]
surv_dt[is.na(date_first_npd), status_date_npd := censor_date]
surv_dt[!is.na(date_first_npd), status_date_npd := min(censor_date, date_first_npd), by = .(encrypted_hesid)]

table(is.na(surv_dt$status_date_npd)) # should all be false
table(surv_dt$status_date_npd >= surv_dt$dob) # should all be true

surv_dt[, status_time_mo_npd := as.integer(trunc(difftime(status_date_npd, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_npd)

surv_dt[, status_npd := 1] # not censored
surv_dt[is.na(date_first_npd), status_npd := 0] # censored
surv_dt[!is.na(date_first_npd) & censor_date < date_first_npd, status_npd := 0] # censored

# * estimate and plot -----------------------------------------------------

cohort_spine[, follow_up_status := as.character(NA)]
cohort_spine[!is.na(time_to_first_hes_mo) & is.na(time_to_first_npd_mo), follow_up_status := "ever_hes_only"]
cohort_spine[is.na(time_to_first_hes_mo) & !is.na(time_to_first_npd_mo), follow_up_status := "ever_npd_only"]
cohort_spine[!is.na(time_to_first_hes_mo) & !is.na(time_to_first_npd_mo), follow_up_status := "ever_npdandhes"]
cohort_spine[is.na(time_to_first_hes_mo) & is.na(time_to_first_npd_mo), follow_up_status := "neither_npdnorhes"]

describe_outcome <- function(exposures, outcome = NULL, dat, n_round = 0, p_round = 1) {
  
  tmp_table <- round(table(dat[[exposures[1]]], useNA = "always"), n_round)
  tmp_table_no_na <- tmp_table[-length(tmp_table)]
  
  prop_tmp_table <- round(prop.table(tmp_table_no_na) * 100, p_round)
  prop_tmp_table <- c(prop_tmp_table, "-")
  
  output <- matrix(
    rep(NA, nrow(tmp_table) * 1),
    nrow = nrow(tmp_table),
    ncol = 1
  )
  
  rownames(output) <- paste0(exposures[1], "_", names(tmp_table))
  output[, 1] <- paste0(tmp_table, " (", prop_tmp_table, "%)")
  
  
  if (length(exposures) > 1) {
    for (j in 2:length(exposures)) {
      
      tmp_table <- round(table(dat[[exposures[j]]], useNA = "always"), n_round)
      tmp_table_no_na <- tmp_table[-length(tmp_table)]
      
      prop_tmp_table <- round(prop.table(tmp_table_no_na) * 100, p_round)
      prop_tmp_table <- c(prop_tmp_table, "-")
      
      tmp_matrix <- matrix(
        rep(NA, nrow(tmp_table) * 1),
        nrow = nrow(tmp_table),
        ncol = 1
      )
      
      rownames(tmp_matrix) <- paste0(exposures[j], "_", names(tmp_table))
      tmp_matrix[, 1] <- paste0(tmp_table, " (", prop_tmp_table, "%)")
      output <- rbind(output, tmp_matrix)
      
    }
  }
  
  return(output)
}

# ** main plot -----------------------------------------------------------

plot_list <- list()
surv_output_data_list <- list()

for (i in 1:length(birth_cohorts)) {
  
  print(birth_cohorts[i])
  
  surv_list <- list(
    hesnpd = survfit(Surv(status_time_mo_hesnpd, status_hesnpd) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]]),
    hes = survfit(Surv(status_time_mo_hes, status_hes) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]]),
    npd = survfit(Surv(status_time_mo_npd, status_npd) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]])
  )
  
  # censor small cell counts by setting to 0
  for (j in 1:3) {
    if (any(surv_list[[j]]$n.event < 10)) {
      rows_to_na <- which(surv_list[[j]]$n.event < 10)
      surv_list[[j]]$n.event[rows_to_na] <- 0
    }
    
    # and round surv to 3 dp
    surv_list[[j]]$surv <- round(surv_list[[j]]$surv, 3)
    surv_list[[j]]$lower <- round(surv_list[[j]]$lower, 3)
    surv_list[[j]]$upper <- round(surv_list[[j]]$upper, 3)
    
    # suppress where cohorts become unobservable
    if (birth_cohorts[i] == 2005) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
    }
    
    if (birth_cohorts[i] == 2006) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
    }
    
    if (birth_cohorts[i] == 2007) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
    }
    
    if (birth_cohorts[i] == 2008) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
    }
    
    if (birth_cohorts[i] == 2009) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
    }
    
    if (birth_cohorts[i] == 2010) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
    }
    
    if (birth_cohorts[i] == 2011) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
    }
    
    if (birth_cohorts[i] == 2012) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
    }
  
  }
  
  plot_list[[i]] <- ggsurvplot(surv_list,
                               combine = T,
                               fun = "event",
                               censor = F,
                               conf.int = T,
                               xlab = paste0("Cohort ", birth_cohorts[i]),
                               ylab = "",
                               legend.labs = c("Time to first HES or NPD",
                                               "Time to first HES",
                                               "Time to first NPD"),
                               legend = "right",
                               palette = "Set1",
                               ylim = c(0, 1),
                               surv.scale = "percent",
                               font.x = c(8),
                               font.y = c(8),
                               font.tickslab = c(7))
  
  plot_list[[i]]$plot <- plot_list[[i]]$plot +
    scale_x_continuous(breaks = seq(0, 192, by = 12),
                       labels = 0:16) +
    guides(fill = guide_legend(title = ""),
           colour = guide_legend(title = ""))
  
  curves <- c("npdhes", "hes", "npd")
  
  surv_output_data_list[[i]] <- data.table(
    academicyearofbirth = birth_cohorts[i],
    curve = curves[1],
    time = surv_list[[1]]$time,
    n.risk = surv_list[[1]]$n.risk,
    n.event = surv_list[[1]]$n.event,
    surv = surv_list[[1]]$surv,
    cumul_inc = 1 - surv_list[[1]]$surv
  )
  
  for (j in 2:3) {
    tmp <- data.table(
      academicyearofbirth = birth_cohorts[i],
      curve = curves[j],
      time = surv_list[[j]]$time,
      n.risk = surv_list[[j]]$n.risk,
      n.event = surv_list[[j]]$n.event,
      surv = surv_list[[j]]$surv,
      cumul_inc = 1 - surv_list[[j]]$surv
    )
    
    surv_output_data_list[[i]] <- rbind(surv_output_data_list[[i]], tmp)
  }
  
  # Venn
  write.csv(describe_outcome(exposure = "follow_up_status", outcome = NULL, dat = cohort_spine),
            file = paste0("chc_cumul/outputs/timetofirst/venn/first_contact_venn_b", birth_cohorts[i], ".csv"))
  
  
}

surv_output_data <- do.call("rbind", surv_output_data_list)
write.csv(surv_output_data, file = "chc_cumul/outputs/timetofirst/km-plots-first-hesnpd-all-underlying-data.csv")

plot_result <- patchworkGrob(
  
  plot_list[[1]]$plot + 
  plot_list[[2]]$plot + 
  plot_list[[3]]$plot + 
  plot_list[[4]]$plot + 
  plot_list[[5]]$plot + 
  plot_list[[6]]$plot + 
  plot_list[[7]]$plot + 
  plot_list[[8]]$plot + 
  plot_list[[9]]$plot + 
  plot_list[[10]]$plot +
  plot_layout(guides = "collect")
  
  )

tiff("chc_cumul/outputs/timetofirst/km-plots-first_hesnpd-all.tiff",
     height = 8, width = 12, units = "in", res = 300)
grid.arrange(plot_result,
             left = "Cumulative incidence (%)",
             bottom = "Age (years)")
dev.off()

rm(surv_list, tmp, i, surv_output_data,
   surv_output_data_list, j, rows_to_na,
   plot_list, curves, plot_result)

# ** HES plot -------------------------------------------------------------

plot_list <- list()
surv_output_data_list <- list()

for (i in 1:length(birth_cohorts)) {
  
  print(birth_cohorts[i])
  
  surv_list <- list(
    hes = survfit(Surv(status_time_mo_hes, status_hes) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]]),
    apc = survfit(Surv(status_time_mo_hes_apc, status_hes_apc) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]]),
    opa = survfit(Surv(status_time_mo_hes_opa, status_hes_opa) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]]),
    ae = survfit(Surv(status_time_mo_hes_ae, status_hes_ae) ~ 1, data = surv_dt[academicyearofbirth == birth_cohorts[i]])
  )
  
  # censor small cell counts by setting to 0
  for (j in 1:4) {
    if (any(surv_list[[j]]$n.event < 10)) {
      rows_to_na <- which(surv_list[[j]]$n.event < 10)
      surv_list[[j]]$n.event[rows_to_na] <- 0
    }
    
    # and round surv to 3 dp
    surv_list[[j]]$surv <- round(surv_list[[j]]$surv, 3)
    surv_list[[j]]$lower <- round(surv_list[[j]]$lower, 3)
    surv_list[[j]]$upper <- round(surv_list[[j]]$upper, 3)
    
    # suppress where cohorts become unobservable
    if (birth_cohorts[i] == 2005) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 1))] <- NA
    }
    
    if (birth_cohorts[i] == 2006) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 2))] <- NA
    }
    
    if (birth_cohorts[i] == 2007) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 3))] <- NA
    }
    
    if (birth_cohorts[i] == 2008) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 4))] <- NA
    }
    
    if (birth_cohorts[i] == 2009) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 5))] <- NA
    }
    
    if (birth_cohorts[i] == 2010) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 6))] <- NA
    }
    
    if (birth_cohorts[i] == 2011) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 7))] <- NA
    }
    
    if (birth_cohorts[i] == 2012) {
      surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$lower[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$upper[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$n.risk[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
      surv_list[[j]]$n.event[surv_list[[j]]$time > (192 - (12 * 8))] <- NA
    }
    
  }
  
  plot_list[[i]] <- ggsurvplot(surv_list,
                   combine = T,
                   fun = "event",
                   censor = F,
                   conf.int = T,
                   xlab = paste0("Age (years)\nCohort ", birth_cohorts[i]),
                   ylab = "CInc (%)",
                   legend.labs = c("Time to first HES (any)",
                                   "Time to first HES APC",
                                   "Time to first HES OPA",
                                   "Time to first HEs A&E"),
                   legend = "right",
                   palette = "Set1",
                   ylim = c(0, 1),
                   surv.scale = "percent",
                   font.x = c(8),
                   font.y = c(8),
                   font.tickslab = c(7))
  
  plot_list[[i]]$plot <- plot_list[[i]]$plot +
    scale_x_continuous(breaks = seq(0, 192, by = 12),
                       labels = 0:16) +
    guides(fill = guide_legend(title = ""),
           colour = guide_legend(title = ""))
  
  curves <- c("hes", "apc", "opa", "ae")
  
  surv_output_data_list[[i]] <- data.table(
    academicyearofbirth = birth_cohorts[i],
    curve = curves[1],
    time = surv_list[[1]]$time,
    n.risk = surv_list[[1]]$n.risk,
    n.event = surv_list[[1]]$n.event,
    surv = surv_list[[1]]$surv,
    cumul_inc = 1 - surv_list[[1]]$surv
  )
  
  for (j in 2:4) {
    tmp <- data.table(
      academicyearofbirth = birth_cohorts[i],
      curve = curves[j],
      time = surv_list[[j]]$time,
      n.risk = surv_list[[j]]$n.risk,
      n.event = surv_list[[j]]$n.event,
      surv = surv_list[[j]]$surv,
      cumul_inc = 1 - surv_list[[j]]$surv
    )
    surv_output_data_list[[i]] <- rbind(surv_output_data_list[[i]], tmp)
  }

}

surv_output_data <- do.call("rbind", surv_output_data_list)
write.csv(surv_output_data, file = "chc_cumul/outputs/timetofirst/km-plots-first-hes-all-underlying-data.csv")

plot_result <- patchworkGrob(
  
  plot_list[[1]]$plot + 
    plot_list[[2]]$plot + 
    plot_list[[3]]$plot + 
    plot_list[[4]]$plot + 
    plot_list[[5]]$plot + 
    plot_list[[6]]$plot + 
    plot_list[[7]]$plot + 
    plot_list[[8]]$plot + 
    plot_list[[9]]$plot + 
    plot_list[[10]]$plot +
    plot_layout(guides = "collect")
  
)

tiff("chc_cumul/outputs/timetofirst/km-plots-first_hes-all.tiff",
     height = 8, width = 12, units = "in", res = 300)
grid.arrange(plot_result,
             left = "Cumulative incidence (%)",
             bottom = "Age (years)")
dev.off()

rm(surv_list, tmp, i, surv_output_data,
   surv_output_data_list, j, rows_to_na,
   plot_list, curves, plot_result)

# * re-do main without suppression to check numbers -------------------

surv_list <- list(
  hesnpd = survfit(Surv(status_time_mo_hesnpd, status_hesnpd) ~ 1, data = surv_dt[academicyearofbirth == 2004])
)

surv_output_data <- data.table(
  curve = "npdhes",
  time = surv_list[[1]]$time,
  n.risk = surv_list[[1]]$n.risk,
  n.event = surv_list[[1]]$n.event,
  surv = surv_list[[1]]$surv,
  cumul_inc = 1 - surv_list[[1]]$surv
)

write.csv(surv_output_data, file = "chc_cumul/outputs/timetofirst/km-plots-time_to_first_npd_or_hes-b2004-underlying-data-NO-SUPPRESSION.csv", row.names = F)

rm(surv_list, surv_output_data)

# * re-do main without censoring to check numbers -------------------

surv_dt <- cohort_spine[academicyearofbirth == 2004, c("encrypted_hesid",
                                                       "dob",
                                                       "bday_16",
                                                       "date_first_npdhes")]

# hesnpd
surv_dt[, status_date_hesnpd := as.Date(NA)]
surv_dt[is.na(date_first_npdhes), status_date_hesnpd := bday_16]
surv_dt[!is.na(date_first_npdhes), status_date_hesnpd := date_first_npdhes]

surv_dt[, status_time_mo_hesnpd := as.integer(trunc(difftime(status_date_hesnpd, dob, units = "days") / 30.417))]
summary(surv_dt$status_time_mo_hesnpd)

surv_dt[, status_hesnpd := 1] # not censored
surv_dt[is.na(date_first_npdhes), status_hesnpd := 0] # censored

surv_list <- list(
  hesnpd = survfit(Surv(status_time_mo_hesnpd, status_hesnpd) ~ 1, data = surv_dt)
)

surv_output_data <- data.table(
  curve = "npdhes",
  time = surv_list[[1]]$time,
  n.risk = surv_list[[1]]$n.risk,
  n.event = surv_list[[1]]$n.event,
  surv = surv_list[[1]]$surv,
  cumul_inc = 1 - surv_list[[1]]$surv
)

write.csv(surv_output_data, file = "chc_cumul/outputs/timetofirst/km-plots-time_to_first_npd_or_hes-b2004-underlying-data-NO-SUPPRESSION-OR-CENSORING.csv", row.names = F)

rm(surv_list, surv_output_data)

# clear -------------------------------------------------------------------

rm(list = ls()); gc()

