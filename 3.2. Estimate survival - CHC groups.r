
# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Calculates time to first chronic health condition for subgroups
# *******************************************

# load --------------------------------------------------------------------

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
library(survival)
library(ggplot2)
library(survminer)
library(ggpubr)

load("1_CHC_CUMUL/processed/chc_diagnoses.rda")
load("1_CHC_CUMUL/processed/cohort_spine_censor.rda")

chc_diagnoses <- chc_diagnoses[, c("encrypted_hesid", "epistart", "type")]
chc_diagnoses <- chc_diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]
gc()

describe_outcome <- function(exposures, outcome = NULL, dat, n_round = 0, p_round = 1) {
  
  if (!is.null(outcome)) {
    # tmp_table <- round(table(dat[[exposures[1]]],
    #                          dat[[outcome]], useNA = "always"), n_round)
    # prop_tmp_table <- round(prop.table(tmp_table, 1) * 100, p_round)
    # output <- matrix(
    #   rep(NA, nrow(tmp_table) * 1),
    #   nrow = nrow(tmp_table),
    #   ncol = 1
    # )
    # colnames(output) <- outcome
    # rownames(output) <- paste0(exposures[1], "_", names(tmp_table[, 2]))
    # output[, 1] <- paste0(tmp_table[, 2], " (", prop_tmp_table[, 2], "%)")
    # 
    # if (length(exposures) > 1) {
    #   for (j in 2:length(exposures)) {
    #     tmp_table <- round(table(dat[[exposures[j]]],
    #                              dat[[outcome]], useNA = "always"), n_round)
    #     prop_tmp_table <- round(prop.table(tmp_table, 1) * 100, p_round)
    #     tmp_matrix <- matrix(
    #       rep(NA, nrow(tmp_table) * 1),
    #       nrow = nrow(tmp_table),
    #       ncol = 1
    #     )
    #     colnames(tmp_matrix) <- outcome
    #     rownames(tmp_matrix) <- paste0(exposures[j], "_", names(tmp_table[, 2]))
    #     tmp_matrix[, 1] <- paste0(tmp_table[, 2], " (", prop_tmp_table[, 2], "%)")
    #     output <- rbind(output, tmp_matrix)
    #   }
    # }
    # 
    # return(output)
    
  } else {
    
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
}

# get first chc diagnoses per child ---------------------------------------

chc_diagnoses <- chc_diagnoses[order(encrypted_hesid, type, epistart)]
chc_diagnoses[, record_idx := seq_len(.N), by = .(encrypted_hesid, type)]
chc_diagnoses <- chc_diagnoses[record_idx == 1]
chc_diagnoses[, record_idx := NULL]

types <- c("Cancer/Blood",
           "Cardiovascular",
           "Codes indicating non-specific chronic condition",
           "Mental health/behavioural",
           "Metabolic/endocrine/digestive/renal/genitourinary",
           "Musculoskeletal/skin",
           "Neurological",
           "Respiratory")

type_codes <- c("canbld",
                "cardio",
                "nonspe",
                "menlth",
                "metetc",
                "mskskn",
                "neurol",
                "respir")

for (i in 1:length(types)) {
  
  tmp <- chc_diagnoses[type == types[i]]
  setnames(tmp, "epistart", type_codes[i])
  tmp <- tmp[, c("encrypted_hesid", type_codes[i]), with = F]
  print(length(unique(tmp$encrypted_hesid)))
  print(nrow(tmp))
  cohort_spine <- merge(cohort_spine,
                        tmp,
                        by = "encrypted_hesid",
                        all.x = T)
  cohort_spine[!is.na(get(type_codes[i])) & get(type_codes[i]) < dob, (type_codes[i]) := dob]
  
}

rm(i, tmp)

# B2004 - ALL ELIGIBILITY SCHEMES -----------------------------------------

p_list <- list()
surv_output_data_list <- list()

for (i in 1:length(types)) {

  # create surv dataset
  surv_dt <- cohort_spine[academicyearofbirth %in% 2003:2004, c("encrypted_hesid",
                                                                "dob",
                                                                "bday_16",
                                                                "dod_approx",
                                                                "date_first_non_eng_hes",
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
                                                                type_codes[i]),
                          with = F]
  
  # censor date
  surv_dt[, censor_date := as.Date(NA)]
  surv_dt[, censor_date := min(bday_16, dod_approx, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]

  # calculate status date (first chc unless censored)
  surv_dt[, status_date := as.Date(NA)]
  surv_dt[is.na(get(type_codes[i])), status_date := censor_date]
  surv_dt[!is.na(get(type_codes[i])), status_date := min(censor_date, get(type_codes[i])), by = encrypted_hesid]

  table(is.na(surv_dt$status_date)) # should all be false
  table(surv_dt$status_date >= surv_dt$dob) # should all be true

  surv_dt[, status_time := as.integer(difftime(status_date, dob, units = "days"))]
  summary(surv_dt$status_time)

  surv_dt[, status_time_mo := as.integer(trunc(difftime(status_date, dob, units = "days") / 30.417))]
  summary(surv_dt$status_time_mo)

  surv_dt[, status := 1] # not censored
  surv_dt[is.na(get(type_codes[i])), status := 0] # censored
  surv_dt[!is.na(get(type_codes[i])) & censor_date < get(type_codes[i]), status := 0] # censored

  # create surv object
  surv_list <- list(
    el_scheme_1 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_1 == T, ]),
    el_scheme_2 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_2 == T, ]),
    el_scheme_3 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_3 == T, ]),
    el_scheme_4 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_4 == T, ]),
    el_scheme_5 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_5 == T, ]),
    el_scheme_6 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_6 == T, ]),
    el_scheme_7 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_7 == T, ]),
    el_scheme_8 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_8 == T, ])
  )


  # censor small cell counts
  for (j in 1:8) {
    table(surv_list[[j]]$n.event < 10)
    if (any(surv_list[[j]]$n.event < 10)) {
      rows_to_na <- which(surv_list[[j]]$n.event < 10)
      surv_list[[j]]$n.event[rows_to_na] <- NA
      #surv_list[[j]]$n.risk[rows_to_na] <- NA
      #surv_list[[j]]$n.censor[rows_to_na] <- NA
      surv_list[[j]]$surv[rows_to_na] <- NA
      #surv_list[[j]]$cumhaz[rows_to_na] <- NA
      surv_list[[j]]$lower[rows_to_na] <- NA
      surv_list[[j]]$upper[rows_to_na] <- NA
    }
  }


  # plot
  plot_title <- types[i]
  if (type_codes[i] == "metetc") { plot_title <- "Metabolic, &c" }
  if (type_codes[i] == "nonspe") { plot_title <- "Non-specific" }

  p_list[[i]] <- ggsurvplot(surv_list,
                            combine = T,
                            fun = "event",
                            censor = F,
                            conf.int = F,
                            xlab = "Age (years)",
                            ylab = "Cumulative incidene (%)",
                            legend.labs = c(paste0("Sensitivity ", 1:7), "Main analysis"),
                            legend = "none",
                            title = plot_title,
                            palette = "Set1")

  p_list[[i]]$plot <- p_list[[i]]$plot +
    scale_x_continuous(breaks = seq(0, 196, by = 12),
                       labels = 0:16) +
    scale_y_continuous(limits = c(0, 0.12),
                       breaks = c(seq(0, 0.12, 0.02)),
                       labels = c(seq(0, 12, 2)))

  # create underlying data
  surv_output_data_list[[i]] <- data.table(
    el_scheme = 1,
    time = surv_list[[1]]$time,
    n.risk = surv_list[[1]]$n.risk,
    n.event = surv_list[[1]]$n.event,
    surv = surv_list[[1]]$surv,
    cumul_inc = 1 - surv_list[[1]]$surv,
    type = type_codes[i]
  )

  for (j in 2:8) {
    tmp <- data.table(
      el_scheme = j,
      time = surv_list[[j]]$time,
      n.risk = surv_list[[j]]$n.risk,
      n.event = surv_list[[j]]$n.event,
      surv = surv_list[[j]]$surv,
      cumul_inc = 1 - surv_list[[j]]$surv,
      type = type_codes[i]
    )

    surv_output_data_list[[i]] <- rbind(surv_output_data_list[[i]], tmp)

  }

}

# extract legend
tmp <- ggsurvplot(surv_list,
                  combine = T,
                  fun = "event",
                  censor = F,
                  conf.int = F,
                  xlab = "Age (years)",
                  ylab = "Cumulative incidence (%)",
                  legend.labs = c(paste0("Sensitivity ", 1:7), "Main analysis"),
                  legend = "bottom",
                  palette = "Set1")

tmp <- tmp + guides(fill = guide_legend(title = "Denominator definition"),
                    colour = guide_legend(title = "Denominator definition"))

leg <- get_legend(tmp)
rm(tmp)

# merge underlying data and output tables
surv_output_data <- do.call("rbind", surv_output_data_list)

table(surv_output_data$n.event < 10)

surv_output_data[, time_yrs := time / 12]
write.csv(surv_output_data, file = "1_CHC_CUMUL/outputs/surv/km-plots-chc-subtypes-all-el-schemes-b2003-b2004-underlying-data.csv", row.names = F)

# output plots
tiff("1_CHC_CUMUL/outputs/surv/km-plots-chc-subtypes-all-el-schemes-b2003-b2004.tiff",
     height = 12, width = 8, units = "in", res = 300)

# ggsurvplot(surv_list,
#            combine = T,
#            fun = "event",
#            censor = F,
#            conf.int = T,
#            xlab = "Age (years)",
#            ylab = "CI (%)",
#            #legend.labs = paste0("Scheme ", 1:8),
#            #legend = "bottom",
#            palette = "Set1")

arrange_ggsurvplots(p_list, nrow = 4, ncol = 2)
dev.off()

tiff("1_CHC_CUMUL/outputs/surv/km-plots-chc-subtypes-all-el-schemes-b2003-b2004-legend.tiff",
     height = 2, width = 6, units = "in", res = 300)
as_ggplot(leg)
dev.off()

rm(leg, p_list, surv_output_data, i, j, surv_output_data_list,
   plot_title, surv_list, surv_dt, rows_to_na); gc()

# ALL COHORTS - EL SCHEME 8 -----------------------------------------

p_list <- list()
surv_output_data_list <- list()

for (i in 1:length(types)) {
  
  # create surv dataset
  surv_dt <- cohort_spine[, c("encrypted_hesid",
                              "dob",
                              "bday_16",
                              "dod_approx",
                              "date_first_non_eng_hes",
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
                              "el_scheme_8",
                              type_codes[i]),
                          with = F]
  
  # combine cohrts
  surv_dt[, academicyearofbirth_comb := "2002/03 & 2003/04"]
  surv_dt[academicyearofbirth %in% 2005:2006, academicyearofbirth_comb := "2004/05 & 2005/06"]
  surv_dt[academicyearofbirth %in% 2007:2008, academicyearofbirth_comb := "2006/07 & 2007/08"]
  surv_dt[academicyearofbirth %in% 2009:2010, academicyearofbirth_comb := "2008/09 & 2009/10"]
  surv_dt[academicyearofbirth %in% 2011:2012, academicyearofbirth_comb := "2010/11 & 2011/12"]
  
  # censor date
  surv_dt[, censor_date := as.Date(NA)]
  surv_dt[, censor_date := min(bday_16, dod_approx, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]
  
  # calculate status date (first chc unless censored)
  surv_dt[, status_date := as.Date(NA)]
  surv_dt[is.na(get(type_codes[i])), status_date := censor_date]
  surv_dt[!is.na(get(type_codes[i])), status_date := min(censor_date, get(type_codes[i])), by = encrypted_hesid]
  
  # table(is.na(surv_dt$status_date)) # should all be false
  # table(surv_dt$status_date >= surv_dt$dob) # should all be true
  
  surv_dt[, status_time := as.integer(difftime(status_date, dob, units = "days"))]
  # summary(surv_dt$status_time)
  
  surv_dt[, status_time_mo := as.integer(trunc(difftime(status_date, dob, units = "days") / 30.417))]
  # summary(surv_dt$status_time_mo)
  
  surv_dt[, status := 1] # not censored
  surv_dt[is.na(get(type_codes[i])), status := 0] # censored
  surv_dt[!is.na(get(type_codes[i])) & censor_date < get(type_codes[i]), status := 0] # censored
  
  # create surv object
  surv_obj <- survfit(Surv(status_time_mo, status) ~ academicyearofbirth_comb, data = surv_dt[surv_dt$el_scheme_8 == T, ])

  # suppress small cell counts
  #table(surv_obj$n.event < 10)
  if (any(surv_obj$n.event < 10)) {
    rows_to_na <- which(surv_obj$n.event < 10)
    surv_obj$n.event[rows_to_na] <- NA
    surv_obj$surv[rows_to_na] <- NA
    surv_obj$lower[rows_to_na] <- NA
    surv_obj$upper[rows_to_na] <- NA
  }
  
  # suppress where cohorts become unobservable
  cohorts <- rep(names(surv_obj$strata), surv_obj$strata)
  cohorts <- gsub("academicyearofbirth_comb=", "", cohorts)
  
  surv_obj$surv[surv_obj$time > (192 - (12 * 2)) & cohorts == "2004/05 & 2005/06"] <- NA
  surv_obj$surv[surv_obj$time > (192 - (12 * 4)) & cohorts == "2006/07 & 2007/08"] <- NA
  surv_obj$surv[surv_obj$time > (192 - (12 * 6)) & cohorts == "2008/09 & 2009/10"] <- NA
  surv_obj$surv[surv_obj$time > (192 - (12 * 8)) & cohorts == "2010/11 & 2011/12"] <- NA
  
  # plot
  plot_title <- types[i]
  if (type_codes[i] == "metetc") { plot_title <- "Metabolic, &c" }
  if (type_codes[i] == "nonspe") { plot_title <- "Non-specific" }
  
  p_list[[i]] <- ggsurvplot(surv_obj,
                            #combine = T,
                            fun = "event",
                            censor = F,
                            conf.int = F,
                            xlab = "Age (years)",
                            ylab = "Cumulative incidence (%)",
                            #legend.labs = c("2004", "2009", "2012"),
                            legend = "none",
                            title = plot_title)
  
  p_list[[i]]$plot <- p_list[[i]]$plot +
    scale_x_continuous(breaks = seq(0, 196, by = 12),
                       labels = 0:16) +
    scale_y_continuous(limits = c(0, 0.12),
                       breaks = c(seq(0, 0.12, 0.02)),
                       labels = c(seq(0, 12, 2)))
  
  # create underlying data
  surv_output_data_list[[i]] <- data.table(
    cohort = rep(names(surv_obj$strata), surv_obj$strata),
    el_scheme = 8,
    time = surv_obj$time,
    n.risk = surv_obj$n.risk,
    n.event = surv_obj$n.event,
    surv = surv_obj$surv,
    cumul_inc = 1 - surv_obj$surv,
    type = type_codes[i]

  )
  
}

# extract legend
tmp <- ggsurvplot(surv_obj,
                  #combine = T,
                  fun = "event",
                  censor = F,
                  conf.int = F,
                  xlab = "Age (years)",
                  ylab = "Cumulative incidence (%)",
                  legend.labs = c("2002/03 & 2003/04",
                                  "2004/05 & 2005/06",
                                  "2006/07 & 2007/08",
                                  "2008/09 & 2009/10",
                                  "2010/11 & 2011/12"),
                  legend = "bottom",
                  title = plot_title)

tmp <- tmp + guides(fill = guide_legend(title = "Birth cohort\n(academic year)"),
                    colour = guide_legend(title = "Birth cohort\n(academic year)"))

leg <- get_legend(tmp)
rm(tmp)

# merge underlying data and output tables
surv_output_data <- do.call("rbind", surv_output_data_list)

table(surv_output_data$n.event < 10)

surv_output_data[, time_yrs := time / 12]
write.csv(surv_output_data, file = "1_CHC_CUMUL/outputs/surv/km-plots-chc-subtypes-el-scheme-8-all-cohorts-underlying-data.csv", row.names = F)

# output plots
tiff("1_CHC_CUMUL/outputs/surv/km-plots-chc-subtypes-el-scheme-8-all-cohorts.tiff",
     height = 12, width = 8, units = "in", res = 300)
arrange_ggsurvplots(p_list, nrow = 4, ncol = 2)
dev.off()

tiff("1_CHC_CUMUL/outputs/surv/km-plots-chc-subtypes-el-scheme-8-all-cohorts-legend.tiff",
     height = 2, width = 10, units = "in", res = 300)
as_ggplot(leg)
dev.off()

rm(leg, p_list, surv_dt, surv_obj, surv_output_data, surv_output_data_list,
   i, plot_title, rows_to_na); gc()

# ALL EL SCHEMS ALL COHORTS -----------------------------------------------

cohorts <- c(2003,
             2005,
             2007,
             2009,
             2011)

surv_output_data_list <- list()

for (cohort in cohorts) {
  
  print(cohort)
  
  for (i in 1:length(types)) {
    
    print(type_codes[i])
    
    # create surv dataset
    surv_dt <- cohort_spine[academicyearofbirth %in% c(cohort, cohort + 1), c("encrypted_hesid",
                                                                              "dob",
                                                                              "bday_16",
                                                                              "dod_approx",
                                                                              "date_first_non_eng_hes",
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
                                                                              type_codes[i]),
                            with = F]
    
    # combine cohrts
    if (cohort == 2003) { surv_dt[, academicyearofbirth_comb := "2002/03 & 2003/04"] }
    if (cohort == 2005) { surv_dt[, academicyearofbirth_comb := "2004/05 & 2005/06"] }
    if (cohort == 2007) { surv_dt[, academicyearofbirth_comb := "2006/07 & 2007/08"] }
    if (cohort == 2009) { surv_dt[, academicyearofbirth_comb := "2008/09 & 2009/10"] }
    if (cohort == 2011) { surv_dt[, academicyearofbirth_comb := "2010/11 & 2011/12"] }
   
    # censor date
    surv_dt[, censor_date := as.Date(NA)]
    surv_dt[, censor_date := min(bday_16, dod_approx, date_first_non_eng_hes, na.rm = T), by = encrypted_hesid]
    
    # calculate status date (first chc unless censored)
    surv_dt[, status_date := as.Date(NA)]
    surv_dt[is.na(get(type_codes[i])), status_date := censor_date]
    surv_dt[!is.na(get(type_codes[i])), status_date := min(censor_date, get(type_codes[i])), by = encrypted_hesid]
    
    surv_dt[, status_time := as.integer(difftime(status_date, dob, units = "days"))]
    surv_dt[, status_time_mo := as.integer(trunc(difftime(status_date, dob, units = "days") / 30.417))]
    
    surv_dt[, status := 1] # not censored
    surv_dt[is.na(get(type_codes[i])), status := 0] # censored
    surv_dt[!is.na(get(type_codes[i])) & censor_date < get(type_codes[i]), status := 0] # censored
    
    # create surv object
    surv_list <- list(
      el_scheme_1 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_1 == T, ]),
      el_scheme_2 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_2 == T, ]),
      el_scheme_3 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_3 == T, ]),
      el_scheme_4 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_4 == T, ]),
      el_scheme_5 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_5 == T, ]),
      el_scheme_6 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_6 == T, ]),
      el_scheme_7 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_7 == T, ]),
      el_scheme_8 = survfit(Surv(status_time_mo, status) ~ 1, data = surv_dt[surv_dt$el_scheme_8 == T, ])
    )
    
    # censor small cell counts
    for (j in 1:8) {
      table(surv_list[[j]]$n.event < 10)
      if (any(surv_list[[j]]$n.event < 10)) {
        rows_to_na <- which(surv_list[[j]]$n.event < 10)
        surv_list[[j]]$n.event[rows_to_na] <- NA
        surv_list[[j]]$surv[rows_to_na] <- NA
        surv_list[[j]]$lower[rows_to_na] <- NA
        surv_list[[j]]$upper[rows_to_na] <- NA
      }
    }
    
    if (cohort >= 2005) {
      for (j in 1:length(surv_list)) {
        if (cohort == 2005) { surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 2))] <- NA }
        if (cohort == 2007) { surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 4))] <- NA }
        if (cohort == 2009) { surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 6))] <- NA }
        if (cohort == 2011) { surv_list[[j]]$surv[surv_list[[j]]$time > (192 - (12 * 8))] <- NA }
      }
    }
    
    # create underlying data
    surv_output_data_list[[i]] <- data.table(
      el_scheme = 1,
      time = surv_list[[1]]$time,
      n.risk = surv_list[[1]]$n.risk,
      n.event = surv_list[[1]]$n.event,
      surv = surv_list[[1]]$surv,
      cumul_inc = 1 - surv_list[[1]]$surv,
      type = type_codes[i]
    )
    
    for (j in 2:length(surv_list)) {
      tmp <- data.table(
        el_scheme = j,
        time = surv_list[[j]]$time,
        n.risk = surv_list[[j]]$n.risk,
        n.event = surv_list[[j]]$n.event,
        surv = surv_list[[j]]$surv,
        cumul_inc = 1 - surv_list[[j]]$surv,
        type = type_codes[i]
      )
      
      surv_output_data_list[[i]] <- rbind(surv_output_data_list[[i]], tmp)
      
    }
    
  }
  
  print("Merging")
  
  # merge underlying data and output tables
  surv_output_data <- do.call("rbind", surv_output_data_list)
  
  # set n to NA where surv is NA
  surv_output_data[is.na(surv), n.risk := NA]
  surv_output_data[is.na(surv), cumul_inc := NA]
  surv_output_data[is.na(surv), n.event := NA]
  
  if (any(surv_output_data$n.event < 10, na.rm = T)) {
    print(paste0(cohort, " has counts <10"))
  }
  
  write.csv(surv_output_data,
            file = paste0("1_CHC_CUMUL/outputs/surv/all-el-schemes/sub-types/km-plots-chc-subtypes-all-el-schemes-b", cohort, "-b", cohort + 1, "-underlying-data.csv"), 
            row.names = F)
  
  
}

# clear -------------------------------------------------------------------

rm(list = ls()); gc()
