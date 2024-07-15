
# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Estimates multimorbidity
# *******************************************

# load --------------------------------------------------------------------

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
library(ggplot2)

load("1_CHC_CUMUL/processed/chc_diagnoses.rda")
load("1_CHC_CUMUL/processed/cohort_spine_censor.rda")

cohort_spine <- cohort_spine[el_scheme_8 == T]
chc_diagnoses <- chc_diagnoses[, c("encrypted_hesid", "epistart", "type")]
chc_diagnoses <- chc_diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]
gc()

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
           "Respiratory",
           "Chronic infections")

type_codes <- c("canbld",
                "cardio",
                "nonspe",
                "menlth",
                "metetc",
                "mskskn",
                "neurol",
                "respir",
                "chrinf")

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

# multimorbidity ----------------------------------------------------------

cohort_spine_tmp <- cohort_spine[, c("encrypted_hesid", "academicyearofbirth", "dob", type_codes), with = F]

for (code in type_codes) {
  
  print(code)
  new_var <- paste0(code, "_age")
  cohort_spine_tmp[, (new_var) := fifelse(is.na(get(code)),
                                          as.numeric(NA),
                                          round(as.integer(difftime(get(code), dob, units = "days") / 365.25), 1))]
  for (i in 0:16) {
    new_var_by_age <- paste0(code, "_by_age_", i)
    cohort_spine_tmp[, (new_var_by_age) := get(new_var) <= i]
  }
  
  cohort_spine_tmp[, (code) := NULL]
  cohort_spine_tmp[, (new_var) := NULL]

}

cohort_spine_tmp[, dob := NULL]

for (i in 0:16) {
  
  print(i)
  varnames <- names(cohort_spine_tmp)[grepl(paste0("by_age_", i, "$"), names(cohort_spine_tmp))]
  new_var <- paste0("n_cond_by_age_", i)
  cohort_spine_tmp[, (new_var) := rowSums(cohort_spine_tmp[, varnames, with = F], na.rm = T)]
  
  for (varname in varnames) {
    cohort_spine_tmp[, (varname) := NULL]
  }
  
}

rm(code, new_var, new_var_by_age, i, varnames)

# check
# table(cohort_spine_tmp$n_cond_by_age_0)
# table(cohort_spine_tmp$n_cond_by_age_1)
# table(cohort_spine_tmp$n_cond_by_age_2)
# table(cohort_spine_tmp$n_cond_by_age_3)
# table(cohort_spine_tmp$n_cond_by_age_4)
# table(cohort_spine_tmp$n_cond_by_age_5)
# table(cohort_spine_tmp$n_cond_by_age_6)
# table(cohort_spine_tmp$n_cond_by_age_7)
# table(cohort_spine_tmp$n_cond_by_age_8)
# table(cohort_spine_tmp$n_cond_by_age_9)
# table(cohort_spine_tmp$n_cond_by_age_10)
# table(cohort_spine_tmp$n_cond_by_age_11)
# table(cohort_spine_tmp$n_cond_by_age_12)
# table(cohort_spine_tmp$n_cond_by_age_13)
# table(cohort_spine_tmp$n_cond_by_age_14)
# table(cohort_spine_tmp$n_cond_by_age_15)
# table(cohort_spine_tmp$n_cond_by_age_16)

# reshape
cohort_spine_tmp[, encrypted_hesid := NULL]
cohort_spine_tmp <- cohort_spine_tmp[order(academicyearofbirth)]

for (i in 0:16) {
  
  print(i)
  cur_var <- paste0("n_cond_by_age_", i)
  
  for (j in 0:5) {

    new_var <- paste0("cond_n_", j, "_by_age_", i)
    cohort_spine_tmp[, (new_var) := sum(get(cur_var) == j), by = .(academicyearofbirth)]
    
  }
  
  cohort_spine_tmp[, (cur_var) := NULL]
  
}

cohort_spine_tmp <- cohort_spine_tmp[!duplicated(cohort_spine_tmp)]

dt_long <- melt(
  cohort_spine_tmp,
  id.vars = "academicyearofbirth",
  value.name = "n_children"
)

dt_long <- dt_long[order(academicyearofbirth)]

dt_long[, by_age := variable]
dt_long[, by_age := gsub("cond_n_[0-9]_by_age_", "", by_age)]

dt_long[, cond_n_group := variable]
dt_long[, cond_n_group := gsub("_by_age_[0-9].*", "", cond_n_group)]
dt_long[, cond_n_group := gsub("cond_n_", "", cond_n_group)]
dt_long[, variable := NULL]

dt_long[, by_age := as.integer(by_age)]
dt_long[, cond_n_group := as.factor(cond_n_group)]



dt_long[by_age > 15 & academicyearofbirth == 2005, n_children := NA]
dt_long[by_age > 14 & academicyearofbirth == 2006, n_children := NA]
dt_long[by_age > 13 & academicyearofbirth == 2007, n_children := NA]
dt_long[by_age > 12 & academicyearofbirth == 2008, n_children := NA]
dt_long[by_age > 11 & academicyearofbirth == 2009, n_children := NA]
dt_long[by_age > 10 & academicyearofbirth == 2010, n_children := NA]
dt_long[by_age > 09 & academicyearofbirth == 2011, n_children := NA]
dt_long[by_age > 08 & academicyearofbirth == 2012, n_children := NA]



tiff("chc_cumul/outputs/multi/multi-graph-all-children.tiff",
     width = 12, height = 8, units = "in", res = 300)
ggplot(dt_long, aes(x = by_age,
                    y = n_children,
                    fill = cond_n_group)) +
  geom_area(position = "fill") +
  scale_fill_discrete(name = "Number of CHC groups",
                      labels = "1", "2", "3", "4", "5+") +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_continuous(breaks = 0:16,
                     labels = 0:16) +
  xlab("By age") +
  ylab("%") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.background = element_blank()) +
  facet_wrap(~academicyearofbirth)
dev.off()

tiff("chc_cumul/outputs/multi/multi-graph-with-chc-only.tiff",
     width = 12, height = 8, units = "in", res = 300)
ggplot(dt_long[cond_n_group != 0], aes(x = by_age,
                                       y = n_children,
                                       fill = cond_n_group)) +
  geom_area(position = "fill") +
  scale_fill_discrete(name = "Number of CHC groups",
                      labels = "1", "2", "3", "4", "5+") +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_continuous(breaks = 0:16,
                     labels = 0:16) +
  xlab("By age") +
  ylab("%") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.background = element_blank()) +
  facet_wrap(~academicyearofbirth)
dev.off()

write.csv(dt_long, file = "chc_cumul/outputs/multi/multi-graphs-underlying-data.csv")
