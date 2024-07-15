# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Calculates cumulative incidence using ONS denominators
# *******************************************

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
library(ggplot2)

master_dir <- "[path omitted]"

load("1_CHC_CUMUL/processed/chc_diagnoses_ons.rda")
pop <- fread("1_CHC_CUMUL/processed/ons_mye.csv")

# Need to remove the last year due to COVID
chc_diagnoses[, keep := T]
chc_diagnoses[birthfyear == 2005 & startage > 14, keep := F]
chc_diagnoses[birthfyear == 2006 & startage > 13, keep := F]
chc_diagnoses[birthfyear == 2007 & startage > 12, keep := F]
chc_diagnoses[birthfyear == 2008 & startage > 11, keep := F]
chc_diagnoses[birthfyear == 2009 & startage > 10, keep := F]
chc_diagnoses[birthfyear == 2010 & startage > 9, keep := F]
chc_diagnoses[birthfyear == 2011 & startage > 8, keep := F]
chc_diagnoses[birthfyear == 2012 & startage > 7, keep := F]
chc_diagnoses[birthfyear == 2013 & startage > 6, keep := F]
chc_diagnoses[birthfyear == 2014 & startage > 5, keep := F]
chc_diagnoses[birthfyear == 2015 & startage > 4, keep := F]
chc_diagnoses[birthfyear == 2016 & startage > 3, keep := F]
chc_diagnoses[birthfyear == 2017 & startage > 2, keep := F]
chc_diagnoses[birthfyear == 2018 & startage > 1, keep := F]
chc_diagnoses[birthfyear == 2019 & startage > 0, keep := F]
chc_diagnoses <- chc_diagnoses[keep == T]
chc_diagnoses[, keep := NULL]

# Keep only one code for any CHC
chc_diagnoses <- chc_diagnoses[order(encrypted_hesid, epistart)]
chc_any <- chc_diagnoses[!duplicated(encrypted_hesid)]

pop[, keep := T]
pop[birth_cohort == 2005 & age > 14, keep := F]
pop[birth_cohort == 2006 & age > 13, keep := F]
pop[birth_cohort == 2007 & age > 12, keep := F]
pop[birth_cohort == 2008 & age > 11, keep := F]
pop[birth_cohort == 2009 & age > 10, keep := F]
pop[birth_cohort == 2010 & age > 9, keep := F]
pop[birth_cohort == 2011 & age > 8, keep := F]
pop[birth_cohort == 2012 & age > 7, keep := F]
pop[birth_cohort == 2013 & age > 6, keep := F]
pop[birth_cohort == 2014 & age > 5, keep := F]
pop[birth_cohort == 2015 & age > 4, keep := F]
pop[birth_cohort == 2016 & age > 3, keep := F]
pop[birth_cohort == 2017 & age > 2, keep := F]
pop[birth_cohort == 2018 & age > 1, keep := F]
pop[birth_cohort == 2019 & age > 0, keep := F]
pop[birth_cohort == 2020, keep := F]
pop <- pop[keep == T]
pop[, keep := NULL]

pop[, chc_n := as.integer(NA)]
pop[, at_risk := as.integer(NA)]

for (year in 2003:2019) {
  print(table(chc_any[birthfyear == year]$startage))
  pop[birth_cohort == year, chc_n := table(chc_any[birthfyear == year]$startage)]
  
  pop[birth_cohort == year & age == 0, at_risk := mye_n]
  
  coh_len <- nrow(pop[birth_cohort == year])
  if (coh_len > 1) {
    for (i in 2:coh_len) {
      pop[birth_cohort == year]$at_risk[i] <-
        pop[birth_cohort == year]$at_risk[i-1] - pop[birth_cohort == year]$chc_n[i-1]
    }
  }
}

rm(year, i)

pop[, hazard := chc_n / at_risk]

pop[, int_hazard := as.double(NA)]
pop[age == 0, int_hazard := hazard]

for (year in 2003:2019) {

  coh_len <- nrow(pop[birth_cohort == year])
  if (coh_len > 1) {
    for (i in 2:coh_len) {
      pop[birth_cohort == year]$int_hazard[i] <-
        pop[birth_cohort == year]$int_hazard[i-1] + pop[birth_cohort == year]$hazard[i]
    }
  }

}

rm(year, i)

pop[, cum_inc := 1 - exp(-int_hazard)]
pop[, cum_inc_p := round(cum_inc * 100, 1)]
pop[, birth_cohort := as.factor(birth_cohort)]

tiff("1_CHC_CUMUL/outputs/ons/ons-any-chc.tiff", width = 6, height = 4, res = 360, units = "in")
ggplot(pop, aes(x = age,
                y = cum_inc_p,
                colour = birth_cohort)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(0, 30),
                     expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("Age (years)") +
  ylab("Cumulative incidence") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.background = element_blank())
dev.off()

write.csv(pop, file = "1_CHC_CUMUL/outputs/ons/ons-any-chc-data.csv")





# sub-types
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

chc_diagnoses <- chc_diagnoses[order(encrypted_hesid, epistart)]

pop_list <- list()

for (i in 1:length(type_codes)) {
  
  print(type_codes[i])
  chc_tmp <- chc_diagnoses[type == types[i]]
  chc_tmp <- chc_tmp[!duplicated(encrypted_hesid)]
  pop_list[[i]] <- fread("1_CHC_CUMUL/processed/ons_mye.csv")
  
  pop_list[[i]][, keep := T]
  pop_list[[i]][birth_cohort == 2005 & age > 14, keep := F]
  pop_list[[i]][birth_cohort == 2006 & age > 13, keep := F]
  pop_list[[i]][birth_cohort == 2007 & age > 12, keep := F]
  pop_list[[i]][birth_cohort == 2008 & age > 11, keep := F]
  pop_list[[i]][birth_cohort == 2009 & age > 10, keep := F]
  pop_list[[i]][birth_cohort == 2010 & age > 9, keep := F]
  pop_list[[i]][birth_cohort == 2011 & age > 8, keep := F]
  pop_list[[i]][birth_cohort == 2012 & age > 7, keep := F]
  pop_list[[i]][birth_cohort == 2013 & age > 6, keep := F]
  pop_list[[i]][birth_cohort == 2014 & age > 5, keep := F]
  pop_list[[i]][birth_cohort == 2015 & age > 4, keep := F]
  pop_list[[i]][birth_cohort == 2016 & age > 3, keep := F]
  pop_list[[i]][birth_cohort == 2017 & age > 2, keep := F]
  pop_list[[i]][birth_cohort == 2018 & age > 1, keep := F]
  pop_list[[i]][birth_cohort == 2019 & age > 0, keep := F]
  pop_list[[i]][birth_cohort == 2020, keep := F]
  pop_list[[i]] <- pop_list[[i]][keep == T]
  pop_list[[i]][, keep := NULL]
  
  pop_list[[i]][, chc_n := as.integer(NA)]
  pop_list[[i]][, at_risk := as.integer(NA)]
  
  for (year in 2003:2019) {
    print(table(chc_tmp[birthfyear == year]$startage))
    pop_list[[i]][birth_cohort == year, chc_n := table(chc_tmp[birthfyear == year]$startage)]
    
    pop_list[[i]][birth_cohort == year & age == 0, at_risk := mye_n]
    
    coh_len <- nrow(pop_list[[i]][birth_cohort == year])
    
    if (coh_len > 1) {
      for (j in 2:coh_len) {
        pop_list[[i]][birth_cohort == year]$at_risk[j] <-
          pop_list[[i]][birth_cohort == year]$at_risk[j-1] - pop_list[[i]][birth_cohort == year]$chc_n[j-1]
      }
    }
  }
  
  rm(year, j)
  
  pop_list[[i]][, hazard := chc_n / at_risk]
  
  pop_list[[i]][, int_hazard := as.double(NA)]
  pop_list[[i]][age == 0, int_hazard := hazard]
  
  for (year in 2003:2019) {
    
    coh_len <- nrow(pop_list[[i]][birth_cohort == year])
    
    if (coh_len > 1) {
      for (j in 2:coh_len) {
        pop_list[[i]][birth_cohort == year]$int_hazard[j] <-
          pop_list[[i]][birth_cohort == year]$int_hazard[j-1] + pop_list[[i]][birth_cohort == year]$hazard[j]
      }
    }
    
  }
  
  rm(year, j)
  
  pop_list[[i]][, cum_inc := 1 - exp(-int_hazard)]
  pop_list[[i]][, cum_inc_p := round(cum_inc * 100, 1)]
  pop_list[[i]][, birth_cohort := as.factor(birth_cohort)]
  pop_list[[i]][, chc_type := type_codes[i]]
  
}

pop <- do.call("rbind", pop_list)

max(pop$cum_inc_p)

tiff("1_CHC_CUMUL/outputs/ons/ons-sub-types.tiff", width = 6, height = 4, res = 360, units = "in")
ggplot(pop, aes(x = age,
                y = cum_inc_p,
                colour = birth_cohort)) +
  geom_line(size = 1.2) +
  scale_y_continuous(limits = c(0, 10),
                     expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("Age (years)") +
  ylab("Cumulative incidence") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        strip.background = element_blank()) +
  facet_wrap(~chc_type)
dev.off()

write.csv(pop, file = "1_CHC_CUMUL/outputs/ons/ons-sub-types-data.csv")