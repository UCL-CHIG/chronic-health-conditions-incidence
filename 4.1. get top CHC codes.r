# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Gets most frequent diagnosis codes
# *******************************************

# load cohort --------------------------------------------------------------

setwd("[path omitted]")
assign(".lib.loc", c(.libPaths(), "[path omitted]"), envir = environment(.libPaths))
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)

load("1_CHC_CUMUL/processed/chc_diagnoses.rda")
load("1_CHC_CUMUL/processed/cohort_spine_censor.rda")

chc_diagnoses <- chc_diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]
chc_diagnoses[, age := as.integer(trunc(difftime(epistart, dob, units = "days")) / 365.25)]
chc_diagnoses <- chc_diagnoses[, c("encrypted_hesid", "epistart", "age", "diag", "type")]
gc()



# # which metetc codes? ---------------------------------------------------

metetc <- chc_diagnoses[type == "Metabolic/endocrine/digestive/renal/genitourinary"]
metetc <- metetc[age < 8]

metetc <- merge(metetc,
                cohort_spine[, c("encrypted_hesid", "academicyearofbirth")],
                by = "encrypted_hesid",
                all.x = T)

write.csv(table(metetc$diag, metetc$academicyearofbirth), file = "1_CHC_CUMUL/outputs/predom_codes/metetc_codes.csv")

# check whether K52.9
master_dir <- "P:/Working/Master data TEST"
diagnoses <- fread(paste0(master_dir, "/HES_APC_DIAG_combined.csv"),
                   header = T,
                   stringsAsFactors = F,
                   integer64 = "character")

diagnoses <- diagnoses[substr(diag, 1, 3) == "K52"]
diagnoses[, diag := substr(diag, 1, 4)]
diagnoses <- diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]
diagnoses <- diagnoses[epistart <= as.Date("2019-08-31")]
diagnoses <- diagnoses[startage < 8]

diagnoses[, dup := duplicated(diagnoses)]
diagnoses <- diagnoses[dup == F]
diagnoses[, dup := NULL]

diagnoses <- merge(diagnoses,
                   cohort_spine[, c("encrypted_hesid", "academicyearofbirth")],
                   by = "encrypted_hesid",
                   all.x = T)

table(diagnoses$diag, diagnoses$academicyearofbirth)
write.csv(table(diagnoses$diag, diagnoses$academicyearofbirth), file = "1_CHC_CUMUL/outputs/predom_codes/metetc_codes_k52.csv")
rm(diagnoses, metetc); gc()



# Top codes in each group ---------------------------------------------------------------------

birth_years <- 2003:2004
cohort_spine <- cohort_spine[academicyearofbirth %in% birth_years]
chc_diagnoses <- chc_diagnoses[encrypted_hesid %in% cohort_spine$encrypted_hesid]

# Only count first occurence of each code
chc_diagnoses <- chc_diagnoses[order(encrypted_hesid, diag)]
chc_diagnoses <- chc_diagnoses[, dups := duplicated(chc_diagnoses[, c("encrypted_hesid", "diag")])]
chc_diagnoses <- chc_diagnoses[dups == F]
chc_diagnoses[, dups := NULL]
gc()


hardelid <- fread("code lists/hardelid.csv",
                  header = T,
                  stringsAsFactors = F)

hardelid[, code := gsub("\\.", "", code)]

chc_diagnoses <- merge(chc_diagnoses,
                       hardelid[, c("code", "description")],
                       by.x = "diag",
                       by.y = "code",
                       all.x = T)

rm(hardelid)

chc_diagnoses[, code_desc_concat := paste0(diag, ": ", description)]

# output
types <- levels(as.factor(chc_diagnoses$type))
chc_codes <- c("cancer",
               "cardio",
               "chrinf",
               "nonspe",
               "menlth",
               "metetc",
               "mskskn",
               "neurol",
               "respir")

alt5_list <- list()
a510_list <- list()
a1115_list <- list()

for (i in 1:length(types)) {

  
  tab <- table(chc_diagnoses[age < 5 & type == types[i]]$code_desc_concat)
  tab <- tab[order(-tab)]
  tab <- tab[1:20]
  tab[tab < 10] <- NA
  alt5_list[[i]] <- as.data.table(tab)
  setnames(alt5_list[[i]], c("V1", "N"), c("code", "n"))
  alt5_list[[i]][, chc_type := types[i]]
  
  tab <- table(chc_diagnoses[age >= 5 & age < 11 & type == types[i]]$code_desc_concat)
  tab <- tab[order(-tab)]
  tab <- tab[1:20]
  tab[tab < 10] <- NA
  a510_list[[i]] <- as.data.table(tab)
  setnames(a510_list[[i]], c("V1", "N"), c("code", "n"))
  a510_list[[i]][, chc_type := types[i]]
  
  tab <- table(chc_diagnoses[age >= 11 & age < 16 & type == types[i]]$code_desc_concat)
  tab <- tab[order(-tab)]
  tab <- tab[1:20]
  tab[tab < 10] <- NA
  a1115_list[[i]] <- as.data.table(tab)
  setnames(a1115_list[[i]], c("V1", "N"), c("code", "n"))
  a1115_list[[i]][, chc_type := types[i]]
  
}

# tidy ouputs
alt5_dt <- do.call("rbind", alt5_list)
a510_dt <- do.call("rbind", a510_list)
a1115_dt <- do.call("rbind", a1115_list)

alt5_dt <- alt5_dt[!is.na(n)]
a510_dt <- a510_dt[!is.na(n)]
a1115_dt <- a1115_dt[!is.na(n)]

write.csv(alt5_dt, file = "1_CHC_CUMUL/outputs/predom_codes/alt5_dt.csv", row.names = F)
write.csv(a510_dt, file = "1_CHC_CUMUL/outputs/predom_codes/a510_dt.csv", row.names = F)
write.csv(a1115_dt, file = "1_CHC_CUMUL/outputs/predom_codes/a1115_dt.csv", row.names = F)

# graphs
# age_grp <- c("lt5",
#              "5-10",
#              "11-15")
# age_grp_label <- c("Less than 5 years",
#                    "5 to 10 years",
#                    "11 to 15 years")
# 
# 
# p_list <- list()
# 
# for (i in 1:3) {
# 
#   curr_list <- list()
#   for (j in 1:length(chc_codes)) {
#     curr_list[[j]] <- fread(paste0("1_CHC_CUMUL/outputs/predom_codes/diags_", chc_codes[j], "_", age_grp[i], ".csv"))
#     curr_list[[j]] <- curr_list[[j]][, c("Var1", "Freq")]
#     setnames(curr_list[[j]], "Var1", "Diagnosis")
# 
#     curr_list[[j]] <- curr_list[[j]][order(-Freq)]
#     #curr_list[[j]] <- curr_list[[j]][1:10]
#     curr_list[[j]][, chc_code := chc_codes[j]]
#   }
# 
#   curr_dt <- do.call("rbind", curr_list)
# 
#   curr_dt[, chc_code := factor(chc_code,
#                                levels = c("cancer",
#                                           "cardio",
#                                           "chrinf",
#                                           "nonspe",
#                                           "menlth",
#                                           "metetc",
#                                           "mskskn",
#                                           "neurol",
#                                           "respir"))]
#   curr_dt[, id := 1:.N]
# 
#   write.csv(curr_dt, file = paste0("1_CHC_CUMUL/outputs/predom_codes/fig/top10_underlying_data_", age_grp[i], ".csv"))
# 
#   if (i < 3) {
# 
#     p_list[[i]] <- ggplot(data = curr_dt,
#                           aes(x = as.factor(id),
#                               y = Freq,
#                               fill = chc_code)) +
#       geom_bar(stat = "identity") +
#       geom_text(data = curr_dt,
#                 aes(x = as.factor(id),
#                     y = Freq,
#                     label = Diagnosis,
#                     hjust = -0.1,
#                     angle = 90),
#                 colour = "black",
#                 inherit.aes = F,
#                 size = 2) +
#       ylab("Frequency") +
#       xlab("") +
#       ggtitle(age_grp_label[i]) +
#       scale_y_continuous(limits = c(0, max(curr_dt$Freq, na.rm = T) + 10000),
#                          expand = c(0, 0),
#                          labels = scales::comma) +
#       scale_fill_discrete(name = "CHC group",
#                           labels = levels(as.factor(chc_diagnoses$type))) +
#       theme(panel.background = element_blank(),
#             panel.grid = element_blank(),
#             axis.text.y = element_text(colour = "black"),
#             axis.text.x = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.title = element_text(colour = "black"),
#             axis.line = element_line(colour = "black"),
#             legend.position = "none")
#   } else {
# 
#     p_list[[i]] <- ggplot(data = curr_dt,
#                           aes(x = as.factor(id),
#                               y = Freq,
#                               fill = chc_code)) +
#       geom_bar(stat = "identity") +
#       geom_text(data = curr_dt,
#                 aes(x = as.factor(id),
#                     y = Freq,
#                     label = Diagnosis,
#                     hjust = -0.1,
#                     angle = 90),
#                 colour = "black",
#                 inherit.aes = F,
#                 size = 2) +
#       ylab("Frequency") +
#       xlab("") +
#       ggtitle(age_grp_label[i]) +
#       scale_y_continuous(limits = c(0, max(curr_dt$Freq, na.rm = T) + 10000),
#                          expand = c(0, 0),
#                          labels = scales::comma) +
#       scale_fill_discrete(name = "CHC group",
#                           labels = levels(as.factor(chc_diagnoses$type))) +
#       theme(panel.background = element_blank(),
#             panel.grid = element_blank(),
#             axis.text.y = element_text(colour = "black"),
#             axis.text.x = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.title = element_text(colour = "black"),
#             axis.line = element_line(colour = "black"),
#             legend.position = "bottom") +
#       guides(fill = guide_legend(nrow = 3, byrow = T))
#   }
# 
# 
# }
# 
# tiff("1_CHC_CUMUL/outputs/predom_codes/fig/top10_per_group_lt5.tiff",
#      width = 8, height = 12, units = "in", res = 300)
# p_list[[1]]
# dev.off()
# 
# tiff("1_CHC_CUMUL/outputs/predom_codes/fig/top10_per_group_5-10.tiff",
#      width = 8, height = 12, units = "in", res = 300)
# p_list[[2]]
# dev.off()
# 
# tiff("1_CHC_CUMUL/outputs/predom_codes/fig/top10_per_group_11-15.tiff",
#      width = 8, height = 12, units = "in", res = 300)
# p_list[[3]]
# dev.off()

rm(list = ls()); gc()
