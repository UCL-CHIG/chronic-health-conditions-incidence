# *******************************************
# Matthew Jay. matthew.jay@ucl.ac.uk
# Creates new plots (new colour schemes) following peer review
# *******************************************

setwd("[[path omitted]")
library(data.table)
library(ggplot2)

dt <- fread("13. km-plots-el-scheme-8-all-birth-cohorts-underlying-data.csv")

dt[, academicyearofbirth := gsub("academicyearofbirth=", "", cohort)]
dt[, cohort := NULL]
dt[, cumul_inc_pc := cumul_inc * 100]


d <- data.table(expand.grid(h=seq(0,0.95,0.05), s=seq(0,0.95,0.05), v=seq(0,1,0.2)))
d <- d[v == 1 & h == 0]
d <- d[s %in% seq(0.15, 1, 0.05)]

# Figure 1 ------

tiff("FIGURE 1 NEW.tiff", width = 6, height = 4, units = "in", res = 360)
ggplot(aes(x = time,
           y = cumul_inc_pc,
           colour = academicyearofbirth),
       data = dt) +
  geom_line(size = 1.1) +
  xlab("Age (years)") +
  ylab("Cumulative incidence (%)") +
  scale_x_continuous(breaks = c((0:16)*12),
                     labels = c(0:16),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 25)) +
  scale_color_manual(values = hsv(d$h, d$s, d$v),
                     name = "Birth cohort\n(academic year ending)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 14),
        legend.key = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 10,
             linetype = "dashed") +
  geom_hline(yintercept = 20,
             linetype = "dashed") +
  geom_hline(yintercept = 25,
             linetype = "dashed")
dev.off()


# Figure 2 ----------------------------------------------------------------

dt <- fread("16. km-plots-chc-subtypes-el-scheme-8-all-cohorts-underlying-data.csv")

dt[, academicyearofbirth := gsub("academicyearofbirth_comb=", "", cohort)]
dt[, cohort := NULL]
dt[, cumul_inc_pc := cumul_inc * 100]

dt[type == "canbld", type := "Cancer/blood"]
dt[type == "cardio", type := "Cardiovascular"]
dt[type == "menlth", type := "Mental health/behavioural"]
dt[type == "metetc", type := "Metabolic, &c"]
dt[type == "mskskn", type := "Musculoskeletal/skin"]
dt[type == "neurol", type := "Neurological"]
dt[type == "nonspe", type := "Non-specific codes"]
dt[type == "respir", type := "Respiratory"]

tiff("FIGURE 2 NEW.tiff", width = 12, height = 8, units = "in", res = 360)
ggplot(aes(x = time,
           y = cumul_inc_pc,
           colour = academicyearofbirth),
       data = dt) +
  geom_line(size = 1.1) +
  xlab("Age (years)") +
  ylab("Cumulative incidence (%)") +
  scale_x_continuous(breaks = c((0:16)*12),
                     labels = c(0:16),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 12)) +
  scale_color_manual(values = hsv(d$h, d$s, d$v),
                     name = "Birth cohort\n(academic year)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 10),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  facet_wrap(~type)
dev.off()


# Supplementary Figrue S3 ------

dt <- fread("08. km-plots-all-el-schemes-b2004-underlying-data.csv")

dt[, cumul_inc_pc := cumul_inc * 100]

dt[, sensitivity_analysis := as.factor(sensitivity_analysis)]
dt[sensitivity_analysis == "8", sensitivity_analysis := "Main analysis"]

tiff("SUPPLEMENTARY FIGURE S3 NEW.tiff", width = 6, height = 4, units = "in", res = 360)
p <- ggplot(data = dt) +
  geom_line(aes(x = time,
                y = cumul_inc_pc,
                linetype = sensitivity_analysis,
                colour = sensitivity_analysis,
                linewidth = sensitivity_analysis))

plot_cols <- unique(ggplot_build(p)$data[[1]]$colour)
plot_lines <- unique(ggplot_build(p)$data[[1]]$linetype)

ggplot(data = dt) +
  geom_line(aes(x = time,
                y = cumul_inc_pc,
                linetype = sensitivity_analysis,
                colour = sensitivity_analysis,
                size = sensitivity_analysis)) +
  xlab("Age (years)") +
  ylab("Cumulative incidence (%)") +
  scale_size_manual(values = c(rep(1, 7), 1.8),
                    name = "Sensitivity Analysis") +
  scale_colour_manual(name = "Sensitivity Analysis",
                      values = plot_cols) +
  scale_linetype_manual(name = "Sensitivity Analysis",
                        values = plot_lines) +
  scale_x_continuous(breaks = c((0:16)*12),
                     labels = c(0:16),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 40)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 14),
        legend.key = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 10,
             linetype = "dashed") +
  geom_hline(yintercept = 20,
             linetype = "dashed") +
  geom_hline(yintercept = 25,
             linetype = "dashed")
dev.off()


# Supplementary Figrue S4 ------

dt <- fread("11. km-plots-chc-subtypes-all-el-schemes-b2003-b2004-underlying-data.csv")


dt[, cumul_inc_pc := cumul_inc * 100]

dt[type == "canbld", type := "Cancer/blood"]
dt[type == "cardio", type := "Cardiovascular"]
dt[type == "menlth", type := "Mental health/behavioural"]
dt[type == "metetc", type := "Metabolic, &c"]
dt[type == "mskskn", type := "Musculoskeletal/skin"]
dt[type == "neurol", type := "Neurological"]
dt[type == "nonspe", type := "Non-specific codes"]
dt[type == "respir", type := "Respiratory"]

dt[, el_scheme := as.factor(el_scheme)]
dt[el_scheme == "8", el_scheme := "Main analysis"]

tiff("SUPPLEMENTARY FIGURE S4 NEW.tiff", width = 12, height = 8, units = "in", res = 360)
ggplot(dt) +
  geom_line(aes(x = time,
                y = cumul_inc_pc,
                linetype = el_scheme,
                colour = el_scheme,
                size = el_scheme)) +
  xlab("Age (years)") +
  ylab("Cumulative incidence (%)") +
  scale_x_continuous(breaks = c((0:16)*12),
                     labels = c(0:16),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 12)) +
  scale_size_manual(values = c(rep(1, 7), 1.8),
                    name = "Sensitivity Analysis") +
  scale_colour_manual(name = "Sensitivity Analysis",
                      values = plot_cols) +
  scale_linetype_manual(name = "Sensitivity Analysis",
                        values = plot_lines) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 10),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  facet_wrap(~type)
dev.off()


# Figure 3 - ONS Method ---------------------------------------------------

dt <- fread("STATS19483/ons-any-chc-data.csv")

dt[, birth_cohort := as.factor(birth_cohort)]

d <- data.table(expand.grid(h=seq(0,0.95,0.05), s=seq(0,0.95,0.05), v=seq(0,1,0.2)))
d <- d[v == 1 & h == 0]
d <- d[s != 0]
#d <- d[s %in% seq(0.05, 1, 0.05)]

tiff("FIGURE 3 NEW.tiff", width = 6, height = 5, units = "in", res = 360)
ggplot(aes(x = age,
           y = cum_inc_p,
           colour = birth_cohort),
       data = dt) +
  geom_line(size = 1.1) +
  xlab("By age (years)") +
  ylab("Cumulative incidence (%)") +
  scale_x_continuous(breaks = 1:16,
                     labels = 1:16,
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 30)) +
  scale_color_manual(values = hsv(d$h, d$s, d$v),
                     name = "Birth cohort\n(financial year)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 14),
        legend.key = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 10,
             linetype = "dashed") +
  geom_hline(yintercept = 20,
             linetype = "dashed") +
  geom_hline(yintercept = 25,
             linetype = "dashed")
dev.off()


# SUPLEMENTARY FIGURE S6 --------------------------------------------------

dt <- fread("STATS19483/ons-sub-types-data.csv")

dt[, birth_cohort := as.factor(birth_cohort)]

dt[chc_type == "canbld", chc_type := "Cancer/blood"]
dt[chc_type == "cardio", chc_type := "Cardiovascular"]
dt[chc_type == "menlth", chc_type := "Mental health/behavioural"]
dt[chc_type == "metetc", chc_type := "Metabolic, &c"]
dt[chc_type == "mskskn", chc_type := "Musculoskeletal/skin"]
dt[chc_type == "neurol", chc_type := "Neurological"]
dt[chc_type == "nonspe", chc_type := "Non-specific codes"]
dt[chc_type == "respir", chc_type := "Respiratory"]

tiff("SUPPLEMENTARY FIGURE S6 NEW.tiff", width = 12, height = 8, units = "in", res = 360)
ggplot(aes(x = age,
           y = cum_inc_p,
           colour = birth_cohort),
       data = dt) +
  geom_line(size = 1.1) +
  xlab("By age (years)") +
  ylab("Cumulative incidence (%)") +
  scale_x_continuous(breaks = 1:16,
                     labels = 1:16,
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 12)) +
  scale_color_manual(values = hsv(d$h, d$s, d$v),
                     name = "Birth cohort") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(colour = "black",
                                 size = 10),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  facet_wrap(~chc_type)
dev.off()

rm(list = ls())
