### Forest Plots

library(readxl)
library(esc)
library(metafor)
library(tidyverse)
library(janitor)


# Complications
dat_comp <- ...

comp_ef <- esc_bin_prop(
  prop2event = dat_comp$Group1_proportion, 
  prop1event = dat_comp$Group2_proportion,
  grp2n = dat_comp$n_Group1, 
  grp1n = dat_comp$n_Group2,
  es.type = "or"
)

dat_comp$ef <- comp_ef$es
dat_comp$ef_se <- comp_ef$se

comp_ma <- rma(yi = ef,vi =  ef_se, data = dat_comp, method = "REML")

summary(comp_ma)

forest(
  x = comp_ma,
  slab = paste0(dat_comp$Study, " (", dat_comp$Group2, "-", dat_comp$Group1, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)


## Recurrent Instability

dat_insta <- ...

insta_ef <- esc_bin_prop(
  prop2event = dat_insta$Group1_proportion, 
  prop1event = dat_insta$Group2_proportion,
  grp2n = dat_insta$n_Group1, 
  grp1n = dat_insta$n_Group2,
  es.type = "or"
)

dat_insta$ef <- insta_ef$es
dat_insta$ef_se <- insta_ef$se

insta_ma <- rma(yi = ef, vi =  ef_se, data = dat_insta, method = "REML")

summary(insta_ma)

forest(
  x = insta_ma,
  slab = paste0(dat_insta$Study, " (", dat_insta$Group2, "-", dat_insta$Group1, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)


## Graft Union, not enough data  

dat_graft <-... 

graft_ef <- esc_bin_prop(
  prop2event = dat_graft$Group1_proportion, 
  prop1event = dat_graft$Group2_proportion,
  grp2n = dat_graft$n_Group1, 
  grp1n = dat_graft$n_Group2,
  es.type = "or"
)

dat_graft$ef <- graft_ef$es
dat_graft$ef_se <- graft_ef$se

graft_ma <- rma(yi = ef,vi =  ef_se, data = dat_graft, method = "REML")

summary(graft_ma)

forest(
  x = graft_ma,
  slab = paste0(dat_graft$Study, " (", dat_graft$Group2, "-", dat_graft$Group1, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)

## Reoperation, not enough data 
dat_reop <- ...

reop_ef <- esc_bin_prop(
  prop2event = dat_reop$Group1_proportion, 
  prop1event = dat_reop$Group2_proportion,
  grp2n = dat_reop$n_Group1, 
  grp1n = dat_reop$n_Group2,
  es.type = "or"
)

dat_reop$ef <- reop_ef$es
dat_reop$ef_se <- reop_ef$se

reop_ma <- rma(yi = ef,vi =  ef_se, data = dat_reop, method = "REML")

summary(reop_ma)

forest(
  x = reop_ma,
  slab = paste0(dat_reop$Study, " (", dat_reop$Group2, "-", dat_reop$Group1, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)

## Modified R. Score
dat_rowe <- ...

rowe_ef <- esc_mean_sd(
  grp1m = dat_rowe$Group2_Mean, 
  grp2m = dat_rowe$Group1_Mean, 
  grp1sd = dat_rowe$Group2_SD, 
  grp2sd = dat_rowe$Group1_SD, 
  grp1n = dat_rowe$n_Group2, 
  grp2n = dat_rowe$n_Group1,
  es.type = "d"
)

dat_rowe$ef <- rowe_ef$es
dat_rowe$ef_se <- rowe_ef$se

rowe_ma <- rma(yi = ef, vi =  ef_se, data = dat_rowe, method = "REML")

summary(rowe_ma)

forest(
  x = rowe_ma,
  slab = paste0(dat_rowe$Study, " (", dat_rowe$Group2, "-", dat_rowe$Group1, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)


# Walch-Duplay Score 
dat_wal <- ...
dat_wal <- dat_wal %>% janitor::clean_names()

wal_ef <- esc_mean_sd(
  grp1m = dat_wal$group2_mean, 
  grp2m = dat_wal$group1_mean, 
  grp1sd = dat_wal$group2_sd, 
  grp2sd = dat_wal$group1_sd, 
  grp1n = dat_wal$n_group2, 
  grp2n = dat_wal$n_group_1,
  es.type = "d"
)

dat_wal$ef <- wal_ef$es
dat_wal$ef_se <- wal_ef$se

wal_ma <- rma(yi = ef,vi =  ef_se, data = dat_wal, method = "REML")

summary(wal_ma)

forest(
  x = wal_ma,
  slab = paste0(dat_wal$study, " (", dat_wal$group2, "-", dat_wal$group_1, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)

## Vertical Graft Position 
dat_vert <- ...

vert_ef <- esc_bin_prop(
  prop2event = dat_vert$Group1_proportion, 
  prop1event = dat_vert$Group2_proportion,
  grp2n = dat_vert$n_Group1, 
  grp1n = dat_vert$n_Group2,
  es.type = "or"
)

dat_vert$ef <- vert_ef$es
dat_vert$ef_se <- vert_ef$se

vert_ma <- rma(yi = ef, vi =  ef_se, data = dat_vert, method = "REML")

summary(vert_ma)


forest(
  x = vert_ma,
  slab = paste0(dat_vert$Study, " (", dat_vert$Group2, "-", dat_vert$`Group 1`, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)


## Horizontal Graft Position 
dat_hori <- ...

hori_ef <- esc_bin_prop(
  prop2event = dat_hori$Group1_proportion, 
  prop1event = dat_hori$Group2_proportion,
  grp2n = dat_hori$n_Group1, 
  grp1n = dat_hori$n_Group2,
  es.type = "or"
)

dat_hori$ef <- hori_ef$es
dat_hori$ef_se <- hori_ef$se

hori_ma <- rma(yi = ef, vi =  ef_se, data = dat_hori, method = "REML")

summary(hori_ma)

forest(
  x = hori_ma,
  slab = paste0(dat_hori$Study, " (", dat_hori$Group2, "-", dat_hori$`Group 1`, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)

# text(-16, -1, pos=4, cex=.8, bquote(paste("RE Model (Q = ",
#                                             .(formatC(hori_ma$QE, digits=2, format="f")), ", df = ", .(hori_ma$k - hori_ma$p),
#                                             ", p = ", .(formatC(hori_ma$QEp, digits=2, format="f")), "; ", I^2, " = ",
#                                             .(formatC(hori_ma$I2, digits=1, format="f")), "%)")))


## VAS Pain 
dat_vas <- ...

vas_ef <- esc_mean_sd(
  grp1m = dat_vas$Group2_Mean, 
  grp2m = dat_vas$`Group1 _Mean`, 
  grp1sd = dat_vas$Group2_SD, 
  grp2sd = dat_vas$Group1_SD, 
  grp1n = dat_vas$n_Group2, 
  grp2n = dat_vas$`n_Group 1`,
  es.type = "d"
)

dat_vas$ef <- vas_ef$es
dat_vas$ef_se <- vas_ef$se

vas_ma <- rma(yi = ef, vi =  ef_se, data = dat_vas, method = "REML")
summary(vas_ma)

forest(
  x = vas_ma,
  slab = paste0(dat_vas$Study, " (", dat_vas$Group2, "-", dat_vas$`Group 1`, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Effect Size",
  mlab=""
)

