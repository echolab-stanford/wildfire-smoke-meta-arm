# 0. Libraries ------

library(tidyverse)
library(mixmeta)
library(haven)
library(metafor)

# 1. Data -------

wf_ed <- read_csv("~/Documents/WF_Meta_Repo/wf_ed_studies.csv")

# 2. Analysis ------

# Respiratory ------
wf_ed_resp <-
  wf_ed %>% filter(Outcome == "Respiratory") %>%
  dplyr::select(Study, logrr, logvar)


wf_ed_resp_rma_pm25 <-
  rma(
    logrr,
    logvar ^ 2,
    slab = Study,
    # random = ~1|Study/est_id,
    data = wf_ed_resp,
    method = "REML",
    verbose = T
  )

wf_ed_resp_rma_pm25

forest(wf_ed_resp_rma_pm25,showweights = T,digits = 4,header=T)


wf_ed_cvd_rma_pm25 <-
  rma(
    logrr,
    logvar ^ 2,
    slab = Study,
    # random = ~1|Study/est_id,
    data = wf_ed_cvd,
    method = "REML",
    verbose = T
  )

wf_ed_cvd_rma_pm25

forest(wf_ed_cvd_rma_pm25,showweights = T,digits = 4,header=T)


