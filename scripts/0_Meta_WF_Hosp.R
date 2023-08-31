# 0. Libraries ------

library(tidyverse)
library(mixmeta)
library(haven)
library(readxl)
library(metafor)
library(splines)

# 1. Data -------

wf_hosp <- read_csv("~/Documents/WF_Meta_Repo/wf_hosp_studies.csv")

wf_hosp_resp_pm25 <-
  wf_hosp %>%
  filter(Outcome == "Respiratory") %>%
  dplyr::select(Study, logrr, logvar)

wf_hosp_cvd_pm25 <-
  wf_hosp %>%
  filter(Outcome == "Cardiovascular")  %>%
  dplyr::select(Study, logrr, logvar)

# 2. Analysis ------

# Respiratory ------
# Pool Martin 2013 estimates
wf_hosp_resp_rma_pm25_Martin <-
  rma(
    logrr,
    logvar ^ 2,
    data = wf_hosp_resp_pm25 %>%
      filter(Study == "Martin 2013"),
    method = "REML"
  )

# Pool Jiang 2023 estimates
wf_hosp_resp_rma_pm25_Jiang <-
  rma(
    logrr,
    logvar ^ 2,
    data = wf_hosp_resp_pm25 %>%
      filter(Study == "Jiang 2023"),
    method = "REML"
  )

# Write pooled Jiang 2023 to new row

wf_hosp_resp_pm25_1 <-
  wf_hosp_resp_pm25 %>%
  filter(Study != "Martin 2013") %>%
  filter(Study != "Jiang 2023") %>%
  filter(Study != "Reid 2016")

wf_hosp_resp_pm25_1[9, ] <-
  list("Martin 2013",
       0.0015,
       0.0004)

wf_hosp_resp_pm25_1[10, ] <-
  list("Jiang 2023",
       0.0626,
       0.0394)

wf_hosp_resp_rma_pm25 <-
  rma(logrr,
      logvar ^ 2,
      slab = Study,
      # random= ~1 | Study / est_id,
      data = wf_hosp_resp_pm25_1,
      method = "REML")

wf_hosp_resp_rma_pm25
forest(wf_hosp_resp_rma_pm25,showweights = T,digits = 4,header=T)


# Cardiovascular --------

wf_hosp_cvd_rma_pm25_Jiang <-
  rma(logrr,
      logvar ^ 2,
      slab = Study,
      # random= ~1 | Study / est_id,
      data = wf_hosp_cvd_pm25 %>%
        filter(Study == "Jiang 2023"))

wf_hosp_cvd_pm25_1 <-
  wf_hosp_cvd_pm25 %>%
  filter(Study != "Jiang 2023")

# Write pooled Jiang 2023 to new row
wf_hosp_cvd_rma_pm25_Jiang
wf_hosp_cvd_pm25_1[9, ] <-
  list("Jiang 2023",
       0.0750,
       0.0503)


wf_hosp_cvd_rma_pm25 <-
  rma(logrr,
      logvar ^ 2,
      slab = Study,
      # random= ~1 | Study / est_id,
      data = wf_hosp_cvd_pm25_1,
      method = "REML")

wf_hosp_cvd_rma_pm25
forest(wf_hosp_cvd_rma_pm25,showweights = T,digits = 4,header=T)

