# Libraries ------

library(tidyverse)
library(metafor)
library(readxl)

# Data ------

wf_mortality_studies <- read_csv("~/Documents/WF_Meta_Repo/wf_mortality.csv")

# Analysis ------

wf_mortality_rma_pm25_0 <-
  rma(
    logrr,
    logvar ^ 2,
    slab = Study,
    # random = ~1|Study/est_id,
    data = wf_mortality_studies,
    method = "REML",
    verbose = T
  )

wf_mortality_rma_pm25_0
summary(wf_mortality_rma_pm25_0)
regtest(wf_mortality_rma_pm25_0)

forest(
  wf_mortality_rma_pm25_0,
  showweights = T,
  header = T,
  digits = 4,
  xlab = "Percent change"
)

