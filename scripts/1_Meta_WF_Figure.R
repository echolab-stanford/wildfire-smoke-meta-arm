# NB. Formatting produced here differs from the published paper because of 
# tweaks made by the journal production staff. 

library(tidyverse)
library(ggtext)
library(cowplot)
library(readxl)
library(jpeg)


plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}


# ASSORTED (RANDOM) DATA ------

myjpg <- jpeg::readJPEG("~/Documents/WF_Meta_Repo/funnel_legend.jpg")

study_region <- read_csv("~/Documents/WF_Meta_Repo/wf_meta_study_region.csv")

# RUN META-ANALYSIS CODE ------

source("~/Documents/0_Meta_WF_ED.R")
source("~/Documents/0_Meta_WF_Hosp.R")
source("~/Documents/0_Meta_WF_Mortality.R")

# PROCESSING META-ANALYSIS RESULTS AND STUDY ESTIMATES -------

# All cause mortality ----

pooled_wf_mortality_pm25 <- 
  data.frame(matrix(NA, 1, 4))
colnames(pooled_wf_mortality_pm25) <- c("Study", "Estimate", "Low", "High")
pooled_wf_mortality_pm25[1,] <- 
  c("Pooled", 
    coef(wf_mortality_rma_pm25_0),
    wf_mortality_rma_pm25_0$ci.lb,
    wf_mortality_rma_pm25_0$ci.ub)

wf_mortality_pm25 <-
  wf_mortality_rma_pm25_0$data %>%
  bind_cols(
    weights(wf_mortality_rma_pm25_0) %>%
      as_tibble()
  ) %>%
  rename(Estimate = logrr,
         Weight = value) %>%
  mutate(Low = Estimate - (1.96*logvar),
         High = Estimate + (1.96*logvar),
         Year = str_sub(Study, nchar(Study)-4, nchar(Study))) %>%
  mutate(Type = "Study",
         Year = as.numeric(Year)) %>%
  dplyr::select(Type, Study, Year, Estimate, Low, High, Weight) %>% 
  bind_rows(
    pooled_wf_mortality_pm25 %>%
      mutate(Estimate = as.numeric(Estimate),
             Low = as.numeric(Low),
             High = as.numeric(High)) %>%
      mutate(Type = "Pooled",
             Year = 2030,
             Weight = 100)
  ) %>%
  mutate(Study = forcats::fct_reorder(Study, desc(Year))) %>%
  mutate(
    estimate_label = paste0(
      round(Estimate*100, digits=2), " (",
      round(Low*100, digits=2), " to ",
      round(High*100, digits=2), ")"
    )) %>%
  mutate(
    Weight_lab = paste0(round(Weight, digits=2))
  ) %>% 
  mutate(Outcome = "All-cause mortality") 

# Respiratory Hosp ------

pooled_wf_resp_hosp_pm25 <- 
  data.frame(matrix(NA, 1, 4))
colnames(pooled_wf_resp_hosp_pm25) <- c("Study", "Estimate", "Low", "High")
pooled_wf_resp_hosp_pm25[1,] <- 
  c("Pooled", 
    coef(wf_hosp_resp_rma_pm25),
    wf_hosp_resp_rma_pm25$ci.lb,
    wf_hosp_resp_rma_pm25$ci.ub)

wf_resp_hosp_pm25 <-
  wf_hosp_resp_rma_pm25$data %>%
  bind_cols(
    weights(wf_hosp_resp_rma_pm25) %>%
      as_tibble()
  ) %>%
  mutate(
    Estimate = logrr,
    Low = logrr - (1.96*logvar),
    High = logrr + (1.96*logvar),
    Year = str_sub(Study, nchar(Study)-3, nchar(Study))
    ) %>%
  mutate(Type = "Study",
         Year = as.numeric(Year)) %>%
  rename(Weight = value) %>% 
  dplyr::select(Type, Study, Year, Estimate, Low, High, Weight) %>% 
  bind_rows(
    pooled_wf_resp_hosp_pm25  %>%
      mutate(Estimate = as.numeric(Estimate),
             Low = as.numeric(Low),
             High = as.numeric(High)) %>%
      mutate(Type = "Pooled",
             Year = 2030,
             Weight = 100)
  ) %>%
  mutate(Study = forcats::fct_reorder(Study, desc(Year))) %>%
  mutate(
    estimate_label = paste0(
      round(Estimate*100, digits=2), " (",
      round(Low*100, digits=2), " to ",
      round(High*100, digits=2), ")"
    )) %>%
  mutate(
    Weight_lab = paste0(round(Weight, digits=2)),
    Outcome = "Respiratory hospitalizations"
                        )

# Respiratory ED ------


pooled_wf_resp_ed_pm25 <- 
  data.frame(matrix(NA, 1, 4))
colnames(pooled_wf_resp_ed_pm25) <- c("Study", "Estimate", "Low", "High")
pooled_wf_resp_ed_pm25[1,] <- 
  c("Pooled", 
    coef(wf_ed_resp_rma_pm25),
    wf_ed_resp_rma_pm25$ci.lb,
    wf_ed_resp_rma_pm25$ci.ub)

wf_resp_ed_pm25 <-
  wf_ed_resp_rma_pm25$data %>%
  bind_cols(
    weights(wf_ed_resp_rma_pm25) %>%
      as_tibble()
  ) %>%
  rename(Estimate = logrr,
         Weight = value) %>%
  mutate(Low = Estimate - (1.96*logvar),
         High = Estimate + (1.96*logvar),
         Year = str_sub(Study, nchar(Study)-3, nchar(Study))) %>%
  mutate(Type = "Study",
         Year = as.numeric(Year)) %>%
  bind_rows(
    pooled_wf_resp_ed_pm25 %>%
      mutate(Estimate = as.numeric(Estimate),
             Low = as.numeric(Low),
             High = as.numeric(High)) %>%
      mutate(Type = "Pooled",
             Year=2030,
             Weight = 100)
  ) %>%
  mutate(Study = forcats::fct_reorder(Study, desc(Year))) %>%
  mutate(
    estimate_label = paste0(
      round(Estimate*100, digits=2), " (",
      round(Low*100, digits=2), " to ",
      round(High*100, digits=2), ")"
    )) %>%
  mutate(
    Weight_lab = paste0(round(Weight, digits=2))
  ) %>% 
  mutate(Outcome = "Respiratory ED visits")  %>% 
  dplyr::select(-logvar)

# CVD Hosp ------

pooled_wf_cvd_hosp_pm25 <- 
  data.frame(matrix(NA, 1, 4))
colnames(pooled_wf_cvd_hosp_pm25) <- c("Study", "Estimate", "Low", "High")
pooled_wf_cvd_hosp_pm25[1,] <- 
  c("Pooled", 
    coef(wf_hosp_cvd_rma_pm25),
    wf_hosp_cvd_rma_pm25$ci.lb,
    wf_hosp_cvd_rma_pm25$ci.ub)

wf_cvd_hosp_pm25 <-
  wf_hosp_cvd_rma_pm25$data %>%
  bind_cols(
    weights(wf_hosp_cvd_rma_pm25) %>%
      as_tibble()
  ) %>%
  mutate(
    Estimate = logrr,
    Low = logrr - (1.96*logvar),
    High = logrr + (1.96*logvar),
    Year = str_sub(Study, nchar(Study)-3, nchar(Study))
  ) %>%
  mutate(Type = "Study",
         Year = as.numeric(Year)) %>%
  rename(Weight = value) %>% 
  dplyr::select(Type, Study, Year, Estimate, Low, High, Weight) %>% 
  bind_rows(
    pooled_wf_cvd_hosp_pm25  %>%
      mutate(Estimate = as.numeric(Estimate),
             Low = as.numeric(Low),
             High = as.numeric(High)) %>%
      mutate(Type = "Pooled",
             Year = 2030,
             Weight = 100)
  ) %>%
  mutate(Study = forcats::fct_reorder(Study, desc(Year))) %>%
  mutate(
    estimate_label = paste0(
      round(Estimate*100, digits=2), " (",
      round(Low*100, digits=2), " to ",
      round(High*100, digits=2), ")"
    )) %>%
  mutate(
    Weight_lab = paste0(round(Weight, digits=2)),
    Outcome = "Cardiovascular hospitalizations"
  )


# CVD ED ------

pooled_wf_cvd_ed_pm25 <- 
  data.frame(matrix(NA, 1, 4))
colnames(pooled_wf_cvd_ed_pm25) <- c("Study", "Estimate", "Low", "High")
pooled_wf_cvd_ed_pm25[1,] <- 
  c("Pooled", 
    coef(wf_ed_cvd_rma_pm25),
    wf_ed_cvd_rma_pm25$ci.lb,
    wf_ed_cvd_rma_pm25$ci.ub)

wf_cvd_ed_pm25 <-
  wf_ed_cvd_rma_pm25$data %>%
  bind_cols(
    weights(wf_ed_cvd_rma_pm25) %>%
      as_tibble()
  ) %>%
  rename(Estimate = logrr,
         Weight = value) %>%
  mutate(Low = Estimate - (1.96*logvar),
         High = Estimate + (1.96*logvar),
         Year = str_sub(Study, nchar(Study)-3, nchar(Study))) %>%
  mutate(Type = "Study",
         Year = as.numeric(Year)) %>%
  bind_rows(
    pooled_wf_cvd_ed_pm25 %>%
      mutate(Estimate = as.numeric(Estimate),
             Low = as.numeric(Low),
             High = as.numeric(High)) %>%
      mutate(Type = "Pooled",
             Weight = 100,
             Year=2030)
  ) %>%
  mutate(Study = forcats::fct_reorder(Study, desc(Year))) %>%
  mutate(
    estimate_label = paste0(
      round(Estimate*100, digits=2), " (",
      round(Low*100, digits=2), " to ",
      round(High*100, digits=2), ")"
    )) %>%
  mutate(
    Weight_lab = paste0(round(Weight, digits=2))
  ) %>% 
  mutate(Outcome = "Cardiovascular ED visits")  %>% 
  dplyr::select(-logvar)

# COMBINE DATA FRAMES -------

combined_wf_meta_df <-
  wf_mortality_pm25 %>%
  bind_rows(
    wf_cvd_ed_pm25,
    wf_cvd_hosp_pm25,
    wf_resp_ed_pm25,
    wf_resp_hosp_pm25
  ) %>% 
  left_join(study_region %>% 
              distinct(),by=c("Study", "Outcome"), multiple="all") 

# write_csv(combined_wf_meta_df, "~/Downloads/combined_wf_meta_df.csv")

# MAKE FIGURES ------

# Mortality -----

combined_wf_meta_df$Study <- 
  fct_reorder(combined_wf_meta_df$Study, 
              desc(combined_wf_meta_df$Year))

wf_mortality_forest_pm25_fig <- 
  ggplot(combined_wf_meta_df %>% 
           filter(Outcome=="All-cause mortality"),
         aes(y=Study, x=Estimate, xmin=Low, xmax=High, fill=Type)) +
  geom_vline(xintercept=0, linetype="dotted", color="grey10") +
  geom_pointrange(size=0.3,shape=21) + 
  scale_fill_manual(values=c("black", "white")) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-.0075, -.005, -.0025, 0, .0025, .005, .0075, .01, .0125),
                     expand=c(0,0)) + 
  coord_cartesian(xlim=c(-.0075, .0125)) +
  ggtitle("All-cause mortality") + 
  xlab("Change in risk (95% CI) per 1 ug/m3") + 
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    # axis.text.y = element_text(color="black", size=8),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank()
  ) 

# wf_mortality_forest_pm25_fig

wf_mortality_Region_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="All-cause mortality"),
            aes(y=Study, x=1, label=Region), size=3) +
  ggtitle("All-cause mortality") +
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    axis.text.y = element_text(color="black", size=9),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size=9, face="bold")
  ) 


wf_mortality_weights_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="All-cause mortality"),
            aes(y=Study, x=1, label=Weight_lab), size=3) +
  theme_void()

wf_mortality_coefs_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="All-cause mortality"),
            aes(y=Study, x=1.02, label=estimate_label), size=3) +
  theme_void()


wf_mortality_pm25_combined <- 
  plot_grid(
    wf_mortality_Region_fig,
    wf_mortality_forest_pm25_fig,
    wf_mortality_weights_fig,
    wf_mortality_coefs_fig,
    nrow=1,
    align="h",
    rel_widths=c(0.7, 1, 0.3, 0.3)
  ) 


# Resp hosp -----

wf_resp_hosp_pm25_fig <- 
  ggplot(combined_wf_meta_df %>% 
           filter(Outcome=="Respiratory hospitalizations"),
         aes(y=Study, x=Estimate, xmin=Low, xmax=High, fill=Type)) +
  geom_vline(xintercept=0, linetype="dotted", color="grey10") +
  geom_pointrange(size=0.3,shape=21) + 
  scale_fill_manual(values=c("black", "white")) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-.0075, -.005, -.0025, 0, .0025, .005, .0075, .01, .0125),
                     expand=c(0,0)) + 
  coord_cartesian(xlim=c(-.0075, .0125)) +
  # ggtitle("All-cause mortality") + 
  # xlab("Change in risk (95% CI) per 1 ug/m3") + 
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    # axis.text.y = element_text(color="black", size=8),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank()
  ) 

# wf_mortality_forest_pm25_fig

wf_resp_hosp_Region_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Respiratory hospitalizations"),
            aes(y=Study, x=1, label=Region), size=3) +
  ggtitle("Respiratory hospitalizations") +
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    axis.text.y = element_text(color="black", size=9),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size=9, face="bold")
  ) 


wf_resp_hosp_weights_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Respiratory hospitalizations"),
            aes(y=Study, x=1, label=Weight_lab), size=3) +
  theme_void()

wf_resp_hosp_coefs_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Respiratory hospitalizations"),
            aes(y=Study, x=1, label=estimate_label), size=3) +
  theme_void()


wf_resp_hosp_pm25_combined <- 
  plot_grid(
    wf_resp_hosp_Region_fig,
    wf_resp_hosp_pm25_fig,
    wf_resp_hosp_weights_fig,
    wf_resp_hosp_coefs_fig,
    nrow=1,
    align="h",
    rel_widths=c(0.7, 1, 0.3, 0.3)
  ) 

wf_resp_hosp_pm25_combined



# cvd hosp -----

wf_cvd_hosp_pm25_fig <- 
  ggplot(combined_wf_meta_df %>% 
           filter(Outcome=="Cardiovascular hospitalizations"),
         aes(y=Study, x=Estimate, xmin=Low, xmax=High, fill=Type)) +
  geom_vline(xintercept=0, linetype="dotted", color="grey10") +
  geom_pointrange(size=0.3,shape=21) + 
  scale_fill_manual(values=c("black", "white")) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-.0075, -.005, -.0025, 0, .0025, .005, .0075, .01, .0125),
                     expand=c(0,0)) + 
  coord_cartesian(xlim=c(-.0075, .0125)) +
  # ggtitle("All-cause mortality") + 
  # xlab("Change in risk (95% CI) per 1 ug/m3") + 
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    # axis.text.y = element_text(color="black", size=8),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank()
  ) 

# wf_mortality_forest_pm25_fig

wf_cvd_hosp_Region_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Cardiovascular hospitalizations"),
            aes(y=Study, x=1, label=Region), size=3) +
  ggtitle("Cardiovascular hospitalizations") +
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    axis.text.y = element_text(color="black", size=9),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size=9, face="bold")
  ) 


wf_cvd_hosp_weights_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Cardiovascular hospitalizations"),
            aes(y=Study, x=1, label=Weight_lab), size=3) +
  theme_void()

wf_cvd_hosp_coefs_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Cardiovascular hospitalizations"),
            aes(y=Study, x=1, label=estimate_label), size=3) +
  theme_void()


wf_cvd_hosp_pm25_combined <- 
  plot_grid(
    wf_cvd_hosp_Region_fig,
    wf_cvd_hosp_pm25_fig,
    wf_cvd_hosp_weights_fig,
    wf_cvd_hosp_coefs_fig,
    nrow=1,
    align="h",
    rel_widths=c(0.7, 1, 0.3, 0.3)
  ) 

wf_cvd_hosp_pm25_combined

# Resp ed -----

wf_resp_ed_pm25_fig <- 
  ggplot(combined_wf_meta_df %>% 
           filter(Outcome=="Respiratory ED visits"),
         aes(y=Study, x=Estimate, xmin=Low, xmax=High, fill=Type)) +
  geom_vline(xintercept=0, linetype="dotted", color="grey10") +
  geom_pointrange(size=0.3,shape=21) + 
  scale_fill_manual(values=c("black", "white")) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-.0075, -.005, -.0025, 0, .0025, .005, .0075, .01, .0125),
                     expand=c(0,0)) + 
  coord_cartesian(xlim=c(-.0075, .0125)) +
  # ggtitle("All-cause mortality") + 
  xlab("Change in risk (95% CI) per 1 ug/m3") +
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    # axis.title.x = element_text(color="black", size=9),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    # axis.text.y = element_text(color="black", size=8),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    # axis.title.y = element_blank(),
    plot.title = element_blank()
  ) 

# wf_mortality_forest_pm25_fig

wf_resp_ed_Region_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Respiratory ED visits"),
            aes(y=Study, x=1, label=Region), size=3) +
  ggtitle("Respiratory ED visits") +
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size=9, face="bold"),
    axis.text.y = element_text(color="black")
  ) 


wf_resp_ed_weights_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Respiratory ED visits"),
            aes(y=Study, x=1, label=Weight_lab), size=3) +
  theme_void()

wf_resp_ed_coefs_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Respiratory ED visits"),
            aes(y=Study, x=1, label=estimate_label), size=3) +
  theme_void()


wf_resp_ed_pm25_combined <- 
  plot_grid(
    wf_resp_ed_Region_fig,
    wf_resp_ed_pm25_fig,
    wf_resp_ed_weights_fig,
    wf_resp_ed_coefs_fig,
    nrow=1,
    align="h",
    rel_widths=c(0.7, 1, 0.3, 0.3)
  ) 

wf_resp_ed_pm25_combined



# cvd ed -----

wf_cvd_ed_pm25_fig <- 
  ggplot(combined_wf_meta_df %>% 
           filter(Outcome=="Cardiovascular ED visits"),
         aes(y=Study, x=Estimate, xmin=Low, xmax=High, fill=Type)) +
  geom_vline(xintercept=0, linetype="dotted", color="grey10") +
  geom_pointrange(size=0.3,shape=21) + 
  scale_fill_manual(values=c("black", "white")) + 
  scale_x_continuous(labels=scales::percent_format(),
                     breaks=c(-.0075, -.005, -.0025, 0, .0025, .005, .0075, .01, .0125),
                     expand=c(0,0)) + 
  coord_cartesian(xlim=c(-.0075, .0125)) +
  xlab("Change in risk (95% CI) per 1 ug/m3") +
  theme_classic() + 
  theme(
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(size = 1,lineend = "square",colour = "black"),
    axis.ticks.length=unit(.15, "cm"),
    plot.background = element_blank(),
    axis.title.x = element_text(color="black", size=9),
    axis.text.x = element_text(color="black", size=8),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_line(linewidth=1),
    # axis.title.y = element_blank(),
    plot.title = element_blank()
  ) 

# wf_mortality_forest_pm25_fig

wf_cvd_ed_Region_fig <-
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Cardiovascular ED visits"),
            aes(y=Study, x=1, label=Region), size=3) +
  ggtitle("Cardiovascular ED visits") +
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    axis.text.y = element_text(color="black", size=9),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size=9, face="bold")
  ) 

wf_cvd_ed_weights_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Cardiovascular ED visits"),
            aes(y=Study, x=1, label=Weight_lab), size=3) +
  theme_void()

wf_cvd_ed_coefs_fig <- 
  ggplot() +
  geom_text(data=combined_wf_meta_df %>% 
              filter(Outcome=="Cardiovascular ED visits"),
            aes(y=Study, x=1, label=estimate_label), size=3) +
  theme_void()


wf_cvd_ed_pm25_combined <- 
  plot_grid(
    wf_cvd_ed_Region_fig,
    wf_cvd_ed_pm25_fig,
    wf_cvd_ed_weights_fig,
    wf_cvd_ed_coefs_fig,
    nrow=1,
    align="h",
    rel_widths=c(0.7, 1, 0.3, 0.3)
  ) 

wf_cvd_ed_pm25_combined


# Combining ------

wf_pm25_combined_fig <- 
  plot_grid(
    NULL,
    wf_mortality_pm25_combined,
    wf_resp_hosp_pm25_combined,
    wf_cvd_hosp_pm25_combined,
    wf_resp_ed_pm25_combined,
    wf_cvd_ed_pm25_combined,
    NULL,
    nrow=7,
    rel_heights=c(0.1, 0.8, 1.0, 0.9, 0.6, 0.6, 0.1),
    align="h"
  ) +
  annotate(
    "richtext", x=0.05, y=.99, label="<b>Study</b>", 
    size=4,fill=NA,label.color=NA
  ) +
  annotate(
    "richtext", x=0.2, y=.99, label="<b>Study sample</b>", 
    size=4,fill=NA,label.color=NA
  ) +
  annotate(
    "richtext", x=0.806, y=.99, label="<b>Weights (%)</b>", 
    size=4,fill=NA,label.color=NA
  ) +
  annotate(
    "richtext", x=0.9325, y=.99, label="<b>% risk (95% CI)</b>", 
    size=4,fill=NA,label.color=NA
  ) + 
  geom_hline(yintercept=.98) +
  
  # Mortality details
  annotate(
    "richtext", x=0.185, y=.795, label="Q(df=7) = 34.3, P<0.001<br>I^2=79.6%; H^2=10.4%", 
    size=2.5,fill=NA,label.color=NA
  ) +
  
  # ED - RESP
  annotate(
    "richtext", x=0.185, y=.19, label="Q(df=4) = 26.1, P<0001<br>I^2=89.2%; H^2=9.3%", 
    size=2.5,fill=NA,label.color=NA
  ) +
  
  # ED - CVD
  annotate(
    "richtext", x=0.185, y=.068, label="Q(df=3) = 7.2, P=0.065<br>I^2=59.9%; H^2=2.5%", 
    size=2.5,fill=NA,label.color=NA
  ) +
  # Hosp - resp
  annotate(
    "richtext", x=0.185, y=.554, label="Q(df=9) = 184.9, P<0.0001<br>I^2=93.8%; H^2=16.2%", 
    size=2.5,fill=NA,label.color=NA
  )  +

  # Hosp - cVD
  annotate(
    "richtext", x=0.185, y=.334, label="Q(df=8) = 14.5, P=0.070<br>I^2=43.5%; H^2=1.8%", 
    size=2.5,fill=NA,label.color=NA
  ) +
  
  geom_segment(
    aes(x=0.455, xend=0.455-.07, y=.02, yend=0.02),
    arrow = arrow(length = unit(0.15, "cm"))
  ) + 
  
  geom_segment(
    aes(x=0.49, xend=0.49+.07, y=.02, yend=0.02),
    arrow = arrow(length = unit(0.15, "cm"))
  ) + 
  
  annotate(
    "richtext", x=.40, y=.01, label="Favors reduced risk", 
    size=3,fill=NA,label.color=NA
  ) + 
  annotate(
    "richtext", x=.55, y=.01, label="Favors increased risk", 
    size=3,fill=NA,label.color=NA
  )
  

# wf_mortality_rma_pm25_0
# wf_ed_cvd_rma_pm25
# wf_ed_resp_rma_pm25
# wf_hosp_cvd_rma_pm25
# wf_hosp_resp_rma_pm25


# cowplot::ggsave2(
#   paste0("~/Documents/wf_pm25_combined_fig1_r1.pdf"),
#   plot = wf_pm25_combined_fig,
#   dpi = 300,
#   height = 300,
#   width = 300,
#   unit = "mm"
# )

# TESTS ------

# Egger's tests -------
regtest(wf_mortality_rma_pm25_0)
regtest(wf_ed_cvd_rma_pm25)
regtest(wf_ed_resp_rma_pm25)
regtest(wf_hosp_cvd_rma_pm25)
regtest(wf_hosp_resp_rma_pm25)


# Trim and fill -------

plot(
  metafor::trimfill(wf_mortality_rma_pm25_0), 
  legend=T, digits=4, showweights=T, header=T
  )
plot(metafor::trimfill(wf_ed_cvd_rma_pm25), legend=T, digits=4, showweights=T, header=T)
plot(metafor::trimfill(wf_ed_resp_rma_pm25), legend=T, digits=4, showweights=T, header=T)
plot(metafor::trimfill(wf_hosp_cvd_rma_pm25), legend=T, digits=4, showweights=T, header=T)
plot(metafor::trimfill(wf_hosp_resp_rma_pm25), legend=T, digits=4, showweights=T, header=T)




# funnel plots -------
par(mfrow = c(3, 2))

f_mort <- funnel.rma(wf_mortality_fe_pm25_0, level=c(90, 95, 99), 
                     shade=c("white", "gray55", "gray75"), 
                     refline=0, legend=F,main = "All-cause mortality")
f_ed_cvd <- funnel.rma(wf_ed_cvd_rma_pm25, level=c(90, 95, 99), 
                     shade=c("white", "gray55", "gray75"), 
                     refline=0, legend=F,main = "Cardiovascular hospitalizations")
f_ed_resp <- funnel.rma(wf_ed_resp_rma_pm25, level=c(90, 95, 99), 
                     shade=c("white", "gray55", "gray75"), 
                     refline=0, legend=F,main = "Respiratory hospitalizations")
f_hosp_cvd <- funnel.rma(wf_hosp_cvd_rma_pm25, level=c(90, 95, 99), 
                     shade=c("white", "gray55", "gray75"), 
                     refline=0, legend=F,main = "Cardiovascular ED visits")
f_hosp_resp <- funnel.rma(wf_hosp_resp_rma_pm25, level=c(90, 95, 99), 
                     shade=c("white", "gray55", "gray75"), 
                     refline=0, legend=F,main = "Respiratory ED visits")
plot_jpeg("~/Desktop/funnel_legend.jpg")




