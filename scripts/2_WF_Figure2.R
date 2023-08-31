library(tidyverse)
library(ggthemes)
library(cowplot)
library(MetBrewer)
library(ggsci)
library(ggtext)
library(patchwork)

# Data -----

hn_df <- read_rds("~/Downloads/wf-health-review-specification-figure-data (1).rds") %>% 
  mutate(percent = coef / brate)
# this data frame looks as follows
# columns: var, coef, outcome, polyDeg, lags, brate, comp, percent
# which are, in order: smoke bin, coefficient estimate, health outcome (all-cause, resp, etc.), polynomial degree, number of lags included, baseline rate of ED visits, comparison (lags vs. linearity)
smoke_dist <- read_rds("~/Downloads/smokePMdistribution.rds")
# this data frame is the distribution of smoke data, from Childs et al. 2022

# Process -----

hn_df_pm <- seq(0, 50, 1) %>% 
  as_tibble() %>% 
  rename(pm25 = value)

table(hn_df$outcome)

# Linearity ------

# All-cause ------
hn_df_all_linearity <- 
  hn_df %>% 
  filter(outcome=="all-cause") %>% 
  filter(comp=="linearity")

hn_df_all_linearity_1 <-
  hn_df_all_linearity %>% 
  filter(polyDeg==1) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_all_linearity_pm <- 
  expand_grid(hn_df_pm, hn_df_all_linearity_1) %>% 
  mutate(polyDeg=1)
  
for (i in 2:4) {
  
  hn_df_all_linearity_i <-
    hn_df_all_linearity %>% 
    filter(polyDeg==i) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_all_linearity_i_pm <- expand_grid(hn_df_pm, hn_df_all_linearity_i)
  
  hn_df_all_linearity_pm <-
    hn_df_all_linearity_pm %>% 
    bind_rows(hn_df_all_linearity_i_pm %>% 
                mutate(polyDeg=i))
  
}

hn_df_all_linearity_pm <-
  hn_df_all_linearity_pm %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    polyDeg = as.character(polyDeg)
  )


# CVD ------

hn_df_cvd_linearity <- 
  hn_df %>% filter(outcome=="cvd")  %>% 
  filter(comp=="linearity")

hn_df_cvd_linearity_1 <-
  hn_df_cvd_linearity %>% 
  filter(polyDeg==1) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_cvd_linearity_pm <- 
  expand_grid(hn_df_pm, hn_df_cvd_linearity_1) %>% 
  mutate(polyDeg=1)

for (i in 2:4) {
  
  hn_df_cvd_linearity_i <-
    hn_df_cvd_linearity %>% 
    filter(polyDeg==i) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_cvd_linearity_i_pm <- expand_grid(hn_df_pm, hn_df_cvd_linearity_i)
  
  hn_df_cvd_linearity_pm <-
    hn_df_cvd_linearity_pm %>% 
    bind_rows(hn_df_cvd_linearity_i_pm %>% 
                mutate(polyDeg=i))
  
}

hn_df_cvd_linearity_pm <-
  hn_df_cvd_linearity_pm %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    polyDeg = as.character(polyDeg)
  )


# Resp ------

hn_df_resp_linearity <- 
  hn_df %>% filter(outcome=="resp") %>% 
  filter(comp=="linearity")

hn_df_resp_linearity_1 <-
  hn_df_resp_linearity %>% 
  filter(polyDeg==1) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_resp_linearity_pm <- 
  expand_grid(hn_df_pm, hn_df_resp_linearity_1) %>% 
  mutate(polyDeg=1)

for (i in 2:4) {
  
  hn_df_resp_linearity_i <-
    hn_df_resp_linearity %>% 
    filter(polyDeg==i) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_resp_linearity_i_pm <- expand_grid(hn_df_pm, hn_df_resp_linearity_i)
  
  hn_df_resp_linearity_pm <-
    hn_df_resp_linearity_pm %>% 
    bind_rows(hn_df_resp_linearity_i_pm %>% 
                mutate(polyDeg=i))
  
}

hn_df_resp_linearity_pm <-
  hn_df_resp_linearity_pm %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    polyDeg = as.character(polyDeg)
  )

# Asthma -----

hn_df_asthma_linearity  <- 
  hn_df %>% filter(outcome=="resp-asthma") %>% 
  filter(comp=="linearity")

hn_df_asthma_linearity_1 <-
  hn_df_asthma_linearity %>% 
  filter(polyDeg==1) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_asthma_linearity_pm <- 
  expand_grid(hn_df_pm, hn_df_asthma_linearity_1) %>% 
  mutate(polyDeg=1)

for (i in 2:4) {
  
  hn_df_asthma_linearity_i <-
    hn_df_asthma_linearity %>% 
    filter(polyDeg==i) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_asthma_linearity_i_pm <- expand_grid(hn_df_pm, hn_df_asthma_linearity_i)
  
  hn_df_asthma_linearity_pm <-
    hn_df_asthma_linearity_pm %>% 
    bind_rows(hn_df_asthma_linearity_i_pm %>% 
                mutate(polyDeg=i))
  
}

hn_df_asthma_linearity_pm <-
  hn_df_asthma_linearity_pm %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    polyDeg = as.character(polyDeg)
  )

# RTI ------

hn_df_rti_linearity  <- 
  hn_df %>% filter(outcome=="resp-rti") %>% 
  filter(comp=="linearity")

hn_df_rti_linearity_1 <-
  hn_df_rti_linearity %>% 
  filter(polyDeg==1) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_rti_linearity_pm <- 
  expand_grid(hn_df_pm, hn_df_rti_linearity_1) %>% 
  mutate(polyDeg=1)

for (i in 2:4) {
  
  hn_df_rti_linearity_i <-
    hn_df_rti_linearity %>% 
    filter(polyDeg==i) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_rti_linearity_i_pm <- expand_grid(hn_df_pm, hn_df_rti_linearity_i)
  
  hn_df_rti_linearity_pm <-
    hn_df_rti_linearity_pm %>% 
    bind_rows(hn_df_rti_linearity_i_pm %>% 
                mutate(polyDeg=i))
  
}

hn_df_rti_linearity_pm <-
  hn_df_rti_linearity_pm %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    polyDeg = as.character(polyDeg)
  )

# Lags ------

# All-cause ------
hn_df_all_lags <- 
  hn_df %>% 
  filter(outcome=="all-cause") %>% 
  filter(comp=="lags")

hn_df_all_lags_0 <-
  hn_df_all_lags %>% 
  filter(lags==0) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_all_lags_pm_0 <- 
  expand_grid(hn_df_pm, hn_df_all_lags_0) %>% 
  mutate(lags=0)

hn_df_all_lags_7 <-
    hn_df_all_lags %>% 
    filter(lags==7) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_all_lags_7_pm <- 
    expand_grid(hn_df_pm, hn_df_all_lags_7) %>% 
    mutate(lags=7)
  

  
  hn_df_all_lags_28 <-
    hn_df_all_lags %>% 
    filter(lags==28) %>% 
    dplyr::select(var, percent) %>% 
    rename(name = var,
           value = percent) %>% 
    pivot_wider()
  
  hn_df_all_lags_28_pm <- 
    expand_grid(hn_df_pm, hn_df_all_lags_28) %>% 
    mutate(lags=28)
  
  hn_df_all_lags_pm <-
    hn_df_all_lags_pm_0 %>% 
    bind_rows(hn_df_all_lags_7_pm) %>% 
    bind_rows(hn_df_all_lags_28_pm) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    lags = as.character(lags)
  )



# Resp ------

hn_df_resp_lags <- 
  hn_df %>% 
  filter(outcome=="resp") %>% 
  filter(comp=="lags")

hn_df_resp_lags_0 <-
  hn_df_resp_lags %>% 
  filter(lags==0) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_resp_lags_pm_0 <- 
  expand_grid(hn_df_pm, hn_df_resp_lags_0) %>% 
  mutate(lags=0)

hn_df_resp_lags_7 <-
  hn_df_resp_lags %>% 
  filter(lags==7) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_resp_lags_pm_7 <- 
  expand_grid(hn_df_pm, hn_df_resp_lags_7) %>% 
  mutate(lags=7)

hn_df_resp_lags_28 <-
  hn_df_resp_lags %>% 
  filter(lags==28) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_resp_lags_pm_28 <- 
  expand_grid(hn_df_pm, hn_df_resp_lags_28) %>% 
  mutate(lags=28)


hn_df_resp_lags_pm <-
  hn_df_resp_lags_pm_0 %>% 
  bind_rows(
    hn_df_resp_lags_pm_7 
  ) %>% 
  bind_rows(
    hn_df_resp_lags_pm_28 
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    lags = as.character(lags)
  )

# Asthma -----

hn_df_asthma_lags <- 
  hn_df %>% 
  filter(outcome=="resp-asthma") %>% 
  filter(comp=="lags")

hn_df_asthma_lags_0 <-
  hn_df_asthma_lags %>% 
  filter(lags==0) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_asthma_lags_pm_0 <- 
  expand_grid(hn_df_pm, hn_df_asthma_lags_0) %>% 
  mutate(lags=0)

hn_df_asthma_lags_7 <-
  hn_df_asthma_lags %>% 
  filter(lags==7) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_asthma_lags_pm_7 <- 
  expand_grid(hn_df_pm, hn_df_asthma_lags_7) %>% 
  mutate(lags=7)

hn_df_asthma_lags_28 <-
  hn_df_asthma_lags %>% 
  filter(lags==28) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_asthma_lags_pm_28 <- 
  expand_grid(hn_df_pm, hn_df_asthma_lags_28) %>% 
  mutate(lags=28)


hn_df_asthma_lags_pm <-
  hn_df_asthma_lags_pm_0 %>% 
  bind_rows(
    hn_df_asthma_lags_pm_7 
  ) %>% 
  bind_rows(
    hn_df_asthma_lags_pm_28 
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    lags = as.character(lags)
  )

# RTI ------

hn_df_rti_lags <- 
  hn_df %>% 
  filter(outcome=="resp-rti") %>% 
  filter(comp=="lags")

hn_df_rti_lags_0 <-
  hn_df_rti_lags %>% 
  filter(lags==0) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_rti_lags_pm_0 <- 
  expand_grid(hn_df_pm, hn_df_rti_lags_0) %>% 
  mutate(lags=0)

hn_df_rti_lags_7 <-
  hn_df_rti_lags %>% 
  filter(lags==7) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_rti_lags_pm_7 <- 
  expand_grid(hn_df_pm, hn_df_rti_lags_7) %>% 
  mutate(lags=7)

hn_df_rti_lags_28 <-
  hn_df_rti_lags %>% 
  filter(lags==28) %>% 
  dplyr::select(var, percent) %>% 
  rename(name = var,
         value = percent) %>% 
  pivot_wider()

hn_df_rti_lags_pm_28 <- 
  expand_grid(hn_df_pm, hn_df_rti_lags_28) %>% 
  mutate(lags=28)


hn_df_rti_lags_pm <-
  hn_df_rti_lags_pm_0 %>% 
  bind_rows(
    hn_df_rti_lags_pm_7 
  ) %>% 
  bind_rows(
    hn_df_rti_lags_pm_28 
  ) %>% 
  mutate(
    percent = smokePM1*pm25 + (smokePM2*(pm25^2)) + (smokePM3*(pm25^3)) + (smokePM4*(pm25^4)),
    lags = as.character(lags)
  )

# Plot -------


# Histogram ------

smoke_dist_hist <- smoke_dist %>% 
  as_tibble() %>% 
  filter(value>0) %>% 
  mutate(value = ifelse(value>50, 50, value)) %>% 
  mutate(smoke_pm = round(value, digits=0)) %>% 
  group_by(smoke_pm) %>% 
  summarize(
    n=n()
  )

histogram_fig <- 
  ggplot(smoke_dist_hist, 
         aes(x=smoke_pm,y=n)) + 
  geom_bar(fill="grey80", color="grey50",
                 stat = "identity") +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(labels=scales::comma_format(),
                     breaks=c(0, 25000, 50000, 75000)) + 
  coord_cartesian(xlim=c(0, 50),clip="off") +
  theme_classic() + 
  theme(
    plot.margin = unit(c(0,0,0,0), "mm"),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(linewidth=1),
    # plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="black", size=8),
    axis.text.y = element_text(color="grey50", size=6),
    legend.position = "none",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_line(linewidth=0.7),
    axis.line.y.left = element_blank(),
    # axis.title.y = element_blank(),
    plot.title = element_text(color="black", size=10, face="bold")
  ) 

# Fig 1: nonlinearities -------

# All-cause -------

all_linearity_fig <- 
  ggplot(hn_df_all_linearity_pm %>% 
           filter(polyDeg==1 | polyDeg==4) %>%
           mutate(polyDeg = ifelse(polyDeg==1, "Linear", "Quartic")), 
       aes(x=pm25, y=percent, color=polyDeg, group=polyDeg)) + 
  geom_line(size=0.7) +
  scale_color_manual(values=c(met.brewer("Cassatt2")[7], met.brewer("Cassatt2")[2])) + 
  scale_y_continuous(expand=c(0,0), 
                     breaks=c(-.08, -.06, -.04, -.02, 0, .02),
                     labels=scales::percent_format()) + 
  scale_x_continuous(expand=c(0,0)) + 
  geom_hline(yintercept=0, color="grey50", linetype="dotted")+
  coord_cartesian(xlim=c(0, 50),
                  ylim=c(-.08, .02),expand = T,clip="off") +
  ggtitle("All-cause") + 
  ylab("% change relative to a day with no wildfire PM2.5") + 
  theme_classic() + 
  theme(
    plot.margin = unit(c(0,0,-20,0), "mm"),
    # axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_line(linewidth=1),
    # axis.ticks.x = element_line(linewidth=1),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=6, color="black",hjust = 1),
    axis.text.y = element_text(color="black", size=8),
    axis.text.x = element_blank(),
    legend.position = c(0.17, 0.5),
    legend.background = element_blank(),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # axis.line.x.bottom = element_line(linewidth=0.7),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_line(linewidth=0.7),
    plot.title = element_text(color="black", size=10, face="bold")
  ) 

all_linearity_fig

# layout <- c(
#   area(t = 1, l = 0, b = 1, r = 1),
#   area(t = 0, l = 0, b = 0.2, r = 1)
# )
# 
# plot(layout)

all_linearity_hist_fig_1 <-
    all_linearity_fig / 
    histogram_fig +
  plot_layout(heights = c(1, 0.2))

all_linearity_hist_fig_1



# resp -------

hn_df_resp_linearity_pm_fig_df <- 
  hn_df_resp_linearity_pm %>% 
  filter(polyDeg==1 | polyDeg==4) %>% 
  mutate(outcome="All respiratory") %>% 
  bind_rows(
    hn_df_asthma_linearity_pm %>% 
      filter(polyDeg==1 | polyDeg==4) %>% 
      mutate(outcome="Asthma")
  ) %>% 
    bind_rows(
      hn_df_rti_linearity_pm %>% 
        filter(polyDeg==1 | polyDeg==4) %>% 
        mutate(outcome="RTIs")
    ) %>% 
  mutate(polyDeg = ifelse(polyDeg==1, "Linear", "Quartic"))

resp_linearity_fig <- 
  ggplot(hn_df_resp_linearity_pm_fig_df, 
         aes(x=pm25, y=percent, color=polyDeg, group=polyDeg)) + 
  geom_line(size=0.7) +
  scale_color_manual(values=c(met.brewer("Cassatt2")[7], met.brewer("Cassatt2")[2])) + 
  scale_y_continuous(expand=c(0,0), 
                     labels=scales::percent_format(),
                     breaks=c(0, .2, .4, .6, .8)) + 
  scale_x_continuous(expand=c(0,0)) + 
  geom_hline(yintercept=0, color="grey50", linetype="dotted")+
  coord_cartesian(xlim=c(0, 50),
                  ylim=c(-.05, .8)) +
  ggtitle("Respiratory diseases") + 
  theme_classic() + 
  theme(
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_line(linewidth=1),
    axis.ticks.x = element_line(linewidth=1),
    plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(color="black", size=8),
    legend.position = "none",
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size=8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_line(linewidth=.7),
    axis.line.y.left = element_line(linewidth=.7),
    # axis.title.y = element_blank(),
    plot.title = element_text(color="black", size=10, face="bold")
  ) +
  facet_grid(.~outcome)



# Fig 2: lags ----------

# All cause ------

all_lags_fig <- 
  ggplot(hn_df_all_lags_pm %>% 
           filter(lags!=28) %>% 
           mutate(lags = ifelse(lags==0, "No lags", 
                                ifelse(lags==7, "7 days of lags", NA)))  %>% 
           mutate(lags = factor(lags, levels=c("No lags", "7 days of lags"))), 
         aes(x=pm25, y=percent, color=lags, group=lags)) + 
  geom_line(size=.7) +
  scale_color_manual(values=c(met.brewer("Cassatt2")[4], met.brewer("Cassatt2")[2])) + 
  scale_y_continuous(expand=c(0,0), 
                     breaks=c(-.2, -.18, -.16, -.14, -.12, -.1,
                              -.08, -.06, -.04, -.02, 0, .02, .04),
                     labels=scales::percent_format()) + 
  scale_x_continuous(expand=c(0,0)) + 
  geom_hline(yintercept=0, color="grey50", linetype="dotted")+
  coord_cartesian(xlim=c(0, 50),
                  ylim=c(-.08, .02),
                  clip="off") +
  ggtitle("All-cause") + 
  ylab("% change relative to a day with no wildfire PM2.5") + 
  theme_classic() + 
  theme(
    plot.margin = unit(c(0,0,-20,0), "cm"),
    axis.title.y = element_text(color="black", size=6, hjust=1),
    axis.line.y = element_blank(),
    axis.ticks.y = element_line(linewidth=1),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(color="black", size=8),
    axis.text.x = element_blank(),
    legend.position = c(0.2, 0.5),
    legend.title = element_blank(),
    legend.text=element_text(size=8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_line(linewidth=.7),
    # axis.title.y = element_blank(),
    plot.title = element_text(color="black", size=10, face="bold")
  ) 

all_lags_fig


all_lags_hist_fig <- 
  ggdraw() +
  draw_plot(all_lags_fig, y = 0.06) + 
  draw_plot(histogram_fig, 
            x=0.065, width = 1-0.065,
            height = 0.25)

all_lags_hist_fig <-
  all_lags_fig / 
  histogram_fig +
  plot_layout(heights = c(1, 0.2))


# Respiratory -----

hn_df_resp_lags_pm_fig_df <- 
  hn_df_resp_lags_pm %>% 
  mutate(outcome="All respiratory") %>% 
  bind_rows(
    hn_df_asthma_lags_pm %>% 
      mutate(outcome="Asthma")
  ) %>% 
  bind_rows(
    hn_df_rti_lags_pm %>% 
      mutate(outcome="RTIs")
  ) 

resp_lags_fig <- 
  ggplot(hn_df_resp_lags_pm_fig_df %>% 
           filter(lags!=28) %>% 
           mutate(lags = ifelse(lags==0, "No lags", 
                                ifelse(lags==7, "7 days of lags", NA))) %>% 
           mutate(lags = factor(lags, levels=c("No lags", "7 days of lags"))), 
         aes(x=pm25, y=percent, color=lags, group=lags)) + 
  geom_line(size=.7) +
  scale_color_manual(values=c(met.brewer("Cassatt2")[4], met.brewer("Cassatt2")[2])) + 
  scale_y_continuous(expand=c(0,0), 
                     labels=scales::percent_format(),
                     breaks=c(0, .2, .4, .6, .8)) + 
  scale_x_continuous(expand=c(0,0)) + 
  geom_hline(yintercept=0, color="grey50", linetype="dotted")+
  coord_cartesian(xlim=c(0, 50),
                  ylim=c(-.05, .8)) +
  ggtitle("Respiratory diseases") + 
  theme_classic() + 
  theme(
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_line(linewidth=1),
    axis.ticks.x = element_line(linewidth=1),
    plot.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(color="black", size=8),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text=element_text(size=8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x.bottom = element_line(linewidth=.7),
    axis.line.y.left = element_line(linewidth=.7),
    # axis.title.y = element_blank(),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=10, face="bold")
  ) +
  facet_grid(.~outcome)




# Combine plots ------

linearity_figs <- 
  plot_grid(
    all_linearity_hist_fig_1,
    resp_linearity_fig,
    nrow=1,
    align="hv",
    labels=c("a", "b")
  )

lags_figs <- 
  plot_grid(
    all_lags_hist_fig,
    resp_lags_fig,
    nrow=1,
    align="hv",
    labels=c("c", "d")
  )

combined_hn_figs <- 
  plot_grid(
    linearity_figs,
    lags_figs,
    NULL,
    nrow=3,
    align="hv",
    rel_heights = c(1, 1, 0.05)
  ) +
  annotate(
    "richtext", x=.5, y=0.02, 
    label="Ambient wildfire PM2.5 (ugm3)", 
    color="black", size=3,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=.23, y=0.6, 
    label="zipcode-day PM2.5 distribution when any smoke", 
    color="grey50", size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.45, y=0.84, 
    label="<b>-1,300 ED visits<b>", 
    color=met.brewer("Cassatt2")[7], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.46, y=0.615, 
    label="<b>+3,300 ED visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +
  
  annotate(
    "richtext", x=0.677, y=0.62, 
    label="<b>+1,000 visits<b>", 
    color=met.brewer("Cassatt2")[7], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.677, y=0.684, 
    label="<b>+380 visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +

  annotate(
    "richtext", x=0.84, y=0.82, 
    label="<b>+710 visits<b>", 
    color=met.brewer("Cassatt2")[7], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.782, y=0.85, 
    label="<b>+630 visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  )+
  
  annotate(
    "richtext", x=0.97, y=0.572, 
    label="<b>+40 visits<b>", 
    color=met.brewer("Cassatt2")[7], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.97, y=0.625, 
    label="<b>-320 visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +
  
  annotate(
    "richtext", x=0.45, y=0.35, 
    label="<b>+300 ED visits<b>", 
    color=met.brewer("Cassatt2")[4], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.46, y=0.127, 
    label="<b>+3,300 ED visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +
  
  annotate(
    "richtext", x=0.677, y=0.13, 
    label="<b>+460 visits<b>", 
    color=met.brewer("Cassatt2")[4], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.677, y=0.20, 
    label="<b>+380 visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +
  
  annotate(
    "richtext", x=0.855, y=0.33, 
    label="<b>+530 visits<b>", 
    color=met.brewer("Cassatt2")[4], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.79, y=0.38, 
    label="<b>+630 visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +
  
  annotate(
    "richtext", x=0.97, y=0.13, 
    label="<b>-320 visits<b>", 
    color=met.brewer("Cassatt2")[2], size=2,
    fill=NA,
    label.color=NA
  ) +
  annotate(
    "richtext", x=0.97, y=0.085, 
    label="<b>-210 visits<b>", 
    color=met.brewer("Cassatt2")[4], size=2,
    fill=NA,
    label.color=NA
  ) 
# combined_hn_figs


# cowplot::ggsave2(
#   paste0("~/Documents/combined_hn_figs.pdf"),
#   plot = combined_hn_figs,
#   dpi = 300,
#   height = 125,
#   width = 200,
#   unit = "mm"
# )
   
