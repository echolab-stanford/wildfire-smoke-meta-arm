# Health effects of wildfire smoke
Repo supporting Gould et al. "Health effects of wildfire smoke" (2023).

The materials in this repository enable replication of the meta-analysis results. Please note that underlying data used to produce Figure 2 are not available here due to data sharing agreements. For more details, see Heft-Neal et al. (2023) or visit https://github.com/echolab-stanford/pnas-ed-smoke-2023.

If you find meaningful errors or have questions or suggestions, please contact Carlos Gould at gould.cf@gmail.com

# Organization of the repository

Scripts and data are primarily contained in this respository.

- **data**: inputs for analysis
- **scripts**: R code for analysis

## List of scripts

- **0_Meta_WF_Mortality.R**: reads in all-cause mortality effect estimates from included studies and conducts meta-analysis
- **0_Meta_WF_ED.R**: reads in respiratory- and cardiovascular-related ED visit effect estimates from included studies and conducts meta-analysis
- **0_Meta_WF_Hosp.R**: reads in respiratory- and cardiovascular-related hospitalizations visit effect estimates from included studies and conducts meta-analysis
- **1_Meta_WF_Figure.R**: runs 0_ scripts, processes data frames for each outcome, and makes figures.
- **2_WF_Figure.R**: runs script to produce Figure 2. 

## List of datasets

- **wf_meta_study_region.csv**: contains the name of the study, the region, and the outcome evaluated. used for figure making.
- **wf_mortality.csv**: contains study specific data for all-cause mortality
- **wf_ed.csv**: contains study specific data for ED visits
- **wf_hosp.csv**: contains study specific data for hospitalizations
- **combined_wf_meta_df.csv**: contains results of meta analysis and all study specific data
- **funnel_legend.jpeg**: just a picture of the legend we used for making funnel plots in the supplemental information

### R session info
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.4.1
