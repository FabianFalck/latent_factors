# A framework for longitudinal latent factor modelling of treatment response in clinical trials with applications to Psoriatic Arthritis and Rheumatoid Arthritis

  **[Setup](#installation)**
| **[Code overview](#code)**
| **[Demo](#demo)**
| **[Citation](#citation)**

![R 4.0](https://img.shields.io/static/v1?label=R&message=%3E=4.0&color=Blue)

Code repository accompanying the manuscript "A framework for longitudinal latent factor modelling of treatment response in clinical trials with applications to Psoriatic Arthritis and Rheumatoid Arthritis" (under submission).

This work provides a framework for interpretable treatment effect estimation in clinical trials under longitudinal, multivariate measurements. 

Our implementation is in *R*.

![Intuition figure](graphics/intro_to_stage_1.jpg)
![Intuition figure](graphics/intro_to_stage_2.jpg)


## Setup

First, we assume you have a working version of R and RStudio installed on your machine with common packages installed. We use the following core dependencies and versions: 

```bash
R (version 4.0.0)
Rstudio (version 1.3.959)
Bioconductor (version 3.11)
```

All dependencies are conveniently wrapped into an `R environment`. 
To load all dependencies, follow these two steps: 

1. Within `R Studio`, set your working to this repository: `setwd("<absolute-path-where-the-repo-is-in>/latent_factors_treatment_effect")`
2. Activate the R environment via `renv::activate()`

Enabled by our efficient implementation, this code be run on regular, academic-size CPU clusters.
We recommend allocating at least 20GB of RAM when launching the job to run this software.

The clinical trial data we use in this manuscript can be requested on [ClinicalStudyDataRequest.com](ClinicalStudyDataRequest.com).




## Code overview

The subfolder `latent_factors_treatment_effect_prep/analysis/` contains the core analysis scripts used throughout this manuscript, producing our main results. 
In the following, we provide a short description of what each of these scripts do with references to the manuscript.


| File or Directory      | Description |
| ----------- | ----------- |
| 00_wrapper.R | Wrapper script to call the analysis scripts below. | 
| 01_load_PsA_RA.R | Load the PsA and RA clinical trial data. |
| 01_load_PsA_RA_newRelease.R | Load the PsA and RA clinical trial data (with latest data release). |
| 02_preprocess_PsA_RA.R | Create R objects containing measurement data, subject metadata and measurement metadata. |
| 03_analysis.R | Implements main analysis pipeline. |
| 04_plotting_intro.R | Sourced by other plotting scripts to import functions and get control parameters. |
| 04a_plot_overview.R | Visual overview of multiple trials' results. |
| 04b_plot_single_study.R | Plot results for a single trial (Figure 5). |
| 04c_plot_all_studies.R | Compare loadings and scores from multiple trials. |
| 04d_meta_analysis.R | Plot meta-analysis of multiple trials (Figure 6). |
| 04e_study_baseline_means.R | Explore variation across indications at baseline. |
| 04f_compare_indications.R | Compare results across indications. |
| 04f_compare_indications_same_plot.R | Compare results across indications with score trajectories superimposed (Figure 7). |
| 04g_compare_loadings.R | Plot comparing loadings across multiple trials (Figure S3).|
| 04h_intro_to_factor_analysis_plot.R | Generates introduction to Stage 1 (Figure 1). |
| 04i_intro_to_stage_2_plot.R | Introduction to Stage 2 with multiple factors. |
| 04i_intro_to_stage_2_single_factor.R | Generates introduction to Stage 2 (Figure 2). |
| 04z_run_paper_plot_scripts.R | Wrapper sourcing the plotting scripts to generate figures for the paper. |


Furthermore, `mlfa_package/` provides the core implementation of our framework. 
We refer to its corresponding README and the docstrings in the respective scripts for a detailed documentation.


## Demo

As we are using proprietary data in this analysis, our data cannot be shared in this public repository.
To still show a working code example of how this framework can be useful in clinical trials, we simulated a dataset which contains similar patterns as we observed in the studies we analysed.
This demo and an accompanying documentation is coming soon!


## Citation

Coming soon!

