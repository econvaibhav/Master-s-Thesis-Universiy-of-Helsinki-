# Thesis Analysis Repository

**Author:** Vaibhav Agarwal  
**R Version:** 4.3.2 (2023-10-31 ucrt)  
**Last Modified:** 6 May 2025

---

## Overview

This repository contains all code, functions, and outputs used for the master's thesis analysis. The workflow covers data cleaning, descriptive analyses, conjoint analysis estimation, subgroup analyses, latent class models, and methodological robustness checks.

---

## R Package Versions

The following package versions were used to ensure reproducibility:

| Package | Version |
|----------|----------|
| tidyverse | 2.0.0 |
| ggplot2 | 3.5.0 |
| scales | 1.3.0 |
| stringr | 1.5.1 |
| gtsummary | 2.1.0 |
| gt | 0.11.1 |
| Hmisc | 5.1-1 |
| vtable | 1.4.8 |
| summarytools | 1.0.1 |
| cregg | 0.4.0 |
| legendry | 0.2.0 |
| survival | 3.8-3 |
| sandwich | 3.1-0 |
| fastDummies | 1.7.4 |
| patchwork | 1.2.0 |
| stargazer | 5.2.3 |
| lmtest | 0.9-40 |
| mlogit | 1.0-2 |
| gmnl | 1.1-3.2 |
| xtable | 1.8-4 |

### Package Compatibility Note

The packages **`mlogit`** and **`gmnl`** were intentionally downgraded due to version conflicts.

The issue stemmed from changes introduced in newer versions of `mlogit`, particularly the integration of `dfidx`. Compatibility was restored through trial-and-error testing and consultation of older documentation, blog posts, and community discussions.

---

## External Code Use

Four modified functions were adapted from Flavien Ganter's repository:

Repository:  
https://github.com/flavienganter/preferences-conjoint-experiments

These functions were modified to match the structure and requirements of the current dataset.

### Imported Functions

```r
base_path <- "C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav"

source(file.path(base_path, "Code", "Functions_Ganter_Modified", "conjacp.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionVCOVCluster.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionTableGraph.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "theme_graph.R"))
```

---

## Code Structure

### `Part0_Cleaning.R`
Cleans and prepares the raw data for all subsequent analyses.

### `Part1_Distributions_Discriptions.R`
Produces descriptive statistics, frequency distributions, and summary tables.

### `Part2_MM_AMCE_ACP.R`
Generates:

- Marginal Means (MM)
- Average Marginal Component Effects (AMCE)
- Average Component Preferences (ACP)
- Conditional Logit attribute importance plots

for the full dataset.

### `Part3_MM_Subgroup.R`
Creates subgroup-specific Marginal Means (MM) plots.

### `Part4_AMCE_Subgroup.R`
Creates subgroup-specific AMCE plots.

### `Part5_ACP_Subgroup.R`
Creates subgroup-specific ACP plots.

### `Part6_Clogit_Subgroup.R`
Generates subgroup-specific attribute importance plots using conditional logit models.

### `Part7_LCAandANA.R`
Runs latent class analysis (LCA) models, both constrained and unconstrained, to assess Attribute Non-Attendance (ANA) behavior.

### `Part8_FinalChecks_MethodologicalCohesion.R`
Conducts final robustness checks and organizes results related to methodological cohesion.

---

## Visualization Folder Structure

```text
Figures/
├── Full Data/
│   ├── All methods/
│   │   ├── Main MM plots
│   │   ├── Main AMCE plots
│   │   ├── Main ACP plots
│   │   └── Attribute importance plots
│   │
│   ├── Cohesion/
│   │   ├── Baseline tests
│   │   └── Task 1 vs. Task 2 comparisons
│   │
│   └── Distribution/
│       └── Frequency distributions for all variables
│
└── Subgroup/
    ├── ACP/
    │   └── Subgroup-specific ACP plots
    │
    ├── AMCE/
    │   └── Subgroup-specific AMCE plots
    │
    ├── C_Logit/
    │   └── Subgroup-specific attribute importance plots
    │
    └── MM/
        └── Subgroup-specific MM plots
```

---

## Reproducibility Notes

- Analyses were conducted using **R 4.3.2**.
- Package versions listed above should be maintained where possible.
- Modified external functions are stored in `Functions_Ganter_Modified`.
- Figures are automatically exported into the folder structure described above.

---

## End of Documentation