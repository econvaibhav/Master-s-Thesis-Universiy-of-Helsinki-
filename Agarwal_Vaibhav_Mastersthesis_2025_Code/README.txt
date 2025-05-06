=================================================================
# File Name: README Code
# Purpose: Important Navigation Information
# Author: Vaibhav Agarwal
# R version used: 4.3.2 (2023-10-31 ucrt)
# Last Modified: 6th May 2025
=================================================================

The R library versions used in this thesis are as follows:

library(tidyverse)     # version 2.0.0  
library(ggplot2)       # version 3.5.0  
library(scales)        # version 1.3.0  
library(stringr)       # version 1.5.1  
library(gtsummary)     # version 2.1.0  
library(gt)            # version 0.11.1  
library(Hmisc)         # version 5.1-1  
library(vtable)        # version 1.4.8  
library(summarytools)  # version 1.0.1  
library(cregg)         # version 0.4.0  
library(legendry)      # version 0.2.0  
library(survival)      # version 3.8-3  
library(sandwich)      # version 3.1-0  
library(fastDummies)   # version 1.7.4  
library(patchwork)     # version 1.2.0  
library(stargazer)     # version 5.2.3  
library(lmtest)        # version 0.9-40  
library(mlogit)        # version 1.0-2  
library(gmnl)          # version 1.1-3.2  
library(xtable)        # version 1.8-4  

# Note:
# Due to package version conflicts, 'mlogit' and 'gmnl' were downgraded.
# This was resolved through trial and error by referring to older blog posts and articles.
# The issue was related to changes in newer versions of 'mlogit' (dfidx integration).

=================================================================
# External Code Use (Ganter Functions)
=================================================================
4 modified functions were adapted from Ganter's GitHub repository:
https://github.com/flavienganter/preferences-conjoint-experiments

They were tailored to match the structure of the current dataset.

Accessed via:
base_path = "C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav"

source(file.path(base_path, "Code", "Functions_Ganter_Modified", "conjacp.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionVCOVCluster.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionTableGraph.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "theme_graph.R"))

=================================================================
# Code Files
=================================================================

# Part0_Cleaning.R
# Cleans and sets up the raw data for further analysis.

# Part1_Distributions_Discriptions.R
# Explores variable distributions and creates summary tables.

# Part2_MM_AMCE_ACP.R
# Generates MM, AMCE, ACP, and conditional logit plots for the full dataset.

# Part3_MM_Subgroup.R
# Creates subgroup-wise MM plots.

# Part4_AMCE_Subgroup.R
# Produces AMCE plots for different subgroups.

# Part5_ACP_Subgroup.R
# Outputs subgroup-specific ACP plots.

# Part6_Clogit_Subgroup.R
# Plots attribute importance using conditional logit for subgroups.

# Part7_LCAandANA.R
# Runs LCA models (constrained/unconstrained) to assess ANA behavior.

# Part8_FinalChecks_MethodologicalCohesion.R
# Conducts final checks and organizes results for methodological cohesion.
