# Header Start =================================================================
# File Name: Part5_ACP_Subgroup.R
# Purpose: Subgroup wise ACP plots 
# Author: Vaibhav Agarwal
# R version: 4.3.2 (2023-10-31 ucrt)
# Last Modified: 11th April 2025
# Header End ===================================================================


# Libraries ====================================================================

library(tidyverse) # version tidyverse_2.0.0
library(cregg) # version cregg_0.4.0
library(ggplot2) # version ggplot2_3.5.0
library(legendry) # version legendry_0.2.0
library(survival) # version survival_3.8-3
library(sandwich) # version  sandwich_3.1-0
library(fastDummies) # version fastDummies_1.7.4 
library(scales) # version scales_1.3.0
library(patchwork) #version patchwork_1.2.0

# Loading Data (All) ===========================================================

#fi_df <- read.csv(file.path(base_path, "Data", "fi_df.csv"))
# Using from env. 

# Subgroup Function ============================================================

generate_acp_plots <- function(data_subset, subset_label) {
  
  fi_df_acp <- as.data.frame(data_subset)
  fi_df_acp <- fi_df_acp %>% 
    dplyr::select(c(ID, Count, Task:choice))
  
  colnames(fi_df_acp) <- c('ID','Count', 'Task', 'Option', 'Decision', 'Agency', 
                           'Mistake', 'Interest', 'Transparency', 'Accountability', 'Choice')
  
  fi_df_acp$CaseID <- fi_df_acp$Count 
  fi_df_acp$contest_no <- fi_df_acp$Task 
  fi_df_acp$Chosen_Decision <- fi_df_acp$Choice
  fi_df_acp$task <- cumsum(!duplicated(fi_df_acp[, c("contest_no", "CaseID")]))
  
  fi_df_acp$CaseID <- as.numeric(fi_df_acp$CaseID)
  fi_df_acp$contest_no <- as.numeric(fi_df_acp$contest_no)
  fi_df_acp$task <- as.numeric(fi_df_acp$task)
  
  conjacp_data <- conjacp.prepdata(
    Chosen_Decision ~ Decision + Agency + Mistake + Interest + Transparency + Accountability,
    data = fi_df_acp,
    tasks = "task",
    id = "CaseID"
  )
  
  results_acp <- conjacp.estimation(
    conjacp_data,
    estimand = "acp",
    adjust = FALSE
  )
  
  ind_estimate <- NULL
  ind_se       <- NULL
  
  for (attribute2 in c('Decision', 'Agency', 'Mistake', 'Interest', 'Transparency', 'Accountability')) {
    data_model2 <- fi_df_acp[, c(attribute2, "Chosen_Decision")]
    names(data_model2)[-ncol(data_model2)] <- paste0(names(data_model2)[-ncol(data_model2)], ".")
    
    model2 <- lm(Chosen_Decision ~ ., data = data_model2)
    
    ind_estimate <- c(ind_estimate, coef(model2)[-1])
    vcov2 <- vcovCluster(model2, fi_df_acp$CaseID)
    ind_se <- c(ind_se, sqrt(diag(vcov2))[2:(nlevels(as.data.frame(fi_df_acp)[, match(attribute2, colnames(fi_df_acp))]))])
  }
  
  var.table <- function(object.var, k = 2) {
    tab1 <- data.frame(attribute = names(object.var$range_estimates),
                       estimates = object.var$range_estimates,
                       lower = object.var$range_lower,
                       upper = object.var$range_upper,
                       estimand = "Range")
    tab2 <- data.frame(attribute = names(object.var$variability_estimates),
                       estimates = k * object.var$variability_estimates,
                       lower = k * object.var$variability_lower,
                       upper = k * object.var$variability_upper,
                       estimand = "Variability")
    return(rbind(tab1, tab2))
  }
  
  var_acp <- conjacp.var(results_acp, alpha = .05, nsimul = 25000)
  table_var_acp <- var.table(var_acp)
  table_var_acp$type <- "Regular"
  table_var2 <- rbind(table_var_acp)
  rownames(table_var2) <- NULL
  table_var2$attribute <- as.factor(table_var2$attribute)
  table_var2$attribute <- factor(table_var2$attribute,
                                 levels = rev(c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")))
  
  acp_attimp <- ggplot(table_var2[table_var2$type == "Regular", ],
                       aes(y = estimates, x = attribute, group = estimand)) +
    coord_flip(ylim = c(0, .43)) +
    geom_pointrange(aes(ymin = lower, ymax = upper, shape = estimand, 
                        color = estimand, fill = estimand),
                    position = position_dodge(width = .5), size = .7) +
    labs(
      title = 'Attribute Relative Importance using ACP for Finland',
      x = '',
      y = '',
      subtitle = subset_label,
      caption = '*Data collected from AGACA project under Daria Gritsenko',
      shape = 'Measure',
      color = 'Measure',
      fill = 'Measure'
    ) +
    scale_color_manual(values = c('black', 'darkgrey')) +
    scale_shape_manual(values = c(16,8)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(face = 'bold', size = 10),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      plot.subtitle = element_text(color = 'black', size = 10, hjust = 0.5),
      axis.title.y = element_text(color = "black", size = 12, face = "bold"),
      plot.title = element_text(color = "black", size = 14, hjust = 0.5),
      plot.caption = element_text(face = "italic"),
      legend.position = 'bottom'
    )
  
  FI_AMCE <- amce(fi_df_acp, Chosen_Decision ~ Decision + Agency + Mistake + 
                    Interest + Transparency + Accountability, id = ~CaseID)
  
  table_acpamce3 <- data.frame(
    modality = c("Decision type:", "   Theft Person", "   Theft Area", "   Robbery Person", "   Robbery Area","",
                 "Agency type:", "   Alg. Only", "   Alg. Overruled", "   Alg. Recommends","",
                 "Mistake type:", "   Humans", "   Algorithms", "   Same","",
                 "Interest type:", "   Private", "   Public","",
                 "Transparency type:", "   Developer", "   Experts", "   Police","",
                 "Accountability type:", "   Developer (Acc)", "   Experts (Acc)", "   Police (Acc)",
                 
                 "Decision type:", "   Theft Person", "   Theft Area", "   Robbery Person", "   Robbery Area","",
                 "Agency type:", "   Alg. Only", "   Alg. Overruled", "   Alg. Recommends","",
                 "Mistake type:", "   Humans", "   Algorithms", "   Same","",
                 "Interest type:", "   Private", "   Public","",
                 "Transparency type:", "   Developer", "   Experts", "   Police","",
                 "Accountability type:", "   Developer (Acc)", "   Experts (Acc)", "   Police (Acc)",
                 
                 "Decision type:", "   Theft Person", "   Theft Area", "   Robbery Person", "   Robbery Area","",
                 "Agency type:", "   Alg. Only", "   Alg. Overruled", "   Alg. Recommends","",
                 "Mistake type:", "   Humans", "   Algorithms", "   Same","",
                 "Interest type:", "   Private", "   Public","",
                 "Transparency type:", "   Developer", "   Experts", "   Police","",
                 "Accountability type:", "   Developer (Acc)", "   Experts (Acc)", "   Police (Acc)"
    ),
    var = c("", rep("Decision", 4), "", "", rep("Agency", 3), "", "", rep("Mistake", 3),
            "", "", rep("Interest", 2), "", "", rep("Transparency", 3), "", "", rep("Accountability", 3)),
    estimate = c(1, results_acp$estimates[1:4], 1, 1,
                 results_acp$estimates[5:7], 1, 1,
                 results_acp$estimates[8:10], 1, 1,
                 results_acp$estimates[11:12], 1, 1,
                 results_acp$estimates[13:15], 1, 1,
                 results_acp$estimates[16:18], 1, 0,
                 ind_estimate[1:3], 1, 1, 0,
                 ind_estimate[4:5], 1, 1, 0,
                 ind_estimate[6:7], 1, 1, 0,
                 ind_estimate[8], 1, 1, 0,
                 ind_estimate[9:10], 1, 1, 0,
                 ind_estimate[11:12], 1,
                 FI_AMCE$estimate[1:4], 1, 1,
                 FI_AMCE$estimate[5:7], 1, 1,
                 FI_AMCE$estimate[8:10], 1, 1,
                 FI_AMCE$estimate[11:12], 1, 1,
                 FI_AMCE$estimate[13:15], 1, 1,
                 FI_AMCE$estimate[16:18]),
    se = c(0, sqrt(diag(results_acp$vcov))[1:4], 0, 0,
           sqrt(diag(results_acp$vcov))[5:7], 0, 0,
           sqrt(diag(results_acp$vcov))[8:10], 0, 0,
           sqrt(diag(results_acp$vcov))[11:12], 0, 0,
           sqrt(diag(results_acp$vcov))[13:15], 0, 0,
           sqrt(diag(results_acp$vcov))[16:18], 0, 0,
           ind_se[1:3], 0, 0, 0,
           ind_se[4:5], 0, 0, 0,
           ind_se[6:7], 0, 0, 0,
           ind_se[8], 0, 0, 0,
           ind_se[9:10], 0, 0, 0,
           ind_se[11:12], 0,
           FI_AMCE$std.error[1:4], 0, 0,
           FI_AMCE$std.error[5:7], 0, 0,
           FI_AMCE$std.error[8:10], 0, 0,
           FI_AMCE$std.error[11:12], 0, 0,
           FI_AMCE$std.error[13:15], 0, 0,
           FI_AMCE$std.error[16:18]),
    type = c(rep("ACP", 29), rep("AMCE_indse", 29), rep("AMCE", 29))
  )
  
  table_acpamce3$modality <- factor(table_acpamce3$modality,
                                    levels = unique(table_acpamce3$modality)[length(table_acpamce3$modality):1])
  
  hline <- data.frame(type = c("AMCE", "ACP", "AMCE_indse"),
                      yint = c(0, 1, 1))
  
  table_acpamce4 <- table_acpamce3 %>%
    filter((type == 'ACP' | type == 'AMCE') & modality != "" & var != "" )
  
  table_acpamce4$var <- factor(table_acpamce4$var,
                               levels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability"))
  
  centered_bracket <- primitive_bracket(
    key = key_range_manual(
      start = c(15, 12, 9, 7, 4, 1),
      end   = c(18, 14, 11, 8, 6, 3),
      labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
    ),
    bracket = "square",
    theme = theme(
      axis.text.y.left = element_text(angle = 0, vjust = -2),
      legend.text = element_text(angle = 0, vjust = -2)
    )
  )
  
  amcevsacp <- ggplot(table_acpamce4, aes(y = estimate, x = modality, group = var, color = type)) +
    coord_flip(ylim = c(-0.25, 0.35)) +
    geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se,
                        color = type, shape = var, fill = type),
                    position = position_dodge(width = 1), size = 0.7) +
    geom_hline(data = subset(hline, type == "AMCE"), aes(yintercept = yint), size = 1) +
    facet_grid(. ~ type) +
    labs(
      x = 'Attribute Levels',
      y = '',
      title = paste0(subset_label),
      shape = 'Attribute'
    ) +
    scale_shape_manual(
      values = c(8, 17, 4, 16, 6, 15),
      labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
    ) +
    scale_color_manual(values = c('black', 'black'), guide = "none") +
    scale_fill_manual(values = c('black', 'black'), guide = "none") +
    theme_bw() +
    theme(
      axis.text.x = element_text(face = 'bold', size = 10),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      plot.subtitle = element_text(color = 'black', size = 10, hjust = 0.5),
      axis.title.y = element_text(color = "black", size = 12, face = "bold"),
      plot.title = element_text(color = "black", size = 14, hjust = 0.5),
      plot.caption = element_text(face = "italic"),
      legend.position = 'none'
    ) +
    guides(
      y = guide_axis_stack("axis", centered_bracket)
    )
  
  filename <- paste0(subset_label, "_ACP.png")
  save_path <- file.path(base_path, "Figures", "Subgroup", "ACP", filename)
  
  ggsave(filename = save_path, plot = amcevsacp, width = 12, height = 7, dpi = 300)
}


# Gender =======================================================================
# Subsetting the data 
male_fi <- fi_df %>% filter(gender_new == 'Male')
female_fi <- fi_df %>% filter(gender_new == 'Female')

# Running the function
generate_acp_plots(male_fi, "Male")
generate_acp_plots(female_fi, "Female")


# Age ==========================================================================
# Subsetting the data
young_fi        <- fi_df %>% filter(age_new == 'Young Adults')
middle_aged_fi  <- fi_df %>% filter(age_new == 'Middle-Aged')
older_fi        <- fi_df %>% filter(age_new == 'Older Adults')

# Running the function for each age subgroup
generate_acp_plots(young_fi, "Young_Adults")
generate_acp_plots(middle_aged_fi, "Middle_Aged")
generate_acp_plots(older_fi, "Older_Adults")


# Education ====================================================================
# Subsetting the data
no_college_fi   <- fi_df %>% filter(edu_new == 'No College/University')
college_fi      <- fi_df %>% filter(edu_new == 'College/University Education')

# Running the function for education subgroups
generate_acp_plots(no_college_fi, "No_College")
generate_acp_plots(college_fi, "College")


# Residence ====================================================================
# Subsetting the data
urban_fi <- fi_df %>% filter(Rural_Urban_Group == 'Urban')
rural_fi <- fi_df %>% filter(Rural_Urban_Group == 'Rural')

# Running the function for residence subgroups
generate_acp_plots(urban_fi, "Urban")
generate_acp_plots(rural_fi, "Rural")


# Employment Status ============================================================
# Subsetting the data
retired_fi   <- fi_df %>% filter(Employment_Status_Group == 'Retired')
students_fi  <- fi_df %>% filter(Employment_Status_Group == 'Students')
employed_fi  <- fi_df %>% filter(Employment_Status_Group == 'Employed')
others_fi    <- fi_df %>% filter(Employment_Status_Group == 'Others')

# Running the function for employment status subgroups
generate_acp_plots(retired_fi, "Retired")
generate_acp_plots(students_fi, "Students")
generate_acp_plots(employed_fi, "Employed")
generate_acp_plots(others_fi, "Others")


# Surveillance Attitude ========================================================
# Subsetting the data
negative_fi <- fi_df %>% filter(surveillance_attitude_new == 'Negative')
positive_fi <- fi_df %>% filter(surveillance_attitude_new == 'Positive')

# Running the function for surveillance attitude subgroups
generate_acp_plots(negative_fi, "Negative")
generate_acp_plots(positive_fi, "Positive")


# Government Satisfaction ======================================================
# Subsetting the data
high_sat_fi   <- fi_df %>% filter(gov_sat_cat == 'High Satisfaction')
medium_sat_fi <- fi_df %>% filter(gov_sat_cat == 'Medium Satisfaction')
low_sat_fi    <- fi_df %>% filter(gov_sat_cat == 'Low Satisfaction')

# Running the function for government satisfaction subgroups
generate_acp_plots(high_sat_fi, "High_Satisfaction")
generate_acp_plots(medium_sat_fi, "Medium_Satisfaction")
generate_acp_plots(low_sat_fi, "Low_Satisfaction")


# Modern Tech Law ==============================================================
# Subsetting the data
appropriate_fi      <- fi_df %>% filter(modrn_tech_law_cat == 'Appropriate')
not_appropriate_fi  <- fi_df %>% filter(modrn_tech_law_cat == 'Not Appropriate')

# Running the function for tech law subgroups
generate_acp_plots(appropriate_fi, "Appropriate")
generate_acp_plots(not_appropriate_fi, "Not_Appropriate")


# Worried About Social Media ===================================================
# Subsetting the data
not_worried_sm_fi       <- fi_df %>% filter(worried_socialmedia == 'Not worried at All')
little_worried_sm_fi    <- fi_df %>% filter(worried_socialmedia == 'Little Worried')
very_worried_sm_fi      <- fi_df %>% filter(worried_socialmedia == 'Very Worried')
somewhat_worried_sm_fi  <- fi_df %>% filter(worried_socialmedia == 'Somewhat Worried')

# Running the function for social media worry subgroups
generate_acp_plots(not_worried_sm_fi, "Not_Worried_SM")
generate_acp_plots(little_worried_sm_fi, "Little_Worried_SM")
generate_acp_plots(very_worried_sm_fi, "Very_Worried_SM")
generate_acp_plots(somewhat_worried_sm_fi, "Somewhat_Worried_SM")


# Worried About E-Services =====================================================
# Subsetting the data
not_worried_eserv_fi       <- fi_df %>% filter(worried_eserv == 'Not worried at All')
little_worried_eserv_fi    <- fi_df %>% filter(worried_eserv == 'Little Worried')
very_worried_eserv_fi      <- fi_df %>% filter(worried_eserv == 'Very Worried')
somewhat_worried_eserv_fi  <- fi_df %>% filter(worried_eserv == 'Somewhat Worried')

# Running the function for e-service worry subgroups
generate_acp_plots(not_worried_eserv_fi, "Not_Worried_Eserv")
generate_acp_plots(little_worried_eserv_fi, "Little_Worried_Eserv")
generate_acp_plots(very_worried_eserv_fi, "Very_Worried_Eserv")
generate_acp_plots(somewhat_worried_eserv_fi, "Somewhat_Worried_Eserv")


# Tech Adoption ================================================================
# Subsetting the data
laggards_fi       <- fi_df %>% filter(tech_adoption_new == 'Laggards')
majority_fi       <- fi_df %>% filter(tech_adoption_new == 'Majority')
early_adopters_fi <- fi_df %>% filter(tech_adoption_new == 'Early Adopters')

# Running the function for tech adoption subgroups
generate_acp_plots(laggards_fi, "Laggards")
generate_acp_plots(majority_fi, "Majority")
generate_acp_plots(early_adopters_fi, "Early_Adopters")

# End ==========================================================================
