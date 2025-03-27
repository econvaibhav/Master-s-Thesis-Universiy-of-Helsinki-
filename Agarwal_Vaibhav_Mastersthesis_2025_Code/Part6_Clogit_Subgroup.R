# Header Start =================================================================
# File Name: Part6_Clogit_Subgroup.R
# Purpose: Subgroup wise Attribute Importance plots using Conditional Logit  
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

clogit_attribute_importance <- function(data_subset, subset_label, subtitle, save_graph = FALSE) {

  dummy_df <- dummy_cols(data_subset,
                         select_columns = c("decision", "agency", "mistake", "interest", "transparency", "accountability"),
                         remove_first_dummy = TRUE)
  
  dummy_df <- dummy_df %>%
    dplyr::select(-c(decision, agency, mistake, interest, transparency, accountability)) %>%
    mutate(stratum = paste(Count, Task, sep = "_"))
  
  dummy_df$Task   <- as.factor(dummy_df$Task)
  dummy_df$Option <- as.factor(dummy_df$Option)
  
  # Model fit
  model_clogit <- clogit(
    choice ~ `decision_Robbery Area` + 
      `decision_Robbery Person` + 
      `decision_Theft Area` +
      `agency_Alg Overruled` +
      `agency_Alg. Recommends` +
      `mistake_Algorithms` +
      `mistake_Same` +
      `interest_Private` +
      `transparency_Experts` +
      `transparency_Police` +
      `accountability_Experts (Acc)` +
      `accountability_Police (Acc)` +
      strata(stratum),
    data = dummy_df,
    method = "breslow"
  )
  
  robust_se <- try(vcovCL(model_clogit, cluster = dummy_df$Count), silent = TRUE)
  
  coef_estimates <- coef(model_clogit)
  names(coef_estimates) <- gsub("`", "", names(coef_estimates))
  importance_alt <- abs(coef_estimates)
  importance_alt <- importance_alt / sum(importance_alt) * 100
  
  df_alt <- data.frame(
    Attribute  = names(importance_alt),
    Importance = as.numeric(importance_alt)
  )
  
  pretty_names <- c(
    "decision_Robbery Area"         = "Decision: Robbery Area",
    "decision_Robbery Person"       = "Decision: Robbery Person",
    "decision_Theft Area"           = "Decision: Theft Area",
    "agency_Alg Overruled"          = "Agency: Alg Overruled",
    "agency_Alg. Recommends"         = "Agency: Alg. Recommends",
    "mistake_Algorithms"            = "Mistake: Algorithms",
    "mistake_Same"                  = "Mistake: Same",
    "interest_Private"              = "Interest: Private",
    "transparency_Experts"          = "Transparency: Experts",
    "transparency_Police"           = "Transparency: Police",
    "accountability_Experts (Acc)" = "Accountability: Experts (Acc)",
    "accountability_Police (Acc)"     = "Accountability: Polic(Acc)"
  )
  
  df_alt$PrettyAttribute <- pretty_names[df_alt$Attribute]
  
  attlvlimp <- ggplot(df_alt, aes(x = reorder(PrettyAttribute, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "black") +
    geom_text(aes(label = round(Importance, 1)), 
              hjust = -0.1, size = 4.5, color = 'black') +
    coord_flip() +
    labs(
      title    = "Attribute Importance",
      subtitle = subtitle,
      x        = "Attribute",
      y        = "Importance (%)"
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(0, max(df_alt$Importance) * 1.2)) +
    theme_bw() +
    theme(
      axis.text.x   = element_text(face = 'bold', size = 10),
      axis.title.x  = element_text(color = "black", size = 12, face = "bold"),
      plot.subtitle = element_text(color = 'black', size = 10, hjust = 0.5),
      axis.title.y  = element_text(color = "black", size = 12, face = "bold"),
      plot.title    = element_text(color = "black", size = 14, hjust = 0.5),
      plot.caption  = element_text(face = "italic"),
      legend.position = 'bottom'
    )
  
  if (save_graph) {
    file_name <- paste0(subset_label, "_Attribute_Importance.png")
    save_path <- file.path("C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav/Figures/Subgroup/C_Logit", file_name)
    ggsave(filename = save_path, plot = attlvlimp, width = 14, height = 7, dpi = 300)
  }
  
  return(attlvlimp)
}


# Gender =======================================================================
# Subsetting the data 
male_fi   <- fi_df %>% filter(gender_new == 'Male')
female_fi <- fi_df %>% filter(gender_new == 'Female')

# Generating plots
male_att_plot   <- clogit_attribute_importance(male_fi, "Male", "Male", FALSE)
female_att_plot <- clogit_attribute_importance(female_fi, "Female", "Female", FALSE)

# Combined plots
combined_plot_gender <- (male_att_plot + female_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Gender (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Gender_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_gender, width = 12, height = 7, dpi = 300)

# Age ==========================================================================
# Subsetting the data
young_fi       <- fi_df %>% filter(age_new == 'Young Adults')
older_fi       <- fi_df %>% filter(age_new == 'Older Adults')
middle_aged_fi <- fi_df %>% filter(age_new == 'Middle-Aged')

# Generating plots
young_att_plot       <- clogit_attribute_importance(young_fi, "Young Adults", "Young Adults", FALSE)
older_att_plot       <- clogit_attribute_importance(older_fi, "Older Adults", "Older Adults", FALSE)
middle_aged_att_plot <- clogit_attribute_importance(middle_aged_fi, "Middle-Aged", "Middle-Aged", FALSE)

# Combined plots
combined_plot_age <- (young_att_plot + older_att_plot + middle_aged_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Age (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Age_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_age, width = 12, height = 10, dpi = 300)

# Education ====================================================================
# Subsetting the data
no_college_fi <- fi_df %>% filter(edu_new == 'No College/University')
college_fi    <- fi_df %>% filter(edu_new == 'College/University Education')

# Generating plots
no_college_att_plot <- clogit_attribute_importance(no_college_fi, "No College/University", "No College/University", FALSE)
college_att_plot    <- clogit_attribute_importance(college_fi, "College/University Education", "College/University Education", FALSE)

# Combined plots
combined_plot_edu <- (no_college_att_plot + college_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Education (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Education_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_edu, width = 12, height = 7, dpi = 300)

# Residence ====================================================================
# Subsetting the data
urban_fi <- fi_df %>% filter(Rural_Urban_Group == 'Urban')
rural_fi <- fi_df %>% filter(Rural_Urban_Group == 'Rural')

# Generating plots
urban_att_plot <- clogit_attribute_importance(urban_fi, "Urban", "Urban", FALSE)
rural_att_plot <- clogit_attribute_importance(rural_fi, "Rural", "Rural", FALSE)

# Combined plots
combined_plot_residence <- (urban_att_plot + rural_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Residence (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Residence_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_residence, width = 12, height = 7, dpi = 300)

# Employment Status ============================================================
# Subsetting the data
retired_fi  <- fi_df %>% filter(Employment_Status_Group == 'Retired')
students_fi <- fi_df %>% filter(Employment_Status_Group == 'Students')
employed_fi <- fi_df %>% filter(Employment_Status_Group == 'Employed')
others_fi   <- fi_df %>% filter(Employment_Status_Group == 'Others')

# Generating plots
retired_att_plot  <- clogit_attribute_importance(retired_fi, "Retired", "Retired", FALSE)
students_att_plot <- clogit_attribute_importance(students_fi, "Students", "Students", FALSE)
employed_att_plot <- clogit_attribute_importance(employed_fi, "Employed", "Employed", FALSE)
others_att_plot   <- clogit_attribute_importance(others_fi, "Others", "Others", FALSE)

# Combined plots
combined_plot_employment <- (retired_att_plot + students_att_plot + employed_att_plot + others_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Employment Status (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Employment_Status_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_employment, width = 12, height = 10, dpi = 300)

# Surveillance Attitude ========================================================
# Subsetting the data
negative_fi <- fi_df %>% filter(surveillance_attitude_new == 'Negative')
positive_fi <- fi_df %>% filter(surveillance_attitude_new == 'Positive')

# Generating plots
negative_att_plot <- clogit_attribute_importance(negative_fi, "Negative", "Negative", FALSE)
positive_att_plot <- clogit_attribute_importance(positive_fi, "Positive", "Positive", FALSE)

# Combined plots
combined_plot_surveillance <- (negative_att_plot + positive_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Surveillance Attitude (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Surveillance_Attitude_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_surveillance, width = 12, height = 7, dpi = 300)

# Government Satisfaction ======================================================
# Subsetting the data
high_sat_fi   <- fi_df %>% filter(gov_sat_cat == 'High Satisfaction')
medium_sat_fi <- fi_df %>% filter(gov_sat_cat == 'Medium Satisfaction')
low_sat_fi    <- fi_df %>% filter(gov_sat_cat == 'Low Satisfaction')

# Generating plots
high_sat_att_plot   <- clogit_attribute_importance(high_sat_fi, "High Satisfaction", "High Satisfaction", FALSE)
medium_sat_att_plot <- clogit_attribute_importance(medium_sat_fi, "Medium Satisfaction", "Medium Satisfaction", FALSE)
low_sat_att_plot    <- clogit_attribute_importance(low_sat_fi, "Low Satisfaction", "Low Satisfaction", FALSE)

# Combined plots
combined_plot_govsat <- (high_sat_att_plot + medium_sat_att_plot + low_sat_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Government Satisfaction (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Government_Satisfaction_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_govsat, width = 12, height = 7, dpi = 300)

# Modern Tech Law ==========================================================
# Subsetting the data
appropriate_fi     <- fi_df %>% filter(modrn_tech_law_cat == 'Appropriate')
not_appropriate_fi <- fi_df %>% filter(modrn_tech_law_cat == 'Not Appropriate')

# Generating plots
appropriate_att_plot     <- clogit_attribute_importance(appropriate_fi, "Appropriate", "Appropriate", FALSE)
not_appropriate_att_plot <- clogit_attribute_importance(not_appropriate_fi, "Not Appropriate", "Not Appropriate", FALSE)

# Combined plots
combined_plot_techlaw <- (appropriate_att_plot + not_appropriate_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Modern Tech Law (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Modern_Tech_Law_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_techlaw, width = 12, height = 7, dpi = 300)

# Worried About Social Media ===================================================
# Subsetting the data
not_worried_sm_fi      <- fi_df %>% filter(worried_socialmedia == 'Not worried at All')
little_worried_sm_fi   <- fi_df %>% filter(worried_socialmedia == 'Little Worried')
very_worried_sm_fi     <- fi_df %>% filter(worried_socialmedia == 'Very Worried')
somewhat_worried_sm_fi <- fi_df %>% filter(worried_socialmedia == 'Somewhat Worried')

# Generating plots
not_worried_sm_att_plot      <- clogit_attribute_importance(not_worried_sm_fi, "Not worried at All", "Not worried at All", FALSE)
little_worried_sm_att_plot   <- clogit_attribute_importance(little_worried_sm_fi, "Little Worried", "Little Worried", FALSE)
very_worried_sm_att_plot     <- clogit_attribute_importance(very_worried_sm_fi, "Very Worried", "Very Worried", FALSE)
somewhat_worried_sm_att_plot <- clogit_attribute_importance(somewhat_worried_sm_fi, "Somewhat Worried", "Somewhat Worried", FALSE)

# Combined plots
combined_plot_sm <- (not_worried_sm_att_plot + little_worried_sm_att_plot + very_worried_sm_att_plot + somewhat_worried_sm_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Worried About Social Media (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Worried_Social_Media_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_sm, width = 12, height = 10, dpi = 300)

# Worried About E-Services =====================================================
# Subsetting the data
not_worried_eserv_fi      <- fi_df %>% filter(worried_eserv == 'Not worried at All')
little_worried_eserv_fi   <- fi_df %>% filter(worried_eserv == 'Little Worried')
very_worried_eserv_fi     <- fi_df %>% filter(worried_eserv == 'Very Worried')
somewhat_worried_eserv_fi <- fi_df %>% filter(worried_eserv == 'Somewhat Worried')

# Generating plots
not_worried_eserv_att_plot      <- clogit_attribute_importance(not_worried_eserv_fi, "Not worried at All", "Not worried at All", FALSE)
little_worried_eserv_att_plot   <- clogit_attribute_importance(little_worried_eserv_fi, "Little Worried", "Little Worried", FALSE)
very_worried_eserv_att_plot     <- clogit_attribute_importance(very_worried_eserv_fi, "Very Worried", "Very Worried", FALSE)
somewhat_worried_eserv_att_plot <- clogit_attribute_importance(somewhat_worried_eserv_fi, "Somewhat Worried", "Somewhat Worried", FALSE)

# Combined plots
combined_plot_eserv <- (not_worried_eserv_att_plot + little_worried_eserv_att_plot + very_worried_eserv_att_plot + somewhat_worried_eserv_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Worried About E-Services (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Worried_E_Services_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_eserv, width = 12, height = 10, dpi = 300)

# Tech Adoption ================================================================
# Subsetting the data
laggards_fi       <- fi_df %>% filter(tech_adoption_new == 'Laggards')
majority_fi       <- fi_df %>% filter(tech_adoption_new == 'Majority')
early_adopters_fi <- fi_df %>% filter(tech_adoption_new == 'Early Adopters')

# Generating plots
laggards_att_plot       <- clogit_attribute_importance(laggards_fi, "Laggards", "Laggards", FALSE)
majority_att_plot       <- clogit_attribute_importance(majority_fi, "Majority", "Majority", FALSE)
early_adopters_att_plot <- clogit_attribute_importance(early_adopters_fi, "Early Adopters", "Early Adopters", FALSE)

# Combined plots
combined_plot_techadopt <- (laggards_att_plot + majority_att_plot + early_adopters_att_plot) +
  plot_annotation(
    title = "Attribute Importance: Tech Adoption (Conditional Logit)",
    caption = "*Data collected from AGACA project under Daria Gritsenko"
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving 
save_path <- file.path(base_path, "Figures", "Subgroup", "C_Logit", "Tech_Adoption_Attribute_Importance.png")
ggsave(filename = save_path, plot = combined_plot_techadopt, width = 12, height = 7, dpi = 300)
