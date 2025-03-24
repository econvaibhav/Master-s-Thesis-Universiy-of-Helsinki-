# Header Start =================================================================
# File Name: Part4_AMCE_Subgroup.R
# Purpose: Subgroup wise AMCE plots 
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

generate_amce_plot <- function(data_subset, subset_label, subtitle, 
                             show_legend = TRUE, show_y = TRUE, save_graph = FALSE) {
  
  amce_data <- amce(data_subset, choice ~ decision + agency + mistake + interest + transparency + accountability, id = ~ID)
  
  legend_position <- if (show_legend) 'right' else 'none'
  
  plot <- ggplot(amce_data, aes(x = estimate, y = level, color = feature)) +
    geom_point(aes(shape = feature), size = 5) +  
    geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0.02) + 
    scale_color_manual(values = rep("black", 6)) +
    scale_shape_manual(values = c(8, 17, 4, 16, 6, 12)) +
    scale_y_discrete(limits = rev(levels(amce_data$level))) + 
    geom_vline(xintercept = 0, size = 1)+
    labs(
      x = 'AMCE',
      y = if (show_y) 'Attributes' else '',
      subtitle = subtitle
    ) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(face = 'bold', size = 10),
      axis.title.x = element_text(color = "black", size = 12, face = "bold"),
      plot.subtitle = element_text(color = 'black', size = 14, hjust = 0.5),
      axis.title.y = if (show_y) element_text(color = "black", size = 12, face = "bold") else element_blank(),
      axis.text.y = if (show_y) element_text() else element_blank(),
      plot.caption = element_text(face = "italic"),
      legend.position = legend_position
    )
  
  if (show_y) {
    plot <- plot + guides(
      y = guide_axis_nested(
        bracket = "square",
        key = key_range_manual(
          start = c(15, 12, 9, 7, 4, 1),
          end = c(18, 14, 11, 8, 6, 3),
          labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
        )
      )
    )
  } else {
    plot <- plot + guides(y = "none")
  }
  
  if (save_graph) {
    base_path = "C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav"
    file_name <- paste0(subset_label, ".png")
    save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", file_name)
    ggsave(filename = save_path, plot = plot, width = 12, height = 10, dpi = 300)
  }
  
  return(plot)
}


# Gender =======================================================================

# Subsetting the data 
male_fi <- fi_df %>% filter(gender_new == 'Male')
female_fi <- fi_df %>% filter(gender_new == 'Female')

# Running AMCE function
male_plot <- generate_amce_plot(male_fi, "Male", "Males", 
                              show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
female_plot <- generate_amce_plot(female_fi, "Female", "Females", 
                                show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_gender <- (male_plot + female_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Gender',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Gender_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_gender, width = 12, height = 7, dpi = 300)

# Age ==========================================================================

# Subsetting the data
young_fi <- fi_df %>% filter(age_new == 'Young Adults')
older_fi <- fi_df %>% filter(age_new == 'Older Adults')
middle_aged_fi <- fi_df %>% filter(age_new == 'Middle-Aged')

# Running AMCE function
young_plot <- generate_amce_plot(young_fi, "Young Adults", "Young Adults",
                               show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
older_plot <- generate_amce_plot(older_fi, "Older Adults", "Older Adults",
                               show_legend = FALSE, show_y = FALSE, save_graph = FALSE)
middle_aged_plot <- generate_amce_plot(middle_aged_fi, "Middle-Aged", "Middle-Aged",
                                     show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_age <- (young_plot + older_plot + middle_aged_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Age',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Age_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_age, width = 12, height = 10, dpi = 300)

# Education ====================================================================

# Subsetting the data
no_college_fi <- fi_df %>% filter(edu_new == 'No College/University')
college_fi <- fi_df %>% filter(edu_new == 'College/University Education')

# Running AMCE function
no_college_plot <- generate_amce_plot(no_college_fi, "No College/University", "No College/University",
                                    show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
college_plot <- generate_amce_plot(college_fi, "College/University Education", "College/University Education",
                                 show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_edu <- (no_college_plot + college_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Education',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Education_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_edu, width = 12, height = 7, dpi = 300)

# Residence ====================================================================

# Subsetting the data
urban_fi <- fi_df %>% filter(Rural_Urban_Group == 'Urban')
rural_fi <- fi_df %>% filter(Rural_Urban_Group == 'Rural')

# Running AMCE function
urban_plot <- generate_amce_plot(urban_fi, "Urban", "Urban",
                               show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
rural_plot <- generate_amce_plot(rural_fi, "Rural", "Rural",
                               show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_residence <- (urban_plot + rural_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Residence',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Residence_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_residence, width = 12, height = 7, dpi = 300)

# Employment Status ============================================================

# Subsetting the data
retired_fi <- fi_df %>% filter(Employment_Status_Group == 'Retired')
students_fi <- fi_df %>% filter(Employment_Status_Group == 'Students')
employed_fi <- fi_df %>% filter(Employment_Status_Group == 'Employed')
others_fi <- fi_df %>% filter(Employment_Status_Group == 'Others')

# Running AMCE function
retired_plot <- generate_amce_plot(retired_fi, "Retired", "Retired",
                                 show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
students_plot <- generate_amce_plot(students_fi, "Students", "Students",
                                  show_legend = FALSE, show_y = FALSE, save_graph = FALSE)
employed_plot <- generate_amce_plot(employed_fi, "Employed", "Employed",
                                  show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
others_plot <- generate_amce_plot(others_fi, "Others", "Others",
                                show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_employment <- (retired_plot + students_plot + employed_plot + others_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Employment Status',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Employment_Status_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_employment, width = 12, height = 10, dpi = 300)

# Surveillance Attitude ========================================================

# Subsetting the data
negative_fi <- fi_df %>% filter(surveillance_attitude_new == 'Negative')
positive_fi <- fi_df %>% filter(surveillance_attitude_new == 'Positive')

# Running AMCE function
negative_plot <- generate_amce_plot(negative_fi, "Negative", "Negative",
                                  show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
positive_plot <- generate_amce_plot(positive_fi, "Positive", "Positive",
                                  show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_surveillance <- (negative_plot + positive_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Surveillance Attitude',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Surveillance_Attitude_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_surveillance, width = 12, height = 7, dpi = 300)

# Government Satisfaction ======================================================

# Subsetting the data
high_sat_fi <- fi_df %>% filter(gov_sat_cat == 'High Satisfaction')
medium_sat_fi <- fi_df %>% filter(gov_sat_cat == 'Medium Satisfaction')
low_sat_fi <- fi_df %>% filter(gov_sat_cat == 'Low Satisfaction')

# Running AMCE function
high_sat_plot <- generate_amce_plot(high_sat_fi, "High Satisfaction", "High Satisfaction",
                                  show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
medium_sat_plot <- generate_amce_plot(medium_sat_fi, "Medium Satisfaction", "Medium Satisfaction",
                                    show_legend = FALSE, show_y = FALSE, save_graph = FALSE)
low_sat_plot <- generate_amce_plot(low_sat_fi, "Low Satisfaction", "Low Satisfaction",
                                 show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_govsat <- (high_sat_plot + medium_sat_plot + low_sat_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Government Satisfaction',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Government_Satisfaction_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_govsat, width = 12, height = 7, dpi = 300)

# Modern Tech Law ==============================================================

# Subsetting the data
appropriate_fi <- fi_df %>% filter(modrn_tech_law_cat == 'Appropriate')
not_appropriate_fi <- fi_df %>% filter(modrn_tech_law_cat == 'Not Appropriate')

# Running AMCE function
appropriate_plot <- generate_amce_plot(appropriate_fi, "Appropriate", "Appropriate",
                                     show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
not_appropriate_plot <- generate_amce_plot(not_appropriate_fi, "Not Appropriate", "Not Appropriate",
                                         show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_techlaw <- (appropriate_plot + not_appropriate_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Modern Tech Law',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Modern_Tech_Law_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_techlaw, width = 12, height = 7, dpi = 300)


# Worried About Social Media ===================================================

# Subsetting the data
not_worried_sm_fi <- fi_df %>% filter(worried_socialmedia == 'Not worried at All')
little_worried_sm_fi <- fi_df %>% filter(worried_socialmedia == 'Little Worried')
very_worried_sm_fi <- fi_df %>% filter(worried_socialmedia == 'Very Worried')
somewhat_worried_sm_fi <- fi_df %>% filter(worried_socialmedia == 'Somewhat Worried')

# Running AMCE function
not_worried_sm_plot <- generate_amce_plot(not_worried_sm_fi, "Not worried at All", "Not worried at All",
                                        show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
little_worried_sm_plot <- generate_amce_plot(little_worried_sm_fi, "Little Worried", "Little Worried",
                                           show_legend = FALSE, show_y = FALSE, save_graph = FALSE)
very_worried_sm_plot <- generate_amce_plot(very_worried_sm_fi, "Very Worried", "Very Worried",
                                         show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
somewhat_worried_sm_plot <- generate_amce_plot(somewhat_worried_sm_fi, "Somewhat Worried", "Somewhat Worried",
                                             show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_sm <- (not_worried_sm_plot + little_worried_sm_plot + very_worried_sm_plot + somewhat_worried_sm_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Worried About Social Media',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Worried_Social_Media_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_sm, width = 12, height = 10, dpi = 300)

# Worried About E-Services =====================================================

# Subsetting the data
not_worried_eserv_fi <- fi_df %>% filter(worried_eserv == 'Not worried at All')
little_worried_eserv_fi <- fi_df %>% filter(worried_eserv == 'Little Worried')
very_worried_eserv_fi <- fi_df %>% filter(worried_eserv == 'Very Worried')
somewhat_worried_eserv_fi <- fi_df %>% filter(worried_eserv == 'Somewhat Worried')

# Running AMCE function
not_worried_eserv_plot <- generate_amce_plot(not_worried_eserv_fi, "Not worried at All", "Not worried at All",
                                           show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
little_worried_eserv_plot <- generate_amce_plot(little_worried_eserv_fi, "Little Worried", "Little Worried",
                                              show_legend = FALSE, show_y = FALSE, save_graph = FALSE)
very_worried_eserv_plot <- generate_amce_plot(very_worried_eserv_fi, "Very Worried", "Very Worried",
                                            show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
somewhat_worried_eserv_plot <- generate_amce_plot(somewhat_worried_eserv_fi, "Somewhat Worried", "Somewhat Worried",
                                                show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_eserv <- (not_worried_eserv_plot + little_worried_eserv_plot + very_worried_eserv_plot + somewhat_worried_eserv_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Worried About E-Services',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Worried_E_Services_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_eserv, width = 12, height = 10, dpi = 300)

# Tech Adoption ================================================================

# Subsetting the data
laggards_fi <- fi_df %>% filter(tech_adoption_new == 'Laggards')
majority_fi <- fi_df %>% filter(tech_adoption_new == 'Majority')
early_adopters_fi <- fi_df %>% filter(tech_adoption_new == 'Early Adopters')

# Running AMCE function
laggards_plot <- generate_amce_plot(laggards_fi, "Laggards", "Laggards",
                                  show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
majority_plot <- generate_amce_plot(majority_fi, "Majority", "Majority",
                                  show_legend = FALSE, show_y = FALSE, save_graph = FALSE)
early_adopters_plot <- generate_amce_plot(early_adopters_fi, "Early Adopters", "Early Adopters",
                                        show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_techadopt <- (laggards_plot + majority_plot + early_adopters_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Tech Adoption',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "AMCE", "Tech_Adoption_AMCE.png")
ggsave(filename = save_path, plot = combined_plot_techadopt, width = 12, height = 7, dpi = 300)


# Task 1 vs 2 ================================================================

# Subsetting the data
task1 <- fi_df %>% filter(Task == '1')
task2 <- fi_df %>% filter(Task == '2')

# Running MM function
task1_plot <- generate_amce_plot(task1, "1", "Task 1",
                               show_legend = FALSE, show_y = TRUE, save_graph = FALSE)
task2_plot <- generate_amce_plot(task2, "2", "Task 2",
                               show_legend = FALSE, show_y = FALSE, save_graph = FALSE)

# Combining Subgroup Plots
combined_plot_t1vst2 <- (task1_plot + task2_plot) +
  plot_annotation(
    title = 'AMCE Plot for Finland: Task 1 vs. Task 2',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Subgroup", "MM", "Tech_Adoption_MM.png")
ggsave(filename = save_path, plot = combined_plot_techadopt, width = 12, height = 7, dpi = 300)


# End===========================================================================
