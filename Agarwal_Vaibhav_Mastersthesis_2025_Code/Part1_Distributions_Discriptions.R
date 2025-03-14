# Header Start =================================================================
# File Name: Part1_Distributions_Discriptions.R
# Purpose: Looking at the distributions of the variables in the data and thier descriptions from summary tables 
# Author: Vaibhav Agarwal
# R version: 4.3.2 (2023-10-31 ucrt)
# Last Modified: 11th April 2025
# Header End ===================================================================


# Libraries ====================================================================

library(tidyverse) # version tidyverse_2.0.0
library(ggplot2) # version ggplot2_3.5.0
library(scales) # version scales_1.3.0
library(stringr) # version stringr_1.5.1 
library(gtsummary) # version gtsummary_2.1.0   
library(gt) # version gt_0.11.1         
library(Hmisc) # version Hmisc_5.1-1
library(vtable) # version vtable_1.4.8       
library(summarytools) # version summarytools_1.0.1 


# Loading Data (All) ===========================================================

#fi_df <- read.csv(file.path(base_path, "Data", "fi_df.csv"))
# Using from env. 

# Distributions ================================================================

## Function =====================================================================

plot_distributions <- function(df, plot_titles, plot_subtitles, xaxis_labels, file_names) {
  
  cols <- c(
    
    "modrn_tech_law_cat",
    "gov_sat_cat",    

    "Trust-current_gov",
    "Trust-parliment",
    "Trust-localgov",
    "Trust-egov",
    "Trust-officials",
    "Trust-courts",
    "Trust-police",
    
    "surveillance_attitude_new", 
    
    "Worried_about_personal_info_leak-social media",
    "Worried_about_personal_info_leak-online_purchases",
    "Worried_about_personal_info_leak-e_public_services",
    
    "Tech_Can_Solve-Economic_Growth",
    "Tech_Can_Solve-National_Security",
    "Tech_Can_Solve-Social_Justice",
    "Tech_Can_Solve-Environment",
    "Tech_Can_Solve-Good_Healthcare",
    "Tech_Can_Solve-Good_Education",
    "Tech_Can_Solve-Crime_Rate",
    "Tech_Can_Solve-Well_Paid_Job",
    
    "Age",
    "Gender",
    "Education_level",
    "Employment_Status",
    "Residence",
    "Province",
    "language",
    "Internet_Use",
    "Task",
    "Option",
    
   
    "age_new",
    "gender_new",
    "tech_adoption_new"       
  )
  
  
  
  
  # Loop through the selected columns
  for (i in seq_along(cols)) {
    col <- cols[i]
    
    # Remove NA values only for the current column
    df_col <- df %>% filter(!is.na(!!sym(col)))
    
    # If the column is numeric (continuous variable)
    if (is.numeric(df[[col]])) {
      p <- ggplot(df_col, aes(x = !!sym(col))) +
        geom_histogram(fill = "black", bins = 30) +
        labs(
          title = plot_titles[i],
          subtitle = plot_subtitles[i],
          x = xaxis_labels[i],  # Custom x-axis label
          y = 'Frequency',
          caption = "*Data collected for AGACA project"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(color = "black", size = 12, face = "bold"),
          axis.text.y = element_text(face = 'bold', size = 10),
          axis.title.y = element_text(color = "black", size = 12, face = "bold"),
          plot.title = element_text(color = "black", size = 14, hjust = 0.5)
        )
      
      # Saving the plot
      ggsave(filename = paste0("C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav/Figures/Full Data/Distributions/", file_names[i], ".png"),
             plot = p, width = 900 / 72, height = 700 / 72, dpi = 72)
      
    }
    
    # If the column is categorical, here defined as less than 15 factors 
    else if (is.factor(df[[col]]) || length(unique(df[[col]])) < 15) {
      df_col[[col]] <- factor(df_col[[col]], levels = unique(df_col[[col]])[table(df_col[[col]]) > 0])
      if (all(!is.na(as.numeric(as.character(levels(df_col[[col]]))))) ) {
        df_col[[col]] <- factor(df_col[[col]], levels = as.character(sort(as.numeric(levels(df_col[[col]])))))
      }
      
      counts <- as.data.frame(table(df_col[[col]]))
      colnames(counts) <- c(col, "count")
      
      counts[[col]] <- str_to_title(counts[[col]])
      
      # Create the plot for categorical variables
      p <- ggplot(counts, aes(x = !!sym(col), y = count)) +
        geom_bar(stat = "identity", fill = "black", width = 0.5) +  # Bar plot
        geom_text(aes(label = count), vjust = -0.3, size = 4, fontface = "bold") +  # Add text on top
        labs(
          title = plot_titles[i],
          subtitle = plot_subtitles[i],
          x = str_to_title(xaxis_labels[i]),  # Convert x-axis label to title case
          y = 'Count',
          caption = "*Data collected for AGACA project"
        ) +
        ylim(0, max(counts$count) * 1.1) +  # Increase y-axis limit by 10%
        theme_bw() +
        theme(
          axis.text.x = element_text(face = 'bold', size = 10),
          axis.title.x = element_text(color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(color = 'black', size = 10, hjust = 0.5),
          axis.title.y = element_text(color = "black", size = 12, face = "bold"),
          plot.title = element_text(color = "black", size = 14, hjust = 0.5),
          plot.caption = element_text(face = "italic"),
          legend.position = 'none',  # No legend needed for a single group
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = 'bold', size = 10)  # Styling y-axis text
        )
      
      # Rotating the xlabels for better fits, if they are large 
      if (any(nchar(levels(df_col[[col]])) > 3)) {
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      
      ggsave(filename = paste0("C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav/Figures/Full Data/Distributions/", file_names[i], ".png"),
             plot = p, width = 1200 / 175, height = 900 / 175, dpi = 175)
    }
  }
}

## Saving ================================================================

# Plot Titles 
plot_titles <- c("Distribution of Government Technology Appropriateness",
                 "Distribution of Government Satisfaction", "Distribution of Trust in Current Government", 
                 "Distribution of Trust in Parliament", "Distribution of Trust in Local Government", 
                 "Distribution of Trust in E-Government", "Distribution of Trust in Officials", 
                 "Distribution of Trust in Courts", "Distribution of Trust in Police",
                 "Distribution of Attitude towards Government Surveillance",
                 "Distribution of Concern about Social Media Privacy", 
                 "Distribution of Concern about Online Purchases Privacy",
                 "Distribution of Concern about E-Public Services Privacy", 
                 "Distribution of Technology’s Impact on Economic Growth", 
                 "Distribution of Technology’s Impact on National Security", 
                 "Distribution of Technology’s Impact on Social Justice", 
                 "Distribution of Technology’s Impact on Environment", 
                 "Distribution of Technology’s Impact on Healthcare", 
                 "Distribution of Technology’s Impact on Education", 
                 "Distribution of Technology’s Impact on Crime Rate", 
                 "Distribution of Technology’s Impact on Well-Paid Jobs", 
                 "Distribution of Age", "Distribution of Gender", "Distribution of Education Level", 
                 "Distribution of Employment Status", "Distribution of Residence", 
                 "Distribution of Province", "Distribution of Language", "Distribution of Internet Use", 
                 "Distribution of Task", "Distribution of Option", "Distribution of Age (*Modified)", 
                 "Distribution of Gender (*Modified)", "Distribution of Trust in Police (*Modified)", 
                 "Distribution of Surveillance Attitude (*Modified)", "Distribution of Technology Adoption (*Modified)")

# Plot Subtitles 
plot_subtitles <- rep("Country: Finland", length.out = length(plot_titles))  # Subtitles for all

xaxis_labels <- c("Appropriateness", "Satisfaction", "Current Gov Trust", "Parliament Trust", "Local Gov Trust", 
                  "E-Gov Trust", "Officials Trust", "Courts Trust", "Police Trust", 
                  "Surveillance Attitude", "Privacy - Social Media", "Privacy - Online Purchases", 
                  "Privacy - E-Public Service Leaks", "Economic Growth", "National Security", 
                  "Social Justice", "Environment", "Healthcare", "Education", 
                  "Crime Rate", "Well-Paid Jobs", "Age", "Gender", "Education Level", 
                  "Employment Status", "Residence", "Province", "Language", "Internet Use", 
                  "Task", "Option", "Age (*Modified)", "Gender (*Modified)", "Trust in Police (*Modified)", 
                  "Surveillance Attitude (*Modified)", "Technology Adoption (*Modified)")

# File names :)
file_names <- c("FI_tech_adpt", "FI_gov_sat", "FI_trust_current_gov", "FI_trust_parliament", 
                "FI_trust_local_gov", "FI_trust_e_gov", "FI_trust_officials", "FI_trust_courts", 
                "FI_trust_police", "FI_surveillance", "FI_privacy_social", 
                "FI_privacy_purchase", "FI_privacy_epub", "FI_econ_growth", "FI_nat_sec", 
                "FI_social_justice", "FI_environment", "FI_healthcare", "FI_education", 
                "FI_crime", "FI_jobs", "FI_age", "FI_gender", "FI_education_level", 
                "FI_employment", "FI_residence", "FI_province", "FI_language", "FI_internet", 
                "FI_task", "FI_option", "FI_age_mod", "FI_gender_mod", "FI_trust_police_mod", 
                "FI_surveillance_mod", "FI_tech_adopt_mod")

# Running the function
plot_distributions(fi_df, plot_titles, plot_subtitles, xaxis_labels, file_names)


# Table - Description ==========================================================

## Demographics ========================================================

demographic_table <- fi_df   %>%
  distinct(ID, .keep_all = TRUE) %>% 
  ungroup() %>%
  select(age_new, gender_new, edu_new, Rural_Urban_Group, Employment_Status_Group, lang_new)


tbl <- tbl_summary(
  demographic_table, 
  missing_text = "Missing",
  label = list(
    age_new ~ "Age",
    gender_new ~ "Gender",
    edu_new ~ "Education Level",
    Rural_Urban_Group ~ "Area",
    Employment_Status_Group ~ "Employment Status",
    lang_new ~ "Language"
  )
) 

tbl %>%
  as_gt() %>%
  as_latex() %>%
  cat()

## Subgroups ===================================================================

subgroups_table <- fi_df   %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup() %>%
  select(surveillance_attitude_new, gov_sat_cat, modrn_tech_law_cat, worried_socialmedia, worried_eserv, tech_adoption_new)


tbl <- tbl_summary(
  subgroups_table, 
  missing_text = "Missing",
  label = list(
    surveillance_attitude_new ~ 'Surveillance Attitude' ,
    gov_sat_cat ~ 'FI Government Satisfaction',
    modrn_tech_law_cat ~ 'Modern Tech. in Law',
    worried_socialmedia ~ 'Worried - Privacy - Social Media',
    worried_eserv ~ 'Worried - Privacy - Public e-services',
    tech_adoption_new ~ 'Technology Adoption'
  )
) 

tbl %>%
  as_gt() %>%
  as_latex() %>%
  cat()

## Robustness Checks ===========================================================

### Chi-sq Test of Ind =========================================================

attributes <- c("decision", "agency", "mistake", "interest", "transparency", "accountability")
subgroups <- c("age_new", "gender_new", "edu_new", "Rural_Urban_Group",
               "Employment_Status_Group", "lang_new", "surveillance_attitude_new",
               "gov_sat_cat", "modrn_tech_law_cat", "worried_socialmedia",
               "worried_eserv", "tech_adoption_new")

# Empty df 
test_results <- data.frame(
  attribute = character(),
  subgroup = character(),
  chisq_stat = numeric(),
  df = numeric(),
  p_value = numeric(),
  p_adjusted = numeric(),
  stringsAsFactors = FALSE
)

for (attr in attributes) {
  for (sub in subgroups) {
    tab <- table(fi_df[[sub]], fi_df[[attr]])
    if (nrow(tab) > 1 && ncol(tab) > 1) {
      test <- chisq.test(tab)
      test_results <- rbind(test_results, data.frame(
        attribute = attr,
        subgroup = sub,
        chisq_stat = as.numeric(test$statistic),
        df = as.numeric(test$parameter),
        p_value = test$p.value,
        p_adjusted = NA
      ))
    }
  }
}

# Apply a multiple testing correction (Bonferroni)
test_results$p_adjusted <- p.adjust(test_results$p_value, method = "bonferroni")

# Function to generate a LaTeX table string from the test results using ChatGPT
generate_latex_table <- function(results) {
  latex_table <- "\\begin{table}[h]\n\\centering\n\\begin{tabular}{@{}l l c c c c@{}}\n\\toprule\n"
  latex_table <- paste0(latex_table, "Attribute & Subgroup & $\\chi^2$ Statistic & df & p-value & Adjusted p-value \\\\\n\\midrule\n")
  
  for (attr in unique(results$attribute)) {
    attr_print <- gsub("_", "\\\\_", attr)
    attr_rows <- results %>% filter(attribute == attr)
    first_row <- TRUE
    for (i in 1:nrow(attr_rows)) {
      row <- attr_rows[i, ]
      sub_print <- gsub("_", "\\\\_", row$subgroup)
      
      chisq_str <- format(round(row$chisq_stat, 3), nsmall = 3)
      p_val_str <- format(round(row$p_value, 3), nsmall = 3)
      padj_str <- format(round(row$p_adjusted, 3), nsmall = 3)
      
      if (first_row) {
        latex_table <- paste0(latex_table, attr_print, " & ", sub_print, " & ", 
                              chisq_str, " & ", row$df, " & ", p_val_str, 
                              " & ", padj_str, " \\\\\n")
        first_row <- FALSE
      } else {
        latex_table <- paste0(latex_table, " & ", sub_print, " & ", 
                              chisq_str, " & ", row$df, " & ", p_val_str, 
                              " & ", padj_str, " \\\\\n")
      }
    }
    latex_table <- paste0(latex_table, "\\midrule\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n")
  latex_table <- paste0(latex_table, "\\caption{Chi-Square Test Results Across Subgroups}\n")
  latex_table <- paste0(latex_table, "\\label{tab:test_results}\n\\end{table}\n")
  
  return(latex_table)
}

latex_code <- generate_latex_table(test_results)
cat(latex_code)

### Sample size and descriptive statistics =====================================

rbchecks_table <- fi_df %>% 
  select(age_new, gender_new, edu_new, Rural_Urban_Group, Employment_Status_Group, lang_new,surveillance_attitude_new, gov_sat_cat, modrn_tech_law_cat, worried_socialmedia, worried_eserv, tech_adoption_new, decision, agency, interest, mistake, transparency, accountability) %>% 
  ungroup()

# df with variable labels
var.labs <- data.frame(var = c("surveillance_attitude_new", "gov_sat_cat", "modrn_tech_law_cat", 
                               "worried_socialmedia", "worried_eserv", "tech_adoption_new",
                               "age_new", "gender_new", "edu_new", "Rural_Urban_Group", 
                               "Employment_Status_Group", "lang_new"),
                       labels = c("Surveillance Attitude", "FI Government Satisfaction", "Modern Tech. in Law", 
                                  "Worried - Privacy - Social Media", "Worried - Privacy - Public e-services", 
                                  "Technology Adoption", "Age", "Gender", "Education Level", "Area", 
                                  "Employment Status", "Language"))

# Define  grouping variables
group_vars <- c("decision", "agency", "interest", "mistake", "transparency", "accountability")

# table for each attribute 
for (group_var in group_vars) {
  cat(paste("\\section*{Table for Group:", group_var, "}\n"))
  
  print(st(rbchecks_table,
           labels = var.labs,
           vars = c("surveillance_attitude_new", "gov_sat_cat", "modrn_tech_law_cat", 
                    "worried_socialmedia", "worried_eserv", "tech_adoption_new",
                    "age_new", "gender_new", "edu_new", "Rural_Urban_Group", 
                    "Employment_Status_Group", "lang_new"),
           group = group_var, group.test = TRUE,
           out = 'latex'))
  
  cat("\n\n")  
}

var.labs <- data.frame(var = c("surveillance_attitude_new", "gov_sat_cat", "modrn_tech_law_cat", 
                               "worried_socialmedia", "worried_eserv", "tech_adoption_new",
                               "age_new", "gender_new", "edu_new", "Rural_Urban_Group", 
                               "Employment_Status_Group", "lang_new"),
                       labels = c("Surveillance Attitude", "FI Government Satisfaction", "Modern Tech. in Law", 
                                  "Worried - Privacy - Social Media", "Worried - Privacy - Public e-services", 
                                  "Technology Adoption", "Age", "Gender", "Education Level", "Area", 
                                  "Employment Status", "Language"))

print(st(rbchecks_table,
         labels = var.labs,
         vars = c("surveillance_attitude_new", "gov_sat_cat", "modrn_tech_law_cat", 
                  "worried_socialmedia", "worried_eserv", "tech_adoption_new",
                  "age_new", "gender_new", "edu_new", "Rural_Urban_Group", 
                  "Employment_Status_Group", "lang_new"),
         out = 'viewer'))

# End ==========================================================================

