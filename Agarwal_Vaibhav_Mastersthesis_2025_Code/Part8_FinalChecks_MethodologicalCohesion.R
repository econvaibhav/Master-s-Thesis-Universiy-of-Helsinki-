# Header Start =================================================================
# File Name: Part8_FinalChecks_MethodologicalCohesion.R
# Purpose: Running final checks and checkig for methodological cohesion. NOTE: This part was run before, however organised in last section
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
library(patchwork) # version patchwork_1.2.0
library(stargazer) # version stargazer_5.2.3   

# Loading Ganter's Modified Functions 
# Modification: Ganter's code used slicing relevant to his data so was not easily reproducible, Hence need to be changed relevant to current data; otherwise worked well
base_path = "C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav"
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "conjacp.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionVCOVCluster.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionTableGraph.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "theme_graph.R"))

#detach("package:gmnl", unload = T) detach, if loaded
#detach("package:mlogit", unload =T) detach, if loaded
install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos = NULL, type = "source")
.libPaths("C:/Users/User/AppData/Local/R/win-library/4.3")
install.packages(c("Formula", "zoo", "lmtest", "statmod", "MASS", "Rdpack"))
.libPaths("C:/Users/User/AppData/Local/R/win-library/4.3")


library(mlogit) # version mlogit_1.0-2
library(gmnl) # version gmnl_1.1-3.2
library(tidyverse) # version tidyverse_2.0.0  
library(xtable) # version xtable_1.8-4 


# Loading Data (All) ===========================================================

fi_df <- read.csv(file.path(base_path, "Data", "fi_df.csv"))
# Using from env. 

# Baseline Check (for dummy variables) =========================================

## Remodifying the data ========================================================
# Test 1: Changing all baselines 
# Test 2: Changing baseline of only 1 attribute 
# Test 3: Changing baseline of half the attributes (most important ones)

# Run Part0_Cleaning.R before running this else will gnerate an error 
fi_df <- fi_df %>%
  mutate(
    decision = relevel(decision, ref = "Theft Person"),
    agency = relevel(agency, ref = "Alg. Only"),
    mistake = relevel(mistake, ref = "Humans"),
    interest = relevel(interest, ref = "Private"),
    transparency = relevel(transparency, ref = "Police"),
    accountability = relevel(accountability, ref = "Developer (Acc)")
  )

## Testing Zone ================================================================

amce_data <- amce(fi_df, choice ~ decision + agency + mistake + interest + transparency + accountability, id = ~ID)

amceplots <- ggplot(amce_data, aes(x = estimate, y = level, color = feature)) +
  geom_point(aes(shape = feature), size = 5) +  
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0.02) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_shape_manual(values = c(8, 17, 4, 16, 6, 12)) +
  scale_y_discrete(limits = rev(levels(mm_data$level))) + 
  geom_vline(xintercept = 0, size = 1)+
  labs(
    title = 'AMCE Plot for Finland',
    x = 'Average Marginal Component Effect',
    y = 'Attributes',
    subtitle = 'Entire Dataset',
    caption = '*Data collected from AGACA project'
  ) +
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
    y = guide_axis_nested(
      bracket = "square",
      key = key_range_manual(
        start = c(15, 12, 9, 7, 4, 1),
        end = c(18, 14, 11, 8, 6, 3),
        labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
      )
    )
  )

## Test Fail ===================================================================

# This code is not included in the entire code as it fails the baseline check
# Relative Attribute Importance not included 
# Attribute Level Importance included

df <- fi_df %>% dplyr::select(Count, Task:choice)

df_dummy <- fastDummies::dummy_cols(df,
                                    select_columns = c("decision", "agency", "mistake", "interest", "transparency", "accountability"),
                                    remove_first_dummy = TRUE
)

df_model <- df_dummy %>%
  dplyr::select(-c(decision, agency, mistake, interest, transparency, accountability))

df_model <- df_model %>% mutate(stratum = paste(Count, Task, sep = "_"))

model <- clogit(
  choice ~ 
    `decision_Robbery Person` + 
    `decision_Robbery Area` + 
    `decision_Theft Area` +
    `agency_Alg Overruled` +
    `agency_Alg. Recommends` +
    `mistake_Algorithms` +
    `mistake_Same` +
    `interest_Public` +
    `transparency_Developer` +
    `transparency_Experts` +
    `accountability_Experts (Acc)` +
    `accountability_Police (Acc)` +
    strata(stratum),
  data = df_model
)

coefs <- coef(model)
names(coefs) <- gsub("`", "", names(coefs))

ranges <- c(
  decision = abs(max(coefs[c("decision_Robbery Person", "decision_Robbery Area", "decision_Theft Area")]) -
                   min(coefs[c("decision_Robbery Person", "decision_Robbery Area", "decision_Theft Area")])),
  agency = abs(max(coefs[c("agency_Alg Overruled", "agency_Alg. Recommends")]) -
                 min(coefs[c("agency_Alg Overruled", "agency_Alg. Recommends")])),
  mistake = abs(max(coefs[c("mistake_Algorithms", "mistake_Same")]) -
                  min(coefs[c("mistake_Algorithms", "mistake_Same")])),
  interest = abs(max(c(coefs["interest_Public"], 0)) -
                   min(c(coefs["interest_Public"], 0))),
  transparency = abs(max(coefs[c("transparency_Developer", "transparency_Experts")]) -
                       min(coefs[c("transparency_Developer", "transparency_Experts")])),
  accountability = abs(max(coefs[c("accountability_Experts (Acc)", "accountability_Police (Acc)")]) -
                         min(coefs[c("accountability_Experts (Acc)", "accountability_Police (Acc)")]))
)

rel_imp <- (ranges / sum(ranges)) * 100
df_plot <- data.frame(
  Attribute = names(rel_imp),
  Importance = as.numeric(rel_imp)
)

pretty_names <- c(
  "decision" = "Decision",
  "agency" = "Agency",
  "mistake" = "Mistake",
  "interest" = "Interest",
  "transparency" = "Transparency",
  "accountability" = "Accountability"
)
df_plot$Attribute <- pretty_names[df_plot$Attribute]

dummyset1 <-ggplot(df_plot, aes(x = reorder(Attribute, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = round(Importance, 1)), hjust = -0.1, size = 4.5) +
  coord_flip() +
  labs(
    title = "Baseline Choice 2",
    x = "Attribute",
    y = "Importance (%)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0, max(df_plot$Importance) * 1.08)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10)
  )

# Dummyset 1 saved in env before

combined_plot_baselinecheck <- (dummyset2 + dummyset1) +
  plot_annotation(
    title = 'Different Baseline Check',
    subtitle = "Randomly changing baseline used to check for differences",
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, hjust = 0.5),
        plot.subtitle  = element_text(color = "black", size = 12, hjust = 0.5)

        )

# Saving the plots
save_path <- file.path(base_path, "Figures", "Full Data", "Cohesion", "Fail_BaselineCheck.png")
ggsave(filename = save_path, plot = combined_plot_baselinecheck, width = 12, height = 10, dpi = 300)



# Task 1 vs. Task 2  ===========================================================

task1 <- fi_df %>% filter(Task ==1)
task2 <- fi_df %>% filter(Task ==2)
  
  
amce_data_task1 <- amce(task1, choice ~ decision + agency + mistake + interest + transparency + accountability, id = ~ID)

amceplots_task1 <- ggplot(amce_data_task1, aes(x = estimate, y = level, color = feature)) +
  geom_point(aes(shape = feature), size = 5) +  
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0.02) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_shape_manual(values = c(8, 17, 4, 16, 6, 12)) +
  scale_y_discrete(limits = rev(levels(mm_data$level))) + 
  geom_vline(xintercept = 0, size = 1)+
  labs(
    title = 'AMCE - Task 1',
    x = 'AMCE',
    y = 'Attributes'
  ) +
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
    y = guide_axis_nested(
      bracket = "square",
      key = key_range_manual(
        start = c(15, 12, 9, 7, 4, 1),
        end = c(18, 14, 11, 8, 6, 3),
        labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
      )
    )
  )


amce_data_task2 <- amce(task2, choice ~ decision + agency + mistake + interest + transparency + accountability, id = ~ID)

amceplots_task2 <- ggplot(amce_data_task2, aes(x = estimate, y = level, color = feature)) +
  geom_point(aes(shape = feature), size = 5) +  
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0.02) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_shape_manual(values = c(8, 17, 4, 16, 6, 12)) +
  scale_y_discrete(limits = rev(levels(mm_data$level))) + 
  geom_vline(xintercept = 0, size = 1)+
  labs(
    title = 'AMCE - Task 2',
    x = 'AMCE',
    y = 'Attributes'
  ) +
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
    y = guide_axis_nested(
      bracket = "square",
      key = key_range_manual(
        start = c(15, 12, 9, 7, 4, 1),
        end = c(18, 14, 11, 8, 6, 3),
        labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
      )
    )
  )

## Checking Comparison =========================================================
combined_plot_t1vst2 <- (amceplots_task1 + amceplots_task2) +
  plot_annotation(
    title = 'AMCE Task1 vs. Task2',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))

# Saving the plots
save_path <- file.path(base_path, "Figures", "Full Data", "Cohesion", "Task1_vs_Task2.png")
ggsave(filename = save_path, plot = combined_plot_t1vst2, width = 12, height = 10, dpi = 300)


# End ==========================================================================


