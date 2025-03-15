# Header Start =================================================================
# File Name: Part2_MM_AMCE_ACP.R
# Purpose: Generating MM, AMCE, ACP and Conditional Logit Graphs 
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
library(lmtest) #version lmtest_0.9-40

# Loading Ganter's Modified Functions 
# Modification: Ganter's code used slicing relevant to his data so was not easily reproducible, Hence need to be changed relevant to current data; otherwise worked well
base_path = "C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav"
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "conjacp.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionVCOVCluster.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "FunctionTableGraph.R"))
source(file.path(base_path, "Code", "Functions_Ganter_Modified", "theme_graph.R"))


# Loading Data (All) ===========================================================

#fi_df <- read.csv(file.path(base_path, "Data", "fi_df.csv"))
# Using from env. 

# MM ===========================================================================

mm_data <- mm(fi_df, choice ~ decision + agency + mistake + interest + transparency + accountability, id = ~ID)

mmplots <- ggplot(mm_data, aes(x = estimate, y = level, color = feature)) +
  geom_point(aes(shape = feature), size = 5) +  
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0.02) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black")) +
  scale_shape_manual(values = c(8, 17, 4, 16, 6, 12)) +
  scale_y_discrete(limits = rev(levels(mm_data$level))) + 
  labs(
    x = 'Marginal Means',
    y = 'Attributes',
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = 'bold', size = 10),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(color = 'black', size = 10, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    plot.title = element_text(color = "black", size = 14, hjust = 0.5),
    plot.caption = element_text(face = "italic"),
    legend.position = 'right'
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


ggsave(filename = file.path(base_path, "Figures", "Full Data", "All Methods", "MM.png"), plot = mmplots, width = 12, height = 7, dpi = 300)

# AMCE =========================================================================

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

ggsave(filename = file.path(base_path, "Figures", "Full Data", "All Methods", "AMCE.png"), plot = amceplots, width = 12, height = 7, dpi = 300)


# ACP ==========================================================================

## Preparations =================================================================

fi_df_acp <- as.data.frame(fi_df)

fi_df_acp <- fi_df_acp %>% 
  dplyr::select(c(ID, Count,Task:choice))

colnames(fi_df_acp) <- c('ID','Count', 'Task', 'Option', 'Decision', 'Agency', 'Mistake', 'Interest','Transparency', 'Accountability', 'Choice')

# Adding new columns to repliate Ganters exercise as closly as possible 
# Converting to similar to how Ganters refers to it as it will make referencing easier
# This is because the Ganter code required a lot of manual editing 

fi_df_acp$CaseID <- fi_df_acp$Count 
fi_df_acp$contest_no <- fi_df_acp$Task 
fi_df_acp$Chosen_Decision <- fi_df_acp$Choice

fi_df_acp$task <- cumsum(!duplicated(fi_df_acp[, c("contest_no","CaseID")]))
fi_df_acp$CaseID <- as.numeric(fi_df_acp$CaseID)
fi_df_acp$contest_no <- as.numeric(fi_df_acp$contest_no)
fi_df_acp$task <- as.numeric(fi_df_acp$task)
fi_df_acp$CaseID <- as.numeric(fi_df_acp$CaseID)

## ACP Estimation =============================================================== 
conjacp_data <- conjacp.prepdata(Chosen_Decision ~ Decision + Agency + Mistake + Interest + Transparency + Accountability,
                                 data = fi_df_acp,
                                 tasks = "task",
                                 id = "CaseID")


results_acp <- conjacp.estimation(conjacp_data,
                                  estimand = "acp",
                                  adjust = FALSE)


# Creating table for ACP Analysis
# Completely independently randomized attributes
# Create empty vectors to store results

ind_estimate <- NULL
ind_se       <- NULL

# Loop over independently randomized attributes
for (attribute2 in c('Decision', 'Agency', 'Mistake' ,'Interest', 'Transparency','Accountability')) {
  
  # create data frame for model
  data_model2 <- fi_df_acp[, c(attribute2, "Chosen_Decision")]
  names(data_model2)[-ncol(data_model2)] <- paste0(names(data_model2)[-ncol(data_model2)], ".")
  
  # estimate model
  model2 <- lm(Chosen_Decision ~ ., data = data_model2)
  
  # store estimates
  ind_estimate <- c(ind_estimate, coef(model2)[-1])
  vcov2         <- vcovCluster(model2, fi_df_acp$CaseID)
  ind_se       <- c(ind_se, sqrt(diag(vcov2))
                    [2:(nlevels(as.data.frame(fi_df_acp)[, match(attribute2, colnames(fi_df_acp))]))])
  
}

# Function to prepare table based on estimates stored above @Ganter

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

var_acp <- conjacp.var(results_acp,
                       alpha = .05,
                       nsimul = 25000)

# To help with visualisations
table_var_acp <- var.table(var_acp)
table_var_acp$type <- "Regular"
table_var2 <- rbind(table_var_acp)
rownames(table_var2) <- NULL
table_var2$attribute <-as.factor(table_var2$attribute)
table_var2$attribute <- factor(table_var2$attribute, levels = rev(c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")))

## ACP - Att. Imp. ==============================================================

acp_attimp <- ggplot(table_var2[table_var2$type == "Regular",],
                             aes(y = estimates, x = attribute, group = estimand)) +
  coord_flip(ylim = c(0, .43)) +
  geom_pointrange(aes(ymin = lower, ymax = upper, shape = estimand, color = estimand, fill = estimand),
                  position = position_dodge(width = .5), size = .7) +
  labs(
    title = 'Attribute Relative Importance using ACP for Finland',
    x = '',
    y = '',
    subtitle = 'Entire Dataset',
    caption = '*Data collected from AGACA project under Daria Gritsenko',
    shape = 'Measure',
    color = 'Measure',
    fill = 'Measure'
  ) +
  scale_color_manual(values = c('black','darkgrey'))+
  scale_shape_manual(values = c(16,8))+
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

ggsave(filename = file.path(base_path, "Figures", "Full Data", "All Methods", "ACP_Att_Imp.png"), plot = acp_attimp, width = 12, height = 7, dpi = 300)


## ACP vs. AMCE =================================================================

FI_AMCE <- amce(fi_df_acp, Chosen_Decision ~ Decision + 
                Agency + 
                Mistake + 
                Interest +
                Transparency +
                Accountability,
              id = ~CaseID)

# Based on Ganter's Table code
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
  var = c("", rep("Decision", 4), "", "", rep("Agency", 3), "", "", rep("Mistake", 3), "", "", rep("Interest", 2), "",
          "", rep("Transparency", 3), "", "", rep("Accountability", 3),"", rep("Decision", 4), "", "", rep("Agency", 3), "",  "",rep("Mistake", 3), "", "", rep("Interest", 2),
          "", "", rep("Transparency", 3), "", "", rep("Accountability", 3),"", rep("Decision", 4), "", "", rep("Agency", 3), "",  "",rep("Mistake", 3), "", "", rep("Interest", 2),
          "", "", rep("Transparency", 3), "", "", rep("Accountability", 3)),
  estimate = c(1, results_acp$estimates[1:4], 1, 1,
               results_acp$estimates[5:7], 1,1, 
               results_acp$estimates[8:10], 1,1,
               results_acp$estimates[11:12], 1,1,
               results_acp$estimates[13:15], 1,1, 
               results_acp$estimates[16:18],1,0,
               ind_estimate[1:3], 1,1,0,
               ind_estimate[4:5], 1,1,0,
               ind_estimate[6:7], 1,1,0,
               ind_estimate[8], 1,1,0,
               ind_estimate[9:10], 1,1,0,
               ind_estimate[11:12],1,
               FI_AMCE$estimate[1:4], 1, 1,
               FI_AMCE$estimate[5:7], 1,1, 
               FI_AMCE$estimate[8:10], 1,1,
               FI_AMCE$estimate[11:12], 1,1,
               FI_AMCE$estimate[13:15], 1,1, 
               FI_AMCE$estimate[16:18]),
  se = c(0, sqrt(diag(results_acp$vcov))[1:4], 0,0,
         sqrt(diag(results_acp$vcov))[5:7], 0,0, 
         sqrt(diag(results_acp$vcov))[8:10], 0,0,
         sqrt(diag(results_acp$vcov))[11:12], 0,0,
         sqrt(diag(results_acp$vcov))[13:15], 0,0, 
         sqrt(diag(results_acp$vcov))[16:18],0,0,
         ind_se[1:3], 0,0,0,
         ind_se[4:5], 0,0,0,
         ind_se[6:7], 0,0,0,
         ind_se[8], 0,0,0,
         ind_se[9:10], 0,0,0,
         ind_se[11:12],0,
         FI_AMCE$std.error[1:4], 0, 0,
         FI_AMCE$std.error[5:7], 0,0, 
         FI_AMCE$std.error[8:10], 0,0,
         FI_AMCE$std.error[11:12], 0,0,
         FI_AMCE$std.error[13:15], 0,0, 
         FI_AMCE$std.error[16:18]),
  type = c(rep("ACP", 29), rep("AMCE_indse", 29),rep("AMCE", 29))
  
)

# Pre Processing Post table generation (To compare AMCE and ACP)
table_acpamce3$modality <- factor(table_acpamce3$modality, levels = unique(table_acpamce3$modality)[length(table_acpamce3$modality):1])
hline <- data.frame(type = c("AMCE", "ACP", "AMCE_indse"), yint = c(0, 1,1))


table_acpamce4 <- table_acpamce3 %>%
  filter((type == 'ACP' | type == 'AMCE') & modality != "" & var != "" ) 

table_acpamce4$var <- factor(table_acpamce4$var, levels = c(
  "Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability"
))

# Before graph generation, fixing guide
# Define a bracket guide with centered text
centered_bracket <- primitive_bracket(
  key = key_range_manual(
    start = c(15, 12, 9, 7, 4, 1),
    end   = c(18, 14, 11, 8, 6, 3),
    labels = c("Decision", "Agency", "Mistake", "Interest", "Transparency", "Accountability")
  ),
  bracket = "square",
  theme = theme(
    # Set horizontal justification (angle 0) and center the text
    axis.text.y.left = element_text(angle = 0, vjust = -2),
    legend.text = element_text(angle = 0, vjust = -2)
  )
)

amcevsacp <- ggplot(table_acpamce4, aes(y = estimate, x = modality, group = var, color = type)) +
  coord_flip(ylim = c(-.25, .35)) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se,
                      color = type, shape = var, fill = type),
                  position = position_dodge(width = 1), size = 0.7) +
  geom_hline(data = subset(hline, type == "AMCE"), aes(yintercept = yint), size = 1) +
  facet_grid(. ~ type) +
  labs(
    x = 'Attribute Levels',
    y = '',
    caption = '*Data collected from AGACA project under Daria Gritsenko',
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

ggsave(filename = file.path(base_path, "Figures", "Full Data", "All Methods", "ACP_vs_AMCE.png"), plot = amcevsacp, width = 12, height = 7, dpi = 300)



# Conditional Logit ============================================================

## Preparations ================================================================

dummy_df <- dummy_cols(fi_df,
                       select_columns = c("decision", "agency", "mistake", "interest", "transparency","accountability"),
                       remove_first_dummy = TRUE)

dummy_df <- dummy_df %>%
  dplyr::select(-c(decision,agency,mistake, interest,transparency,accountability)) %>% 
  mutate(stratum = paste(Count, Task, sep = "_"))  

dummy_df$Task <- as.factor(dummy_df$Task)
dummy_df$Option <- as.factor(dummy_df$Option)

## C.Logit =====================================================================

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

# Adding Robusr se 
robust_se <- try(vcovCL(model_clogit, cluster = dummy_df$Count), silent = TRUE)

# Obtain robust p-values using coeftest()
robust_test <- coeftest(model_clogit)
robust_p <- robust_test[, 4]

# Generating the latex table
stargazer(model_clogit, type = "text", # can be converted to latex using type = "latex"
          se = list(robust_se),
          p = list(robust_p),
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Conditional Logit Results with Cluster Robust Standard Errors",
          dep.var.labels = "Choice",
          covariate.labels = c("Decision Robbery Area", 
                               "Decision Robbery Person", 
                               "Decision Theft Area", 
                               "Agency Alg Overruled", 
                               "Agency Alg. Recommends", 
                               "Mistake Algorithms", 
                               "Mistake Same", 
                               "Interest Private", 
                               "Transparency Experts", 
                               "Transparency Police", 
                               "Accountability Experts (Acc)", 
                               "Accountability Polic(Acc)"),
          notes = "Standard errors are cluster-robust.")



# Getting attribute level importance (% terms)
coef_estimates <- coef(model_clogit)
names(coef_estimates) <- gsub("`", "", names(coef_estimates))
importance_alt <- abs(coef_estimates)
importance_alt <- importance_alt / sum(importance_alt) * 100

## Attribute Imp. Graph ========================================================

# Mapping for individual attribute names
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

# Data frame for attribute-level importance
df_alt <- data.frame(
  Attribute = names(importance_alt),
  Importance  = as.numeric(importance_alt)
)
df_alt$PrettyAttribute <- pretty_names[df_alt$Attribute]

# Clogit Attribute Level Importance
attlvlimp <- ggplot(df_alt, aes(x = reorder(PrettyAttribute, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = round(Importance, 1)), 
            hjust = -0.1, size = 4.5, color = 'black') +
  coord_flip() +
  labs(
    title = "Attribute Importance",
    subtitle = "Using Conditional Logit Regression",
    x = "Attribute",
    y = "Importance (%)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(df_alt$Importance) * 1.2)) +
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

# Relative Importance using ACP 
acp_relimp <- ggplot(table_var2[table_var2$type == "Regular",],
                             aes(y = estimates, x = attribute, group = estimand)) +
  coord_flip(ylim = c(0, .43)) +
  geom_pointrange(aes(ymin = lower, ymax = upper, shape = estimand, color = estimand, fill = estimand),
                  position = position_dodge(width = .5), size = .7) +
  labs(
    title = 'Attribute Relative Importance',
    x = '',
    y = '',
    subtitle = 'Using ACP',
    shape = 'Measure',
    color = 'Measure',
    fill = 'Measure'
  ) +
  scale_color_manual(values = c('black','darkgrey'))+
  scale_shape_manual(values = c(16,8))+
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


#Combining relative and level wise imp
combined_cool <- (acp_relimp + attlvlimp) +
  plot_annotation(
    caption = '*Data collected from AGACA project under Daria Gritsenko'
  ) &
  theme(plot.title = element_text(color = "black", size = 16, hjust = 0.5))


ggsave(filename = file.path(base_path, "Figures", "Full Data", "All Methods", "Attribute_Imp.png"), plot = combined_cool, width = 12, height = 7, dpi = 300)


# End ==========================================================================


