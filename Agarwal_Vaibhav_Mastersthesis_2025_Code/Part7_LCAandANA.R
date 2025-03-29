# Header Start =================================================================
# File Name: Part7_LCAandANA.R
# Purpose: Running Constraint and Unconstraint LCA to check for ANA
# Author: Vaibhav Agarwal
# R version: 4.3.2 (2023-10-31 ucrt)
# Last Modified: 11th April 2025
# Header End ===================================================================


# Libraries ====================================================================

# Due to various package and version conflcits, the mglot and gmnl pachages had to be downgraded to thier previous versions
# This was resolved by downgrading packagages using trial and error based on the publishing time of blog posts and articles which used this package
# The source of the error was due to changes in newer versions of mlogit (dfidx Integration)

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

#fi_df <- read.csv(file.path(base_path, "Data", "fi_df.csv"))
# Using from env. 

# Unconstraint Latent Class Analysis ===========================================

## Preparation ==================================================================

conditional_logit_df <- fi_df %>%
  dplyr::select(c(Count,Task:choice))


conditional_logit_df$decision      <- as.factor(conditional_logit_df$decision)
conditional_logit_df$agency        <- as.factor(conditional_logit_df$agency)
conditional_logit_df$mistake       <- as.factor(conditional_logit_df$mistake)
conditional_logit_df$interest      <- as.factor(conditional_logit_df$interest)
conditional_logit_df$transparency  <- as.factor(conditional_logit_df$transparency)
conditional_logit_df$accountability<- as.factor(conditional_logit_df$accountability)

levels(conditional_logit_df$decision)      <- c("Robbery Area", "Robbery Person", "Theft Area", "Theft Person")
levels(conditional_logit_df$agency)        <- c("Alg. Only", "Alg Overruled", "Alg. Recommends")
levels(conditional_logit_df$mistake)       <- c("Algorithms", "Humans", "Same")
levels(conditional_logit_df$interest)      <- c("Private", "Public")
levels(conditional_logit_df$transparency)  <- c("Developer", "Experts", "Police")
levels(conditional_logit_df$accountability)<- c("Developer (Acc)", "Experts (Acc)", "Polic(Acc)")


df <- conditional_logit_df %>%
  rename(id = Count, chid = Task, alt = Option) %>%
  select(id, chid, alt, choice, decision, agency, mistake, interest, transparency, accountability) %>%
  distinct() %>%
  mutate(
    id = as.factor(id),
    chid = as.factor(chid),
    alt = as.factor(alt),
    chid2 = paste(id, chid, sep = "_"),
    decision = as.numeric(factor(decision, levels = c("Robbery Area", "Robbery Person", "Theft Area", "Theft Person"))),
    agency = as.numeric(factor(agency, levels = c("Alg. Only", "Alg Overruled", "Alg. Recommends"))),
    mistake = as.numeric(factor(mistake, levels = c("Algorithms", "Humans", "Same"))),
    interest = as.numeric(factor(interest, levels = c("Private", "Public"))),
    transparency = as.numeric(factor(transparency, levels = c("Developer", "Experts", "Police"))),
    accountability = as.numeric(factor(accountability, levels = c("Developer (Acc)", "Experts (Acc)", "Polic(Acc)")))
  )

## Running the model ===========================================================

mlogit_df <- mlogit.data(df,
                         shape = "long",
                         choice = "choice",
                         id.var = "id",
                         alt.var = "alt",
                         chid.var = "chid2")

mlogit_df$chid <- mlogit_df$chid2

# This is an example. 
# Similarly the method can be changed to bfgs, nr and bhhh
# Number of classes can be changed using Q = ...
# Commented out code can help find ideal number of classes based on LL and BIC

lcmod <- gmnl(
  formula = choice ~ decision + agency + mistake + interest + transparency + accountability | 0 | 0 | 0 | 1,
  data    = mlogit_df,
  model   = "lc",
  Q       = 4,
  index   = c("id", "chid"),
  method  = "bfgs",
  seed    = 123
)



# checking number of classes 
#for (Q in 2:7) {
#  cat("Estimating", Q, "class model...\n")
#  lcmod <- gmnl(
#    formula = choice ~ decision + agency + mistake + interest + transparency + accountability | 0 | 0 | 0 | 1,
#    data    = mlogit_df,
#    model   = "lc",
#    Q       = Q,
#    index   = c("id", "chid"),
#    method  = "bfgs",
#    seed    = 123
#  )
#  LC_results[[as.character(Q)]] <- lcmod
#  BIC_values[Q] <- BIC(lcmod)
#}


## Model Summary ================================================================

lc_summary <- summary(lcmod)
coef_table <- lc_summary$CoefTable

# Renaming rows to make them more beautiful 
rename_row <- function(x) {
  if (grepl("^class\\.[0-9]+\\.", x)) {
    parts   <- strsplit(x, "\\.")[[1]] 
    clnum   <- parts[2]
    varname <- parts[3]
    varname <- paste0(toupper(substr(varname, 1, 1)), substring(varname, 2))
    return(paste0("Class ", clnum, ": ", varname))
  }
  if (grepl("^\\(class\\)[0-9]+$", x)) {
    clnum <- gsub("\\(class\\)", "", x)  # e.g. turn "(class)2" into "2"
    return(paste0("Class ", clnum, " membership"))
  }
  return(x)
}

new_rownames <- sapply(rownames(coef_table), rename_row)

est_se <- paste0(
  round(coef_table[, "Estimate"], 2), 
  " (", 
  round(coef_table[, "Std. Error"], 2), 
  ")"
)

# Extract p-values and making star significance like stargazer since the outputs are not compatible 
pvals <- round(coef_table[, "Pr(>|z|)"], 3)
stars <- ifelse(pvals < 0.01, "***", 
                ifelse(pvals < 0.05, "**", 
                       ifelse(pvals < 0.1, "*", "")))
pvals_with_stars <- paste0(pvals, stars)

new_coef_table <- data.frame(
  Parameter      = new_rownames,
  `Estimate(SE)` = est_se,
  `P-value`      = pvals_with_stars,
  row.names      = NULL
)

## Latex Output - 1 Table =======================================================

xtable_obj <- xtable(
  new_coef_table,
  caption = "Latent Class Logit Model Results",
  label   = "tab:lcmod"
)

addtorow <- list()
addtorow$pos <- list(nrow(new_coef_table))
addtorow$command <- "\\hline \n\\multicolumn{3}{l}{\\footnotesize{Note: * p<0.1; ** p<0.05; *** p<0.01}} \\\\ \n"

# output 
print(
  xtable_obj,
  type             = "latex",
  include.rownames = FALSE,  
  include.colnames = TRUE,
  table.placement  = "ht",
  add.to.row       = addtorow,
  hline.after      = c(-1, 0, nrow(new_coef_table))
)

# Constraint LCA ===============================================================

## Mistake Fix =================================================================

fixed_vec_2class_mistake <- c(
  # Class 1: fix mistake (position 3)
  FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
  # Class 2: no restrictions
  rep(FALSE, 6),
  # Class assignment parameters
  FALSE
)

lcmod_2class_mistake <- gmnl(
  formula = choice ~ decision + agency + mistake + interest + transparency + accountability | 0 | 0 | 0 | 1,
  data    = mlogit_df,
  model   = "lc",
  Q       = 2,
  index   = c("id", "chid"),
  seed    = 123,
  fixed   = fixed_vec_2class_mistake
)
summary(lcmod_2class_mistake)

## Interest Fix ================================================================

# Class 1: fix interest (position 4)
fixed_vec_2class_interest <- c(
  FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
  rep(FALSE, 6),
  FALSE
)

lcmod_2class_interest <- gmnl(
  formula = choice ~ decision + agency + mistake + interest + transparency + accountability | 0 | 0 | 0 | 1,
  data    = mlogit_df,
  model   = "lc",
  Q       = 2,
  index   = c("id", "chid"),
  seed    = 123,
  fixed   = fixed_vec_2class_interest
)
summary(lcmod_2class_interest)

## Accountability Fix ==========================================================

# Class 1: fix accountability (position 6)
fixed_vec_2class_account <- c(
  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
  rep(FALSE, 6),
  FALSE
)

lcmod_2class_account <- gmnl(
  formula = choice ~ decision + agency + mistake + interest + transparency + accountability | 0 | 0 | 0 | 1,
  data    = mlogit_df,
  model   = "lc",
  Q       = 2,
  index   = c("id", "chid"),
  seed    = 123,
  fixed   = fixed_vec_2class_account
)
summary(lcmod_2class_account)

# End ================================================================
