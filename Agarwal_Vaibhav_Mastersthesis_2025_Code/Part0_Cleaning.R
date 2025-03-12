# Header Start =================================================================
# File Name: Part0_Cleaning.R
# Purpose: Cleaning and setting up data for future analysis
# Author: Vaibhav Agarwal
# R version: 4.3.2 (2023-10-31 ucrt)
# Last Modified: 11th April 2025
# Header End ===================================================================



# Libraries ====================================================================

library(tidyverse) # version tidyverse_2.0.0


# Loading Data (All) ===========================================================

base_path = "C:/Users/User/OneDrive - University of Helsinki/Desktop/MA Thesis - Final - Vaibhav"
fi_df <- read_delim(
  file.path(base_path, "Data", "Predpol_FI.csv"), 
  delim = ";", 
  escape_double = FALSE, 
  col_names = TRUE, 
  trim_ws = TRUE, 
  skip = 2
)

# Cleaning =====================================================================

## Renamming Columns ===========================================================

colnames(fi_df) <- c('Count','ID',
                     'Language','Recaptch_Score','Continue?',
                     'Have you ever dealt with digital government before? Tick any applicable',
                     'Conjoint_S1','Conjoint_S2',
                     'To what extent do you think such use of modern computer technologies in law enforcement agencies is appropriate?', 'How satisfied are you with the way government agencies work in the Finland?',
                     'Trust-current_gov','Trust-parliment','Trust-localgov','Trust-egov','Trust-officials','Trust-courts','Trust-police',
                     'What is your attitude towards adopting new technologies?','How do you feel about the government use of video surveillance systems in public places in your local area, designed to reduce crime?',
                     'Worried_about_personal_info_leak-social media','Worried_about_personal_info_leak-online_purchases','Worried_about_personal_info_leak-e_public_services',
                     'Tech_Can_Solve-Economic_Growth','Tech_Can_Solve-National_Security','Tech_Can_Solve-Social_Justice','Tech_Can_Solve-Environment','Tech_Can_Solve-Good_Healthcare','Tech_Can_Solve-Good_Education','Tech_Can_Solve-Crime_Rate','Tech_Can_Solve-Well_Paid_Job',
                     'Age','Gender','Education_level','Employment_Status','Residence','Province','language','Internet_Use',
                     'vers_CBCONJOINT','C1.1_1','C1.1_2','C1.1_3','C1.1_4','C1.1_5','C1.1_6','C1.2_1','C1.2_2','C1.2_3','C1.2_4','C1.2_5','C1.2_6','C2.1_1','C2.1_2','C2.1_3','C2.1_4','C2.1_5','C2.1_6','C2.2_1','C2.2_2','C2.2_3','C2.2_4','C2.2_5','C2.2_6','Revision_conj','StraightliningCount','StraightliningPercentage','StraightliningQuestions','UnansweredPercentage','Q_UnansweredQuestions')


## Pivoting Longer =============================================================

#Based on the 24 cols (C1.1_1...)
fi_df <- fi_df %>%
  pivot_longer(
    cols = matches("^C\\d\\.\\d_\\d$"),  
    names_to = c("Scenario", "Attribute"),
    names_pattern = "C(\\d\\.\\d)_(\\d)",
    values_to = "Choice_Given"
  )

fi_df <- fi_df %>%
  group_by(Scenario, ID) %>% 
  pivot_wider(names_from = Attribute, values_from = Choice_Given)  


fi_df <- fi_df %>%
  separate(col = Scenario, into = c("Task", "Option"), sep = "\\.")

## Choice Variable (Dependant Variable) ========================================

fi_df$choice <- NA
fi_df$choice <- if_else((fi_df$Task== 1 & fi_df$Conjoint_S1 == fi_df$Option) | (fi_df$Task == 2 & fi_df$Conjoint_S2 == fi_df$Option), 1, 0)


## Attribute Levels ============================================================

fi_df <- fi_df %>%
  rename(decision = `1`,
         agency = `2`,
         mistake = `3`,
         interest = `4`,
         transparency = `5`,
         accountability = `6`)


# Renaming every column individually, so it is easy to check and correct mistakes
fi_df <- fi_df %>%
  mutate(
    decision = case_when(
      decision == "Ennakoi autovarkauksien riskejä ja tunnistaa erityisen riskialttiita alueita." ~ "theft_area",
      decision == "Ennakoi ryöstö- ja pahoinpitelyriskiä kaduilla ja tunnistaa erityisen riskialttiita alueita." ~ "robbery_area",
      decision == "Tunnistaa henkilöt, jotka todennäköisimmin syyllistyvät autovarkauksiin." ~ "theft_person",
      decision == "Tunnistaa henkilöt, jotka todennäköisimmin syyllistyvät ryöstöihin ja pahoinpitelyihin kaduilla." ~ "robbery_person",
      TRUE ~ NA  
    )
  )


fi_df <- fi_df %>%
  mutate(
    agency = case_when(
      agency == "Tietokoneohjelma suosittelee päätöstä, joka on hyväksyttävä paikallisella poliisiviranomaisella." ~ "alg_recommends",
      agency == "Tietokoneohjelma tekee päätöksen ilman paikallisen poliisiviranomaisen osallistumista." ~ "alg_only",
      agency == "Tietokoneohjelma tekee päätöksen, jonka paikallinen poliisiviranomainen voi kumota." ~ "alg_overruled",
      TRUE ~ NA  
    )
  )


fi_df <- fi_df %>%
  mutate(
    mistake = case_when(
      mistake == "Ihminen tekee vähemmän virheitä kuin tietokoneohjelmat." ~ "humans",
      mistake == "Tietokoneohjelmat ja ihmiset tekevät suunnilleen saman määrän virheitä." ~ "same",
      mistake == "Tietokoneohjelmat tekevät vähemmän virheitä kuin ihmiset." ~ "algorithms",
      TRUE ~ NA  
    )
  )

fi_df <- fi_df %>%
  mutate(
    interest = case_when(
      interest == "Poliisin oma tietohallinto." ~ "public",
      interest == "Yksityinen yritys poliisin tilauksella." ~ "private",
      TRUE ~ NA  
    )
  )

fi_df <- fi_df %>%
  mutate(
    transparency = case_when(
      transparency == "Ohjelma antaa paikalliselle poliisiviranomaiselle mahdollisuuden seurata päätöksenteon logiikkaa." ~ "police",
      transparency == "Ohjelman avulla sisäministeriön alainen digitaalitekniikan hallinnan asiantuntijaneuvosto voi seurata päätöksenteon logiikkaa." ~ "experts",
      transparency == "Vain ohjelman kehittäjä (koodari) voi jäljittää päätöksentekologiikan." ~ "developer",
      TRUE ~ NA  
    )
  )

fi_df <- fi_df %>%
  mutate(
    accountability  = case_when(
      accountability  == "Paikallinen poliisiviranomainen, joka käyttää kyseistä ohjelmaa." ~ "police_acc",
      accountability  == "Sisäministeriön alainen digitaalitekniikan hallinnan asiantuntijaneuvosto." ~ "experts_acc",
      accountability  == "Tietokoneohjelman kehittäjä (koodari)." ~ "developer_acc",
      TRUE ~ NA  
    )
  )

## as.factor Conversion ========================================================
fi_df <- fi_df %>%
  mutate(across(`Continue?`:Internet_Use, as.factor),   
         across(Task:accountability, as.factor))  


## NA remove ===================================================================

# checking for NA values based on the options and removing all rows which have NA values 
columns_to_check <- c("choice", "decision", "agency", "mistake", "interest", "transparency", "accountability")

# Remove rows with NA values in the specified columns
fi_df <- fi_df[complete.cases(fi_df[, columns_to_check]), ]

# Additional check --> only keeping those who have completed both task one and two
# Creating a fi_df with BOTH tasks - first geting IDS with only one task
task_count_per_id <- fi_df %>%
  group_by(ID) %>%
  summarise(task_count = n_distinct(Task))

valid_ids <- task_count_per_id %>%
  filter(task_count == 2) %>%
  pull(ID)

#Creating new fi_df, with those individuals who have both IDs
fi_df <- fi_df %>%
  filter(ID %in% valid_ids)

## Clean Subgroup Creation ======================================================

# Age
fi_df <- fi_df %>%
  mutate(
    age_new = Age %>% 
      fct_recode(
        "Young Adults" = "< 25",
        "Young Adults" = "26-35",
        "Middle-Aged" = "36-45",
        "Middle-Aged" = "46-55",
        "Middle-Aged" = "56-65",
        "Older Adults" = "66-75",
        "Older Adults" = ">76"
      )
  )

#Gender
fi_df <- fi_df %>%
  mutate(
    gender_new = case_when(
      Gender == "Mies" ~ "Male",
      Gender == "Nainen" ~ "Female",
      TRUE ~ NA_character_
    )
  )


# Education
fi_df <- fi_df %>%
  mutate(
    edu_new = Education_level  %>% 
      fct_recode(
        "College/University Education" = "Opisto- tai korkeakoulututkinto",
        "No College/University" = "Lukio, ylioppilas- tai ammatillinen tutkinto",
        "No College/University" = "Peruskoulun yläaste (7-9/10 luokat), keskikoulu",
        "No College/University" = "Lisensiaatin tai tohtorin tutkinto",
        "No College/University" = "Peruskoulun ala-aste (1-6 luokat), kansakoulu",
        "No College/University" = "Ei mitään näistä",
        "No College/University" = "Vähemmän kuin peruskoulun ala-aste tai vastaava"
      )
  )

# Rural/Urban
fi_df <- fi_df %>%
  mutate(
    Rural_Urban_Group = Residence %>% 
      fct_recode(
        "Urban" = "Kaupunki",
        "Rural" = "Maaseutu"
      )
  )

# Employment Status
fi_df <- fi_df %>%
  mutate(
    Employment_Status_Group = Employment_Status  %>% 
      fct_recode(
        "Employed" = "Työllistynyt",
        "Employed" = "Osa-aikatyö",
        "Retired" = "Eläkkeellä",
        "Students" = "Opiskelija",
        "Others" = "Taloudenhoito/Hoitovapaalla",
        "Others" = "Työtön, etsin aktiivisesti työtä",
        "Others" = "Työtön, en etsi työtä"
      )
  )

# Language
fi_df <- fi_df %>%
  mutate(
    lang_new = case_when(
      language == "suomi" ~ "Finnish",
      language == "ruotsi" ~ "Swedish",
      TRUE ~ NA_character_
    )
  )



# Surveillance Attitudes
fi_df <- fi_df %>%
  mutate(
    surveillance_attitude_new = `How do you feel about the government use of video surveillance systems in public places in your local area, designed to reduce crime?` %>%
      fct_recode(
        "Positive" = "Erittäin positiivinen",
        "Positive" = "Jonkin verran positiivinen",
        "Negative" = "Erittäin negatiivinen",
        "Negative" = "Melko negatiivinen"
      ),
    surveillance_attitude_new = na_if(surveillance_attitude_new, "En osa sanoa")
  )

# Tech Adoption
fi_df <- fi_df %>%
  mutate(
    tech_adoption_new = `What is your attitude towards adopting new technologies?` %>%
      fct_recode(
        "Early Adopters" = "Olen ensimmäisten uutta teknologiaa käyttävien joukossa.",
        "Majority" = "Odotan, että teknologia vakiintuu.",
        "Laggards" = "Alan käyttää uutta teknologiaa vasta kun ei ole muuta vaihtoehtoa."
      )
  )


# Government Satisfaction
colnames(fi_df)[10] <- 'gov_sat'
fi_df <- fi_df %>%
  mutate(gov_sat_cat = case_when(
    as.numeric(as.character(gov_sat)) <= 3 ~ "Low Satisfaction",
    as.numeric(as.character(gov_sat)) >= 4 & as.numeric(as.character(gov_sat)) <= 5 ~ "Medium Satisfaction",
    as.numeric(as.character(gov_sat)) >= 6 ~ "High Satisfaction"
  ))


# Modern Tech Law
colnames(fi_df)[9] <- 'modrn_tech_law'
fi_df <- fi_df %>%
  mutate(modrn_tech_law_cat = case_when(
    as.numeric(as.character(modrn_tech_law)) <= 4 ~ "Not Appropriate",
    as.numeric(as.character(modrn_tech_law)) >= 5 ~ "Appropriate"
  ))


# Worry - Social Media
fi_df <- fi_df %>%
  mutate(
    worried_socialmedia = `Worried_about_personal_info_leak-social media` %>%
      fct_recode(
        "Not worried at All" = "En lainkaan huolestunut",
        "Little Worried" = "Hieman huolestunut",
        "Very Worried" = "Hyvin huolestunut",
        "Somewhat Worried" = "Melko huolestunut"
      )
  )

# Worry - E Services
fi_df <- fi_df %>%
  mutate(
    worried_eserv = `Worried_about_personal_info_leak-e_public_services` %>%
      fct_recode(
        "Not worried at All" = "En lainkaan huolestunut",
        "Little Worried" = "Hieman huolestunut",
        "Very Worried" = "Hyvin huolestunut",
        "Somewhat Worried" = "Melko huolestunut"
      ))


## Straightliners Remove =======================================================

fi_df$straightlining_score <- apply(fi_df[ ,9:30], 1, function(x) length(unique(x)))

# IDs to remove based on straightlining
remove_responses <- c("R_1jIZqQ4vcDlrUlg", "R_2aRVOJtbspCWrBM", "R_1CqBnaWWlzJTM8s", 
                      "R_3EclIgCSFfLIPmv", "R_yXasGUdRrGKlVXr", "R_1P4QdtobvmlWhhw",
                      "R_3JxckOYPz28Bz5S", "R_323L3du5Jw6IWSg", "R_2rAbAXayuymBEBp",
                      "R_3RyEyOX10lPSssb", "R_2Eg7UAVKt8i4m3v")

fi_df <- fi_df %>%
  filter(!ID %in% remove_responses)

# Reording Ref Levels and Renaming =============================================

fi_df <- fi_df %>%
  mutate(
    decision = factor(decision,
                      levels = c("theft_person", "theft_area", "robbery_person", "robbery_area"),
                      labels = c("Theft Person", "Theft Area", "Robbery Person", "Robbery Area")),
    
    agency = factor(agency,
                    levels = c("alg_only", "alg_overruled", "alg_recommends"),
                    labels = c("Alg. Only", "Alg Overruled", "Alg. Recommends")),
    
    mistake = factor(mistake,
                     levels = c("algorithms", "humans", "same"),
                     labels = c("Algorithms", "Humans", "Same")),
    
    interest = factor(interest,
                      levels = c("private", "public"),
                      labels = c("Private", "Public")),
    
    transparency = factor(transparency,
                          levels = c("developer", "experts", "police"),
                          labels = c("Developer", "Experts", "Police")),
    
    accountability = factor(accountability,
                            levels = c("developer_acc", "experts_acc", "police_acc"),
                            labels = c("Developer (Acc)", "Experts (Acc)", "Police (Acc)"))
  ) %>%
  mutate(
    decision = relevel(decision, ref = "Theft Person"),
    agency = relevel(agency, ref = "Alg. Only"),
    mistake = relevel(mistake, ref = "Humans"),
    interest = relevel(interest, ref = "Public"),
    transparency = relevel(transparency, ref = "Developer"),
    accountability = relevel(accountability, ref = "Developer (Acc)")
  )

# Saving df ====================================================================

write.csv(fi_df, file = file.path(base_path, "Data" , "fi_df.csv"), row.names = FALSE)

# End ==========================================================================
