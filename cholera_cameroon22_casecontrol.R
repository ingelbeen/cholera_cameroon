######################################
# CHOLERA OUTBREAK, CAMEROON, 2022   #
# CASE-CONTROL STUDY                 #
######################################
# last update Brecht Ingelbeen 08 March 2023: create script
# pushed to github 21/04/2023 as a test
#### install and load packages ####
# install.packages("pacman")
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,ggmap,osmdata,gtsummary,zoo,reshape2,tidyr,stringr,wesanderson,tidyr, knitr, epitools, naniar, survey, ggpattern, tidyverse)

#### import data ####
cc <- read_excel("Cholera Case Control Final Data.xlsx", sheet = "data", 
                 col_types = c("date", "date", "text", "text", "text", "text",  "date", "text", "text", "text", "text", "date", "numeric", "text", "text", 
                                                     "text", "text", "text", "text", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", 
                                                     "text", "text", "text", "text", "text",  "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", 
                                                     "numeric", "numeric", "numeric",   "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", 
                                                     "text", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", 
                                                     "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text", "text", "text", 
                                                     "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", 
                                                     "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text",
                                                     "text", "text", "text", "text", "numeric", "numeric", 
                                                     "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric"))

# reasons for relocating are in another tab of the excel
IDPdetail <- read_excel("Cholera Case Control Final Data.xlsx", 
                        sheet = "DetailsRelocation")
# create a new variable 'IDP'
IDPdetail$IDP <- 0
IDPdetail$IDP[IDPdetail$`Q15c. Reason for relocation ${ID}`=="Internally displaced due to instability"] <- 1
IDP <- IDPdetail %>%
  group_by(`_submission__id`) %>%
  summarise(IDP=max(IDP))
IDP$IDP[IDP$IDP==0] <- "no"
IDP$IDP[IDP$IDP=="1"] <- "yes"

# rename common ID to link
IDP$uid <- IDP$`_submission__id`
IDP <- IDP %>% select(uid, IDP)
cc$uid <- cc$`_id`
cc <- cc %>% select(-`_id`, -end, -`_tags`, -`_submitted_by`, -`_submission_time`, -`_status`)

# merge the IDP variable with the original database
cc <- merge(cc, IDP, by = "uid", all.x = T)

#### clean data ####
# remove those that have no age or case vs control status recorded
cc <- cc %>% filter(!is.na(casecontrol) & !is.na(`Q9. Age:`))
# age
table(cc$`Q9. Age:`, useNA = "always") # missing ages don't have dob
table(cc$`Q9. Age:`)
# agegroups - highest age 82 y
cc$agegr <- cut(cc$`Q9. Age:`, breaks = seq(0, 90, 10), labels = paste0(seq(0,80,10), "-", seq(10, 90, 10)-1))
table(cc$agegr)
# agegroups large
cc$agegr_3levels[cc$`Q9. Age:`<18] <- "0-17 years"
cc$agegr_3levels[cc$`Q9. Age:`>17.999] <- "18-49 years"
cc$agegr_3levels[cc$`Q9. Age:`>49.999] <- ">= 50 years"

# sex
table(cc$`Q10. Sex:`, cc$casecontrol)
prop.table(table(cc$`Q10. Sex:`, cc$casecontrol),1)

# occupation
cc$occupation <- tolower(cc$`Q12. Occupation:`)
cc$`Q12a. Specify Occupation:` <- tolower(cc$`Q12a. Specify Occupation:`)
cc$occupation[grepl("fish",cc$`Q12a. Specify Occupation:`)==T & grepl("roast",cc$`Q12a. Specify Occupation:`)==F] <- "fishing"
cc$occupation[grepl("import",cc$`Q12a. Specify Occupation:`)==T] <- "trade"
cc$occupation[grepl("trade",cc$`Q12a. Specify Occupation:`)==T] <- "trade"
cc$occupation[grepl("trading",cc$`Q12a. Specify Occupation:`)==T] <- "trade"
cc$occupation[grepl("famer",cc$`Q12a. Specify Occupation:`)==T] <- "agriculture/forestry"
cc$occupation[grepl("faming",cc$`Q12a. Specify Occupation:`)==T] <- "agriculture/forestry"
cc$occupation[grepl("farm",cc$`Q12a. Specify Occupation:`)==T] <- "agriculture/forestry"
cc$occupation[grepl("evenromentalist",cc$`Q12a. Specify Occupation:`)==T] <- "agriculture/forestry"
cc$occupation[grepl("sales",cc$`Q12a. Specify Occupation:`)==T] <- "retail"
cc$occupation[grepl("small business",cc$`Q12a. Specify Occupation:`)==T] <- "retail"
cc$occupation[grepl("doctor",cc$`Q12a. Specify Occupation:`)==T] <- "healthcare"
cc$occupation[grepl("nurse",cc$`Q12a. Specify Occupation:`)==T] <- "healthcare"
cc$occupation[grepl("student",cc$`Q12a. Specify Occupation:`)==T] <- "student"
cc$occupation[!is.na(cc$`Q12a. Specify Occupation:`) & is.na(cc$occupation)] <- "other"
cc$occupation[cc$occupation=="employed"|cc$occupation=="self-employed"] <- "employed but not specified"
table(cc$occupation, useNA = "always")

# number of HH members
cc$nhousemembers[cc$`Q28. Number of people living within the accomodation:`<3] <- "<3"
cc$nhousemembers[cc$`Q28. Number of people living within the accomodation:`>= 3 & cc$`Q28. Number of people living within the accomodation:` <= 5] <- "3-5"
cc$nhousemembers[cc$`Q28. Number of people living within the accomodation:`>5] <- ">5"

# other water source is all 'sping'
cc$`Q38. Participant's main drinking water source :`[cc$`Q38. Participant's main drinking water source :`=="Other"] <- "Spring"

# water source when original water source is interrupted -> what's "sonara water"??
cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?` <- tolower(cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)
cc$watersource_when_interrupted[grepl("hol",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "public tap/borehole"
cc$watersource_when_interrupted[grepl("forag",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "public tap/borehole"
cc$watersource_when_interrupted[grepl("not",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "no interruption"
cc$watersource_when_interrupted[grepl("min",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "packed water"
cc$watersource_when_interrupted[grepl("none",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "no interruption"
cc$watersource_when_interrupted[grepl("other",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "other"
cc$watersource_when_interrupted[grepl("pipe",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "piped"
cc$watersource_when_interrupted[grepl("other",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "other"
cc$watersource_when_interrupted[grepl("protected well",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "public tap/borehole"
cc$watersource_when_interrupted[grepl("public",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "public tap/borehole"
cc$watersource_when_interrupted[grepl("river",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "river"
cc$watersource_when_interrupted[grepl("spring",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "spring"
cc$watersource_when_interrupted[grepl("stream",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "river"
cc$watersource_when_interrupted[grepl("sea",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "river"
cc$watersource_when_interrupted[grepl("super",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "packed water"
cc$watersource_when_interrupted[grepl("supper",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "packed water"
cc$watersource_when_interrupted[cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`=="tap water"] <- "piped"
cc$watersource_when_interrupted[cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`=="tap"] <- "piped"
cc$watersource_when_interrupted[grepl("tape",cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`)==T] <- "piped"
cc$watersource_when_interrupted[cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`=="wel"] <- "unprotected well"
cc$watersource_when_interrupted[cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`=="well"] <- "unprotected well"
cc$watersource_when_interrupted[cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`=="well water"] <- "unprotected well"
cc$watersource_when_interrupted[cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`=="wellwater"] <- "unprotected well"
table(cc$`Q41. Which water source did/do you resort to during the time of pipe borne water scarcity?`[is.na(cc$watersource_when_interrupted)])

# travel history
cc$`Q32.Did you travel within the last 1 week before sickness?`[cc$`Q32.Did you travel within the last 1 week before sickness?`=="Not applicable"] <- "No"

# health district
table(cc$`Q6. Name of Health district:`, cc$casecontrol)

# type of residence -> group types with too small numbers to say anything meaningfull as "other"
cc$`Q24. Type of current accommodation`[cc$`Q24. Type of current accommodation`=="Group accommodation within the same space: church"] <- "Other"
cc$`Q24. Type of current accommodation`[cc$`Q24. Type of current accommodation`=="Group accommodation within the same space: orphanage"] <- "Other"
cc$`Q24. Type of current accommodation`[cc$`Q24. Type of current accommodation`=="Group accommodation within the same space: other"] <- "Other"

# matchingvariable
cc$matched <- tolower(cc$`Q2. Study Code:`)
cc$matched <- gsub("o", "0", cc$matched)
cc$matched <- gsub("\\s+|-", "", cc$matched)
cc$first_two <- substr(cc$matched, 1, 2) # keep first two characters
cc$right_three <- substr(cc$matched, start = nchar(cc$matched) - 2, stop = nchar(cc$matched))
cc$matchednew <- paste0(cc$first_two, cc$right_three)
table(cc$matchednew)

# municip
cc$municip <- tolower(cc$`Q17. Current municipality (district) where participant lives at the moment:`)

# rename column names
cc <- cc %>% rename(age = `Q9. Age:`, sex = `Q10. Sex:`, education = `Q11. Educational level:`, income = `Q13. Monthly income (in FCFA):`,
                    residencetype = `Q24. Type of current accommodation`, housing = `Q25. Type of accomadation (house):`, toilet= `Q26. Type of Toilet:`,
                    kitchentype = `Q27. Type of Kitchen:`, traveltoNigeria = `Q31. Did you visit any of the following places during the past one year?/Nigeria`,
                    travelhistory = `Q32.Did you travel within the last 1 week before sickness?`,
                    watersource = `Q38. Participant's main drinking water source :`,
                    wateravailable = `Q47. Is water available in the household at the time of the interviewer's visit? :`,
                    waterstorage = `Q48. How is drinking water stored today?:`,
                    waterunavailability = `Q39. How frequently do you experience pipe-borne water unavailability? :`,
                    healthdistrict = `Q6. Name of Health district:`)

# convert character variables to factor variables
cc$agegr_3levels <- factor(cc$agegr_3levels)
cc$sex <- factor(cc$sex)
cc$income <- factor(cc$income)
cc$education <- factor(cc$education)
cc$occupation <- factor(cc$occupation)
cc$residencetype <- factor(cc$residencetype)
cc$housing <- factor(cc$housing)
cc$toilet <- factor(cc$toilet)
cc$nhousemembers <- factor(cc$nhousemembers)
cc$watersource <- factor(cc$watersource)
cc$wateravailable <- factor(cc$wateravailable)
cc$waterstorage <- factor(cc$waterstorage)
cc$waterunavailability <- factor(cc$waterunavailability)
cc$watersource_when_interrupted <- factor(cc$watersource_when_interrupted)
cc$traveltoNigeria <- factor(cc$traveltoNigeria)
cc$travelhistory <- factor(cc$travelhistory)
cc$casecontrol <- as.factor(cc$casecontrol)
cc$IDP <- as.factor(cc$IDP)

# ensure an order that makes sense
# view the levels of the factor variable
levels(cc$casecontrol)
levels(cc$agegr_3levels)
levels(cc$income)
levels(cc$education)
levels(cc$occupation)
levels(cc$housing)
levels(cc$toilet)
levels(cc$wateravailable)
levels(cc$residencetype)

# reverse the order of the levels
cc$casecontrol <- relevel(cc$casecontrol, ref = "control")
cc$agegr_3levels <- relevel(cc$agegr_3levels, ref = "18-49 years")
cc$income <- relevel(cc$income, ref = "50000 –  99000  FCFA")
cc$education <- relevel(cc$education, ref = "Secondary education (completed)")
cc$occupation <- relevel(cc$occupation, ref = "employed but not specified")
cc$housing <- relevel(cc$housing, ref = "Apartment")
cc$toilet <- relevel(cc$toilet, ref = "Internal")
cc$wateravailable <- relevel(cc$wateravailable, ref = "Yes")
cc$waterstorage <- relevel(cc$waterstorage, ref = "Jerrycans/gallons")
cc$watersource <- relevel(cc$watersource, ref = "Tap water (Public)")
cc$residencetype <- relevel(cc$residencetype, ref = "Stable accommodation (did not relocate during the past 5 years)" )
cc$IDP <- relevel(cc$IDP, ref = "no")



#### table with univariate odds ratios ####
# Create a list of exposure variables
explanatory_vars  <- c("agegr_3levels", "sex", "income", "education", "occupation", "IDP", "residencetype", "housing", 
                   "toilet", "nhousemembers", "watersource", "wateravailable", "waterstorage", 
                   "waterunavailability", "watersource_when_interrupted", "traveltoNigeria", "travelhistory"
                   )

# build an univar table
table(cc$agegr_3levels, cc$casecontrol)
# counts
cc %>% 
  select(agegr_3levels, sex, casecontrol, income, education, occupation, IDP, residencetype, housing, 
         toilet, nhousemembers, watersource, wateravailable, waterstorage, 
         waterunavailability, watersource_when_interrupted, traveltoNigeria, travelhistory) %>% # keep only columns of interest
  tbl_summary(     
    by = casecontrol,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} / {N} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      casecontrol   ~ "Case/Control",                           
      agegr_3levels ~ "Agegroup (years)",
      sex    ~ "Sex",
      income      ~ "Income Level",
      education  ~ "Education level",
      occupation ~ "Occupation",
      IDP ~ "Internally displaced",
      watersource ~ "Main drinking water source",
      wateravailable ~ "Water available at time of interview"),
    missing_text = "Missing"                                    # how missing values should display
  )                                                # default

# odds ratios
univ_tab <- cc %>% 
  dplyr::select(explanatory_vars, casecontrol) %>% ## select variables of interest
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = casecontrol,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

## view univariate results table 
univ_tab
univ_tab_df <- as.data.frame(univ_tab$table_body)

# export
write.table(univ_tab_df, file = "univ_tab_df.txt")

#### multivariable logistic regression to control for district (and potentially age group)  ####
# remove observations with missing values for vars of itnerest
ccmva <- cc %>% filter(!is.na(travelhistory) & !is.na(nhousemembers) & !is.na(casecontrol) & !is.na(wateravailable))
## run a regression with all variables of interest 
model1 <- glm(casecontrol ~ travelhistory, family = "binomial", data = ccmva)
model2 <- glm(casecontrol ~ travelhistory + nhousemembers, family = "binomial", data = ccmva)
lmtest::lrtest(model1, model2)
model3 <- glm(casecontrol ~ travelhistory + nhousemembers + healthdistrict, family = "binomial", data = ccmva)
lmtest::lrtest(model2, model3) # not better with health district
model4 <- glm(casecontrol ~ travelhistory + nhousemembers + occupation, family = "binomial", data = ccmva)
lmtest::lrtest(model2, model4)
model5 <- glm(casecontrol ~ travelhistory + nhousemembers + wateravailable, family = "binomial", data = ccmva)
lmtest::lrtest(model2, model5) # model 2 has the best LR
model6 <- glm(casecontrol ~ travelhistory + nhousemembers + housing, family = "binomial", data = ccmva)
lmtest::lrtest(model2, model6) # model 2 has the best LR
model7 <- glm(casecontrol ~ travelhistory + nhousemembers + agegr_3levels + sex, family = "binomial", data = ccmva)
lmtest::lrtest(model2, model7) # model 2 has the best LR
# table with adjusted odds ratios
mv_tab_base <- model2 %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))          ## round 
mv_tab_base
# table with adjusted edds ratios if selection of controls also based on age and sex
mv_tab_base <- model7 %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))          ## round 
mv_tab_base
