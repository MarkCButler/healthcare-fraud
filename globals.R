# Define global variables, i.e., variables that should be available in each R
# script as well as in the R markdown file.

library(dplyr)
library(stringr)

# Clean up column names for chronic conditions, e.g., replace
# ChronicCond_rheumatoidarthritis by RheumatoidArthritis
clean_condition_names <- function(column_names) {
    new_names <- str_replace(column_names, '^ChronicCond_', '')
    new_names <- case_when(
        str_detect(new_names, 'Heartfailure') ~ 'HeartFailure',
        str_detect(new_names, 'Osteoporasis') ~ 'Osteoporosis',
        str_detect(new_names, 'rheumatoidarthritis') ~ 'RheumatoidArthritis',
        str_detect(new_names, 'stroke') ~ 'Stroke',
        TRUE ~ new_names
    )
    return(new_names)
}

chronic_conditions_raw <- c(
    'ChronicCond_Alzheimer', 'ChronicCond_Heartfailure',
    'ChronicCond_KidneyDisease', 'ChronicCond_Cancer',
    'ChronicCond_ObstrPulmonary', 'ChronicCond_Depression',
    'ChronicCond_Diabetes', 'ChronicCond_IschemicHeart',
    'ChronicCond_Osteoporasis', 'ChronicCond_rheumatoidarthritis',
    'ChronicCond_stroke'
)
chronic_conditions <- clean_condition_names(chronic_conditions_raw)

# Simple descriptions of conditions associated with the most common admission
# diagnosis codes.
code_descriptions <- c(
    'Chest pain', 'Shortness of breath', 'Pneumonia',
    'Congestive heart failure', 'Syncope (fainting)', 'Mammogram',
    'Irregular heartbeat', 'High blood pressure', 'Diabetes',
    'Prescription monitoring'
)
names(code_descriptions) <- c(
    '78650', '78605', '486', '4280', '7802',
    'V7612', '42731', '4019', '25000', 'V5883'
)

# Analysis is performed separately for inpatient and outpatient visits, so
# plots are typically generated as a loop over claim_types.
claim_types <- c('inpatient', 'outpatient')
