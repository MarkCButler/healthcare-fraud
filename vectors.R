# Define vector variables that are used in other scripts and/or in the main
# R markdown file.
#
# Note in the current project, the default argument local = FALSE is used with
# the source command, so all variables defined by any script are available in
# the same global environment.  In particular, the variables defined in the
# current script are available in all other scripts provided the current script
# is sourced before the others.

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

# Plot labels corresponding to chronic conditions.  For instance,
# 'Alzheimer' is a cleaned-up column name included in the chronic_conditions
# vector, while 'Alzheimer\'s disease' is the corresponding plot label.
chronic_condition_labels <- case_when(
    str_detect(chronic_conditions, 'Alzheimer') ~ 'Alzheimer\'s disease',
    str_detect(chronic_conditions, 'HeartFailure') ~ 'heart failure',
    str_detect(chronic_conditions, 'KidneyDisease') ~ 'kidney disease',
    str_detect(chronic_conditions,
               'IschemicHeart') ~ 'coronary\nheart\ndisease',
    str_detect(chronic_conditions,
               'ObstrPulmonary') ~ 'obstructive\npulmonary\ndisease',
    str_detect(chronic_conditions,
               'RheumatoidArthritis') ~ 'rheumatoid arthritis',
    TRUE ~ tolower(chronic_conditions)
)
names(chronic_condition_labels) <- chronic_conditions

# Column names and plot labels associated with claim payments.
payment_variables <- c('total_reimbursed', 'reimbursed_per_visit',
                      'total_cost_of_claims', 'claim_cost_per_visit')
payment_labels <- c(
    'Total payment by insurance per patient',
    'Mean payment by insurance per patient per visit',
    'Total paid to hospitals per patient',
    'Mean paid to hospital per patient per visit'
)
names(payment_labels) <- payment_variables

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

# Locations corresponding to SSA "state codes," i.e., the first two digits of
# fibe-digits SSA county codes.  Copied from
# https://www.resdac.org/cms-data/variables/state-code-claim-ssa
state_codes <- c('01' = 'Alabama',
                 '02' = 'Alaska',
                 '03' = 'Arizona',
                 '04' = 'Arkansas',
                 '05' = 'California',
                 '06' = 'Colorado',
                 '07' = 'Connecticut',
                 '08' = 'Delaware',
                 '09' = 'District of Columbia',
                 '10' = 'Florida',
                 '11' = 'Georgia',
                 '12' = 'Hawaii',
                 '13' = 'Idaho',
                 '14' = 'Illinois',
                 '15' = 'Indiana',
                 '16' = 'Iowa',
                 '17' = 'Kansas',
                 '18' = 'Kentucky',
                 '19' = 'Louisiana',
                 '20' = 'Maine',
                 '21' = 'Maryland',
                 '22' = 'Massachusetts',
                 '23' = 'Michigan',
                 '24' = 'Minnesota',
                 '25' = 'Mississippi',
                 '26' = 'Missouri',
                 '27' = 'Montana',
                 '28' = 'Nebraska',
                 '29' = 'Nevada',
                 '30' = 'New Hampshire',
                 '31' = 'New Jersey',
                 '32' = 'New Mexico',
                 '33' = 'New York',
                 '34' = 'North Carolina',
                 '35' = 'North Dakota',
                 '36' = 'Ohio',
                 '37' = 'Oklahoma',
                 '38' = 'Oregon',
                 '39' = 'Pennsylvania',
                 '40' = 'Puerto Rico',
                 '41' = 'Rhode Island',
                 '42' = 'South Carolina',
                 '43' = 'South Dakota',
                 '44' = 'Tennessee',
                 '45' = 'Texas',
                 '46' = 'Utah',
                 '47' = 'Vermont',
                 '48' = 'Virgin Islands',
                 '49' = 'Virginia',
                 '50' = 'Washington',
                 '51' = 'West Virginia',
                 '52' = 'Wisconsin',
                 '53' = 'Wyoming',
                 '54' = 'Africa')
