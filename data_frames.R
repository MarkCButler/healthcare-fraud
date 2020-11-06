# This script loads csv data and uses dplyr operations to create data frames
# that are used for data analysis.

library(dplyr)
library(forcats)
library(lubridate)
library(purrr)
library(stringr)
library(tidyr)


#############################################################################
# Load data from csv files.
#############################################################################

data_filenames <- list(
    train_outpatient = 'data/Train_Outpatientdata-1542865627584.csv',
    train_inpatient = 'data/Train_Inpatientdata-1542865627584.csv',
    train_provider = 'data/Train-1542865627584.csv',
    train_beneficiary = 'data/Train_Beneficiarydata-1542865627584.csv',
    test_outpatient = 'data/Test_Outpatientdata-1542969243754.csv',
    test_inpatient = 'data/Test_Inpatientdata-1542969243754.csv',
    test_provider = 'data/Test-1542969243754.csv',
    test_beneficiary = 'data/Test_Beneficiarydata-1542969243754.csv'
)

read_data <- function(file) {
    data <- read.csv(file, stringsAsFactors = FALSE,
                     na.strings = c('NA', ''))
    return(data)
}

data_frames <- map(data_filenames, read_data)

# Characterize missing values, ignoring columns such as ClmDiagnosisCode_n and
# ClmProcedureCode_n.
get_na_counts <- function(data) {
    na_counts <- data.frame(name = colnames(data),
                            count = as.integer(colSums(is.na(data))),
                            stringsAsFactors = FALSE) %>%
        filter(str_detect(name, 'Clm.+Code', negate = TRUE)) %>%
        arrange(desc(count)) %>%
        filter(count != 0)
    return(na_counts)
}


#############################################################################
# Characterize missing values.
#############################################################################

na_counts <- map(data_frames,
                 get_na_counts)

# The column BeneId uniquely identifies patients, so group by BeneId allows a
# calculation of the number of visits per patient.
get_visit_counts <- function(visit_data) {
    visit_counts <- visit_data %>%
        group_by(BeneID) %>%
        summarise(visit_count = n(), .groups = 'drop')
    return(visit_counts)
}


#############################################################################
# Add columns to a data frame of hospital visits.
#############################################################################

# For time-series analysis, we want dates binned into weekly periods.  In
# order to facilite time-series analysis, generate a data frame of dates
# corresponding to the beginning of each weekly period.  Avoid errors
# associated with partial weeks at the end of a year by using lubridate's
# isoweek and isoyear functions.
get_claim_dates <- function(inpatient_data, outpatient_data) {

    all_claim_dates <- c(inpatient_data$ClaimStartDt,
                         outpatient_data$ClaimStartDt) %>%
        as_date(format = '%Y-%m-%d')
    start_date <- min(all_claim_dates)
    end_date <- max(all_claim_dates)
    daily_dates <- seq(start_date, end_date, by = 1)

    weekly_dates <- data.frame(date = daily_dates) %>%
        mutate(year = isoyear(date), week = isoweek(date)) %>%
        group_by(year, week) %>%
        summarise(week_date = min(date), .groups = 'drop')

    claim_dates <- list(
        daily = data.frame(ClaimStartDt = daily_dates),
        weekly = weekly_dates
    )

    return(claim_dates)
}

claim_dates <- get_claim_dates(
    data_frames$train_inpatient, data_frames$train_outpatient
)

# Arguments:
#
#   visit_data:  a data frame with rows that specify visits to a hospital
#   patient_data:  a data frame with rows giving information about patients
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to add columns to the argument
# visit_data, which is then returned.
#
augment_visit_data <- function(visit_data, patient_data, visit_type,
                               add_fraud_flag = TRUE) {
    visit_counts <- get_visit_counts(visit_data)
    visit_data <- visit_data %>%
        mutate(type = visit_type) %>%
        replace_na(list(DeductibleAmtPaid = 0)) %>%
        mutate(visit_cost = InscClaimAmtReimbursed + DeductibleAmtPaid) %>%
        mutate(across(c(ClaimStartDt, ClaimEndDt),
                      ~ as_date(., format = '%Y-%m-%d'))) %>%
        mutate(claim_duration = ClaimEndDt - ClaimStartDt) %>%
        mutate(year = isoyear(ClaimStartDt),
               week = isoweek(ClaimStartDt)) %>%
        left_join(claim_dates$weekly, by = c('year', 'week')) %>%
        select(-c(year, week)) %>%
        left_join(visit_counts, by = 'BeneID')
    if (visit_type == 'inpatient') {
        visit_data <- visit_data %>%
            mutate(across(c(AdmissionDt, DischargeDt),
                          ~ as_date(., format = '%Y-%m-%d'))) %>%
            mutate(visit_duration = DischargeDt - AdmissionDt)
    }
    patient_data <- patient_data %>%
        select(BeneID, DOB, DOD) %>%
        mutate(across(c(DOB, DOD),
                      ~ as_date(., format = '%Y-%m-%d')))
    visit_data <- left_join(visit_data, patient_data, by = 'BeneID') %>%
        mutate(age = (ClaimStartDt - DOB) / 365.24)
    if (add_fraud_flag) {
        visit_data <- left_join(visit_data, data_frames$train_provider,
                                by = 'Provider')
    }

    return(visit_data)
}

inpatient_visits <- augment_visit_data(data_frames$train_inpatient,
                                       data_frames$train_beneficiary,
                                       visit_type = 'inpatient')
outpatient_visits <- augment_visit_data(data_frames$train_outpatient,
                                        data_frames$train_beneficiary,
                                        visit_type = 'outpatient')
inpatient_only_columns <- c('AdmissionDt', 'DischargeDt',
                            'DiagnosisGroupCode', 'visit_duration')
patient_visits <- rbind(
    select(inpatient_visits, -all_of(inpatient_only_columns)),
    outpatient_visits,
    make.row.names = FALSE
)


#############################################################################
# Add columns to a data frame of hospital patients.
#############################################################################

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

# For each patient, calculate the median age of that patient during the
# hospital visits in the data set.  (The visits occurred within roughly a
# year-long period.)
patient_ages <- patient_visits %>%
    select(BeneID, age) %>%
    group_by(BeneID) %>%
    summarise(age = median(age), .groups = 'drop')

# Arguments:
#
#   patient_data:  a data frame with rows giving information about patients
#   visit_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to add columns to the argument
# patient_data, which is then returned.
#
augment_patient_data <- function(patient_data, visit_data, visit_type) {
    patient_data <- patient_data %>%
        filter(BeneID %in% visit_data$BeneID) %>%
        mutate(type = visit_type) %>%
        rename_with(.fn = clean_condition_names,
                    .cols = all_of(chronic_conditions_raw)) %>%
        mutate(across(all_of(chronic_conditions),
                      ~ fct_recode(factor(.), 'Y' = '1', 'N' = '2')))
    visit_counts <- get_visit_counts(visit_data)
    provider_counts <- visit_data %>%
        select(Provider, BeneID) %>%
        distinct() %>%
        group_by(BeneID) %>%
        summarise(provider_count = n(), .groups = 'drop')
    # For both the training and test sets, the inpatient data frame has
    # missing values for DeductibleAmtPaid, but the minimum value for the
    # column is about $1000.  So in this context, it seems that NA is used in
    # place of zero.
    #
    # For the outpatient data frames, however, there are many zero values in
    # DeductibleAmtPaid but no missing values.
    #
    # However, it appears that there are missing values in
    # InscClaimAmtReimbursed and DeductibleAmtPaid for both the inpatient and
    # outpatient data.  For a number of rows, InscClaimAmtReimbursed and
    # DeductibleAmtPaid are both zero, which gives a cost of zero for the
    # visit to the hospital.  For the training set of outpatient visits, for
    # instance, there are about 19k rows like this (out of about 520k). Data
    # analysis involving vist cost needs to take account of this.
    payments <- visit_data %>%
        select(BeneID, InscClaimAmtReimbursed, DeductibleAmtPaid) %>%
        replace_na(list(DeductibleAmtPaid = 0)) %>%
        mutate(visit_cost = InscClaimAmtReimbursed + DeductibleAmtPaid) %>%
        filter(visit_cost != 0) %>%
        select(-DeductibleAmtPaid) %>%
        group_by(BeneID) %>%
        summarise(
            total_reimbursed = sum(InscClaimAmtReimbursed),
            reimbursed_per_visit = mean(InscClaimAmtReimbursed),
            total_charged = sum(visit_cost),
            charged_per_visit = mean(visit_cost),
            .groups = 'drop'
        )
    patient_data <- patient_data %>%
        left_join(payments, by = 'BeneID') %>%
        left_join(patient_ages, by = 'BeneID') %>%
        left_join(provider_counts, by = 'BeneID') %>%
        left_join(visit_counts, by = 'BeneID') %>%
        mutate(type = visit_type)

    return(patient_data)
}

inpatients <- augment_patient_data(
    data_frames$train_beneficiary,
    data_frames$train_inpatient,
    'inpatient'
)
outpatients <- augment_patient_data(
    data_frames$train_beneficiary,
    data_frames$train_outpatient,
    'outpatient'
)
patients <- rbind(inpatients, outpatients,
                  make.row.names = FALSE)


#############################################################################
# Generate a data frame of doctor information, i.e., with doctors as
# observations.
#############################################################################

doctor_colnames <- c('AttendingPhysician', 'OperatingPhysician',
                     'OtherPhysician')

# Arguments:
#
#   visit_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about doctors and
# generate a data frame with doctors as observations.
#
get_doctor_data <- function(visit_data, visit_type) {
    doctor_data <- visit_data %>%
        select(Provider, all_of(doctor_colnames)) %>%
        pivot_longer(all_of(doctor_colnames), names_to = 'role',
                     values_to = 'doctor') %>%
        select(-role) %>%
        drop_na(doctor) %>%
        distinct() %>%
        group_by(doctor) %>%
        summarise(provider_count = n(), .groups = 'drop') %>%
        mutate(type = visit_type)
    return(doctor_data)
}

in_doctors <- get_doctor_data(
    data_frames$train_inpatient,
    'inpatient'
)
out_doctors <- get_doctor_data(
    data_frames$train_outpatient,
    'outpatient'
)
doctors <- rbind(in_doctors, out_doctors,
                 make.row.names = FALSE)


#############################################################################
# Generate two data frames of provider information.  One of these data frames
# gives information about monthly number of claims per provider, and the other
# gives information about mean claim duration per provider.
#
# These data frames are kept separate for the first, a unique row id would
# consist of (provider, year, month, visit_type), while for the second, the
# (provider, visit_type) is a unique row id.
#############################################################################

# Arguments:
#
#   visit_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about monthly
# claim counts per provider
#
get_claim_counts <- function(visit_data, visit_type,
                             add_fraud_flag = TRUE) {
    claim_counts <- visit_data %>%
        mutate(
            ClaimStartDt = as_date(ClaimStartDt, format = '%Y-%m-%d')
        ) %>%
        mutate(claim_year = year(ClaimStartDt),
               claim_month = month(ClaimStartDt, label = TRUE, abbr = TRUE)) %>%
        group_by(Provider, claim_year, claim_month) %>%
        summarise(claim_count = n(), .groups = 'drop') %>%
        mutate(type = visit_type)
    if (add_fraud_flag) {
        claim_counts <- left_join(claim_counts, data_frames$train_provider,
                                by = 'Provider')
    }
    return(claim_counts)
}

in_claim_counts <- get_claim_counts(
    data_frames$train_inpatient,
    'inpatient'
)
out_claim_counts <- get_claim_counts(
    data_frames$train_outpatient,
    'outpatient'
)
claim_counts <- rbind(in_claim_counts, out_claim_counts,
                      make.row.names = FALSE)

# Arguments:
#
#   visit_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about mean claim
# durations per provider
#
get_claim_durations <- function(visit_data, visit_type,
                                add_fraud_flag = TRUE) {
    claim_durations <- visit_data %>%
        mutate(across(c(ClaimStartDt, ClaimEndDt),
                      ~ as_date(., format = '%Y-%m-%d'))) %>%
        mutate(claim_duration = ClaimEndDt - ClaimStartDt) %>%
        group_by(Provider) %>%
        summarise(mean_claim_duration = mean(claim_duration),
                  .groups = 'drop') %>%
        mutate(type = visit_type)
    if (add_fraud_flag) {
        claim_durations <- left_join(
            claim_durations, data_frames$train_provider, by = 'Provider'
        )
    }
    return(claim_durations)
}

in_claim_durations <- get_claim_durations(
    data_frames$train_inpatient,
    'inpatient'
)
out_claim_durations <- get_claim_durations(
    data_frames$train_outpatient,
    'outpatient'
)
claim_durations <- rbind(in_claim_durations, out_claim_durations,
                         make.row.names = FALSE)


#############################################################################
# Generate a data frame that gives information about the diagnosis codes most
# frequently used when a patient is admitted.
#############################################################################

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

# Arguments:
#
#   visit_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about the most
# frequency admission codes and generate a data frame with the frequent
# admission codes as observations.
#
find_frequent_codes <- function(visit_data, visit_type) {
    frequent_codes <- visit_data %>%
        mutate(code = ClmAdmitDiagnosisCode) %>%
        group_by(code) %>%
        summarise(count = n(), .groups = 'drop') %>%
        drop_na() %>%
        arrange(desc(count)) %>%
        head(n = 5) %>%
        mutate(description = code_descriptions[code], type = visit_type)
    return(frequent_codes)
}
freq_admit_codes_inpatient <- find_frequent_codes(
    data_frames$train_inpatient,
    'inpatient'
)
freq_admit_codes_outpatient <- find_frequent_codes(
    data_frames$train_outpatient,
    'outpatient'
)
freq_admit_codes <- rbind(
    freq_admit_codes_inpatient,
    freq_admit_codes_outpatient,
    make.row.names = FALSE
)
