# This script loads csv data and uses dplyr operations to create/manipulate
# data frames that are used for data analysis.

library(dplyr)
library(forcats)
library(lubridate)
library(purrr)
library(stringr)
library(tidyr)

source('globals.R')


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
# Helper funcctions for manipulating data frames.
#############################################################################

# Drop rows that have NA for all code columns.
filter_out_no_codes <- function(code_data) {
    code_colnames <- intersect(all_code_colnames, colnames(code_data))
    bool_index <- code_data %>%
        select(all_of(code_colnames)) %>%
        # The goal is to replace a row by FALSE if all values are NA.  This
        # can be done with the pipeline
        #
        # is.na() %>% `!`() %>% rowSums() %>% as.logical()
        #
        # but the code becomes difficult to understand.  Using apply allows
        # the filtering operation to be expressed more directly.
        is.na() %>%
        as.matrix() %>%
        apply(MARGIN = 1,
              function(row) {
                  return(!all(row))
              })
    return(code_data[bool_index, ])
}

# Add the column PotentialFraud to a data frame.  The functions in the current
# script are designed to handle either training data or test data.  Some of
# them take a boolean argument that indicates whether the training data set is
# being processed.  If so, the function add_fraud_flag can be used to
# add the training label to a data frame that has a Provider column.
add_fraud_flag <- function(data) {
    data <- left_join(data, data_frames$train_provider, by = 'Provider')
    return(data)
}


#############################################################################
# Characterize missing values.
#############################################################################

na_counts <- map(data_frames,
                 get_na_counts)

# The column BeneId uniquely identifies patients, so group by BeneId allows a
# calculation of the number of visits per patient.
get_visit_counts <- function(claim_data) {
    visit_counts <- claim_data %>%
        group_by(BeneID) %>%
        summarise(visit_count = n(), .groups = 'drop')
    return(visit_counts)
}


#############################################################################
# Add columns to a data frame of claims, each representing a hospital visit.
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
#   claim_data:  a data frame with rows that specify visits to a hospital
#   patient_data:  a data frame with rows giving information about patients
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to add columns to the argument
# claim_data, which is then returned.
#
augment_claim_data <- function(claim_data, patient_data, visit_type,
                               training = TRUE) {
    visit_counts <- get_visit_counts(claim_data)
    claim_data <- claim_data %>%
        mutate(visit_type = !!visit_type) %>%
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
        claim_data <- claim_data %>%
            mutate(across(c(AdmissionDt, DischargeDt),
                          ~ as_date(., format = '%Y-%m-%d'))) %>%
            mutate(visit_duration = DischargeDt - AdmissionDt)
    }
    patient_data <- patient_data %>%
        select(BeneID, DOB, DOD) %>%
        mutate(across(c(DOB, DOD),
                      ~ as_date(., format = '%Y-%m-%d')))
    claim_data <- left_join(claim_data, patient_data, by = 'BeneID') %>%
        mutate(age = (ClaimStartDt - DOB) / 365.24)
    if (training) {
        claim_data <- add_fraud_flag(claim_data)
    }

    return(claim_data)
}

inpatient_claims <- augment_claim_data(data_frames$train_inpatient,
                                       data_frames$train_beneficiary,
                                       visit_type = 'inpatient')
outpatient_claims <- augment_claim_data(data_frames$train_outpatient,
                                        data_frames$train_beneficiary,
                                        visit_type = 'outpatient')
inpatient_only_columns <- c('AdmissionDt', 'DischargeDt',
                            'DiagnosisGroupCode', 'visit_duration')
claims <- rbind(
    select(inpatient_claims, -all_of(inpatient_only_columns)),
    outpatient_claims,
    make.row.names = FALSE
)


#############################################################################
# Add columns to a data frame of hospital patients.
#############################################################################

# For each patient, calculate the median age of that patient during the
# hospital visits in the data set.  (The visits occurred within roughly a
# year-long period.)
patient_ages <- claims %>%
    select(BeneID, age) %>%
    group_by(BeneID) %>%
    summarise(age = median(age), .groups = 'drop')

# Arguments:
#
#   patient_data:  a data frame with rows giving information about patients
#   claim_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to add columns to the argument
# patient_data, which is then returned.
#
augment_patient_data <- function(patient_data, claim_data, visit_type) {
    patient_data <- patient_data %>%
        filter(BeneID %in% claim_data$BeneID) %>%
        mutate(visit_type = !!visit_type) %>%
        rename_with(.fn = clean_condition_names,
                    .cols = all_of(chronic_conditions_raw)) %>%
        mutate(across(all_of(chronic_conditions),
                      ~ fct_recode(factor(.), 'Y' = '1', 'N' = '2')))
    visit_counts <- get_visit_counts(claim_data)
    provider_counts <- claim_data %>%
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
    payments <- claim_data %>%
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
        mutate(visit_type = !!visit_type)

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

# Arguments:
#
#   claim_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about doctors and
# generate a data frame with doctors as observations.
#
get_doctor_data <- function(claim_data, visit_type) {
    doctor_data <- claim_data %>%
        select(Provider, all_of(doctor_colnames)) %>%
        pivot_longer(all_of(doctor_colnames), names_to = 'role',
                     values_to = 'doctor') %>%
        select(-role) %>%
        drop_na(doctor) %>%
        distinct() %>%
        group_by(doctor) %>%
        summarise(provider_count = n(), .groups = 'drop') %>%
        mutate(visit_type = !!visit_type)
    return(doctor_data)
}

inpatient_doctors <- get_doctor_data(
    data_frames$train_inpatient,
    'inpatient'
)
outpatient_doctors <- get_doctor_data(
    data_frames$train_outpatient,
    'outpatient'
)
doctors <- rbind(inpatient_doctors, outpatient_doctors,
                 make.row.names = FALSE)


#############################################################################
# Generate two data frames of provider information.  One of these data frames
# gives information about monthly number of claims per provider, and the other
# gives information about mean claim duration per provider.
#
# These data frames are kept separate because for the first, a unique row id
# would consist of (provider, year, month, visit_type), while for the second,
# the (provider, visit_type) is a unique row id.
#############################################################################

# Arguments:
#
#   claim_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about monthly
# claim counts per provider
#
get_claim_counts <- function(claim_data, visit_type,
                             training = TRUE) {
    claim_counts <- claim_data %>%
        mutate(
            ClaimStartDt = as_date(ClaimStartDt, format = '%Y-%m-%d')
        ) %>%
        mutate(
            claim_year = year(ClaimStartDt),
            claim_month = month(ClaimStartDt, label = TRUE, abbr = TRUE)
        ) %>%
        group_by(Provider, claim_year, claim_month) %>%
        summarise(claim_count = n(), .groups = 'drop') %>%
        mutate(visit_type = !!visit_type)
    if (training) {
        claim_counts <- add_fraud_flag(claim_counts)
    }
    return(claim_counts)
}

inpatient_claim_counts <- get_claim_counts(
    data_frames$train_inpatient,
    'inpatient'
)
outpatient_claim_counts <- get_claim_counts(
    data_frames$train_outpatient,
    'outpatient'
)
claim_counts <- rbind(inpatient_claim_counts, outpatient_claim_counts,
                      make.row.names = FALSE)

# Arguments:
#
#   claim_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about mean claim
# durations per provider
#
get_claim_durations <- function(claim_data, visit_type,
                                training = TRUE) {
    claim_durations <- claim_data %>%
        mutate(across(c(ClaimStartDt, ClaimEndDt),
                      ~ as_date(., format = '%Y-%m-%d'))) %>%
        mutate(claim_duration = ClaimEndDt - ClaimStartDt) %>%
        group_by(Provider) %>%
        summarise(mean_claim_duration = mean(claim_duration),
                  .groups = 'drop') %>%
        mutate(visit_type = !!visit_type)
    if (training) {
        claim_durations <- add_fraud_flag(claim_durations)
    }
    return(claim_durations)
}

inpatient_claim_durations <- get_claim_durations(
    data_frames$train_inpatient,
    'inpatient'
)
outpatient_claim_durations <- get_claim_durations(
    data_frames$train_outpatient,
    'outpatient'
)
claim_durations <- rbind(inpatient_claim_durations, outpatient_claim_durations,
                         make.row.names = FALSE)


#############################################################################
# Generate a data frame that gives information about the diagnosis codes most
# frequently used when a patient is admitted.
#############################################################################

# Arguments:
#
#   claim_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about the most
# frequency admission codes and generate a data frame with the frequent
# admission codes as observations.
#
find_frequent_codes <- function(claim_data, visit_type) {
    frequent_codes <- claim_data %>%
        mutate(code = ClmAdmitDiagnosisCode) %>%
        group_by(code) %>%
        summarise(count = n(), .groups = 'drop') %>%
        drop_na() %>%
        arrange(desc(count)) %>%
        head(n = 5) %>%
        mutate(description = code_descriptions[code],
               visit_type = !!visit_type)
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


#############################################################################
# Generate data frames that give information about duplicated claims.
#############################################################################

# Names of columns that contain a code for a hospital visit.
all_claim_colnames <- union(colnames(data_frames$train_inpatient),
                            colnames(data_frames$train_outpatient))
bool_index <- str_detect(tolower(all_claim_colnames), 'code')
all_code_colnames <- all_claim_colnames[bool_index]

# Arguments:
#
#   claim_data:  a data frame with rows that specify visits to a hospital
#   visit_type:  either 'inpatient' or 'outpatient'
#
# The function uses dplyr operations to extract information about duplicate
# claims, i.e., claims with different ClaimID but identical values for all
# codes associated with the claim.
#
get_duplicates <- function(claim_data, visit_type,
                           training = TRUE) {
    code_colnames <- intersect(all_code_colnames, colnames(claim_data))

    # Starting with claim_data, gradually build a code_data data frame with
    # information about duplicated codes.
    code_data <- claim_data %>%
        mutate(across(ClaimStartDt,
                      ~ as_date(., format = '%Y-%m-%d'))) %>%
        select(ClaimStartDt, BeneID, ClaimID, Provider,
               all_of(c(code_colnames, doctor_colnames)))

    code_data <- code_data %>%
        # For each claim, find how many identical claims there are (where
        # identical means "has exactly the same set of codes").  Also find how
        # many distinct providers filed an identical claim and the earliest
        # ClaimStartDt for each set of identical claims.
        group_by(across(all_of(code_colnames))) %>%
        summarise(identical_claim_count = n_distinct(ClaimID),
                  provider_count = n_distinct(Provider),
                  first_date = min(ClaimStartDt), .groups = 'drop') %>%
        mutate(
            identical_claims_per_provider = identical_claim_count / provider_count
        ) %>%
        # Treating the set of codes for a claim as a row ID, join the new
        # columns to code_data.
        right_join(code_data, by = code_colnames) %>%
        # Add column 'original' indicating whether the ClaimStartDt for that
        # claim is the earliest among the set of identical claims.
        mutate(original = (ClaimStartDt == first_date))

    # For each set of identical claims, find how many rows are labeled as
    # original, i.e., how many have the earliest ClaimStartDt.  Add this as a
    # column to the full data frame code_data.
    code_data <- code_data %>%
        filter(original == TRUE) %>%
        group_by(across(all_of(code_colnames))) %>%
        summarise(original_claim_count = n(), .groups = 'drop') %>%
        right_join(code_data, by = code_colnames)

    # Next add a column determining whether the provider for a claim is one of
    # the providers for an "original" claim (i.e., a claim with the earliest
    # ClaimStartDt among identical claims).  This is done by first finding all
    # ClaimID's for which the provider is one of the original providers.
    code_data <- code_data %>%
        filter(original == TRUE) %>%
        select(Provider, all_of(code_colnames)) %>%
        distinct() %>%
        rename(original_provider = Provider) %>%
        right_join(code_data, by = code_colnames) %>%
        # This filtering step gives a data frame of claims that have one of
        # the original providers for the corresponding set of identical claims.
        filter(Provider == original_provider) %>%
        select(ClaimID) %>%
        mutate(provider_is_original = TRUE) %>%
        full_join(code_data, by = 'ClaimID') %>%
        replace_na(list(provider_is_original = FALSE)) %>%
        # Add a column indicating whether the claim is for an inpatient visit
        # or an outpatient visit.
        mutate(visit_type = !!visit_type)

    if (training) {
        code_data <- add_fraud_flag(code_data)
    }

    return(code_data)
}

inpatient_duplicates <- get_duplicates(data_frames$train_inpatient, 'inpatient')
outpatient_duplicates <- get_duplicates(data_frames$train_outpatient, 'outpatient')
