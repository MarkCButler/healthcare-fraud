# Define plotting functions used in healthcare_fraud.Rmd.

library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)
library(scales)
library(stlplus)
library(stringr)
library(tidyr)

source('globals.R')


#############################################################################
# Format plot/legend labels for chronic conditions.
#############################################################################

chronic_condition_labels <- case_when(
    str_detect(chronic_conditions, 'Alzheimer') ~ 'Alzheimer\'s disease',
    str_detect(chronic_conditions, 'HeartFailure') ~ 'heart failure',
    str_detect(chronic_conditions, 'KidneyDisease') ~ 'kidney disease',
    str_detect(chronic_conditions, 'IschemicHeart') ~ 'ischemic heart',
    str_detect(chronic_conditions,
               'ObstrPulmonary') ~ 'obstructive\npulmonary\ndisease',
    str_detect(chronic_conditions,
               'RheumatoidArthritis') ~ 'rheumatoid arthritis',
    TRUE ~ tolower(chronic_conditions)
)
names(chronic_condition_labels) <- chronic_conditions

first_char_to_upper <- function(label) {
    first_char <- str_sub(label, 1, 1)
    str_sub(label, 1, 1) <- str_to_upper(first_char)
    return(label)
}

# The input to this function is a vector of strings that specify a chronic
# condition.  The strings in the vector are column names such as
# 'IschemicHeart'.  The vector returned by the function has strings that work
# better in labelling the y axis of a plot, e.g., 'IschemicHeart' has been
# replaced by 'Ischemic heart'.
convert_to_labels <- function(condition_vec) {
    labels <- chronic_condition_labels[condition_vec] %>%
        first_char_to_upper() %>%
        # Replace only the first newline (in cases where 2 newlines are used
        # in legend titles), so that very long labels still include a newline.
        str_replace('\n', ' ')
    return(labels)
}


#############################################################################
# Labels for payment variables.
#############################################################################

payment_variables <- c('total_reimbursed', 'reimbursed_per_visit',
                      'total_cost_of_claims', 'claim_cost_per_visit')
payment_labels <- c(
    'Total payment by insurance per patient',
    'Mean payment by insurance per patient per visit',
    'Total paid to hospitals per patient',
    'Mean paid to hospital per patient per visit'
)
names(payment_labels) <- payment_variables


#############################################################################
# Manipulate data frames for plotting.
#############################################################################

# Pivot a data frame of hospital visits, moving information about chronic
# conditions into a single column.  Before the pivot, each hospital visit is
# represented by a single row; after the pivot.  The resulting data frame is
# used to generate a box plot that breaks down the distribution of a variable
# by chronic condition.
generate_condition_column <- function(data) {
    data <- pivot_longer(data, all_of(chronic_conditions),
                         names_to = 'condition', values_to = 'value') %>%
        filter(value == 'Y') %>%
        select(-value) %>%
        # Modify the conditions names to improve plot readability.
        mutate(condition = convert_to_labels(condition))
    return(data)
}


#############################################################################
# Format plot axes.
#############################################################################

# Function log_scale_dollar is used to avoid repeated verbose calls involving
# the pseudo_log_trans function of the scales package.
log_scale_dollar <- function(axis_label, axis) {
    trans <- pseudo_log_trans(base = 10)
    breaks <- c(0, 10, 100, 1e3, 1e4, 1e5)
    if (axis == 'x') {
        return(
            scale_x_continuous(axis_label, trans = trans, breaks = breaks,
                               labels = label_dollar(largest_with_cents = -1))
        )
    } else if (axis == 'y') {
        return(
            scale_y_continuous(axis_label, trans = trans, breaks = breaks,
                               labels = label_dollar(largest_with_cents = -1))
        )
    } else {
        stop('Argument "axis" must be "x" or "y".')
    }
}


#############################################################################
# Plot a bar chart or histogram in two different formats:
#
# 1. Bars represents count and have a uniform color.
#
# 2. Bars have two colors based on the PotentialFraud flag and show percentage
#    for each color (position = 'fill').
#
#############################################################################

# Plot a bar chart in two formats.  Note that the argument geom_func can be
# geom_bar or geom_col.
plot_bar_charts <- function(fig_base, y_label, geom_func = geom_bar) {
    fig <- fig_base +
        geom_func(fill = 'navyblue') +
        scale_y_continuous(str_c('Number of ', y_label),
                           labels = label_comma())
    print(fig)

    fig <- fig_base +
        geom_func(aes(fill = PotentialFraud),
                 position = 'fill') +
        scale_y_continuous(str_c('Percentage of ', y_label),
                           labels = label_percent()) +
        scale_fill_discrete('Potential fraud')
    print(fig)
}

# Plot a histogram in two formats.
plot_histograms <- function(fig_base, y_label, bins,
                            breaks = waiver()) {
    fig <- fig_base +
        geom_histogram(fill = 'navyblue', bins = bins) +
        scale_y_continuous(str_c('Number of ', y_label),
                           breaks = breaks,
                           labels = label_comma())
    print(fig)

    fig <- fig_base +
        geom_histogram(aes(fill = PotentialFraud),
                       position = 'fill',
                       bins = bins) +
        scale_y_continuous(str_c('Percentage of ', y_label),
                           labels = label_percent()) +
        scale_fill_discrete('Potential fraud')
    # A warning is issued here if the histogram has any bins with count zero.
    # The warning comes from geom_bar, which is apparently called to handle
    # the fill.  The fact that this spurious warning is issued could be viewed
    # as a bug.  The warning can be eliminated from the markdown output with
    # suppressWarnings.
    suppressWarnings(print(fig))
}


#############################################################################
# Overlay histograms and frequency polygons with fill/color determined by a
# categorical variable.
#############################################################################

plot_hist_with_freqpoly <- function(fig_base, variable_name,
                                    legend_title, bins) {
    fig <- fig_base +
        geom_histogram(
            aes_string(fill = variable_name), position = 'identity',
            bins = bins, alpha = 0.15
        ) +
        geom_freqpoly(
            aes_string(color = variable_name), position = 'identity',
            bins = bins, size = 0.7
        ) +
        labs(fill = legend_title, color = legend_title)
    print(fig)
}


#############################################################################
# Plot time-series, manipulate data for time-series plots.
#############################################################################

extract_series_data <- function(claim_data, date_range) {
    series_data <-  claim_data %>%
        group_by(ClaimStartDt) %>%
        summarise(count = n(), .groups = 'drop') %>%
        # The full join guarantees that there are no missing days in the
        # time-series data.  This is needed for seasonal analysis.
        full_join(claim_dates$daily, by = 'ClaimStartDt') %>%
        replace_na(replace = list(count = 0)) %>%
        filter(ClaimStartDt >= date_range[1],
               ClaimStartDt <= date_range[2]) %>%
        arrange(ClaimStartDt)
    return(series_data)
}

plot_series <- function(to_plot, title) {
    fig <- to_plot %>%
        ggplot(aes(x = ClaimStartDt, y = count)) +
        geom_point(color = 'navyblue') +
        geom_line(color = 'navyblue') +
        xlab('Date') +
        ylab('Number of visits') +
        ggtitle(title)
    return(fig)
}

plot_seasonality <- function(series_data, stl_model, title) {
    seasonality <- seasonal(stl_model)[1:7]
    weekday <- wday(series_data$ClaimStartDt[1:7], label = TRUE)
    seasonality_data <- data.frame(weekday = weekday,
                                   seasonality = seasonality)

    fig <- seasonality_data %>%
        ggplot(aes(x = weekday, y = seasonality, group = 1)) +
        geom_line(color = 'navyblue') +
        geom_point(color = 'navyblue') +
        xlab('Weekday') +
        ylab('Number of visits') +
        ggtitle(title)

    return(fig)
}


#############################################################################
# Generate miscellaneous plots for the rendered markdown file.
#
# These functions are defined in order to simplify the R markdown file.
#############################################################################

plot_chronic_condition_claim_counts <- function(patients) {
    for (variable_name in chronic_conditions) {
        condition_label <- chronic_condition_labels[variable_name]
        legend_title <- first_char_to_upper(condition_label)
        condition_label <- str_replace_all(condition_label, '\n', ' ')
        title_base <- str_c('Distribution of ', condition_label)

        fig <- patients %>%
            ggplot(aes_string(x = 'claim_type', fill = variable_name)) +
            geom_bar(position = 'fill') +
            scale_y_continuous('Percentage of patients',
                               labels = label_percent()) +
            scale_x_discrete(name = '') +
            scale_fill_discrete(legend_title) +
            ggtitle(title_base)
        print(fig)

        for (claim_type in claim_types) {
            to_plot <- patients %>%
                filter(claim_type == .env$claim_type)
            x_axis_values <- seq(min(to_plot$patient_claim_count),
                                 max(to_plot$patient_claim_count))
            title <- str_c(title_base, ', ', claim_type, 's')
            fig <- to_plot %>%
                ggplot(aes_string(x = 'patient_claim_count',
                                  fill = variable_name)) +
                geom_bar(position = 'fill') +
                scale_x_discrete('Number of visits',
                                 limits = factor(x_axis_values)) +
                scale_fill_discrete(legend_title) +
                ylab('Number of patients') +
                ggtitle(title)
            print(fig)
        }
    }
}

plot_payments <- function(patients) {
    for (claim_type in claim_types) {
        to_select <- c(payment_variables, chronic_conditions)
        to_plot <- patients %>%
            filter(claim_type == .env$claim_type) %>%
            select(all_of(to_select)) %>%
            # Patients for which the only visit has visit_cost == 0 show up
            # with missing values for the columns in payment_variables.
            drop_na() %>%
            generate_condition_column()
        for (variable_name in payment_variables) {
            title <- str_c(payment_labels[variable_name], ', ',
                           claim_type, 's')
            fig <- to_plot %>%
                ggplot(aes_string(x = 'condition', y = variable_name)) +
                geom_boxplot(aes(fill = condition)) +
                log_scale_dollar(axis_label = 'Payment amount', axis = 'y') +
                xlab('Chronic condition') +
                coord_flip() +
                guides(fill = FALSE) +
                ggtitle(title)
            print(fig)
        }
    }
}

plot_provider_claim_counts <- function(claim_counts, claim_type) {
    plot_data <- claim_counts %>%
        filter(claim_type == .env$claim_type, claim_year == 2009)

    to_plot <- plot_data %>%
        group_by(Provider) %>%
        summarise(count = sum(claim_count), .groups = 'drop') %>%
        left_join(
            select(plot_data, Provider, PotentialFraud),
            by = 'Provider'
        )

    title <- str_c('Number of claims per provider in 2009, ', claim_type, 's')
    fig_base <- to_plot %>%
        ggplot(aes(x = count)) +
        scale_x_continuous('Number of claims',
                           trans = log_trans(base = 10),
                           breaks = c(1, 10, 100, 1000),
                           labels = label_comma(accuracy = 1)) +
        ggtitle(title)
    plot_histograms(fig_base, 'providers', bins = 30)

    cat('The maximum number of ', claim_type, ' claims for a provider is ',
        max(to_plot$count), '.\n', sep = '')

    to_plot <- plot_data %>%
        group_by(claim_month, PotentialFraud) %>%
        summarise(count = sum(claim_count), .groups = 'drop')

    title <- str_c('Number of claims per month in 2009, ', claim_type, 's')
    fig_base <- to_plot %>%
        ggplot(aes(x = claim_month, y = count)) +
        xlab('Month') +
        ggtitle(title)
    plot_bar_charts(fig_base, 'claims', geom_func = geom_col)

    to_display <- plot_data %>%
        group_by(claim_month) %>%
        summarise(max_claims = max(claim_count), .groups = 'drop')
    table_alignment <- list(className = 'dt-center', targets = '_all')
    return(datatable(
        to_display,
        rownames = FALSE,
        colnames = c('Month',
                     str_c('Max ', claim_type, ' claims per provider')),
        options = list(
            dom = 't',
            columnDefs = list(table_alignment),
            pageLength = 12
        )
    ))
}

plot_payment_distribution <- function(data, variable, title) {
    fig <- data %>%
        ggplot(aes(x = {{variable}})) +
        geom_histogram(fill = 'navyblue', bins = 30) +
        log_scale_dollar('Payment amount', 'x') +
        scale_y_continuous('Number of visits', labels = label_comma()) +
        ggtitle(title)
    return(fig)
}

plot_cost_vs_duration <- function(fig_base, title) {
    fig <- fig_base +
        geom_point(aes(color = PotentialFraud)) +
        scale_y_continuous('Claim amount', labels = label_dollar()) +
        scale_color_discrete('Potential fraud') +
        ggtitle(title)
    print(fig)
}

plot_cost_per_day <- function(to_plot, variable_name, claim_type,
                              duration_label) {

    # Simplify code by creating a cost_per_day column equal to the column
    # specified by variable_name.
    to_plot <- to_plot %>%
        mutate(cost_per_day = .data[[variable_name]])

    title <- str_c('Cost per day of ', duration_label, ' duration, ',
                   claim_type, ' visits')
    axis_label <- str_c('Cost per day of ', duration_label, ' duration')
    legend_title <- 'Potential fraud'
    fig_base <- to_plot %>%
        ggplot(aes(x = cost_per_day)) +
        ylab('Number of claims') +
        log_scale_dollar(axis_label, 'x') +
        ggtitle(title)
    plot_hist_with_freqpoly(fig_base, variable_name = 'PotentialFraud',
                            legend_title = legend_title, bins = 60)

    # Since the histograms show the same distribution to a high level of
    # accuracy, use a qq plot to compare the distributions.
    to_plot <- to_plot %>%
        select(PotentialFraud, cost_per_day) %>%
        mutate(cost_per_day = log10(cost_per_day + 1))
    temp_data <- to_plot %>%
        filter(PotentialFraud == 'No')
    cost_per_day_no_fraud <- temp_data$cost_per_day %>%
        as.numeric()

    temp_data <- to_plot %>%
        filter(PotentialFraud == 'Yes')
    cost_per_day_yes_fraud <- temp_data$cost_per_day %>%
        as.numeric()

    title <- str_c('Q-Q plot, log(cost per day of ', duration_label,
                   ' duration)')
    qqplot(cost_per_day_no_fraud,
           cost_per_day_yes_fraud,
           xlab = 'No fraud',
           ylab = 'Possible fraud',
           main = title)
}

plot_fraction_original <- function(duplicates_data, claim_type) {
    to_plot <- duplicates_data %>%
        filter(claim_type == .env$claim_type) %>%
        filter_out_no_codes() %>%
        group_by(Provider, PotentialFraud) %>%
        summarise(
            original_claims = sum(provider_is_original),
            number_of_claims = n(),
            .groups = 'drop'
        ) %>%
        mutate(fraction_original = original_claims / number_of_claims)

    title <- str_c('Percentage of claims by provider that are original, ',
                   claim_type, 's')
    x_axis_label <- 'Percentage of claims by provider that are original'
    fig_base <- to_plot %>%
        ggplot(aes(x = fraction_original)) +
        scale_x_continuous(x_axis_label,
                           labels = label_percent()) +
        ggtitle(title)
    plot_histograms(fig_base, y_label = 'providers', bins = 30)

    title <- str_c('Number of claims vs percent that are original, ',
                   claim_type, 's')
    fig <- to_plot %>%
        ggplot(aes(x = fraction_original, y = number_of_claims)) +
        geom_point(aes(color = PotentialFraud)) +
        ylab('Number of claims by provider') +
        scale_x_continuous(x_axis_label,
                           labels = label_percent()) +
        scale_color_discrete('Potential fraud') +
        ggtitle(title)
    print(fig)
}
