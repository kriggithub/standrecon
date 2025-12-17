#' Create conclass vector from tree status and decay vectors.
#'
#' @param status_vec A numeric vector of tree status values.
#' @param decay_vec A numeric vector of tree decay values.
#'
#' @returns A numeric vector of tree conclass values.
#' @noRd
.create_conclass <- function(status_vec, decay_vec){
  ifelse(status_vec == 2 & decay_vec %in%  1:2, 3,
  ifelse(status_vec == 2 & decay_vec %in%  3:4, 4,
  ifelse(status_vec == 2 & decay_vec %in%  5:6, 5,
  ifelse(status_vec == 3, 6,
  ifelse(status_vec == 4 & decay_vec %in%  1:2, 3,
  ifelse(status_vec == 4 & decay_vec %in%  3:4, 4,
  ifelse(status_vec == 4 & decay_vec %in%  5:6, 5,
  ifelse(status_vec == 4 & decay_vec == 7, 8,
  ifelse(status_vec == 6, 7,
  ifelse(status_vec == 5, 7, NA))))))))))
}



#' Calculate Fulé tree decomposition rates for sensitivity analysis.
#'
#' @param rate Previous decay rate.
#' @param percentiles Decay sensitivity percentiles as decimals.
#'
#' @returns A numeric vector of decomposition rates.
#' @noRd
.fule_decomp <- function(rate, percentiles){
  log(percentiles)/(log(1 + rate))
}



#' Predict unknown live tree ages from DBH of known aged live trees.
#'
#' @param data Data with species, age, DBH, and tree status columns.
#' @param species_col Species column name in `data` as a string.
#' @param age_col Age column name in `data` as a string.
#' @param dbh_col DBH column name in `data` as a string.
#' @param status_col Tree Status column name in `data` as a string. Live trees coded as 1.
#' @param meas_year Measurement year as a single numeric value.
#'
#' @returns `data` with missing ages for live trees filled where possible, plus a column for established year, `estab_year`.
#' @noRd
.predict_live_tree_age <- function(data,
                                  species_col,
                                  age_col,
                                  dbh_col,
                                  status_col,
                                  meas_year){

  # Pull out column names as strings
  Species <- species_col
  Age <- age_col
  DBH <- dbh_col
  Status <- status_col

  # Loop for each unique species in data
  for (sp in unique(data[[Species]])) {

    # Set LM to predict Age by DBH
    form <- stats::as.formula(paste(Age, "~", DBH))

    # Create new data frame for live trees with know ages for LM
    speciesData <- data[data[[Species]] == sp &
                          !is.na(data[[Age]]) &
                          data[[Status]] == 1, ]

    # Fit LM for known trees (at least 2 needed per species)
    if (nrow(speciesData) > 1) {
      fit <- stats::lm(form, data = speciesData)

      # Grab live trees that have missing age data
      missingRows <- which(is.na(data[[Age]]) &
                             !is.na(data[[Species]]) &
                             data[[Species]] == sp &
                             data[[Status]] == 1)

      # Fill in missing age data for live trees
      if (any(missingRows)) {
        predictedAge <- stats::predict(fit, newdata = data[missingRows, ])
        data[[Age]][missingRows] <- predictedAge
      }
    }
  }

  # Round ages up to next whole number
  data[[Age]] <- ceiling(as.numeric(data[[Age]]))

  # Create new column for established year
  data$estab_year <- meas_year - data[[Age]]

  # Return data
  data
}


