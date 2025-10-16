#' Predict unknown live tree ages from DBH of known live trees
#'
#' @param data Data with species, age, DBH, and tree status columns.
#' @param speciesCol Species column in data.
#' @param ageCol Age column in data.
#' @param dbhCol DBH column in data.
#' @param statusCol Tree Status column in data.
#' @param measYear Year that data was measured.
#'
#' @returns Data with predicted ages for all live trees.
#' @export
#'
#' @examples
liveTreeAge <- function(data, speciesCol = Species, ageCol = Age, dbhCol = DBH, statusCol = Status, measYear){

  # Pull out column names as strings
  Species <- deparse(substitute(speciesCol))
  Age <- deparse(substitute(ageCol))
  DBH <- deparse(substitute(dbhCol))
  Status <- deparse(substitute(statusCol))

  # Loop for each unique species in data
  for (sp in unique(data[[Species]])) {

    # Set LM to predict Age by DBH
    formula <- stats::as.formula(paste(Age, "~", DBH))

    # Create new data frame for live trees with know ages for LM
    speciesData <- data[data[[Species]] == sp &
                          !is.na(data[[Age]]) &
                          data[[Status]] == 1, ]

    # Fit LM for known trees (at least 2 needed per species)
    if (nrow(speciesData) > 1) {
      fit <- stats::lm(formula, data = speciesData)

      # Grab live trees that have missing age data
      missingRows <- is.na(data[[Age]]) & data[[Species]] == sp & data[[Status]] == 1

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
  data$estabYear <- measYear - data[[Age]]

  # Return data
  data
}


