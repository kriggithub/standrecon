liveTreeAge <- function(data, Species, Age, DBH, Status, measYear){

  # Loop for each unique species in data
  for (sp in unique(data[[Species]])) {

    # Set LM to predict Age by DBH
    formula <- as.formula(paste(Age, "~", DBH))

    # Create new data frame for live trees with know ages for LM
    speciesData <- data[data[[Species]] == sp &
                          !is.na(data[[Age]]) &
                          data[[Status]] == 1, ]

    # Fit LM for known trees (at least 2 needed per species)
    if (nrow(speciesData) > 2) {
      fit <- lm(formula, data = speciesData)

      # Grab live trees that have missing age data
      missingRows <- is.na(data[[Age]]) & data[[Species]] == sp

      # Fill in missing age data for live trees
      if (any(missingRows)) {
        predictedAge <- predict(fit, newdata = data[missingRows, ])
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


