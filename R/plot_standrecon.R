plot_standrecon <- function(out,
                            years,
                            percentiles,
                            density_ba = c("density", "basal_area"),
                            species_col = "species",
                            year_col = "year",
                            percentile_col = "percentile",
                            type_col = "type",
                            measured_label = "measured",
                            reconstructed_label = "reconstructed",
                            main = NULL,
                            ylab = NULL,
                            xlab = "",
                            col = NULL,
                            legend_pos = "topright",
                            ...) {

  # Match function to whether user wants to plot stem density or basal area
  density_ba <- match.arg(density_ba)

  # Choose the correct column for plotting
  value_col <- if (density_ba == "density") "stem_density" else "basal_area"

  # Update main plot title based on user choice
  if (is.null(main)) {
    main <- if (density_ba == "density") {
      "Stem density by species"
    } else {
      "Basal area by species"
    }
  }

  # Update y axis title based on user choice
  if (is.null(ylab)) {
    ylab <- if (density_ba == "density") {
      "Stem density"
    } else {
      "Basal area"
    }
  }

  # Keep species order consistent across all bars
  species_levels <- sort(unique(out[[species_col]]))

  # Pull measured rows
  measured_df <- out[out[[type_col]] == measured_label, , drop = FALSE]

  # Pull out measured year
  measured_year <- unique(measured_df[[year_col]])

  # Keep only that measured year
  measured_df <- measured_df[measured_df[[year_col]] == measured_year, , drop = FALSE]

  # Helper: make one full species vector for one stacked bar
  make_species_vector <- function(df_subset) {
    vals <- tapply(df_subset[[value_col]], df_subset[[species_col]], sum, na.rm = TRUE)
    out_vec <- setNames(rep(0, length(species_levels)), species_levels)
    out_vec[names(vals)] <- vals
    out_vec
  }

  # Start with measured bar
  bar_list <- list(make_species_vector(measured_df))
  bar_names <- as.character(measured_year)

  # Add reconstructed bars
  for (yr in years) {
    for (p in percentiles) {
      sub <- out[
        out[[type_col]] == reconstructed_label &
          out[[year_col]] == yr &
          out[[percentile_col]] == p,
        ,
        drop = FALSE
      ]

      stopifnot(nrow(sub) > 0)

      bar_list[[length(bar_list) + 1]] <- make_species_vector(sub)
      bar_names <- c(bar_names, paste0(yr, " (", p * 100, "%)"))
    }
  }

  # Convert list of species vectors into matrix
  bar_mat <- do.call(cbind, bar_list)
  rownames(bar_mat) <- species_levels
  colnames(bar_mat) <- bar_names

  # Handle colors
  if (is.null(col)) {
    # Create default rainbow colors
    col <- grDevices::rainbow(length(species_levels))
    names(col) <- species_levels
  } else {
    # Ensure user supplied named colors for all species
    stopifnot(all(species_levels %in% names(col)))
  }

  # Plot stacked bars
  bp <- barplot(
    bar_mat,
    beside = FALSE,
    col = col[species_levels],
    main = main,
    ylab = ylab,
    xlab = xlab,
    names.arg = bar_names,
    ...
  )

  # Add legend
  legend(
    legend_pos,
    legend = species_levels,
    fill = col[species_levels],
    bty = "n",
    cex = 0.9
  )

  # Return useful objects invisibly
  invisible(list(
    bar_matrix = bar_mat,
    bar_positions = bp,
    measured_year = measured_year,
    plotted_value = density_ba
  ))
}


data(standrecon_example_data)


# Reconstruct stand conditions
out <- standrecon(
  data = standrecon_example_data,
  meas_year = 2025,
  ref_year = c(1940, 1910),
  avg_inc_vec = c(PIEN = 1.5, ABBI = 1.3, PIPO = 1.4),
  plot_size = 1000
)


plot_standrecon(
  out = out,
  years = c(1940, 1910),
  percentiles = c(0.25, 0.5, 0.75),
  density_ba = "density",
  col = c(
    "PIEN" = "forestgreen",
    "ABBI" = "#d95f02",
    "PIPO" = "#7570b3"
  )
)

plot_standrecon(
  out = out,
  years = c(1940, 1910),
  percentiles = c(0.25, 0.5, 0.75),
  density_ba = "basal_area",
  col = c(
    "PIEN" = "forestgreen",
    "ABBI" = "#d95f02",
    "PIPO" = "#7570b3"
  )
)


plot_standrecon(
  out = out,
  years = 1940,
  percentiles = 0.5,
  density_ba = "basal_area"
)









