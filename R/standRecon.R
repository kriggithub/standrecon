#' Reconstruct stand conditions from a given reference year
#'
#' @param data Data with species, age, DBH, tree status, and tree decay columns.
#' @param meas_year Year that data was measured.
#' @param ref_year Reference years to reconstruct stand conditions.
#' @param avg_inc_vec Vector of average increment growth per species with units mm/year. (PIEN = 0.5, ABBI = 0.3)
#' @param plot_size Size of plots in m².
#' @param n_plots Number of plots with plot_size.
#' @param bark_eq_list List of bark correction equations. Default bark corrections for species codes used otherwise. list(PIPO = function(x) x*1.1029 + 0.7162)
#' @param species_col Species column name in data as a string.
#' @param age_col Species column name in data as a string.
#' @param dbh_col DBH column name in data as a string.
#' @param status_col Species status column name in data as a string.
#' @param decay_col Species decay column name in data as a string.
#' @param percentiles Percentiles to be calculated for sensitivity analysis.
#' @param min_dbh Minimum DBH threshold for stand calculations in cm.
#'
#' @returns A data frame with one row per species, reference year, and percentile. Columns include output type (reconstructed or measured),
#' reference year, percentile, species code, basal area per hectare, and stem density per hectare.
#' @export
#'
#' @examples
#' data(standrecon_example_data)
#'
#' out <- standrecon(
#'   data = standrecon_example_data,
#'   meas_year = 2025,
#'   ref_year = c(1950, 1975),
#'   avg_inc_vec = c(PIEN = 0.5, ABBI = 0.3, PIPO = 0.4),
#'   plot_size = 1000)
#'
#' out
standrecon <- function(data,
                       meas_year,
                       ref_year,
                       avg_inc_vec,
                       plot_size,
                       n_plots = 1,
                       bark_eq_list = list(),
                       species_col = "Species",
                       age_col = "Age",
                       dbh_col = "DBH",
                       status_col = "Status",
                       decay_col = "Decay",
                       percentiles = c(0.25, 0.5, 0.75),
                       min_dbh = 5){

  # Step 0: Pre-function helpers/definitions

  # (a) Pull out column names as strings
  Species <- species_col
  Age     <- age_col
  DBH     <- dbh_col
  Status  <- status_col
  Decay   <- decay_col

  # (b) Create column names for each percentile
  percent_names <- paste0("p", percentiles*100)

  # (c) Create lookup table for default bark equations
  default_bark_eq_list <- list(
    PIEN = function(x) x*1.0508 + 0.2824,
    ABBI = function(x) x*1.0508 + 0.2824,
    ABCO = function(x) x*1.1238 + 0.1952,
    ABMA = function(x) ((x*1.1799)^0.9803) + 0.4403,
    CADE = function(x) x*1.1975 - 0.0427,
    PILA = function(x) x*1.1252 + 0.2488,
    PIST = function(x) x*1.1252 + 0.2488,
    PIED = function(x) x*1.1252 + 0.2488,
    PIPO = function(x) x*1.1029 + 0.7162,
    PIJE = function(x) x*1.1029 + 0.7162,
    PSME = function(x) x*1.1759 - 0.2721
  )

  # (d) Update bark equation list based on user inputs
  bark_eq_list <- utils::modifyList(default_bark_eq_list, bark_eq_list)

  # (e) Setup final data frame
  final_output <- data.frame(
    type = character(),
    year = integer(),
    percentile = numeric(),
    species = character(),
    basal_area = numeric(),
    stem_density = numeric()
  )









  # Step 1: Determine size of live trees at reference date

  # (a) Run linear regression to find missing ages of live trees (helper function)

  all_tree_data <- .predict_live_tree_age(data = data,
                             species_col = species_col,
                             age_col = age_col,
                             dbh_col = dbh_col,
                             status_col = status_col,
                             meas_year = meas_year)

  # (b) Filter trees that are alive at reference date

  for (rY in ref_year) {

    # Subset data frame for only live tree calculations
    live_tree_data <- all_tree_data[all_tree_data[[Status]] == 1, ]

    # Filter trees alive at reference and make sure they have required columns
    live_tree_data <- live_tree_data[live_tree_data$estab_year <= rY & stats::complete.cases(live_tree_data[, c(DBH, Status, Species)]), ]


    # (c) Subtract annual growth rate per species to find DBH
    # at reference date for all live trees

    # Find growth to subtract (from diameter in mm/year)
    growth_to_sub <- (avg_inc_vec*2)/10
    # Create vector of matched growth to subtract by species to data
    live_gts <- growth_to_sub[live_tree_data[[Species]]]
    # Calculate DBH at reference date
    live_tree_data$RefDBH <- live_tree_data[[DBH]] - ((meas_year - rY) * live_gts)


    # (d) Remove trees below minimum DBH threshold

    # Filter live trees to those above minimum DBH threshold (cm)
    live_tree_data <- live_tree_data[live_tree_data$RefDBH >= min_dbh, ]











    # Step 2: Classification of dead trees

    # (a) Assign conclasses to dead trees (helper function)

    # Apply conclasses to all_tree_data
    all_tree_data$Conclass <- .create_conclass(all_tree_data[[Status]], all_tree_data[[Decay]])
    # Subset all_tree_data for only dead trees with conclasses
    dead_tree_data <- all_tree_data[!is.na(all_tree_data$Conclass), ]











    # Step 3: Determine decomposition rates and year of death for dead trees

    # (a) Find decomposition rates between classes

    # Calculate average DBH for each species for all live pre-settlement trees (cm)
    avg_dbh_cm <- tapply(
      live_tree_data[[DBH]],
      live_tree_data[[Species]],
      mean,
      na.rm = T
    )
    # Convert to inches
    avg_dbh_in <- avg_dbh_cm/2.54
    # Calculate snag life
    snag_life <- 2 * avg_dbh_in
    # Calculate snag fall
    snagFall <- 1 / snag_life

    # Create reference table
    decomp_reference <- expand.grid(Species = names(avg_dbh_cm),
                                   Conclass = 3:8)
    decomp_reference$Rate <- NA_real_
    # Fill decomposition rates by conclass
    decomp_reference$Rate[decomp_reference$Conclass == 3] <- 0
    decomp_reference$Rate[decomp_reference$Conclass == 4] <- 0.2
    decomp_reference$Rate[decomp_reference$Conclass == 5] <- 0.15
    decomp_reference$Rate[decomp_reference$Conclass == 6] <- snagFall
    decomp_reference$Rate[decomp_reference$Conclass == 7] <- snagFall
    decomp_reference$Rate[decomp_reference$Conclass == 8] <- snagFall
    # Create empty columns for each percentile
    for (n in percent_names){
      decomp_reference[[as.character(n)]] <- NA
    }





    # (b) Apply Fule's equation to find year of death for trees at each percentile for sensitivity analysis (helper function)

    # Loop through species
    for (sp in unique(decomp_reference$Species)) {
      # Create logical species row vector
      sp_rows <- decomp_reference$Species == sp
      # Subset rows for that specific species
      decomp_sub_df <- decomp_reference[sp_rows, ]
      # Order subsetted data by conclass
      decomp_sub_df <- decomp_sub_df[order(decomp_sub_df$Conclass), ]

      # Loop for each percentile
      for (i in seq_along(percentiles)){
        # Index each percentile
        p <- percentiles[i]
        # Index percentile columns
        p_col <- percent_names[i]


        # Create placeholder vector for years dead
        years_dead <- rep(NA_real_, nrow(decomp_sub_df))
        # Conclass 3 always 0 years dead
        years_dead[decomp_sub_df$Conclass == 3] <- 0
        # Select conclasses 4 and up
        conclass_4_up <- which(decomp_sub_df$Conclass >= 4)
        # Apply Fule's equation and cumulatively sum them per each conclass
        if (length(conclass_4_up) > 0){
          years_dead[conclass_4_up] <- cumsum(.fule_decomp(decomp_sub_df$Rate[conclass_4_up], p))
        }

        # Write back into decomp_reference for each percentile
        decomp_reference[sp_rows, p_col] <- years_dead

      }

    }


    # Merge decomposition reference table with dead tree data

    dead_tree_data <- merge(dead_tree_data,
                          decomp_reference,
                          by.x = c(Species, "Conclass"),
                          by.y = c("Species", "Conclass"),
                          all.x = T)










    # Step 4: Grow dead tree DBH back to reference date
    # Loop over all percentiles
    for (n in percent_names){
      # Create column names
      death_col <- paste0(n, "DeathYear")
      p_ref_dbh <- paste0(n, "refDBH")

      # (a) Determine age of dead trees at reference date
      # Find year of death for dead trees by adding measured year and years dead
      dead_tree_data[[death_col]] <- ceiling(meas_year + dead_tree_data[[n]])


      # (b) Subtract annual growth rate per species to find DBH at reference date for all dead trees
      dead_gts <- growth_to_sub[dead_tree_data[[Species]]]
      # Create column for DBH at reference date
      dead_tree_data[[p_ref_dbh]] <- ifelse(dead_tree_data[[death_col]] > rY,
                                        dead_tree_data[[DBH]] - (dead_tree_data[[death_col]] - rY) * dead_gts,
                                        dead_tree_data[[DBH]])


      # (c) Remove trees below minimum DBH threshold
      dead_tree_data <- dead_tree_data[dead_tree_data[[p_ref_dbh]] > min_dbh, ]






      # Step 5: Correct DBH for dead trees with missing bark

      # (a) Correct bark for conclasses that are not 3 or 4 and update DBH
      # No bark correction for conclass 3 or 4
      dead_tree_data$RefDBH[dead_tree_data$Conclass %in% 3:4] <- dead_tree_data[[p_ref_dbh]][dead_tree_data$Conclass %in% 3:4]
      # Bark correction for conclasses 5-7 using look up table
      # If no equation in table, uses uncorrected DBH
      conclass_5_7 <- dead_tree_data$Conclass %in% 5:7
      # Loop per species in these rows
      for (sp in unique(dead_tree_data[[Species]][conclass_5_7])){
        # Subset by conclass and species
        sp_conclass_5_7 <- conclass_5_7 & dead_tree_data[[Species]] == sp

        # Apply bark corrections if species name is in look up table
        if (sp %in% names(bark_eq_list)) {
          # Apply bark correction if in table
          dead_tree_data$RefDBH[sp_conclass_5_7] <- bark_eq_list[[sp]](dead_tree_data[[p_ref_dbh]][sp_conclass_5_7])
        } else {
          # No changes to DBH otherwise
          dead_tree_data$RefDBH[sp_conclass_5_7] <- dead_tree_data[[p_ref_dbh]][sp_conclass_5_7]
        }
      }





      # Step 6: Calculate stand density and basal area at reference dates and store

      # (a) Add reconstructed live and dead tree DBH to one table
      final_tree_data <- rbind(
        live_tree_data[, c(Species, "RefDBH")],
        dead_tree_data[, c(Species, "RefDBH")]
      )
      # Remove rows that are NA
      final_tree_data <- final_tree_data[rowSums(is.na(final_tree_data)) != ncol(final_tree_data), ]


      # (b) Calculate basal area for reference date
      # Basal area per tree
      final_tree_data$BA <- (0.00007854 * (final_tree_data$RefDBH^2)) * (10000/(plot_size*n_plots))
      # Sum basal area per species as a named vector
      recon_ba_sum <- tapply(final_tree_data$BA,
                       final_tree_data[[Species]],
                       sum,
                       na.rm = TRUE)

      # (c) Calculate stand density for reference date
      # Extract number of trees per species to initiate density table
      recon_density <- table(final_tree_data[[Species]])
      # Multiply species counts by stems per hectare
      recon_density <- as.numeric(recon_density) * (10000/(plot_size*n_plots))
      # Reassign species names
      names(recon_density) <- names(table(final_tree_data[[Species]]))

      # Pull out names for both basal area and tree density
      recon_species <- union(names(recon_ba_sum), names(recon_density))



      # (d) Return stand density and basal area by species per percentile and reference year
      recon_rows <- data.frame(
        type = "reconstructed",
        year = rY,
        percentile = as.numeric(sub("^p", "", n)) / 100,
        species = recon_species,
        basal_area = as.numeric(recon_ba_sum[recon_species]),
        stem_density = as.numeric(recon_density[recon_species])
      )


      # write to final output
      final_output <- rbind(final_output, recon_rows)

    }

  }


  # Step 7: Calculate stand density and basal area at measured date and store

  # (a) Subset live trees from measured data
  meas_tree_data <- all_tree_data[all_tree_data[[Status]] == 1 &
                                !is.na(all_tree_data[[DBH]]),
                              c(Species, DBH)]

  # (b) Calculate basal area
  # Basal area per tree
  meas_tree_data$BA <- (0.00007854 * (meas_tree_data[[DBH]]^2)) * (10000/(plot_size*n_plots))
  # Sum basal area per species
  meas_ba_sum <- tapply(meas_tree_data$BA,
                   meas_tree_data[[Species]],
                   sum,
                   na.rm = TRUE)

  # (c) Calculate stand density for reference date
  # Extract number of trees per species to initiate density table
  meas_density <- table(meas_tree_data[[Species]])
  # Multiply species counts by stems per hectare
  meas_density <- as.numeric(meas_density) * (10000/(plot_size*n_plots))
  # Reassign species names
  names(meas_density) <- names(table(meas_tree_data[[Species]]))

  # Pull out names for both basal area and tree density
  meas_species <- union(names(meas_ba_sum), names(meas_density))


  meas_rows <- data.frame(
    type = "measured",
    year = meas_year,
    percentile = NA_real_,
    species = meas_species,
    basal_area = as.numeric(meas_ba_sum[meas_species]),
    stem_density = as.numeric(meas_density[meas_species])
  )

  final_output <- rbind(final_output, meas_rows)


  # Step 8: Return data
  return(final_output)

}
