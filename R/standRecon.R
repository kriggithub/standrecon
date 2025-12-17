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
#' @returns
#' @export
#'
#' @examples
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

  # Step 0: Prefunction helpers/definitions

  # (a) Pull out column names as strings
  Species <- species_col
  Age     <- age_col
  DBH     <- dbh_col
  Status  <- status_col
  Decay   <- decay_col

  # (b) create column names for each percent
  percentnames <- paste0("p", percentiles*100)

  # (c) create lookup table for default bark equations
  defaultBarkEq <- list(
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

  # (d) update bark equation list based on user inputs
  bark_eq_list <- utils::modifyList(defaultBarkEq, bark_eq_list)

  # (e) setup final dataframe
  finalOutput <- list()









  # Step 1: Determine size of live trees at reference date

  # (a) Run linear regression to find missing ages of live trees (func)

  allTreeData <- .predict_live_tree_age(data = data,
                             species_col = species_col,
                             age_col = age_col,
                             dbh_col = dbh_col,
                             status_col = status_col,
                             meas_year = meas_year)

  # (b) Filter trees that are alive at reference date

  for (rY in ref_year) {

    # subset dataframe for only live tree calculations
    liveTreeData <- allTreeData[allTreeData[[Status]] == 1, ]

    # filter trees alive at reference and make sure they have required columns
    liveTreeData <- liveTreeData[liveTreeData$estab_year <= rY & stats::complete.cases(liveTreeData[, c(DBH, Status, Species)]), ]


    # (c) Subtract annual growth rate per species to find DBH
    # at reference date for all live trees

    # find growth to subtract (from diameter in mm/year)
    growthToSub <- (avg_inc_vec*2)/10
    # create vector of matched growth to subtract by species to data
    spGrowthToSubLive <- growthToSub[liveTreeData[[Species]]]
    # calculate DBH at reference date
    liveTreeData$RefDBH <- liveTreeData[[DBH]] - ((meas_year - rY) * spGrowthToSubLive)


    # (d) Remove trees below minimum DBH threshold

    # filter live trees to those above minimum DBH threshold (cm)
    liveTreeData <- liveTreeData[liveTreeData$RefDBH >= min_dbh, ]











    # Step 2: Classification of dead trees

    # (a) Assign conclasses to dead trees (func)

    # apply conclasses to allTreeData
    allTreeData$Conclass <- .create_conclass(allTreeData[[Status]], allTreeData[[Decay]])
    # subset allTreeData for only dead trees with conclasses
    deadTreeData <- allTreeData[!is.na(allTreeData$Conclass), ]











    # Step 3: Determine decomposition rates and year of death for dead trees

    # (a) Find decomposition rates between classes

    # calculate average DBH for each species for all live presettlement trees (cm)
    avgDBHcm <- tapply(
      liveTreeData[[DBH]],
      liveTreeData[[Species]],
      mean,
      na.rm = T
    )
    # convert to inches
    avgDBHin <- avgDBHcm/2.54
    # calculate snag life
    snagLife <- 2 * avgDBHin
    # calculate snag fall
    snagFall <- 1 / snagLife

    # create reference table
    decompreference <- expand.grid(Species = names(avgDBHcm),
                                   Conclass = 3:8)
    decompreference$Rate <- NA_real_
    # fill decomposition rates by conclass
    decompreference$Rate[decompreference$Conclass == 3] <- 0
    decompreference$Rate[decompreference$Conclass == 4] <- 0.2
    decompreference$Rate[decompreference$Conclass == 5] <- 0.15
    decompreference$Rate[decompreference$Conclass == 6] <- snagFall
    decompreference$Rate[decompreference$Conclass == 7] <- snagFall
    decompreference$Rate[decompreference$Conclass == 8] <- snagFall
    # create empty columns for each percentile
    for (n in percentnames){
      decompreference[[as.character(n)]] <- NA
    }





    # (b) Apply Fule's equation to find year of death for
    # trees at each percentile for sensitvity analysis (func)

    # loop through species
    for (sp in unique(decompreference$Species)) {
      # create logical species row vector
      spRows <- decompreference$Species == sp
      # subset rows for that specific species
      decompSubDF <- decompreference[spRows, ]
      # order subsetted data by conclass
      decompSubDF <- decompSubDF[order(decompSubDF$Conclass), ]

      # loop for each percentile
      for (i in seq_along(percentiles)){
        # index each percentile
        p <- percentiles[i]
        # index percentile columns
        pcol <- percentnames[i]


        # create placeholder vector for years dead
        yearsDead <- rep(NA_real_, nrow(decompSubDF))
        # conclass 3 always 0 years dead
        yearsDead[decompSubDF$Conclass == 3] <- 0
        # select conclasses 4 and up
        conclass4up <- which(decompSubDF$Conclass >= 4)
        # apply Fule's equation and cumulatively sum them per each conclass
        if (length(conclass4up) > 0){
          yearsDead[conclass4up] <- cumsum(.fule_decomp(decompSubDF$Rate[conclass4up], p))
        }

        # write back into decompreference for each percentile
        decompreference[spRows, pcol] <- yearsDead

      }

    }


    # merge decomposition reference table with dead tree data

    deadTreeData <- merge(deadTreeData,
                          decompreference,
                          by.x = c(Species, "Conclass"),
                          by.y = c("Species", "Conclass"),
                          all.x = T)










    # Step 4: Grow dead tree DBH back to reference date
    # loop over all percentiles
    for (n in percentnames){
      # create column names
      deathCol <- paste0(n, "DeathYear")
      pRefDBH <- paste0(n, "refDBH")

      # (a) Determine age of dead trees at reference date
      # find year of death for dead trees by adding measured year and years dead
      deadTreeData[[deathCol]] <- ceiling(meas_year + deadTreeData[[n]])


      # (b) Subtract annual growth rate per species to find DBH
      # at reference date for all dead trees
      spGrowthToSubDead <- growthToSub[deadTreeData[[Species]]]
      # create column for DBH at reference date
      deadTreeData[[pRefDBH]] <- ifelse(deadTreeData[[deathCol]] > rY,
                                        deadTreeData[[DBH]] - (deadTreeData[[deathCol]] - rY) * spGrowthToSubDead,
                                        deadTreeData[[DBH]])


      # (c) Remove trees below minimum DBH threshold
      deadTreeData <- deadTreeData[deadTreeData[[pRefDBH]] > min_dbh, ]






      # Step 5: Correct DBH for dead trees with missing bark

      # (a) Correct bark for conclasses that are not 3 or 4 and update DBH
      # no bark correction for conclass 3 or 4
      deadTreeData$RefDBH[deadTreeData$Conclass %in% 3:4] <- deadTreeData[[pRefDBH]][deadTreeData$Conclass %in% 3:4]
      # bark correction for conclasses 5-7 using lookup table
      # if no equation in table, uses uncorrected DBH
      conclass57 <- deadTreeData$Conclass %in% 5:7
      # loop per species in these rows
      for (sp in unique(deadTreeData[[Species]][conclass57])){
        # subset by conclass and species
        spConclass57 <- conclass57 & deadTreeData[[Species]] == sp

        # apply bark corrections if species name is in lookup table
        if (sp %in% names(bark_eq_list)) {
          # apply bark correction if in table
          deadTreeData$RefDBH[spConclass57] <- bark_eq_list[[sp]](deadTreeData[[pRefDBH]][spConclass57])
        } else {
          # no changes to DBH otherwise
          deadTreeData$RefDBH[spConclass57] <- deadTreeData[[pRefDBH]][spConclass57]
        }
      }





      # Step 6: Calculate stand density and basal area at reference dates and store

      # (a) Add reconstructed live and dead tree DBH to one table

      finalTreeData <- rbind(
        liveTreeData[, c(Species, "RefDBH")],
        deadTreeData[, c(Species, "RefDBH")]
      )
      # remove rows that are NA
      finalTreeData <- finalTreeData[rowSums(is.na(finalTreeData)) != ncol(finalTreeData), ]


      # (b) Calculate basal area for reference date
      # basal area per tree
      finalTreeData$BA <- (0.00007854 * (finalTreeData$RefDBH^2)) * (10000/(plot_size*n_plots))
      # sum basal area per species
      BAsum <- rowsum(finalTreeData$BA, finalTreeData[[Species]], na.rm = T)
      # create output list of BA
      BAlist <- stats::setNames(as.list(BAsum[,1]), paste0(rownames(BAsum), ".ba"))

      # (c) Calculate stand density for reference date
      # extract number of trees per species
      spCounts <- table(finalTreeData[[Species]])
      # create output list of tree density
      DensList <- stats::setNames(as.list(as.numeric(spCounts) * (10000/(plot_size*n_plots))),
                           paste0(names(spCounts), ".density"))


      # (d) Return stand density and basal area by species per percentile and reference year
      # and append to final data
      rowOut <- data.frame(
        ref_year = rY,
        BAlist,
        DensList,
        check.names = FALSE
      )

      # write to final output
      if (is.null(finalOutput[[n]])) {
        finalOutput[[n]] <- rowOut
      } else {
        finalOutput[[n]] <- rbind(finalOutput[[n]], rowOut)
      }

    }

  }


  # Step 7: Calculate stand density and basal area at measured date and store

  # (a) Subset live trees from measured data
  measTreeData <- allTreeData[allTreeData[[Status]] == 1 &
                                !is.na(allTreeData[[DBH]]),
                              c(Species, DBH)]

  # (b) Calculate basal area
  # basal area per tree
  measTreeData$BA <- (0.00007854 * (measTreeData[[DBH]]^2)) * (10000/(plot_size*n_plots))
  # sum basal area per species
  measBAsum <- rowsum(measTreeData$BA, measTreeData[[Species]], na.rm = T)
  # create output list of BA
  measBAlist <- stats::setNames(as.list(measBAsum[,1]), paste0(rownames(measBAsum), ".ba"))

  # (c) Calculate stand density for reference date
  # extract number of trees per species
  measSpCounts <- table(measTreeData[[Species]])
  # create output list of tree density
  measDensList <- stats::setNames(as.list(as.numeric(measSpCounts) * (10000/(plot_size*n_plots))),
                           paste0(names(measSpCounts), ".density"))


  # (d) Return stand density and basal area by species per percentile and reference year
  # and append to final data
  measRowOut <- data.frame(
    ref_year = meas_year,
    measBAlist,
    measDensList,
    check.names = FALSE
  )

  # write to final output
  if (is.null(finalOutput[["measured"]])) {
    finalOutput[["measured"]] <- measRowOut
  } else {
    finalOutput[["measured"]] <- rbind(finalOutput[["measured"]], measRowOut)
  }


  # Step 8: Return data
  return(list(
    finalOutput = finalOutput,
    finalTreeData = finalTreeData,
    allTreeData = allTreeData,
    liveTreeData = liveTreeData,
    deadTreeData = deadTreeData
  ))

}
