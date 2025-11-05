#' Reconstruct stand conditions from a given reference year
#'
#' @param data Data with species, age, DBH, tree status, and tree decay columns.
#' @param measYear Year that data was measured.
#' @param refYear Reference years to reconstruct stand conditions.
#' @param avgIncVec Vector of average increment growth per species with units mm/year. (PIEN = 0.5, ABBI = 0.3)
#' @param plotSize Size of plots in m².
#' @param nPlots Number of plots with plotSize.
#' @param barkEqList List of bark correction equations. Default bark corrections for species codes used otherwise. list(PIPO = function(x) x*1.1029 + 0.7162)
#' @param speciesCol Species column name in data as a string.
#' @param ageCol Species column name in data as a string.
#' @param dbhCol DBH column name in data as a string.
#' @param statusCol Species status column name in data as a string.
#' @param decayCol Species decay column name in data as a string.
#' @param percentiles Percentiles to be calculated for sensitivity analysis.
#' @param minDbh Minimum DBH threshold for stand calculations in cm.
#'
#' @returns
#' @export
#'
#' @examples
standRecon <- function(data = data,
                       measYear,
                       refYear,
                       avgIncVec,
                       plotSize,
                       nPlots = 1,
                       barkEqList = list(),
                       speciesCol = "Species",
                       ageCol = "Age",
                       dbhCol = "DBH",
                       statusCol = "Status",
                       decayCol = "Decay",
                       percentiles = c(0.25, 0.5, 0.75),
                       minDbh = 5){

  # Step 0: Prefunction helpers/definitions

  # (a) Pull out column names as strings
  Species <- speciesCol
  Age     <- ageCol
  DBH     <- dbhCol
  Status  <- statusCol
  Decay   <- decayCol

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
  barkEqList <- utils::modifyList(defaultBarkEq, barkEqList)

  # (e) setup final dataframe
  finalOutput <- list()









  # Step 1: Determine size of live trees at reference date

  # (a) Run linear regression to find missing ages of live trees (func)

  allTreeData <- liveTreeAge(data = data,
                             speciesCol = speciesCol,
                             ageCol = ageCol,
                             dbhCol = dbhCol,
                             statusCol = statusCol,
                             measYear = measYear)

  # (b) Filter trees that are alive at reference date

  for (rY in refYear) {

    # subset dataframe for only live tree calculations
    liveTreeData <- allTreeData[allTreeData[[Status]] == 1, ]

    # filter trees alive at reference and make sure they have required columns
    liveTreeData <- liveTreeData[liveTreeData$estabYear <= rY & stats::complete.cases(liveTreeData[, c(DBH, Status, Species)]), ]


    # (c) Subtract annual growth rate per species to find DBH
    # at reference date for all live trees

    # find growth to subtract (from diameter in mm/year)
    growthToSub <- (avgIncVec*2)/10
    # create vector of matched growth to subtract by species to data
    spGrowthToSubLive <- growthToSub[liveTreeData[[Species]]]
    # calculate DBH at reference date
    liveTreeData$RefDBH <- liveTreeData[[DBH]] - ((measYear - rY) * spGrowthToSubLive)


    # (d) Remove trees below minimum DBH threshold

    # filter live trees to those above minimum DBH threshold (cm)
    liveTreeData <- liveTreeData[liveTreeData$RefDBH >= minDbh, ]











    # Step 2: Classification of dead trees

    # (a) Assign conclasses to dead trees (func)

    # apply conclasses to allTreeData
    allTreeData$Conclass <- createConclass(allTreeData[[Status]], allTreeData[[Decay]])
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
                                   Conclass = 3:7)
    decompreference$Rate <- NA_real_
    # fill decomposition rates by conclass
    decompreference$Rate[decompreference$Conclass == 3] <- 0
    decompreference$Rate[decompreference$Conclass == 4] <- 0.2
    decompreference$Rate[decompreference$Conclass == 5] <- 0.15
    decompreference$Rate[decompreference$Conclass == 6] <- snagFall
    decompreference$Rate[decompreference$Conclass == 7] <- snagFall
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
          yearsDead[conclass4up] <- cumsum(fuleDecomp(decompSubDF$Rate[conclass4up], p))
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
      deadTreeData[[deathCol]] <- ceiling(measYear + deadTreeData[[n]])


      # (b) Subtract annual growth rate per species to find DBH
      # at reference date for all dead trees
      spGrowthToSubDead <- growthToSub[deadTreeData[[Species]]]
      # create column for DBH at reference date
      deadTreeData[[pRefDBH]] <- ifelse(deadTreeData[[deathCol]] > rY,
                                        deadTreeData[[DBH]] - (deadTreeData[[deathCol]] - rY) * spGrowthToSubDead,
                                        deadTreeData[[DBH]])


      # (c) Remove trees below minimum DBH threshold
      deadTreeData <- deadTreeData[deadTreeData[[pRefDBH]] > minDbh, ]






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
        if (sp %in% names(barkEqList)) {
          # apply bark correction if in table
          deadTreeData$RefDBH[spConclass57] <- barkEqList[[sp]](deadTreeData[[pRefDBH]][spConclass57])
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
      finalTreeData$BA <- (0.00007854 * (finalTreeData$RefDBH^2)) * (10000/(plotSize*nPlots))
      # sum basal area per species
      BAsum <- rowsum(finalTreeData$BA, finalTreeData[[Species]], na.rm = T)
      # create output list of BA
      BAlist <- stats::setNames(as.list(BAsum[,1]), paste0(rownames(BAsum), ".ba"))

      # (c) Calculate stand density for reference date
      # extract number of trees per species
      spCounts <- table(finalTreeData[[Species]])
      # create output list of tree density
      DensList <- stats::setNames(as.list(as.numeric(spCounts) * (10000/(plotSize*nPlots))),
                           paste0(names(spCounts), ".density"))


      # (d) Return stand density and basal area by species per percentile and reference year
      # and append to final data
      rowOut <- data.frame(
        refYear = rY,
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
  measTreeData$BA <- (0.00007854 * (measTreeData[[DBH]]^2)) * (10000/(plotSize*nPlots))
  # sum basal area per species
  measBAsum <- rowsum(measTreeData$BA, measTreeData[[Species]], na.rm = T)
  # create output list of BA
  measBAlist <- stats::setNames(as.list(measBAsum[,1]), paste0(rownames(measBAsum), ".ba"))

  # (c) Calculate stand density for reference date
  # extract number of trees per species
  measSpCounts <- table(measTreeData[[Species]])
  # create output list of tree density
  measDensList <- stats::setNames(as.list(as.numeric(measSpCounts) * (10000/(plotSize*nPlots))),
                           paste0(names(measSpCounts), ".density"))


  # (d) Return stand density and basal area by species per percentile and reference year
  # and append to final data
  measRowOut <- data.frame(
    refYear = measYear,
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
