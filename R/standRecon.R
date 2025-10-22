standRecon <- function(data = data,
                       measYear,
                       refYear,
                       avgIncVec,
                       plotSize,
                       barkEqList = list(),
                       speciesCol = Species,
                       ageCol = Age,
                       dbhCol = DBH,
                       statusCol = Status,
                       decayCol = Decay,
                       percentiles = c(0.25, 0.5, 0.75),
                       minDbh = 5){


  # Step 1: Determine size of live trees at reference date

  # (a) Run linear regression to find missing ages of live trees (func)

  liveTreeAges <- liveTreeAge(data = data,
                        speciesCol = speciesCol,
                        ageCol = ageCol,
                        dbhCol = dbhCol,
                        statusCol = statusCol,
                        measYear = measYear)

  # (b) Filter trees that are alive at reference date

  for (rY in refYear) {

    # filter trees alive at reference and make sure they have required columns
    liveTreeAges <- liveTreeAges[liveTreeAges$estabYear <= rY & complete.cases(liveTreeAges[, c(DBH, Status, Species)]), ]


  # (c) Subtract annual growth rate per species to find DBH
  # at reference date for all live trees

    # find growth to subtract (from diameter in mm/year)
    growthToSub <- (avgIncVec*2)/10
    # create vector of matched growth to subtract by species to data
    spGrowthToSub <- growthToSub[liveTreeAges[[Species]]]
    # calculate DBH at reference date
    liveTreeAges$RefDBH <- dat[[DBH]] - ((measYear - rY) * spGrowthToSub)


  # (d) Remove trees below minimum DBH threshold

    # filter live trees to those above minimum DBH threshold (cm)
    liveTreeAges <- liveTreeAges[liveTreeAges$RefDBH >= minDbh, ]











  # Step 2: Classification of dead trees

  # (a) Assign conclasses to dead trees (func)



  # Step 3: Determine decomposition rates and year of death for dead trees

  # (a) Find decomposition rates between classes

  # (b) Apply Fule's equation to find year of death for
  # trees at each percentile for sensitvity analysis (func)




  # Step 4: Grow dead tree DBH back to reference date

  # (a) Determine age of dead trees at reference date

  # (b) Subtract annual growth rate per species to find DBH
  # at reference date for all dead trees

  # (c) Remove trees below minimum DBH threshold



  # Step 5: Correct DBH for dead trees with missing bark

  # (a) Setup default bark equation list

  # (b) Correct bark for conclasses that are not 3 or 4 and update DBH




  # Step 6: Calculate stand density and basal area at reference date and return

  # (a) Add reconstructed live and dead tree DBH to one table

  # (b) Calculate stand density for reference date

  # (c) Calculate basal area for reference date

  # (d) Return stand density and basal area by species

  }

}
