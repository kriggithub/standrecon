## Code to prepare `standrecon_example_data`

# Set seed
set.seed(123)

# Set number of rows and species
n <- 200


# Set up example dataframe

standrecon_example_data <- data.frame(

  # Assign species
  Species = sample(
    c("PIEN", "ABBI", "PIPO"),
    n,
    replace = TRUE,
    prob = c(0.45, 0.35, 0.20)
  ),

  # Assign ages to ~30% of trees
  Age = ifelse(
    runif(n) < 0.7,
    NA_integer_,
    sample(40:200, n, replace = TRUE)
  ),

  # Assign tree status (~75% live trees)
  Status = sample(
    c(1:6),
    n,
    replace = TRUE,
    prob = c(0.75, 0.09, 0.06, 0.04, 0.04, 0.02)
  ),

  # Assign tree decay
  Decay = sample(
    c(1:8),
    n,
    replace = TRUE,
    prob = c(0.17, 0.16, 0.15, 0.12, 0.10, 0.10, 0.10, 0.10)
  )
)


# DBH column

# Logical vector for trees that have known ages
known_age <- !is.na(standrecon_example_data$Age)

# Trees without ages: Randomized DBH measurements between 5 and 45cm, rounded to 1 decimal
standrecon_example_data$DBH <- round(runif(n, 5, 45), 1)

# Trees with ages: Rough linear relationship, around 0.15 cm growth in DBH per year, with a normal distribution of mean = 0 and sd = 5
standrecon_example_data$DBH[known_age] <- round(
  5 + (0.15 * standrecon_example_data$Age[known_age] + rnorm(sum(known_age), mean = 0, sd = 5)),
  1
)


usethis::use_data(standrecon_example_data, overwrite = TRUE)
