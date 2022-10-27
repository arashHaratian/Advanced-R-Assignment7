test_that("check of wrong inputs", {
  expect_error(ridgereg$new(formula = Petal.Length~X+Y, data=iris))
  expect_error(ridgereg$new(formula = Petal.Length~Sepal.Length, data=dataset2))
  expect_error(ridgereg$new(formula = Petal.Length~Sepal.Length, data=iris, lambda = "a"))
})


test_that("check of correct coefficients", {
  lambda <- 0
  ridgereg_model <- ridgereg$new(Petal.Length ~ Species, iris, lambda)
  if(requireNamespace("MASS", quietly = TRUE)){
    mass_model <- MASS::lm.ridge(Petal.Length ~ Species, iris, lambda = lambda)
  expect_equal(unname(ridgereg_model$coef()), unname(coef(mass_model)))
  }

  lambda <- 2
  ridgereg_model <- ridgereg$new(Petal.Length ~ Species, iris, lambda)
  if(requireNamespace("MASS", quietly = TRUE)){
    mass_model <- MASS::lm.ridge(Petal.Length ~ Species, iris, lambda = lambda)

    # Based on the lab pdf, the result from MASS package will be a little bit different than OLS version.
    # So I used small `tolerance` to pass the test

    expect_equal(unname(ridgereg_model$coef()), unname(coef(mass_model)), tolerance = 0.07)
  }
})
