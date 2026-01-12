library(testthat)
# context("Test sur la fonction calculer_mode")

test_that("calculer_mode retourne la modalité la plus fréquente", {
  # Cas normal
  expect_equal(calculer_mode(c(1, 2, 2, 3, 4)), 2)

  expect_equal(calculer_mode(c("a", "a", "b", "c", "d")), "a")

  # Cas où il n'y a qu'une seule valeur
  expect_equal(calculer_mode(c(1, 1, 1, 1, 1)), 1)

  # Cas où il y a plusieurs modalités avec la même fréquence
  expect_equal(calculer_mode(c(1, 1, 2, 2, 3)), 1) # retourne la première modalité rencontrée

  # Cas où x est NULL
  expect_null(calculer_mode(NULL))
})
