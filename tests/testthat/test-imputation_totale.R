library(testthat)
context("Test sur la fonction imputation_totale")

test_that("imputation_totale fonctionne correctement", {

  # Création de données de test
  tablo <- data.frame(
    id = 1:6,
    value_to_impute = c(1, NA, 2, NA, 3, NA)
  )

  table_de_reference <- data.frame(
    id = 1:6,
    group1 = c("A", "A", "B", "B", "C", "C"),
    group2 = c("X", "Y", "X", "Y", "X", "Y"),
    ratio_var = c(10, 20, 30, 40, 50, 60)
  )

  # Test avec une seule variable de groupe
  result_single_group <- imputation_totale(tablo, "value_to_impute", "id", table_de_reference, "group1", "ratio_var")
  # Vous devez remplacer expected_single_group avec les valeurs réellement attendues
  expected_single_group <- c(1, NA, 2, NA, 3, NA)
  expect_equal(result_single_group, expected_single_group)

  # Test avec plusieurs variables de groupe
  result_multiple_group <- imputation_totale(tablo, "value_to_impute", "id", table_de_reference, c("group1", "group2"), "ratio_var")
  # Vous devez remplacer expected_multiple_group avec les valeurs réellement attendues
  expected_multiple_group <- c(1, NA, 2, NA, 3, NA)
  expect_equal(result_multiple_group, expected_multiple_group)

})
