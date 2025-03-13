test_that("format_tablo_vers_liste_v fonctionne correctement", {
  library(data.table)
  library(testthat)

  # Création d'un exemple de tableau
  dt <- data.table(
    FINESS = rep(1:3, each = 3),
    ANNEE = rep(2019:2021, times = 3),
    Var1 = rnorm(9),
    Var2 = runif(9)
  )

  # Application de la fonction
  liste_var <- format_tablo_vers_liste_v(dt, variable_ident = "FINESS", variable_temporelle = "ANNEE")

  # Vérifications
  expect_type(liste_var, "list")  # Vérifie que le résultat est une liste
  expect_named(liste_var, c("Var1", "Var2"))  # Vérifie que les noms sont bien définis

  # Vérifie que chaque élément de la liste est un data.frame
  expect_true(all(sapply(liste_var, is.data.frame)))

  # Vérifie la structure du tableau pivoté
  expect_true(all(c("FINESS", "2019", "2020", "2021") %in% names(liste_var$Var1)))

  # Test avec `format_sortie = "data.table"`
  liste_dt <- format_tablo_vers_liste_v(dt, variable_ident = "FINESS", variable_temporelle = "ANNEE", format_sortie = "data.table")
  expect_true(all(sapply(liste_dt, is.data.table)))
})
