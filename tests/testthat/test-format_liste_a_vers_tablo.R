test_that("format_liste_a_vers_tablo fonctionne correctement", {
  library(data.table)
  library(testthat)

  # Création d'une liste d'exemple (représentant des années)
  liste_annees <- list(
    "2019" = data.table(FINESS = c(1, 2, 3), Var1 = c(10, 20, 30), Var2 = c(0.1, 0.2, 0.3)),
    "2020" = data.table(FINESS = c(1, 2, 3), Var1 = c(15, 25, 35), Var2 = c(0.15, 0.25, 0.35)),
    "2021" = data.table(FINESS = c(1, 2, 3), Var1 = c(12, 22, 32), Var2 = c(0.12, 0.22, 0.32))
  )

  # Test de conversion avec format par défaut (data.frame)
  tablo_df <- format_liste_a_vers_tablo(liste_annees, var_liste = "ANNEE")

  # Vérifications générales
  expect_s3_class(tablo_df, "data.frame")  # Vérifie que la sortie est un data.frame
  expect_true("ANNEE" %in% names(tablo_df))  # Vérifie que la colonne ANNEE est bien présente
  expect_true("FINESS" %in% names(tablo_df))  # Vérifie que la colonne FINESS est bien présente
  expect_true(all(c("Var1", "Var2") %in% names(tablo_df)))  # Vérifie la présence des variables

  # Vérifie que les valeurs sont bien empilées
  expect_equal(nrow(tablo_df), length(liste_annees) * nrow(liste_annees[[1]]))  # 9 lignes attendues

  # Vérifie que la colonne ANNEE contient bien les valeurs attendues
  expect_equal(sort(unique(tablo_df$ANNEE)), c("2019", "2020", "2021"))

  # Test avec `format_sortie = "data.table"`
  tablo_dt <- format_liste_a_vers_tablo(liste_annees, format_sortie = "data.table")

  expect_s3_class(tablo_dt, "data.table")  # Vérifie que la sortie est un data.table
  expect_true(all(c("ANNEE", "FINESS", "Var1", "Var2") %in% names(tablo_dt)))  # Vérifie les colonnes
  expect_equal(nrow(tablo_dt), length(liste_annees) * nrow(liste_annees[[1]]))  # Toujours 9 lignes attendues
})
