test_that("format_tablo_vers_liste_a fonctionne correctement", {
  library(data.table)
  library(testthat)

  # Création d'un exemple de tableau
  dt <- data.table(
    FINESS = rep(1:3, each = 3),
    ANNEE = rep(2019:2021, times = 3),
    Var1 = rnorm(9),
    Var2 = runif(9)
  )

  # Application de la fonction avec format par défaut (data.frame)
  liste_annees <- format_tablo_vers_liste_a(dt)

  # Vérifications générales
  expect_type(liste_annees, "list")  # Vérifie que le retour est une liste
  expect_named(liste_annees, c("2019", "2020", "2021"))  # Vérifie que les noms correspondent aux années

  # Vérifie que chaque élément de la liste est un data.frame par défaut
  expect_true(all(sapply(liste_annees, is.data.frame)))

  # Vérifie que chaque data.frame contient bien la colonne "FINESS" et les variables attendues
  for (annee in names(liste_annees)) {
    expect_true("FINESS" %in% names(liste_annees[[annee]]))
    expect_true(all(c("Var1", "Var2") %in% names(liste_annees[[annee]])))
  }

  # Vérifie que la colonne ANNEE a bien été supprimée
  for (annee in names(liste_annees)) {
    expect_false("ANNEE" %in% names(liste_annees[[annee]]))
  }

  # Vérifie le bon fonctionnement avec format_sortie = "data.table"
  liste_dt <- format_tablo_vers_liste_a(dt, format_sortie = "data.table")

  expect_type(liste_dt, "list")  # Toujours une liste
  expect_named(liste_dt, c("2019", "2020", "2021"))  # Noms cohérents avec les années
  expect_true(all(sapply(liste_dt, is.data.table)))  # Vérifie que la sortie est bien en data.table

  # Vérifie que chaque data.table contient bien les bonnes colonnes
  for (annee in names(liste_dt)) {
    expect_true("FINESS" %in% names(liste_dt[[annee]]))
    expect_true(all(c("Var1", "Var2") %in% names(liste_dt[[annee]])))
    expect_false("ANNEE" %in% names(liste_dt[[annee]]))  # Vérifie que la colonne année a bien été supprimée
  }
})
