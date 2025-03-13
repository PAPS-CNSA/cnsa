library(testthat)
library(data.table)
context("Test sur la fonction format liste_v_vers_tablo")

test_that("format_liste_v_vers_tablo renvoie une data.frame ou data.table selon le paramètre format_sortie", {
  liste_v <- list(
    Var1 = data.table(FINESS = c(1, 2), `2020` = c(10, 20), `2021` = c(15, 25)),
    Var2 = data.table(FINESS = c(1, 2), `2020` = c(100, 200), `2021` = c(150, 250))
  )

  result_df <- format_liste_v_vers_tablo(liste_v, format_sortie = "data.frame")
  result_dt <- format_liste_v_vers_tablo(liste_v, format_sortie = "data.table")

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_dt, "data.table")
})

test_that("format_liste_v_vers_tablo retourne un tableau avec les bonnes colonnes", {
  liste_v <- list(
    Var1 = data.table(FINESS = c(1, 2), `2020` = c(10, 20), `2021` = c(15, 25)),
    Var2 = data.table(FINESS = c(1, 2), `2020` = c(100, 200), `2021` = c(150, 250))
  )

  result <- format_liste_v_vers_tablo(liste_v)

  expect_true(all(c("FINESS", "ANNEE", "Var1", "Var2") %in% names(result)))
})

test_that("format_liste_v_vers_tablo garde toutes les valeurs correctement", {
  liste_v <- list(
    Var1 = data.table(FINESS = c(1, 2), `2020` = c(10, 20), `2021` = c(15, 25)),
    Var2 = data.table(FINESS = c(1, 2), `2020` = c(100, 200), `2021` = c(150, 250))
  )

  result <- format_liste_v_vers_tablo(liste_v)

  # Extraction correcte des valeurs sous forme de vecteurs numériques
  expect_equal(result[FINESS == 1 & ANNEE == "2020", Var1][[1]], 10)
  expect_equal(result[FINESS == 2 & ANNEE == "2021", Var2][[1]], 250)
})

test_that("format_liste_v_vers_tablo fonctionne avec des noms d'identifiants et années différents", {
  liste_v <- list(
    Var1 = data.table(ID = c(1, 2), `2018` = c(5, 10), `2019` = c(7, 12)),
    Var2 = data.table(ID = c(1, 2), `2018` = c(50, 100), `2019` = c(70, 120))
  )

  result <- format_liste_v_vers_tablo(liste_v, variable_ident = "ID", variable_temporelle = "YEAR")

  expect_true(all(c("ID", "YEAR", "Var1", "Var2") %in% names(result)))
})

test_that("format_liste_v_vers_tablo gère correctement une liste vide", {
  liste_v <- list()

  result <- format_liste_v_vers_tablo(liste_v)

  expect_true(nrow(result) == 0)
})

test_that("format_liste_v_vers_tablo gère correctement une seule variable", {
  liste_v <- list(
    Var1 = data.table(FINESS = c(1, 2), `2017` = c(30, 40), `2018` = c(50, 60))
  )

  result <- format_liste_v_vers_tablo(liste_v)

  expect_true(all(c("FINESS", "ANNEE", "Var1") %in% names(result)))
  expect_equal(nrow(result), 4)  # 2 FINESS x 2 années
})

test_that("Conversion liste_v -> tablo -> liste_v conserve les données", {
  liste_v_original <- list(
    Var1 = data.table(FINESS = c(1, 2), `2020` = c(10, 20), `2021` = c(15, 25)),
    Var2 = data.table(FINESS = c(1, 2), `2020` = c(100, 200), `2021` = c(150, 250))
  )

  # Convertir liste_v en tablo puis revenir en liste_v
  tablo <- format_liste_v_vers_tablo(liste_v_original)
  liste_v_result <- format_tablo_vers_liste_v(tablo)

  # Vérification : chaque variable doit correspondre exactement
  expect_equal(names(liste_v_result), names(liste_v_original))

  for (var in names(liste_v_original)) {
    expect_true(all.equal(setDT(liste_v_result[[var]]), setDT(liste_v_original[[var]])))
  }
})

test_that("Conversion tablo -> liste_v -> tablo conserve les données", {
  tablo_original <- data.table(
    FINESS = c(1, 1, 2, 2),
    ANNEE = c(2020, 2021, 2020, 2021),
    Var1 = c(10, 15, 20, 25),
    Var2 = c(100, 150, 200, 250)
  )

  # Convertir tablo en liste_v puis revenir en tablo
  liste_v <- format_tablo_vers_liste_v(tablo_original)
  tablo_result <- format_liste_v_vers_tablo(liste_v)

  # Forcer le type de ANNEE pour qu'il corresponde à l'original
  tablo_result[, ANNEE := as.numeric(as.character(ANNEE))]

  # Vérification : la structure et les valeurs doivent être identiques
  expect_equal(setDT(tablo_result), setDT(tablo_original), ignore_attr = TRUE)
})


