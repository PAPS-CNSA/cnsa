test_that("test de regrouper_statut_juridique", {

  df_test <- data.frame(stat_juridique = c("01", "10", "40", "70", "99"))
  resultat_test <- regrouper_statut_juridique(df_test, "stat_juridique")

  # Vérifier que la colonne "statut_jur_fin" est bien ajoutée
  expect_true("statut_jur_fin" %in% names(resultat_test))

  # Vérifier que la fonction catégorise correctement les valeurs
  expect_equal(resultat_test$statut_jur_fin[1], "PublicAutonome")
  expect_equal(resultat_test$statut_jur_fin[2], "PublicRattacheEPS")
  expect_equal(resultat_test$statut_jur_fin[3], "PriveNonLucratif")
  expect_equal(resultat_test$statut_jur_fin[4], "PriveLucratif")
  expect_equal(resultat_test$statut_jur_fin[5], "Autre")

  # Vérifier que le nombre de lignes n'a pas changé après application de la fonction
  expect_equal(nrow(df_test), nrow(resultat_test))
})
