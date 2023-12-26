#' @importFrom stats rnorm
#' @importFrom fitdistrplus fitdist plotdist
NULL

#' Fonction qui tire selon une loi normale
#'
#' Cette fonction permet de tirer aléatoirement des variables qui suivent une loi normale identique à celle calculée pour une variable de référence.
#' Pour tester si l'hypothèse de normalité est plausible, il est possible d'utiliser l'argument tests = TRUE
#'
#' @param vecteur vecteur de variables (numériques) de référence
#' @param nb_a_tirer nb d'éléments à tirer selon la loi normale
#' @param tests TRUE ou FALSE, selon qu'on veut afficher les résultats des tests de normalité (facultatif ; par défaut : FALSE)
#' @param positif TRUE ou FALSE selon qu'on accepte ou non les valeurs négatives. Par défaut, TRUE (le résultat ne doit inclure que des valeurs positives)
#'
#' @return un vecteur de taille nb_a_tirer tiré suivant une loi normale qui suit la même que l'élément "vecteur" en entrée
#' @export

tirer_selon_loi_normale <- function(vecteur, nb_a_tirer, tests = FALSE, positif = TRUE) {
  # Cette fonction fournit un vecteur qui tire selon une disibution normale cohérente avec celle fournit en entrée. Il est également proposé de fournir
  # quelques tests pour voir si la variable en entrée a effectivement une distribution normale

  # On calcule la moyenne et l'écart type :
  moy = mean(vecteur, na.rm = T)
  etype = sd(vecteur, na.rm = T)

  # On génère des éléments en suivant la loi normale
  resultat <- rnorm(nb_a_tirer, mean = moy, sd = etype)
  nb_negatifs <- sum(resultat<=0,na.rm = T)

  if (positif) {
    while (nb_negatifs>0) { # On ne veut que des résultats positifs : dans ce cas, on retire
      resultat[resultat<=0] <- rnorm(nb_negatifs, mean = moy, sd = etype)
      nb_negatifs <- sum(resultat<=0,na.rm = T)
    }
  }


  if (tests) {
    fit_norm <- fitdist(vecteur, "norm")
    plotdist(vecteur, "norm", para = list(mean = fit_norm$estimate["mean"], sd = fit_norm$estimate["sd"]))

    ad_test_result <- ad.test(vecteur) # Anderson-Darling test
    lillie_test_result <- lillie.test(vecteur) # Lilliefors (Kolmogorov-Smirnov) test
    cvm_test_result <- cvm.test(vecteur) # Cramer-von Mises test

    # Afficher les résultats
    print(ad_test_result)
    print(lillie_test_result)
    print(cvm_test_result)

  }

  return(resultat)
}
