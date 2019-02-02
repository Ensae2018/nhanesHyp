#####################################################################################
###Script pour transcodifier les code et variable vers leur description##############
####################################################################################

#importer le fichier webscrapping code -> Libelle d'INSAF
transco <- read.csv("data/transco.csv")
transco$X <- NULL

#importer le fichier webscrapping variable -> Libelle d'Yifan
signification <- read.csv("var_lib.csv")
signification$X <- NULL

# importer le fichier Nhanes_hypertension pour mon cas
nhanes_hyper <- read.csv("data/nhanes_hyper.csv")
nhanes_hyper$X <- NULL

# remplacer le nom de la variable à expliquer par Y au lieu de nhanes.y
names(nhanes_hyper)[ncol(nhanes_hyper)] <- "y"

# Mettre tous les noms de variable au majuscule afin de match la recherche du texte
names(nhanes_hyper) <- toupper(names(nhanes_hyper))

# vecteur des variables à garder dans la trascofication des codes -> Libelle
var_a_garder_transco <- colnames(nhanes_hyper)[2:(dim(nhanes_hyper)[2] - 1)]

# simplifier le vecteur transcodification code -> Libelle
transco_hyper <- dplyr::filter(transco, transco$var_tab %in% var_a_garder_transco)

# passer de facteur à character pour la colonne transcodification de dataframe hypertension
transco_hyper$Value.Description <- as.character(transco_hyper$Value.Description)

########################################################################
### Boucle à lancer pour la transcodification Code-> Libelle d'Insaf ###
########################################################################

# remplacer les valeur par "nom colonne"_valeur pour la transcodification
for (i in 2:(dim(nhanes_hyper)[2] - 1)) {
  nhanes_hyper[, i] <- factor(nhanes_hyper[, i])
  if (nlevels(nhanes_hyper[, i]) < 10) {
    nhanes_hyper[, i] <-
      paste(colnames(nhanes_hyper)[i], nhanes_hyper[, i], sep = "_")
  }
}

# transcodification améliorée (à utiliser)
debut <- Sys.time()
for (k in 2:(dim(nhanes_hyper)[2] - 1)) {
  nhanes_hyper[, k] <- factor(nhanes_hyper[, k])
  if (nlevels(nhanes_hyper[, k]) < 10) {
    for (i in 1:length(levels(nhanes_hyper[, k]))) {
      for (j in 1:dim(transco_hyper)[1]) {
        if (levels(nhanes_hyper[, k])[i] == transco_hyper$jointure[j]) {
          levels(nhanes_hyper[, k])[i] <- transco_hyper$Value.Description[j]
        } else{
          levels(nhanes_hyper[, k])[i]
        }
      }
    }
  }
}
fin <- Sys.time()
fin - debut
##########################################################################
###Boucle à lancer pour la transcodification de nom de variable -> Libelle
##########################################################################
signification$libel <- as.character(signification$libel)

vec <- names(nhanes_hyper)
for (i in 1:length(vec)) {
  for (j in 1:nrow(signification)) {
    if (vec[i] == signification$code[j]) {
      vec[i] = signification$libel[j][1]
    }
  }
}
names(nhanes_hyper) <- vec
##########################################################################

# Annulation d'un champs non utile pour mon cas hypertension
nhanes_hyper$`Interview/Examination status` <- NULL

write.csv(nhanes_hyper,"data/nhanes_hyper_transcodifie.csv")
