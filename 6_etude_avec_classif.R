################################
#Etude classification###########
################################

# Chargement du jeu de donnée
don <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)

# trouver les variables quali
ind <- which(sapply(colnames(don), function(x) class(don[,x]))=="factor")
# table qu'avec les variables quali
don[,ind]
# table avec les variables quantis
don[,-ind]

# librairie utile pour construire la table disjonctive
library(ade4)

#ponderation par la frequence des indicatrices
PF <- function(x){
  m <- mean(x)
  return(x/sqrt(m))
}

# On transforme la table avec les variables quali en tableau disjonctif et on pondere par la frequence
don_disj <- sapply(acm.disjonctif(don[,ind]),PF)
# On réunit la table des variable quanti centrés réduits et le tableau disjonctif
donb <- cbind(scale(don[,-ind]),don_disj)

# On applique l'ACP avec le critère de distance Ward
cah <- hclust(dist(donb),method = "ward.D2")
plot(as.dendrogram(cah))
rect.hclust(cah,h=140)
cutree(cah, h=140)

# Methode de Kmeans
km <- kmeans
for (i in 1:30){

}
