##########################################################################
###Script pour importer le fichier nhanesV1 de Jean-Vincent et obtenir un
###jeu simplifié de hypertension
##########################################################################

library(data.table)
# Chargement du fichier nhanesV1 de Jean-Vincent
nhanes <- read.csv("data/nhanes_29122018.csv")

# ajout de 2 attributs dr1tot pour les 2 variables manquantes: DR1TFIBE et DR1TATOC
names(nhanes)[89] <- "DR1TFIBE_dr1tot"
names(nhanes)[95] <- "DR1TATOC_dr1tot"

# Creer le vecteur des individus qui n'ont pas repondu à la question d'hypertension
ind_a_enlever <- which(is.na(nhanes$BPQ020_bpq))

# enlever ces individus de la table nhanes
nhanes <- nhanes[-ind_a_enlever,]

# conversion en data.table pour pouvoir intégrer le code de Jean-Vincent
nhanes <- as.data.table(nhanes)

# reduction du nombre de level de la variable education
nhanes[DMDEDUC2_demo %in% c("1","2","3"),Var_EDUCATION:="secondaire",]
nhanes[DMDEDUC2_demo %in% c("4","5"),Var_EDUCATION:="superieur",]
nhanes[is.na(DMDEDUC2_demo) | DMDEDUC2_demo %in% c("7","9"),Var_EDUCATION:=NA,]
nhanes[,c("DMDEDUC2_demo"):=NULL,]

# reduction du nombre de level de la variable emploi
nhanes[OCD150_ocq %in% c("1","2"),Var_TRAVAIL:="oui",]
nhanes[OCD150_ocq %in% c("3", "4","7","9") | is.na(OCD150_ocq),Var_TRAVAIL:="non",]
nhanes[,c("OCD150_ocq"):=NULL,]

# reduction du nombre de variable liée à la drogue
nhanes[DUQ200_duq=="1" | DUQ240_duq=="1" | DUQ370_duq=="1",Var_DROGUE:="oui",]
nhanes[!(DUQ200_duq=="1" | DUQ240_duq=="1" | DUQ370_duq=="1"),Var_DROGUE:="non",]
nhanes[,c("DUQ200_duq","DUQ240_duq","DUQ370_duq"):=NULL,]

# reduction du nombre de level de la variable depression
nhanes[DPQ020_dpq=="0",Var_DEPRESSION:="non",]
nhanes[DPQ020_dpq %in% c("1","2","3"),Var_DEPRESSION:="oui",]
nhanes[DPQ020_dpq %in% c("7","9") | is.na(DPQ020_dpq),Var_DEPRESSION:=NA, ]
nhanes[,c("DPQ020_dpq"):=NULL,]

# reduction du nombre de level de la variable status marital
nhanes[DMDMARTL_demo %in% c("1","6"),Var_SITUATION:="couple",]
nhanes[DMDMARTL_demo %in% c("2","3","4","5"),Var_SITUATION:="seul",]
nhanes[DMDMARTL_demo %in% c("77","99") | is.na(DMDMARTL_demo),Var_SITUATION:=NA,]
nhanes[,c("DMDMARTL_demo"):=NULL,]

nhanes <- as.data.frame(nhanes)

# repérer les variable qui ne dépassent pas les 5% de NA's
var_na_acceptable <- labels(which(apply(nhanes,2,function (x) sum(is.na(x)))<=floor(dim(nhanes)[1]/20)))

# netoyage de nhanes pour ne garder que les variables moins de 5% de NA's
nhanes <- nhanes[,var_na_acceptable]

# enlever la champs "X" qui ne sert à rien
nhanes$X <- NULL

# renommer la varible d'etude en y, pour mon cas -> hypertension
# BPQ020 - Ever told you had high blood pressure (à garder)
colnames(nhanes)[grep("BPQ020*",colnames(nhanes))] <- "y"

# deplacer le y en dernier
nhanes <- cbind(subset(nhanes,select=-y),nhanes$y)

# le fichier de sortie pour l'étude d'obésité, il reste à faire l'étude statistique dessus
write.csv(nhanes,"data/nhanes_hyper.csv")

