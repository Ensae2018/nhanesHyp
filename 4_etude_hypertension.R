rm(list=ls())

library(mice)
library(VIM)
library(data.table)

# Chargement du fichier transcodifié, enlevement de la 1ere colonne X et renommer la variable de sortie en Y
nhanes_hyper <- read.csv("data/nhanes_hyper_transcodifie.csv")
nhanes_hyper$X <- NULL
names(nhanes_hyper)[ncol(nhanes_hyper)] <- "Y"
nhanes_hyper$Y <- as.factor(nhanes_hyper$Y)
levels(nhanes_hyper$Y) <- c("Yes","No",NA)

# on remet les noms des champs propre
names(nhanes_hyper) <- gsub("\\.\\.?\\.?","_",names(nhanes_hyper))
names(nhanes_hyper) <- gsub("_$","", names(nhanes_hyper))
names(nhanes_hyper)[89] <- "Total_plain_water_drank_yesterday"
names(nhanes_hyper)[57] <- paste0("alpha_",names(nhanes_hyper)[57])
names(nhanes_hyper)[58] <- paste0("beta_",names(nhanes_hyper)[58])
names(nhanes_hyper)[58] <- sub("_1$","",names(nhanes_hyper)[58])

# pour les codes qui ont et des soucis de transcodification, transco manuel
levels(nhanes_hyper$Ever_told_you_have_Hepatitis_B) <- c("Yes","No","Refused","Don't know")
levels(nhanes_hyper$Ever_told_you_have_Hepatitis_C) <- c("Yes","No","Don't know")
levels(nhanes_hyper$Received_Hepatitis_A_vaccine) <- c("At least 2 doses","Less than 2 doses",
                                                       "No doses","Refused", "Don't know")
levels(nhanes_hyper$Received_Hepatitis_B_3_dose_series) <- c("At least 2 doses","Less than 2 doses",
                                                             "No doses","Refused", "Don't know")
levels(nhanes_hyper$Ever_been_told_you_have_asthma) <- c("Yes", "No", "Don't know")
levels(nhanes_hyper$X_of_people_who_live_here_smoke_tobacco) <- c("No one in houseold is a smoker",
                                                                  "1 household member is a smoker",
                                                                  "2 household members are smokers",
                                                                  "3 or more household members are smokers",
                                                                  "Refused",NA)
levels(nhanes_hyper$Doctor_ever_said_you_were_overweight) <- c("Yes","No","Don't know")
levels(nhanes_hyper$Home_owned_bought_rented_other)[2] <- NA
levels(nhanes_hyper$Overall_recommendation_for_care)[2] <- NA
levels(nhanes_hyper$Overall_recommendation_for_care)[3] <- NA
levels(nhanes_hyper$VAR_TRAVAIL) <- c("No","Yes")

#Alleger un peu la description
levels(nhanes_hyper$Total_number_of_people_in_the_Household)[7] <- "7 or more"
levels(nhanes_hyper$Total_number_of_people_in_the_Family)[7] <- "7 or more"

#Qlqs nettoyage des variables non utile
nhanes_hyper$fed_infant_either_day <- NULL
nhanes_hyper$Number_of_days_of_intake <- NULL
nhanes_hyper$SEQN <- NULL

for (i in 1:ncol(nhanes_hyper)){
  if (class(nhanes_hyper[,i])=="factor"){
    for (j in 1:length(levels(nhanes_hyper[,i]))){
      if (levels(nhanes_hyper[,i])[j]=="Don't know"){
        levels(nhanes_hyper[,i])[j]=NA
        break
      }
    }
  }
}
for (i in 1:ncol(nhanes_hyper)){
  if (class(nhanes_hyper[,i])=="factor"){
    for (j in 1:length(levels(nhanes_hyper[,i]))){
      if (levels(nhanes_hyper[,i])[j]=="Refused"){
        levels(nhanes_hyper[,i])[j]=NA
        break
      }
    }
  }
}

# Etude du jeu de donnée nhanes_hyper
don <- nhanes_hyper

md.pattern(don)
aggr_plot <- aggr(don, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

don$Received_Hepatitis_A_vaccine <- NULL
don$Received_Hepatitis_B_3_dose_series <- NULL

# Remplacement des valeurs NA à l'aide de la méthode Mice
tempdata <- mice(don,m=1,seed=1234)
tempdata$imp
don <- complete(tempdata,1)



write.csv(don,"data/nhanes_hyper_mice.csv")


############################################################################
# Etude sur la prediction hypertension
############################################################################
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel

don <- read.csv("data/nhanes_hyper_mice.csv")
don$X <- NULL
levels(don$Y) <- c(0,1)

XX <- as.matrix(model.matrix(~.,don)[,-ncol(model.matrix(~.,don))])
YY <- as.matrix(model.matrix(~.,don)[,ncol(model.matrix(~.,don))])

bloc <- 4
ind= sample(1:nrow(don) %% 4+1)
res <- data.frame(Y=don$Y, log=0, ridge=0, lasso=0, elastic=0,
                  foret=0, adabo=0, logibo=0, svm=0, pm=0, arbre=0)
set.seed(1234)
deb <-   Sys.time()
foreach (i = 1:bloc, .packages = c("gbm","e1071","glmnet","randomForest", "rpart")) %dopar% {
  # logisque
  mod <- glm(Y~.,data=don[ind!=i,],family="binomial")
  res[ind==i,"log"] <- predict(mod,don[ind==i,],type="response")
  # XXA <- XX[ind!=i,]
  # YYA <- YY[ind!=i,]
  # XXT <- XX[ind==i,]
  # # ridge
  # tmp <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  # mod <- glmnet(XXA,YYA,alpha=0,lambda=tmp$lambda.min, family="binomial")
  # res[ind==i,"ridge"] <- predict(mod,newx=XXT,type="response")
  # # lass0
  # tmp <- cv.glmnet(XXA,YYA, alpha=1, family="binomial")
  # mod <- glmnet(XXA,YYA,alpha=1, lambda =tmp$lambda.1se,family="binomial" )
  # res[ind==i,"lasso"] <- predict(mod,newx=XXT, type="response")
  # # elasticnet
  # tmp <- cv.glmnet(XXA,YYA, alpha=0.5, family="binomial")
  # mod <- glmnet(XXA,YYA,alpha = 0.5, lambda = tmp$lambda.min, family="binomial")
  # res[ind==i,"elastic"] <- predict(mod,newx=XXT,type="response")
  # # foret
  # mod <- randomForest(Y~., data = don[ind!=i,], ntree=500)
  # res[ind==i, "foret"] <- predict(mod, don[ind==i,], type="prob")[,2]
  # # Adaboost
  # tmp <- gbm(as.numeric(Y)-1~.,data = don[ind!=i,], distribution = "adaboost", interaction.depth = 2,
  #            shrinkage = 0.1,n.trees = 500)
  # M <- gbm.perf(tmp)[1]
  # mod <- gbm(as.numeric(Y)-1~.,data = don[ind!=i,], distribution = "adaboost", interaction.depth = 2,
  #            shrinkage = 0.1,n.trees = M)
  # res[ind==i, "adabo"] <- predict(mod, newdata=don[ind==i,], type = "response", n.trees = M)
  # # Logiboost
  # tmp <- gbm(as.numeric(Y)-1~.,data=don[ind!=i,], distribution="bernoulli", interaction.depth = 2,
  #            shrinkage=0.1,n.trees=500)
  # M <- gbm.perf(tmp)[1]
  # mod <- gbm(as.numeric(Y)-1~.,data=don[ind!=i,], distribution="bernoulli", interaction.depth = 2,
  #            shrinkage=0.1,n.trees=M)
  # res[ind==i, "logibo"] <- predict(mod,newdata=don[ind==i,], type= "response", n.trees = M)
  # # SVM
  # mod <- svm(Y~.,data=don[ind!=i,], kernel="linear",probability=T)
  # # tmp <- tune(svm,Y~.,data=don[ind!=i,], kernel="linear",probability=T,ranges =
  # #        list(cost=c(0.1,1,10,100)))
  # # mod <- tmp$best.model
  # res[ind==i,"svm"] <- attr(predict(mod,newdata = don[ind==i,],probability = T),"probabilities")[,2]
  # # arbre
  # mod <- rpart(Y~., data = don[ind!=i,], method="class")
  # res[ind==i, "arbre"] <- predict(mod, don[ind==i,], type="prob")[,2]
}
fin <- Sys.time()
fin-deb

############################################
# Perceptron multicouche
############################################
library(keras)

for (i in 1:bloc){

  # instanciation du model
  pm.keras <- keras_model_sequential()

  # architecture
  pm.keras %>%
    layer_dense(units = 2, input_shape=ncol(XXA), activation = "sigmoid") %>%
    layer_dense(units = 1, activation = "sigmoid")

  # configuration de l'apprentissage
  pm.keras %>% compile(
    loss="mean_squared_error",
    optimizer="sgd",
    metrics="mae"
  )

  # lancement de l'apprentissage
  pm.keras %>% fit(
    XXA <- XX[ind!=i,],
    YYA <- YY[ind!=i,],
    epochs=80,
    batch_size=8
  )

  # proba prediction
  res[ind==i,"pm"] <- pm.keras %>% predict(XX[ind==i,])
}

############################################
monerreur <- function(X, Y, seuil=0.5){
  table(cut(X, breaks = c(0,seuil,1)), Y)
}

# matrice de confusion
monerreur(res[,2],res[,1])#log
monerreur(res[,3],res[,1])#Ridge
monerreur(res[,4],res[,1])#Lasso
monerreur(res[,5],res[,1])#Elastic
monerreur(res[,6],res[,1])#Foret
monerreur(res[,7],res[,1])#Adabo
monerreur(res[,8],res[,1])#Logibo
monerreur(res[,9],res[,1])#SVM
monerreur(res[,10],res[,1])#perceptron
monerreur(res[,11],res[,1])#arbre

# erreur
monerreurb <- function(X,Y,seuil=0.5){
  Xc <- cut(X,breaks=c(0,seuil,1),labels=c(0,1))
  sum(as.factor(Y)!=Xc)
}
apply(res[,-1],2,monerreurb,Y=res[,1],seuil=0.5)

library(pROC)
auc(res[,1],res[,2])#log
auc(res[,1],res[,3])#Ridge
auc(res[,1],res[,4])#Lasso
auc(res[,1],res[,5])#Elastic
auc(res[,1],res[,6])#Foret
auc(res[,1],res[,7])#Adabo
auc(res[,1],res[,8])#Logibo
auc(res[,1],res[,9])#SVM
auc(res[,1],res[,10])#perceptron
auc(res[,1],res[,11])#arbre

plot(roc(res[,1],res[,2]))
lines(roc(res[,1],res[,3]), col="red")#log
lines(roc(res[,1],res[,4]), col="blue")#Ridge
lines(roc(res[,1],res[,5]), col="green")#Lasso
lines(roc(res[,1],res[,6]), col="brown")#Elastic
lines(roc(res[,1],res[,7]), col="yellow")#Adabo
lines(roc(res[,1],res[,8]), col="purple")#Logibo
lines(roc(res[,1],res[,9]), col="grey")#SVM
lines(roc(res[,1],res[,9]), col="black")#perceptron
lines(roc(res[,1],res[,9]), col="orange")#arbre

