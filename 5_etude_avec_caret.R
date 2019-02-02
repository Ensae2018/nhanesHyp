####################################
#Avec Caret
####################################
library(caret)
library(pROC)

don <- read.csv("data/nhanes_hyper_mice.csv")
don$X <- NULL

ind <- createDataPartition(don$Y,p=0.7, list=F)
donA <- don[ind,]
donT <- don[-ind,]
res <- data.frame("Y"=donT$Y, "log"=0, "logchoix"=0, "logDown"=0, "lift"=0, "svm"=0)

# Methode d'apprentissage avec cv avec glm
fitControl <- trainControl(method = "cv", number=4)
mod <- train(Y~.,donA, method="glm", trControl=fitControl)
res[,"log"] <- predict(mod, donT,type="raw")
matlog <- confusionMatrix(res[,"Y"], res[,"log"])
varImp(mod)

# Methode d'apprentissage sans cv avec glm avec choix de variable
fitControl <- trainControl(method="none")
mod <- train(Y~., donA, method="glmStepAIC",trControl=fitControl)
res[,"logchoix"] <- predict(mod, donT,type="raw")
matlogchoix <- confusionMatrix(res[,"Y"], res[,"logchoix"])

# Courbe lift
res[,"lift"] <- predict(mod, donT, type="prob")["Yes"]
plot(lift(res[,"Y"]~res[,"lift"], data=donT, class = "Yes"))

# Courbe roc
rocobj <- roc(res[,"Y"],res[,"lift"])
plot(rocobj)
plot(1-rocobj$specificities, rocobj$sensitivities, type="l")
abline(0,1)

# traitement des classes déséquilibrées
donDown <- downSample(donA[,-89],donA$Y, yname = "Y")
table(donDown$Y)
table(donA$Y)
mod <- train(Y~., data=donDown, trControl=trainControl(method="none"), method="glm")
res[,"logDown"] <- predict(mod, donT)
matlogDown <- confusionMatrix(res[,"Y"],res[,"logDown"])

# Methode d'apprentissage avec SVM
fitControl <- trainControl(method = "none")
mod <- train(Y~., donA, method = "svmLinear", trControl=fitControl)
res[,"svm"] <- predict(mod,donT)
matsvm <- confusionMatrix(res[,"Y"],res[,"svm"], positive="Yes")

######################################################################
# Comparaison methode
######################################################################
evaluation <- function(approche){
  mod <- train(Y~., data= donA, trControl=trainControl(method = "cv", number = 5), method=approche)
  pred <- predict(mod, donT)
  mat <- confusionMatrix(donT$Y, pred, positive="Yes")
  return(c(max(mod$results[,"Accuracy"]),mat$overall["Accuracy"]))
}

methodes <- c("glm","rpart","lda", "svmLinear")
perfos <- sapply(methodes, evaluation)
colnames(perfos) <- methodes
rownames(perfos) <- c("cv", "test set")
print(perfos)

####################fin#####################"
