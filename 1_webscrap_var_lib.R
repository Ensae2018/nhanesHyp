###############################################################################
###Script pour faire le webscrapping de correspondance variable-desciption#####
###############################################################################

library("rvest") #utile pour faire du webscrapping
library(stringr) #utile pour utiliser la fonction str_sub

table <- c() #définier un vecteur vide pour stocker les noms des tables
signification <- data.frame(code="",libel="") # définir un dataframe pour stocker variable et desc
# les différents topic de Nhanes
topic <- c("Demographics&CycleBeginYear=2015",
           "Dietary&CycleBeginYear=2015",
           "Examination&CycleBeginYear=2015",
           "Laboratory&CycleBeginYear=2015",
           "Questionnaire&CycleBeginYear=2015")

# Webscrapping de tous les noms de tables
for (i in 1:length(topic)){
url_nhanes <- "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component="
url_topic <- topic[i]
url <- paste0(url_nhanes,url_topic)
data_html <- read_html(url)
questionnaire <- as.data.frame(data_html %>%
  html_nodes(xpath = '(//*[@id="GridView1"])') %>%
  html_table())
table <- c(table,str_sub(questionnaire$Doc.File,1,-5)) # avec stringr
}

# Webscrappinf de tous les noms de varaible ainsi que la description
for (i in 1:length(table)){
  print(i)
url <- paste0("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/", table[i], ".htm")
data_html <- read_html(url)
tempo <- data_html %>%
  html_nodes(xpath = '(//*[@id="CodebookLinks"]//li/a)') %>%
  html_text()
if (length(tempo) != 0){
signification <- rbind(signification,cbind(code=paste(gsub("^([a-z0-9_]+).*-.*","\\1", tempo, ignore.case = TRUE),
                                                      table[i],sep="_"),
                                           libel=gsub("[A-Z0-9]+.*- *","", tempo, ignore.case = F)))
}}

# Enlevement de la 1ere ligne vide qui ne sert à rien
signification <- signification[-1,]

# Enlevement de "_I" du champs code
signification$code <- str_sub(signification$code,1,-3)

# Vérification des 5 premières lignes
head(signification)

# Ecriture sous le fichier csv
write.csv(signification,"var_lib.csv")
