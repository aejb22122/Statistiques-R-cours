#               Introduction à la statistique avec R
#                     Oct 2018
#                     Annick E.
#                     Devoir no 3
#                Satisfaction à l'hôpital (3/3)

# ------- Préliminaires :
remove(list = ls())

# La première chose à faire c'est de définir le répertoire de travail, le fichier où sera contenu
# le script *.R et le fichier.

# Définition du répertoire de travail
setwd("~/OneDrive/Documents/1. After-doctorate/R_projects/Introduction a la statistique avec R/working_directory")

# Importation du fichier à analyser et définition du jeu de données (df -> pour dataframe)
# Le jeu de données est importé dans R
df <- read.csv2("satisfaction_hopital.csv") 

# Structure du jeu de données et identification des variables
str(df)

# ###
# Les variables catégorielles (ou qualitatives) mesurent juste des “états”, 
#  des catégories. Les variables catégorielles du jeu de données sont les suivantes :
# 1.- la variable 'service'
# 2.- variable 'sexe'
# 3.- la variable 'profession' 

# Faisons un recodage pour rendre les résultats des analyses plus claires
df$profession <- factor(df$profession, labels = c("1", "2", "3", 
                                                  "4", "5", "6", 
                                                  "7", "8"))

df$service <- factor(df$service, labels = c("1", "2", "3",  "4", "5", "6", "7", "8"))

df$sexe <- factor(df$sexe, labels = c("0", "1"))

# Vérifions que les codes sont corrects :
table(df$sexe, useNA = "always")
table(df$profession, useNA = "always")
table(df$service, useNA = "always")
# Vérifions que les variables sont effectivement des variables catégorielles
class(df$sexe)
class(df$service)
class(df$profession)




# ---- Question 1 ----

# Estimez le modèle de régression linéaire expliquant la variable « score.relation » par 
# les variables « age », « sexe », « score.information », « amelioration.sante », 
# « amelioration.moral », « profession »,  « service ». 

# (le script doit inclure la vérification éventuelle des 
# conditions de validité de la méthode utilisée).


# ---- Modèle de régression multiple
modele1 <- lm(score.relation ~ age + sexe + score.information + amelioration.sante + amelioration.moral
              + profession + service, data = df)


summary(modele1)

# ---- Conditions de validité du modèle
# ---- 1. normalité du bruit.
# ---- 2. la variance du bruit ne doit dépendre ni des valeurs de la variables à expliquer
# ni des variables explicatives.
# ---- 3. Le bruit doit etre un "vrai" bruit (pas de structure de corrélation évidente)

# Au plus, nous vérifions la normalité des résidus
hist(resid(modele1), main = "Résidus du modèle de régression multiple",  col="cornflowerblue", border="white")

# ---- Question 2 ----

# Estimez le modèle de régression logistique expliquant la variable « recommander.b » 
# par les variables « age », « sexe », « score.information », « amelioration.sante », 
# « amelioration.moral », « profession »,  « service ».  Notons que la variable « recommander.b » 
# est une transformation de la variable « recommander» en une variable binaire où « recommander.b » 
# vaut 0 si « recommander» vaut 0 ou 1, et 1 si « recommander» vaut 2. 
# (le script doit inclure la vérification éventuelle des conditions de validité de la méthode utilisée)


# Transformation de la variable « recommander » en une variable binaire « recommander.b » 
# Nous faisons le recodage à partir de "ifelse" : 
# L'instruction que l'on passe à R :si recommander est égale à 2, recoder 2 en 1 et en 0 si ce n'est pas le cas :
df$recommander.b <- ifelse(df$recommander == 2, 1, 0)

# Pour vérifier que les codes sont corrects :
str(df)                                         # On voit bien que la variable recommander.b a été crée
table(df$recommander.b, useNA = "always")       # avec les modalités anticipées

# Parce que nous allons utiliser un modèle logistique, transformons cette variable en une variable
# catégorielle afin que R puisse la traiter comme tel :

df$recommander.b <- factor(df$recommander.b)
class(df$recommander.b)

# Le modèle de régression logistique 
modele2 <- glm(recommander.b ~ age + sexe + score.information + amelioration.sante + 
                       amelioration.moral + profession + service, data = df, family=binomial("logit"))
summary(modele2)

##### Verifions les conditions de validité du modèle avant l'interprétation des résultats du modèle
##### de régression logistique

# Les conditions de validité => au moins 5 à 10 évènements par variable explicatives

# Attention, profession et service seront transformés en 7 variables chacune car 8 classes 
# 5+7+7=19 variables explicatives
# il faudrait donc au moins entre 19*5 = 95 et 19*10=190 sujets

nrow(df) 
# Nous avons 534 sujets --> les conditions de validité sont correctes.

# Estimons les coefficients du modèle
modele2$coefficients
summary(modele2)

# Uniquement les variables "score.information" et "amelioration.sante" sont statistiquement
# différent de zéro. 

# Nous pouvons interpréter l'amplitude du coefficient et nous savons que les
# coefficients tels quels ne sont pas interprétables. 
# Il faut prendre leur exponentielle à l'aide de l'instruction exp(coefficient). 
# Nous obtenons les résultats qui suivent :

# interprétation avec les odds ratio :
exp(coefficients(modele2))

table(df$recommander.b) 

##### Évaluons l'effet global des variables catégorielles "profession" et "service".
drop1(modele2,.~., test = "Chisq") 

# Globalement, il n'y a pas d'effet des 2 variables catégorielle "profession" et "service" sur "recommander.b".


print("FIN !!!")