#               Introduction à la statistique avec R
#                     26 Sept 2018
#                     Annick E.
#                     Devoir no 1

# ------- Préliminaires :
# La première chose à faire c'est de définir le répertoire de travail, le fichier où sera contenu
# le script *.R et le fichier.

# Définition du répertoire de travail
setwd("~/OneDrive/Documents/1. After-doctorate/R_projects/Introduction a la statistique avec R/working_directory")

# Importation du fichier à analyser et définition du jeu de données (df -> pour dataframe)
# Le jeu de données est importé dans R


df <- read.csv2("satisfaction_hopital.csv") 

# Structure du jeu de données et identification des variables
str(df)


# ---- Question No 1 ----
# Pour les trois variables catégorielles du fichier (à vous de déterminer de quelles 
# variables il s'agit), présentez les pourcentages de sujets relevant de chacune des 
# modalités.

# Les variables catégorielles (ou qualitatives) mesurent juste des “états”, 
# des catégories. Les variables catégorielles du jeux de données sont les suivantes :
# 1.- la variable 'service'
# 2.- variable 'sexe'
# 3.- la variable 'profession' 

# Faisons un recodage pour rendre les résultats des analyses plus claires
df$profession <- factor(profession, labels = c("agriculteur", 
                                               "artisan", 
                                               "cadre", 
                                               "intermédiaire", 
                                               "employé", "ouvrier", 
                                               "sans emploi", "autre"))

df$service <- factor(service, labels = c("1", "2", "3",  "4", "5", "6", "7", "8"))

df$sexe <- factor(sexe, labels = c("Homme", "Femme"))

# Vérifions que les codes sont corrects :
table(df$sexe, useNA = "always")
table(df$profession, useNA = "always")
table(df$service, useNA = "always")

# Calcul des pourcentages de sujets relevant de chacune des modalités
# Les pourcentages de sujets relevant de chacune des modalités
# Avec la fonction table(), nous pouvons obtenir les fréquences en divisant chaque valeur de la # table par le nombre total d'enregistrements ....
# La fonction round() permet juste d'avoir les résultats arrondis à 2 chiffres:

freq_sex <- table(df$sexe)/sum(table(df$sexe))*100
round(freq_sex, 2)

freq_profession <- table(df$profession)/sum(table(df$profession))*100
round(freq_profession, 2)

freq_service <- table(df$service)/sum(table(df$service))*100
round(freq_service, 2)


# ---- Question No 2 ----

# Pour les autres variables, donnez de façon synthétique : 
# moyenne, médiane, écart-type, minimum, maximum, nombre de données disponibles 
# (non manquantes)."

# Nous utilisons la librarie 'prettyR' (après l'avoir installée)
# install.packages('prettyR')
# Faisons appel à la librarie :
library(prettyR)

? describe
# Après avoir revu la documentation de la fonction "describe()" ; 

# Nous affichons les variables numériques; donc uniquement les variables :
# âge (en 3e position dans le jeu de données), amelioration.sante (en 5e position)
# amelioration.moral (en 6e position dans le jeu de données) et ainsi de suite ... :
# On obtient alors :

describe(df[c(3, 5, 6, 7, 8, 9)], 
         num.desc = c("mean", "median", "var", "sd", "valid.n"),
         xname = NA, 
         horizontal = FALSE)

# ---- Question No 3 ----
# Un histogramme du score de relation (score.relation).
hist(score.relation, 
     ylab = "Nombre de sujets", 
     xlab = "score.relation", 
     main = "Histogramme de la variable 'score de relation'", 
     labels = TRUE, 
     col = "blue")

# ---- Question No 4 ----
# A l’aide de deux « boxplots », représentez côte à côte la distribution 
# du score de relation chez les hommes et les femmes.

# Représentation côte à côte la distribution du score de relation 
# chez les hommes et chez les femmes.
boxplot(df$score.relation ~ df$sexe, 
        main = "Distribution du score de relation chez les hommes et les femmes", 
        names = c("Homme", "Femme"), 
        frame = FALSE, 
        ylab = "score.relation", xlab = "Sexe",
        col = "blue")

