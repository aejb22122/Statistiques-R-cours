#               Introduction à la statistique avec R
#                     26 Sept 2018
#                     Annick E.
#                     Devoir no 2
#                Satisfaction à l'hôpital (2/3)

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

# ---- Question No 1 ----
# Transformation de la variable « recommander » en une variable binaire « recommander.b » 
# Nous faisons le recodage à partir de "ifelse" : 
# L'instruction que l'on passe à R :si recommander est égale à 2, recoder 2 en 1 et en 0 si ce n'est pas le cas :
df$recommander.b <- ifelse(df$recommander == 2, 1, 0)

# Pour vérifier que les codes sont corrects :
str(df)                                         # On voit bien que la variable recommander.b a été crée
table(df$recommander.b, useNA = "always")       # avec les modalités anticipées

# L'option deparse.level=2, est utilisée afin de nous renseigner sur le nom de nos variables 
table(df$recommander, df$recommander.b, deparse.level = 2, useNA = "always")

# Nous avons bien les modalités 0 et 1 de la variable "recommander" qui ont été recodées en "1" (16+120)
# et la modalité "2" qui a été recodée en "1" dans la nouvelle variable crée. 
# Nous avons toujours la même quantité de valeurs manquantes ("NA" = 129).

# ---- Question No 2 ----
# A l’aide d’un odds-ratio, estimez la force de l’association entre « recommander.b » 
# et « sexe ». 
# Estimez un intervalle de confiance de cet odds-ratio".

# à partir de la librarie "Epi"
# install.packages("Epi")       # Si cela n'a pas été déjà fait
library(Epi)

# ---- Calcul de l'odd-ratio
# La fonction twoby2() nous donne l'odd-ratio, de même que la fonction fisher.test()
# et les intervalles de confiance
twoby2(1-df$recommander.b, 1-df$sexe)
fisher.test(df$recommander.b, df$sexe)

# L'odd ratio est de  
# 1.083487

# Avec un intervalle à 95% tel que : [ 0.7169 ; 1.6383]
# Avec un odd-ratio = 1,08 on ne peut pas affirmer qu'il y a une relation entre la variable "recommander.b"
# et la variable "sexe” ; le p-value = 0,7523 permet de conclure à la même conclusion.

# ---- Question No 3 ----
# Calcul de la corrélation (de Pearson) entre « score.relation » et « âge ».

# D'abord, il faut que la distribution de la variable "score.relation" ou celle de "âge" 
# suivent une loi normale.
hist(df$score.relation, main = "Histogramme de la variable score.relation")
hist(df$age, main = "Histogramme de la variable âge")

# De manière graphique (visuelle), on peut affirmer que le "score.relation" ne suit pas une loi normale
# tel n'est pas le cas de la variable "âge" qui semble suivre une loi normale (plus ou moins) ...
# de manière plus rigoureuse, répondons à la question de la normalité des deux variables avec un qq-plot.
qqnorm(df$age) ; qqline(df$age)
qqnorm(df$score.relation) ; qqline(df$score.relation)

# La variable 'score.relation' ne suit pas une loi normale;
# peut être que la variable 'âge' un peu une loi normale ; avec un léger écart à la normalité ...

# ---- Calcul du coeficient de corrélation de Pearson :
cor.test(df$score.relation, df$age, use = "complete.obs", method = "pearson")
# Nous avons un p-value = 0.07336 > 0,05 ; les deux variables n'ont pas de corrélation.

# ---- Question No 4 ----
# La moyenne du score de relation est-il significativement différent chez 
# les hommes et chez les femmes ? (Le script doit inclure la vérification 
# éventuelle des conditions de validité de la méthode utilisée)"

# Comparaison de deux moyennes : score.relation et sexe :
# Nous allons utiliser le test t de Student ; mais d'abord, voyons si les conditions de validité sont respectées :
# 1) Classiquement, on dit qu'on peut utiliser un test t de Student quand on a n>30
# Nous avons 534 individus ce qui est suffisant pour cette première condition de validité du test.
length(df$score.relation)
length(df$sexe)

# 2) On peut appliquer le test de t-Student quand les variables à étudier suivent une loi normale
# La variable "sexe" étant binaire, nous anticipons qu'il y a peu de chance qu'elle suive une loi normale ...

# de manière plus rigoureuse, répondons à la question de la normalité des deux variables avec un qq-plot.
qqnorm(df$score.relation) ; qqline(df$score.relation)

# 3) Il faut que les variances de nos variables soient égales dans chaque groupe
# Nous utilisons l'écart-type, plus facile à interpréter
by(df$score.relation, df$sexe, sd, na.rm = TRUE)

#  Les écart-types sont plus ou moins proches

# ---- Calcul du t-student :
# Nous pouvons maintenant calculer le t-student avec la fonction t.test():
t.test(df$score.relation ~ df$sexe, var.equal = TRUE)

# Le p-value = 0.2649 est largement supérieur à 0,05, on ne peut donc pas dire qu'il existe 
# une différence statistiquement significative entre score relatif à la qualité des relations 
# avec le personnel soignant pendant le séjour (score.relation) et le sexe des patiens.

# S'il y a des doutes sur les conditions d'utilisation d'un test t-student; nous pouvons
# utiliser le test de Wilcoxon Mann-Whitney
# avec la fonction wilcox.test()
wilcox.test(df$score.relation ~ df$sexe)

# On arrive aux mêmes résultats que le test de student, mais nous préférons utiliser le test de 
# Student ; avec le test de Wilcoxon Mann-Whitney, on ne pourra pas aller plus loin avec les analyses ...

print("Fin")
