# Introduction à la statistique avec R
# Semaine 4 ~ Régression linéaire simple et multiple, régression logistique

# Set working directory 
setwd("~/OneDrive/Documents/1. After-doctorate/R_projects/Introduction a la statistique avec R/working_directory")

# Liste des fichiers du directory
list.files()

# Nettoyage 
remove(list = ls())

# Importons le nouveau fichier - qui condient plus de variables
smp.1 <- read.csv2("smp2.csv")
str(smp.1)

# ---- Régression linéaire simple ----
# Doite de régression
plot(smp.1$age, smp.1$dur.interv)
plot(jitter(smp.1$age), jitter(smp.1$dur.interv, factor = 4))

# ajout de la ligne de régression - lwd = 2 permet d'avoir l'épaisseur de la droite
# de regression
abline(lm(smp.1$dur.interv ~ smp.1$age), lwd = 2)

# Est-ce que b = 0 (durée = a + b*age)
# Tester statistiquement "b":
# Estimation d'un modèle de régression linéaire :
mod1 <- lm(dur.interv ~ age, data = smp.1)
summary(mod1)

# Calcul de la corélation entre les deux variables
cor.test(smp.1$dur.interv, smp.1$age)

# La conclusion
# Dans un modèle linéaire simple, nous testons la nullité du coefficient de régression b de la variable à expliquer Y sur la variable explicative quantitative X. 
# Ce test cela apparente à un test la nullité du coefficient de correlation entre la variable X et la variable Y
# r = e.t.(age)/e.t.(durée entretient) * b

# Régression avec une variable binaire (x)
plot(jitter(smp.1$dep.cons), jitter(smp.1$dur.interv, factor = 4))
abline(lm(smp.1$dep.cons ~ smp.1$dur.interv))

# Test b=/=0 dans la régression =? 
# test t où durée(déprimés) =/= durée(non déprimés)?
mod2 <- lm(dur.interv ~ dep.cons, data = smp.1)
summary(mod2)

# Test t
t.test(smp.1$dur.interv ~ smp.1$dep.cons, var.equal = TRUE)
# On obtient le meme "p"
# Dans la régression --> on a le coef = 7,61 qui est la différence entre les deux moyennes entre les groupes
# Dans la t-test --> (mean in group 0 = 66.53767) - (mean in group 1 = 58.92341) = 7.61426
# Faire le modèle de régression ~= fait la meme chose que le t-test...

# ---- Régression linéaire multiple ----
# durée = a + b*age + c*dep + d*subst + e*scz + bruit
mod3 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons, data = smp.1)
summary(mod3)

# Y doit être quantitative
# X peut être binaire

# Variable explicative catégorielle avec plus de 2 modalités
# Il faut recoder toutes les modalités de la variable ....(1,0) pour chaques modalités (on aura n-1 modalités == recodés en variables)
# Ex : variable "profession"
# R le fait directement et recode autant de variable binaires que de n-1 variables 
mod4 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons + prof, data = smp.1)
summary(mod4)

# Nous obtenons, 7 variables crées à partir du re-codage de la variable prof.
# Le recodage est fait par ordre alphabétique et non parce que c'est intéressant ou pas ...
# Procédons au recodage de la variable "prof" en ayant pour référence la modalité "ouvrier".
smp.1$prof <- relevel(smp.1$prof, ref = "ouvrier")
mod5 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons + prof, data = smp.1)
summary(mod5)

# Avoir les différentes variables == modalités ne permet pas de répondre à la question
# Est-ce que globalement la variable profession a un effet sur la durée de l'interview:
help("drop1")
drop1(mod5, .~., test = "F")

# ---- Interaction entre deux variables explicatives ----
mod3 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons, data = smp.1)
summary(mod3)

# Synergie - potentialisation entre les deux variables 
# Il y a une relation entre trouble de sustance et depression...
# on met un * entre les deux variables
mod6 <- lm(dur.interv ~ age + dep.cons*subst.cons + scz.cons, data = smp.1)
summary(mod6)

# Il n'y a pas d'interaction entre trouble de sustance et depression sur la durée de 
# l'interview.
# Quand on met un terme d'intéraction entre 2 variables, on ne peut plus interpréter 
# les autres de coefficients.

# ---- ANalysis Of VAriance ----
# Régression linéaire où les variables explicatives sont catégorielles!
# L'analyse de variance est un cas particulier, où tous les variables explicatives sont
# catégorielles.
# Analyse de variance à un facteur
mod7 <- lm(dur.interv ~ prof, data = smp.1)
summary(mod7)

# Si on veut avoir l'effet global ...
drop1(mod7, .~., test = "F")


# ---- Conditions de validité du modèle de régression linéraire multiple ----

# ---- 1. normalité du bruit.
# ---- 2. la variance du bruit ne doit dépendre ni des valeurs de la variables à expliquer
# ni des variables explicatives.
# ---- 3. Le bruit doit etre un "vrai" bruit (pas de structure de corrélation évidente)

# Il faut vérifier les conditions de validité d'un modèle de régression 
# Normalité des résidus
mod3 <- lm(dur.interv ~ age + dep.cons + subst.cons + scz.cons, data = smp.1)
hist(resid(mod3), col = "blue", main = "Résidus")


# ---- Regression logistique ----
# Variable à expliquée binaire (y binaire)
# Ex :  risque de suicide en prison ... fonction de plusieurs variables 
# existence de mesures disciplinaires, antécédents d'abus dans l'enfance
# haut risque de suicide = a + b*durée + c*discip +  d*abus + bruit
# == > on ne peux utiliser un modèle linéaire multiple

# Regression logistique
# family = "binomial" pour le modèle logistique
mod1 <- glm(suicide.hr ~ abus, data = smp.1, family = "binomial")
summary(mod1)

# Le coeficient "b", n'est pas vraiment interpretable .... exp(b) est un odd-ratio
exp(0.7688)

# On peut le vérifier avec la fonction twoby2()
library(Epi)
twoby2(1-smp.1$suicide.hr, 1-smp.1$abus)

# L'odd-ratio et l'exp(b) de la régression logistique, sont identiques...


# ---- Modèle logistique multiple ----
mod2 <- glm(suicide.hr ~ abus + discip + duree, data = smp.1, family = "binomial")
summary(mod2)

# Les 3 variables significatifs sont significatifs.
# Les coefficients tel quel ne sont pas interprétables, il faut prendre leur exponentiels
exp(coefficients(mod2))

# ---- Conditions de validité de la régression logistique
# 1) il faut 5-10 évènements par variables explicatives (10 détenus qui ont les conditions ...).
