###PREAMBULE
#Installation des packages necessaires aux GLMM (generalized linear mixed models)
install.packages("lme4")
library(lme4)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(cowplot)

###DATA
#Chargement des données (il faut mettre le raccourci du dossier dans lequel est situé ton fichier csv)
data <- as.data.frame(read.csv("C:/Users/reyne/Desktop/MaxMath/ProjManip10.csv",sep=";"),headers=TRUE)
#On renomme les colonnes car il y a un bug sur la première
colnames(data) <- c("Mice","Score","Group","Day","Cage")
#On transforme les données de "catégorie" comme la cage, le traitement ou les souris avec as.factor pour qu'elles soient bien traitées comme catégories (factors) et non autre chose (numrical)
data$Mice <- as.factor(data$Mice)
data$Group <- as.factor(data$Group)
data$Cage <- as.factor(data$Cage)
#On centre la variable de temps "Day" que l'on veut considerer comme une variable continue et non catégorie pour des raisons stats (+ de pouvoir stat comme ça).
#On la centre pour éviter une inflation de la variance (si je ne m'abuse c'est ça) + c'est quasi toujours ce qui est fait en analyse data
data$Day <- data$Day-mean(data$Day)
#On peut juste regarder nos données pour s'assurer que tout est ok
head(data)
#On retire la souris 1_1 car mauvaise injection
data<-data[!(data$Mice=="1_1"),]

###MODEL
#GLM avec données booléennes (1 ou 0)
model <- glmer(Score ~ Group+Day+(Day|Mice), data=data, family="binomial")
#On call summary pour voir tous les résultats stats de notre modèle
summary(model)

###TESTING MODEL'S ASSUMPTIONS
#Lors d'une regression linéaire "standard" on doit vérifier que les "residuals" sont gaussiens.
#Dans le cas d'un GLMM, il faut vérifier si les "residuals" sont gaussiens mais cela est plus complexe. Pour cela on va utiliser un autre package.
#https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
#le site donne des explications sur le pourquoi du comment + le package
install.packages("DHARMa") #c long c normal
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
residuals(simulationOutput)
plot(simulationOutput)
#Voir le lien ci-dessus pour l'explication des figures
#On peut aussi faire des tests stats en plus (décris sur le lien)


###ET VOILA
#Attention les valeurs de la GLMM ne sont pas à considérer directement de la sorte elles doivent être retransformées (car elles sont passées par la fonction logit donc utiliser plogis pour avoir les "vraies valeurs")
#example:
plogis(1.29879)


###GLMM avec les MEANS
datamean <- as.data.frame(read.csv("C:/Users/reyne/Desktop/MaxMath/ProjMean.csv",sep=";"),headers=TRUE)
colnames(datamean) <- c("Mice","MeanScore","Day","Group")
#On retire la souris 1_1 car mauvaise injection
datamean<-datamean[!(datamean$Mice=="1_1"),]

model <- glmer(MeanScore ~ Group+Day+(Day|Mice), data=datamean, family="binomial")
#On call summary pour voir tous les résultats stats de notre modèle
summary(model)


###DE BEAUX PLOTS AVEC LES MOYENNES (car on peut pas plot les 0/1) :) 

l <- ggplot(datamean, aes(x = factor(Day), y = MeanScore, color = Mice, group=Mice)) +
  geom_line() + ggtitle("Mice learning score accross the days") +
  xlab("Sessions") + ylab("Mean score per session") + theme_bw() 

p <- ggplot(datamean, aes(x = factor(Day), y = MeanScore, color = Mice, group=Mice)) +
   geom_point() +
  xlab("Sessions") + ylab("Mean score per session") + theme_bw() + aes(color = Group) + geom_smooth(method = "glm", se = FALSE) + 
  scale_colour_manual(values = c("#FF3333","#0066FF"))
save_plot("SWMGLMMMeans.jpeg",p, dpi=700)

g <- ggplot(datamean, aes(x = factor(Day), y = MeanScore, color = Mice, group=Mice)) + 
  geom_line() + 
  xlab("Sessions") + 
  ylab("Mean score per session") + 
  theme_bw()  + 
  stat_summary(aes(group=Group), fun=mean, geom = "pointrange", size = 0.2,
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)), colour="black") + 
  stat_summary(aes(group=Group),fun = "mean", geom = "line", colour="black", linewidth=1) + 
  facet_grid(cols = vars(Group)) + theme(
    strip.background = element_rect(fill = "white")
  )

save_plot("SWMGroups.jpeg",g, dpi=700)

comp <- ggplot(datamean, aes(x = factor(Day), y = MeanScore, color = Mice, group=Mice)) + 
  xlab("Sessions") + 
  ylab("Mean score per session") + 
  theme_bw()  + 
  stat_summary(aes(color=paste("mean", Group), group=Group), fun=mean, geom = "pointrange", size = 0.2,
               fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - sd(x) / sqrt(length(x))) + 
  stat_summary(aes(color=paste("mean", Group), group=Group),fun = "mean", geom = "line", linewidth=1, ) + 
  scale_colour_manual(values = c("#FF3333","#0066FF"))

save_plot("SWMMeans.jpeg",comp, dpi=700)

p.adjust(0.01230,method="bonferroni",3)
plogis(0.12656)
