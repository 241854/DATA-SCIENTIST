#ISTALLAZIONE DEI PAKCKAGES

if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(dplyr, tidyr, data.table, lubridate, caret, shiny, rmarkdown,ggplot2)

#importazione dati
data_csv <- read.csv("data_impiegati.csv",sep = ",",dec=".",header = T)
view(data_csv)


#trattamento della tabella
colnames(data_csv)<- c(
  "ID_impiegato" ,
  "Età",
  "Stipendio",
  "Soddisfazione",
  "Settore",
  "Assenza",
  "Rendimento",
  "Turnover_rate"
)


#OSSERVAZIONE DEI DATI MANCANTI
Valori_mancanti <- is.na(data_csv)
Valori_mancanti


#DESCRIZIONE  STATISTICA E OSSERVAZIONE VARIABILE QUALITATIVA
summary(data_csv)

#VISUALIZZAZIONE DEI PARAMETRI QUALITATIVI
data_csv$Assenza <- as.factor(data_csv$Assenza)
data_csv$Turnover_rate <- as.factor(data_csv$Settore)
Esperienza <- data_csv$Esperienza

#visualizzazione grafica qualitativa
plot(data_csv$Assenza)
plot(data_csv$Settore)
plot(data_csv$Età, data_csv$Turnover_rate)
plot(data_csv)


#grafici con label
?plot
plot(data_csv$Assenza, data_csv$Turnover_rate, 
     main = "frequenza_Assenza vs frequenza_turnover ",  
     xlab = "freq_Assenza",                     
     ylab = "Turnover",                       
     pch = 19,                                       
     col ="blue",                                    
     cex = 0.5)  

abline(lm(data_csv$Turnover_rate ~ data_csv$Assenza), col = "red") #retta di regressione

grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted") # migliora la visione
#la legenda

legend("topleft",                                    
       legend = c("Point", "Régression"),            
       col = c("blue", "red"),                        
       pch = c(19, NA),                               
       lty = c(NA, 1))             
#vissualizazione barplot qualitativa
effettivo_Settore <- prop.table(data_csv$Settore)
effettivo_Settore
barplot(effettivo_Settore, 
        main = "numero di dipendanti da settore", 
        xlab = "Settore", 
        ylab = "Numeri di dipendenti", 
        col = "steelblue", 
        las = 2) # girare in modo orizontale

proportion_Settore <- prop.table(effettivo_Settore)

#  barchart 

bp <- barplot(proportion_Settore, 
              main = "Proportion dipendente secondo settore", 
              xlab = "Settore", 
              ylab = "Proportion_dipendenti ", 
              col = "steelblue", 
              las = 2) # girare l asse x

# etichette
text(bp, proportion_Settore, pos = 3, cex = 0.8, labels = round(proportion_Settore, 2))

#pie 
pie(proportion_Settore, 
    main = "Proportion dipendente secondo settore", 
    col = rainbow(length(effettivo_Settore)), 
    clockwise = TRUE, 
    labels = paste(round(proportion_Settore * 100), "%")) # in percentuale

# Ajouter une légende pour le diagramme
#legend("topright", 
       #legend = names(effettivo_Settore), 
       #fill = rainbow(length(effettivo_Settore)),
       #title = "settori")

#VISUALIZZAZIONE QUANTITATIVA


# identification of numeric number
vars_quantitativa <- sapply(data_csv, is.numeric)

#osservazione univariato(stipendio)
hist(data_csv$Stipendio)

hist(data_csv$Stipendio, 
     main = "Histogram stipendio", 
     xlab = "Stipendio", 
     col = "lightblue", 
     border = "black")

#creazione boxplot per stipendio
boxplot(data_csv$Stipendio)

boxplot(data_csv$Stipendio, 
        main = "Boxplot stipendio", 
        ylab = "Stipendio", 
        col = "red", 
        horizontal = F)
legend("bottomright", 
       legend = c("Histogram", "boxplot"), 
       col = c("lightblue", "red"), 
       lwd = c(2 ,2),
       cex = 0.8)

# definizione dei parametri per mettere nella stessa mappa tanti grafici 
par(mfrow = c(3, 3)

# first graphic: Histogram of 'Age'
hist(data_csv$Età, main = "Histogramme de l'età", xlab = "Age", col = "lightblue")

# second graphic: Boxplot de 'Stipendio'
boxplot(data_csv$Stipendio, main = "Boxplot Stipendi", ylab = "Salaire", col = "lightgreen")

# second graphic : Boxplot stipendio
boxplot(data_csv$Età, main = "Boxplot des Età", ylab = "Salaire", col = "lightgreen")
boxplot(data_csv$Assenza, main = "Boxplot Assenza", ylab = "Salaire", col = "lightgreen")

#osservazioni della correlazione tra variabile

matrice_correlation <- cor(data_csv)

#  heatmap 

if (!require(ggplot2)) install.packages("ggplot2")
if (!require(corrplot)) install.packages("corrplot")
data_q <- data_csv[, vars_quantitativa]
matrice_correlation <- cor(data_q, use = "complete.obs")

corrplot(matrice_correlation, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         cl.pos = "n",
         cl.cex = 1.2, 
         addCoefasPercent = TRUE, 
         number.cex = 1) 

#ANALISI ACP E MACHINE LEARNING 
#scaling and reduction of data

data_center_reduction <- scale(vars_quantitativa,center = TRUE,scale=TRUE)
?scale
```
# Installation of les packages
if (!require("FactoMineR")) install.packages("FactoMineR")
if (!require("devtools")) install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")

# Charge  packages
library(FactoMineR)
library("factoextra")

# create l'ACP
result_acp <- PCA(data_center_reduction, graph = FALSE)
print(result_acp)

## choice of number factoriel
true_value <- result_acp$eig
true_value

#osservazione del barplot per numero di componente
barplot(true_value[, 2], names.arg=2:nrow(true_value), 
        main = "percentuale della varianza spiegata da ogni componente",
        xlab = "componente principale",
        ylab = "percentuale della varianza speigata",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(true_value), true_value[, 2], 
      type="b", pch=19, col = "red")

# graphic of true_value
fviz_eig(result_acp, addlabels = TRUE)
##NB: QUI L'ACP non conta tanto



#MACHINE LEARNING


# Fissare per la reproductibilità
set.seed(42)

#split data
indexes <- sample(data_csv$, size = 100)
train <- data_csv[indexes, ]
test <- data_csv[-indexes, ]


# Model régression linear
model_lin <- lm(Stipendio ~ Settore, data = train)

# summary du model
summary(model_lin)

# Visualisation of result
ggplot(train, aes(x = Stipendio, y = Età)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Régression Linear :Stipendio vs Età ")


# Formation d'un modèle d'arbre de décision
model_tree <- tree(Species ~ ., data = train)
?tree
# Visualisation de l'arbre de décision
plot(model_tree)
text(model_tree)

# Prédiction avec l'arbre de décision
predictions <- predict(model_tree, newdata = test, type = "class")

# Matrice de confusion pour évaluer la classification
confusionMatrix(predictions, test$Species)


# Sauvegarde du modèle d'arbre
save(model_tree, file = "/Users/natachanjongwayepnga/Documents/GitHub/LeCoinStat/LesDebutsEnR/model_tree.RData")

# Sauvegarde des données d'entraînement
save(train, file = "/Users/natachanjongwayepnga/Documents/GitHub/LeCoinStat/LesDebutsEnR/train_data.RData")

