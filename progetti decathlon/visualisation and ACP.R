---
  title: "VISUALISATION"
format: html
editor: visual
---
  
  # DESCRIPTION OF THE DATABASE
  
  #The database contains athletes' performances at a decathlon competition. The database contains 41 lines and inlut results for various events such as 100m, long jump, shot put, etc. 
  #Each line details an athlete's performance in each event, their ranking, total points, and the name of the competition.
  
  #The CPA will identify the tests that are most strongly correlated with each other, to**determine the most influential tests for the total score**, and identify patterns or groups of events that tend to go hand in hand in determining athletic performance.
  
  #CPA will help **identify groups of athletes with similar skill profiles,** identify athletes who stand out in certain events, and to visualize the dispersion of athletes according to their versatility and their specific strengths and weaknesses.
  #This will provide insights into how individual skills and performance are distributed across all participants.
  
# PRACTICAL USES OF THE PROJECT

#1.  **Identification of Key Success Factors**: By analyzing the correlations between different events, the CPA can reveal which skills and events are most critical to success in decathlon. This can help coaches and athletes focus their training on the most influential aspects.

#2.  **Training and Competition Strategies**: By understanding the strengths and weaknesses of individual athletes, coaches can develop customized training strategies. It can also help develop competitive strategies, focusing on events where athletes are most likely to earn points.

#3.  **Athlete Selection and Recruitment**: Sport teams and organizations can use CPA results to identify athletes with specific competency profiles that match their training needs or philosophy.

#4.  **Fan and Media Engagement**: The results of the analysis can be used to create interesting narratives for fans and media, highlighting the intricacies of athlete performance and the complex dynamics of decathlon.


```{r}
# URL OF FILE
url <- "https://raw.githubusercontent.com/LeCoinStat/LeCoinStat/main/ACPAvecR/data/decathlon.txt"

# Import data
decathlon_data <- read.table(url, header = TRUE, sep = "\t")

## osservation of first and land lines
head(decathlon_data)
tail(decathlon_data)
str(decathlon_data)

## Rename columns sports competition names
colnames(decathlon_data) <-  c(
  "Course100m",     # X100m
  "SautEnLongueur", # Long.jump
  "LancerDePoids",  # Shot.put
  "SautEnHauteur",  # High.jump
  "Course400m",     # X400m
  "Course110mHaies",# X110m.hurdle
  "LancerDeDisque", # Discus
  "SautALaPerche",  # Pole.vault
  "LancerDeJavelot",# Javeline
  "Course1500m",    # X1500m
  "Classement",     # Rank
  "Points",         # Points
  "Compétition"     # Competition
)

head(decathlon_data)

##osservazione carateristiche statistiche

summary(decathlon_data)


## analysis of missing values

proportion_value_missing <- function(data) {
  nb_value_missing <- sapply(data, function(x) sum(is.na(x)))
  
  # Calculation of the proportion of missing values
  proportion_manquantes <- nb_value_missing / nrow(data)
  
  # Creating a dataframe for the result
  result <- data.frame(Nombre = nb_value_missing, Proportion = proportion_missing)
  
  return(result)
}


result <- proportion_value_missing (decathlon_data)
result


# Another option to identifive missings data( package VIM )
if (!require(dplyr)) install.packages("VIM")
library(VIM)

# Utilisation de la fonction aggr() pour visualiser les valeurs manquantes
aggr(decathlon_data, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, 
     labels=names(decathlon_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


## Description of features

# Installation  packages 
if (!require(ggplot2)) install.packages("ggplot2")

# Charger les packages
library(ggplot2)
# installation
vars_quantitatives <- sapply(decathlon_data, is.numeric)



# creation histogram
for (var in names(decathlon_data)[vars_quantitatives]) {
  print(ggplot(decathlon_data, aes_string(x = var)) +
          geom_histogram(bins = 30, fill = "blue", color = "black") +
          theme_minimal() +
          labs(title = paste("Histogramme de", var), x = var, y = "Fréquence"))
}

# boxplot

for (var in names(decathlon_data)[vars_quantitatives]) {
  print(ggplot(decathlon_data, aes_string(x = factor(1), y = var)) +
          geom_boxplot(fill = "skyblue", color = "darkblue") +
          theme_minimal() +
          labs(title = paste("Boxplot de", var), x = "", y = var))
  
  
  # barplot 
  creer_barplot_proportion <- function(data, column_name) {
    # Calculer les proportions
    proportions <- data %>%
      count(.data[[column_name]]) %>%
      mutate(Proportion = n / sum(n))
    
    # barplot
    ggplot(proportions, aes_string(x = column_name, y = "Proportion", fill = column_name)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = column_name, y = "Proportion (%)") +
      theme_minimal()
  }
  
  
  
  # step 2: correlation
  # Installation packages 
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(corrplot)) install.packages("corrplot")
  
  
  # Charge packages
  library(ggplot2)
  library(corrplot)
  
  
  donnees_quantitatives <- decathlon_data[, vars_quantitatives]
  
  
  
  #  matrix of corrélation
  matrice_correlation <- cor(donnees_quantitatives, use = "complete.obs")
  
  
  # heatmap 
  corrplot(matrice_correlation, method = "color", type = "upper", order = "hclust",
           tl.col = "black", tl.srt = 45, 
           addCoef.col = "black", # Couleur des coefficients
           cl.pos = "n", # Position de la légende de couleur
           cl.cex = 1.2, # Taille de la légende de couleur
           addCoefasPercent = TRUE, # Afficher les coefficients en pourcentage
           number.cex = 0.8) # Taille des chiffres des coefficients
  
  
  ```
  
  ### step 3: Centrer et réduire les données
  
  # scaling et reduction of data
  donnees_centrees_reduites <- scale(donnees_quantitatives,center = TRUE,scale=TRUE)
  ?scale
  
  # step 4: Réalisation of ACP 
  # Installation of packages 
  if (!require("FactoMineR")) install.packages("FactoMineR")
  if (!require("devtools")) install.packages("devtools")
  library("devtools")
  install_github("kassambara/factoextra")
  
  # load of packages
  library(FactoMineR)
  library("factoextra")
  
  # Réalise l'ACP
  result_acp <- PCA(donnees_centrees_reduites, graph = FALSE)
  
  # Afficher les résultats de l'ACP
  print(result_acp)
  
  ## number axes acp
  
  valeurspropres <- result_acp$eig
  valeurspropres
  
  barplot(valeurspropres[, 2], names.arg=1:nrow(valeurspropres), 
          main = "Pourcentage de la variance expliquée par chaque composante",
          xlab = "Composantes principales",
          ylab = "Pourcentage de variance expliquée",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(valeurspropres), valeurspropres[, 2], 
        type="b", pch=19, col = "red")
  
  #using of package factoextra
  # Create  graphic 
  fviz_eig(resultat_acp, addlabels = TRUE)
  
  ```
  
  ## correlation circle 
  
  fviz_pca_var(resultat_acp, 
               col.var = "cos2", # Utiliser la qualité de représentation (cos2) pour la couleur
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Palette de couleurs
               repel = TRUE, # Éviter le chevauchement des étiquettes
               title = "Cercle de Corrélation des Variables")
  
  
  # Create of circle of corelation
  fviz_pca_var(resultat_acp, 
               col.var = "contrib", # Utiliser la contribution
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Palette de couleurs
               repel = TRUE, # Éviter le chevauchement des étiquettes
               title = "Cercle de Corrélation des Variables")
  
  # data of persons
  head(resultat_acp$ind$coord)
  
  # Cos 2 of persons
  head(resultat_acp$ind$cos2)
  #Contribution of each person
  head(resultat_acp$ind$contrib)
  
  # Contributions of variables to PC1"
  fviz_contrib(resultat_acp, choice = "var", axes = 1, top = 3)
  # Contributions of variables to PC2
  fviz_contrib(resultat_acp, choice = "var", axes = 2, top = 10)
  
  # Cosinus carré des variables sur la première composante principale (PC1)
  fviz_cos2(resultat_acp, choice = "var", axes = 1, top = 10) +
    ggtitle("Qualité de la représentation des variables sur la PC1 (cos²)")
  
  # Cosinus carré des variables sur la deuxième composante principale (PC2)
  fviz_cos2(resultat_acp, choice = "var", axes = 2, top = 10) +
    ggtitle("Qualité de la représentation des variables sur la PC2 (cos²)")
  
  fviz_pca_ind(resultat_acp,  col.ind="cos2") +
    scale_color_gradient2(low="blue", mid="white", 
                          high="red", midpoint=0.50)+
    theme_minimal()
  
  
  # Filtrer les individus avec cos² > 50%
  ind_cos2 <- apply(resultat_acp$ind$cos2, 1, max) > 0.5
  
  # Filtrer les variables avec cos² > 50%
  var_cos2 <- apply(resultat_acp$var$cos2, 1, max) > 0.5
  
  # Creating  graphic  individus and features
  fviz_pca_biplot(resultat_acp,
                  select.ind = list(cos2 = 0.5), # Sélectionner les individus avec cos² > 50%
                  select.var = list(cos2 = 0.5), # Sélectionner les variables avec cos² > 50%
                  repel = TRUE, # Éviter le chevauchement des étiquettes
                  title = "Biplot des Individus et des Variables (cos² > 50%)",
                  col.ind = "blue", # Couleur des individus
                  col.var = "red" # Couleur des variables
  )
  ```
  
  #add upplement information
  
  resultat_acp <- PCA(decathlon_data, 
                      quanti.sup = 10,
                      quali.sup = 13, # Numéro de colonne de la variable qualitative
                      graph = TRUE)
  
  fviz_pca_ind(resultat_acp, habillage = 13,
               addEllipses =TRUE, ellipse.level = 0.68) +
    scale_color_brewer(palette="Dark2") +
    theme_minimal()
  ```
  
  # Creation of graphic
  fviz_pca_ind(resultat_acp, habillage = 13, # Utiliser la 13ème colonne pour le coloriage
               addEllipses = TRUE, ellipse.level = 0.68) +
    scale_color_brewer(palette = "Dark2") + # Palette de couleurs
    theme_minimal() + # Thème minimaliste
    ggtitle("ACP avec R") # Ajouter un titre (assurez-vous que le titre n'est pas NA/NaN)
  
  resultat_acp$quanti.sup
  
  res.desc <- dimdesc(resultat_acp, axes = c(4,5), proba = 0.05)
  # Description of dimension 1
  res.desc
