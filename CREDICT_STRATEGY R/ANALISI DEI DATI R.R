#1) IMPORTATION LIBRARY

library(tidyverse)

library(ggthemes)

library(ROSE)

library(pROC)

# Paramètres du rendu des graphiques

options(repr.plot.width=6, repr.plot.height=4)

theme_set(theme_minimal())



#--------------ESPLORAZIONE DEI DATI--------------------------


#1) IMPORTAZIONE DEI DATI

url = 'https://github.com/JosueAfouda/Credit-Risk-Modeling/raw/master/data_credit.txt'
df <- read.csv(url)
head(df)

# Structura del dataframe
str(df)

#valori mancanti
is.na(df)

# descrizione dei dati
summary(df)


#2) ESPLORAZIONE QUALITATIVA

df$loan_status <- as.factor(df$loan_status)

#frequenza  ('loan_status')

print(prop.table(table(df$loan_status)))

# barplot 'loan_status'

plot(df$loan_status, main = "loan_status")

#3) ANALISI QUANTITATIVA
# nuovole di punti tra stipendio e la durata d'attività professionale(analisi bivariata)

ggplot(df, aes(x=person_emp_length, y=loan_percent_income)) +
  geom_point(aes(color = loan_status, shape = loan_status))

#Osservazione tra loan_percent_income vs loan_status(analisi bivariata)

ggplot(df, aes(x = "", y=loan_percent_income, fill = loan_status)) +
  geom_boxplot() +
  labs(x = "", y = "loan_percent_income")

#relazione importo vs loan_status
show_relation <- function(data, feature, title_y) {
  ggplot(data, aes(x = "", y=feature, fill = loan_status)) +
    geom_boxplot() +
    labs(x = "", y = title_y)
}
show_relation(df, df$loan_amnt, "loan_amnt")

# Histogram dell'importo creditto

hist(df$loan_amnt, main = "Importo del creditto")

#si nota che la distribuzione non è normale, ci serve una distribuzione guassiana. perciò andiamo a vericare soppratutto chi non rieco a rimborsare
# Histogramme de 'loan_amnt' discrétisé par 'loan_status'

ggplot(df, aes(x=loan_amnt)) +
  geom_histogram(color="red", fill="red", bins=50) +
  facet_wrap(~ loan_status, scales="free", ncol=2)


## Nuovole dei punti

plot(df$person_income, ylab="stipendio_annuale")

# person_income vs loan_status
show_relation(subset(df, df$person_income < 100000), subset(df, df$person_income < 100000)$person_income, "person_income")

# osservare la distribuzione 
hist(df$loan_int_rate, main = "loan_int_rate")

# loan_int_rate vs loan_status
show_relation(df, df$loan_int_rate, "loan_int_rate")

## person_emp_length vs loan_status
show_relation(subset(df, df$person_emp_length < 40), 
              subset(df, df$person_emp_length < 40)$person_emp_length, 
              'person_emp_length')

# person_age vs loan_status
show_relation(df, df$person_age, 'person_age')


# person_home_ownership vs loan_status
library(dplyr)
df %>%
  ggplot(aes(x=person_home_ownership, fill = person_home_ownership)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~ loan_status, scales = "free", ncol = 2)


# Motivo di chiedere il credito 

df %>%
  ggplot(aes(x=loan_intent, fill = loan_intent)) +
  geom_bar() +
  theme_bw()
#moivo del creditto vs loan_status(target)
df %>%
  ggplot(aes(x=loan_intent, fill = loan_intent)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~ loan_status, scales = "free", ncol = 2)

##---------togliere aoulet--------------
df_clean <- df
nrow(df_clean)
#identificazione e suppresione di outlet
outliers <- df_clean  %>% filter( person_income < quantile(person_income, 0.25) - 1.5 * IQR(person_income) | person_income > quantile(person_income, 0.75) + 1.5 * IQR(person_income))                   
                                                               
# delete outlet 'person_income'
                                                               
library(dplyr)

df_clean <- anti_join(df, outliers, by = "person_income")

# Vérification : person_income

hist(df_clean$person_income, main = "person_income")


# delete outlet 'person_age'

df_clean <- subset(df_clean, df_clean$person_age < 100)


#observation of nrown 

nrow(df_clean)

#sostituizione dei valori mancanti
library(zoo)
df_clean$person_emp_length <- na.aggregate(df_clean$person_emp_length, FUN = median) 
df_clean$loan_int_rate <- na.aggregate(df_clean$loan_int_rate, FUN = median) 

# Vérification (person_emp_length)

print(summary(df_clean$person_emp_length))

# Vérifiaction (loan_int_rate)

print(summary(df_clean$loan_int_rate))


##---------preparazione dei dati per il modello

df_clean2 <- df_clean


##-------------------MACHINE LEARNING


# funzione di normalizazione

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Normalisation of data

for (col in colnames(df_clean2)) {
  if (class(df_clean2[, col]) != 'factor') {
    df_clean2[, col] <- normalize(df_clean2[, col])
  }
}

# Vérification of normalisation

head(df_clean2)

#split data
#train (80%) and test (20%) 

seed <- 131

set.seed(seed)

index_train <- sample(1:nrow(df_clean2), 0.8 * nrow(df_clean2))

train_set <- df_clean2[index_train, ]

test_set <- df_clean2[-index_train, ]


# numero di linea

print(nrow(train_set))

print(nrow(test_set))


# frequenza target in train

prop.table(table(train_set$loan_status))

# frequenza target in tes

prop.table(table(test_set$loan_status))





# Nombre d'observations de la classe majoritaire (non-défauts)

nrow(subset(train_set, train_set$loan_status == 0))

19353

# Nombre d'observations de la classe minoritaire (défauts)

nrow(subset(train_set, train_set$loan_status == 1))


# Méthode de sur-échantillonnage de la classe minoritaire (ROS)

train_oversampled <- ovun.sample(formula = loan_status ~ ., data = train_set, metho = 'over', seed = seed)

# Affichage du résultat

print(class(train_oversampled))

print(summary(train_oversampled))

# Dataframe obtenue par la méthode de sur-échantillonnage de la classe minoritaire

train_oversampled_df <- train_oversampled$data

head(train_oversampled_df)



prop.table(table(train_oversampled_df$loan_status))
# Méthode de sous-échantillonnage de la classe majoritaire (RUS)

train_undersampled <- ovun.sample(formula = loan_status ~ ., data = train_set, method = 'under', seed = seed)

# Affichage du résultat

print(class(train_undersampled))

# Dataframe obtenue par la méthode de sous-échantillonnage de la classe majoritaire

train_undersampled_df <- train_undersampled$data



prop.table(table(train_undersampled_df$loan_status))




# Combinaison des techniques ROS et RUS

train_ros_rus <- ovun.sample(formula = loan_status ~ ., data = train_set, method = 'both', N = 12000, seed =seed)

# Résultat

train_ros_rus_df <- train_ros_rus
loan_status))






##-------MODELLO REGREZZIONE



#  régression logistic

log_modeling <- function(train) {
  model <- glm(loan_status ~ ., family = 'binomial', data = train)
  return (model)
}



# costruzione del model logistica con train data

log_model <- log_modeling(train_set)

# summary of modèle

summary(log_model)



# Prédictions of test_data

preds_log <- predict(log_model, newdata = test_set, type = 'answer')

head(preds_log)




# definition of Seuil

seuil <- 0.3

# probabilità (0 ou 1) della variabile 'loan_status'

preds_status <- ifelse(preds_log > seuil, 1, 0)

# Matrix of confusion

conf_mat <- table(test_set$loan_status, preds_status)
conf_mat




# Componente della matrice of confution

TP <- conf_mat[2, 2]

TN <- conf_mat[1, 1]

FP <- conf_mat[1, 2]

FN <- conf_mat[2, 1]


# Score of classification

accuracy <- (TP+TN) / nrow(test_set)

accuracy


#sensibilità of  modello

sensitivity <- TP / (FN + TP)

sensitivity

# Specificità du modèle

specificity <- TN / (TN + FP)

specificity


# evaluation of model

model_evaluation <- function(Model, Seuil) {
  
  predictions <- predict(Model, newdata = test_set, type = 'response')
  predicted_status <- ifelse(predictions > Seuil, 1, 0)
  Conf_Mat <- table(test_set$loan_status, predicted_status)
  Accuracy <- (Conf_Mat[2,2] + Conf_Mat[1,1]) / nrow(test_set)
  Sensitivity <- Conf_Mat[2,2] / (Conf_Mat[2,2] + Conf_Mat[2,1])
  Specificity <- Conf_Mat[1,1] / (Conf_Mat[1,1] + Conf_Mat[1,2])
  One_minus_spec <- 1 - Specificity
  
  results <- list(Conf_Mat, Accuracy, Sensitivity, Specificity, One_minus_spec)
  names(results) <- c('Matrice de confusion', 'Score de classification', 
                      'Sensibilité du modèle', 'Spécificité du modèle', 
                      'One minus Specificity')
  
  return (results)
}


#Vérification de la fonction

model_evaluation(log_model, 0.3)




print(summary(train_undersampled))





# vissualisation of model

print_results <- function(model) {
  
  # définition des seuils
  seuils <- seq(0.01, 0.99, by = 0.01)
  
  # Vecteurs vides pour stocker les métriques
  acc_model <- c()
  sens_model <- c()
  spec_model <- c()
  one_minus_spec_model <- c()
  
  for (i in seuils) {
    r <- model_evaluation(model, i)
    acc_model <- append(acc_model, r[['Score de classification']])
    sens_model <- append(sens_model, r[['Sensibilité du modèle']])
    spec_model <- append(spec_model, r[['Spécificité du modèle']])
    one_minus_spec_model <- append(one_minus_spec_model, r[['One minus Specificity']])
  }
  
  # Dataframe des métriques pour divers seuils
  resultats <- data.frame(cbind(seuils, acc_model, sens_model, spec_model, one_minus_spec_model))
  
  # Graphique montrant les métriques pour différents seuils
  plots <- ggplot() + 
    geom_line(data=resultats, aes(x=seuils, y=acc_model, colour="red")) + 
    geom_line(data=resultats, aes(x=seuils, y=sens_model,colour="green")) +
    geom_line(data=resultats, aes(x=seuils, y=spec_model,colour="blue")) +
    labs(x = 'Seuil', y = 'Score') +
    scale_color_discrete(name = "Métriques", labels = c("Accuracy", "Sensitivity", "Specificity"))

  # Courbe ROC
  roc_curve <- ggplot() +
    geom_line(data=resultats, aes(x=one_minus_spec_model, y=sens_model)) +
    labs(x = '1 - Specificity', y = 'Sensitivity')
  
  # Résultats
  all_results <- list(resultats, plots, roc_curve)
  names(all_results) <- c('Metrics', 'Plot', 'ROC Curve')
  
  return (all_results)
}


# Métriques de log_model for seuils 

head(print_results(log_model)[['Metrics']])


tail(print_results(log_model)[['Metrics']])



# Graphic montrant  log_model 

print_results(log_model)[['Plot']]

# Courb ROC of log_model

print_results(log_model)[['ROC Curve']]




# AUC vs seuils of log_model

auc_model <- function(model) {
  predictions <- predict(model, newdata = test_set, type = "response")
  return (auc(roc(test_set$loan_status, predictions)))
}


auc_model(log_model)



# automatically  (forward stepwise regression)

null_model <- glm(loan_status ~ 1, data = train_set, family = 'binomial')

forward_model <- step(null_model, 
                      scope = list(lower = null_model, upper = log_model), 
                      direction = 'forward', 
                      steps = 2000)




# Metrix of forward_model 

print_results(forward_model)[['Plot']]


# Courb ROC de forward_model

print_results(forward_model)[['ROC Curve']]

# Métriques de log_model_ros of various seuils

print_results(log_model_ros)[['Plot']]


# Courb ROC de log_model_rus

print_results(log_model_rus)[['ROC Curve']]


# Métriques de log_model_ros_rus pour divers seuils

print_results(log_model_ros_rus)[['Plot']]



