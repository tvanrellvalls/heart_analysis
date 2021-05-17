library(dplyr)
library(tidyr)

# Carreguem el fitxer "SFO.csv"
data <- read.csv('heart.csv')

colnames(data) <- c('age', 'sex', 'chestPain', 'bloodPressure', 'cholesterol', 'bloodSugar', 'restecg',
                         'maxHeartRate', 'indAngina', 'stDepression', 'stSlope', 'numVessels', 'scintigraphy', 'target')

# Resum de les variables categòriques i numèriques
summary(data)

# Observem una mostra prèvia del dataset
head(data,5)

# Anàlisi de valors nuls
sapply(data, function(x) sum(is.na(x)))

# En cas de valors nuls en una fila d'una columna concreta l'eliminariem així:
# data <- data[!is.na(data$columna),]

#
data2 <- data %>% mutate(
  sex = if_else(sex == 1, "Male", "Female"),
  bloodSugar = ifelse(bloodSugar == 1, '> 120 mg/dl', '< 120 mg/dl'),
  chestPain = ifelse(chestPain == 1,'Typical Angina',
              ifelse(chestPain == 2, 'Atypical Angina',
              ifelse(chestPain == 3,'Non-Anginal Pain', 'Asymptomatic'))),
  
  target = if_else(target == 1, "YES", "NO")
  ) %>% mutate_if(is.character, as.factor)


summary(data2)


# https://www.kaggle.com/icaronunes/doctor-machine-learning

# https://www.kaggle.com/ekrembayar/heart-disease-uci-eda-pca-kmeans-hc-rf-with-r




