library(bslib)
library(dplyr)
library(pROC)
library(HH)
library(corrplot)

# Carreguem el fitxer "SFO.csv"
data <- read.csv('heart.csv')
colnames(data) <- c('age', 'sex', 'chestPain', 'bloodPressure',
                    'cholesterol', 'bloodSugar', 'restecg',
                    'maxHeartRate', 'indAngina', 'stDepression', 
                    'stSlope', 'numVessels', 'scintigraphy', 'target')

## 2. Recodifiquem i seleccionem variables d'interés per la nostra anàlisi
data2 <- data %>% mutate(
  sex = if_else(sex == 1, "Male", "Female"),
  bloodSugar = ifelse(bloodSugar == 1, '> 120 mg/dl', '< 120 mg/dl'),
  chestPain = ifelse(chestPain == 1,'Typical Angina',
                     ifelse(chestPain == 2, 'Atypical Angina',
                            ifelse(chestPain == 3,'Non-Anginal Pain', 'Asymptomatic'))),
  indAngina = ifelse(indAngina == 0,'No','Yes'),
  target = if_else(target == 1, "YES", "NO")
) %>% mutate_if(is.character, as.factor)
data2 <- data2[,c(1:6,8,9,14)]
summary(data2)


## 3. Neteja de les dades.

### 3.1. Valors nuls
sapply(data2, function(x) sum(is.na(x)))

### 3.2. Valors atípics.
boxplot(data2$age)
bpage <- quantile(data2$age, 0.75) - quantile(data2$age, 0.25)
sum(data2$age > median(data2$age)+3*bpage)
boxplot(data2$bloodPressure)
bprq <- quantile(data2$bloodPressure, 0.75) - quantile(data2$bloodPressure, 0.25)
sum(data2$bloodPressure > median(data2$bloodPressure)+3*bprq)
boxplot(data2$cholesterol)
chorq <- quantile(data2$cholesterol, 0.75) - quantile(data2$cholesterol, 0.25)
sum(data2$cholesterol > median(data2$cholesterol)+3*chorq)
boxplot(data2$maxHeartRate)
mhrrq <- quantile(data2$maxHeartRate, 0.75) - quantile(data2$maxHeartRate, 0.25)
sum(data2$maxHeartRate < median(data2$maxHeartRate)-3*mhrrq)

#### Exportació #####
heart <- data2
write.csv(heart,"heart_clean.csv")


## 4. Anàlisi de les dades.

### 4.2. Normalitat i Homogeneïtat

# Shapiro-Wilks (Normalitat)
normtest <- data.frame(matrix(ncol = 2, nrow = 4))
colnames(normtest) <- c("Nom", "Pvalor")
normtest[1,1]<-"Edat"
normtest[2,1]<-"Pressió"
normtest[3,1]<-"Colesterol"
normtest[4,1]<-"Freqüència"
normtest[1,2]<-shapiro.test(heart$age)$p.value # No hi ha normalitat
normtest[2,2]<-shapiro.test(heart$bloodPressure)$p.value  # No hi ha normalitat
normtest[3,2]<-shapiro.test(heart$cholesterol)$p.value  # No hi ha normalitat
normtest[4,2]<-shapiro.test(heart$maxHeartRate)$p.value  # No hi ha normalitat
normtest

hist(heart$age)
hist(heart$bloodPressure)
hist(heart$cholesterol)
hist(heart$maxHeartRate)

hov(heart$age ~ heart$target) # No hi ha homogeneïtat
hov(heart$bloodPressure ~ heart$target) # Hi ha homogeneïtat
hov(heart$cholesterol ~ heart$target) # Hi ha homogeneïtat
hov(heart$maxHeartRate ~ heart$target) # No hi ha homogeneïtat

### 4.3 Tres mètodes d'anàlisi diferents.

#### 4.3.1 Correlació entre les variables numèriques.
pvalorcor<-c(1:6)
mcor <- cor(heart[,c(1,4,5,7)], method = "kendall")
c<-1
for(i in c(1,4,5,7)){
  for(j in c(1,4,5,7)){
    
    if(i!=j & i<j ){ pvalorcor[c] <- cor.test(heart[,i],heart[,j], method = "kendall")$p.value
    c <- c+1 }
  }
}
Varcor <- c("Age-bloodPressure","Age-Cholesterol","Age-HeartRate","bloodPressure-cholesterol","bloodPressure-heartRate", "cholesterol-heartRate")
cordf <- data.frame(Varcor, pvalorcor)
cordf

#### 4.3.2 Test Chi-quadrat
pvalor_chi_quadrat <- c(1:4)
c <- 1
for(i in c(2,3,6,8)){ 
  pvalor_chi_quadrat[c] <- chisq.test(heart$target,heart[,i])$p.value
  c <- c+1
}
Variables <- c("Target-Sex","Target-ChestPain","Target-Bloodsugar","Target-IndAngina")
chidf <- data.frame(Variables, pvalor_chi_quadrat )
chidf

#### 4.3.3 Regressió Logística
model1 <- glm(target~ ., data = heart, family = "binomial")
summary(model1)
#predict(object=model1, newdata=heart_test_vars)

model2 <- glm(target~.-bloodSugar - age, data = heart, family = "binomial")
summary(model2)


## 5. Representació dels resultats a partir de taules i gràfiques

### 5.1 Matriu de correlació
corrplot(mcor)

### 5.2 Taules de contingència test Chi-quadrat

# Target-Sex:
Target_Sex = table(heart$sex,heart$target) 
print(Target_Sex)

# Target-ChestPain:
Target_ChestPain = table(heart$chestPain,heart$target) 
print(Target_ChestPain)

# Target-Bloodsugar:
Target_Bloodsugar = table(heart$bloodSugar,heart$target) 
print(Target_Bloodsugar)

# Target-IndAngina:
Target_IndAngina = table(heart$indAngina,heart$target) 
print(Target_IndAngina)

### 5.3 Corba ROC
prob = predict(model2, heart, type="response")
roc_model2 = roc(heart$target, prob, data=heart)
plot (roc_model2, main= 'Corba ROC del model logístic 2')
roc_model2