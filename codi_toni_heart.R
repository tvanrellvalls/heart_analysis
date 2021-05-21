library(dplyr)
library(tidyr)
##### 1 ######
# Carreguem el fitxer "SFO.csv"
data <- read.csv('heart.csv')

colnames(data) <- c('age', 'sex', 'chestPain', 'bloodPressure', 'cholesterol', 'bloodSugar', 'restecg',
                         'maxHeartRate', 'indAngina', 'stDepression', 'stSlope', 'numVessels', 'scintigraphy', 'target')

# Resum de les variables categ?riques i num?riques
summary(data)

# Observem una mostra pr?via del dataset
head(data,5)
###### 2 ########
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

##### 3.1 ######

# An?lisi de valors nuls
sapply(data2, function(x) sum(is.na(x)))

# No n'hi ha, explicar que els fariem per imputació probabilística

##### 3.2 ######
boxplot(data2$bloodPressure)
bprq <- quantile(data2$bloodPressure, 0.75) - quantile(data2$bloodPressure, 0.25)
sum(data2$bloodPressure > median(data2$bloodPressure)+3*bprq)
boxplot(data2$cholesterol)
chorq <- quantile(data2$cholesterol, 0.75) - quantile(data2$cholesterol, 0.25)
sum(data2$cholesterol > median(data2$cholesterol)+3*chorq)
boxplot(data2$maxHeartRate)
mhrrq <- quantile(data2$maxHeartRate, 0.75) - quantile(data2$maxHeartRate, 0.25)
sum(data2$maxHeartRate < median(data2$maxHeartRate)-3*mhrrq)

# Valors atípics són possibles i per tant els deixam com estan.

#### Exportació #####
heart <- data2
write.csv(heart,"heart_clean.csv")


##### 4.1 ######
# Explicam que farem:
# Dades quantitatives correlació
# Dades qualitatives test chi quadrat
# Regressió logística (model)

##### 4.2 #####
# Shapiro-Wilks (Normalitat)
normtest <- data.frame(matrix(ncol = 2, nrow = 4))
colnames(normtest) <- c("Nom", "Pvalor")
normtest[1,1]<-"Edat"
normtest[2,1]<-"Pressió"
normtest[3,1]<-"Colesterol"
normtest[4,1]<-"Freqüència"
normtest[1,2]<-shapiro.test(heart$age)$p.value
normtest[2,2]<-shapiro.test(heart$bloodPressure)$p.value
normtest[3,2]<-shapiro.test(heart$cholesterol)$p.value
normtest[4,2]<-shapiro.test(heart$maxHeartRate)$p.value
hist(heart$age)
hist(heart$bloodPressure)
hist(heart$cholesterol)
hist(heart$maxHeartRate)

library(HH)
hov(heart$age ~ heart$target) # No hi ha homogeneïtat
hov(heart$bloodPressure ~ heart$target) # Hi ha homogeneïtat
hov(heart$cholesterol ~ heart$target) # Hi ha homogeneïtat
hov(heart$maxHeartRate ~ heart$target) # No hi ha homogeneïtat

####### 4.3 ######
##### Correlació 
pvalorcor<-c(1:6)
cor(heart[,c(1,4,5,7)], method = "kendall")
c<-1
for(i in c(1,4,5,7)){
  for(j in c(1,4,5,7)){

    if(i!=j & i<j ){ pvalorcor[c] <- cor.test(heart[,i],heart[,j], method = "kendall")$p.value
               c <- c+1 }
  }
}
Varcor <- c("Age-blood","Age-Cholesterol","Age-HeartRate","blood-cholesterol","blood-heartRate", "cholesterol-heartRate")
cordf <- data.frame(Varcor, pvalorcor)

# Hi ha relació però es tan baixa que no podem parlar multicolinealitat

#### Chi-quadrat
pvalorchi <- c(1:4)
c <- 1
for(i in c(2,3,6,8)){ 
  pvalorchi[c] <- chisq.test(heart$target,heart[,i])$p.value
  c <- c+1
  }
Varchi <- c("Target-Sex","Target-ChestPain","Target-Bloodsugar","Target-IndAngina")
chidf <- data.frame(Varchi, pvalorchi )

# Bloodsugar és l'única variable significativament independent

#### Regressió logística
mod1 <- glm(target~., data = heart, family = "binomial")
summary(mod1)
# Treiem bloodsugar perquè ho hem vist abans i aquí ens diu que no és significatiu 
#i l'edat perquè ens indica no significació
# Té sentit amb el nostre anàlisi de correlació doncs l'edat té relació 
#amb la resta de variables quantitatives (no importa l'edat sinó lo cascat que estàs)
mod2 <- glm(target~.-bloodSugar, data = heart, family = "binomial")
summary(mod2)
library(pROC)
prob_c1=predict(mod1, heart, type="response")
r_c1=roc(heart$target,prob_c1, data=heart)
plot (r_c, main= 'Corba ROC del model logístic major AIC')
r_c1


prob_c2=predict(mod2, heart, type="response")
r_c2=roc(heart$target,prob_c2, data=heart)
plot (r_c2, main= 'Corba ROC del model logístic menor AIC')
r_c2


# https://www.kaggle.com/icaronunes/doctor-machine-learning

# https://www.kaggle.com/ekrembayar/heart-disease-uci-eda-pca-kmeans-hc-rf-with-r




