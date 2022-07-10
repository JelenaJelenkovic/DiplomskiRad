#ucitavanje potrebnih biblioteka:
library(themis) 
library(car)
library(tidyverse)
library(broom)
library(caret) 
library(pROC)

#ucitavanje fajla u kojem se nalaze potrebne funkcije:
source("funkcije.r")

#ucitavanje dataseta:
baza<-readRDS("baza.rds")
str(baza)

#zbog velicine dataseta, da bi algoritam mogao da se izvrsi uzima se manji podskup.
#kreiranje stratifikovanog random uzorka:
seed<-23
set.seed(seed)
indeksi_smanjeno <- createDataPartition(baza$HeartDisease, p=0.05, list = FALSE)
baza_smanjeno <- baza[indeksi_smanjeno,]


#smanjujem broj kategorija varijable AgeCategory radi lakseg pracenja rezultata:
levels(baza_smanjeno$AgeCategory) 
#varijabla ima 13 nivoa, bice svedena na 3:
#mlado do 30 god.
#zrelo 30-64 god.
#staro >64 god.


baza_smanjeno$ageProba<-as.integer(baza_smanjeno$AgeCategory)
baza_smanjeno$ageProba[baza_smanjeno$ageProba==1 | baza_smanjeno$ageProba==2]<-1
baza_smanjeno$ageProba[baza_smanjeno$ageProba==3 | baza_smanjeno$ageProba==4 | baza_smanjeno$ageProba==5
              | baza_smanjeno$ageProba==6 | baza_smanjeno$ageProba==7 | baza_smanjeno$ageProba==8 | baza_smanjeno$ageProba==9]<-2
baza_smanjeno$ageProba[baza_smanjeno$ageProba==10 | baza_smanjeno$ageProba==11 | baza_smanjeno$ageProba==12| baza_smanjeno$ageProba==13]<-3
summary(baza_smanjeno$ageProba)
table(baza_smanjeno$ageProba)

#sredjujem varijablu AgeCategory (vracam je sa numericke u faktorsku) i brisem pomocnu varijablu:
baza_smanjeno$AgeCategory<-baza_smanjeno$ageProba
baza_smanjeno$ageProba<-NULL

baza_smanjeno$AgeCategory<-factor(baza_smanjeno$AgeCategory,
                                  levels = 1:3,
                                  labels = c("up_to_30", "30-64", "over_64"))
class(baza_smanjeno$AgeCategory)
levels(baza_smanjeno$AgeCategory)

str(baza_smanjeno)


#podela dataseta na trening i test:
set.seed(seed)
indeksi <- createDataPartition(baza_smanjeno$HeartDisease, p=0.8, list = F)
trening <- baza_smanjeno[indeksi,]
test <- baza_smanjeno[-indeksi,]


#kreiranje PRVOG modela:
#(prvi kreiran model sa svim ukljucenim varijablama i bez sredjivanja outliera)
model1<-glm(HeartDisease ~., family="binomial", data = trening)
summary(model1)

#na osnovu poziva funkcije summary(), zakljucujem sledece:
#varijable koje potencijalno mogu da se iskljuce iz modela: 
#PhisicalHealth, MentalHealth, Rase (nemaju nijednu zvezdicu, ne uticu znacajno na izlaznu varijablu)
#kod varijable dijabetes nije znacajna kategorija postojanja dijabetesa tokom trudnoce, ali ostale 
#kategorije su znacajne, tako da ce varijabla ostati u modelu

#kreiranje verovatnoca i na osnovu njih predikcija za prvi model:
verovatnoce1 = predict(model1, newdata = test, type = "response")
predikcije1 = ifelse(verovatnoce1 < 0.5, "No", "Yes")

#pregled prvih nekoliko opservacija sa testa u odnosu na rezultate koje daje model:
head(predikcije1)
head(test$HeartDisease)

#kreiranje matrice konfuzije:
matrica1 = table (stvarne = test$HeartDisease, predvidjene = predikcije1)
matrica1

#izracunavanje metrika:
rez1<-izracunajMetrike(matrica1)
rez1

#REZULTATI za prvi model:
#accuracy precision    recall        F1 
#0.9161714 0.5333333 0.1465201 0.2298851 

AIC1 <- model1$aic 
AIC1
#5779.892
roc1 <- roc(HeartDisease ~ verovatnoce1, data = test)$auc 
roc1
# AUC1=0.8501



#ANALIZA MODELA RADI POBOLJSANJA:

#1. 
#provera linearne zavisnosti izmedju numerickih varijabli i 
#logit funkcije izlazne varijable:

#novi dataset sa samo numerickim kolonama:
novi <- trening %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(novi)

#kreiranje verovatnoca za trening skup podataka:
vrv1 <- predict(model1, type = "response")
pred1 <- ifelse(vrv1 > 0.5, "Yes", "No")
head(pred1)

novi <- novi %>%
  mutate(logit = log(vrv1/(1-vrv1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#kreiranje grafika koji proverava linearnu zavisnost:
ggplot(novi, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#sa plotova se vidi da ne postoji lin. zavisnost izmedju izlazne varijable i varijabli MentalHealth i
#PhisicalHealth, tako da ove dve varijable nece biti ukljucene u sledeci model


#2. 
#provera postojanja ekstremnih vrednosti koje mogu lose da uticu na model, koriscenjem cook's distance
#i standardized residual error:

#izdvajanje tri najvece vrednosti:
plot(model1, which = 4, id.n = 3)

#izdvajanje rezultata modela:
model.data <- augment(model1) %>% 
  mutate(index = 1:n()) 

#izdvajanje redova kod kojih je cook's distance velik: 
model.data %>% top_n(3, .cooksd)
#problematicni redovi: (63037,128692, 191414)

#da bi se proverilo koje opservacije su influential points, rade se standardizovane 
#greske reziduala (standardized residual error) - one opservacije kod kojih je ova greska > 3
#mogu praviti problem.

#grafik standardizovanih reziduala:
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = HeartDisease), alpha = .5) +
  theme_bw()

#izdvajanje redova kod kojih je std.resid > 3, koji mogu biti problem:
model.data %>% 
  filter(abs(.std.resid) > 3)

#problematicni redovi: (38890, 48742, 123198, 128692,165657, 188795,198566,238013 ,276083, 286874, 296180, 298077)

#posto je dataset dovoljno veliki, ovi redovi ce biti uklonjeni radi poboljsanja modela.

#redovi za uklanjanje:
ukloni <- model.data %>% 
  filter(abs(.std.resid) > 3) %>%
  pull(.rownames) %>%
  as.integer()
ukloni <- append(ukloni, c(63037,128692, 191414))

#3.
#proverava postojanja multikolinearnosti izmedju varijabli prediktora:
sqrt(vif(model1))


#vrednosti korena su odgovarajuce kod svih varijabli (nisu vece od 2)
#ali ce varijabla Race biti iskljucena
#iz modela, jer nema veliku prediktivnu moc, sto se vidi pozivom: summary(model1)
#ostale varijable ce ostati u modelu
#varijable nisu medjusobno korelisane
   

#sredjivanje varijable Diabetic (posto je ustanovljeno da njena kategorija: Yes (during pregnancy) 
#nema uticaja na izlaznu varijablu):

sum(trening$Diabetic=="Yes (during pregnancy)")
#ova kategorija nije znacajna jer ukupno postoje samo 103 opservacije od skoro 13000
#koje imaju tu vrednost, pa ne uticu na izlaznu varijabu

#proveravanje raspodela opservacija po kategorijama za varijablu Diabetic: 
table(baza_smanjeno$Diabetic)
table(trening$Diabetic)
table(test$Diabetic)
#kategorija "No, borderline diabetes" takodje obuhvata mali broj opservacija, pa ce
#i ona biti zamenjena sa "No", dok ce sporna kategorija "Yes (during pregnancy)" 
#biti zamenjena sa "Yes", kako bi se poboljsao model


baza_smanjeno$Diabetic[baza_smanjeno$Diabetic=="Yes (during pregnancy)"]<-"Yes"
trening$Diabetic[trening$Diabetic=="Yes (during pregnancy)"]<-"Yes"
test$Diabetic[test$Diabetic=="Yes (during pregnancy)"]<-"Yes"

baza_smanjeno$Diabetic[baza_smanjeno$Diabetic=="No, borderline diabetes"]<-"No"
trening$Diabetic[trening$Diabetic=="No, borderline diabetes"]<-"No"
test$Diabetic[test$Diabetic=="No, borderline diabetes"]<-"No"

baza_smanjeno$Diabetic<-factor(baza_smanjeno$Diabetic)
trening$Diabetic <- factor(trening$Diabetic) 
test$Diabetic <- factor(test$Diabetic) 

#proveravanje broja opservacija posle sredjivanja:
table(baza_smanjeno$Diabetic) 
table(trening$Diabetic)
table(test$Diabetic)

#kod PhysicalActivity=Yes je slicna situacija kao iznad, ali ova varijabla nije toliko 
#znacajna (sto se moglo videti i na osnovu plota u skripti sredjivanjePodataka.R),
#pa ce biti iskljucena iz modela




#kreiranje DRUGOG modela:

#ovaj model je kreiran bez varijabli: PhysicalHealth, MentalHealth, Rase i PhysicalActivity i sa uklonjenim redovima koji
#su influential data points i outlieri
#takodje varijabla dijabetes je svedena na kategorije "Yes" i "No" 

str(trening)

model2<-glm(HeartDisease ~ Smoking+AlcoholDrinking+Stroke+
              DiffWalking + Sex + AgeCategory + Diabetic +
              GenHealth + SleepTime + Asthma + KidneyDisease + SkinCancer, family="binomial", data = trening[-ukloni, ])


#proverava znacajnosti varijabli i postojanja multikolinearnosti izmedju varijabli prediktora za drugi model:
summary(model2)
sqrt(vif(model2))

#sada su sve varijable znacajne za predvidjanje vrednosti izlazne varijable i 
#nisu medjusobno lin. zavisne, tako da nadalje moze da se koristi ovaj model


#kreiranje verovatnoca i na osnovu njih predikcija za drugi model:
verovatnoce2 = predict(model2, newdata = test, type = "response")
predikcije2 = ifelse(verovatnoce2 < 0.5, "No", "Yes")

#pregled prvih nekoliko opservacija sa testa u odnosu na rezultate koje daje model:
head(predikcije2)
head(test$HeartDisease)

#kreiranje matrice konfuzije:
matrica2 = table(stvarne = test$HeartDisease, predvidjene = predikcije2)
matrica2

#izracunavanje metrika:
rez2<-izracunajMetrike(matrica2)
rez2

#REZULTATI za drugi model:
#accuracy precision    recall        F1 
#0.9155458 0.5189873 0.1501832 0.2329545 
#rezultati nisu sjajni, jer skup podataka nije izbalansiran (opet je velika tacnost, a male ostale metrike, kao 
#sto je bio slucaj i kod algoritama naivni Bajes i stablo odlucivanja)


AIC2 <- model2$aic  
AIC2
# 5785.212
roc2 <- roc(HeartDisease ~ verovatnoce2, data = test)$auc 
roc2
#0.8504



# BALANSIRANJE TRENING PODATAKA:

ctrl <- trainControl(method = "cv", number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#DOWN-SAMPLING:
set.seed(seed)
trDown <- train(x = trening[,-1],
                     y = trening$HeartDisease,
                     method = "glm",
                     family=binomial(),
                     metric = "ROC",
                     trControl = ctrl)

#UP-SAMPLING:
ctrl$sampling <- "up"
set.seed(seed)
trUp <- train(x = trening[,-1],
              y = trening$HeartDisease,
                   method = "glm",
                   family=binomial(),
                   metric = "ROC",
                   trControl = ctrl)

#ROSE:
ctrl$sampling <- "rose"
set.seed(seed)
trRose <- train(x = trening[,-1],
                y = trening$HeartDisease,
                     method = "glm",
                     family=binomial(),
                     metric = "ROC",
                     trControl = ctrl)

#ORIGINALNI SKUP PODATAKA:
ctrl$sampling <- NULL
set.seed(seed)
troriginal<- train(x = trening[,-1], 
                        y = trening$HeartDisease, 
                        method =  "glm",
                        family=binomial(),
                        metric = "ROC",
                        trControl = ctrl)

inside_models <- list(down = trDown,
                      up = trUp,
                      original = troriginal,
                      ROSE = trRose)


inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")

#ponovo nije uradjen SMOTE algoritam zbog neodgovarajucih podataka (kao i u skriptama stabloOdlucivanja.R i naivniBajes.R)

#ROSE tehnika se pokazala kao najbolja 
#kreiranje izbalansiranog dataseta ROSE metodom:
set.seed(seed)
rose_train <- ROSE(HeartDisease ~ ., data  = trening)$data                         
table(rose_train$HeartDisease) 


#kreiranje TRECEG modela sa izbalansiranim podacima:

model3<-glm(HeartDisease ~ Smoking+AlcoholDrinking+Stroke+
              DiffWalking + Sex + AgeCategory + Diabetic +
              GenHealth + SleepTime + Asthma + KidneyDisease + SkinCancer, family="binomial", data = rose_train[-ukloni, ])

#proverava znacajnosti varijabli i postojanja multikolinearnosti izmedju varijabli prediktora za treci model:
summary(model3)
sqrt(vif(model3))

#kreiranje verovatnoca i na osnovu njih predikcija za treci model:
verovatnoce3 = predict(model3, newdata = test, type = "response")
predikcije3 = ifelse(verovatnoce3 < 0.5, "No", "Yes")

#pregled prvih nekoliko opservacija sa testa u odnosu na rezultate koje daje model:
head(predikcije3)
head(test$HeartDisease)

#kreiranje matrice konfuzije:
matrica3 = table (stvarne = test$HeartDisease, predvidjene = predikcije3)
matrica3

#izracunavanje metrika
rez3<-izracunajMetrike(matrica3)
rez3

#REZULTATI za treci model:
#accuracy precision    recall        F1 
#0.7804191 0.2525952 0.8021978 0.3842105

AIC3 <- model3$aic
AIC3 
#12538.71
roc3 <- roc(test$HeartDisease,
               predict(trUp, test, type = "prob")[, "Yes"],
               levels = c("No", "Yes"))$auc
roc3 
#0.8508


#sumarni prikaz sva tri kreirana modela i njihovih metrika:
AUC <- c(roc1,roc2, roc3)
AIC <- c(AIC2, AIC2, AIC3)
df_lr_metrics <- data.frame(rbind(rez1, rez2,rez3), row.names = c("model 1 ", "model 2", "model 3"))
df_lr_metrics <- cbind(df_lr_metrics, AUC)
df_lr_metrics <- cbind(df_lr_metrics, AIC) 
df_lr_metrics

#         accuracy precision    recall        F1       AUC       AIC
#model 1  0.9161714 0.5333333 0.1465201 0.2298851 0.8501231  5785.212
#model 2  0.9155458 0.5189873 0.1501832 0.2329545 0.8504081  5785.212
#model 3  0.7804191 0.2525952 0.8021978 0.3842105 0.8507514 12538.711


#analiza znacajnosti varijabli:
logreg3var <- varImp(trRose, scale = TRUE)
logreg3var



