#ucitavanje potrebnih paketa:
library(pROC)
library(e1071)
library(lattice)
library(ROSE)
library(caret)
library(rpart)
library(rpart.plot)
library(gplots)


#ukljucivanje fajla u kojem se nalaze funkcije koje ce biti koriscene u svim modelima:
source("funkcije.R")

#ucitavanje dataseta:
baza<-readRDS("baza.rds")
str(baza)
summary(baza)

#vrednost za seed:
seed <- 23

#zbog velicine dataseta, da bi algoritam mogao da se izvrsi uzima se manji podskup
#kreiranje stratifikovanog random uzorka:
set.seed(seed)
indeksi_smanjeno <- createDataPartition(baza$HeartDisease, p=0.05, list = FALSE)
smanjeni_ds<- baza[indeksi_smanjeno,]

#podela dataseta na trening i test:
set.seed(seed)
indeksi<- createDataPartition(smanjeni_ds$HeartDisease, 
                                     p = 0.8, list = FALSE) 
trening <- smanjeni_ds[indeksi,]
test <- smanjeni_ds[-indeksi,]


#okvirni pregled raspodele izlazne promenljive na treningu i testu:
prop.table(table(trening$HeartDisease))
prop.table(table(test$HeartDisease))
# oko 91% opservacija pripada klasi No i na treningu i na testu, sto nije dobro i ukazuje na to da će morati da
#se vrsi balansiranje



#kreiranje PRVOG modela, sa predefinisanim vrednostima:

set.seed(seed)
drvo1 <- rpart(HeartDisease ~ ., data = trening, method = "class",
               control = rpart.control(minsplit = 20, cp = 0.01)) 



#prikaz prvog modela:
print(drvo1)
rpart.plot(drvo1, extra=106)


#kreiranje predikcija za prvi model:
predikcije1<- predict(object = drvo1, newdata = test,type = "class")

#pregled prvih nekoliko opservacija sa testa u odnosu na rezultate koje daje model:
head(predikcije1)
head(test$HeartDisease)

#crtanje ROC krive i izracunavanje AUC metrike:
drvo1.pred.prob <- predict (object = drvo1, newdata = test)
drvo1.auc <- roc.curve(test$HeartDisease, drvo1.pred.prob[,1])$auc 
drvo1.auc 
#AUC=0.5

#kreiranje matrice konfuzije:
matrica1<-table(stvarne=test$HeartDisease, predvidjene= predikcije1)
matrica1

#poziv funkcije uz importovanog fajla funkcije.R koja racuna metrike:
rez1<-izracunajMetrike(matrica1)
rez1

#REZULTATI za prvi model:
#accuracy precision    recall        F1 
#0.9146074       NaN 0.0000000       NaN  


#BALANSIRANJE TRENING PODATAKA RADI KREIRANJA DRUGOG MODELA:

ctrl <- trainControl(method = "repeatedcv", number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")
cpGrid <- expand.grid(.cp = seq(from = 0.001, to = 0.05, by = 0.001))

#DOWN-SAMPLING: 
set.seed(seed)
treningdown <- train(x = trening[,-1],
                     y = trening$HeartDisease,
                     method = "rpart",
                     metric = "ROC",
                     tuneGrid = cpGrid,
                     trControl = ctrl)


#UP-SAMPLING:
ctrl$sampling <- "up"
set.seed(seed)
treningup <- train(x = trening[,-1],
                   y = trening$HeartDisease,
                   method = "rpart",
                   metric = "ROC",
                   tuneGrid = cpGrid,
                   trControl = ctrl)


#ROSE:
ctrl$sampling <- "rose"
set.seed(seed)
treningrose <- train(x = trening[,-1],
                     y = trening$HeartDisease,
                     method = "rpart",
                     metric = "ROC",
                     tuneGrid = cpGrid,
                     trControl = ctrl)

#ORIGINALNI SKUP PODATAKA:
ctrl$sampling <- NULL
set.seed(seed)
treningoriginal <- train(x = trening[,-1],
                     y = trening$HeartDisease,
                     method = "rpart",
                     metric = "ROC",
                     tuneGrid = cpGrid,
                     trControl = ctrl)


#ctrl$sampling <- "smote"
#set.seed(13)
#treningsmote <- train(x = trening[,-1],
#                     y = trening$HeartDisease,
#                     method = "rpart",
#                     metric = "ROC",
#                     tuneGrid = cpGrid,
#                     trControl = ctrl)

#za SMOTE algoritam sve kolone moraju biti numericke, sto kod ovog dataseta nije slucaj, tako da
#ova metoda nece biti uzet u obzir, jer ne moze da se izvrsi


modeli <- list(down = treningdown,
               up = treningup,
               ROSE = treningrose,
               original = treningoriginal)

treningresampling <- resamples(modeli) 

#funkcija koja rezimira i uporedjuje balansiranе setovе podataka:
summary(treningresampling, metric = "ROC")
#najbolje vrednosti dobijene su tehnikom up-sampling, pa ce ova tehnika biti iskoriscena za balansiranje

#kreiranje izbalansiranog dataseta up-sampling metodom:
set.seed(seed)
trening_up <- upSample(x = trening[,-1], y = trening$HeartDisease)
colnames(trening_up)[17] <- "HeartDisease"

str(trening_up)
table(trening_up$HeartDisease)
#podjednak broj opservacija pripao je obema klasama izlazne varijable; dataset je izbalansiran


#kreiranje DRUGOG modela, sa optimalnom vrednoscu za cp i izbalansiranim stablom:
set.seed(seed)
drvo2 <- rpart(HeartDisease ~ ., 
               data = trening_up, 
               method = "class",
               control = rpart.control(cp = treningup$bestTune$cp)) 

#optimalno cp= 0.003

#prikaz drugog modela:
drvo2
rpart.plot(drvo2, extra=106)

#kreiranje predikcija na osnovu modela:
predikcije2<- predict(object = drvo2,newdata = test,type = "class")

#crtanje ROC krive i izracunavanje AUC metrike:
drvo2.pred.prob <- predict (object = drvo2, newdata = test)
drvo2.auc <- roc.curve(test$HeartDisease, drvo2.pred.prob[,1])$auc 
drvo2.auc 
#AUC=0.7888906

#kreiranje matrice konfuzije:
matrica2<-table(stvarne=test$HeartDisease, predvidjene= predikcije2)
matrica2

#racunanje evaluacionih metrika za drugi model:
rez2<-izracunajMetrike(matrica2)
rez2

#REZULTATI za drugi model:
#accuracy precision    recall        F1 
#0.7075383 0.2023381 0.8241758 0.3249097


#graficki prikaz ROC krivih za oba modela:
roc_zajedno <- plot(roc(test$HeartDisease, drvo1.pred.prob[,1]), print.auc = TRUE, col = "blue")
roc_zajedno <- plot(roc(test$HeartDisease, drvo2.pred.prob[,1]), print.auc = TRUE, 
                 col = "red", print.auc.y = .4, add = TRUE)


#sumarni prikaz rezultata evaluacionih metrika za oba modela:
AUC <- c(drvo1.auc,drvo2.auc)
rezultati <- data.frame(rbind(rez1, rez2), row.names = c("drvo1 ", "drvo2"))
rezultati <- cbind(rezultati, AUC)
rezultati

#         accuracy precision    recall        F1       AUC
#drvo1  0.9146074       NaN 0.0000000       NaN 0.5000000
#drvo2  0.7075383 0.2023381 0.8241758 0.3249097 0.7888906





