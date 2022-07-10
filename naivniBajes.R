#ucitavanje dataseta:
baza<-readRDS("baza.rds")

#ucitavanje fajla u kojem se nalaze potrebne funkcije:
source("funkcije.r")

#ucitavanje potrebnih biblioteka:
library(e1071)
library(bnlearn)
library(caret)
library(ggplot2)
library(pROC)
library(ROSE)

#pregled dataseta:
str(baza)
summary(baza)

#algoritam radi sa faktorskim varijablama i numerickim koje imaju normalnu raspodelu
#u skripti sredjivanjePodataka.R je proverena raspodela za sve numericke varijable i ustanovljeno je da nijedna
#ne podleze normalnoj raspodeli, tako da ce sve biti diskretizovane u narednim koracima:


numericke<-c(5,6,14)

#analiza opsega numerickih varijabli. 
summary(baza[,numericke])
#iz rezultata se vidi da su opsezi prilicno slicni: 0-30


#postupak diskretizacije prvo pokusavam tako da svaka varijabla bude podeljena na 5 intervala:
diskretizovane<- discretize(data = baza[,numericke], method = 'quantile', breaks = c(5,5,5))

#ovo daje kao rezultat gresku, uz poruku da varijable ne mogu da se podele na 5 intervala, jer
#tako daju intervale koji nemaju u sebi nijednu opservaciju

#sada kreiram grafike da bih ustanovila kako su raspodeljene varijable koje prave problem
#i na koliko intervala da ih podelim

ggplot(data = baza, mapping = aes(x = PhysicalHealth)) +
  geom_histogram(bins = 30) +
  theme_minimal()


ggplot(data = baza, mapping = aes(x = SleepTime)) +
  geom_histogram(bins = 30) +
  theme_minimal()

ggplot(data = baza, mapping = aes(x = MentalHealth)) +
  geom_histogram(bins = 20) +
  theme_minimal()

#posto nijedna varijabla ne moze diskretizacijom da se pretvori u faktorsku, to ce biti uradjeno rucno:

#physicalHealth - uzimam da je normalno (Ok) da su ispitanici imali neke vrste tegoba sa fizickim 
#zdravljem manje od 5 puta u toku mesec dana:
baza$PhysicalHealth <- ifelse(test = baza$PhysicalHealth >5,
                              yes = "NotOK", no = "OK")
unique(baza$PhysicalHealth)
table(baza$PhysicalHealth)
baza$PhysicalHealth<-as.factor(baza$PhysicalHealth)
class(baza$PhysicalHealth)
prop.table(table(baza$PhysicalHealth))


#mentalHealth - uzimam da je normalno (Ok) da su ispitanici imali neke vrste tegoba sa mentalnim 
#zdravljem manje od 5 puta u toku mesec dana:
baza$MentalHealth <- ifelse(test = baza$MentalHealth >5,
                              yes = "NotOK", no = "OK")
unique(baza$MentalHealth)
table(baza$MentalHealth)
baza$MentalHealth<-as.factor(baza$MentalHealth)
class(baza$MentalHealth)
prop.table(table(baza$MentalHealth))

#sleepTime - uzimam da je normalno (Ok) izmedju 5 i 10 sati sna dnevno:
baza$SleepTime <- ifelse(test = (baza$SleepTime >5 & baza$SleepTime <10),
                            yes = "OK", no = "NotOK")
unique(baza$SleepTime)
table(baza$SleepTime)
baza$SleepTime<-as.factor(baza$SleepTime)
class(baza$SleepTime)
prop.table(table(baza$SleepTime))

#provera da li je dataset sada pgodan za algoritam:
str(baza)
#posto su sve varijable pretvorene u faktorske, moze se preci na kreiranje modela:

#zbog velicine dataseta, da bi algoritam mogao da se izvrsi uzima se manji podskup
#kreiranje stratifikovanog random uzorka: 
seed<-23
set.seed(seed)
indeksi_smanjeno <- createDataPartition(baza$HeartDisease, p=0.05, list = FALSE)
smanjeni_ds <- baza[indeksi_smanjeno,]


#podela dataseta na trening i test:
set.seed(seed)
indeksi <- createDataPartition(smanjeni_ds$HeartDisease, p=0.8, list = F)
trening <- smanjeni_ds[indeksi,]
test <- smanjeni_ds[-indeksi,]


#KREIRANJE PRVOG MODELA MODELA
bajes1 <- naiveBayes(HeartDisease ~ ., data = trening)
print(bajes1)

#predikcije za prvi model:
predikcije1 <- predict(bajes1, newdata = test, type = 'class')

#poredjenje prvih nekoliko opservacija dobijenih predikcijama na osnovu modela i opservacija sa testa:
head(predikcije1)
head(test$HeartDisease)

#kreiranje matrice konfuzije:
matrica1<-table(stvarne=test$HeartDisease, predvidjene=predikcije1)
matrica1

#racunam evaluacione metrike za prvi model:
rez1<-izracunajMetrike(matrica1)

#REZULTATI za prvi model:
# accuracy precision    recall        F1 
#0.8761339 0.3267606 0.4249084 0.3694268 

#izracunavanje verovatnoca za izlaznu varijablu, koriscenjem prvog modela:
bajes1vrvpredikcije <- predict(bajes1, newdata = test, type = "raw")

#kreiranje ROC krive i izracunavanje AUC metrike:
bajes1roc <- roc(response = as.numeric(test$HeartDisease), 
               predictor = bajes1vrvpredikcije[,2],
               levels = c(1,2)) 

bajes1roc$auc 
#AUC=0.8492

#crtanje ROC krive na osnovu prvog modela:
plot.roc(bajes1roc,
         print.thres = TRUE,
         print.thres.best.method = "youden") 
#optimalni treshold za prvi model: 0.034


#BALANSIRANJE TRENING PODATAKA RADI KREIRANJA DRUGOG MODELA:

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#DOWN-SAMPLING:
set.seed(seed)
downtrening <- train(x = trening[,-1], 
                     y = trening$HeartDisease,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl)

#UP-SAMPLING:
ctrl$sampling <- "up"

set.seed(seed)
uptrening <- train(x = trening[,-1], 
                   y = trening$HeartDisease,
                   method = "naive_bayes",
                   metric = "ROC",
                   trControl = ctrl)

#ROSE:
ctrl$sampling <- "rose"

set.seed(seed)
rosetrening <- train(x = trening[,-1], 
                     y = trening$HeartDisease,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl)

#ORIGINALNI SKUP PODATAKA:
ctrl$sampling <- NULL


set.seed(seed)
originaltrening<- train(x = trening[,-1], 
                  y = trening$HeartDisease, 
                  method = "naive_bayes",
                  metric = "ROC",
                  trControl = ctrl)


inside_models <- list(original = originaltrening,
                      down = downtrening,
                      up = uptrening,
                      ROSE = rosetrening)

inside_resampling <- resamples(inside_models)

#funkcija koja rezimira i uporedjuje balansiranе setovе podataka:
summary(inside_resampling, metric = "ROC")

#najbolje vrednosti dobijene su tehnikom ROSE, pa ce ova tehnika biti iskoriscena za balansiranje


#kreiranje izbalansiranog dataseta ROSE metodom:
set.seed(seed)
rose_train <- ROSE(HeartDisease ~ ., data  = trening)$data                         
table(rose_train$HeartDisease) 

str(rose_train)
table(rose_train$HeartDisease)
#dataset je izbalansiran

#kreiranje DRUGOG modela sa izbalansiranim datasetom:
bajes2 <- naiveBayes(HeartDisease ~ ., data = rose_train)
print(bajes2)

#predikcije za drugi  model:
predikcije2 <- predict(bajes2, newdata = test, type = 'class')

#poredjenje prvih nekoliko opservacija dobijenih predikcijama na osnovu modela i opservacija sa testa:
head(predikcije2)
head(test$HeartDisease)

#kreiranje matrice konfuzije:
matrica2<-table(stvarne=test$HeartDisease, predvidjene=predikcije2)
matrica2

#racunanje evaluacionih metrika za drugi model:
rez2<-izracunajMetrike(matrica2)

#REZULTATI za drugi model:
#accuracy precision    recall        F1 
#0.7801063 0.2458629 0.7619048 0.3717605

#izracunavanje verovatnoca za izlaznu varijablu, koriscenjem drugog modela:
bajes2vrvpredikcije <- predict(bajes2, newdata = test, type = "raw")

#kreiranje ROC krive i izracunavanje AUC metrike:
bajes2roc <- roc(response = as.numeric(test$HeartDisease), 
                 predictor = bajes2vrvpredikcije[,2],
                 levels = c(1,2)) 

bajes2roc$auc 
#AUC=0.8499

#crtanje ROC krive na osnovu drugog modela:
plot.roc(bajes2roc,
         print.thres = TRUE,
         print.thres.best.method = "youden")
#optimalni treshold za prvi model: 0.285
#0.285 (0.688, 0.890)


#izmena tresholda i kreiranje novih predikcija sa izbalansiranim datasetom i optimalnim tresholdom:
bajes3 <- ifelse(test = bajes2vrvpredikcije[,2] >= 0.285, yes = "Yes", no = "No")
predikcije3 <- as.factor(bajes3)

#kreiranje matrice konfuzije:
matrica3<- table(stvarne = test$HeartDisease, predicted = predikcije3)
matrica3

#racunanje evaluacionih metrika sa izbalansiranim datasetom i optimalnim treshold-om:
rez3<-izracunajMetrike(matrica3)
rez3


#REZULTATI:
#accuracy precision    recall        F1 
#0.7056616 0.2105719 0.8901099 0.3405746  

#sumarni prikaz rezultata evaluacionih metrika za sva tri modela:
data.frame(rbind(rez1, rez2, rez3),
           row.names = c(paste("model ", 1:3, sep = "")))

#         accuracy precision    recall        F1
#model 1 0.8761339 0.3267606 0.4249084 0.3694268
#model 2 0.7801063 0.2458629 0.7619048 0.3717605
#model 3 0.7056616 0.2105719 0.8901099 0.3405746


#analiza znacajnosti varijabli:
bajes3var <- varImp(rosetrening, scale = TRUE)
bajes3var
