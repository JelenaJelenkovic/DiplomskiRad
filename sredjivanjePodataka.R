#ucitavanje skupa podataka u csv formatu
baza<-read.csv("heart_2020_cleaned.csv", stringsAsFactors = FALSE)

#provera strukture dataseta
str(baza)
summary(baza)


#analiza i sredjivanje izlazne varijable
summary(baza$HeartDisease)
table(baza$HeartDisease)
baza$HeartDisease<-factor(baza$HeartDisease)
class(baza$HeartDisease)
prop.table(summary(baza$HeartDisease))

#proveravanje da li numericke varijable podlezu normalnoj raspodeli
qqnorm(baza$BMI)
qqnorm(baza$PhysicalHealth)
qqnorm(baza$MentalHealth)
qqnorm(baza$SleepTime)

#jos neke provere strukture podataka ostalih varijabli
unique(baza$Diabetic)
summary(baza$AgeCategory)
str(baza$AgeCategory)
min(baza$SleepTime)
max(baza$SleepTime)
unique(baza$GenHealth)
table(baza$GenHealth)


#provera postojanja neodgovarajucih vrednosti kod character varijabli 
sum(baza$Smoking=="" | baza$Smoking==" " | baza$Smoking=="-" | baza$Smoking=="." | baza$Smoking=="*"  )
karakter<-c(3:5,8:14,16:18)

apply(baza[,karakter],2, function(x)
sum(x=="" | x==" " | x=="-" | x=="." | x=="*"  ))


#pretvaranje character varijabli u faktorske
#3,4,5 8 9 10 11 12 13 14 16 17 18

baza$Smoking<-as.factor(baza$Smoking)
class(baza$Smoking)
baza$AlcoholDrinking<-as.factor(baza$AlcoholDrinking)
class(baza$AlcoholDrinking)
baza$Stroke<-as.factor(baza$Stroke)
class(baza$Stroke)
baza$DiffWalking<-as.factor(baza$DiffWalking)
class(baza$DiffWalking)
baza$Sex<-as.factor(baza$Sex)
class(baza$Sex)
baza$AgeCategory<-as.factor(baza$AgeCategory)
class(baza$AgeCategory)
baza$AgeCategory<-as.factor(baza$AgeCategory)
class(baza$AgeCategory)
baza$Race<-as.factor(baza$Race)
class(baza$Race)
baza$Diabetic<-as.factor(baza$Diabetic)
class(baza$Diabetic)
baza$PhysicalActivity<-as.factor(baza$PhysicalActivity)
class(baza$PhysicalActivity)
baza$GenHealth<-as.factor(baza$GenHealth)
class(baza$GenHealth)
baza$Asthma<-as.factor(baza$Asthma)
class(baza$Asthma)
baza$KidneyDisease<-as.factor(baza$KidneyDisease)
class(baza$KidneyDisease)
baza$SkinCancer<-as.factor(baza$SkinCancer)
class(baza$SkinCancer)

#provera da li su sve varijable promenjene
str(baza)

#analiza povezanosti varijabli sa izlaznom varijablom

#POL
xtabs(~Sex + HeartDisease, data = baza) #nema preterane razlike u polovima

#DIJABETES
xtabs(~Diabetic + HeartDisease, data = baza) 

#OPSTE ZDRAVLJE
xtabs(~GenHealth + HeartDisease, data = baza) #ljudi su otprilike dobro procenili svoje zdravlje

#PUSENJE
xtabs(~Smoking + HeartDisease, data = baza) #oni koji puse i imaju srcano oboljenje oko 16 000

#ALKOHOL
xtabs(~AlcoholDrinking + HeartDisease, data = baza) 

#MOZDANI UDAR
xtabs(~Stroke + HeartDisease, data = baza) 

#ASTMA
xtabs(~Asthma + HeartDisease, data = baza) 

#PLOTOVI UTICAJA FAKTORSKIH VARIJABLI NA IZLAZNU VARIJABLU

library(ggplot2)
plotSMOKING <- ggplot(baza, aes(x = Smoking, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("Smoking") +
  theme_bw()
plotSMOKING


plotalc <- ggplot(baza, aes(x = AlcoholDrinking, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("AlcoholDrinking") +
  theme_bw()
plotalc

plotSTR <- ggplot(baza, aes(x = Stroke, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("Stroke") +
  theme_bw()
plotSTR

plotWALK <- ggplot(baza, aes(x = DiffWalking, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("DiffWalking") +
  theme_bw()
plotWALK


plotage <- ggplot(baza, aes(x = AgeCategory, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("AgeCategory") +
  theme_bw()
plotage


plotdiabetes <- ggplot(baza, aes(x = Diabetic, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("Diabetic") +
  theme_bw()
plotdiabetes


plotbubrezi <- ggplot(baza, aes(x = KidneyDisease, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("KidneyDisease") +
  theme_bw()
plotbubrezi




plotAsthma <- ggplot(baza, aes(x = Asthma, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("Asthma") +
  theme_bw()
plotAsthma


plotPhysicalActivity <- ggplot(baza, aes(x = PhysicalActivity, fill=HeartDisease)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Bolesti srca") +
  xlab("PhysicalActivity") +
  theme_bw()
plotPhysicalActivity


#PLOTOVI UTICAJA NUMERICKIH VARIJABLI NA IZLAZNU VARIJABLU

plotBMI <- ggplot(data = baza,
              mapping = aes(x = BMI, fill=HeartDisease)) +
  geom_density(alpha = 0.5) +
  theme_minimal()
plotBMI


plotPhysicalHealth <- ggplot(data = baza,
                  mapping = aes(x = PhysicalHealth, fill=HeartDisease)) +
  geom_density(alpha = 0.5) +
  theme_minimal()
plotPhysicalHealth


plotMentalHealth <- ggplot(data = baza,
                             mapping = aes(x = MentalHealth, fill=HeartDisease)) +
  geom_density(alpha = 0.5) +
  theme_minimal()
plotMentalHealth


plotSleepTime <- ggplot(data = baza,
                           mapping = aes(x = SleepTime, fill=HeartDisease)) +
  geom_density(alpha = 0.5) +
  theme_minimal()
plotSleepTime


#PLOTOVI POVEZANOSTI KATEGORICKIH VARIJABLI SA IZLAZNOM U %

#Smoking
library(ggplot2)
plotsmoking <- ggplot(baza, aes(x = Smoking, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nSmoking") +
  theme_bw()
plotsmoking


#AlcoholDrinking
plotalc <- ggplot(baza, aes(x = AlcoholDrinking, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nAlkohol") +
  theme_bw()
plotalc

#Stroke
plotstroke <- ggplot(baza, aes(x = Stroke, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nStroke") +
  theme_bw()
plotstroke

#DiffWalking
plotdiffwalk <- ggplot(baza, aes(x =DiffWalking, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nDiffWalking") +
  theme_bw()
plotdiffwalk

#PhisycalActivity
plotphisicala <- ggplot(baza, aes(x =PhysicalActivity, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nPhysicalActivity") +
  theme_bw()
plotphisicala


#Asthma
plotastma <- ggplot(baza, aes(x =Asthma, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nAsthma") +
  theme_bw()
plotastma

#KidenyDisease
plotKidney <- ggplot(baza, aes(x =KidneyDisease, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nKidneyDisease") +
  theme_bw()
plotKidney

#SkinCancer
plotSkin <- ggplot(baza, aes(x =SkinCancer, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nSkinCancer") +
  theme_bw()
plotSkin

#Sex
plotSex <- ggplot(baza, aes(x =Sex, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nPol") +
  theme_bw()
plotSex

#AgeCategory
plotAge <- ggplot(baza, aes(x =AgeCategory, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nGodine") +
  theme_bw()
plotAge

#Race
plotRace <- ggplot(baza, aes(x =Race, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nRasna pripadnost") +
  theme_bw()
plotRace

#Diabetic
plotDiab <- ggplot(baza, aes(x =Diabetic, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nDijabetes") +
  theme_bw()
plotDiab

#GenHealth
plotgeneral <- ggplot(baza, aes(x =GenHealth, fill = HeartDisease)) +
  geom_bar(position = "fill", width = 0.45) + 
  ylab("Procenat obolelih\n") +
  xlab("\nGenHealth") +
  theme_bw()
plotgeneral



#PRVERA KORELACIJE KATEGORICKIH VARIJABLI SA IZLAZNOM VARIJABLOM PRIMENOM X-KVADRAT TESTA:

#H0: Varijable su medjusobno nezavisne
#H1: Varijable su medjusobno zavisne

#Ako je p<0.05 odbacuje se nulta hipoteza---->varijable su zavisne


#Smoking
chisq.test(baza$HeartDisease, baza$Smoking, correct=FALSE)
#zavisne


#AlcoholDrinking
chisq.test(baza$HeartDisease, baza$AlcoholDrinking, correct=FALSE)
#zavisne

#Stroke
chisq.test(baza$HeartDisease, baza$Stroke, correct=FALSE)
#zavisne

#DiffWalking
chisq.test(baza$HeartDisease, baza$DiffWalking, correct=FALSE)
#zavisne

#PhisycalActivity
chisq.test(baza$HeartDisease, baza$PhysicalActivity, correct=FALSE)
#zavisne

#Asthma
chisq.test(baza$HeartDisease, baza$Asthma, correct=FALSE)
#zavisne

#KidenyDisease
chisq.test(baza$HeartDisease, baza$KidneyDisease, correct=FALSE)
#zavisne

#SkinCancer
chisq.test(baza$HeartDisease, baza$SkinCancer, correct=FALSE)
#zavisne

#Sex
chisq.test(baza$HeartDisease, baza$Sex, correct=FALSE)
#zavisne

#AgeCategory
chisq.test(baza$HeartDisease, baza$AgeCategory, correct=FALSE)
#zavisne

#Race
chisq.test(baza$HeartDisease, baza$Race, correct=FALSE)
#zavisne

#Diabetic
chisq.test(baza$HeartDisease, baza$Diabetic, correct=FALSE)
#zavisne

#GenHealth
chisq.test(baza$HeartDisease, baza$GenHealth, correct=FALSE)
#zavisne


install.packages("rcompanion")
library(rcompanion)

kruskal.test(HeartDisease ~ BMI, data = baza)
epsilonSquared(x = baza$HeartDisease, g = baza$BMI)
#zavisne,epsilon=0.021, slaba zavisnost

kruskal.test(HeartDisease ~ PhysicalHealth, data = baza)
epsilonSquared(x = baza$HeartDisease, g = baza$PhysicalHealth)
#zavisne,epsilon=0.0304, slaba zavisnost

kruskal.test(HeartDisease ~ MentalHealth, data = baza)
epsilonSquared(x = baza$HeartDisease, g = baza$MentalHealth)
#zavisne,epsilon=0.0304, slaba zavisnost

kruskal.test(HeartDisease ~ SleepTime, data = baza)
epsilonSquared(x = baza$HeartDisease, g = baza$SleepTime)
#zavisne,epsilon=0.0072, slaba zavisnost


#iskljucivanje varijable BMI
baza$BMI<-NULL

#cuvanje sredjenog dataseta u RDS formatu
bazaRDS<-data.frame(baza)
str(bazaRDS)
saveRDS(bazaRDS, "baza.rds")








