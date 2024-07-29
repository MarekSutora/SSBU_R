


who = read.csv("WHO.csv")
str(who)
summary(who)
who_europa = subset(who, Region == "Europe")
str(who_europa)
summary(who_europa)
write.csv(who_europa, "europa.csv")

nrow(subset(who, Population > 100000))

who$Country[which.max(who$LifeExpectancy)]
who$Country[which.min(who$ChildMortality)]

who$Country[which.min(who$LiteracyRate)]
who$Country[which.max(who$LiteracyRate)]

who$LiteracyRate[who$Country == "Cuba"]
who$Country

who$LifeExpectancy[who$Country == "Slovakia"]
who$ChildMortality[who$Country == "Slovakia"]
who$FertilityRate[who$Country == "Slovakia"]
who$FertilityRate[who$Country == "Cuba"]
who$LifeExpectancy[who$Country == "Cuba"]
who$FertilityRate[who$Country == "Democratic People's Republic of Korea"]
who$LifeExpectancy[who$Country == "Democratic People's Republic of Korea"]
who$FertilityRate[who$Country == "Mexico"]
who$ChildMortality[who$Country == "Mexico"]
who$FertilityRate[who$Country == "India"]
who$ChildMortality[who$Country == "India"]
who$FertilityRate[who$Country == "China"]
who$LifeExpectancy[who$Country == "China"]
who$FertilityRate[who$Country == "Ukraine"]
who$ChildMortality[who$Country == "Ukraine"]
who$FertilityRate[who$Country == "Malaysia"]
who$ChildMortality[who$Country == "Malaysia"]
who$ChildMortality[who$Country == "Sri Lanka"]
who$LifeExpectancy[who$Country == "Sri Lanka"]
who$FertilityRate[who$Country == "Romania"]
who$LifeExpectancy[who$Country == "Romania"]

#kat1: GNI <= 2000
#kat2: > 2000 <= 5000
#kat3: >5000 <= 10000
#kat4: >10000

kat1 <- subset(who, GNI <= 2000)
kat2 <- subset(who, GNI > 2000 & GNI <= 5000)
kat3 <- subset(who, GNI > 5000 & GNI <= 10000)
kat4 <- subset(who, GNI > 10000)
nrow(kat1)
nrow(kat2)
nrow(kat3)
nrow(kat4)
kat1$Country
kat2$Country
kat3$Country
kat4$Country

kat1[c("Country","GNI")]
kat2[c("Country","GNI")]
kat3[c("Country","GNI")]
kat4[c("Country","GNI")]

plot(who$GNI, who$FertilityRate)

Outliers = subset(who, GNI > 10000 & FertilityRate > 2.5)

Outliers[c("Country", "GNI","FertilityRate")]

hist(who$CellularSubscribers)
boxplot(who$LifeExpectancy)
boxplot(who$LifeExpectancy ~ who$Region)
table(who$Region)
tapply(who$Over60, who$Region, mean)
tapply(who$LiteracyRate, who$Region, min, na.rm = TRUE)

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/2")
USDA = read.csv("USDA.csv")
str(USDA)
summary(USDA)

USDA$Description[which.max(USDA$Sodium)]
USDA$Sodium[USDA$Description == "CAVIAR"]
plot(USDA$Protein, USDA$TotalFat)
USDA$Sodium[1] > mean(USDA$Sodium, na.rm = TRUE)
USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)
VysokySodik = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
VysokySodik 
VysokyProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
VysokyProtein 
VysokyTuk = as.numeric(USDA$SaturatedFat > mean(USDA$SaturatedFat , na.rm = TRUE))
VysokyTuk 
VysokySach = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate , na.rm = TRUE))
VysokySach 

nrow(subset(USDA, Sugar > 50))

nizkotucne = nrow(subset(USDA, TotalFat <= mean(USDA$TotalFat, na.rm = TRUE)))
nizkotucne 
VysokySach = nrow(subset(USDA, Carbohydrate > mean(USDA$Carbohydrate , na.rm = TRUE)))
VysokySach 

VysokoKal = nrow(subset(USDA, Calories > 500))
VysokoKal 

USDA$SaturatedFat[USDA$Description == "CHEESE,FETA"]

nrow(subset(USDA, Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE)))
test = subset(USDA, Sugar > 50)
nrow(test)
tapply(USDA$Iron, USDA$Protein > mean(USDA$Protein , na.rm = TRUE), mean, na.rm = TRUE)
tapply(USDA$Iron, USDA$Protein < mean(USDA$Protein , na.rm = TRUE), mean, na.rm = TRUE)

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/3")

vina = read.csv("wine.csv")
str(vina)

#excel y = b0 +b1*x1
#Price = b0 + b1*AGST
#Price = 100 + 200*AGST

plot(vina$HarvestRain, vina$Price)
plot(vina$WinterRain, vina$Price)
plot(vina$Age, vina$Price)
plot(vina$AGST, vina$Price)

model1 = lm(Price ~ AGST, data = vina)
summary(model1)
#Price = -3.41 + 0.63*AGST

model2 = lm(Price ~ AGST + HarvestRain, data = vina)
summary(model2)

model3 = lm(Price ~ AGST + WinterRain + Age, data = vina)
summary(model3)

model4 = lm(Price ~ WinterRain + Age, data = vina)
summary(model4)

model5 = lm(Price ~ HarvestRain + Age, data = vina)
summary(model5)

modelM3 = lm(Price ~ AGST + WinterRain + Age, data = vina)
summary(modelM3)
SSEM3 = sum(modelM3$residuals^2)
MSEM3 = SSEM3/nrow(vina)
MSEM3

plot(vina$Price, vina$AGST)

# p-value > alfa => H0
# p-value < alfa => H1 
# H0: b0 = 0
# H1: b0 <> 0
#f-statistika H0: model je statisticky nevyznamny H1: model je statisticky vyznamny
#spravnost modelu -t-test, F-test
#hodnotenie modelu - koeficient determinacie R2 a chyba
#

model1$residuals
SSE1 = sum(model1$residuals^2)
SSE1

SSE2 = sum(model2$residuals^2)
SSE2

model3= lm(Price ~ AGST + HarvestRain + Age, data = vina)
summary(model3)
#Price = -1.47 + 0.53*AGST - 0.004*HR + 0.02*Age

SSE3 = sum(model3$residuals^2)
SSE3

MSE = SSE3/nrow(vina)
MSE

RMSE = sqrt(MSE)
RMSE 

RMSE3 = sqrt((sum(model3$residuals^2))/nrow(vina))
RMSE3

model4 = lm(Price ~ AGST + HarvestRain + Age + FrancePop, data = vina)
summary(model4)

plot(vina$Age, vina$FrancePop)
cor(vina$Age, vina$FrancePop)
cor(vina)

vinaTest = read.csv("wine_test.csv")
str(vinaTest)

predictTest = predict(model3, newdata = vinaTest)
predictTest

SSE3_test = sum((vinaTest$Price - predictTest)^2)
SSE3_test

#R^2_test = 1-SSE/SST
#MAPE_test

#####################################################################################################
#############################    CVICENIE 4 #########################################################
#####################################################################################################


setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/4")

framingham = read.csv("framingham.csv")
summary(framingham)
str(framingham)


#NA ignorujem
framingham = na.omit(framingham) 


muzy = subset(framingham, TenYearCHD == 1 & male == 1)
nrow(muzy)
zeny = subset(framingham, TenYearCHD == 1 & male == 0)
nrow(zeny)

table(framingham$TenYearCHD, framingham$male)

tapply(framingham$TenYearCHD, framingham$male, mean)

#clovek co vyfajci najviac cigariet
which.max(framingham$cigsPerDay)
framingham$cigsPerDay[which.max(framingham$cigsPerDay)]


install.packages("caTools")
library(caTools)
set.seed(1)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
split

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
nrow(train)
nrow(test)

#generalizovany lin. model, param 1 - zav. premenna, . - vsetky ostatne
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

#vypise pravdepodobnosti jednotlivych poloziek v modeli
predictTrain=predict(framinghamLog, type = "response")
predictTrain

table(train$TenYearCHD, predictTrain > 0.2)

total = (2003 + 25) / (2003 + 13 + 337 + 25)
total

specificita = 2003 / (2003 + 13)
specifita 

senzitivita = 25 / (337 + 25)
senzitivita 


table(framingham$TenYearCHD)
#aritmeticka presnost - chceme co najvyssie
AA = (specificita + senzitivita) / 2
AA #0.531

table(train$TenYearCHD, predictTrain > 0.4)

total2 = (1969 + 51) / (1969 + 47 + 311 + 51)
total2

specificita2 = 1969 / (1969 + 47)
specificita2 

senzitivita2 = 51 / (311 + 51)
senzitivita2 

AA2 = (specificita2 + senzitivita2) / 2
AA2 #0.558

table(train$TenYearCHD, predictTrain > 0.3)

total3 = (1858 + 101) / (1858 + 158 + 261 + 101)
total3

specificita3 = 1858 / (1858 + 158)
specificita3 

senzitivita3 = 101 / (261 + 101)
senzitivita3 

AA3 = (specificita3 + senzitivita3) / 2
AA3 # 0.6


 
#priemerna pravdepodobnost tych co dostali a tych co nedostali infarkt odhadovana modelo
tapply(predictTrain, train$TenYearCHD, mean)
tapply(predictTrain, train$TenYearCHD, median)

table(train$TenYearCHD, predictTrain > 0.16)

total4 = (1418 + 235) / (1418 + 598 + 127 + 235)
total4
 
specificita4 = 1418 / (1418 + 598)
specificita4 

senzitivita4 = 235 / (127 + 235)
senzitivita4 

AA4 = (specificita4 + senzitivita4) / 2
AA4 #0.67

#t = 0,16 bolo najlepsie , ideme na test. mnoz.

predictTest = predict(framinghamLog, newdata = test, type="response")
table(test$TenYearCHD, predictTest > 0.16)

totalTest = (741+136)/(741+344+59+136)
totalTest

specTest = 741/(741+344)
specTest 

senzTest = 136/(59+136)
senzTest 

AATest = (specTest + senzTest)/2
AATest 
#####################################################################################################
#############################    CVICENIE 5 #########################################################
#####################################################################################################

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/5")
stevens = read.csv("stevens.csv")
str(stevens)
set.seed(1)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, split == TRUE)
test = subset(stevens, split == FALSE)
nrow(train)
nrow(test)
install.packages("rpart")
install.packages("rpart.plot")

Strom1 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25)
Strom2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
Strom3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 1)
#class - klasifikacny strom

Strom4 = rpart(Reverse ~ Petitioner + Respondent, data = stevens, method = "class", minbucket = 25)
predictTrain4 = predict(Strom4, type = "class")
table(stevens$Reverse, predictTrain4)
(138+246)/(138+119+63+246)


Strom5 = rpart(Reverse ~ Issue + Respondent, data = stevens, method = "class", minbucket = 20)
predictTrain5 = predict(Strom5, type = "class")
table(stevens$Reverse, predictTrain5)
(82+277)/(82+175+32+277)

Strom6 = rpart(Reverse ~ Issue + Petitioner, data = stevens, method = "class", minbucket = 20)
predictTrain6 = predict(Strom6, type = "class")
table(stevens$Reverse, predictTrain6)
(82+277)/(82+175+32+277)

Strom7 = rpart(Reverse ~ Issue + Unconst, data = stevens, method = "class", minbucket = 25)
predictTrain7 = predict(Strom7, type = "class")
table(stevens$Reverse, predictTrain7)
(160+182)/(160+97+127+182)

Strom8 = rpart(Reverse ~ LowerCourt + Issue, data = stevens, method = "class", minbucket = 25)
predictTrain8 = predict(Strom8, type = "class")
table(stevens$Reverse, predictTrain8)
(171+207)/(171+86+102+207)

prp(Strom1)
prp(Strom2)
prp(Strom3)

predictTrain1 = predict(Strom1, type = "class")
table(train$Reverse, predictTrain1)
(158 + 130) / (158 + 22 + 86 + 130)
158 / (158 + 22) #speci
130 / (86 + 130) #senzi
(0.877+0.601)/2 #0.739


predictTrain2 = predict(Strom2, type = "class")
table(train$Reverse, predictTrain2)
(126 + 139) / (126 + 54 + 77 + 139)
126 / (126+54)
139/(77+139)
(0.7+0.643)/2 #0.6715

predictTrain3 = predict(Strom3, type = "class")
table(train$Reverse, predictTrain3)
(136 + 193) / (136 + 44 + 23 + 193)
136 / (136+44)
193/(23+193)
(0.756+0.893)/2 #0.82

#minbucket1 = total 83, spec 75, senz 89, s+s/2 82

predictTest1 = predict(Strom1, newdata = test,type = "class")
table(test$Reverse, predictTest1)
(55 + 47)/(55 + 22 + 46 + 47)
#0.6

predictTest2 = predict(Strom2, newdata = test,type = "class")
table(test$Reverse, predictTest2)
(45+68)/(45+32+25+68)
#0.66

predictTest3 = predict(Strom3, newdata = test,type = "class")
table(test$Reverse, predictTest3)
(40+66)/(40+37+27+66)
#0.62


#generalizovany lin. model, param 1 - zav. premenna, . - vsetky ostatne
stevensLog = glm(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, family = binomial)
summary(stevensLog)

#vypise pravdepodobnosti jednotlivych poloziek v modeli
predictTrainStevensLog=predict(stevensLog, type = "response")
predictTrainStevensLog

table(train$Reverse, predictTrainStevensLog > 0.5)

total = (128 +161) / (128 + 52 + 55 + 161)
total

specificita = 128 / (128+ 52)
specificita 

senzitivita = 161/(161+55)
senzitivita 
AATestLogStevens = (specificita + senzitivita )/2
AATestLogStevens 

predictTest = predict(stevensLog , newdata = test, type="response")
table(test$Reverse, predictTest > 0.5)
(45+65)/(45+32+28+65)

install.packages("randomForest")
library(randomForest)
#train$Reverse = as.factor(train$Reverse)
#test$Reverse = as.factor(test$Reverse)
Les = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, ntree = 200, nodesize = 25)

#pretypujem premennu na kategoricku
predictTrainLes = predict(Les)
table(train$Reverse, predictTrainLes)
(109+155)/(109+71+61+155)

(2369+266)/(2369+732+291+266)

#####################################################################################################
#############################    CVICENIE 6 #########################################################
#####################################################################################################

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/5")

boston = read.csv("boston.csv")
summary(boston)
str(boston)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = 'blue', pch = 19)

summary(boston$NOX)
points(boston$LON[boston$NOX > 0.55],boston$LAT[boston$NOX > 0.55],col = 'green', pch = 19)

#subset(boston, NOX > 0.5547)

summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV >= 21.2],col = 'red', pch = 19)

#medv priklad:
#-solinky:100, 105, 110, 120, 121    => 110
#-vlcince:105, 107, 112, 118, 125    => 112
#-hajik:  98,99,100,100,105,120,121  => 100
#-centrum:      120,120,150,155,168  => 150
#-borik:            140,145,180      => 145

#100,110,112,145,150    => 112(MEDV)

cor(boston$LAT, boston$MEDV)
plot(boston$LAT, boston$MEDV)

cor(boston$LON, boston$MEDV)
plot(boston$LON, boston$MEDV)

latlonmodel = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonmodel)


predictTrain = predict(latlonmodel)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV >= 21.2],col = 'red', pch = 19)
points(boston$LON[predictTrain >= 21.2],boston$LAT[predictTrain >= 21.2],col = 'green', pch = "$")

library(rpart)
library(rpart.plot)

latlonstrom = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlonstrom)
predictTrain_latlonstrom = predict(latlonstrom)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV >= 21.2],col = 'red', pch = 19)
points(boston$LON[predictTrain_latlonstrom>= 21.2],boston$LAT[predictTrain_latlonstrom >= 21.2],col = 'blue', pch = "$")

latlonstrom50 = rpart(MEDV ~ LAT + LON, data = boston, minbucket=50)
prp(latlonstrom50)
plot(latlonstrom50)
text(latlonstrom50)

plot(boston$LON, boston$LAT)
abline(v = -71.07)
abline(h = 42.28)
abline(h = 42.17)
abline(h = 42.21)

#rozdelit data na trenovaciu a test
#seed = 123
#train - 70%
#test - 30%
#njaprv lin regso vsetkymi prem
#SSE1 modelu na test mnoz
#strom medv tiez od 13 premennych
#SSE2 na test

plot(latlonstrom50)
prp(latlonstrom50)
abline = (v = -71.07)

set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.70)
split
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

modelB = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
predictTestB = predict(modelB, newdata = test)
SSE1 = sum((test$MEDV - predictTestB)^2)
SSE1

latlonstrom = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
predictTest_latlonstrom = predict(latlonstrom, newdata = test)
SSE2 = sum((test$MEDV - predictTest_latlonstrom)^2)
SSE2

#####################################################################################################
#############################    CVICENIE 7 #########################################################
#####################################################################################################

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/6")

kvet = read.csv("flower.csv", header=FALSE)
str(kvet)
kvetMatica = as.matrix(kvet)
str(kvetMatica)
kvetVektor = as.vector(kvetMatica)
str(kvetVektor)
  
#50*50pixelov => 2500*2500 matica vzdialenosdti

vzdialenosti = dist(kvetVektor, method = "euclidean")
summary(vzdialenosti)
str(vzdialenosti)
zhlukyPodlaIntenzity = hclust(vzdialenosti, method = "ward.D2")
plot(zhlukyPodlaIntenzity )

kvetZhluky = cutree(zhlukyPodlaIntenzity, k=3)
kvetZhluky
str(kvetZhluky)
tapply(kvetVektor, kvetZhluky ,mean) #priemerna intenzita farby podla zhluku

kvetZhluky2 = cutree(zhlukyPodlaIntenzity, k=4)
kvetZhluky2
str(kvetZhluky2)
tapply(kvetVektor, kvetZhluky2 ,mean) #priemerna intenzita farby podla zhluku

kvetZhluky3 = cutree(zhlukyPodlaIntenzity, k=2)
kvetZhluky3
str(kvetZhluky3)
tapply(kvetVektor, kvetZhluky3 ,mean) #priemerna intenzita farby podla zhluku

dim(kvetZhluky) = c(50,50)
str(kvetZhluky)

image(kvetZhluky, axes = FALSE, col = rainbow(3))
image(kvetMatica, axes = FALSE, col = grey(seq(0,1,length = 256)))

zdravy = read.csv("healthy.csv", header=FALSE)
str(zdravy)
566*646
zdravyMatica = as.matrix(zdravy)
zdravyVektor = as.vector(zdravyMatica)
vzdialenostiZ = dist(zdravyVektor, method = "euclidean")
str(vzdialenostiZ)
str(zdravyVektor)
((365636*365635))/2

#k-means
k = 5

set.seed(1)
KM = kmeans(zdravyVektor, centers = k, iter.max = 1000)
str(KM)

zdravyZhluky = KM$cluster
dim(zdravyZhluky ) = c(566,646)


image(zdravyZhluky , axes = FALSE, col = rainbow(5))
image(zdravyMatica, axes = FALSE, col = grey(seq(0,1,length = 256)))

#euklidova vzdialenost
sqrt(sum((a1 - a2)^2) + (b1 - b2)^2)

euclidean <- function(a, b) sqrt(sum((a - b)^2))
a <- c(6,7)
b <- c(3,4)
euclidean(a, b)

#####################################################################################################
#############################    CVICENIE 8 #########################################################
#####################################################################################################

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/7")

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)

Trump =subset(tweets, is_retweet == "False" & handle == "realDonaldTrump")
TrumpR = subset(tweets, handle == "realDonaldTrump")
Hillary = subset(tweets, is_retweet == "False" & handle == "HillaryClinton")
sum(nchar(TrumpR $text))
sum(nchar(Hillary$text))

PotusTweety = subset(tweets, original_author == "POTUS") 
sum(nchar(PotusTweety$text))

HillaryTweety = subset(tweets, handle == "HillaryClinton")
sum(nchar(HillaryTweety$text))

Retweet = subset(tweets, is_retweet == "True")
RetweetKorpus = Corpus(VectorSource(Retweet$text))

RetweetKorpus = tm_map(RetweetKorpus , tolower)
RetweetKorpus = tm_map(RetweetKorpus , removePunctuation)
RetweetKorpus = tm_map(RetweetKorpus , removeNumbers)
RetweetKorpus = tm_map(RetweetKorpus , removeWords, stopwords("english"))
RetweetKorpus = tm_map(RetweetKorpus , stripWhitespace)
RetweetKorpus = tm_map(RetweetKorpus , stemDocument)
dtm1 = DocumentTermMatrix(RetweetKorpus )
dtm1 = removeSparseTerms(dtm1, 0.97)
dtm1
frekvencia1 = colSums(as.matrix(dtm1))
str(frekvencia1)
frekvencia1 
sort(frekvencia1, decreasing = TRUE)


sum(nchar(Trump$text))
sum(nchar(Hillary$text))
install.packages("tm")
install.packages("SnowballC")
library(tm)

TrumpKorpus = Corpus(VectorSource(Trump$text))
str(TrumpKorpus)
writeLines(as.character(TrumpKorpus[[1]]))

TrumpKorpus = tm_map(TrumpKorpus, tolower)
TrumpKorpus = tm_map(TrumpKorpus, removePunctuation)
TrumpKorpus = tm_map(TrumpKorpus, removeNumbers)
TrumpKorpus = tm_map(TrumpKorpus, removeWords, stopwords("english"))
TrumpKorpus = tm_map(TrumpKorpus, stripWhitespace)
TrumpKorpus = tm_map(TrumpKorpus, stemDocument)

dtm = DocumentTermMatrix(TrumpKorpus)
dtm = removeSparseTerms(dtm, 0.97)
dtm
frekvencia = colSums(as.matrix(dtm))
str(frekvencia)
frekvencia 
sort(frekvencia, decreasing = TRUE)

#####################################################################################################
#############################    CVICENIE 9 #########################################################
#####################################################################################################

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia/5")

emaily = read.csv("energy_bids.csv", stringsAsFactors=FALSE)
str(emaily)

emaily$email[5]
podvodne = nrow(subset(emaily, responsive == 1))
korpus = Corpus(VectorSource(emaily$email))
writeLines(as.character(korpus[[1]]))
korpus = tm_map(korpus, tolower)
korpus = tm_map(korpus, removePunctuation)
korpus = tm_map(korpus, removeNumbers)
korpus = tm_map(korpus, removeWords, stopwords("english"))
korpus = tm_map(korpus, stripWhitespace)
korpus = tm_map(korpus, stemDocument)
dtm = DocumentTermMatrix(korpus)
dtm
dtm = removeSparseTerms(dtm, 0.97)

slova = as.data.frame(as.matrix(dtm))
str(slova)
slova$responsive = emaily$responsive
str(slova$responsive)
#train, test 7:3
#strom zavisla - responsive
#presnost stromu na train a test 

set.seed(1)
split = sample.split(slova$responsive, SplitRatio = 0.7)
train = subset(slova, split == TRUE)
test = subset(slova, split == FALSE)
nrow(train)
nrow(test)

Strom = rpart(responsive ~ ., data = train, method = "class")
#class - klasifikacny strom
prp(Strom)


predictTrain = predict(Strom, type = "class")
table(train$responsive, predictTrain)

predictTest = predict(Strom, newdata = test, type = "class")
table(test$responsive , predictTest)

(476+71)/(476+25+26+71)
(194+27)/(194+21+15+27)

#####################################################################################################
#############################    CVICENIE 10 #########################################################
#####################################################################################################
install.packages("tidyverse")

setwd("C:/Users/marek/Desktop/Dáta informácie a znalosti/cvicenia")
kradeze = read.csv("kradeze.csv", stringsAsFactors=FALSE)
str(kradeze)
str(who)
plot(who$GNI, who$FertilityRate)

setwd("C:/Users/marek/Desktop/DIZ/cvicenia/1")

usvideos = read.csv("USVideosdruheexport.csv", header=TRUE, sep=";", row.names=NULL)
str(usvideos)
summary(usvideos)



plot(usvideos$views, usvideos$likes, col= ifelse(usvideos$category_id == 1, "red", ifelse(usvideos$category_id == 2,"blue", "black")), ylim = c(0, 10))
plot(usvideos$views, usvideos$likes)


points(usvideos$views[usvideos$category == 1],usvideos$likes[usvideos$category == 1],col = 'red', pch = 19)
points(usvideos$views[usvideos$category == 2],usvideos$likes[usvideos$category == 2],col = 'green', pch = "$")
points(usvideos$views[usvideos$category == 3],usvideos$likes[usvideos$category == 3],col = 'blue', pch = "#")
points(usvideos$views[usvideos$category == 4],usvideos$likes[usvideos$category == 4],col = 'yellow', pch = "@")
points(usvideos$views[usvideos$category == 5],usvideos$likes[usvideos$category == 5],col = 'orange', pch = "*")
points(usvideos$views[usvideos$category == 6],usvideos$likes[usvideos$category == 6],col = 'red', pch = 19)
points(usvideos$views[usvideos$category == 7],usvideos$likes[usvideos$category == 7],col = 'red', pch = 19)
points(usvideos$views[usvideos$category == 8],usvideos$likes[usvideos$category == 8],col = 'red', pch = 19)

ggplot(usvideos, aes(x=likes, y=views, colour=category_id)) + geom_point() + geom_tile(aes(fill = category_id)) +
scale_fill_gradient(low = "yellow", high = "red")


ggplot(data=booksRows, aes(x=views, y=category_ig, colour=likes)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig, colour=dislikes)) + geom_point()


points(usvideos$views[predictTrain >= 21.2],usvideos$likes[predictTrain >= 21.2],col = 'green', pch = "$")

ggplot(usvideos, aes(x = views, y = likes)) + geom_point()

install.packages("ggplot2")
library(ggplot2)

gg_graf = ggplot(who, aes(x = GNI, y = FertilityRate))

gg_graf + geom_point(color = "blue", size = 3, shape = 10) +
ggtitle("Zavislost HNP a porodnosti") +
xlab("hruby narodny produkt") + ylab("porodnost") +
theme(plot.title = element_text(size = 5, face = "bold", hjust = 0.5), axis.title = element_text(face = "bold", size = 12))

gg_graf + geom_point(colour = "blue", size = 3, shape = 17) +
ggtitle("Zavislost GNI a porodnosti") +
scale_x_continuous(name = "hruby narodny prijeaaaam", breaks = seq(0,100000,10000), limits =c(0,100000)) +
scale_y_continuous(name = "porodnost") +
theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5), axis.title = element_text(face = "bold", size = 12))


gg_graf + geom_point(color = "blue", size = 3, shape = 10) +
ggtitle("Zavislost HNP a porodnosti") +
theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.title = element_text(face = "bold", size = 12)) +
scale_x_continuous(name = "hruby narodny produkt", breaks = seq(0,100000, 10000), limits = c(0,100000)) +
scale_y_continuous(name = "porodnost", breaks = seq(0,10,1), limits=c(0,10))

porodnostHNP = gg_graf + geom_point(colour = "green")
pdf("moj_gg_graf.pdf")
print(porodnostHNP)
dev.off()

ggplot(who, aes(x = GNI, y = FertilityRate)) + geom_point() + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = "bold")

ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(who, aes(x = Under15, y = FertilityRate)) + geom_point()

ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

summary(who)
who = na.omit(who)
modell = lm(Under15 ~ log(FertilityRate), data = who)
summary(modell)

SSE = sum(modell$residuals^2)
SSE

MSE = SSE/length(modell$residuals^2)
MSE

RMSE = sqrt(MSE)
RMSE 

summary(kradeze)

table(kradeze$DenTyzdna)

DniPocty = as.data.frame(table(kradeze$DenTyzdna))
DniPocty

DenHodinaPocty = as.data.frame(table(kradeze$DenTyzdna, kradeze$Hodina))


ggplot(DniPocty, aes(x = Var1, y = Freq, group = 1)) + geom_line(color = "red")

DniPocty$Var1 = factor(DniPocty$Var, ordered = TRUE, levels = c("pondelok", "utorok", "streda", "tvrtok", "piatok", "sobota", "nede¾a"))

DenHodinaPocty$Var1 = factor(DenHodinaPocty$Var1, ordered = TRUE, levels=c("pondelok", "utorok", "streda", "tvrtok", "piatok", "sobota", levels(DenHodinaPocty$Var1)[1]))

ggplot(DenHodinaPocty, aes(x = Var2, y = Var1)) + geom_tile(aes(fill = Freq))
ggplot(DenHodinaPocty, aes(x = Var2, y = Var1)) +
geom_tile(aes(fill = Freq)) +
scale_fill_gradient(low = "yellow", high = "red")

#legenda theme(), heatmapa geom_tile(), lin reg do grafu stat_smooth(), prisposobenie heat mapy scale_fill_gradient()

ggplot(DenHodinaPocty, aes(x = Var2, y = Var1)) + geom_tile(aes(fill = Freq))
ggplot(DenHodinaPocty, aes(x = Var2, y = Var1)) +
geom_tile(aes(fill = Freq)) +
scale_fill_gradient(low = "yellow", high = "red")


# teraz seriozne cez databazu
#install.packages("RMariaDB")
library(RMariaDB)

myDB <- dbConnect(RMariaDB::MariaDB(), user='root',password='Uniba2Hanka', dbname='test', host='localhost')
#dbListTables(myDB)
dbAnswer <- dbSendQuery(myDB,'select * from data2')
booksRows <- dbFetch(dbAnswer)

# deskriptivna statistika
summary(booksRows['likes'])
quantile(booksRows$likes,0.25)
quantile(booksRows$likes,0.5)
quantile(booksRows$likes,0.75)
quantile(booksRows$likes,0.95)

min(booksRows$likes)
max(booksRows$likes)
range(booksRows$likes)
# pozor range je vacsinou reprezentovany ako max-min, takze tu treba pocita takto:
range(booksRows$likes)[2]-range(booksRows$likes)[1]
#alebo klasika
max(booksRows$likes)-min(booksRows$likes)
mean(booksRows$likes)
median(booksRows$likes)
IQR(booksRows$likes)
sd(booksRows$likes)
var(booksRows$likes)

#statistika podla skupiny dat
by(booksRows,booksRows$category_ig,summary)

#iny package
#install.packages("pastecs")
library(pastecs)

stat.desc(booksRows)

#mode/modus nie je taky jednoduchy ?
install.packages("modeest")
library(modeest)

mlv(booksRows$likes, method='mlv')

# best package na pivot tabulky 😉
# install.packages("pivottabler")
library("pivottabler")
bestPT <- PivotTable$new()
bestPT$addData(booksRows)
bestPT$addColumnDataGroups("category_ig")
bestPT$addRowDataGroups("trending_date")
bestPT$defineCalculation(calculationName="TotalCategory", summariseExpression="n()")
bestPT$renderPivot()

install.packages("ggplot2")
library(ggplot2)

#XY plots
ggplot(data=booksRows, aes(x=views, y=category_ig)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig, colour=likes)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig, colour=dislikes)) + geom_point()

ggplot(data=usvideos) + stat_summary(
  mapping = aes(x =category_id , y = views ),
  fun.min = min,
  fun.max = max,
  fun = median
)

# frekvencna tabulka alebo histogram
hist(usvideos$views)
hist(usvideos$category_id, xlab="Kategorie", ylab = "Pocet videi", main = paste("Histogram poctu videí podla kategorie"))

# a boxplot
boxplot(usvideos$views)
boxplot(usvideos$comment_count)

# rychly nahlad na data v premennej z databazy
str(booksRows$views)
str(booksRows$comment_count)
head(booksRows, )
head(booksRows, n=3)
tail(booksRows)
tail(booksRows, n=3)

ggplot(usvideos, aes(category_id)) + geom_histogram(binwidth = 5, fill = "red") + 
ggtitle("test") + theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5), axis.title = element_text(face = "bold", size = 12))

ggplot(usvideos, aes(category_id, views)) + geom_col() +
ggtitle("Graf poctu pozreti podla kategorie") +
scale_x_continuous(name = "cislo kategorie", breaks = seq(1,40,1), limits = c(1,40)) 
 
 
ggplot(usvideos, aes(category_id, views)) + geom_boxplot()

ggplot(usvideos, aes(x = views, y = likes, colour = comment_count)) + geom_point() +
scale_colour_gradient(
  low = "yellow",
  high = "red",
  space = "Lab",
  na.value = "grey50",
  guide = "coloursteps",
  aesthetics = "colour",
) + xlab("počet pozretí") + ylab("počet likov") +
ggtitle("Závislosť počtu likov od počtu pozretí") +
theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.title = element_text(face = "bold", size = 12))


ggplot(usvideos, aes(x = views, y = comment_count, colour = likes)) + geom_point() +
scale_colour_gradient(
  low = "yellow",
  high = "red",
  space = "Lab",
  na.value = "grey50",
  guide = "coloursteps",
  aesthetics = "colour",
) + xlab("počet pozretí") + ylab("počet komentárov") +
ggtitle("Závislosť počtu komentárov od počtu pozretí") +
theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.title = element_text(face = "bold", size = 12))


#XY plots
#install.packages("ggplot2")
library(ggplot2)

ggplot(data=booksRows, aes(x=views, y=category_ig)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig, colour=likes)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig, colour=dislikes)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig, colour=comment_count)) + geom_point()
ggplot(data=booksRows, aes(x=views, y=category_ig)) + geom_line()
ggplot(data=booksRows, aes(x=views, y=category_ig)) + geom_line(color="red", linetype="dashed")
ggplot(data=booksRows, aes(x=views, y=category_ig)) + geom_area()
