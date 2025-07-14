#===== 0. Funkcje z pliku CDA.r =====
GK.gamma<-function(x, pr=0.95){
  # x is a matrix of counts.  You can use output of crosstabs or xtabs in R.
  # A matrix of counts can be formed from a data frame by using design.table.
  
  # Confidence interval calculation and output from Greg Rodd
  
  # Check for using S-PLUS and output is from crosstabs (needs >= S-PLUS 6.0)
  if(is.null(version$language) && inherits(x, "crosstabs")) { oldClass(x)<-NULL; attr(x, "marginals")<-NULL}
  
  n <- nrow(x)
  m <- ncol(x)
  pi.c<-pi.d<-matrix(0,nr=n,nc=m)
  
  row.x<-row(x)
  col.x<-col(x)
  
  for(i in 1:(n)){
    for(j in 1:(m)){
      pi.c[i, j]<-sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
      pi.d[i, j]<-sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
    }
  }
  
  C <- sum(pi.c*x)/2
  D <- sum(pi.d*x)/2
  
  psi<-2*(D*pi.c-C*pi.d)/(C+D)^2
  sigma2<-sum(x*psi^2)-sum(x*psi)^2
  
  gamma <- (C - D)/(C + D)
  pr2 <- 1 - (1 - pr)/2
  CIa <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
  
  list(gamma = gamma, C = C, D = D, sigma = sqrt(sigma2), Level = paste(
    100 * pr, "%", sep = ""), CI = paste(c("[", max(CIa[1], -1), 
                                           ", ", min(CIa[2], 1), "]"), collapse = ""))     
}

rcont2<-function(nr, nc, nrowt, ncolt, ntotal, fact){
  tab<-matrix(0,nr,nc)
  code<-0
  
  jwork<-ncolt
  jc<-ntotal
  for(l in 1:(nr-1)){
    nrowtl<-nrowt[l]
    ia<-nrowtl; ic<-jc; jc<-jc-nrowtl
    goto190<-FALSE
    for(m in 1:(nc-1)){
      id<-jwork[m]; ie<-ic; ic<-ic-id; ib<-ie-ia; ii<-ib-id
      if( ie==0 ){
        for(j in m:nc) tab[l,j]<-0;
        goto190<-TRUE
        break
      }
      rand<-runif(1)
      
      to<-0
      while(TRUE){        
        #compute conditional expected value of tab[l,m]
        nlm<-as.integer(ia*id/ie + 0.5)
        iap<-ia+1; idp<-id+1; igp<-idp-nlm; ihp<-iap-nlm; nlmp<-nlm+1; iip<-ii+nlmp
        x<-exp(fact[iap]+fact[ib+1]+fact[ic+1]+fact[idp]-fact[ie+1]-fact[nlmp]-fact[igp]-fact[ihp]-fact[iip])
        if(x >= rand){
          tab[l,m]<-nlm
          ia<-ia-nlm
          jwork[m]<-jwork[m]-nlm
          tab[l,nc]<-ia
          goto190<-TRUE
          break
        }
        sumprb<-x
        y<-x
        nll<-nlm; lsp<-FALSE; lsm<-FALSE
        while(TRUE){
          #increment entry in row l column m
          j=(id-nlm)*(ia-nlm)
          if(j==0) lsp<-TRUE
          else{
            nlm<-nlm+1
            x<-x*j/(nlm*(ii+nlm))
            sumprb<-sumprb+x
            if(sumprb >= rand){
              to<-160
              break
            }
          }
          while(!lsm){
            #decrement entry in row l column m
            j<-nll*(ii+nll)
            if(j==0)lsm=TRUE
            else{
              nll<-nll-1
              y<-y*j/((id-nll)*(ia-nll))
              sumprb<-sumprb+y
              if(sumprb >= rand){
                to<-159
                break
              }
              else if(!lsp) break
            }
          }
          if(!(to == 0)) break
          if(lsp) break
        }
        if(!(to == 0)) break
        rand<-sumprb*runif(1)
      }
      if(to==159 || to==160) {
        if(to==159) nlm<-nll
        tab[l,m]<-nlm
        ia<-ia-nlm
        jwork[m]<-jwork[m]-nlm
      }
    } #180
    tab[l,nc]<-ia
  } #190
  
  # compute entries in last row of matrix
  for(m in 1:(nc-1)) tab[nr,m]<-as.integer(jwork[m])
  tab[nr,nc]<-as.integer(ib)-tab[nr,nc-1]
  
  list(tab=tab,code=code)
}

fisher.gamma.test<-function(x, B=10000){
  nr<-nrow(x); nc<-ncol(x)
  nrowt<-rowSums(x)
  ncolt<-colSums(x)
  
  # prepare log-factorials for multiple use
  ntotal=sum(nrowt)
  fact<-c(0,log(seq(1:ntotal)))
  for(i in 3:(ntotal+1)) fact[i]<-fact[i-1]+fact[i]
  
  # observed gamma
  observed<-GK.gamma(x)$gamma
  
  # generate tables and calculate gamma's and p-value
  count<-0
  for(i in 1:B){
    generated<-GK.gamma(rcont2(nr, nc, nrowt, ncolt, ntotal, fact)$tab)$gamma
    if( generated >= observed ) count<-count+1
  }
  p<-count/B
  p
}


#===== 1. Wczytanie zbioru danych =====

# Wczytuję plik cvc, dane są już przygotowane
data <- read.csv("dane_przygotowane.csv")
str(data)
names(data)

# Należy zamienić dane na factory
data$Count.Visits <- as.factor(data$Count.Visits)
data$Count.Visits <- factor(data$Count.Visits, levels = c("0-1", "2+"), ordered = TRUE)
table(data$Count.Visits)
  
data$Phys.Health <- as.factor(data$Phys.Health)
data$Phys.Health <- factor(data$Phys.Health, 
                               levels = c("Poor", "Fair", "Good","VeryGood",
                                          "Excellent"),
                               ordered = TRUE)
table(data$Phys.Health)

data$Mental.Health <- as.factor(data$Mental.Health)
data$Mental.Health <- factor(data$Mental.Health, 
                                 levels = c("Poor", "Fair", "Good","VeryGood",
                                            "Excellent"),
                                 ordered = TRUE)
table(data$Mental.Health)

data$Dental.Health <- as.factor(data$Dental.Health)
table(data$Dental.Health)

data$Employment <- as.factor(data$Employment)
table(data$Employment)
  
data$Stress.vs.Sleep <- as.factor(data$Stress.vs.Sleep)
table(data$Stress.vs.Sleep)

data$Medication.vs.Sleep <- as.factor(data$Medication.vs.Sleep)
table(data$Medication.vs.Sleep)

data$Pain.vs.Sleep <- as.factor(data$Pain.vs.Sleep)
table(data$Pain.vs.Sleep)

data$Bathroom.vs.Sleep <- as.factor(data$Bathroom.vs.Sleep)
table(data$Bathroom.vs.Sleep)

data$Unknown.vs.Sleep <- as.factor(data$Unknown.vs.Sleep)
table(data$Unknown.vs.Sleep)

data$Trouble.Sleeping <- as.factor(data$Trouble.Sleeping)
data$Trouble.Sleeping <- factor(data$Trouble.Sleeping, 
                                    levels = c("LittleOrNot", "Some", "GreatDeal"),
                                    ordered = TRUE)
table(data$Trouble.Sleeping)

data$Sleep.Medication <- as.factor(data$Sleep.Medication)
data$Sleep.Medication <- factor(data$Sleep.Medication, 
                                    levels = c("Dont.Use", "Occasionally", "Regularly"),
                                    ordered = TRUE)
table(data$Sleep.Medication)

data$Race <- as.factor(data$Race)
table(data$Race)

data$Gender <- as.factor(data$Gender)
table(data$Gender)

# Sprawdzam, czy wszystko jest ok
str(data)
## Każda zmienna to factor, 5 z nich uporzątkowana


#===== 2. Analiza zbioru danych =====
table(data$Count.Visits)
# 0-1  2+ 
# 126 570

# proporcja danych
126/696 #0.181034
570/696 #0.8189655


# Chisq test?
## sprabdzam tablice kontyngencji dla
table(data$Mental.Health, data$Phys.Health)
## testu chi-kwadrat raczej nie użyjemy


# Ale próbujemy
res <- chisq.test(data$Mental.Health, data$Phys.Health)
## otrzymujemy ostrzeżenie
## "Aproksymacja chi-kwadrat może być niepoprawna"


# Więc używamy testu dokładnego fishera 

## Test
(xyz <- table(data$Mental.Health, data$Phys.Health))

#fisher.test(xyz)
### Brakuje symulacji?

fisher.test(xyz, simulate=TRUE)
fisher.gamma.test(xyz, B=10000)
### fisher gamma test bardziej widzi zależność akurat w tych danych


# Fisher gamma test dla całego zbioru zmiennych
## Przygotowanie data frame dla wyników
n <- 14 
column_names <- c("CV", "PH", "MH", 
                  "DH", "Emp", "SvS", 
                  "MvS", "PvS", "BvS", 
                  "UvS", "TS", "SM", 
                  "Rac", "Gen")

fgtest <- data.frame(matrix(NA, nrow = n, ncol = n))
colnames(fgtest) <- column_names
rownames(fgtest) <- column_names
print(fgtest)

## Test na całym zbiorze danych z B = 10 000
k<-0

# do wykresu użyłem parametru B = 10 000
#losowania <- 10000

# dla szybkości trwania kodu zostawiam B = 1000
losowania <- 1000

for(i in 1:14){
  for(j in 1:14){
    temp <- table(data[,i], data[,j])
    res <- fisher.gamma.test(temp, B=losowania)
    fgtest[i,j] <- res
    k <- k+1
    cat(k,"") # Dla obserwacji postępu, 14*14 daje 196 iteracji
  }
}

round(fgtest, 2)
# > round(fgtest, 2)
# CV   PH   MH   DH  Emp  SvS  MvS  PvS  BvS  UvS   TS   SM  Rac  Gen
# CV  0.00 0.99 0.63 0.76 0.03 0.04 0.02 0.07 0.26 0.76 0.30 0.00 0.22 0.26
# PH  0.99 0.00 0.00 0.00 0.99 0.80 1.00 1.00 0.48 0.33 1.00 1.00 0.02 0.60
# MH  0.62 0.00 0.00 0.00 0.95 1.00 1.00 1.00 0.84 0.09 1.00 1.00 0.14 0.46
# DH  0.76 0.00 0.01 0.00 0.53 0.47 0.45 0.70 0.16 0.82 0.86 0.61 0.23 0.42
# Emp 0.03 0.99 0.95 0.53 0.00 0.94 0.01 0.38 0.10 0.41 0.08 0.27 0.04 0.85
# SvS 0.03 0.80 1.00 0.47 0.94 0.00 0.37 0.00 0.51 1.00 0.00 0.22 0.92 1.00
# MvS 0.02 1.00 1.00 0.45 0.01 0.36 0.00 0.00 0.03 1.00 0.00 0.12 0.89 0.38
# PvS 0.08 1.00 1.00 0.68 0.38 0.00 0.00 0.00 0.00 1.00 0.00 0.00 0.60 0.58
# BvS 0.26 0.47 0.85 0.17 0.11 0.50 0.03 0.00 0.00 1.00 0.84 0.80 0.46 0.28
# UvS 0.76 0.34 0.08 0.81 0.41 1.00 1.00 1.00 1.00 0.00 0.33 0.28 0.16 0.83
# TS  0.30 1.00 1.00 0.85 0.08 0.00 0.00 0.00 0.84 0.32 0.00 0.00 0.51 0.99
# SM  0.00 1.00 1.00 0.62 0.27 0.23 0.12 0.00 0.81 0.28 0.00 0.00 0.08 0.21
# Rac 0.22 0.02 0.14 0.23 0.04 0.91 0.90 0.58 0.45 0.16 0.50 0.08 0.00 0.24
# Gen 0.26 0.61 0.45 0.41 0.85 1.00 0.39 0.59 0.28 0.83 0.99 0.21 0.25 0.00

# Mapa ciepła
library(pheatmap)
pheatmap(fgtest, 
         color = colorRampPalette(c("white", "blue"))(100), 
         main = "Mapa ciepła", 
         cluster_rows = FALSE, 
         cluster_cols = FALSE)



# Podział danych na treningowe i testowe
library(caret)

RNGversion("4.4.0")
set.seed(123)

parts <- createDataPartition(data$Count.Visits, p = .8, list = F)

train <- data[parts, ]
test <- data[-parts, ]

prop.table(table(train$Count.Visits))
# 0-1        2+ 
# 0.1813285 0.8186715 

prop.table(table(test$Count.Visits))
# 0-1        2+ 
# 0.1798561 0.8201439 
## mamy zachowaną proporcję całego zbioru danych

names(data)

#===== 3. Modele =====
library(caret)
library(mltools)
names(data)

#===== Model1: pełny bez interakcji =====
model0 <- glm(Count.Visits ~ 1, data=train, family=binomial)
summary(model0)
model1 <- glm(Count.Visits ~ . , data = train, family = binomial)
summary(model1)

# Phys.Health.L           -1.41460    0.79029  -1.790   0.0735 .
# Dental.HealthExcellent   1.61081    0.67157   2.399   0.0165 *
# Sleep.Medication.L       1.23439    0.74947   1.647   0.0996 .
# AIC: 539.63

# Anova: model pełny (nasycony) vs model pusty
anova(model0, model1)
## test odrzucił hipotezę zerową -> dodanie

# Standardowe reszty Pearsonowskie
reszty_std <- resid(model1, type="pearson")/sqrt(1-lm.influence(model1)$hat)
head(reszty_std)
plot(reszty_std, 
     main = "Model1", 
     xlab = "Indeks obserwacji", 
     ylab = "Standaryzowane reszty Pearsonowskie", 
     pch = 20, col = "blue")
abline(h = c(-2, 2), col = "red", lty = 2)  # Linie odniesienia
## Jeśli reszta przekracza ±2, obserwacja może być odstająca..
## Jeśli przekracza ±3, obserwacja jest wysoce odstająca i może wymagać uwagi.
 
# Prawdopodobieństwa
res <- predict(model1, type="response", newdata = test)
plot(res)
plot(sort(res))

# Znalezienie najlepszego progu decyzyjnego
# F1 jako najważniejsza metryka
# MCC i GMEAN jako pomocnicze
thresholds <- seq(0.6, 0.95, by = 0.005)
results <- data.frame(Threshold = thresholds, F1 = NA, MCC = NA, GMEAN = NA)

for (i in seq_along(thresholds)){
  # Predykcje
  pred <- ifelse(res > thresholds[i] , "2+", "0-1")
  pred <- as.factor(pred)
  pred <- factor(pred, levels = levels(data$Count.Visits))
  
  # Macierz błędów
  cm <- confusionMatrix(pred, test$Count.Visits)
  
  # Wyciąganie metryk
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  specificity <- cm$byClass["Specificity"]
  
  # F1-Score
  f1 <- 2 * (precision * recall) / (precision + recall)
  results$F1[i] <- f1
  
  # MCC
  results$MCC[i] <- mcc(pred, test$Count.Visits)
  
  # G-Mean
  results$GMEAN[i] <- sqrt(recall * specificity)
}

results

plot(results$Threshold, results$F1, type = "l")
plot(results$Threshold, results$MCC, type = "l")
plot(results$Threshold, results$GMEAN, type = "l")

results$Threshold[which.max(results$F1)] #0.86
results$Threshold[which.max(results$MCC)] #0.86
results$Threshold[which.max(results$GMEAN)] #0.815


# Teraz zanalizujmy dla tego najlepszego
threshold <- 0.86

## to co było opisane wczesniej, klikamy, dalej będą wyniki
pred <- ifelse(res > threshold , "2+", "0-1")
pred <- as.factor(pred)
pred <- factor(pred, levels = levels(data$Count.Visits))
cm <- confusionMatrix(pred, test$Count.Visits)
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
specificity <- cm$byClass["Specificity"]
f1 <- cm$byClass["F1"]
MCC <- mcc(pred, test$Count.Visits)
GMEAN <- sqrt(recall * specificity)

## Wyniki
cm$table

f1
MCC
GMEAN


# > cm$table
#           Reference
# Prediction  0-1 2+
#         0-1  22 65
#         2+    3 49
# F1 
# 0.3928571 
# > MCC
# [1] 0.2459104
# > GMEAN
# Recall 
# 0.6150168 



#===== Model2: step  =====
model2 <- step(model1, direction = "backward")
summary(model2)

# Dental.HealthExcellent   1.1417     0.6227   1.833   0.0668 .
# Sleep.Medication.L       1.3482     0.7284   1.851   0.0642 .
# AIC: 519.3

# Anova
anova(model1, model2)
## pwartość 0.69 -> nie ma 

# Standardowe reszty Pearsonowskie
reszty_std <- resid(model2, type="pearson")/sqrt(1-lm.influence(model2)$hat)
head(reszty_std)
plot(reszty_std, 
     main = "Model 2", 
     xlab = "Indeks obserwacji", 
     ylab = "Standaryzowane reszty Pearsonowskie", 
     pch = 20, col = "blue")
abline(h = c(-2, 2), col = "red", lty = 2)  # Linie odniesienia
## Jeśli reszta przekracza ±2, obserwacja może być odstająca..
## Jeśli przekracza ±3, obserwacja jest wysoce odstająca i może wymagać uwagi.


# Prawdopodobieństwa
res <- predict(model2, type="response", newdata = test)
plot(res)
plot(sort(res))

# Znalezienie najlepszego progu decyzyjnego
# F1 jako najważniejsza metryka
# MCC i GMEAN jako pomocnicze
thresholds <- seq(0.6, 0.95, by = 0.005)
results <- data.frame(Threshold = thresholds, F1 = NA, MCC = NA, GMEAN = NA)

for (i in seq_along(thresholds)){
  # Predykcje
  pred <- ifelse(res > thresholds[i] , "2+", "0-1")
  pred <- as.factor(pred)
  pred <- factor(pred, levels = levels(data$Count.Visits))
  
  # Macierz błędów
  cm <- confusionMatrix(pred, test$Count.Visits)
  
  # Wyciąganie metryk
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  specificity <- cm$byClass["Specificity"]
  
  # F1-Score
  f1 <- 2 * (precision * recall) / (precision + recall)
  results$F1[i] <- f1
  
  # MCC
  results$MCC[i] <- mcc(pred, test$Count.Visits)
  
  # G-Mean
  results$GMEAN[i] <- sqrt(recall * specificity)
}

results

plot(results$Threshold, results$F1, type = "l")
plot(results$Threshold, results$MCC, type = "l")
plot(results$Threshold, results$GMEAN, type = "l")

results$Threshold[which.max(results$F1)] #0.785
results$Threshold[which.max(results$MCC)] #0.785
results$Threshold[which.max(results$GMEAN)] #0.785
## Każda metryka zwróciła to samo !


# Teraz zanalizujmy dla tego najlepszego
threshold <- 0.785

## to co było opisane wczesniej, klikamy, dalej będą wyniki
pred <- ifelse(res > threshold , "2+", "0-1")
pred <- as.factor(pred)
pred <- factor(pred, levels = levels(data$Count.Visits))
cm <- confusionMatrix(pred, test$Count.Visits)
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
specificity <- cm$byClass["Specificity"]
f1 <- cm$byClass["F1"]
MCC <- mcc(pred, test$Count.Visits)
GMEAN <- sqrt(recall * specificity)

## Wyniki
cm$table

f1
MCC
GMEAN

#           Reference
#     Prediction 0-1 2+
#           0-1  12 32
#           2+   13 82
# F1 
# 0.3478261 
# > MCC
# [1] 0.1645651
# > GMEAN
# 0.587591 



#===== Model3: cloglog =====
model0c <- glm(Count.Visits ~1., data=train, family = binomial(link="cloglog"))
summary(model0c)
model3 <- glm(Count.Visits ~ . , data = train, family = binomial(link="cloglog"))

summary(model3)

# Phys.Health.L          -0.65320    0.31134  -2.098   0.0359 *
# Dental.HealthExcellent  0.67971    0.29529   2.302   0.0213 *
# Sleep.Medication.L      0.52270    0.25725   2.032   0.0422 *
# GenderMale              0.19813    0.11662   1.699   0.0893 .
# AIC: 537.57

# Anova
anova(model3, model0c)
## 0.008012 czyli model pełny lepszy


# Standardowe reszty Pearsonowskie
reszty_std <- resid(model3, type="pearson")/sqrt(1-lm.influence(model3)$hat)
head(reszty_std)
plot(reszty_std, 
     main = "Model 3", 
     xlab = "Indeks obserwacji", 
     ylab = "Standaryzowane reszty Pearsonowskie", 
     pch = 20, col = "blue")
abline(h = c(-2, 2), col = "red", lty = 2)  # Linie odniesienia
## Jeśli reszta przekracza ±2, obserwacja może być odstająca..
## Jeśli przekracza ±3, obserwacja jest wysoce odstająca i może wymagać uwagi.

## Jedna znacznie odstająca, nie widać skali pozostałych
reszty_std <- reszty_std[-which.max(reszty_std)]
plot(reszty_std, 
     main = "Model 3", 
     xlab = "Indeks obserwacji", 
     ylab = "Standaryzowane reszty Pearsonowskie", 
     pch = 20, col = "blue")
abline(h = c(-2, 2), col = "red", lty = 2)

  
# Prawdopodobieństwa
res <- predict(model3, type="response", newdata = test)
plot(res)
plot(sort(res))

# Znalezienie najlepszego progu decyzyjnego
# F1 jako najważniejsza metryka
# MCC i GMEAN jako pomocnicze
thresholds <- seq(0.6, 0.95, by = 0.005)
results <- data.frame(Threshold = thresholds, F1 = NA, MCC = NA, GMEAN = NA)

for (i in seq_along(thresholds)){
  # Predykcje
  pred <- ifelse(res > thresholds[i] , "2+", "0-1")
  pred <- as.factor(pred)
  pred <- factor(pred, levels = levels(data$Count.Visits))
  
  # Macierz błędów
  cm <- confusionMatrix(pred, test$Count.Visits)
  
  # Wyciąganie metryk
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  specificity <- cm$byClass["Specificity"]
  
  # F1-Score
  f1 <- 2 * (precision * recall) / (precision + recall)
  results$F1[i] <- f1
  
  # MCC
  results$MCC[i] <- mcc(pred, test$Count.Visits)
  
  # G-Mean
  results$GMEAN[i] <- sqrt(recall * specificity)
}

results

plot(results$Threshold, results$F1, type = "l")
plot(results$Threshold, results$MCC, type = "l")
plot(results$Threshold, results$GMEAN, type = "l")

results$Threshold[which.max(results$F1)] #0.81
results$Threshold[which.max(results$MCC)] #0.82
results$Threshold[which.max(results$GMEAN)] #0.81


# Teraz zanalizujmy dla tego najlepszego
threshold <- 0.81

## to co było opisane wczesniej, klikamy, dalej będą wyniki
pred <- ifelse(res > threshold , "2+", "0-1")
pred <- as.factor(pred)
pred <- factor(pred, levels = levels(data$Count.Visits))
cm <- confusionMatrix(pred, test$Count.Visits)
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
specificity <- cm$byClass["Specificity"]
f1 <- cm$byClass["F1"]
MCC <- mcc(pred, test$Count.Visits)
GMEAN <- sqrt(recall * specificity)

## Wyniki
cm$table

f1
MCC
GMEAN

# Dla step
# > cm$table
#           Reference
# Prediction 0-1 2+
#       0-1  18 44
#       2+    7 70
# F1 
# 0.4137931  
# > MCC
# [1] 0.2580913
# > GMEAN
# 0.66491 



#===== Model4: cloglog step =====
model4 <- step(model3, direction = "backward")

summary(model4)
anova(model4, model3)
# Phys.Health.L          -0.65320    0.31134  -2.098   0.0359 *
# Dental.HealthExcellent  0.67971    0.29529   2.302   0.0213 *
# Sleep.Medication.L      0.52270    0.25725   2.032   0.0422 *
# GenderMale              0.19813    0.11662   1.699   0.0893 .
# AIC=518.49

# Standardowe reszty Pearsonowskie
reszty_std <- resid(model4, type="pearson")/sqrt(1-lm.influence(model4)$hat)
head(reszty_std)
plot(reszty_std, 
     main = "Model 4", 
     xlab = "Indeks obserwacji", 
     ylab = "Standaryzowane reszty Pearsonowskie", 
     pch = 20, col = "blue")
abline(h = c(-2, 2), col = "red", lty = 2)  # Linie odniesienia
## Jeśli reszta przekracza ±2, obserwacja może być odstająca..
## Jeśli przekracza ±3, obserwacja jest wysoce odstająca i może wymagać uwagi.


# Prawdopodobieństwa
res <- predict(model4, type="response", newdata = test)
plot(res)
plot(sort(res))

# Znalezienie najlepszego progu decyzyjnego
# F1 jako najważniejsza metryka
# MCC i GMEAN jako pomocnicze
thresholds <- seq(0.6, 0.95, by = 0.005)
results <- data.frame(Threshold = thresholds, F1 = NA, MCC = NA, GMEAN = NA)

for (i in seq_along(thresholds)){
  # Predykcje
  pred <- ifelse(res > thresholds[i] , "2+", "0-1")
  pred <- as.factor(pred)
  pred <- factor(pred, levels = levels(data$Count.Visits))
  
  # Macierz błędów
  cm <- confusionMatrix(pred, test$Count.Visits)
  
  # Wyciąganie metryk
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  specificity <- cm$byClass["Specificity"]
  
  # F1-Score
  f1 <- 2 * (precision * recall) / (precision + recall)
  results$F1[i] <- f1
  
  # MCC
  results$MCC[i] <- mcc(pred, test$Count.Visits)
  
  # G-Mean
  results$GMEAN[i] <- sqrt(recall * specificity)
}

results

plot(results$Threshold, results$F1, type = "l")
plot(results$Threshold, results$MCC, type = "l")
plot(results$Threshold, results$GMEAN, type = "l")

results$Threshold[which.max(results$F1)] #0.79
results$Threshold[which.max(results$MCC)] #0.91
results$Threshold[which.max(results$GMEAN)] #0.79


# Teraz zanalizujmy dla tego najlepszego
threshold <- 0.79

## to co było opisane wczesniej, klikamy, dalej będą wyniki
pred <- ifelse(res > threshold , "2+", "0-1")
pred <- as.factor(pred)
pred <- factor(pred, levels = levels(data$Count.Visits))
cm <- confusionMatrix(pred, test$Count.Visits)
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
specificity <- cm$byClass["Specificity"]
f1 <- cm$byClass["F1"]
MCC <- mcc(pred, test$Count.Visits)
GMEAN <- sqrt(recall * specificity)

## Wyniki
cm$table

f1
MCC
GMEAN

# Dla step
# > cm$table
#           Reference
# Prediction 0-1 2+
#       0-1  13 34
#       2+   12 80
# F1 
# 0.3611111 
# > MCC
# [1] 0.1800329
# > GMEAN
# 0.6040797




#===== 4 Wnioski =====
summary(model4)

# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)  
# (Intercept)             3.27686   60.25594   0.054   0.9566  
# Phys.Health.L          -0.65320    0.31134  -2.098   0.0359 *
# Phys.Health.Q          -0.05373    0.25559  -0.210   0.8335  
# Phys.Health.C           0.04331    0.18599   0.233   0.8159  
# Phys.Health^4          -0.09991    0.11812  -0.846   0.3976  
# Dental.HealthExcellent  0.67971    0.29529   2.302   0.0213 *
# Dental.HealthFair      -0.19336    0.24799  -0.780   0.4356  
# Dental.HealthGood       0.18726    0.23769   0.788   0.4308  
# Dental.HealthPoor       0.07633    0.33512   0.228   0.8198  
# Dental.HealthVeryGood   0.21799    0.24426   0.892   0.3722  
# Stress.vs.SleepYes      0.22140    0.13521   1.637   0.1016  
# Sleep.Medication.L      0.52270    0.25725   2.032   0.0422 *
# Sleep.Medication.Q      0.01554    0.28120   0.055   0.9559  
# RaceBlack.NoHis        -2.48563   60.25567  -0.041   0.9671  
# RaceHispanic           -3.13841   60.25586  -0.052   0.9585  
# RaceOther.NoHis        -3.10106   60.25621  -0.051   0.9590  
# RaceWhite.NoHis        -2.62248   60.25539  -0.044   0.9653  
# GenderMale              0.19813    0.11662   1.699   0.0893 .



# Koniec