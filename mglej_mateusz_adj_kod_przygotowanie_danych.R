#===== 1. Wczytanie danych =====
data_original <- read.csv("NPHA-doctor-visits.csv")
data_new <- read.csv("NPHA-doctor-visits.csv")

# Wyświetlam oryginalne nazwy kolumn (zmiennych)
colnames(data_original)



#===== 2. Number.of.Doctors.Visited =====
# Zmiana na krótszą nazwę "Count.Visits"
colnames(data_new)[colnames(data_new) == "Number.of.Doctors.Visited"] <- "Count.Visits"

# Zamiast 3 klas, tworzę 2
# Klasa 1 -> "0-1"
# Klasy 2 i 3 -> "2+"
data_new$Count.Visits <- ifelse(data_original$Number.of.Doctors.Visited == 1, "0-1", "2+")
data_new$Count.Visits <- as.factor(data_new$Count.Visits)
data_new$Count.Visits <- factor(data_new$Count.Visits, levels = c("0-1", "2+"), ordered = TRUE)

table(data_new$Count.Visits)



#===== 3. Age =====
table(data_original$Age)


# Mamy tylko jedną klasę "65-80" -> usuwamy tę zmienną
x <- which(colnames(data_new) == "Age")
data_new <- data_new[,-x]



#===== 4. Phyiscal.Health =====
# Zmiana na krótszą nazwę "Phys.Healh"
colnames(data_new)[colnames(data_new) == "Phyiscal.Health"] <- "Phys.Health"
table(data_new$Phys.Health)

# usuwam jedną obserwację z "-1"
data_new <- data_new[data_new$Phys.Health != -1, ]
backup <- table(data_new$Phys.Health)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Phys.Health==1)
x2 <- which(data_new$Phys.Health==2)
x3 <- which(data_new$Phys.Health==3)
x4 <- which(data_new$Phys.Health==4)
x5 <- which(data_new$Phys.Health==5)

data_new[x1,2] <- "Excellent"
data_new[x2,2] <- "VeryGood"
data_new[x3,2] <- "Good"
data_new[x4,2] <- "Fair"
data_new[x5,2] <- "Poor"

data_new$Phys.Health <- as.factor(data_new$Phys.Health)
data_new$Phys.Health <- factor(data_new$Phys.Health, 
                               levels = c("Poor", "Fair", "Good","VeryGood",
                                          "Excellent"),
                               ordered = TRUE)
table(data_new$Phys.Health)
backup



#===== 5. Mental.Health =====
table(data_new$Mental.Health)

# usuwam 10 obserwacji z "-1"
data_new <- data_new[data_new$Mental.Health != -1, ]
backup <- table(data_new$Mental.Health)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Mental.Health==1)
x2 <- which(data_new$Mental.Health==2)
x3 <- which(data_new$Mental.Health==3)
x4 <- which(data_new$Mental.Health==4)
x5 <- which(data_new$Mental.Health==5)

data_new[x1,3] <- "Excellent"
data_new[x2,3] <- "VeryGood"
data_new[x3,3] <- "Good"
data_new[x4,3] <- "Fair"
data_new[x5,3] <- "Poor"

data_new$Mental.Health <- as.factor(data_new$Mental.Health)
data_new$Mental.Health <- factor(data_new$Mental.Health, 
                               levels = c("Poor", "Fair", "Good","VeryGood",
                                          "Excellent"),
                               ordered = TRUE)
table(data_new$Mental.Health)
backup



#===== 6. Dental.Health =====
table(data_new$Dental.Health)

# usuwam 3 obserwacji z "-1"
data_new <- data_new[data_new$Dental.Health != -1, ]
backup <- table(data_new$Dental.Health)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Dental.Health==1)
x2 <- which(data_new$Dental.Health==2)
x3 <- which(data_new$Dental.Health==3)
x4 <- which(data_new$Dental.Health==4)
x5 <- which(data_new$Dental.Health==5)
x5 <- which(data_new$Dental.Health==5)
x6 <- which(data_new$Dental.Health==6)

data_new[x1,4] <- "Excellent"
data_new[x2,4] <- "VeryGood"
data_new[x3,4] <- "Good"
data_new[x4,4] <- "Fair"
data_new[x5,4] <- "Poor"
data_new[x6,4] <- "Dentures"

# To będzie zmienna nominalna - "dentures" oznacza po prostu posiadanie protezy
data_new$Dental.Health <- as.factor(data_new$Dental.Health)

table(data_new$Dental.Health)
backup



#===== 7. Employment =====
table(data_new$Employment)

backup <- table(data_new$Employment)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Employment==1)
x2 <- which(data_new$Employment==2)
x3 <- which(data_new$Employment==3)
x4 <- which(data_new$Employment==4)

data_new[x1,5] <- "Full-Time"
data_new[x2,5] <- "Part-Time"
data_new[x3,5] <- "Retired"
data_new[x4,5] <- "Not-Working"

# To będzie zmienna jakościowa nominalna
data_new$Employment <- as.factor(data_new$Employment)

table(data_new$Employment)
backup



#===== 8. Stress.Keeps.Patient.from.Sleeping =====
# Zmiana na krótszą nazwę "Stress.vs.Sleep"
colnames(data_new)[colnames(data_new) == "Stress.Keeps.Patient.from.Sleeping"] <- "Stress.vs.Sleep"
table(data_new$Stress.vs.Sleep)

backup <- table(data_new$Stress.vs.Sleep)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Stress.vs.Sleep==0)
x2 <- which(data_new$Stress.vs.Sleep==1)

data_new[x1,6] <- "No"
data_new[x2,6] <- "Yes"

# Zmienna jakościowa nominalna typu Yes/No
data_new$Stress.vs.Sleep <- as.factor(data_new$Stress.vs.Sleep)

table(data_new$Stress.vs.Sleep)
backup



#===== 9. Medication.Keeps.Patient.from.Sleeping =====
# Zmiana na krótszą nazwę "Medication.vs.Sleep"
colnames(data_new)[colnames(data_new) == "Medication.Keeps.Patient.from.Sleeping"] <- "Medication.vs.Sleep"
table(data_new$Medication.vs.Sleep)

backup <- table(data_new$Medication.vs.Sleep)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Medication.vs.Sleep==0)
x2 <- which(data_new$Medication.vs.Sleep==1)

data_new[x1,7] <- "No"
data_new[x2,7] <- "Yes"

# Zmienna jakościowa nominalna typu Yes/No
data_new$Medication.vs.Sleep <- as.factor(data_new$Medication.vs.Sleep)

table(data_new$Medication.vs.Sleep)
backup



#===== 10. Pain.Keeps.Patient.from.Sleeping =====
# Zmiana na krótszą nazwę "Pain.vs.Sleep"
colnames(data_new)[colnames(data_new) == "Pain.Keeps.Patient.from.Sleeping"] <- "Pain.vs.Sleep"
table(data_new$Pain.vs.Sleep)

backup <- table(data_new$Pain.vs.Sleep)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Pain.vs.Sleep==0)
x2 <- which(data_new$Pain.vs.Sleep==1)

data_new[x1,8] <- "No"
data_new[x2,8] <- "Yes"

# Zmienna jakościowa nominalna typu Yes/No
data_new$Pain.vs.Sleep <- as.factor(data_new$Pain.vs.Sleep)

table(data_new$Pain.vs.Sleep)
backup



#===== 11. Bathroom.Needs.Keeps.Patient.from.Sleeping =====
# Zmiana na krótszą nazwę "Bathroom.vs.Sleep"
colnames(data_new)[colnames(data_new) == "Bathroom.Needs.Keeps.Patient.from.Sleeping"] <- "Bathroom.vs.Sleep"
table(data_new$Bathroom.vs.Sleep)

backup <- table(data_new$Bathroom.vs.Sleep)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Bathroom.vs.Sleep==0)
x2 <- which(data_new$Bathroom.vs.Sleep==1)

data_new[x1,9] <- "No"
data_new[x2,9] <- "Yes"

# Zmienna jakościowa nominalna typu Yes/No
data_new$Bathroom.vs.Sleep <- as.factor(data_new$Bathroom.vs.Sleep)

table(data_new$Bathroom.vs.Sleep)
backup



#===== 12. Uknown.Keeps.Patient.from.Sleeping =====
# Zmiana na krótszą nazwę "Unknown.vs.Sleep"
colnames(data_new)[colnames(data_new) == "Uknown.Keeps.Patient.from.Sleeping"] <- "Unknown.vs.Sleep"
table(data_new$Unknown.vs.Sleep)

backup <- table(data_new$Unknown.vs.Sleep)

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Unknown.vs.Sleep==0)
x2 <- which(data_new$Unknown.vs.Sleep==1)

data_new[x1,10] <- "No"
data_new[x2,10] <- "Yes"

# Zmienna jakościowa nominalna typu Yes/No
data_new$Unknown.vs.Sleep <- as.factor(data_new$Unknown.vs.Sleep)

table(data_new$Unknown.vs.Sleep)
backup



#===== 13. Trouble.Sleeping =====
table(data_new$Trouble.Sleeping)

# usuwam 2 obserwacji z "-1"
data_new <- data_new[data_new$Trouble.Sleeping != -1, ]
backup <- table(data_new$Trouble.Sleeping)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Trouble.Sleeping==1)
x2 <- which(data_new$Trouble.Sleeping==2)
x3 <- which(data_new$Trouble.Sleeping==3)

data_new[x1,11] <- "GreatDeal"
data_new[x2,11] <- "Some"
data_new[x3,11] <- "LittleOrNot"

# Zmienna jakościowa uporzątkowana
data_new$Trouble.Sleeping <- as.factor(data_new$Trouble.Sleeping)
data_new$Trouble.Sleeping <- factor(data_new$Trouble.Sleeping, 
                                 levels = c("LittleOrNot", "Some", "GreatDeal"),
                                 ordered = TRUE)

table(data_new$Trouble.Sleeping)
backup



#===== 14. Prescription.Sleep.Medication =====
# Zmiana na krótszą nazwę "Sleep.Medication"
colnames(data_new)[colnames(data_new) == "Prescription.Sleep.Medication"] <- "Sleep.Medication"
table(data_new$Sleep.Medication)

# usuwam 2 obserwacji z "-1"
data_new <- data_new[data_new$Sleep.Medication != -1, ]
backup <- table(data_new$Sleep.Medication)
backup

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Sleep.Medication==1)
x2 <- which(data_new$Sleep.Medication==2)
x3 <- which(data_new$Sleep.Medication==3)

data_new[x1,12] <- "Regularly"
data_new[x2,12] <- "Occasionally"
data_new[x3,12] <- "Dont.Use"

# Zmienna jakościowa uporzątkowana
data_new$Sleep.Medication <- as.factor(data_new$Sleep.Medication)
data_new$Sleep.Medication <- factor(data_new$Sleep.Medication, 
                                    levels = c("Dont.Use", "Occasionally", "Regularly"),
                                    ordered = TRUE)

table(data_new$Sleep.Medication)
backup



#===== 15. Race =====
table(data_new$Race)
backup <- table(data_new$Race)

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Race==1)
x2 <- which(data_new$Race==2)
x3 <- which(data_new$Race==3)
x4 <- which(data_new$Race==4)
x5 <- which(data_new$Race==5)

data_new[x1,13] <- "White.NoHis"
data_new[x2,13] <- "Black.NoHis"
data_new[x3,13] <- "Other.NoHis"
data_new[x4,13] <- "Hispanic"
data_new[x5,13] <- "2+Races.NoHis"

# Zmienna jakościowa nominalna
data_new$Race <- as.factor(data_new$Race)

table(data_new$Race)
backup



#===== 16. Gender =====
table(data_new$Gender)
backup <- table(data_new$Gender)

# Zmiana z liczbowych na słowne
## Szukam konkretnych indeksów
x1 <- which(data_new$Gender==1)
x2 <- which(data_new$Gender==2)

data_new[x1,14] <- "Male"
data_new[x2,14] <- "Female"

# Zmienna jakościowa nominalna
data_new$Gender <- as.factor(data_new$Gender)

table(data_new$Gender)
backup



#===== 17. Podsumowanie =====
summary(data_new)
str(data_new)

## Wszystkie zmienne są jakościowe, 5 uporządkowanych

# resetuję numery wierszy, bo brakuje po usunięciu kilku obserwacji
rownames(data_new) <- NULL
data <- data_new

# Zapisanie do pliku CSV
write.csv(data, "dane_przygotowane.csv", row.names = FALSE)

# Zapisanie do pliku RData -> zachowanie informacji o factorach
#save(data, file = "dane_przygotowane_factor.RData")

























