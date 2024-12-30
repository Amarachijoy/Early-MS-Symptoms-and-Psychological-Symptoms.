
#Exploring the assosciation between early MS symptoms and psychological symptoms.

#Setting work directory

getwd()
main_dir <- getwd()
load_dir <- "S:/Datathon - UK MS Register Datathon/DataIn"
setwd(load_dir)
setwd(main_dir)
?read.csv

Demographics <- read.csv("S:/Datathon - UK MS Register Datathon/DataIn/Portal/Short_Demographics_Portal_final.csv", na.strings = c(''),
                             strip.white = T,
                             stringsAsFactors = F)
Early_symptoms <- read.csv("S:/Datathon - UK MS Register Datathon/DataIn/Clinical/Onset_Symptoms_Clinical.csv", 
                           na.strings = c(''),
                           strip.white = T,
                           stringsAsFactors = F)
HADS <- read.csv("S:/Datathon - UK MS Register Datathon/DataIn/Portal/HADS_Sums.csv",
                 na.strings = c(''), strip.white = T, stringsAsFactors = F)

StudyID_ClinicalID <- read.csv("S:/Datathon - UK MS Register Datathon/DataIn/Clinical/StudyId_Clinical.csv",
                               na.strings = c(''), stringsAsFactors = F, strip.white = T)
#Remove Duplicates #Data Cleaning
Demographics <- distinct(Demographics)
Early_symptoms <- distinct(Early_symptoms)
Demographics <- Demographics %>% filter(!duplicated(Demographics$UserId))
Demographics <- Demographics %>% filter(!duplicated(Demographics$StudyID))
Demographics <- Demographics %>% filter(!duplicated(Demographics))
Demographics <- Demographics %>% rename(StudyId = StudyID)

Early_symptoms <- Early_symptoms %>% filter(!duplicated(Early_symptoms$ClinicalId))
Early_symptoms <- Early_symptoms %>% filter(!duplicated(Early_symptoms))

HADS <- HADS %>% filter(!duplicated(HADS$UserId))
HADS <- HADS %>% filter(!duplicated(HADS))

StudyID_ClinicalID <- StudyID_ClinicalID %>% filter(!duplicated(StudyID_ClinicalID$StudyId))
StudyID_ClinicalID <- StudyID_ClinicalID %>% filter(!duplicated(StudyID_ClinicalID))


Demographics <- unique(Demographics)
Early_symptoms <- unique(Early_symptoms)
HADS <- unique(HADS)
StudyID_ClinicalID <- unique(StudyID_ClinicalID)

#Calculate OnsetAge
Demographics <- Demographics %>% mutate(OnsetAge = Demographics$OnsetYear - Demographics$YearOfBirth)
#Identifying the OnsetAge stratify the patients into early and late MS Onset. This informs whether early onset MS causes higher prevalence
#or severity of psychological conditions or not. 
#Calculate TimeSinceDiagnosis
Demographics <- Demographics %>% mutate(TimeSinceDiagnosis = 2024 - Demographics$DiagnosisYear )
#TimeSinceDiagnosis was created as it helps to understand the duration of MS from onset and Diagnosis. The duration of MS in a patient may determine 
#the prevalence or severity of psychological conditions. Simply, It explores whether psychological symptoms are more likely to develop soon after the onset
#of MS or later on. 

#Data Manipulation

Demographics <- Demographics %>% select(-DiagnosisYear, -DiagnosisWeek, -OnsetWeek, -OnsetYear, -WeekOfBirth, -YearOfBirth)
HADS <- HADS %>% select(-anxiety_sums_norm, -depression_sums_norm)
HADS <- HADS %>% mutate(Anxiety_category = cut(anxiety_sums, breaks = c(0,7,10,21), 
                                               include.lowest = T,
                                               labels = c('Normal', 'Borderline', 'Abnormal'),
                                               ordered_result = T))
HADS <- HADS %>% mutate(Depression_category = cut(depression_sums, breaks = c(0,7,10,21),
                                                  include.lowest = T,
                                                  labels = c('Normal', 'Borderline', 'Abnormal'),
                                                  ordered_result = T))

 #Joining tables
DEM_HADS <- inner_join(Demographics, HADS, by = "UserId")
DEM_HADS_ID <- inner_join(DEM_HADS, StudyID_ClinicalID, by = "StudyId")
Data <- inner_join(DEM_HADS_ID, Early_symptoms, by = "ClinicalId")
Data <- Data %>% select(UserId, StudyId, ClinicalId, everything())

#Is there a significant relationship between gender and Depression
chisq.test(x = Data$Gender,
           y = Data$Depression_category)
#p value is less than 0.05 so we fail tho reject the  null hypothesis indicating a significant association.
chisq.test(x = Data$Gender,
           y = Data$Anxiety_category)
#No relationship
chisq.test(x = Data$Depression_category,
           y = Data$Anxiety_category)
#there is a relationship between OnsetAge and Depression?
Anova_result <- aov(OnsetAge ~ Depression_category, data = Data)
summary(Anova_result)
#There is a signifabt difference between age at onset and Depression levels. 
Anova_result_2 <- aov(OnsetAge ~ Anxiety_category, data = Data)
summary(Anova_result_2)
#There is a significant difference between OnsetAge and Anxiety.

#Visualisation
boxplot(OnsetAge ~ Depression_category, data = Data, main = "Depression levels at early symptoms", xlab = "Depression Levels", 
        ylab = "Age at Onset", col = c("light blue", "white", "dark blue"))
plot(Data$depression_sums, Data$OnsetAge, main = "Depression levels and Onset Age", 
     xlab = "Depression Sum", ylab = "Age at Onset", col = "light blue", pch = 18)
hist(Data$depression_sums, main = "Depression levels", xlab = "Depression", col = "light blue", border = "white" )

boxplot(OnsetAge ~ Anxiety_category, data = Data, main = "Anxiety levels at early symptoms", xlab = "Anxiety Levels", 
        ylab = "Age at Onset", col = c("light blue", "white", "dark blue"))
plot(Data$anxiety_sums, Data$OnsetAge, main = "Anxiety levels and Onset Age", 
     xlab = "Anxiety Sum", ylab = "Age at Onset", col = "light blue", pch = 18)
hist(Data$anxiety_sums, main = "Anxiety levels", xlab = "Anxiety Sum", col = "light blue", border = "white" )

#Table1
library(compareGroups)

table_1 <- Data
names(table_1)

table_1_Depression <- compareGroups(formula = Depression_category ~
                Vision + Coordination + Cognitive + Motor + BladderBowel + 
                Encephalopathy + Fatigue + Sensory + Other + OnsetAge + TimeSinceDiagnosis,
              data = table_1,
              method = c(3,3,3, 3, 3, 3, 3, 3, 3, 1, 1))
table_2 <- Data
names(table_2)

table_2_Anxiety <- compareGroups(formula = Anxiety_category ~
                                   Vision + Coordination + Cognitive + Motor + BladderBowel + 
                                   Encephalopathy + Fatigue + Sensory + Other + OnsetAge + TimeSinceDiagnosis,
                                 data = table_2,
                                 method = c(3,3,3, 3, 3, 3, 3, 3, 3, 1, 1))

table_1 <-
  createTable(table_1_Depression);table_1


table_2 <- createTable(table_2_Anxiety);table_2





