##### ##### ##### #####    Get additional data from excel files ##### ##### ##### #####
#                                  November 2019 
#                                     
# 
# Load helper functions
setwd("")
source('./MSc_Thesis_LindaTempel/r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr','foreign')
getPacks(pkgs)
rm(pkgs)

# ------1) Read in data from file------

Data_file <- read.csv(file = "./Codierung_Akte.csv", sep = ";", header=T)

# Rename variables

names(Data_file) <- c('VP_2', 'Crime','TypeCrime1', 'TypeCrime2', 'Drug1', 'Drug2', 'Drug3',
                      'Drug4', 'Drug5', 'Nicotine_Add','Treatment', 'Therapy', 
                      'Substitution', 'Comorbid', 'Comorbid1', 'Comorbid2', 'Med1', 'Med2', 'Education', 'Work')
                     

# Create and label factor variables

Data_file$Crime<-factor(Data_file$Crime, levels = c(1,2), 
                             labels=c("Yes", "No"))


Data_file$TypeCrime1<-factor(Data_file$TypeCrime1, levels =c(1,2,3, 4), labels =c("Theft",
                                                                               "Drugs", 
                                                                               "Other", 
                                                                               "Assault"))

Data_file$TypeCrime2<-factor(Data_file$TypeCrime2, levels =c(1,2,3, 4), labels =c("Theft",
                                                                                  "Drugs", 
                                                                                  "Other", 
                                                                                  "Assault"))

Data_file$Drug1<-factor(Data_file$Drug1, levels =c(1,2,3,4,5,6,7,8), labels =c("Alcohol", 
                                                                               "Opioids", 
                                                                               "Cannabinoids", 
                                                                               "Sedative hypnotics", 
                                                                               "Cocaine", 
                                                                               "Other Stimulants", 
                                                                               "Hallucinogens", 
                                                                               "Solvents"))

Data_file$Drug2<-factor(Data_file$Drug2, levels =c(1,2,3,4,5,6,7,8), labels =c("Alcohol", 
                                                                               "Opioids", 
                                                                               "Cannabinoids", 
                                                                               "Sedative hypnotics", 
                                                                               "Cocaine", 
                                                                               "Other Stimulants", 
                                                                               "Hallucinogens", 
                                                                               "Solvents"))

Data_file$Drug3<-factor(Data_file$Drug3, levels =c(1,2,3,4,5,6,7,8), labels =c("Alcohol", 
                                                                               "Opioids", 
                                                                               "Cannabinoids", 
                                                                               "Sedative hypnotics", 
                                                                               "Cocaine", 
                                                                               "Other Stimulants", 
                                                                               "Hallucinogens", 
                                                                               "Solvents"))

Data_file$Drug4<-factor(Data_file$Drug4, levels =c(1,2,3,4,5,6,7,8), labels =c("Alcohol", 
                                                                               "Opioids", 
                                                                               "Cannabinoids", 
                                                                               "Sedative hypnotics", 
                                                                               "Cocaine", 
                                                                               "Other Stimulants", 
                                                                               "Hallucinogens", 
                                                                               "Solvents"))

Data_file$Drug5<-factor(Data_file$Drug5, levels =c(1,2,3,4,5,6,7,8), labels =c("Alcohol", 
                                                                               "Opioids", 
                                                                               "Cannabinoids", 
                                                                               "Sedative hypnotics", 
                                                                               "Cocaine", 
                                                                               "Other Stimulants", 
                                                                               "Hallucinogens", 
                                                                               "Solvents"))

Data_file$Nicotine_Add<-factor(Data_file$Nicotine_Add, levels = c(1,2), 
                        labels=c("Yes", "No"))



Data_file$Therapy<-factor(Data_file$Therapy, levels = c(1,2), 
                               labels=c("Yes", "No"))

Data_file$Substitution<-factor(Data_file$Substitution, levels = c(1,2), 
                               labels=c("Yes", "No"))

Data_file$Comorbid<-factor(Data_file$Comorbid, levels = c(1,2), 
                               labels=c("Yes", "No"))

Data_file$Comorbid1<-factor(Data_file$Comorbid1, levels = c(1,2,3,4,5,6), 
                              labels=c("Personality disorders", "Affective disorders", 
                                       "Somatoform disorders", "Substance-induced psychosis", 
                                       "PTSD", "Pathological gambling"))

Data_file$Comorbid2<-factor(Data_file$Comorbid2, levels = c(1,2,3,4,5,6), 
                            labels=c("Personality disorders", "Affective disorders", 
                                     "Somatoform disorders", "other disorders from substances", 
                                     "PTSD", "Pathological gambling"))

Data_file$Med1<-factor(Data_file$Med1, levels = c(1,2), 
                              labels=c("Yes", "No"))

Data_file$Med2<-factor(Data_file$Med2, levels = c(0,1,2), 
                              labels=c("None", "Antipsychotics", "Antidepressants"))

Data_file$Education<-factor(Data_file$Education, levels =c(1,2,3,4), labels =c("None", 
                                                                               "Hauptschule", 
                                                                               "Realschule",
                                                                               "(Fach)abitur"))

Data_file$Work<-factor(Data_file$Work, levels = c(1,2), 
                               labels=c("Yes", "No"))




# Add variable drug with two categories: cannabis/alcohol vs opioids/stimulants

for (i in 1:nrow(Data_file)) {
  
  if (Data_file[i, 5]=='Alcohol'){
      Data_file[i,21]<-1
    
  } else if  (Data_file[i, 5] == 'Cannabinoids' ) {
    Data_file[i,21] <- 1

  } else if  (Data_file[i, 5] == 'Opioids' ) {
    Data_file[i,21] <- 2
    
  } else if  (Data_file[i, 5] == 'Cocaine' ) {
    Data_file[i,21] <- 2
    
  } else if  (Data_file[i, 5] == 'Other Stimulants' ) {
    Data_file[i,21] <- 2
    
    
  }
  
}


rm(i)
names(Data_file)[21] <- c('Drug')

Data_file$Drug<-factor(Data_file$Drug, levels =c(1,2), labels =c('Alcohol/Cannabis','Opioids/Stimulants'))



# Combine with other patient ID

for (i in 1:nrow(Data_file)) {
  
  if (Data_file[i, 1]=='1401'){
    Data_file[i,22]<-'1560'

  } else if  (Data_file[i, 1] == '1090' ) {
    Data_file[i,22] <-'1996'
    
  } else if  (Data_file[i, 1] == '1780' ) {
    Data_file[i,22] <- '1320'
    
  } else if  (Data_file[i, 1] == '1180' ) {
    Data_file[i,22] <- '1152'
    
  } else if  (Data_file[i, 1] == '1287' ) {
    Data_file[i,22] <- '1145'
    
  } else if  (Data_file[i, 1] == '1850' ) {
    Data_file[i,22] <- '1227'

  } else if  (Data_file[i, 1] == '1053' ) {
    Data_file[i,22] <- '1073'
    
  } else if  (Data_file[i, 1] == '1059' ) {
    Data_file[i,22] <- '1127'
    
  } else if  (Data_file[i, 1] == '1284' ) {
    Data_file[i,22] <- '1302'
    
  } else if  (Data_file[i, 1] == '1848' ) {
    Data_file[i,22] <- '1023'
    
  } else if  (Data_file[i, 1] == '1619'  ) {
    Data_file[i,22] <- '1838'
    
  } else if  (Data_file[i, 1] == '1376' ) {
    Data_file[i,22] <- '1355' 
    
  } else if  (Data_file[i, 1] == '1852'  ) {
    Data_file[i,22] <- '1600'
    
  } else if  (Data_file[i, 1] == '1637' ) {
    Data_file[i,22] <- '1164' 
    
  } else if  (Data_file[i, 1] == '1426'  ) {
    Data_file[i,22] <- '1621'
    
  } else if  (Data_file[i, 1] == '1441' ) {
    Data_file[i,22] <- '1531' 
    
  } else if  (Data_file[i, 1] == '1111'  ) {
    Data_file[i,22] <- '1409'
    
  } else if  (Data_file[i, 1] == '1583'  ) {
    Data_file[i,22] <- '1881'
    
  } else if  (Data_file[i, 1] == '1890'  ) {
    Data_file[i,22] <- '1878'
    
  } else if  (Data_file[i, 1] == '1512'  ) {
    Data_file[i,22] <- '1898'
    
  } else if  (Data_file[i, 1] == '1140'  ) {
    Data_file[i,22] <- '1296'
    
  } else if  (Data_file[i, 1] == '1717'  ) {
    Data_file[i,22] <- '1986'
    
  }  
}


names(Data_file)[22] <-c('VP')
rm(i)


# ------2) Read in data from interview ------

Data_interview <- read.csv(file = "./Codierung_Nachbefragung.csv", sep = ";", header=T)

# Rename variables

names(Data_interview) <- c('VP_2','VP', 'Head', 'Withdrawal', 'Caffeine', 'Nicotine', 
                           'Smokebreak', 'Payment','First_month', 'Study_month', 'Length')


# Create and label factor variables

Data_interview$Head<-factor(Data_interview$Head, levels = c(1,2), 
                        labels=c("Yes", "No"))

Data_interview$Withdrawal<-factor(Data_interview$Withdrawal, levels = c(1,2), 
                            labels=c("Yes", "No"))

Data_interview$Smokebreak<-factor(Data_interview$Smokebreak, levels = c(1,2), 
                            labels=c("Yes", "No"))

Data_interview$Payment<-factor(Data_interview$Payment, levels = c(1,2), 
                                  labels=c("Yes", "No"))

# ------3) Read in data from file containing outcome variables----------------------

Data_outcome <- read.csv(file = "./Codierung_Outcomes.csv", sep = ";", header=T)

# Rename variables

names(Data_outcome) <- c('VP_2','VP', 'Number_Incidents', 'Serious_Incident_Therapy',
                         'Serious_Incident_Behav', 'Endstatus',
                         'Rules_scale', 'Rules', 'Relapse', 'WAI')

# Create and label factor variables

Data_outcome$Endstatus<-factor(Data_outcome$Endstatus, levels = c(0,1), 
                            labels=c("completed/still in therapy", "Dropout"))

Data_outcome$Rules<-factor(Data_outcome$Rules, levels =c(0,1), 
                           labels = c("no problems", "specific rules"))

Data_outcome$Relapse<-factor(Data_outcome$Relapse, levels =c(0,1), 
                           labels = c("No", "Yes"))


# ------4) Combine dataframes---------

Data_file <- Data_file %>% dplyr::select(Crime:VP)
Data_interview <- Data_interview %>% dplyr::select(VP:Length)
Data_outcome <- Data_outcome %>% dplyr::select(VP:WAI)

Data_additional <- merge(Data_file, Data_interview, by.x = 'VP', by.y = 'VP')
Data_additional <- merge(Data_additional, Data_outcome, by.x = 'VP', by.y = 'VP')


rm(Data_file, Data_interview, Data_outcome)
