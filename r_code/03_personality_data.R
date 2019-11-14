########GET Sosci Survey Data



# Load helper functions
setwd("D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Masterarbeit\\Daten")
source('./r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'Hmisc', 'multcomp', 'effects', 'phia', 'emmeans', 'lme4',
          'sjPlot', 'lmerTest', 'stargazer', 'lemon', 'gridExtra', 'ggplot2', 'tidyr',
          'reshape2', 'corrplot', 'visreg', 'pwr', 'lattice', 'viridis', 'rcompanion',
          'apaTables', 'scales', 'foreign', 'psych', 'pastecs', 'ppcor', 'car', 'ez', 
          'nlme', 'QuantPsyc', 'stats', 'mediation', 'jtools')
getPacks(pkgs)
rm(pkgs)



# ----- 1) Read Soscisurvey file ----

Data_pers <- read.csv(file = "./data_gambling2019_2.csv", sep = ";", header=T)


# ----- 2) Remove Pilot Data and Dropouts-----------------------------
Data_pers <- Data_pers[-c(1, 6, 10), ]

# ----- 4) Select relevant variables ---------------------
Data_pers <- Data_pers %>% dplyr::select(RS01_01: RS01_84, 
                                         SD01:SD23_01, 
                                         SO01_01:SO16_11,
                                         BR11_01:BR11_55)


names(Data_pers)[names(Data_pers) == 'SD23_01'] <- 'VP'

# ----- 7) Compute RST scale scores -----------------------

# Select RST data
RST <- Data_pers %>% dplyr::select(VP, RS01_01:RS01_84)

# Drop control items
RST <- dplyr::select(RST, -RS01_63, -RS01_49, -RS01_59, -RS01_72)


# ----- Create aggregated scale values-------------------------
# RST SCALE VALUES
RST <- dplyr::mutate(RST, 
                     FFFS = (RS01_10 + RS01_24 + RS01_52 + RS01_60 + RS01_61 + RS01_64 + RS01_69 + RS01_77 + RS01_78 + RS01_81),
                     BIS = (RS01_01 + RS01_02 + RS01_07 + RS01_08 + RS01_11 + RS01_21 + RS01_23 + RS01_28 + 
                              RS01_37 + RS01_41 + RS01_42 + RS01_55 + RS01_56 + RS01_62 + RS01_65 + RS01_66 +
                              RS01_74 + RS01_75 + RS01_76 +  RS01_79 + RS01_80 + RS01_82 + RS01_83),
                     BAS_Rew_Int = (RS01_12 + RS01_15 + RS01_17 + RS01_18 + RS01_33 + RS01_40 + RS01_44),
                     BAS_Rew_Reac = (RS01_03 + RS01_04 + RS01_09 + RS01_19 + RS01_30 + RS01_31 + RS01_32 + RS01_38 + RS01_45 + RS01_47),
                     BAS_Goal_Drive = (RS01_05 + RS01_13 + RS01_25 + RS01_39 + RS01_54 + RS01_71 + RS01_84),
                     BAS_Impulsiv = (RS01_29 +  RS01_35 + RS01_36 + RS01_48 + RS01_53 + RS01_57 + RS01_68 + RS01_70))

# ----- Overall RST-SCORE
# RST SCORE
RST <- dplyr::mutate(RST, 
                     BAS_Score = (BAS_Rew_Int + BAS_Rew_Reac + BAS_Goal_Drive + BAS_Impulsiv))

RST_scales <- dplyr::select(RST, VP, FFFS:BAS_Score)


##---------------- BRIEF SCALE VALUES------------

BRIEF <- Data_pers %>% dplyr::select(VP, BR11_01:BR11_55)

BRIEF <- dplyr::mutate(BRIEF, 
                     Inhibit = (BR11_10 + BR11_11 + BR11_12 + BR11_13 + BR11_14 + BR11_15 + BR11_16 + BR11_17),
                     Shift = (BR11_33 + BR11_34 + BR11_35 + BR11_36 + BR11_37 + BR11_38 + BR11_39 + BR11_40),
                     Emotional_control = (BR11_01 + BR11_02 + BR11_03 + BR11_04 + BR11_05 + BR11_06),
                     Self_monitor = (BR11_28 + BR11_29 + BR11_30 + BR11_31 + BR11_32),
                     Working_memory = (BR11_48 + BR11_49 + BR11_50 + BR11_51 + BR11_52 + BR11_53 + BR11_54 + BR11_55),
                     Plan_organize = (BR11_18 +  BR11_19 + BR11_20 + BR11_21 + BR11_22 + BR11_23 + BR11_24 + BR11_25 + BR11_26 + BR11_27),
                     Task_completion = (BR11_41 + BR11_42 + BR11_43 + BR11_44 + BR11_45 + BR11_46 + BR11_47),
                     Validity = (BR11_07 + BR11_08 + BR11_09))

# ----- Overall BRIEF-SCORE
BRIEF <- dplyr::mutate(BRIEF, 
                     BRI = (Inhibit + Self_monitor), 
                     ERI= (Shift + Emotional_control), 
                     CRI= (Task_completion + Working_memory + Plan_organize), 
                     GEC= (Inhibit + Self_monitor + Shift + Emotional_control + Task_completion + Working_memory + Plan_organize))

BRIEF_scales <- dplyr::select(BRIEF, VP, Inhibit:GEC)


#-------SOGS Score-----------------------------

SOGS <-Data_pers %>% dplyr::select(VP, SO01_01:SO16_11)

#Nicht mitberechnet werden: Frage 1,2,3, 12, 16j, 16k
#generell: Antwort ja =1 
# 4: meistens/immer, 5: ja, weniger als Häfte/ja, meistens 6: ja in Vergangenheit/ja

SOGS <-Data_pers %>% dplyr::select(VP, SO04:SO11, SO13:SO16_09)

#ist NA wenn Frage nicht angezeigt wurde aufgrund vorheriger Antwort
SOGS$SO13[is.na(SOGS$SO13)] <- 2


SOGS <- mutate (SOGS, Score1 =ifelse(SO04>2, 1, 0))
SOGS <- mutate (SOGS, Score2 =ifelse(SO05>1, 1, 0))
SOGS <- mutate (SOGS, Score3 =ifelse(SO06>1, 1, 0))
SOGS <- mutate (SOGS, Score4 =ifelse(SO07==1, 1, 0))
SOGS <- mutate (SOGS, Score5 =ifelse(SO08==1, 1, 0))
SOGS <- mutate (SOGS, Score6 =ifelse(SO09==1, 1, 0))
SOGS <- mutate (SOGS, Score7 =ifelse(SO10==1, 1, 0))
SOGS <- mutate (SOGS, Score8 =ifelse(SO11==1, 1, 0))
SOGS <- mutate (SOGS, Score9 =ifelse(SO13==1, 1, 0))
SOGS <- mutate (SOGS, Score10 =ifelse(SO14==1, 1, 0))
SOGS <- mutate (SOGS, Score11 =ifelse(SO15==1, 1, 0))
SOGS <- mutate (SOGS, Score12 =ifelse(SO16_01==2, 1, 0))
SOGS <- mutate (SOGS, Score13 =ifelse(SO16_02==2, 1, 0))
SOGS <- mutate (SOGS, Score14 =ifelse(SO16_03==2, 1, 0))
SOGS <- mutate (SOGS, Score15 =ifelse(SO16_04==2, 1, 0))
SOGS <- mutate (SOGS, Score16 =ifelse(SO16_05==2, 1, 0))
SOGS <- mutate (SOGS, Score17 =ifelse(SO16_06==2, 1, 0))
SOGS <- mutate (SOGS, Score18 =ifelse(SO16_07==2, 1, 0))
SOGS <- mutate (SOGS, Score19 =ifelse(SO16_08==2, 1, 0))
SOGS <- mutate (SOGS, Score20 =ifelse(SO16_09==2, 1, 0))

SOGS <-mutate (SOGS, SOGS_Score=(Score1+Score2+Score3+Score4+Score5+Score6+Score7+Score8+Score9+
                                  Score10+Score11+Score12+Score13+Score14+Score15+Score16+Score17+
                                  Score18+Score19+Score20))

SOGS_score <-SOGS %>% dplyr::select(VP, SOGS_Score)

##-----------------Create Personality scale data set
Data_sosci <- merge (BRIEF_scales, RST_scales, by.x = 'VP', by.y = 'VP')

Data_sosci <- merge (Data_sosci, SOGS_score, by.x = 'VP', by.y = 'VP')



### Add scores to the rest of data

Data_pers2 <-Data_pers %>% dplyr::select(VP, SD01)
Data_pers_full <-merge(Data_pers2, Data_sosci, by.x = 'VP', by.y = 'VP')


names(Data_pers_full)[names(Data_pers_full) == 'SD01'] <- 'Gender'



#Add Age variable

Data_age <- dplyr::select(Data_pers, VP)


for (i in 1:nrow(Data_age)) {
  
  if (Data_age[i, 1]=='1560'){
    Data_age[i,2]<-40
    Data_age[i,3]<-2
    
  } else if  (Data_age[i, 1] == '1996' ) {
    Data_age[i,2] <- 43
    Data_age[i,3]<-3
    
  } else if  (Data_age[i, 1] == '1320' ) {
    Data_age[i,2] <- 29
    Data_age[i,3]<-1
    
  } else if  (Data_age[i, 1] == '1152' ) {
    Data_age[i,2] <- 25
    Data_age[i,3]<-1
    
  } else if  (Data_age[i, 1] == '1145' ) {
    Data_age[i,2] <- 40
    Data_age[i,3]<-2
    
  } else if  (Data_age[i, 1] == '1227' ) {
    Data_age[i,2] <- 50
    Data_age[i,3]<-3
    
  } else if  (Data_age[i, 1] == '1073' ) {
    Data_age[i,2] <- 46
    Data_age[i,3]<-3
   
  } else if  (Data_age[i, 1] == '1127' ) {
    Data_age[i,2] <- 37
    Data_age[i,3]<-2
    
  } else if  (Data_age[i, 1] == '1302' ) {
    Data_age[i,2] <- 35
    Data_age[i,3]<-2

  } else if  (Data_age[i, 1] == '1023' ) {
    Data_age[i,2] <- 42
    Data_age[i,3]<-3
    
  } else if  (Data_age[i, 1] == '1838' ) {
    Data_age[i,2] <- 36
    Data_age[i,3]<-2
    
  } else if  (Data_age[i, 1] == '1355' ) {
    Data_age[i,2] <- 48
    Data_age[i,3]<-3
    
  }
  
}

rm(i)
names(Data_age)[2:3] <- c('age', 'age_cat')

Data_age$age_cat<-factor(Data_age$age_cat, levels =c(1,2,3,4), labels =c('18-30','31-40', '41-50', '50+'))

Data_pers_full <- merge (Data_pers_full, Data_age)


#Missings
which(is.na(Data_pers_full))




#-----Internal consistency------

# Chronbach's Alpha (internal concistency) 
psych::alpha(RST[, grep(names(RST), pattern = '^RS')], 
             check.keys = TRUE)


# Chronbach's Alpha (internal concistency) for sub scales
# FFFS
psych::alpha(dplyr::select(RST, RS01_10, RS01_24, RS01_52, 
                           RS01_60, RS01_61, RS01_64, RS01_69,
                           RS01_77, RS01_78, RS01_81), 
             check.keys = TRUE)

# BIS
psych::alpha(dplyr::select(RST, RS01_01, RS01_02, RS01_07, 
                           RS01_08, RS01_11, RS01_21, RS01_23, 
                           RS01_28, RS01_37, RS01_41, RS01_42, 
                           RS01_55, RS01_56, RS01_62, RS01_65, 
                           RS01_66, RS01_74, RS01_75, RS01_76, 
                           RS01_79, RS01_80, RS01_82, RS01_83), 
             check.keys = TRUE)

# BAS Reward Interest
psych::alpha(dplyr::select(RST, RS01_12, RS01_15, RS01_17, 
                           RS01_18, RS01_33, RS01_40, RS01_44), 
             check.keys = TRUE)

# BAS Reward Reactivity
psych::alpha(dplyr::select(RST, RS01_03, RS01_04, RS01_09, RS01_19, 
                           RS01_30, RS01_32, RS01_32, RS01_38, 
                           RS01_45, RS01_47), 
             check.keys = TRUE)

# BAS Goal-Drive Persistance
psych::alpha(dplyr::select(RST, RS01_05, RS01_13, RS01_25, RS01_39, 
                           RS01_54, RS01_71, RS01_84), 
             check.keys = TRUE)

# BAS Impulsivity
psych::alpha(dplyr::select(RST, RS01_29, RS01_35, RS01_36, RS01_48, 
                           RS01_53, RS01_57, RS01_68, RS01_70), 
             check.keys=TRUE)

# BAS ALL
psych::alpha(dplyr::select(RST, RS01_12, RS01_15, RS01_17, 
                           RS01_18, RS01_33, RS01_40, RS01_44,
                           RS01_03, RS01_04, RS01_09, RS01_19, 
                           RS01_30, RS01_32, RS01_32, RS01_38, 
                           RS01_45, RS01_47,
                           RS01_05, RS01_13, RS01_25, RS01_39, 
                           RS01_54, RS01_71, RS01_84,
                           RS01_29, RS01_35, RS01_36, RS01_48, 
                           RS01_53, RS01_57, RS01_68, RS01_70), 
             check.keys=TRUE)


##BRIEF all
psych::alpha(BRIEF[,grep(names(BRIEF), pattern= '^BR')],
             check.keys = TRUE)


##Inhibit
psych::alpha(dplyr::select(BRIEF, BR11_10, BR11_11, BR11_12, BR11_13, BR11_14,
                             BR11_15, BR11_16, BR11_17),
             check.keys = TRUE)

##Shift
psych::alpha(dplyr::select(BRIEF, BR11_33, BR11_34, BR11_35, BR11_36, BR11_37, 
                             BR11_38, BR11_39, BR11_40), 
             check.keys = TRUE)

##Emotional_control
psych::alpha(dplyr::select(BRIEF, BR11_01, BR11_02, BR11_03, BR11_04, BR11_05, BR11_06), 
             check.keys = TRUE)

##Self_monitor
psych::alpha(dplyr::select(BRIEF, BR11_28, BR11_29, BR11_30, BR11_31, BR11_32), 
             check.keys = TRUE)

##Working_memory
psych::alpha(dplyr::select(BRIEF, BR11_48, BR11_49, BR11_50, BR11_51, BR11_52,  
                             BR11_53, BR11_54, BR11_55), 
             check.keys = TRUE)

##Plan_organize
psych::alpha(dplyr::select(BRIEF, BR11_18, BR11_19, BR11_20, BR11_21, BR11_22, 
                             BR11_23, BR11_24, BR11_25, BR11_26, BR11_27), 
             check.keys = TRUE)

##Task_completion
psych::alpha(dplyr::select(BRIEF, BR11_41, BR11_42, BR11_43, BR11_44,BR11_45,
                             BR11_46, BR11_47), 
             check.keys = TRUE)

##Validity
psych::alpha(dplyr::select(BRIEF, BR11_07,BR11_08, BR11_09), 
             check.keys = TRUE)

##BRI

psych::alpha(dplyr::select(BRIEF, BR11_10, BR11_11, BR11_12, BR11_13, BR11_14, 
                            BR11_15, BR11_16, BR11_17, BR11_28, BR11_29, BR11_30, 
                             BR11_31, BR11_32), check.keys = TRUE)
##ERI

psych::alpha(dplyr::select(BRIEF, BR11_33, BR11_34, BR11_35, BR11_36, BR11_37,  
                              BR11_38, BR11_39, BR11_40, BR11_01, BR11_02, BR11_03,
                             BR11_04, BR11_05, BR11_06), 
              check.keys = TRUE)
##CRI

psych::alpha(dplyr::select(BRIEF, BR11_41, BR11_42, BR11_43,BR11_44, BR11_45, 
                             BR11_46, BR11_47, BR11_48, BR11_49, BR11_50, BR11_51, BR11_52, 
                             BR11_53, BR11_54, BR11_55, BR11_18, BR11_19, BR11_20, BR11_21, BR11_22,
                             BR11_23, BR11_24, BR11_25, BR11_26, BR11_27), 
             check.keys = TRUE)
##GEC

psych:alpha(dplyr::select(BRIEF, BR11_10, BR11_11, BR11_12, BR11_13, BR11_14, 
                            BR11_15, BR11_16, BR11_17, BR11_28, BR11_29, BR11_30,
                            BR11_31, BR11_32, BR11_33, BR11_34, BR11_35, BR11_36, BR11_37, 
                            BR11_38, BR11_39, BR11_40, BR11_01, BR11_02, BR11_03, 
                            BR11_04, BR11_05, BR11_06, BR11_41, BR11_42, BR11_43, BR11_44, 
                            BR11_45, BR11_46, BR11_47, BR11_48, BR11_49, BR11_50, BR11_51,
                            BR11_52, BR11_53, BR11_54, BR11_55, BR11_18, BR11_19, BR11_20, 
                            BR11_21, BR11_22, BR11_23, BR11_24, BR11_25, BR11_26, BR11_27), 
            check.keys = TRUE)



