##### ##### ##### #####    Analysis script for descriptive analysis ##### ##### ##### #####
#                                  December 2019 
#                                     
# 
# Load helper functions
setwd("")
source('./MSc_Thesis_LindaTempel/r_functions/getPacks.R') # <- path to getPacks function
source('./MSc_Thesis_LindaTempel/r_functions/flattenCorrMatrix.R')

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'tidyr', 'reshape2','corrplot','Hmisc',
          'ggplot2', 'viridis','foreign', 'psych', 'RColorBrewer',
          'naniar', 'ggpubr', 'sjmisc')
getPacks(pkgs)
rm(pkgs)


#--------1) Calculate dependent variables-----------

Data_sum <- Data_card %>% 
  dplyr::group_by(VP, Block, Card, reward, Condition) %>% 
  dplyr::summarise(N=sum(!is.na(Card))) %>% 
  dplyr::filter(!Card==0)

#-- IGT-Score

#turning long into wide format
Data_score <- dcast(Data_sum, VP + Block + reward + Condition~ Card, value.var="N")
Data_score <-dplyr::select(Data_score, VP, Condition, reward, Block, A, B, C, D)

#Calculating Score for each Block
Data_score[is.na(Data_score)] <- 0
Data_score<- dplyr::mutate(Data_score,IGT_Score=((C+D)-(A+B)))

#-- Loss-Frequency-Score
Data_score<-dplyr::mutate(Data_score,Freq_Score=((B+D)-(A+C)))

#-- Mean of RT 

#remove outlier

RT <-Data_card%>% dplyr::select(VP, Block, Condition, reward, RT )

range(RT$RT)
boxplot(RT$RT)

#remove values above 10000 and below 100
RT <- RT %>% dplyr::filter(RT < 10000)
RT <- RT %>% dplyr::filter(RT > 100)

boxplot(RT$RT)

#remove individual outliers
Data_1<-dplyr::filter(RT, VP==1023)
outliers1 <- boxplot(Data_1$RT, plot=FALSE)$out
Data_1 <- Data_1[-which(Data_1$RT %in% outliers1),]
Data_2<-dplyr::filter(RT, VP==1073)
outliers2 <- boxplot(Data_2$RT, plot=FALSE)$out
Data_2 <- Data_2[-which(Data_2$RT %in% outliers2),]
Data_3<-dplyr::filter(RT, VP==1127)
outliers3 <- boxplot(Data_3$RT, plot=FALSE)$out
Data_3 <- Data_3[-which(Data_3$RT %in% outliers3),]
Data_4<-dplyr::filter(RT, VP==1145)
outliers4 <- boxplot(Data_4$RT, plot=FALSE)$out
Data_4 <- Data_4[-which(Data_4$RT %in% outliers4),]
Data_5<-dplyr::filter(RT, VP==1152)
outliers5 <- boxplot(Data_5$RT, plot=FALSE)$out
Data_5 <- Data_5[-which(Data_5$RT %in% outliers5),]
Data_6<-dplyr::filter(RT, VP==1164)
outliers6 <- boxplot(Data_6$RT, plot=FALSE)$out
Data_6 <- Data_6[-which(Data_6$RT %in% outliers6),]
Data_7<-dplyr::filter(RT, VP==1227)
outliers7 <- boxplot(Data_7$RT, plot=FALSE)$out
Data_7 <- Data_7[-which(Data_7$RT %in% outliers7),]
Data_8<-dplyr::filter(RT, VP==1302)
outliers8 <- boxplot(Data_8$RT, plot=FALSE)$out
Data_8 <- Data_8[-which(Data_8$RT %in% outliers8),]
Data_9<-dplyr::filter(RT, VP==1320)
outliers9 <- boxplot(Data_9$RT, plot=FALSE)$out
Data_9 <- Data_9[-which(Data_9$RT %in% outliers9),]
Data_10<-dplyr::filter(RT, VP==1355)
outliers10 <- boxplot(Data_10$RT, plot=FALSE)$out
Data_10 <- Data_10[-which(Data_10$RT %in% outliers10),]
Data_11<-dplyr::filter(RT, VP==1409)
outliers11 <- boxplot(Data_11$RT, plot=FALSE)$out
Data_11 <- Data_11[-which(Data_11$RT %in% outliers11),]
Data_12<-dplyr::filter(RT, VP==1531)
outliers12 <- boxplot(Data_12$RT, plot=FALSE)$out
Data_12 <- Data_12[-which(Data_12$RT %in% outliers12),]
Data_13<-dplyr::filter(RT, VP==1560)
outliers13 <- boxplot(Data_13$RT, plot=FALSE)$out
Data_13 <- Data_13[-which(Data_13$RT %in% outliers13),]
Data_14<-dplyr::filter(RT, VP==1600)
outliers14 <- boxplot(Data_14$RT, plot=FALSE)$out
Data_14 <- Data_14[-which(Data_14$RT %in% outliers14),]
Data_15<-dplyr::filter(RT, VP==1621)
outliers15 <- boxplot(Data_15$RT, plot=FALSE)$out
Data_15 <- Data_15[-which(Data_15$RT %in% outliers15),]
Data_16<-dplyr::filter(RT, VP==1838)
outliers16 <- boxplot(Data_16$RT, plot=FALSE)$out
Data_16 <- Data_16[-which(Data_16$RT %in% outliers16),]
Data_17<-dplyr::filter(RT, VP==1878)
outliers17 <- boxplot(Data_17$RT, plot=FALSE)$out
Data_17 <- Data_17[-which(Data_17$RT %in% outliers17),]
Data_18<-dplyr::filter(RT, VP==1881)
outliers18 <- boxplot(Data_18$RT, plot=FALSE)$out
Data_18 <- Data_18[-which(Data_18$RT %in% outliers18),]
Data_19<-dplyr::filter(RT, VP==1996)
outliers19 <- boxplot(Data_19$RT, plot=FALSE)$out
Data_19 <- Data_19[-which(Data_19$RT %in% outliers19),]
Data_20 <-dplyr::filter(RT, VP==1898)
outliers20 <- boxplot(Data_20$RT, plot=FALSE)$out
Data_20 <- Data_20[-which(Data_20$RT %in% outliers20),]
Data_21 <-dplyr::filter(RT, VP==1296)
outliers21 <- boxplot(Data_21$RT, plot=FALSE)$out
Data_21 <- Data_21[-which(Data_21$RT %in% outliers21),]
Data_22 <-dplyr::filter(RT, VP==1986)
outliers22 <- boxplot(Data_22$RT, plot=FALSE)$out
Data_22 <- Data_22[-which(Data_22$RT %in% outliers22),]

RT<-rbind(Data_1, Data_2, Data_3, Data_4, Data_5, Data_6, Data_7, Data_8, Data_9, 
          Data_10, Data_11, Data_12, Data_13, Data_14, Data_15, Data_16, Data_17, 
          Data_18, Data_19, Data_20, Data_21, Data_22)

rm(Data_1, Data_2, Data_3, Data_4, Data_5, Data_6, Data_7, Data_8, Data_9, 
   Data_10, Data_11, Data_12, Data_13, Data_14, Data_15, Data_16, Data_17, 
   Data_18, Data_19, Data_20, Data_21, Data_22, outliers1, outliers2, outliers3, outliers4, outliers5, 
   outliers6, outliers7, outliers8, outliers9, outliers10, outliers11, 
   outliers12, outliers13, outliers14, outliers15, outliers16, 
   outliers17, outliers18, outliers19, outliers20, outliers21, outliers22)

boxplot(RT$RT)
range(RT$RT)

#calculate means for condition/block
Data_RT <- RT %>% 
  dplyr::group_by(VP, Block, reward, Condition) %>% 
  dplyr::summarise(RT_mean=mean(RT))


#--------2) Merge all data into one dataframe---------

#make missing into NA
Data_additional <- 
  Data_additional %>% replace_with_na(replace = list(Rules_scale = 999))
Data_additional <- 
  Data_additional %>% replace_with_na(replace = list(Head = 999))


Data_all <- merge (Data_pers_full, Data_additional)
Data_behavioral <- merge(Data_score, Data_RT)
Data_all <- merge(Data_behavioral, Data_all)
rm(Data_behavioral) 


#make smaller data set: averaging the dependent variable across block/condition

Data_pred <- dplyr::select(Data_all, VP, IGT_Score, Freq_Score, RT_mean)
Data_pred <- Data_pred %>% 
  dplyr::group_by(VP) %>%
  dplyr::summarise(mean = mean(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   mean3 = mean(RT_mean))
names(Data_pred) <- c('VP', 'IGT_Score', 'Freq_Score','RT_mean')
Data_pred <-merge (Data_pred, Data_additional)
Data_pred2 <- dplyr::select(Data_pers_full, VP, age, Gender,
                            Inhibit: Task_completion, BRI:GEC, SOGS)
Data_pred <-merge(Data_pred, Data_pred2)
rm(Data_pred2)


#--------3) Demographics (mean, sd, frequency, plots)------------------------------

#--Age and Gender distribution

table(Data_pers_full$Gender)
table(Data_pers_full$age)

#--Education

table(Data_additional$Education)

table(Data_additional$Work)


#--Barplot main drug

Drug<-count(Data_additional, 'Drug1')

#Dummy group variable
Drug$row <- 1
Drug$row<-as.factor(Drug$row)

#Create a label variable
Drug$percent <- (Drug$freq/sum(Drug$freq)) * 100
Drug$percent <- round(Drug$percent, digits = 2)

Drug$Drug1 <- factor(Drug$Drug1, levels = c('Alcohol', 'Cannabinoids', 'Cocaine', 
                                            'Opioids', 'Other Stimulants')) 

#Graph
Drug <- ggplot(Drug, aes(x = row, y = freq, fill=Drug1)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = percent),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#440154FF", "#453781FF", "#32648EFF", 
                              "#29AF7FFF", "#74D055FF")) +
  xlab(NULL)+ ylab(NULL)+ 
  ggtitle("Principal diagnosis")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+ 
  theme(legend.text = element_blank())+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=10, face = "bold"))
  

#--Barplot all drugs 

Data_drug2 <- dplyr::select(Data_additional, VP, Drug1, Drug2, Drug3, Drug4, Drug5)

Data_drug2 <- melt(Data_drug2, id.vars = c("VP"), variable.name = "Drug", 
                   variable.factor=T)

Data_drug2 <- na.omit(Data_drug2)
Data_drug2 <-dplyr::select(Data_drug2, VP, value)
Drug2<- count(Data_drug2, 'value')

#Dummy group variable
Drug2$row <- 1
Drug2$row<-as.factor(Drug2$row)

#Create a label variable
Drug2$percent <- (Drug2$freq/sum(Drug2$freq)) * 100
Drug2$percent <- round(Drug2$percent, digits = 2)

#Graph
Drug2 <- ggplot(Drug2, aes(x = row, y = freq, fill = value)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = percent),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#440154FF", "#453781FF", "#32648EFF", "#238A8DFF",
                             "#29AF7FFF", "#74D055FF", "#FDE725FF"))+
  xlab(NULL)+ ylab(NULL)+ 
  ggtitle("All diagnoses")+
  theme_minimal()+
  theme(axis.text.x=element_blank()) + 
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=11, face="bold"))+
  labs(fill = "Type of substance")+
  theme(plot.title = element_text(size=10, face = "bold"))



plot <- ggarrange(Drug2, Drug, ncol=2, nrow=1, common.legend = T,  legend="right")

ggsave("plot.png", width = 16, height = 10, units = "cm")


table(Data_additional$Drug)

rm(Data_drug2, Drug, Drug2, plot)

#--Frequency of Nicotine Addiction

table(Data_additional$Nicotine_Add)


#--Current consumption

mean(Data_additional$Caffeine, na.rm = T)
sd(Data_additional$Caffeine, na.rm = T)
mean(Data_additional$Nicotine, na.rm = T)
sd(Data_additional$Nicotine, na.rm = T)

#--Comorbidities

table(Data_additional$Comorbid)

Data_comorb <- dplyr::select(Data_additional, VP, Comorbid1, Comorbid2)

Data_comorb <- melt(Data_comorb, id.vars = c("VP"), variable.name = "Comorb", 
                    variable.factor=T)

Data_comorb <- na.omit(Data_comorb)
Data_comorb <-dplyr::select(Data_comorb, VP, value)
Comorbid<- count(Data_comorb, 'value')

rm(Data_comorb, Comorbid)


#--Gambling


table(Data_sosci$SOGS)
table(Data_sosci$SOGS_Score)


#--Frequency of any crime

table(Data_additional$Crime)

#--Barplot Crime all

Data_crime <- dplyr::select(Data_additional, VP, TypeCrime1, TypeCrime2)

Data_crime <- melt(Data_crime, id.vars = c("VP"), variable.name = "Crime", 
                   variable.factor=T)

Data_crime <- na.omit(Data_crime)
Data_crime<- count(Data_crime, 'value')

#Dummy group variable
Data_crime$row <- 1
Data_crime$row<-as.factor(Data_crime$row)

#Create a label variable
Data_crime$percent <- (Data_crime$freq/sum(Data_crime$freq)) * 100
Data_crime$percent <- round(Data_crime$percent, digits = 2)

#Graph
ggplot(Data_crime, aes(x = row, y = freq, fill = value)) +
  geom_col(width = 0.3) +
  geom_text(aes(label = percent),
            position = position_stack(vjust = 0.5)) +
  scale_fill_viridis(option="viridis", discrete = T, name=("Crime"), 
                     labels= c("Assault", "Offense against narcotic law", "Other, non-violent offense", "Theft or burglary")) +
  xlab(NULL)+ ylab(NULL)+ 
  theme_minimal()+
  theme(axis.text.x=element_blank()) + 
  theme(legend.title = element_text(size=15, face = "bold"))+
  theme(legend.text = element_text(size=13))


ggsave("Crime.png", width = 13, height = 10, units = "cm")


rm(Data_crime)

#--Previous Treatment

table(Data_additional$Withdrawal)

mean(Data_additional$Treatment)
sd(Data_additional$Treatment)

table(Data_additional$Therapy)
table(Data_additional$Substitution)

#--Length of treatment

range(Data_additional$Length)
median(Data_additional$Length)

#--Medical

table(Data_additional$Med1)
table(Data_additional$Med2)

table(Data_additional$Head)

#--Experiment

table(Data_additional$Smokebreak)

table(Data_additional$Payment)

table(Data_behav$Condition)

#randomization check
sample(c(1,0), size=22, replace=T)
mean(sample(c(1,0), size=22, replace=T))

#--------4) Means and standard deviation for questionnaire data-----

##RST-PQ
mean (Data_sosci$FFFS)
sd (Data_sosci$FFFS)

mean(Data_sosci$BIS)
sd(Data_sosci$BIS)

mean(Data_sosci$BAS_Score)
sd(Data_sosci$BAS_Score)

mean(Data_sosci$BAS_Rew_Int)
sd(Data_sosci$BAS_Rew_Int)

mean(Data_sosci$BAS_Rew_Reac)
sd(Data_sosci$BAS_Rew_Reac)

mean(Data_sosci$BAS_Goal_Drive)
sd(Data_sosci$BAS_Goal_Drive)

mean(Data_sosci$BAS_Impulsiv)
sd(Data_sosci$BAS_Impulsiv)

##BRIEF 

mean(Data_sosci$Inhibit)
sd(Data_sosci$Inhibit)

mean(Data_sosci$Shift)
sd(Data_sosci$Shift)

mean(Data_sosci$Emotional_control)
sd(Data_sosci$Emotional_control)

mean(Data_sosci$Self_monitor)
sd(Data_sosci$Self_monitor)

mean(Data_sosci$Working_memory)
sd(Data_sosci$Working_memory)

mean(Data_sosci$Plan_organize)
sd(Data_sosci$Plan_organize)

mean(Data_sosci$Task_completion)
sd(Data_sosci$Task_completion)

mean(Data_sosci$Validity)
sd(Data_sosci$Validity)

mean(Data_sosci$BRI)
sd(Data_sosci$BRI)

mean(Data_sosci$ERI)
sd(Data_sosci$ERI)

mean(Data_sosci$CRI)
sd(Data_sosci$CRI)

mean(Data_sosci$GEC)
sd(Data_sosci$GEC)


#--------5) Reliability questionnaire scales-----------------------

# --> see: 03_personality-data

#--------6) Correlations among questionnaire scales--------------------------------

#centering 
Data_sosci <- within(Data_sosci, {
  BAS_Score_c <- BAS_Score - mean(BAS_Score, na.rm=T )
  BIS_c <- BIS - mean(BIS, na.rm=T)
  FFFS_c <- FFFS - mean(FFFS, na.rm=T)
  BAS_Rew_Int_c <- BAS_Rew_Int - mean(BAS_Rew_Int, na.rm=T)
  BAS_Rew_Reac_c <- BAS_Rew_Reac - mean(BAS_Rew_Reac, na.rm=T)
  BAS_Goal_Drive_c <- BAS_Goal_Drive - mean(BAS_Goal_Drive, na.rm=T)
  BAS_Impulsiv_c <- BAS_Impulsiv - mean(BAS_Impulsiv, na.rm=T)
})

Data_sosci <- within(Data_sosci, {
  Inhibit_c <- Inhibit - mean(Inhibit, na.rm=T )
  Shift_c <- Shift - mean(Shift, na.rm=T)
  Emotional_control_c <- Emotional_control - mean(Emotional_control, na.rm=T)
  Self_monitor_c <- Self_monitor - mean(Self_monitor, na.rm=T)
  Working_memory_c <- Working_memory - mean(Working_memory, na.rm=T)
  Plan_organize_c <- Plan_organize - mean(Plan_organize, na.rm=T)
  Task_completion_c <- Task_completion - mean(Task_completion, na.rm=T)
  BRI_c <- BRI - mean(BRI, na.rm=T)
  ERI_c <- ERI - mean(ERI, na.rm=T)
  CRI_c <- CRI - mean(CRI, na.rm=T)
  GEC_c <- GEC - mean(GEC, na.rm=T)
})


#for color scale
coul <- colorRampPalette(brewer.pal(11, "RdBu"))(22)

#RST

Data_corr_rst<- Data_sosci %>% dplyr::select (FFFS_c,BIS_c, BAS_Rew_Int_c, BAS_Rew_Reac_c,
                                              BAS_Goal_Drive_c, BAS_Impulsiv_c, 
                                               BAS_Score_c)
#Switching columns
Data_corr_rst <- Data_corr_rst[c("FFFS_c", "BIS_c", "BAS_Score_c", "BAS_Rew_Int_c", 
                                  "BAS_Goal_Drive_c", "BAS_Rew_Reac_c" , "BAS_Impulsiv_c")]


#Corrplot 
matrix<-rcorr(as.matrix(Data_corr_rst), type=c("pearson"))

#for p-values
matrix$r
matrix$P

flattenCorrMatrix(matrix$r, matrix$P)


#adjust p values for multiple comparisons
pAdj <- p.adjust(c(matrix$P), method = "fdr")

pAdj <-matrix(pAdj, 7,7)

#Rename variables
colnames(matrix$r) <- c("FFFS", "BIS", "BAS", "Reward Interest", 
                        "Goal Drive Persistence", "Reward Reactivity", "Impulsivity" )
rownames(matrix$r) <- c("FFFS", "BIS", "BAS", "Reward Interest", 
                        "Goal Drive Persistence", "Reward Reactivity", "Impulsivity")

#Plot: only significant correlation 
corrplot(matrix$r, method=c("square"), type="full", order="original", 
         p.mat = pAdj, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45, 
         col = rev(coul))

#Plot: all correlation
corrplot(matrix$r, method=c("number"), type="full", order="original", tl.col="black", 
         tl.srt=45, col = rev(coul))


#BRIEF

Data_corr_brief<- Data_sosci %>% dplyr::select (Inhibit_c, Shift_c, Emotional_control_c, Self_monitor_c, 
                                               Working_memory_c, Plan_organize_c, Task_completion_c, 
                                               BRI_c, ERI_c, CRI_c, GEC_c)

#Switching columns
Data_corr_brief <- Data_corr_brief[c("Inhibit_c", "Self_monitor_c", "BRI_c", "Shift_c", 
                                     "Emotional_control_c", "ERI_c", 
                                      "Working_memory_c", "Plan_organize_c", 
                                     "Task_completion_c", "CRI_c", "GEC_c")]


#Corrplot 
matrix<-rcorr(as.matrix(Data_corr_brief), type=c("pearson"))

#for p-values
matrix$r
matrix$P

flattenCorrMatrix(matrix$r, matrix$P)

#adjust p values for multiple comparisons
pAdj <- p.adjust(c(matrix$P), method = "fdr")

pAdj <-matrix(pAdj, 11,11)


#Rename variables
colnames(matrix$r) <- c("Inhibit", "Self-monitor","BRI", "Shift", "Emotional Control", "ERI", 
                        "Working Memory", "Plan/Organize", "Task Completion", "CRI", "GEC" )
rownames(matrix$r) <- c("Inhibit", "Self-monitor","BRI", "Shift", "Emotional Control","ERI", 
                        "Working Memory", "Plan/Organize", "Task Completion", "CRI", "GEC")

# Plot: only significant correlation 
corrplot(matrix$r, method=c("square"), type="full", order="original", 
         p.mat = pAdj, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45, 
         col = rev(coul))

#Plot: all correlation
corrplot(matrix$r, method=c("number"), type="full", order="original", tl.col="black", 
         tl.srt=45,  col = rev(coul))


rm(Data_corr_brief, Data_corr_rst, matrix, pAdj)

#--------7) Bar plots for dependent variables-----------------------------


mean(Data_all$IGT_Score)
sd(Data_all$IGT_Score)

mean(Data_all$Freq_Score)
sd(Data_all$Freq_Score)

mean(Data_all$RT_mean)
sd(Data_all$RT_mean)


##Frequency of cards 

Graph1 <- Data_sum %>% 
  dplyr::group_by(reward, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))

Graph1_1 <- Data_sum %>% 
  dplyr::group_by(Block, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))

#by condition
ggplot(Graph1, aes(x=reward, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Condition") +
  ylab("Chosen deck (mean)") +
  scale_fill_viridis(option="viridis", discrete = T, name=("Card")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=18))

#by block
ggplot(Graph1_1, aes(x=Block, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Chosen deck (mean)") +
  scale_fill_viridis(option="viridis", discrete = T, name=("Card")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=18))

# split by condition
Data_cond2<-dplyr::filter(Data_sum, Condition== '2_Belohnt')
Data_cond1<-dplyr::filter(Data_sum, Condition== '1_Belohnt')

Graph1_2 <- Data_cond1 %>% 
  dplyr::group_by(Block, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))

Graph1_3 <- Data_cond2 %>% 
  dplyr::group_by(Block, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))

#reward first group
ggplot(Graph1_2, aes(x=Block, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Chosen deck (mean)") +
  ggtitle("Group: reward during first block (N = 16)")+
  scale_fill_viridis(option="viridis", discrete = T, name=("Card")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=18))+ 
  theme(plot.title = element_text(size=18, face = "bold"))

#reward second group
ggplot(Graph1_3, aes(x=Block, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Chosen deck (mean)") +
  ggtitle("Group: reward during second block (N = 6)")+
  scale_fill_viridis(option="viridis", discrete = T, name=("Card")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(size=18, face = "bold"))


##IGT Score 

Graph2 <- Data_score %>% 
  dplyr::group_by(reward) %>% 
  dplyr::summarise(M=mean(IGT_Score), SD=sd(IGT_Score), SE=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))))

Graph2_1 <- Data_score %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(IGT_Score), SD=sd(IGT_Score), SE=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))))

#by condition
ggplot(Graph2, aes(x=reward, y=M, fill=reward)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Condition") +
  ylab("IGT-Score (mean)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))

#by block
ggplot(Graph2_1, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("IGT-Score (mean)") +
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))


# split by condition
Data_cond2<-dplyr::filter(Data_score, Condition== '2_Belohnt')
Data_cond1<-dplyr::filter(Data_score, Condition== '1_Belohnt')

Graph2_2 <- Data_cond1 %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(IGT_Score), SD=sd(IGT_Score), SE=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))))

Graph2_3 <- Data_cond2 %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(IGT_Score), SD=sd(IGT_Score), SE=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))))

#reward first group
ggplot(Graph2_2, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("IGT-Score (mean)") +
  ggtitle("Group: reward during first block (N = 16)")+
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) + 
  theme(plot.title = element_text(size=18, face = "bold"))

#reward second group
ggplot(Graph2_3, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("IGT-Score (mean)") +
  ggtitle("Group: reward during second block (N = 6)")+
  scale_fill_viridis(option="viridis", discrete = T) +
  scale_y_continuous(breaks=seq(-50,50,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) + 
  theme(plot.title = element_text(size=18, face = "bold"))

##Loss-Frequency Score

Graph3 <- Data_score %>% 
  dplyr::group_by(reward) %>% 
  dplyr::summarise(M=mean(Freq_Score), SD=sd(Freq_Score), SE=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))))

Graph3_1 <- Data_score %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(Freq_Score), SD=sd(Freq_Score), SE=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))))

#by condition
ggplot(Graph3, aes(x=reward, y=M, fill=reward)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Condition") +
  ylab("Loss- Frequency- Score (mean)") +
  scale_fill_viridis(option="viridis", discrete = T, begin = .3, end = .7) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))

#by block
ggplot(Graph3_1, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Loss- Frequency- Score (mean)") +
  scale_fill_viridis(option="viridis", discrete = T, begin = .3, end = .7) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))


# split by condition
Data_cond2<-dplyr::filter(Data_score, Condition== '2_Belohnt')
Data_cond1<-dplyr::filter(Data_score, Condition== '1_Belohnt')

Graph3_2 <- Data_cond1 %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(Freq_Score), SD=sd(Freq_Score), SE=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))))

Graph3_3 <- Data_cond2 %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(Freq_Score), SD=sd(Freq_Score), SE=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))))

#reward first group
ggplot(Graph3_2, aes(x=Block, y=M,  fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Freq-Score (mean)") +
  ggtitle("Group: reward during first block (N = 16)")+
  scale_fill_viridis(option="viridis", discrete = T,  begin = .3, end = .7) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))+ 
  theme(plot.title = element_text(size=18, face = "bold"))

#reward second group
ggplot(Graph3_3, aes(x=Block, y=M,  fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Freq-Score (mean)") +
  ggtitle("Group: reward during second block (N = 6)")+
  scale_fill_viridis(option="viridis", discrete = T,  begin = .3, end = .7) +
  scale_y_continuous(breaks=seq(-40,40,5)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18))+
  theme(plot.title = element_text(size=18, face = "bold"))


## RT_mean 

Graph4 <- RT %>% 
  dplyr::group_by(reward) %>% 
  dplyr::summarise(M=mean(RT), SD=sd(RT), SE=sd(RT)/sqrt(sum(!is.na(RT))))

Graph4_1 <- RT %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(RT), SD=sd(RT), SE=sd(RT)/sqrt(sum(!is.na(RT))))

#by condition
ggplot(Graph4, aes(x=reward, y=M, fill=reward)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Condition") +
  ylab("RT in ms (mean)") +
  scale_fill_viridis(option="inferno", discrete = T, begin = .4, end = .8) +
  scale_y_continuous(breaks=seq(0,800,100)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  coord_cartesian(ylim = c(300,650))

#by block
ggplot(Graph4_1, aes(x=Block, y=M, fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA, width=.5) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.05,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("RT in ms (mean)") +
  scale_fill_viridis(option="inferno", discrete = T, begin = .4, end = .8) +
  scale_y_continuous(breaks=seq(0,800,100)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  coord_cartesian(ylim = c(300,650))

# split by condition
Data_cond2<-dplyr::filter(RT, Condition== '2_Belohnt')
Data_cond1<-dplyr::filter(RT, Condition== '1_Belohnt')

Graph4_2 <- Data_cond1 %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(RT), SD=sd(RT), SE=sd(RT)/sqrt(sum(!is.na(RT))))

Graph4_3 <- Data_cond2 %>% 
  dplyr::group_by(Block) %>% 
  dplyr::summarise(M=mean(RT), SD=sd(RT), SE=sd(RT)/sqrt(sum(!is.na(RT))))

#reward first group
ggplot(Graph4_2, aes(x=Block, y=M,  fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("RT (mean)") +
  ggtitle("Group: reward during first block (N = 16)")+
  scale_fill_viridis(option="inferno", discrete = T, begin = .4, end = .8) +
  scale_y_continuous(breaks=seq(0,800,100)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(plot.title = element_text(size=18, face = "bold"))+
  coord_cartesian(ylim = c(300,700))

#reward second group
ggplot(Graph4_3, aes(x=Block, y=M,  fill=Block)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("RT (mean)") +
  ggtitle("Group: reward during second block (N = 6)")+
  scale_fill_viridis(option="inferno", discrete = T, begin = .4, end = .8) +
  scale_y_continuous(breaks=seq(0,800,100)) +
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(plot.title = element_text(size=18, face = "bold"))+
  coord_cartesian(ylim = c(300,700))

rm(Graph1, Graph1_1, Graph1_2, Graph1_3, 
   Graph2, Graph2_1, Graph2_2, Graph2_3,
   Graph3, Graph3_1, Graph3_2, Graph3_3, 
   Graph4, Graph4_1, Graph4_2, Graph4_3,
   Data_cond1, Data_cond2)


#--------8) Treatment outcomes------------------------

#relapse during treatment

table(Data_additional$Relapse)

#Endstatus

table(Data_additional$Endstatus)

#critical incidents

mean(Data_additional$Number_Incidents, na.rm=T)
sd(Data_additional$Number_Incidents, na.rm=T)
range(Data_additional$Number_Incidents)

mean(Data_additional$Serious_Incident_Therapy, na.rm=T)
sd(Data_additional$Serious_Incident_Therapy, na.rm=T)
range(Data_additional$Serious_Incident_Therapy)

mean(Data_additional$Serious_Incident_Behav, na.rm=T)
sd(Data_additional$Serious_Incident_Behav, na.rm=T)
range(Data_additional$Serious_Incident_Behav)

#rules/rules scale

mean(Data_additional$Rules_scale, na.rm=T)
sd(Data_additional$Rules_scale, na.rm=T)

table(Data_additional$Rules)

#WAI

Data_wai <- read.csv(file = "./WAI.csv", sep = ";", header=T)
Data_wai <- Data_wai[-c(1, 2, 3), ]

names(Data_wai) <- c('VP','t1', 't2', 'b3', 'g4', 'b5','g6', 'b7','g8', 'b9', 't10', 'g11', 't12', 'e', 'g','t', 'b', 's' )

Data_wai <- Data_wai %>% dplyr::select('VP','t1', 't2', 'b3', 'g4', 'b5','g6', 'b7','g8', 'b9', 't10', 'g11', 't12')

wai <- dplyr::mutate(Data_wai, 
                     Goal = (g4 + g6 + g8 + g11 ),
                     Task= (t1 + t2+ t10 + t12),
                     Bond = (b3 + b5 + b7 +b9),
                     WAI_sum = (Goal+ Task + Bond))
                     
mean(wai$WAI_sum)
sd(wai$WAI_sum, na.rm=T)

mean(wai$Goal)
sd(wai$Goal, na.rm=T)

mean(wai$Task)
sd(wai$Task, na.rm=T)

mean(wai$Bond)
sd(wai$Bond, na.rm=T)

#goal
psych::alpha(dplyr::select(Data_wai, g4,  g6 , g8 , g11), 
             check.keys = TRUE)
#task
psych::alpha(dplyr::select(Data_wai, t1 , t2, t10,  t12), 
             check.keys = TRUE)
#bond
psych::alpha(dplyr::select(Data_wai, b3 , b5 , b7 , b9), 
             check.keys = TRUE)

#all
psych::alpha(dplyr::select(Data_wai,g4 , g6 , g8 , g11, t1 , t2,  t10 , t12, 
                           b3 , b5 , b7 , b9),
             check.keys = TRUE)



rm(wai,Data_wai)

#--------9) Correlations between all variables--------------


#--Analysis Part 1

#centering
Data_all <- within(Data_all, {
  BAS_Score_c <- BAS_Score - mean(BAS_Score, na.rm=T )
  BIS_c <- BIS - mean(BIS, na.rm=T)
  FFFS_c <- FFFS - mean(FFFS, na.rm=T)
  BAS_Rew_Int_c <- BAS_Rew_Int - mean(BAS_Rew_Int, na.rm=T)
  BAS_Rew_Reac_c <- BAS_Rew_Reac - mean(BAS_Rew_Reac, na.rm=T)
  BAS_Goal_Drive_c <- BAS_Goal_Drive - mean(BAS_Goal_Drive, na.rm=T)
  BAS_Impulsiv_c <- BAS_Impulsiv - mean(BAS_Impulsiv, na.rm=T)
})

Data_all <- within(Data_all, {
  IGT_c <- IGT_Score - mean(IGT_Score, na.rm=T)
  Freq_c <- Freq_Score - mean(Freq_Score, na.rm =T)
  RT_c <- RT_mean -mean(RT_mean, na.rm=T)
})

Data_corr_all <-dplyr::select(Data_all, reward, Block, BIS_c, FFFS_c, BAS_Score_c, 
                              Drug, Crime, Length2, 
                              SOGS, Gender,age, Med1, Comorbid, 
                              Education, Work, Head, 
                              Smokebreak, Payment, 
                              IGT_c, Freq_c, RT_c)

#change factors to numeric
Data_corr_all$Crime <-as.character(Data_corr_all$Crime)
Data_corr_all$Crime <-plyr::revalue(Data_corr_all$Crime, c("Yes"="0", "No"="1"))
Data_corr_all$Crime <-as.numeric(Data_corr_all$Crime)
Data_corr_all$Comorbid <-as.character(Data_corr_all$Comorbid)
Data_corr_all$Comorbid <-plyr::revalue(Data_corr_all$Comorbid, c("Yes"="0", "No"="1"))
Data_corr_all$Comorbid <-as.numeric(Data_corr_all$Comorbid)
Data_corr_all$Head <-as.character(Data_corr_all$Head)
Data_corr_all$Head <-plyr::revalue(Data_corr_all$Head, c("Yes"="0", "No"="1"))
Data_corr_all$Head <-as.numeric(Data_corr_all$Head)
Data_corr_all$Med1 <-as.character(Data_corr_all$Med1)
Data_corr_all$Med1 <-plyr::revalue(Data_corr_all$Med1, c("Yes"="0", "No"="1"))
Data_corr_all$Med1 <-as.numeric(Data_corr_all$Med1)
Data_corr_all$Payment <-as.character(Data_corr_all$Payment)
Data_corr_all$Payment <-plyr::revalue(Data_corr_all$Payment, c("Yes"="0", "No"="1"))
Data_corr_all$Payment <-as.numeric(Data_corr_all$Payment)
Data_corr_all$Smokebreak <-as.character(Data_corr_all$Smokebreak)
Data_corr_all$Smokebreak <-plyr::revalue(Data_corr_all$Smokebreak, c("Yes"="0", "No"="1"))
Data_corr_all$Smokebreak <-as.numeric(Data_corr_all$Smokebreak)
Data_corr_all$Work <-as.character(Data_corr_all$Work)
Data_corr_all$Work <-plyr::revalue(Data_corr_all$Work, c("Yes"="0", "No"="1"))
Data_corr_all$Work <-as.numeric(Data_corr_all$Work)
Data_corr_all$Drug <-as.character(Data_corr_all$Drug)
Data_corr_all$Drug <-plyr::revalue(Data_corr_all$Drug, c("Alcohol/Cannabis"="0", "Opioids/Stimulants"="1"))
Data_corr_all$Drug <-as.numeric(Data_corr_all$Drug)
Data_corr_all$Length2 <-as.character(Data_corr_all$Length2)
Data_corr_all$Length2 <-plyr::revalue(Data_corr_all$Length2, c("less than 2 months"="0", "more than 2 months"="1"))
Data_corr_all$Length2 <-as.numeric(Data_corr_all$Length2)
Data_corr_all$reward <-as.character(Data_corr_all$reward)
Data_corr_all$reward <-plyr::revalue(Data_corr_all$reward, c("reward"="0", "no reward"="1"))
Data_corr_all$reward <-as.numeric(Data_corr_all$reward)
Data_corr_all$Block <-as.character(Data_corr_all$Block)
Data_corr_all$Block <-plyr::revalue(Data_corr_all$Block, c("1"="0", "2"="1"))
Data_corr_all$Block <-as.numeric(Data_corr_all$Block)
Data_corr_all$Gender <-as.character(Data_corr_all$Gender)
Data_corr_all$Gender <-plyr::revalue(Data_corr_all$Gender, c("Female"="0", "Male"="1"))
Data_corr_all$Gender <-as.numeric(Data_corr_all$Gender)
Data_corr_all$SOGS <-as.character(Data_corr_all$SOGS)
Data_corr_all$SOGS <-plyr::revalue(Data_corr_all$SOGS, c("no pathological gambling"="0", "pathological gambling"="1"))
Data_corr_all$SOGS <-as.numeric(Data_corr_all$SOGS)


Data_corr_all$age <-as.character(Data_corr_all$age)
Data_corr_all$age <-plyr::revalue(Data_corr_all$age, c("18-29"="0", "30-39"="1", "40-49"="2", "50+"="3"))
Data_corr_all$age <-as.numeric(Data_corr_all$age)
Data_corr_all$Education <-as.character(Data_corr_all$Education)
Data_corr_all$Education <-plyr::revalue(Data_corr_all$Education, c("None"="0", "Hauptschule"="1", "Realschule"="2", "(Fach)abitur"="3"))
Data_corr_all$Education <-as.numeric(Data_corr_all$Education)


#Corrplot 
Corr_all<-rcorr(as.matrix(Data_corr_all), type=c("pearson"))

#for p-values
Corr_all$r
Corr_all$P

flattenCorrMatrix(Corr_all$r, Corr_all$P)

#table
apa.cor.table(Data_corr_all, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Masterarbeit\\Daten\\Table2.doc", table.number = 1)

#Rename variables
colnames(Corr_all$r) <- c("Condition", "Block", "BIS", "FFFS", "BAS", 
                          "Type of Substance", "Criminal Offense", "Treatment Length", 
                          "SOGS", "Gender", "Age", "Medication", 
                          "Comorbidities", "Education", "Apprenticeship", 
                          "Head injuries", "Break", "Payment info", 
                          "IGT-Score", "Loss-Frequency-Score", "Reaction time")
rownames(Corr_all$r) <- c("Condition", "Block", "BIS", "FFFS", "BAS", 
                          "Type of Substance", "Criminal Offense", "Treatment Length", 
                          "SOGS", "Gender", "Age", "Medication", 
                          "Comorbidities", "Education", "Apprenticeship", 
                          "Head injuries", "Break", "Payment info", 
                          "IGT-Score", "Loss-Frequency-Score", "Reaction time")

#Plot: all correlation
corrplot(Corr_all$r, method=c("square"), type="full", order="hclust", hclust.method = "complete", 
         tl.col="black", tl.srt=45, col = rev(coul))

corrplot(Corr_all$r, method=c("number"), type="full", order="hclust", tl.col="black", 
         tl.srt=45, col = rev(coul))

#Plot: only significant correlation 
corrplot(Corr_all$r, method=c("square"), type="full", order="hclust",
         p.mat = Corr_all$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45, 
         col = rev(coul))


#---Analysis Part 2

#centering
Data_pred <- within(Data_pred, {
  Inhibit_c <- Inhibit - mean(Inhibit, na.rm=T )
  Shift_c <- Shift - mean(Shift, na.rm=T)
  Emotional_control_c <- Emotional_control - mean(Emotional_control, na.rm=T)
  Self_monitor_c <- Self_monitor - mean(Self_monitor, na.rm=T)
  Working_memory_c<- Working_memory - mean(Working_memory, na.rm=T)
  Plan_organize_c <- Plan_organize - mean(Plan_organize, na.rm=T)
  Task_completion_c <- Task_completion - mean(Task_completion, na.rm=T)
  BRI_c <- BRI - mean(BRI, na.rm=T)
  ERI_c <- ERI - mean(ERI, na.rm=T)
  CRI_c <- CRI - mean(CRI, na.rm=T)
  GEC_c <- GEC - mean(GEC, na.rm=T)
})

Data_pred <- within(Data_pred, {
  IGT_c <- IGT_Score - mean(IGT_Score, na.rm=T)
  WAI_c <- WAI -mean(WAI, na.rm =T)
  Number_Incidents_c <-Number_Incidents-mean(Number_Incidents, na.rm=T)
  Serious_Therapy_c <- Serious_Incident_Therapy-mean(Serious_Incident_Therapy, na.rm=T)
  Serious_Behav_c <- Serious_Incident_Behav - mean(Serious_Incident_Behav, na.rm=T)
  Rules_scale_c <- Rules_scale - mean(Rules_scale, na.rm=T)
})


Data_corr_all2 <-dplyr::select(Data_pred, IGT_c, 
                               BRI_c, ERI_c, CRI_c, GEC_c, WAI_c, Number_Incidents_c, 
                               Serious_Therapy_c,Serious_Behav_c,  Endstatus, 
                               Rules_scale_c, Rules, Relapse)
#change factors to numeric

Data_corr_all2$Endstatus <-as.character(Data_corr_all2$Endstatus)
Data_corr_all2$Endstatus <-plyr::revalue(Data_corr_all2$Endstatus, c("completed/still in therapy"="0", "Dropout"="1"))
Data_corr_all2$Endstatus <-as.numeric(Data_corr_all2$Endstatus)
Data_corr_all2$Rules <-as.character(Data_corr_all2$Rules)
Data_corr_all2$Rules <-plyr::revalue(Data_corr_all2$Rules, c("no problems"="0", "specific rules"="1"))
Data_corr_all2$Rules <-as.numeric(Data_corr_all2$Rules)
Data_corr_all2$Relapse <-as.character(Data_corr_all2$Relapse)
Data_corr_all2$Relapse<-plyr::revalue(Data_corr_all2$Relapse, c("No"="0", "Yes"="1"))
Data_corr_all2$Relapse <-as.numeric(Data_corr_all2$Relapse)

#Corrplot 
Corr_all2<-rcorr(as.matrix(Data_corr_all2), type=c("pearson"))

#for p-values
Corr_all2$r
Corr_all2$P

flattenCorrMatrix(Corr_all2$r, Corr_all2$P)

#table
apa.cor.table(Data_corr_all2, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Masterarbeit\\Daten\\Table3.doc", table.number = 1)

#Rename variables
colnames(Corr_all2$r) <- c("IGT-Score","BRI", "ERI", "CRI", "GEC", "WAI", 
                           "Number of incidents", "Severe incidents therapy","Severe incidents behavior",
                           "Completion of therapy","Rules Scale", "Difficult rules", "Relapse")

rownames(Corr_all2$r) <- c("IGT-Score", "BRI", "ERI", "CRI", "GEC", "WAI", 
                           "Number of incidents", "Severe incidents therapy","Severe incidents behavior",
                           "Completion of therapy","Rules Scale", "Difficult rules", "Relapse")


#Plot: all correlation
corrplot(Corr_all2$r, method=c("square"), type="full", order="hclust", tl.col="black", 
         tl.srt=45, col = rev(coul))

corrplot(Corr_all2$r, method=c("number"), type="full", order="hclust", tl.col="black", 
         tl.srt=45, col = rev(coul))

#Plot: only significant correlation 
corrplot(Corr_all2$r, method=c("square"), type="full", order="hclust", 
         p.mat = Corr_all2$P, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45, 
         col = rev(coul))



rm(Data_corr_all, Corr_all, Data_corr_all2, Corr_all2)


#--------10) Control variables------

Graph <- dplyr::select(Data_pred, VP,  IGT_Score, Freq_Score, RT_mean,
                       Gender, SOGS, age, Comorbid, Med1, Med2, Education, Work,
                       Head, Smokebreak, Payment)

#change education for this graph

for (i in 1:nrow(Graph)) {
  
  if (Graph[i, 11]=='None'){
    Graph[i,16]<-1
    
  } else if  (Graph[i, 11] == 'Hauptschule' ) {
    Graph[i,16] <- 1
    
  } else if  (Graph[i, 11] == 'Realschule' ) {
    Graph[i,16] <- 2
    
  } else if  (Graph[i, 11] == '(Fach)abitur' ) {
    Graph[i,16] <- 2
    
  }
  
}

rm(i)
names(Graph)[16] <- c('Education2')

Graph$Education2<-factor(Graph$Education2, levels =c(1,2), labels =c('Yes','No'))

# Calculating means, sd and se for all subgroups
Graph1 <- Graph %>% 
  dplyr::group_by(Gender) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
            se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
            sd=sd(IGT_Score), 
            mean2 = mean(Freq_Score), 
            se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
            sd2=sd(Freq_Score),
            mean3 = mean(RT_mean), 
            se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
            sd3=sd(RT_mean))


Graph1$Gender<-c("Yes", "No")

Graph2 <- Graph %>% 
  dplyr::group_by(SOGS) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph2$SOGS<-c("No", "Yes")

Graph3 <- Graph %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph4 <- Graph %>% 
  dplyr::group_by(Comorbid) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph5 <- Graph %>% 
  dplyr::group_by(Med1) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph6 <- Graph %>% 
  dplyr::group_by(Med2) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph6$Med2<-c("No medication", "Antipsychotics", "Antidepressants")

Graph7 <- Graph %>% 
  dplyr::group_by(Education) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))),
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph7$Education<-c("No education", "Nine years", "Ten years", "Twelve years")


Graph8 <- Graph %>% 
  dplyr::group_by(Work) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph9 <- Graph %>% 
  dplyr::filter(!VP==1320)
Graph9 <- Graph9 %>% 
  dplyr::group_by(Head) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph10 <- Graph %>% 
  dplyr::group_by(Smokebreak) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))


Graph11 <- Graph %>% 
  dplyr::group_by(Payment) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))

Graph12 <- Graph %>% 
  dplyr::group_by(Education2) %>% 
  dplyr::summarise(mean = mean(IGT_Score),
                   se=sd(IGT_Score)/sqrt(sum(!is.na(IGT_Score))), 
                   sd=sd(IGT_Score), 
                   mean2 = mean(Freq_Score), 
                   se2=sd(Freq_Score)/sqrt(sum(!is.na(Freq_Score))), 
                   sd2=sd(Freq_Score),
                   mean3 = mean(RT_mean), 
                   se3=sd(RT_mean)/sqrt(sum(!is.na(RT_mean))), 
                   sd3=sd(RT_mean))
#labeling
names(Graph1) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph2) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph3) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph4) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph5) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph6) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph7) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph8) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph9) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph10) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph11) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')
names(Graph12) <- c('Group', 'IGT_mean', 'IGT_se','IGT_sd','Freq_mean', 'Freq_se', 'Freq_sd', 'RT_mean', 'RT_se', 'RT_sd')


Graph <- rbind(Graph1, Graph2, Graph3, Graph4, Graph5, Graph6, Graph7,
               Graph8, Graph9, Graph10, Graph11, Graph12)

#add control variable as character
Graph$Variable <- c("Female", "Female", "SOGS", "SOGS", "Age", "Age", "Age", "Age",
                    "Comorbid", "Comorbid", "Med1","Med1", "Med", "Med", "Med", "Education", 
                    "Education", "Education", "Education", "Work", "Work", "Head", "Head", 
                    "Smokebreak", "Smokebreak", "Payment", "Payment", "Education2", "Education2")

rm(Graph1, Graph2, Graph3, Graph4, Graph5, Graph6, Graph7,Graph8, Graph9, Graph10, Graph11, Graph12)

#split in two graphs because legends are different
Graph_d <- Graph[-c(5:8, 13:19), ]

Graph_m <-Graph[-c(1:4, 9:12, 20:29), ]


#IGT Score

ggplot(data=Graph_d, aes(x=Variable, y=IGT_mean, color= Group)) +
  geom_point(aes(y=IGT_mean), size=6,  position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=IGT_mean-IGT_se, ymax=IGT_mean+IGT_se), 
                size=1.5, width=.3, position = position_dodge(0.25))+
  xlab("Control Variable") +
  ylab("IGT-Score (mean)")+ 
  scale_color_manual(values=c("#3CBB75FF","#440154FF" ))+
  theme_light()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=18))+
  scale_x_discrete(labels = c("Comorbidities?", "9 years school or less?", "Female?", "Head injuries?", 
                             "Medication?", "Payment?", "Break?", "SOGS >4? ", 
                             "Apprenticeship?"))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=15)) 


ggplot(data=Graph_m, aes(x=Variable, y=IGT_mean, color= Group)) +
  geom_point(aes(y=IGT_mean, shape =Group), size=6,  position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=IGT_mean-IGT_se, ymax=IGT_mean+IGT_se), 
                size=1.5, width=.3, position = position_dodge(0.25))+
  xlab("Control Variable") +
  ylab("IGT-Score (mean)")+ 
  scale_shape_manual(values=c(16, 16, 16, 16, 15, 15, 17, 17, 15,17,17))+
  scale_color_manual(values=c("#FDE725FF", "#440154FF",  "#2D718EFF", "#3CBB75FF",
                              "#2D718EFF", "#FDE725FF", "#2D718EFF", 
                              "#440154FF", "#440154FF",  "#3CBB75FF", "#FDE725FF" ))+ 
  theme_light()+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=15))+
  scale_x_discrete(labels = c("Age", "Education", "Type of Medication"))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=15)) 


#Loss-Frequency Score

ggplot(data=Graph_d, aes(x=Variable, y=Freq_mean, color= Group)) +
  geom_point(aes(y=Freq_mean), size=6,  position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=Freq_mean-Freq_se, ymax=Freq_mean+Freq_se), 
                size=1.5, width=.3, position = position_dodge(0.25))+
  xlab("Control Variable") +
  ylab("Loss- Frequency- Score (mean)")+ 
  scale_color_manual(values=c("#3CBB75FF","#440154FF" ))+
  theme_light()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=18))+
  scale_x_discrete(labels = c("Comorbidities?", "9 years school or less?", "Female?", "Head injuries?", 
                            "Medication?", "Payment?", "Break?", "SOGS >4? ", 
                              "Apprenticeship?"))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=15)) 


ggplot(data=Graph_m, aes(x=Variable, y=Freq_mean, color= Group)) +
  geom_point(aes(y=Freq_mean, shape=Group), size=6,  position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=Freq_mean-Freq_se, ymax=Freq_mean+Freq_se), 
                size=1.5, width=.3, position = position_dodge(0.25))+
  xlab("Control Variable") +
  ylab("Loss- Frequency- Score (mean)")+ 
  scale_shape_manual(values=c(16, 16, 16, 16, 15, 15, 17, 17, 15,17,17))+
  scale_color_manual(values=c("#FDE725FF", "#440154FF",  "#2D718EFF", "#3CBB75FF",
                              "#2D718EFF", "#FDE725FF", "#2D718EFF", 
                              "#440154FF", "#440154FF",  "#3CBB75FF", "#FDE725FF" ))+
  theme_light()+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=15))+
  scale_x_discrete(labels = c("Age", "Education", "Type of Medication"))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=15)) 


#RT

ggplot(data=Graph_d, aes(x=Variable, y=RT_mean, color= Group)) +
  geom_point(aes(y=RT_mean), size=6, position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=RT_mean-RT_se, ymax=RT_mean+RT_se), 
                size=1.5, width=.3, position = position_dodge(0.25))+
  xlab("Control Variable") +
  ylab("RT in ms (mean)")+ 
  scale_color_manual(values=c("#3CBB75FF","#440154FF" ))+
  theme_light()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=18))+
  scale_x_discrete(labels = c("Comorbidities?",  "9 years school or less?","Female?", "Head injuries?", 
                              "Medication?", "Payment?", "Break?", "SOGS >4? ", 
                              "Apprenticeship?"))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=15)) 


ggplot(data=Graph_m, aes(x=Variable, y=RT_mean, color= Group)) +
  geom_point(aes(y=RT_mean, shape = Group), size=6,  position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=RT_mean-RT_se, ymax=RT_mean+RT_se), 
                size=1.5, width=.3, position = position_dodge(0.25))+
  xlab("Control Variable") +
  ylab("RT in ms (mean)")+ 
  scale_shape_manual(values=c(16, 16, 16, 16, 15, 15, 17, 17, 15,17,17))+
  scale_color_manual(values=c("#FDE725FF", "#440154FF",  "#2D718EFF", "#3CBB75FF",
                              "#2D718EFF", "#FDE725FF", "#2D718EFF", 
                              "#440154FF", "#440154FF",  "#3CBB75FF", "#FDE725FF" ))+ 
  theme_light()+
  theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=15))+
  scale_x_discrete(labels = c("Age", "Education", "Type of Medication"))+
  theme(legend.title = element_text(size=19, face = "bold"))+
  theme(legend.text = element_text(size=15)) 




rm(Graph, Graph_d, Graph_m, p1, p2, p3)


#--------11) Table predictors-------------------

predictor <- dplyr::select(Data_sosci, VP, BAS_Score_c, BIS_c, FFFS_c)
Data_pred <-merge(Data_pred, predictor)
rm(predictor)

#split continous variables in above/below mean
Data_pred$BAS2 <-dicho(Data_pred$BAS_Score_c, dich.by = "mean")
Data_pred$BAS2<-factor(Data_pred$BAS2, levels=c(0,1), labels=c("below mean", "above mean"))

Data_pred$BIS2 <-dicho(Data_pred$BIS_c, dich.by = "mean")
Data_pred$BIS2<-factor(Data_pred$BIS2, levels=c(0,1), labels=c("below mean", "above mean"))

Data_pred$FFFS2 <-dicho(Data_pred$FFFS_c, dich.by = "mean")
Data_pred$FFFS2<-factor(Data_pred$FFFS2, levels=c(0,1), labels=c("below mean", "above mean"))

#calculate means and sd for all subgroups
by(Data_pred$IGT_Score, Data_pred$Drug, stat.desc)
by(Data_pred$IGT_Score, Data_pred$Crime, stat.desc)
by(Data_pred$IGT_Score, Data_pred$Length2, stat.desc)
by(Data_pred$IGT_Score, Data_pred$BAS2, stat.desc)
by(Data_pred$IGT_Score, Data_pred$BIS2, stat.desc)
by(Data_pred$IGT_Score, Data_pred$FFFS2, stat.desc)

by(Data_pred$Freq_Score, Data_pred$Drug, stat.desc)
by(Data_pred$Freq_Score, Data_pred$Crime, stat.desc)
by(Data_pred$Freq_Score, Data_pred$Length2, stat.desc)
by(Data_pred$Freq_Score, Data_pred$BAS2, stat.desc)
by(Data_pred$Freq_Score, Data_pred$BIS2, stat.desc)
by(Data_pred$Freq_Score, Data_pred$FFFS2, stat.desc)

by(Data_pred$RT_mean, Data_pred$Drug, stat.desc)
by(Data_pred$RT_mean, Data_pred$Crime, stat.desc)
by(Data_pred$RT_mean, Data_pred$Length2, stat.desc)
by(Data_pred$RT_mean, Data_pred$BAS2, stat.desc)
by(Data_pred$RT_mean, Data_pred$BIS2, stat.desc)
by(Data_pred$RT_mean, Data_pred$FFFS2, stat.desc)

