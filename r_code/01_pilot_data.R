##### ##### #####     Analysis scripts for pilot data   ##### ##### #####
#                                 August 2019 
#                                     
# 
###########
# Load helper functions
setwd("")
source('./MSc_Thesis_LindaTempel/r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'Hmisc', 'ggplot2', 'tidyr', 'corrplot', 'viridis', 'rcompanion',
          'apaTables', 'scales', 'foreign', 'psych', 'pastecs', 'ppcor', 'QuantPsyc', 'stats')
getPacks(pkgs)
rm(pkgs)


#------ CREATE DATAFRAME FOR PILOT DATA--------------------------------------
#------ 1) Read in the test data --------------------

# # Set path before start
path <- c("") # <- location of files

paths <- dir(path = path, full.names = T, pattern = "\\.txt$")
names(paths) <- basename(paths)


#------ 2) Create data frame containing all files and observations------

# Read in files
Data_test <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Delete variables

Data_test<- Data_test %>% dplyr::select (.id, V2, V4, V6, V8, V10, V12, V14)

# Rename variables

names(Data_test) <- c('VP', 'Condition', 'Block', 'Trial', 'Deck', 'Value', 'RT', 'Payoff')

# Clean String Variables

Data_test$VP<- gsub(Data_test$VP, pattern = "rawdata.txt", replacement = "")

# Creata and label factor variables
Data_test$Condition<-factor(Data_test$Condition, levels = c(1,2), 
                               labels=c("1_Belohnt", "2_Belohnt"))

Data_test$VP<-factor(Data_test$VP, levels = c(301,302,303,304,305), 
                     labels=c("301", "302", "303", "304", "305"))

Data_test$Block<-factor(Data_test$Block, levels =c(1,2), labels =c(1,2))

# Delete trials that were misses (Deck other than 1,2,3 or 4)

Data_test <- Data_test[-c(581),]
Data_test <- Data_test[-c(901),] 
Data_test <- Data_test[-c(55),]


#------ 3) Create new variables-----------------------------------------

# Add variable REWARD

for (i in 1:nrow(Data_test)) {
  
  if (Data_test[i, 2]=='1_Belohnt'){
    
    if (Data_test[i,3]==1){
      Data_test[i,9]<-1
    } else if(Data_test[i,3]==2){
      Data_test[i,9]<-2
    }
    
  } else if  (Data_test[i, 2] == '2_Belohnt' ) {
    
    if (Data_test[i, 3] == 1) {
      Data_test[i, 9] <- 2
    } else if (Data_test[i, 3] == 2) {
      Data_test[i,9] <- 1
    }
  }
}


rm(i)
names(Data_test)[9] <- c('reward')

Data_test$reward<-factor(Data_test$reward, levels =c(1,2), labels =c('reward','no reward'))

# Add variable CARD

Data_B1<-dplyr::filter(Data_test, Block==1)
unique(Data_B1$Block)
Data_B2<-dplyr::filter(Data_test, Block==2)
unique(Data_B2$Block)

Data_B1$Deck <- factor(Data_B1$Deck)
Data_B1$Card <- plyr::revalue(Data_B1$Deck, c('1' = 'A', '2' = 'B', '3' = 'C', '4' = 'D'))

Data_B2$Deck <- factor(Data_B2$Deck)
Data_B2$Card <- plyr::revalue(Data_B2$Deck, c('1' = 'B', '2' = 'C', '3' = 'D', '4' = 'A'))


# ------ Add GAIN & LOSS columns for Block 1 and variable NET_PAYOFF

for (i in 1:nrow(Data_B1)) {  
  
  if (Data_B1[i, 10] == 'A' ) {
    
    if (Data_B1[i, 6] == 1) {
      Data_B1[i, 11] <- 100
      Data_B1[i, 12] <- 0
      Data_B1[i, 13] <- 100
    } else if (Data_B1[i, 6] == 2) {
      Data_B1[i,11] <- 100
      Data_B1[i,12] <- -150
      Data_B1[i,13]<- -50
    } else if (Data_B1[i, 6]  == 3) {
      Data_B1[i,11] <- 100
      Data_B1[i,12] <- -200 
      Data_B1[i,13] <- -100
    } else if (Data_B1[i, 6]  == 4) {
      Data_B1[i,11] <- 100
      Data_B1[i,12] <- -250
      Data_B1[i,13] <- -150
    } else if (Data_B1[i, 6]  == 5) {
      Data_B1[i,11] <- 100
      Data_B1[i,12] <- -300
      Data_B1[i,13] <- -200
    } else if (Data_B1[i, 6]  == 6) {
      Data_B1[i,11] <- 100
      Data_B1[i,12] <- -350
      Data_B1[i,13] <- -250
    }
    
  } else if  (Data_B1[i, 10] == 'B' ) {
    
    if (Data_B1[i, 6] == 1) {
      Data_B1[i, 11] <- 100
      Data_B1[i, 12] <- 0
      Data_B1[i, 13] <- 100
    } else if (Data_B1[i, 6] == 2) {
      Data_B1[i,11] <- 100
      Data_B1[i,12] <- -1250
      Data_B1[i,13] <- -1150
    }
    
  } else if (Data_B1[i, 10] == 'C' ) {
    
    if (Data_B1[i, 6] == 1) {
      Data_B1[i, 11] <- 50
      Data_B1[i, 12] <- 0
      Data_B1[i,13] <- 50
    } else if (Data_B1[i, 6] == 2) {
      Data_B1[i,11] <- 50
      Data_B1[i,12] <- -25
      Data_B1[i,13] <- 25
    } else if (Data_B1[i, 6] == 3) {
      Data_B1[i,11] <- 50
      Data_B1[i,12] <- -50
      Data_B1[i,13] <- 0
    } else if (Data_B1[i, 6] == 4) {
      Data_B1[i,11] <- 50
      Data_B1[i,12] <- -75
      Data_B1[i,13] <- -25
    }
    
  } else if (Data_B1[i, 10] == 'D' ) {
    
    if (Data_B1[i, 6] == 1) {
      Data_B1[i, 11] <- 50
      Data_B1[i, 12] <- 0
      Data_B1[i, 13] <- 50
    } else if (Data_B1[i, 6] == 2) {
      Data_B1[i,11] <- 50
      Data_B1[i,12] <- -250
      Data_B1[i,13] <- -200
    }
    
  }
  
}

rm(i)
names(Data_B1)[11:13] <- c('gain','loss', 'net_payoff')

# ------ Add GAIN & LOSS columns for Block 2 and variable NET_PAYOFF

for (i in 1:nrow(Data_B2)) {  
  
  if (Data_B2[i, 10] == 'A' ) {
    
    if (Data_B2[i, 6] == 1) {
      Data_B2[i, 11] <- 200
      Data_B2[i, 12] <- -100
      Data_B2[i, 13] <- 100
    } else if (Data_B2[i, 6] == 2) {
      Data_B2[i,11] <- 50
      Data_B2[i,12] <- -100
      Data_B2[i,13]<- -50
    } else if (Data_B2[i, 6]  == 3) {
      Data_B2[i,11] <- 150
      Data_B2[i,12] <- -400
      Data_B2[i,13] <- -150
    } else if (Data_B2[i, 6]  == 4) {
      Data_B2[i,11] <- 150
      Data_B2[i,12] <- -350
      Data_B2[i,13] <- -200
    } else if (Data_B2[i, 6]  == 5) {
      Data_B2[i,11] <- 50
      Data_B2[i,12] <- -200
      Data_B2[i,13] <- -150
    } else if (Data_B2[i, 6]  == 6) {
      Data_B2[i,11] <- 50
      Data_B2[i,12] <- -150
      Data_B2[i,13] <- -100
    }
    
  } else if  (Data_B2[i, 10] == 'B' ) {
    
    if (Data_B2[i, 6] == 1) {
      Data_B2[i, 11] <- 200
      Data_B2[i, 12] <- -100
      Data_B2[i, 13] <- 100
    } else if (Data_B2[i, 6] == 2) {
      Data_B2[i,11] <- 200
      Data_B2[i,12] <- -1350
      Data_B2[i,13] <- -1150
    }
    
  } else if (Data_B2[i, 10] == 'C' ) {
    
    if (Data_B2[i, 6] == 1) {
      Data_B2[i,11] <- 100
      Data_B2[i,12] <- -50
      Data_B2[i,13] <- 50
    } else if (Data_B2[i, 6] == 2) {
      Data_B2[i,11] <- 100
      Data_B2[i,12] <- -75
      Data_B2[i,13] <- 25
    } else if (Data_B2[i, 6] == 3) {
      Data_B2[i,11] <- 100
      Data_B2[i,12] <- -100
      Data_B2[i,13] <- 0
    } else if (Data_B2[i, 6] == 4) {
      Data_B2[i,11] <- 100
      Data_B2[i,12] <- -125
      Data_B2[i,13] <- -25
    }
    
  } else if (Data_B2[i, 10] == 'D' ) {
    
    if (Data_B2[i, 6] == 1) {
      Data_B2[i, 11] <- 100
      Data_B2[i, 12] <- -50
      Data_B2[i, 13] <- 50
    } else if (Data_B2[i, 6] == 2) {
      Data_B2[i,11] <- 100
      Data_B2[i,12] <- -300
      Data_B2[i,13] <- -200
    }
    
  }
  
}

rm(i)
names(Data_B2)[11:13] <- c('gain','loss', 'net_payoff')


#------ 4) Create new data frame containing all blocks-------------------

Data_card<-rbind(Data_B1, Data_B2)
rm(Data_B1, Data_B2)

# remove missings
print(levels(Data_card$Card)) 
Data_card$Card <-droplevels.factor(Data_card$Card, exclude= c(7))
print(levels(Data_card$Card))


#------ 5) Add additional variables (sex and age)-------------------------

for (i in 1:nrow(Data_card)) {
  
  if (Data_card[i, 1]=='301'){
      Data_card[i,14]<-27
      Data_card[i,15]<-1
    
  } else if  (Data_card[i, 1] == '302' ) {
      Data_card[i,14] <- 27
      Data_card[i,15]<-1
    
  } else if  (Data_card[i, 1] == '303' ) {
    Data_card[i,14] <- 25
    Data_card[i,15] <-2
    
  } else if  (Data_card[i, 1] == '304' ) {
   Data_card[i,14] <- 25
   Data_card[i,15] <-2
  
  } else if  (Data_card[i, 1] == '305' ) {
   Data_card[i,14] <- 30
   Data_card[i,15] <-1
  }

}

rm(i)
names(Data_card)[14:15] <- c('age', 'sex')

Data_card$sex<-factor(Data_card$sex, levels = c(1,2), 
                            labels=c("male", "female"))



#------ ANALYSIS OF PILOT DATA-----------

#------ Calculate dependent variables-----

#----IGT-Score

#Summarize data frame 
Data_sum <- Data_card %>% 
  dplyr::group_by(VP, Block, Card, reward, age, sex) %>% 
  dplyr::summarise(N=sum(!is.na(Card)))

Data_sum$Card <- droplevels.factor(Data_sum$Card, exclude= c(0))
Data_sum <- Data_sum[-c(41),]

#Turning long into wide format
Data_score <- dcast(Data_sum, VP + Block +reward ~ Card, value.var="N")

#Calculating Score for each Block
Data_score[is.na(Data_score)] <- 0
Data_score<- dplyr::mutate(Data_score,IGT_Score=((C+D)-(A+B)))

Data_score<- Data_score %>% dplyr::select (VP, Block, A, B, C, D, IGT_Score, reward)

#----Payoff

#Select payoff at end of Block only
Data_payoff<-Data_test %>% dplyr::select(VP, Block, Trial, Payoff, reward)
Data_payoff<-dplyr::filter(Data_payoff, Trial==100)

Data_payoff<-Data_payoff %>% dplyr::select(VP, Block,Payoff, reward)

#----Reaction time

#Calculating mean of RT for each Block
Data_RT <- Data_card %>% 
  dplyr::group_by(VP, Block, reward) %>% 
  dplyr::summarise(RT_mean=mean(RT))

#----Combine data frames

Data_pilot <- merge (Data_RT, Data_score)
Data_pilot <- merge (Data_pilot, Data_payoff)


#------ Descriptive Statistics -----------

mean(Data_sum$age)
sd(Data_sum$age)

#RT
mean(Data_pilot$RT)
sd(Data_pilot$RT)
by (Data_pilot$RT, Data_pilot$Block, stat.desc)
by (Data_pilot$RT, Data_pilot$reward, stat.desc)

#Payoff
mean(Data_pilot$Payoff)
sd(Data_pilot$Payoff)
by (Data_pilot$Payoff, Data_pilot$Block, stat.desc)
by (Data_pilot$Payoff, Data_pilot$reward, stat.desc)

#IGT-Score
mean(Data_pilot$IGT_Score)
sd(Data_pilot$IGT_Score)
by (Data_pilot$IGT_Score, Data_pilot$Block, stat.desc)
by (Data_pilot$IGT_Score, Data_pilot$reward, stat.desc)



#------ Graphs------------------

# By Condition
Graph1 <- Data_sum %>% 
  dplyr::group_by(reward, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))

ggplot(Graph1, aes(x=reward, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Condition") +
  ylab("Chosen Decks(mean)") +
  scale_fill_viridis(option="viridis", discrete = T, name=("Card")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=15, face = "bold"))+
  theme(legend.text = element_text(size=12))


# By Block
Graph2 <- Data_sum %>% 
  dplyr::group_by(Block, Card) %>% 
  dplyr::summarise(M=mean(N), SD=sd(N), SE=sd(N)/sqrt(sum(!is.na(N))))

ggplot(Graph2, aes(x=Block, y=M,  fill=Card)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour=NA) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE),
                width=.2,
                size=.8,
                position=position_dodge(.9),
                colour="gray20")+
  xlab("Block") +
  ylab("Chosen Decks(mean)") +
  scale_fill_viridis(option="viridis", discrete = T, name=("Card")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(legend.title = element_text(size=15, face = "bold"))+
  theme(legend.text = element_text(size=12))



#clean environment for main data
rm(list = ls())
