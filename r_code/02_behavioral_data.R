##### ##### ##### #####    Get behavioral data   ##### ##### ##### #####
#                              September 2019 
#                                     
# 
# Load helper functions
setwd("")
source('./MSc_Thesis_LindaTempel/r_functions/getPacks.R') # <- path to getPacks function

# Load necessary packages
pkgs <- c('dplyr', 'plyr')
getPacks(pkgs)
rm(pkgs)



# ----- 1) Set path -------------------------------------------

path <- c("") # <- location of files

paths <- dir(path = path, full.names = T, pattern = "\\.txt$")
names(paths) <- basename(paths)



# ----- 2) Create data frame containing all files and observations------

# Read in files
Data_behav <- plyr::ldply(paths, read.table, sep =",", dec = ".", header=F)
rm(paths)

# Delete variables

Data_behav<- Data_behav %>% dplyr::select (.id, V2, V4, V6, V8, V10, V12, V14)

# Rename variables

names(Data_behav) <- c('VP', 'Condition', 'Block', 'Trial', 'Deck', 'Value', 'RT', 'Payoff')

# Clean String Variables

Data_behav$VP<- gsub(Data_behav$VP, pattern = "rawdata.txt", replacement = "")

# Create and label factor variables

Data_behav$Condition<-factor(Data_behav$Condition, levels = c(1,2), 
                            labels=c("1_Belohnt", "2_Belohnt"))


Data_behav$Block<-factor(Data_behav$Block, levels =c(1,2), labels =c(1,2))

# ----- 3) Create new variables-----------------------------------------

#Add variable REWARD

for (i in 1:nrow(Data_behav)) {
  
  if (Data_behav[i, 2]=='1_Belohnt'){
    
    if (Data_behav[i,3]==1){
      Data_behav[i,9]<-1
    } else if(Data_behav[i,3]==2){
      Data_behav[i,9]<-2
    }
    
  } else if  (Data_behav[i, 2] == '2_Belohnt' ) {
    
    if (Data_behav[i, 3] == 1) {
      Data_behav[i, 9] <- 2
    } else if (Data_behav[i, 3] == 2) {
      Data_behav[i,9] <- 1
    }
  }
}

rm(i)
names(Data_behav)[9] <- c('reward')

Data_behav$reward<-factor(Data_behav$reward, levels =c(1,2), labels =c('reward','no reward'))


# Add variable CARD

Data_B1<-dplyr::filter(Data_behav, Block==1)
unique(Data_B1$Block)

Data_B2<-dplyr::filter(Data_behav, Block==2)
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
      Data_B2[i,13] <- -250
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

## Remove Missings/drop incorrect factor levels

which(is.na(Data_card))
Data_card <- na.omit(Data_card)
which(is.na(Data_card))

print(levels(Data_card$Card))
Data_card$Card <-droplevels.factor(Data_card$Card, exclude= c(0))
print(levels(Data_card$Card))




