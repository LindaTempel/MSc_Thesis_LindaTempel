##### ##### ##### #####    Analysis script for inferential analysis ##### ##### ##### #####
#                                  January 2020 
#                                     
#   
# Load helper functions
setwd("")
source('./MSc_Thesis_LindaTempel/r_functions/getPacks.R') # <- path to getPacks function
source('./MSc_Thesis_LindaTempel/r_functions/flattenCorrMatrix.R')
   
# Load necessary packages
pkgs <- c('dplyr', 'plyr', 'Hmisc', 'emmeans', 'lme4','sjPlot', 
          'ggplot2', 'tidyr','reshape2', 'corrplot', 'visreg', 'viridis',
          'apaTables', 'pastecs', 'car','RColorBrewer')
getPacks(pkgs)
rm(pkgs)


#ANALYSIS PART 1: Prediction of performance-----------------------------------------
#--------1) Deck Choice-------------------------

Data_deck<- dplyr::select(Data_card, VP, Block, reward, Card, Trial)

#create data frame for deck choice: which deck was chosen in each trial
for (i in 1:nrow(Data_deck)) {
  
  if (Data_deck[i, 4]=='A'){
    Data_deck[i,6]<-1
    Data_deck[i,7]<-0
    Data_deck[i,8]<-0
    Data_deck[i,9]<-0
    
    
  } else if  (Data_deck[i, 4] == 'B' ) {
    Data_deck[i,6]<-0
    Data_deck[i,7]<-1
    Data_deck[i,8]<-0
    Data_deck[i,9]<-0
    
  } else if  (Data_deck[i, 4] == 'C' ) {
    Data_deck[i,6]<-0
    Data_deck[i,7]<-0
    Data_deck[i,8]<-1
    Data_deck[i,9]<-0
    
  } else if  (Data_deck[i, 4] == 'D' ) {
    Data_deck[i,6]<-0
    Data_deck[i,7]<-0
    Data_deck[i,8]<-0
    Data_deck[i,9]<-1
    
  }
}

rm(i)
names(Data_deck)[6:9] <- c('A_choice', 'B_choice', 'C_choice', 'D_choice')

Data_deck <- gather(Data_deck, deck, pick, A_choice:D_choice, factor_key=TRUE)

Data_deck <- dplyr::arrange(Data_deck, VP, Trial, deck)

#filter by reward to get smaller sample size
Data_deck1<-dplyr::filter(Data_deck, reward=="reward")
Data_deck2<-dplyr::filter(Data_deck, reward=="no reward")

# logistic regression models separated by condition
mod1<-glm(pick ~ deck  , family=binomial(link='logit'), 
           data=Data_deck1, 
           contrasts = list(deck ="contr.sum"))

mod2<-glm(pick ~ deck , family=binomial(link='logit'), 
         data=Data_deck2, 
         contrasts = list(deck ="contr.sum"))


#change factor levels, refit model then tab_model
Data_deck1$deck <- factor(Data_deck1$deck, levels = c('D_choice', 'A_choice', 'B_choice','C_choice'))  
Data_deck2$deck <- factor(Data_deck2$deck, levels = c('D_choice', 'A_choice', 'B_choice','C_choice'))

#get tables
tab_model(mod1, show.aic = T, show.r2 = T, 
          pred.labels = c("(Intercept)", "Card D", "Card A", "Card B"), 
          dv.labels='Deck Choice')
tab_model(mod2, show.aic = T, show.r2 = T, 
          pred.labels = c("(Intercept)", "Card D", "Card A", "Card B"),
          dv.labels='Deck Choice')

#Anova
car::Anova(mod1, type=3, test='F')
car::Anova(mod2, type=3, test='F')

#post-hoc tests
emmeans(mod1, pairwise~deck, type="response", 
        adjust="Bonferroni")
emmeans(mod2, pairwise~deck, type="response", 
        adjust="Bonferroni")

#change factor level, refit, then graphs
Data_deck1$deck <- factor(Data_deck1$deck, levels = c('A_choice', 'B_choice','C_choice','D_choice'))  
Data_deck2$deck <- factor(Data_deck2$deck, levels = c('A_choice', 'B_choice','C_choice','D_choice'))

#Graphs
p1 <- emmip(mod1, ~deck, CIs=T, type = "response", plotit = F)

p1$Group <-1
p1$Group<-as.factor(p1$Group)

plot1<-ggplot(data=p1, aes(x=xvar, y=yvar, group = Group, color=xvar))+
  geom_point(aes(y=yvar), size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL), size=1.5, width=.2)+
  geom_line(size=1.5)+
  xlab("Deck choice") +
  ylab("Predicted probability")+ 
  scale_y_continuous(breaks = seq(0,.35, by = .05))+
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=18))+
  theme(legend.position="none")+
  scale_color_manual(values=c("#440154FF", "#2D718EFF", "#3CBB75FF","#FDE725FF"))+
  scale_x_discrete(labels = c("Deck A", "Deck B", "Deck C", "Deck D"))+
  ggtitle("Deck selection in the rewarded condition")+
  theme(plot.title = element_text(size=18, face = "bold"))+
  coord_cartesian(ylim=c(.13,.35))


p2 <- emmip(mod2, ~ deck, CIs = T, type = "response", plotit=F)

p2$Group <-1
p2$Group<-as.factor(p2$Group)

plot2 <-ggplot(data=p2, aes(x=xvar, y=yvar, group = Group, color=xvar))+
  geom_point(aes(y=yvar), size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL), size=1.5, width=.2)+
  geom_line(size=1.5)+
  xlab("Deck choice") +
  ylab("Predicted probability")+ 
  scale_y_continuous(breaks = seq(0,.35, by = .05))+
  theme_bw()+
  theme(axis.title.x = element_text(size=18))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.text.y = element_text(size=15))+
  theme(axis.text.x= element_text(angle = 10, size=18))+
  theme(legend.position="none")+
  scale_color_manual(values=c("#440154FF", "#2D718EFF", "#3CBB75FF","#FDE725FF"))+
  scale_x_discrete(labels = c("Deck A", "Deck B", "Deck C", "Deck D"))+
  ggtitle("Deck selection in the non- rewarded condition")+
  theme(plot.title = element_text(size=18, face = "bold"))+
  coord_cartesian(ylim=c(.13,.35))

#combine both graphs into one
ggarrange(plot1, plot2, ncol=2, nrow=1, common.legend = F,  legend="none")


##Overfitted model with random intercept
mod3<-glmer(pick ~ 
           Block*deck+ reward*deck + (1|VP), family=binomial(link='logit'), data=Data_deck, 
         contrasts = list(reward="contr.sum", Block="contr.sum", deck ="contr.sum"))

#change factor levels, refit model then tab_model
Data_deck$deck <- factor(Data_deck$deck, levels = c('D_choice', 'A_choice', 'B_choice','C_choice'))  
Data_deck$reward <- factor(Data_deck$reward, levels = c('reward', 'no reward'))
Data_deck$Block <- factor(Data_deck$Block, levels = c('2', '1'))

#get table
tab_model(mod3, show.aic = T, show.r2 = T, 
                  pred.labels = c("(Intercept)", "Block 2", "Card D", "Card A", "Card B",
                                  "Reward", "Block2*Card D", "Block2*Card A", "Block2*Card B",
                                  "Reward*Card D", "Reward*Card A", "Reward*Card B"), 
                  dv.labels='Deck Choice')

#change factor level, refit, then graphs
Data_deck$deck <- factor(Data_deck$deck, levels = c('A_choice', 'B_choice','C_choice','D_choice'))  
Data_deck$Block <- factor(Data_deck$Block, levels = c('1', '2'))

#Anova and post-hoc tests
car::Anova(mod3, type=3)
emmeans(mod3, pairwise~deck|Block, type="response", 
        adjust="Bonferroni")
emmeans(mod3, pairwise~deck|reward, type="response", 
        adjust="Bonferroni")
emmeans(mod3, pairwise~deck, type="response", 
        adjust="Bonferroni")

#Graphs
emmip(mod3,reward ~ deck|Block,                    
      CIs=T,engine ="ggplot", type="response") + theme_bw()+
      theme(axis.text.x = element_text(size = 18)) + 
      theme(axis.title.x = element_text(size=20))+
      theme(axis.title.y = element_text(size=20))+
      theme(axis.text.y = element_text(size=18))+
      scale_color_manual(values = viridis(2, option = 'D', end=.6, direction = -1),
                     (name="Condition"), labels=c("reward", "no reward"))+
      theme(legend.text = element_text(size=15))+
      theme(legend.title = element_text(size = 17, face = "bold" ))


rm(Data_deck,Data_deck1, Data_deck2, mod1, mod2, mod3, p1, p2, plot1, plot2)

#--------2) IGT-Score---------------------

#add Education as factor with only two levels

for (i in 1:nrow(Data_all)) {
  
  if (Data_all[i, 52]=='None'){
    Data_all[i,83]<-1
    
  } else if  (Data_all[i, 52] == 'Hauptschule' ) {
    Data_all[i,83] <- 1
    
  } else if  (Data_all[i, 52] == 'Realschule' ) {
    Data_all[i,83] <- 2
    
  } else if  (Data_all[i, 52] == '(Fach)abitur' ) {
    Data_all[i,83] <- 2
    
  }
  
}

rm(i)
names(Data_all)[83] <- c('Education2')

Data_all$Education2<-factor(Data_all$Education2, levels =c(1,2), labels =c('9 years or less','more than 9 years'))


#----Model with random intercept, by Condition, BAS, BIS, FFFS

m1<-lmer(IGT_Score ~ 
           Block+
           reward+
           BAS_Score_c+ 
           BIS_c+ 
           FFFS_c + 
           Education2 + 
           SOGS+
           Med1+
           (1|VP), data=Data_all, REML = T, 
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum", 
                            Education2="contr.sum", 
                            SOGS="contr.sum", 
                            Med1="contr.sum"))

# #with interaction
# m1<-lmer(IGT_Score ~
#           Block+
#           reward+
#           BAS_Score_c *reward+
#           BIS_c * reward+
#           FFFS_c * reward +
#           Education2 +
#           SOGS+
#           Med1+(1|VP), data=Data_all, REML = T,
#           contrasts = list(reward="contr.sum",
#                           Block="contr.sum",
#                           Education2="contr.sum",
#                           SOGS="contr.sum",
#                           Med1="contr.sum"))

#Anova

car::Anova(m1, type=3, test='F')                                 
 
#change order of blocks for tablem, refit model
Data_all$Block <- factor(Data_all$Block, levels = c('2', '1')) 

#Get table

sjPlot::tab_model(m1, show.aic = T, show.std = T, show.r2 = T, 
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "BAS", "BIS", "FFFS", "School 9 years or less", 
                                  "No pathological gambling", "Taking medication"),
                  dv.labels='IGT-Score')

#if p only has two digits, add digits=3 for correct p values, same applies to other tab_models

#Residuals ok?

sjPlot::plot_model(m1, "diag") 
plot(Data_all$IGT_Score, resid(m1), xlab= "Observed values")
car::vif(m1)
  

#----Model with random intercept, by Condition, Drug, Crime and Treatment Length

# m1_1<-lmer(IGT_Score ~ 
#            reward +
#            Block+
#            Drug + 
#            Crime + 
#            Length2 +
#            Education2 + 
#            SOGS+
#            Med1+
#              (1|VP), 
#            data=Data_all, REML = T, 
#            contrasts = list(reward="contr.sum", 
#                             Block="contr.sum",
#                             Drug="contr.sum", 
#                             Crime="contr.sum", 
#                             Length2="contr.sum",
#                             Education2="contr.sum", 
#                             SOGS="contr.sum", 
#                             Med1="contr.sum"))


#with interaction
m1_1<-lmer(IGT_Score ~ 
             Block +
             reward+
             Drug *reward+ 
             Crime* reward + 
             Length2* reward+
             Education2 + 
             SOGS+
             Med1+(1|VP), 
           data=Data_all, REML = T, 
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum",
                            Drug="contr.sum", 
                            Crime="contr.sum", 
                            Length2="contr.sum",
                            Education2="contr.sum", 
                            SOGS="contr.sum", 
                            Med1="contr.sum"))
#Anova

car::Anova(m1_1, type=3, test= 'F')

#Get table

sjPlot::tab_model(m1_1, show.aic = T, show.std = T, show.r2 = T,digits=3,
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "Alcohol/Cannabis", "Committed crime",
                                  "Treatment < 2 months", "School 9 years or less",
                                  "No pathological gambling", "Taking medication",
                                  "Reward*Alcohol/Cannabis", "Reward*Committed crime",
                                  "Reward*Treatment < 2 months"),
                   dv.labels='IGT-Score')

#Post-hoc 

emmeans(m1_1, pairwise~Drug, 
        adjust='Bonferroni')

emmeans(m1_1, pairwise~reward,
        adjust='Bonferroni')

emmeans(m1_1, pairwise~reward|Crime,
        adjust='Bonferroni')

#Graphs

emmip(m1_1,reward ~ Crime,                    
      CIs=T,engine ="ggplot") + theme_bw() + 
  theme(axis.text.x = element_text(size = 18)) + 
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.y = element_text(size=18))+
  scale_color_manual(values = viridis(2, option = 'D', end=.7, direction = -1),
                     (name="Condition"), labels=c("reward", "no reward"))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size = 17, face = "bold" ))+
  xlab("Committed any crime") + ylab("IGT-Score")

visreg(m1_1, xvar='Drug', line=list(col='#440154FF'),  
       xlab=("Main Drug"), ylab=("IGT-Score"), band = F, gg=T)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))


visreg(m1_1, xvar='reward', line=list(col='#440154FF'), 
       xlab=("Condition"), ylab=("IGT-Score"), band =F, gg=T)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))

#Residuals ok?

sjPlot::plot_model(m1_1, "diag") 
plot(Data_all$IGT_Score, resid(m1_1), xlab= "Observed values")
car::vif(m1_1)

#--------3) Loss Frequency Score----------------------

#----Model with random intercept, by Condition, BAS, BIS, FFFS

m2<-lmer(Freq_Score ~ 
           Block+
           reward +
           BAS_Score_c + 
           BIS_c + 
           FFFS_c + 
           Head+
           Payment+
           SOGS +(1|VP), data=Data_all, REML = T,
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum", 
                            Head="contr.sum", 
                            Payment="contr.sum", 
                            SOGS="contr.sum"))

#with interaction
# m2<-lmer(Freq_Score ~
#            reward +
#            Block+
#            BAS_Score_c * reward +
#            BIS_c * reward+
#            FFFS_c * reward+
#            Head+
#            Payment+
#            SOGS+ (1|VP), data=Data_all, REML = T,
#          contrasts = list(reward="contr.sum",
#                           Block="contr.sum",
#                           Head="contr.sum",
#                           Payment="contr.sum",
#                           SOGS="contr.sum"))

#Anova

car::Anova(m2, type=3, test='F')  

#Get table

sjPlot::tab_model(m2, show.aic = T, show.std = T, show.r2 = T, 
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "BAS", "BIS", "FFFS", "Head injury", 
                                  "Payment known", "No pathological gambling"), 
                  dv.labels='Loss- Frequency- Score')

#Post-hoc 

emmeans(m2, pairwise~Head, 
        adjust='Bonferroni')

#Graphs

visreg(m2, xvar='Head', line=list(col='#39558CFF'), 
       xlab=("Headinjury"), ylab=("Loss- Frequency -Score"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))


#Residuals ok?

#remove case with missing value otherwise plot does not work
test <- Data_all %>% dplyr::filter(!VP==1320)

sjPlot::plot_model(m2, "diag") 
plot(test$Freq_Score, resid(m2), xlab= "Observed values")
car::vif(m2)


####Model with N = 22 - for appendix

m2<-lmer(Freq_Score ~ 
           Block+
           reward +
           BAS_Score_c + 
           BIS_c + 
           FFFS_c + 
           Payment+
           SOGS +(1|VP), data=Data_all, REML = T,
         contrasts = list(reward="contr.sum", 
                          Block="contr.sum", 
                          Payment="contr.sum", 
                          SOGS="contr.sum"))
#Anova

car::Anova(m2, type=3, test='F')  

#Get table

sjPlot::tab_model(m2, show.aic = T, show.std = T, show.r2 = T, 
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "BAS", "BIS", "FFFS", 
                                  "Payment known", "No pathological gambling"), 
                  dv.labels='Loss- Frequency- Score')


#----Model with random intercept, by Condition, Drug, Crime and Treatment Length

m2_1<-lmer(Freq_Score ~ 
             Block+
             reward +
             Drug + 
             Crime + 
             Length2 +
             Head+
             Payment+
             SOGS +
             (1|VP), 
           data=Data_all, REML = T, 
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum",
                            Drug="contr.sum", 
                            Crime="contr.sum",
                            Length2="contr.sum",
                            Head="contr.sum", 
                            Payment="contr.sum", 
                            SOGS="contr.sum"))

#with interaction
# m2_1<-lmer(Freq_Score ~
#                reward+
#                Block+
#                Drug * reward +
#                Crime * reward +
#                Length2 * reward +
#                Head+
#                Payment+
#                SOGS +
#                (1|VP),
#            data=Data_all, REML = T,
#            contrasts = list(reward="contr.sum",
#                                          Block="contr.sum",
#                                          Drug="contr.sum",
#                                          Crime="contr.sum",
#                                          Length2="contr.sum",
#                                          Head="contr.sum",
#                                          Payment="contr.sum",
#                                          SOGS="contr.sum"))

#Anova

car::Anova(m2_1, type=3, test='F')                                 

#Get table

sjPlot::tab_model(m2_1, show.aic = T, show.std = T, show.r2 = T,
                  pred.labels = c("(Intercept)","Block 2","Reward", 
                                  "Alcohol/Cannabis", "Committed crime",
                                  "Treatment < 2 months", "Headinjury", 
                                  "Payment known", "No pathological gambling"),  
                  dv.labels='Loss- Frequency -Score')

#Post-hoc 

emmeans(m2_1, pairwise~Crime,
        adjust='Bonferroni')

emmeans(m2_1, pairwise~Head,
        adjust='Bonferroni')

#Graphs

visreg(m2_1, xvar='Head', line=list(col='#39558CFF'),  
       xlab=("Headinjury"), ylab=("Loss- Frequency -Score"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))

visreg(m2_1, xvar='Crime', line=list(col='#39558CFF'),  
       xlab=("Committed any crime"), ylab=("Loss- Frequency -Score"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))


#Residuals ok?

sjPlot::plot_model(m2_1, "diag") 
plot(test$Freq_Score, resid(m2_1), xlab= "Observed values")
car::vif(m2_1)


#Model with N = 22 - for appendix

m2_1<-lmer(Freq_Score ~
             reward+
             Block+
             Drug+
             Crime+
             Length2 +
             Payment+
             SOGS +
             (1|VP),
           data=Data_all, REML = T, 
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum",
                            Drug="contr.sum", 
                            Crime="contr.sum",
                            Length2="contr.sum",
                            Payment="contr.sum", 
                            SOGS="contr.sum"))
#Anova

car::Anova(m2_1, type=3, test='F')  

#Get table

sjPlot::tab_model(m2_1, show.aic = T, show.std = T, show.r2 = T,
                  pred.labels = c("(Intercept)","Block 2","Reward", 
                                  "Alcohol/Cannabis", "Committed crime",
                                  "Treatment < 2 months", 
                                  "Payment known", "No pathological gambling"),  
                  dv.labels='Loss- Frequency -Score')


#--------4) Reaction time--------------------

#----Model with random intercept, by Condition, BAS, BIS, FFFS

#log transform RT to make assumptions fit better
Data_all$RT_log = log(Data_all$RT_mean)

m3<-lmer(RT_log ~              
           Block+
           reward+
           BAS_Score_c + 
           BIS_c + 
           FFFS_c + 
           Payment+
           Comorbid+(1|VP), data=Data_all, REML = T, 
         contrasts = list(reward="contr.sum", 
                          Block="contr.sum", 
                          Payment="contr.sum", 
                          Comorbid="contr.sum"))

#with interaction
# m3<-lmer(RT_log ~
#            Block+
#            reward+
#            BAS_Score_c * reward+
#            BIS_c * reward+
#            FFFS_c * reward+
#            Payment+
#            Comorbid+(1|VP)
#           ,data=Data_all, REML = T,
#          contrasts = list(reward="contr.sum",
#                           Block="contr.sum",
#                           Payment="contr.sum",
#                           Comorbid="contr.sum"))


#Anova 

car::Anova(m3, type=3, test='F')                                

#Get table

sjPlot::tab_model(m3, show.aic = T, show.std = T, show.r2 = T,
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "BAS", "BIS", "FFFS", "Headinjury", 
                                  "Payment known", "Comorbidities"),
                  dv.labels='Reaction time')

#Post-hoc

emmeans(m3, pairwise~Block, 
        adjust='Bonferroni')


#change factor order for graph, refit
Data_all$Block <- factor(Data_all$Block, levels = c('1', '2'))

#Graphs

visreg(m3, xvar='Block', line=list(col='#AB2F5EFF'), 
       xlab=("Block"), ylab=("Reaction time"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))


#Residuals ok?

sjPlot::plot_model(m3, "diag") 
plot(Data_all$RT_log, resid(m3), xlab= "Observed values")
car::vif(m3)



#model with N = 21 and with head injury- for appendix

m3<-lmer(RT_log ~              
           Block+
           reward+
           BAS_Score_c + 
           BIS_c + 
           FFFS_c + 
           Head+
           Payment+
           Comorbid+(1|VP), data=Data_all, REML = T, 
         contrasts = list(reward="contr.sum", 
                          Block="contr.sum", 
                          Head="contr.sum", 
                          Payment="contr.sum", 
                          Comorbid="contr.sum"))

#Anova

car::Anova(m3, type=3, test='F')

#table

sjPlot::tab_model(m3, show.aic = T, show.std = T, show.r2 = T,
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "BAS", "BIS", "FFFS", 
                                  "Payment known", "Comorbidities"),
                  dv.labels='Reaction time')

#----Model with random intercept, by Condition, Drug, Crime and Treatment Length

m3_1<-lmer(RT_log ~        
             Block +
             reward+
             Drug + 
             Crime + 
             Length2 + 
             Payment+
             Comorbid+(1|VP), 
           data=Data_all, REML = T, 
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum",
                            Drug="contr.sum", 
                            Crime="contr.sum",
                            Length2="contr.sum",
                            Payment="contr.sum", 
                            Comorbid="contr.sum"))


#with interaction
# m3_1<-lmer(RT_log ~
#              Block +
#              reward+
#              Drug * reward +
#              Crime * reward +
#              Length2 * reward +
#              Head+
#              Payment+
#              Comorbid+
#              (1|VP),
#            data=Data_all, REML = T,
#            contrasts = list(reward="contr.sum",
#                             Block="contr.sum",
#                             Drug="contr.sum",
#                             Crime="contr.sum",
#                             Length2="contr.sum",
#                             Head="contr.sum",
#                             Payment="contr.sum",
#                             Comorbid="contr.sum"))

#Anova

car::Anova(m3_1, type=3, test='F')

#change factor order for table, refit model
Data_all$Block <- factor(Data_all$Block, levels = c('2', '1'))      

#Get table

sjPlot::tab_model(m3_1, show.aic = T, show.std = T, show.r2 = T,
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "Alcohol/Cannabis", "Committed crime",
                                  "Treatment < 2 months", "Headinjury", 
                                  "Payment known", "Comorbidities"),
                  dv.labels='Reaction time')

#Post-hoc 

emmeans(m3_1, pairwise~Payment, 
        adjust='Bonferroni')

emmeans(m3_1, pairwise~Crime,
        adjust='Bonferroni')

emmeans(m3_1, pairwise~Block,
        adjust='Bonferroni')

#Graphs

visreg(m3_1, xvar='Payment', line=list(col='#AB2F5EFF'), 
       xlab=("Payment known in advance"), ylab=("Reaction time"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))

visreg(m3_1, xvar='Crime', line=list(col='#AB2F5EFF'), 
       xlab=("Committed any crime"), ylab=("Reaction time"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))

#change factor order for graph, refit model
Data_all$Block <- factor(Data_all$Block, levels = c('1', '2'))

visreg(m3_1, xvar='Block', line=list(col='#AB2F5EFF'), 
       xlab=("Block"), ylab=("Reaction time"), gg=T, band=F)+ theme_bw()+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.text.y = element_text(size = 18))


#Residuals ok?

sjPlot::plot_model(m3_1, "diag") 
plot(Data_all$RT_log, resid(m3_1), xlab= "Observed values")
car::vif(m3_1)

#model with N=21 and with headinjury - for appendix

m3_1<-lmer(RT_log ~        
             Block +
             reward+
             Drug + 
             Crime + 
             Length2 + 
             Head+
             Payment+
             Comorbid+(1|VP), 
           data=Data_all, REML = T, 
           contrasts = list(reward="contr.sum", 
                            Block="contr.sum",
                            Drug="contr.sum", 
                            Crime="contr.sum",
                            Length2="contr.sum",
                            Head="contr.sum", 
                            Payment="contr.sum", 
                            Comorbid="contr.sum"))
#Anova

car::Anova(m3_1, type=3, test='F')

#table 

sjPlot::tab_model(m3_1, show.aic = T, show.std = T, show.r2 = T, digits = 3,
                  pred.labels = c("(Intercept)","Block 2", "Reward",
                                  "Alcohol/Cannabis", "Committed crime",
                                  "Treatment < 2 months",
                                  "Payment known", "Comorbidities"),
                  dv.labels='Reaction time')


rm(m1, m1_1, m2, m2_1, m3, m3_1, test)


#change order of levels back to initial order
Data_all$Block <- factor(Data_all$Block, levels = c('1', '2'))  


#ANALYSIS PART 2: Prediction of treatment outcomes--------
#--------1) Completion of treatment-------

#Completion
mod<-glm(Endstatus ~ 
             BRI_c + IGT_c , family=binomial(link='logit'), data=Data_pred) 

#Anova
car::Anova(mod, type=3, test='F')

#get table

tab_model(mod, show.aic = T, show.std = T, show.r2 = T,
          pred.labels = c("(Intercept)", "BRI", "IGT-Score"), 
          dv.labels='Completion of therapy')


#Relapse

mod2<-glm(Relapse ~ 
          BRI_c + IGT_c , family=binomial(link='logit'), data=Data_pred) 
#Anova
car::Anova(mod2, type=3, test='F')

#get table

tab_model(mod2, show.aic = T, show.std = T, show.r2 = T,
          pred.labels = c("(Intercept)", "BRI", "IGT-Score"), 
          dv.labels='Relapse')

#--------2) Behavior during therapy---------

# Rules Scale

rules1<-lm(Rules_scale ~ IGT_c + BRI_c, data = Data_pred)

#Anova 
car::Anova(rules1, type=3, test='F')

##remove case with missing value otherwise plot does not work
test <- Data_pred %>% dplyr::filter(!VP==1320)

#Residuals okay?

sjPlot::plot_model(rules1, "diag")
plot(test$Rules_scale, resid(rules1), xlab= "Observed values")
car::vif(rules1)


# Rules self report

rules2<-glm(Rules ~ 
           BRI_c + IGT_c, family=binomial(link='logit'), data=Data_pred) 

#get table

tab_model(rules2, show.aic = T, show.std = T, show.r2 = T,
          pred.labels = c("(Intercept)", "BRI", "IGT-Score"), 
          dv.labels='Specific rules')



# Dienstbuch Number of Incidents

behav1 <-lm(Number_Incidents ~ IGT_c + BRI_c, data = Data_pred)

#Anova

car::Anova(behav1, type=3, test='F')

#Residuals okay?

sjPlot::plot_model(behav1, "diag")
plot(Data_pred$Number_Incidents, resid(behav1), xlab= "Observed values")
car::vif(behav1)


# Dienstbuch Seriousness of Incidents Therapy

behav2 <-lm(Serious_Incident_Therapy ~ IGT_c + BRI_c, data = Data_pred)

#Anova

car::Anova(behav2, type=3, test='F')

#Residuals okay?

sjPlot::plot_model(behav2, "diag")
plot(Data_pred$Serious_Incident_Therapy, resid(behav2), xlab= "Observed values")
car::vif(behav2)

# Dienstbuch Seriousness of Incidents Behavior

behav3 <-lm(Serious_Incident_Behav ~ IGT_c + BRI_c, data = Data_pred)

#Anova

car::Anova(behav3, type=3, test='F')

#Residuals okay?

sjPlot::plot_model(behav3, "diag")
plot(Data_pred$Serious_Incident_Behav, resid(behav3), xlab= "Observed values")
car::vif(behav3)


#--------3) WAI------------------
wai <-lm(WAI ~ IGT_c + BRI_c,  data = Data_pred)

#Anova

car::Anova(wai, type=3, test='F')

#Residuals okay?

sjPlot::plot_model(wai, 'diag')
plot(Data_pred$WAI, resid(wai), xlab= "Observed values")
car::vif(wai)


# blueprint for anova tables
apa.aov.table(wai, filename = "D:\\Users\\Linda Tempel\\Documents\\Psychologie\\Masterarbeit\\Daten\\Table11a.doc", table.number = 1)


rm(wai, mod, mod2, rules1, rules2, behav1, behav2, behav3, test)


#--------4) Correlations ---------------------

Corr_exe <-dplyr::select(Data_pred, IGT_c, BRI_c, WAI_c, Number_Incidents_c, 
                        Serious_Therapy_c,Serious_Behav_c, Rules_scale_c, Rules, 
                         Relapse, Endstatus)

#change factors into numeric
Corr_exe$Endstatus <-as.character(Corr_exe$Endstatus)
Corr_exe$Endstatus <-plyr::revalue(Corr_exe$Endstatus, c("completed/still in therapy"="0", "Dropout"="1"))
Corr_exe$Endstatus <-as.numeric(Corr_exe$Endstatus)
Corr_exe$Rules <-as.character(Corr_exe$Rules)
Corr_exe$Rules <-plyr::revalue(Corr_exe$Rules, c("no problems"="0", "specific rules"="1"))
Corr_exe$Rules <-as.numeric(Corr_exe$Rules)
Corr_exe$Relapse <-as.character(Corr_exe$Relapse)
Corr_exe$Relapse<-plyr::revalue(Corr_exe$Relapse, c("No"="0", "Yes"="1"))
Corr_exe$Relapse <-as.numeric(Corr_exe$Relapse)

#Corrplot 
Corr_exe<-rcorr(as.matrix(Corr_exe), type=c("pearson"))

#for p-values
Corr_exe$r
Corr_exe$P

flattenCorrMatrix(Corr_exe$r, Corr_exe$P)

#adjust p values for multiple comparisons
pAdj <- p.adjust(c(Corr_exe$P), method = "fdr")

pAdj <-matrix(pAdj, 10,10)

#Rename variables
colnames(Corr_exe$r) <- c("IGT-Score", "BRI",  "WAI", "Number of incidents", 
                          "Severe incidents therapy","Severe incidents behavior", 
                          "Rules Scale", "Difficult rules", "Relapse", "Completion of therapy")

rownames(Corr_exe$r) <- c("IGT-Score", "BRI",  "WAI", "Number of incidents", 
                          "Severe incidents therapy","Severe incidents behavior", 
                          "Rules Scale", "Difficult rules", "Relapse", "Completion of therapy")


#Plot: only significant correlation 
corrplot(Corr_exe$r, method=c("number"), type="full", order="original", 
         p.mat = pAdj, sig.level = 0.05, insig = "blank", tl.col="black", tl.srt=45, 
         col = rev(coul))

#Plot: all correlation
corrplot(Corr_exe$r, method=c("number"), type="full", order="original", tl.col="black", 
         tl.srt=45, col = rev(coul))


rm(Corr_exe, pAdj)



