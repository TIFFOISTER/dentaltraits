setwd('~/hypsodonty') 
library(tidyverse)  
library(dplyr)
library(ggmap)
library(raster)
library(sf)
library(ggforce)
library(FSA)

dat2<-read.csv('./now_export_06_11.csv') #this file is an export of the NOW database#  

# select the LIDNUMs want # 
dat2<- subset(dat2, LIDNUM==28666|LIDNUM==23189| LIDNUM == 21050 | LIDNUM == 23189 | LIDNUM == 23190 | LIDNUM == 23200 | LIDNUM == 23201 | LIDNUM == 23225 | 
                LIDNUM == 23246 |LIDNUM == 23247 | LIDNUM == 23521 | LIDNUM == 23682 | LIDNUM == 23858 | LIDNUM == 23864 | LIDNUM == 24398 | LIDNUM == 23931 | 
                LIDNUM == 24084 |LIDNUM == 24089 | LIDNUM == 24094 | LIDNUM == 24147 | LIDNUM == 24149 | LIDNUM == 24150 |
                LIDNUM == 24261 |LIDNUM == 24348 | LIDNUM == 24397 | LIDNUM == 24400 | LIDNUM == 24401 | LIDNUM == 24402 | LIDNUM == 24403 | LIDNUM == 24404 |
                LIDNUM == 24436 |LIDNUM == 24455 | LIDNUM == 24554 | LIDNUM == 26905 | LIDNUM == 26913 | LIDNUM == 26914 | LIDNUM == 26915 | LIDNUM == 26916 |
                LIDNUM == 26960 |LIDNUM == 27897 | LIDNUM == 27898 | LIDNUM == 27941 | LIDNUM == 27996 | LIDNUM == 27998 | LIDNUM == 28091 | LIDNUM == 28095 |
                LIDNUM == 28327 |LIDNUM == 28328 | LIDNUM == 28339 | LIDNUM == 28340 | LIDNUM == 28344 | LIDNUM == 28345 | LIDNUM == 28346 | LIDNUM == 28347 | 
                LIDNUM == 28348 |LIDNUM == 28362 | LIDNUM == 28363 | LIDNUM == 28364 | LIDNUM == 28366 | LIDNUM == 28367 | LIDNUM == 28369 | LIDNUM == 28372 | 
                LIDNUM == 28373 |LIDNUM == 28392 | LIDNUM == 28397 | LIDNUM == 28415 | LIDNUM == 28455 | LIDNUM == 28652 | LIDNUM == 28653 | LIDNUM == 28656 | 
                LIDNUM == 29096 |LIDNUM == 29097 | LIDNUM == 29106 | LIDNUM == 29108 | LIDNUM == 29151 | LIDNUM == 29152 | LIDNUM == 29184 | LIDNUM == 29185 | 
                LIDNUM == 29187 |LIDNUM == 29308 | LIDNUM == 29369 | LIDNUM == 29370 | LIDNUM == 29371 | LIDNUM == 29372 | 
                LIDNUM == 29373 |LIDNUM == 29385 | LIDNUM == 29386 | LIDNUM == 29387 | LIDNUM == 29394 | LIDNUM == 29395 | LIDNUM == 29444 | LIDNUM == 29442 | 
                LIDNUM == 29443 |LIDNUM == 29451 |LIDNUM == 29446 )
#group these lidnums for ATAPUERCA TD 4-6 27897, 27898, 24397,24398# grouped as 27897#  
dat2$LIDNUM[dat2$LIDNUM== 27898] <- 27897
dat2$LIDNUM[dat2$LIDNUM== 24397] <- 27897
dat2$LIDNUM[dat2$LIDNUM== 24398] <- 27897
#group these lidnums for Atapuerca TE 9-14 24401, 29446, 24402, 24403, 24404, 24400# grouped as 24401# 
dat2$LIDNUM[dat2$LIDNUM== 29446] <- 24401
dat2$LIDNUM[dat2$LIDNUM== 24402] <- 24401
dat2$LIDNUM[dat2$LIDNUM== 24403] <- 24401
dat2$LIDNUM[dat2$LIDNUM== 24404] <- 24401
dat2$LIDNUM[dat2$LIDNUM== 24400] <- 24401 
#group 29185, 29184, 29096 for  Majangou Nihewan # grouped as 29185 
dat2$LIDNUM[dat2$LIDNUM== 29184] <- 29185
dat2$LIDNUM[dat2$LIDNUM== 29096] <- 29185
# group these lidnums for Barranc de la boella 29372, 29373, 29371, 29370 # grouped as 29370# 
dat2$LIDNUM[dat2$LIDNUM== 29372] <- 29370
dat2$LIDNUM[dat2$LIDNUM== 29373] <- 29370
dat2$LIDNUM[dat2$LIDNUM== 29371] <- 29370
#azyhk cave 27998 grouped with 27996#
dat2$LIDNUM[dat2$LIDNUM==27998] <- 27996

#order Artiodactyla, Perissodactyla, Primates, Proboscidea#
art <- dat2 %>% filter(dat2$ORDER == 'Artiodactyla')  
per<- dat2 %>% filter(dat2$ORDER=='Perissodactyla') 
pri <- dat2 %>% filter(dat2$ORDER=='Primates') 
pri_nohomo <- subset(pri, !(pri$GENUS %in% c('Homo')))
pro <- dat2 %>% filter(dat2$ORDER=='Proboscidea') 

dat1 <- rbind(art,per,pri,pro) 
dat1_nohomo <- rbind(art,per,pri_nohomo,pro)

dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28872']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='35291']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29053']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85726']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29977']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='30166']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='31389']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82317']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29960']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='31383']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='84443']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29864']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82301']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='84312']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29791']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29921']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29926']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='30070']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82296']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82362']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82363']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28192']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82280']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82360']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82361']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27343']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27650']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='84309']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85301']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27990']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28293']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29293']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29304']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='32089']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27690']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28121']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85168']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27791']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28000']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28122']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28787']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29200']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='32057']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='32984']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='86115']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82311']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85787']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='86294']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82295']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27924']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='32947']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29307']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85725']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='86281']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29918']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29586']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='34084']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82358']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85743']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85744']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85727']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='86125']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='31014']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='21345']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='32950']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82354']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82314']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85933']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29862']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='22744']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='27928']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29802']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='30155']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='33973']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85606']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85840']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='30094']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28294']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29786']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='86331']<-'bra/mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85729']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='31382']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29787']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29785']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82298']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='85730']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='28840']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29789']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='34129']<-'hyp' 
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29920']<-'mes' 
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='82297']<-'hyp' 
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='31951']<-'hyp' 
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='33520']<-'hyp'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='86311']<-'mes'   
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29792']<-'bra'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29707']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='29708']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='34128']<-'mes'
dat1_nohomo$TCRWNHT[dat1_nohomo$SIDNUM=='34130']<-'mes'

dat1_nohomo<-dat1_nohomo%>% mutate(hyps = case_when(TCRWNHT =='hyp'~3, TCRWNHT == 'mes'~2, TCRWNHT == 'bra'~1, TCRWNHT == 'bra/mes'~1.5))

dat2 <- dat1_nohomo 

#28186 Hippopotamus/Hexaprotodon# 
hip <- dat2 %>% filter(dat2$GENUS == 'Hippopotamus')  
hex <- dat2 %>% filter(dat2$GENUS == 'Hexaprotodon')
hip_hex <- rbind(hip,hex)
hip_hex_mean <- mean(hip_hex$hyps, na.rm=T) 
dat2$hyps[dat2$SIDNUM=='28186']<-hip_hex_mean 

#21023 Bovidae indet. indet. no subfamily# 
bov <- dat2 %>% filter(dat2$FAMILY =='Bovidae') 
bov_mean <- mean(bov$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21023']<-bov_mean

#21026 Giraffidae indet. indet. no subfamily#
gir<- dat2 %>% filter(dat2$FAMILY=='Giraffidae')
gir_mean <- mean(gir$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21026'] <-gir_mean

#21158 Bovini (subfam) indet. indet.#
bovi<- dat2 %>% filter(dat2$SUBFAMILY=='Bovini')
bovi_mean <- mean(bovi$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21158'] <-bovi_mean

#21160 Redunicini (subfam) indet. indet.#
redi<- dat2 %>% filter(dat2$SUBFAMILY=='Reduncini')
redi_mean <- mean(redi$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21160'] <-redi_mean

#21165 Antilopini(subfam) indet. indet. # 
anti<- dat2 %>% filter(dat2$SUBFAMILY=='Antilopini')
anti_mean <- mean(anti$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21165'] <-anti_mean

#21166 Cephalophini (subfam) indet. indet.#
cepi<- dat2 %>% filter(dat2$SUBFAMILY=='Cephalophini')
cepi_mean <- mean(cepi$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21166'] <-cepi_mean

#21177 Neotragini (subfam) indet. indet.# 
neo<- dat2 %>% filter(dat2$SUBFAMILY=='Neotragini')
neo_mean <- mean(neo$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21177'] <-neo_mean

#21659 Rhino- fam indet. indet.#
rhi<- dat2 %>% filter(dat2$FAMILY=='Rhinocerotidae')
rhi_mean <- mean(rhi$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21659'] <-rhi_mean

#25812 Bovinae (subfam) indet. indet.#
bovi<- dat2 %>% filter(dat2$SUBFAMILY=='Bovinae')
bovi_mean <- mean(bovi$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='25812'] <-bovi_mean

#27355 Tragelaphini (subfam) indet. indet. # 
trag<- dat2 %>% filter(dat2$SUBFAMILY=='Tragelaphini')
trag_mean <- mean(trag$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='27355'] <-trag_mean

#27930 Reducini (subfam-large) indet. indet.# 
dat2$hyps[dat2$SIDNUM=='27930'] <-redi_mean
#27963 Reducini (subfam-med) indet. indet.# 
dat2$hyps[dat2$SIDNUM=='27963'] <-redi_mean
#27964 Reducini (subfam-small) indet. indet.# 
dat2$hyps[dat2$SIDNUM=='27964'] <-redi_mean

#28211 Hippopotamidae - fam indet. indet. #
hipe<- dat2 %>% filter(dat2$FAMILY=='Hippopotamidae')
hipe_mean <- mean(hipe$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='28211'] <-hipe_mean

#85540 Bovidae indet. indet. 75-150kg#
dat2$hyps[dat2$SIDNUM=='85540']<-bov_mean

#86112 Suidae indet. indet large # 
suid<- dat2 %>% filter(dat2$FAMILY=='Suidae')
suid_mean <- mean(suid$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='86112'] <-suid_mean
#86113 Suidae indet. indet. medium#
dat2$hyps[dat2$SIDNUM=='86113'] <-suid_mean

#86114 Tragelaphini indet. indet. large #
dat2$hyps[dat2$SIDNUM=='86114'] <-trag_mean

#21400 Cervinae indet.# 
cerv<- dat2 %>% filter(dat2$SUBFAMILY=='Cervinae') 
cerv_mean <- mean(cerv$hyps, na.rm=T)
dat2$hyps[dat2$SIDNUM=='21400'] <-cerv_mean 

#mean hyp @ each LIDNUM#
dat3 <- aggregate(hyps ~ LIDNUM, data = dat2, FUN = mean)
locdat1 <- distinct(dat2, LIDNUM, .keep_all=TRUE) 
locdat1 <- locdat1 %>% dplyr::select(5:6)

#regions for visualisation# 
dat3<- dat3 %>% mutate(region = case_when(LIDNUM=='29443'|LIDNUM=='23931'|LIDNUM=='27941'|LIDNUM=='28347'|LIDNUM=='23858'|LIDNUM=='28364'|LIDNUM=='23682'|LIDNUM=='28339'|LIDNUM=='28346'|LIDNUM=='28362'|LIDNUM=='29444'|
                                            LIDNUM=='28363'|LIDNUM=='28369'|LIDNUM=='28327'|LIDNUM=='23225'|LIDNUM=='23247'|LIDNUM=='26913'|LIDNUM=='28415'|LIDNUM=='28372'|LIDNUM=='23189'|LIDNUM=='26905'|LIDNUM=='29442'|
                                            LIDNUM=='23201'|LIDNUM=='26914'|LIDNUM=='26915'|LIDNUM=='28392'|LIDNUM=='28373'|LIDNUM=='23246'|LIDNUM=='26916'|LIDNUM=='28340'|LIDNUM=='28345'|LIDNUM=='29386'|LIDNUM=='28397'|
                                            LIDNUM=='28366'|LIDNUM=='28328'|LIDNUM=='29395'|LIDNUM=='28344'|LIDNUM=='28091'|LIDNUM=='28367'|LIDNUM=='23190'|LIDNUM=='29394'|LIDNUM=='26960'|LIDNUM=='28095'|LIDNUM=='28348'|
                                            LIDNUM=='23864'|LIDNUM=='29385'|LIDNUM=='29387'	~'Africa',
                                          LIDNUM=='23200'|LIDNUM=='24084'|LIDNUM=='24149'|LIDNUM=='24554'|LIDNUM=='24147'|LIDNUM=='24150'|LIDNUM=='24089'|LIDNUM=='24094'|LIDNUM=='29097'|
                                            LIDNUM=='29308'|LIDNUM=='29185'|LIDNUM=='29151'|LIDNUM=='29106'|LIDNUM=='29108'|LIDNUM=='29152'~'Asia',
                                          LIDNUM=='24401'|LIDNUM=='27897'|LIDNUM=='29370'|LIDNUM=='29187'|LIDNUM=='24455'|LIDNUM=='24261'|LIDNUM=='28652'|LIDNUM=='28653'|LIDNUM=='28666'|LIDNUM=='29369'|LIDNUM=='24436'|
                                            LIDNUM=='28656'	~'Europe', 
                                          LIDNUM=='28455'|LIDNUM=='27996'|LIDNUM=='29451'|LIDNUM=='21050'|LIDNUM=='24348'|LIDNUM=='23521'~ 'Levant & Caucasus'))
#violin plot# species level with homo# 
dat3$region  <- factor(dat3$region,levels = c('Africa', 'Levant & Caucasus','Asia','Europe'))
vio <- ggplot(dat3, aes(x=region, y=hyps, fill=region))+
  geom_violin()+ 
  geom_sina(size=3,alpha=0.8)+
  scale_fill_manual(values = c('Africa' = '#D9534F','Levant & Caucasus'='#bada55', 'Asia'= '#800080','Europe'='#428bca' ))+
  ylab('Mean Ordinated Hypsodonty')+ xlab('Region')+ 
  theme(text=element_text(size=20), axis.title.x = element_text(hjust=2),
        axis.title.y = element_text(hjust=2)) + 
  theme_classic()
vio +theme(text=element_text(size=23)) + theme(
  axis.title.x = element_text(vjust=-0.2),
  axis.title.y = element_text(vjust=1.75)) + theme(legend.position="none")

#genus level# 
#gendat<- distinct(spdat, GENUS, .keep_all = TRUE) 
#write.csv(gendat, file='./nowgenus_20_10.csv')
my_gen <- read.csv('./my_gen.csv')
my_gen<-my_gen%>% mutate(hyps= case_when(Score =='hyp'~3, Score == 'mes'~2, Score == 'bra'~1, Score == 'bra/mes'~1.5))
#calculate avgs. for mixed hyps genera# 
#Kolpochoerus
kol <- dat2 %>% filter(dat2$GENUS =='Kolpochoerus')
kol_mean<- mean(kol$hyps, na.rm=T)
my_gen$hyps[my_gen$GENUS=='Kolpochoerus']<-kol_mean
#Gazella
gaz <- dat2 %>% filter(dat2$GENUS =='Gazella')
gaz_mean<- mean(gaz$hyps, na.rm=T)
my_gen$hyps[my_gen$GENUS=='Gazella']<-gaz_mean
#Tragelaphus
tra<- dat2 %>% filter(dat2$GENUS =='Tragelaphus')
tra_mean<- mean(tra$hyps, na.rm=T)
my_gen$hyps[my_gen$GENUS=='Tragelaphus']<-tra_mean
#Cervus
cer<- dat2 %>% filter(dat2$GENUS =='Cervus')
cer_mean<- mean(cer$hyps, na.rm=T)
my_gen$hyps[my_gen$GENUS=='Cervus']<-cer_mean
#notochoerus
not<- dat2 %>% filter(dat2$GENUS =='Notochoerus')
not_mean<- mean(not$hyps, na.rm=T)
my_gen$hyps[my_gen$GENUS=='Notochoerus']<-not_mean
#Diceros
dic<- dat2 %>% filter(dat2$GENUS =='Diceros')
dic_mean<- mean(dic$hyps, na.rm=T)
my_gen$hyps[my_gen$GENUS=='Diceros']<-dic_mean

#cut dataset (dat2) to genus level# 
#keep each genus at site only once & fill in scores#
#indet<- dat2 %>% filter(dat2$GENUS=='indet.') 
#Indet<-dat2%>%filter(dat2$GENUS=='Indet.') 
#gen<-dat2%>%filter(dat2$GENUS=='gen.')
#ABOVEGENINDETS<-rbind(indet,Indet,gen)
#write.csv(ABOVEGENINDETS,file='./abovegenindets.csv')
dat2 <- subset(dat2, GENUS !='Indet.')
dat2 <- subset(dat2, GENUS !='indet.')
dat2 <- subset(dat2, GENUS !='gen.')
a_genindets<- read.csv('./abovegenindets.csv') 
a_genindets <- subset( a_genindets, select = -X )
my_indets <- distinct(a_genindets, GENUS,.keep_all = T)
my_indets<- subset(my_indets, select = c('ORDER','FAMILY','GENUS'))
my_indets[, 'Score'] = NA
my_indets[, 'hyps'] = NA
my_gen<-rbind(my_gen, my_indets)
my_gen_nohomo<- my_gen[-c(34),]
dat2<-rbind(a_genindets,dat2)
gen_dat<- dat2 %>% group_by(LIDNUM) %>% distinct(GENUS, .keep_all=TRUE) %>% ungroup()
gig <- data.frame(ORDER= c('Primates'), FAMILY=c('Hominidae'), GENUS=c('Gigantopithecus'), Score = c('bra'), hyps=c('1'))
my_gen2<- rbind(gig, my_gen_nohomo)
gen_dat2 <- merge(my_gen2, gen_dat, by = c('GENUS'))
#fill in scores# 
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21161'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27931'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27968'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21165'] <-anti_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21023']<- bov_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='85540'] <-bov_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='25812'] <-bovi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21158'] <-bovi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27973'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='23849'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21166'] <-cepi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21025'] <-1
gen_dat2$hyps.x[gen_dat2$SIDNUM=='22154'] <-1
gen_dat2$hyps.x[gen_dat2$SIDNUM=='86311'] <-2 
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21175'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='31951'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='23057'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21026'] <-gir_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='29440'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='28211'] <-hipe_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21159'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='24328'] <-1
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21177'] <-neo_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27615'] <-1
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21160'] <-redi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27963'] <-redi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27930'] <-redi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='21659'] <-rhi_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='33520'] <-3
gen_dat2$hyps.x[gen_dat2$SIDNUM=='86113'] <-suid_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='86112'] <-suid_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='27355'] <-trag_mean
gen_dat2$hyps.x[gen_dat2$SIDNUM=='86114'] <-trag_mean

#aggregate lidnums and get mean#  
dat4<-subset(gen_dat2, select = c('GENUS', 'ORDER.x', 'FAMILY.x', 'Score','hyps.x','LIDNUM','NAME'))
dat4$hyps.x <- as.numeric(as.character(dat4$hyps.x))
dat5 <- aggregate(hyps.x ~ LIDNUM, data = dat4, FUN = mean) 
names(dat5)[names(dat5)=="hyps.x"] <- "hyps"
#assign LIDNUMs to region# 
dat5<- dat5 %>% mutate(region = case_when(LIDNUM=='29443'|LIDNUM=='23931'|LIDNUM=='27941'|LIDNUM=='28347'|LIDNUM=='23858'|LIDNUM=='28364'|LIDNUM=='23682'|LIDNUM=='28339'|LIDNUM=='28346'|LIDNUM=='28362'|LIDNUM=='29444'|
                                            LIDNUM=='28363'|LIDNUM=='28369'|LIDNUM=='28327'|LIDNUM=='23225'|LIDNUM=='23247'|LIDNUM=='26913'|LIDNUM=='28415'|LIDNUM=='28372'|LIDNUM=='23189'|LIDNUM=='26905'|LIDNUM=='29442'|
                                            LIDNUM=='23201'|LIDNUM=='26914'|LIDNUM=='26915'|LIDNUM=='28392'|LIDNUM=='28373'|LIDNUM=='23246'|LIDNUM=='26916'|LIDNUM=='28340'|LIDNUM=='28345'|LIDNUM=='29386'|LIDNUM=='28397'|
                                            LIDNUM=='28366'|LIDNUM=='28328'|LIDNUM=='29395'|LIDNUM=='28344'|LIDNUM=='28091'|LIDNUM=='28367'|LIDNUM=='23190'|LIDNUM=='29394'|LIDNUM=='26960'|LIDNUM=='28095'|LIDNUM=='28348'|
                                            LIDNUM=='23864'|LIDNUM=='29385'|LIDNUM=='29387'	~'Africa',
                                          LIDNUM=='23200'|LIDNUM=='24084'|LIDNUM=='24149'|LIDNUM=='24554'|LIDNUM=='24147'|LIDNUM=='24150'|LIDNUM=='24089'|LIDNUM=='24094'|LIDNUM=='29097'|
                                            LIDNUM=='29308'|LIDNUM=='29185'|LIDNUM=='29151'|LIDNUM=='29106'|LIDNUM=='29108'|LIDNUM=='29152'~'Asia',
                                          LIDNUM=='24401'|LIDNUM=='27897'|LIDNUM=='29370'|LIDNUM=='29187'|LIDNUM=='24455'|LIDNUM=='24261'|LIDNUM=='28652'|LIDNUM=='28653'|LIDNUM=='28666'|LIDNUM=='29369'|LIDNUM=='24436'|
                                            LIDNUM=='28656'	~'Europe',LIDNUM=='28455'|LIDNUM=='27996'|LIDNUM=='29451'|LIDNUM=='21050'|LIDNUM=='24348'|LIDNUM=='23521'~ 'Levant & Caucasus'))
#violin plot 
dat5$region  <- factor(dat5$region,levels = c('Africa', 'Levant & Caucasus','Asia','Europe'))
vio <- ggplot(dat5, aes(x=region, y=hyps, fill=region))+
  geom_violin()+ 
  geom_sina(size=3,alpha=0.8)+
  scale_fill_manual(values = c('Africa' = '#D9534F','Levant & Caucasus'='#bada55', 'Asia'= '#800080','Europe'='#428bca'))+
  ylab('Mean Ordinated Hypsodonty')+ xlab('Region')+ 
  theme(text=element_text(size=20), axis.title.x = element_text(hjust=2),
        axis.title.y = element_text(hjust=2)) + 
  theme_classic()
vio +theme(text=element_text(size=23)) + theme(
  axis.title.x = element_text(vjust=-0.2),
  axis.title.y = element_text(vjust=1.75)) + theme(legend.position="none")

#lets make a map# 
#get data back to with homo version#  
#get co-ords for dat 5# 
loc_grab <-  subset(gen_dat2, select = c('LIDNUM','NAME','LAT','LONG'))
dat6 <- merge(loc_grab, dat5, by = c('LIDNUM'))  
dat6$hyps<- round(dat6$hyps, digits=2)

world.map <- map_data("world")
world.map.plot <- ggplot() + geom_polygon(data=world.map, aes(x = long, y = lat, group=group), colour = '#d1beaa',fill='#d1beaa') + coord_map(xlim=c(-180,180)) + theme_void() + theme(panel.background = element_rect(fill = 'white',size = 0.5, linetype = 'solid',colour='white'))
world.map.plot
world.map.crop<- world.map.plot + coord_sf(xlim = c(-12, 121), ylim = c(-31.5, 47.5))
world.map.crop
#map of points#
loc_map <- world.map.crop + geom_point(data=dat6,aes(x=LONG,y=LAT,colour=region),alpha=0.8, size=3)+ scale_color_manual(values = c('Africa' = '#D9534F','Levant & Caucasus'='#bada55', 'Asia'= '#800080','Europe'='#428bca' ))
loc_map+ theme(legend.position="none") 
#points coloured by hypsodonty# 
hyp_map <- world.map.crop + geom_point(data=dat6,aes(x=LONG,y=LAT,colour=hyps),alpha=0.6, size=8)+ 
  scale_colour_gradient(low='#e0fa45', high='#ff609d')    
hyp_map  

#northern, southern, eastern africa plot#  
afdat <- filter(dat5, region == "Africa")
afdat<- afdat%>%mutate(region = case_when(LIDNUM=='23201'|LIDNUM=='23225'|LIDNUM=='23246'|LIDNUM=='23247'|LIDNUM=='23682'|LIDNUM=='23858'|LIDNUM=='23931'|LIDNUM=='26905'|
                                            LIDNUM=='28327'|LIDNUM=='28328'|LIDNUM=='28339'|LIDNUM=='28340'|LIDNUM=='28344'|LIDNUM=='28345'|LIDNUM=='28346'|LIDNUM=='28347'|
                                            LIDNUM=='28348'|LIDNUM=='28362'|LIDNUM=='28363'|LIDNUM=='28364'|LIDNUM=='28366'|LIDNUM=='28367'|LIDNUM=='28369'|LIDNUM=='28372'|
                                            LIDNUM=='28373'|LIDNUM=='28392'|LIDNUM=='28397'|LIDNUM=='28415'|LIDNUM=='29385'|LIDNUM=='29386'|LIDNUM=='29387'|LIDNUM=='29394'|
                                            LIDNUM=='29395'|LIDNUM=='29442'|LIDNUM=='29443'|LIDNUM=='29444'	~ 'Eastern Africa',LIDNUM=='23189'|LIDNUM=='23190'|LIDNUM=='26960'	~ 'Northern Africa',
                                          LIDNUM=='23864'|LIDNUM=='26913'|LIDNUM=='26914'|LIDNUM=='26915'|LIDNUM=='26916'|LIDNUM=='27941'|LIDNUM=='28091'|LIDNUM=='28095'	~ 'Southern Africa'))
#edit this to be for African regions!
afdat$region  <- factor(afdat$region,levels = c('Northern Africa', 'Eastern Africa','Southern Africa'))
vio <- ggplot(afdat, aes(x=region, y=hyps, fill=region))+
  geom_violin()+ 
  geom_sina(size=3,alpha=0.8)+
  scale_fill_manual(values = c('Northern Africa' = '#ffb9b9','Eastern Africa'='#ee7272', 'Southern Africa'= '#a31818'))+
  ylab('Mean Ordinated Hypsodonty')+ xlab('Region')+ 
  theme(text=element_text(size=20), axis.title.x = element_text(hjust=2),
        axis.title.y = element_text(hjust=2)) + 
  theme_classic()
vio +theme(text=element_text(size=23)) + 
  theme(axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=1.75)) + theme(legend.position="none")

#stats stats stats# 
kw_all <- kruskal.test(hyps ~ region, data = dat5) #p-value =  p-value = 9.83e-05 = need Dunn's 
kw_af <- kruskal.test(hyps ~ region, data = afdat) #p-value = 0.2411 
dunn_all <- dunnTest(hyps ~ region, data=dat5,method="bonferroni")
kw_holm <- kruskal.test(hyps ~ region, data = dat5, method='holm')
dunn_holm <-dunnTest(hyps ~ region, data=dat5,method="holm")
library("ggplot2"); theme_set(theme_bw())
library("lmPerm")
library("coin")
library("gtools") 

#make dat with Eurasia & Africa as regions# 
dat7<- dat5 
fix(dat7)

set.seed(101) ## for reproducibility
nsim <- 9999
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(dat7))
  bdat <- transform(dat7,hyps=hyps[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat$hyps[bdat$region=="Africa"])-
    mean(bdat$hyps[bdat$region=="Eurasia"])
}
obs <- mean(dat7$hyps[dat7$region=="Africa"])-
  mean(dat7$hyps[dat7$region=="Eurasia"])
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red") 

summary(lmp(hyps~region,data=dat7))
