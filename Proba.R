rm(list=ls())
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(mlr)
setwd('E://Adriana/Ostalo/Ektimo naloga')
# ucna in testna mnozica
data <- read.table('podatki.csv',sep=',',header=T,na.strings = c('','?')) 
str(data) # imamo 1000 vrstic in 35 spremenljivk
n <- length(data[,1])
data <- select(data,-Row_ID)
##############################################################################################
####################################################################################
################### Obdelava podatkov ###################################
# 1. Pri spremenljivkah Blind_model, Blind_Submodel in Blind_Make imamo
# veliko skupin.
# Če pogledamo v podatkovje vidimo, da so si te spremenljivke zelo podobne,
# kar že samo ime pove. V spremenljivki Blind_Submodel so shranjene vse informacije
# od Blind_Make in Blind_model. Odločili smo se spremenljivko Blind_Submodel razbiti:
# Najprej smo ločili na dva dela: na del s številkami in na del s črkami (= Blind_Make)
# Nato smo pa še Blind_Make v dva stolpca, v vsakem stolpcu ena črka.
# S tem smo zreducirali število kategorij pri faktorskem vektorju.

# 2. Ocenili smo približno starost avtomobila.
# Ko je bilo avto zavarovano - Model_Year, kar nam da pa tudi vrednost -1.
# To pa zgodi zato: Ko je avto že kupljen in zavarovan, so na avtu spremenili
# nek del avta (npr luci) in je potem v našem primeru lahko avto eno leto manjše,
# kot je sicer. To je o starosti najboljše kar lahko.
data <- data %>% separate(Blind_Make, into = c('Blind_M1', 'Blind_M2','Blind_M33','Blind_M34'),
                          fill='right',sep='()') %>% select(-Blind_M1,-Blind_M34) %>%
  mutate(Blind_M3 = replace(Blind_M33, Blind_M33 == '', NA)) %>%
  select(-Blind_M33)

data <- data %>%  mutate(Age = Calendar_Year - Model_Year) %>% 
  separate(Blind_Submodel, c('Blind', 'model','submodel'), sep ='[.]') %>% 
  select(-c(Blind,Blind_Model)) %>%
  unite('Submodel',c(model,submodel),sep='.')