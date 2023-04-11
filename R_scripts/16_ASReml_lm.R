rm(list = ls())

library(interactions)
library(jtools)
library(tidyverse)
library(ggcorrplot)
library(patchwork)
library(Matrix)
library(asreml)
library(asremlPlus)
library(data.table)
library(xlsx)
library(reshape2)
require(sqldf)
library(emmeans)
library(ggplot2)
library(ggpubr)
library(dae)

setwd("~/Documents/git/Dreger_2022/raw_data/")

b1 <- read.csv("cols_rows1.csv")
colnames(b1)
head(b1)

b2 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/harvest_dates.csv")
b2$date <- as.Date(b2$date, format = "%m/%d/%y")
b2$date_init <- as.Date(b2$date_init, format = "%m/%d/%y")
lev1 <- c("env","loc","year")
b2[,lev1] <- lapply(b2[,lev1], factor)
head(b2)
b2 <- b2[,-c(5,12)]

b3 <- read.csv("~/Documents/git/Dreger_2022/raw_data/FD_raw2.csv") # FD class

a1 <- read.csv("pheno1.csv")
a2 <- read.csv("all_19.csv")
a3 <- read.csv("all_18.csv")
a4 <- read.csv("all_20_21.csv")
a5 <- read.csv("all_20.csv")

colnames(a1) # 18 and 19 cut 1
colnames(a2) # 19 cuts 2-5
colnames(a3) # 18 cuts 2-3
colnames(a4) # 20 and 21 cut 1
colnames(a5) # 20 cuts 2-5

a1 <- a1 %>% select(., -c("Name"))
a2 <- a2 %>% select(., -c("Variety"))

a1 <- inner_join(b1, a1, by = c("ID","block","position","loc","gen")) 
a2 <- inner_join(b1, a2, by = c("loc","ID")) 
a3 <- inner_join(b1, a3, by = c("ID")) 
a4 <- inner_join(b1, a4, by = c("block","position","loc","gen")) 
a5 <- inner_join(b1, a5, by = c("ID")) 

# to estimate RFV

a1$RFV <- ((88.9 - (.779 * a1$ADF)) * (120/a1$aNDF)) / 1.29
a2$RFV <- ((88.9 - (.779 * a2$ADF)) * (120/a2$aNDF)) / 1.29
a3$RFV <- ((88.9 - (.779 * a3$ADF)) * (120/a3$aNDF)) / 1.29
a4$RFV <- ((88.9 - (.779 * a4$ADF)) * (120/a4$aNDF)) / 1.29
a5$RFV <- ((88.9 - (.779 * a5$ADF)) * (120/a5$aNDF)) / 1.29

a1 <- a1 %>% gather(key = "trait", value = "raw", 10:25)
a2 <- a2 %>% gather(key = "trait", value = "raw", 10:28)
a3 <- a3 %>% gather(key = "trait", value = "raw", 10:25)
a4 <- a4 %>% gather(key = "trait", value = "raw", 10:30)
a5 <- a5 %>% gather(key = "trait", value = "raw", 10:28)

head(a1)
head(a2)
head(a3)
head(a4)
head(a5)

common_ID <- unique(a2$ID)

a6 <- rbind(a1, a2, a3, a4, a5)
a6$cut <- as.factor(a6$cut)
a6$cut <- ordered(a6$cut)
a6$ID <- as.factor(a6$ID)
a6$trait <- as.factor(a6$trait)
levels(a6$trait)
# a6 <- a6 %>% dplyr::filter(ID %in% common_ID)

a6$gen <- as.factor(a6$gen)
b3$gen <- as.factor(b3$gen)
a6 <- inner_join(b3, a6, by = "gen")

# a6 <- a6 %>% dplyr::filter(!trait == "DM")
a6 <- a6 %>% dplyr::filter(!trait %in% c("DM","IVTDMD30","IVTDMD48","TTNDFD"))
a6 <- droplevels(a6)
a7 <- a6 %>% dplyr::filter(!gen == "202")
a7 <- droplevels(a7)
levels(a7$gen)
a7.4 <- a7
# ID block 1,4,1,6,2
# OR block 4,8,3,4,3
# WA block 8,4,2,8,11

a7.1 <- a7 %>% dplyr::filter(loc == "ID") %>% dplyr::filter(block %in% c(1,4,1,6,2))
a7.2 <- a7 %>% dplyr::filter(loc == "OR") %>% dplyr::filter(block %in% c(4,8,3,4,3))
a7.3 <- a7 %>% dplyr::filter(loc == "WA") %>% dplyr::filter(block %in% c(8,4,2,8,11))
a7.4 <- rbind(a7.1,a7.2,a7.3)

data2 <- a7.4 %>% dplyr::filter(gen %in% c(201))
data2 <- data2 %>% dplyr::select(block, raw, cut, year, trait, loc)
colnames(data2)[2] <- "cv"
a7.4 <- inner_join(a7.4, data2, by = c("block", "cut", "year", "trait", "loc"))

a7.5 <- a7.4 %>% dplyr::filter(year == "2018")
a7.5 <- droplevels(a7.5)
levels(a7.5$cut)
a7.5$cut <- recode_factor(a7.5$cut, 
                          "1" = "2", "2" = "3", "3" = "4")

a7.6 <- a7.4 %>% dplyr::filter(!year == "2018") %>% dplyr::filter(!cut == "5")

a7.6 <- rbind(a7.6, a7.5)
levels(a7.6$cut)
a7.6 <- droplevels(a7.6)

a7.6 <- a7.4 %>% dplyr::filter(year %in% c("2019","2020")) %>% dplyr::filter(!cut == "5")
a8 <- a7.6 %>% unite("env2", c(loc, year, cut, trait), sep = "_", remove = F)


a8 <- split(a8, a8$env2)
names(a8)

ST0 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  data <- na.omit(data)
  
  data$FD <- as.factor(data$FD)
  data$FD <- ordered(data$FD)
  head(data)
  
  m3 <- asreml::asreml(fixed = raw ~ cv + FD,
                       data = data,
                       na.action = list(x = "include", y = "include"),
                       family = asreml::asr_gaussian(dispersion = 1))
  
  m3 <- update.asreml(m3)
  
  preds <- predict.asreml(m3, classify="FD", vcov=TRUE, aliased = T)
  pvals <- preds$pvals
  sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value), ] <- 0
  vcov <- as.matrix(preds$vcov)
  vcov <- vcov[sel == 1, sel == 1]
  pvals$weight[sel == 1] <- diag(solve(vcov))
  
  ST0[[length(ST0)+1]] <- pvals
  
}

length(a8)
length(ST0)
names(ST0) <- names(a8)

ST01 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")

ST01 <- ST01 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)

ST01 <- inner_join(ST01, b2, by = c("env","loc","year"))

colnames(ST01)
ST01 <- as.data.frame(ST01)
lev2 <- colnames(ST01)[c(1:8,13,21)]
ST01[,lev2] <- lapply(ST01[,lev2], factor)
str(ST01)

####

ST02 <- ST01
# ST02 <- ST01 %>% dplyr::filter(year %in% c(2019,2020)) %>% dplyr::filter(!cut == "5")
# ST02 <- ST01 %>% dplyr::filter(!cut == "5")

cc <- dplyr::count(ST02, year, cut, loc)

ST03 <- split(ST02, ST02$trait)
ST03 <- ST03[-c(12,13)]
names(ST02)

names(ST03)


lev6 <- c("FD","FD:loc","FD:cut","FD:cut:loc","cut")
lev5 <- c("FD","FD_loc","FD_cut","FD_cut_loc","cut")

ST05 <- list()
ST06 <- list()
for (i in 1:(length(ST03))) {
  data <- ST03[[8]]
  data1 <- na.omit(data)
  data1$env <- as.factor(data1$env)
  data1$FD <- as.factor(data1$FD)
  data1$FD <- ordered(data1$FD)
  data1$cut <- ordered(data1$cut)
  data1$year <- ordered(data1$year)
  data1$loc <- as.factor(data1$loc)
  data1$env <- as.factor(data1$env)
  head(data1)

  M1 <- asreml::asreml(fixed = predicted.value ~ FD * cut * loc,
                       random = ~ id(env):FD,
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))


  W1 <- wald.asreml(M1)
  ST06[[length(ST06)+1]] <- W1

  W2 <- summary(M1)$varcomp
  ST4[[length(ST4)+1]] <- W2
  
  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt, update = F)
  
  # lev6[j]
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predictPlus(classify = "cut", 
                         asreml.obj = M1, 
                         wald.tab = current.asrt$wald.tab, 
                         present = c("FD","loc","cut"))
    
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST05[[length(ST05)+1]] <- ST2
}



length(ST03)
length(ST05)
length(ST06)
names(ST05) <- names(ST03)
names(ST06) <- names(ST03)

