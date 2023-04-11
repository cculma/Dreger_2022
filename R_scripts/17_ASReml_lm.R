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
a6$gen <- as.factor(a6$gen)
levels(a6$trait)
levels(a6$gen)
# a6 <- a6 %>% dplyr::filter(cut == "1")
# a6 <- a6 %>% dplyr::filter(ID %in% common_ID)

a6$gen <- as.factor(a6$gen)
b3$gen <- as.factor(b3$gen)
a6 <- inner_join(b3, a6, by = "gen")

# a6 <- a6 %>% dplyr::filter(!trait == "DM")
# a6 <- a6 %>% dplyr::filter(!trait %in% c("DM","IVTDMD30","IVTDMD48","TTNDFD"))
a6 <- a6 %>% dplyr::filter(!trait %in% c("DM","NDFD30","NDFD48","TTNDFD"))


a6 <- droplevels(a6)
a7 <- a6 %>% dplyr::filter(!gen == "202")
a7 <- droplevels(a7)
levels(a7$gen)

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


a7.6 <- a7.4 %>% dplyr::filter(year %in% c("2019","2020")) %>% dplyr::filter(!cut == "5")
a7.6 <- a7.4 %>% dplyr::filter(!year == "2018") %>% dplyr::filter(!cut == "5")

a7.6 <- rbind(a7.6, a7.5)
levels(a7.6$cut)
a7.6 <- droplevels(a7.6)
levels(a7.6$cut)

a8 <- a7.6 %>% unite("env2", c(loc, year, cut), sep = "_", remove = F)

a8 <- split(a6, a6$trait)
a8 <- split(a8, a8$trait)
names(a8)

# a8 <- a8[-c(12,13)]

lev6 <- c("FD","FD:loc","FD:cut","FD:cut:loc","cut", "loc")
lev5 <- c("FD","FD_loc","FD_cut","FD_cut_loc","cut", "loc")

ST05 <- list()
ST06 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  data <- na.omit(data)
  
  data$loc <- as.factor(data$loc)
  data$FD <- as.factor(data$FD)
  data$cut <- as.factor(data$cut)
  data$year <- as.factor(data$year)
  data$FD <- ordered(data$FD)
  data$cut <- ordered(data$cut)
  data$env <- as.factor(data$env)
  data <- data[order(data$env), ]
  head(data)
  
  m3 <- asreml::asreml(fixed = raw ~ cv + FD * loc * cut,
                       random = ~  year ,
                       residual = ~ dsum(~units | env),
                       data = data,
                       na.action = list(x = "include", y = "include"),
                       family = asreml::asr_gaussian(dispersion = 1))
  
  m3 <- update.asreml(m3)
  # plot(m3)
  W1 <- wald(m3)
  # W1 <- wald.asreml(m3, denDF = "numeric")
  ST06[[length(ST06)+1]] <- W1
  
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predict.asreml(m3, classify=lev6[j], vcov=TRUE, aliased = T)$pvals
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST05[[length(ST05)+1]] <- ST2
  
}

length(a8)
length(ST05)
length(ST06)
names(ST05) <- names(a8)
names(ST06) <- names(a8)

FD <- ST05[[1]][[1]]
cut <- ST05[[1]][[5]]
loc <- ST05[[1]][[6]]

FD_loc <- ST05[[1]][[2]]
FD_cut <- ST05[[1]][[3]]
FD_loc_cut <- ST05[[1]][[4]]

FD_cut$loc <- NA
FD_loc$cut <- NA

FD$cut <- NA
FD$loc <- NA

cut$loc <- NA
cut$FD <- NA

loc$cut <- NA
loc$FD <- NA

data1 <- rbind(FD, cut, loc, FD_cut, FD_loc, FD_loc_cut)
colnames(data1)

data1 <- data1[,c(1,5,6,2,3)]


setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")

names(ST05)
ST07 <- list()
for (i in 1:length(ST05)) {
  data <- ST05[[i]]
  FD <- ST05[[i]][[1]]
  cut <- ST05[[i]][[5]]
  loc <- ST05[[i]][[6]]
  
  FD_loc <- ST05[[i]][[2]]
  FD_cut <- ST05[[i]][[3]]
  FD_loc_cut <- ST05[[i]][[4]]
  
  FD_cut$loc <- NA
  FD_loc$cut <- NA
  
  FD$cut <- NA
  FD$loc <- NA
  
  cut$loc <- NA
  cut$FD <- NA
  
  loc$cut <- NA
  loc$FD <- NA
  
  data1 <- rbind(FD, cut, loc, FD_cut, FD_loc, FD_loc_cut)
  
  data1 <- data1[,c(1,5,6,2,3)]
  ST07[[length(ST07)+1]] <- data1

}

names(ST07) <- names(ST05)

lapply(names(ST07), function(x) write.xlsx(ST07[[x]], 'predicted.values.xlsx', sheetName=x, append=T, row.names=T, showNA = F))
