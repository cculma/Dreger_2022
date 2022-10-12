# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)


S_FD4 <- read.csv("~/Documents/git/big_files/FD_scores.csv")
head(S_FD4)
S_FD4$gen <- as.factor(S_FD4$gen)


lev1 <- c("block", "gen", "row", "col")

setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar5 <- data_ar[c(7,21,26)] # 5_FD
data_ar6 <- data_ar[c(10,14,18)] # OR Height to FD


He_FD <- list()
M_He_FD <- list()
for (i in 1:length(data_ar6)) {
  data <- read.csv(data_ar6[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  head(data)
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data$resp <- cm(data$resp)
  data$cov1 <- cm(data$cov1)
  data$cov2 <- cm(data$cov2)
  data[,lev1] <- lapply(data[,lev1], factor)
  data1 <- inner_join(data, S_FD4, by = "gen")
  data2 <- lm(FD ~ resp, data = data1)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  data$cov1 <- data2$coefficients[2] * data$cov1 + data2$coefficients[1]
  data$cov2 <- data2$coefficients[2] * data$cov2 + data2$coefficients[1]
  
  data <- data[order(data$row, data$col), ]
  str(data)
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block , 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  i1 <- infoCriteria.asreml(m1)
  i2 <- infoCriteria.asreml(m2)
  i3 <- infoCriteria.asreml(m3)
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)
  M_He_FD[[length(M_He_FD)+1]] = data3
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  He_FD[[length(He_FD)+1]] <- blue
  
}
names(He_FD) <- gsub(".csv", "", gsub("./", "", data_ar6))
names(M_He_FD) <- gsub(".csv", "", gsub("./", "", data_ar6))

He_FD1 <-rbindlist(He_FD, use.names=TRUE, fill=TRUE, idcol="env")
head(He_FD1)


M_FD <- list()
BLUE_FD <- list()
for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  data1 <- inner_join(data, S_FD4, by = "gen")
  data2 <- lm(FD ~ resp, data = data1)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  data$cov1 <- data2$coefficients[2] * data$cov1 + data2$coefficients[1]
  data$cov2 <- data2$coefficients[2] * data$cov2 + data2$coefficients[1]
  
  data <- data[order(data$row, data$col), ]
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, 
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)
  M_FD[[length(M_FD)+1]] = data3
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  BLUE_FD[[length(BLUE_FD)+1]] = blue
}


names(M_FD) <- gsub(".csv", "", gsub("./", "", data_ar5))


names(BLUE_FD) <-  gsub(".csv", "", gsub("./", "", data_ar5))
BLUE_FD1 <-rbindlist(BLUE_FD, use.names=TRUE, fill=TRUE, idcol="env")
head(BLUE_FD1)


BLUE_FD2 <- rbind(BLUE_FD1, He_FD1)
BLUE_FD2 <- BLUE_FD2 %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
BLUE_FD2 <- as.data.frame(BLUE_FD2)
lev2 <- c("env","loc","year","cut","gen")
BLUE_FD2[,lev2] <- lapply(BLUE_FD2[,lev2], factor)
str(BLUE_FD2)
levels(BLUE_FD2$year)
levels(BLUE_FD2$gen)
levels(BLUE_FD2$loc)
head(BLUE_FD2)
hist(BLUE_FD2$BLUE)

data1 <- na.omit(BLUE_FD2)
head(data1)

M1 <- asreml::asreml(fixed = BLUE ~ gen,
                     random = ~ sfa(loc, 1):ar1(year):id(gen) + env,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))
M1 <- update.asreml(M1)
wald.asreml(M1)
summary(M1)$varcomp

current.asrt <- as.asrtests(M1, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)

diffs <- predictPlus(classify = "gen:loc", 
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("gen","loc","year","env"))
names(diffs)
ST2 <- diffs[[1]]
head(ST2)
class(ST2)
str(ST2)
dim(ST2)
hist(ST2$predicted.value)
ST3 <- ST2 %>% select(1:3)
colnames(ST3)[3] <- "FD"
ST3 <- ST3 %>% spread(key = loc, value = predicted.value) %>% column_to_rownames("gen")
cor(ST3, use = "complete")

# ~~~~~~~~~~~~~~~~~~

setwd("~/Documents/git/Dreger_2022/raw_data/")

b1 <- read.csv("cols_rows1.csv")
colnames(b1)
head(b1)

a1 <- read.csv("pheno1.csv")
a2 <- read.csv("all_19.csv")
a3 <- read.csv("all_18.csv")
a4 <- read.csv("all_20_21.csv")


colnames(a1) # 18 and 19 cut 1
colnames(a2) # 19
colnames(a3) # 18
colnames(a4) # 20 21

a1 <- a1 %>% select(., -c("Name"))
a2 <- a2 %>% select(., -c("Variety"))
a3 <- a3 %>% select(., -c("Variety"))

a1 <- inner_join(b1, a1, by = c("ID","block","position","loc","gen")) 
a2 <- inner_join(b1, a2, by = c("loc","ID")) 
a3 <- inner_join(b1, a3, by = c("ID")) 
a4 <- inner_join(b1, a4, by = c("block","position","loc","gen")) 

a1 <- a1 %>% gather(key = "trait", value = "raw", 10:24)
a2 <- a2 %>% gather(key = "trait", value = "raw", 10:27)
a3 <- a3 %>% gather(key = "trait", value = "raw", 10:24)
a4 <- a4 %>% gather(key = "trait", value = "raw", 10:29)
a3 <- a3 %>% relocate(year, .after=Cutting)

head(a1)
head(a2)
head(a3)
head(a4)

a5 <- rbind(a1, a2, a3, a4)
head(a5)
a5 <- a5 %>% unite("env", c(loc, year, Cutting, trait), sep = "_", remove = F)
lev3 <- colnames(a5)[1:11]
a5[,lev3] <- lapply(a5[,lev3], factor)
a6 <- a5 %>% dplyr::filter(Cutting %in% 1)
a6 <- droplevels(a6)

a6 <- split(a6, a6$env)

names(a6)
head(data)

ST0 <- list()
for (i in 1:length(a6)) {
  data <- a6[[i]]
  data <- data[order(data$row, data$col), ]
  m1 <- asreml::asreml(fixed = raw ~ 1 + gen, 
                       random = ~ + block , 
                       residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = raw ~ 1 + gen,
                       random = ~ + block, 
                       residual = ~sar(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = raw ~ 1 + gen,
                       random = ~ + block, 
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  i1 <- infoCriteria.asreml(m1)
  i2 <- infoCriteria.asreml(m2)
  i3 <- infoCriteria.asreml(m3)
  
  i1$model <- "ar1_id"
  i2$model <- "sar_id"
  i3$model <- "ar1_ar1"
  
  data3 <- rbind(i1, i2, i3)

  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC | i1$AIC == i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  blue$weight <- (1/blue$std.error)^2
  ST0[[length(ST0)+1]] <- blue
  
}

names(ST0) <- names(a6)

ST0 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST0)
ST01 <- ST0 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env2", c(loc, year), sep = "_", remove = F) %>% left_join(., ST3, by = c("gen", "loc")) %>% select(-c("cut", "status")) %>% relocate(FD, .after=gen)
head(ST01)
ST01 <- as.data.frame(ST01)

lev4 <- c("gen","env2","loc","year","trait")
ST01[,lev4] <- lapply(ST01[,lev4], factor)
str(ST01)
levels(ST01$env2)
levels(ST01$year)
ST02 <- split(ST01, ST01$trait)

# ~~~~~~~~~~~~~~~~
lev6 <- c("FD","loc","year","gen","env2",
          "FD:loc","FD:year","FD:env2","gen:env2")

lev5 <- c("FD","loc","year","gen","loc_year",
          "FD_loc","FD_year","FD_loc_year","gen_loc_year")
# ~~~~~~~~~~~~~~~~


ST1 <- list()
ST3 <- list()
ST4 <- list()
for (i in 1:(length(ST02))) {
  data <- ST02[[1]]
  data1 <- na.omit(data)
  head(data1)
  
  # M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
  #                      random = ~ us(loc):ar1(env3):id(gen) + year,
  #                      data = data1, na.action = list(x = "include", y = "include"),
  #                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  M1 <- asreml::asreml(cbind(predicted.value, FD) ~ trait -1,
                       random = ~ corgh(loc):ar1(year):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  
  M1 <- asreml::asreml(fixed = predicted.value ~ FD * gen,
                       random = ~ corgh(loc):ar1(year):id(gen) + env2,
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  W1 <- wald.asreml(M1)
  ST3[[length(ST3)+1]] <- W1
  
  summary(M1, coef = T)$coef.fixed
  summary(M1, coef = T)$coef.random
  W2 <- summary(M1)$varcomp
  ST4[[length(ST4)+1]] <- W2
  
  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt)
  
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predictPlus(classify = "FD", 
                         asreml.obj = M1, 
                         wald.tab = current.asrt$wald.tab, 
                         present = c("FD","gen","loc","env2"))
    
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST1[[length(ST1)+1]] <- ST2
  
}


save.image("~/Documents/git/big_files/Dreger_200.RData")
load("~/Documents/git/big_files/Dreger_200.RData")