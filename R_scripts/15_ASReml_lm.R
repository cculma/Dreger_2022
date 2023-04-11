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

sum(nrow(a1),nrow(a2),nrow(a3),nrow(a4),nrow(a5))

common_ID <- unique(a2$ID)

a6 <- rbind(a1, a2, a3, a4, a5)
a6$cut <- as.factor(a6$cut)
levels(a6$cut)

a6 <- a6 %>% dplyr::filter(ID %in% common_ID)

head(b2)
a6$gen <- as.factor(a6$gen)
b3$gen <- as.factor(b3$gen)
a6 <- inner_join(b3, a6, by = "gen")
levels(a6$gen)
a6 <- a6 %>% dplyr::filter(!trait == "DM")
a6 <- droplevels(a6)
a7 <- a6 %>% dplyr::filter(!gen == "202")
a7 <- droplevels(a7)

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
# a7.4 <- a7.4 %>% dplyr::filter(!cut == "5")
# a7.4 <- a7.4 %>% dplyr::filter(year %in% c("2019","2020")) %>% dplyr::filter(!cut == "5")

a7.5 <- a7.4 %>% dplyr::filter(year == "2018")
a7.5 <- droplevels(a7.5)
levels(a7.5$cut)
a7.5$cut <- recode_factor(a7.5$cut, 
                              "1" = "2", "2" = "3", "3" = "4")

a7.6 <- a7.4 %>% dplyr::filter(!year == "2018") %>% dplyr::filter(!cut == "5")

a7.6 <- rbind(a7.6, a7.5)
levels(a7.6$cut)
a7.6 <- droplevels(a7.6)

a8 <- a7.6 %>% unite("env2", c(loc, year, cut, trait), sep = "_", remove = F)
a8 <- a7 %>% unite("env2", c(loc, year, cut, trait), sep = "_", remove = F)

a8 <- split(a8, a8$trait)
a8 <- split(a8, a8$env2)

names(a8)
a8 <- a8[-c(14,15,21)]


ST05 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  data <- na.omit(data)
  
  data$loc <- as.factor(data$loc)
  data$FD <- as.factor(data$FD)
  data$cut <- as.factor(data$cut)
  data$year <- as.factor(data$year)
  data$FD <- ordered(data$FD)
  data$cut <- ordered(data$cut)
  head(data)
  
  m3 <- asreml::asreml(fixed = raw ~ cv + FD * loc * cut,
                       random = ~ year,
                       data = data,
                       na.action = list(x = "include", y = "include"),
                       family = asreml::asr_gaussian(dispersion = 1))
  
  
  m3 <- update.asreml(m3)
  
  wald(m3)
  # preds <- predict.asreml(m3, classify="gen", vcov=TRUE, aliased = T) 
  # pvals <- preds$pvals
  # sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
  # sel[is.na(pvals$predicted.value), ] <- 0
  # vcov <- as.matrix(preds$vcov)
  # vcov <- vcov[sel == 1, sel == 1]
  # pvals$weight[sel == 1] <- diag(solve(vcov))
  
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predict.asreml(m3, classify=lev6[j], vcov=TRUE, aliased = T)$pvals
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST05[[length(ST05)+1]] <- ST2
  
}

length(ST05)
length(a8)
names(ST05) <- names(a8)


ST01 <-rbindlist(ST05, use.names=TRUE, fill=TRUE, idcol="trait")

b3$gen <- as.factor(b3$gen)
ST01 <- ST01 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)  %>% unite("env2", c(loc, year), sep = "_", remove = F) %>% unite("env3", c(year, cut), sep = "_", remove = F) %>% inner_join(., b3, by = "gen")

ST01 <- inner_join(ST01, b2, by = c("env","loc","year"))

setwd("~/Documents/git/Dreger_2022/BLUPs/")
write.csv(ST01, "ST01.csv", quote = F, row.names = F)

head(ST01)
colnames(ST01)
ST01 <- as.data.frame(ST01)
lev2 <- colnames(ST01)[c(1:8,13,21)]
ST01[,lev2] <- lapply(ST01[,lev2], factor)
str(ST01)

ST02 <- ST01
ST02 <- ST01 %>% dplyr::filter(year %in% c(2019,2020)) %>% dplyr::filter(!cut == "5")
ST02 <- ST01 %>% dplyr::filter(!cut == "5")

cc <- dplyr::count(ST02, year, cut, loc)

ST02 <- split(ST02, ST02$trait)
names(ST02)

ST03 <- ST02[-c(9,10,14,15,21)]

ST03 <- ST02[-c(14,15,21)]
names(ST03)


data <- ST03[[1]]

#########

# ST1
names(ST03)

data <- ST03[[1]] 
colnames(data)[9] <- "predicted.value"
lev2 <- colnames(data)[c(1:8,13)]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
data$FD <- ordered(data$FD)
data$cut <- ordered(data$cut)
data$year <- ordered(data$year)
data$cons_days <- ordered(data$cons_days)

data <- data %>% dplyr::filter(year %in% c("2018","2019","2020")) %>% dplyr::filter(!cut == "5")
data <- data %>% dplyr::filter(!year == "2018") %>% dplyr::filter(!cut == "5")
data <- data %>% dplyr::filter(year %in% c("2019","2020")) %>% dplyr::filter(!cut == "5")
# data <- data %>% dplyr::filter(!cut == "5")


str(data1)
data1 <- na.omit(data)
cc <- count(data1, loc, FD)

lev6 <- c("FD","FD:loc","FD:cut","FD:cut:loc","cut")
lev5 <- c("FD","FD_loc","FD_cut","FD_cut_loc","cut")

ST05 <- list()
ST06 <- list()
for (i in 1:(length(ST03))) {
  data <- ST03[[1]]
  # colnames(data)[9] <- "predicted.value"
  data <- data %>% dplyr::filter(!cut == "5")
  data1 <- na.omit(data)
  data1$env <- as.factor(data1$env)
  data1$FD <- as.factor(data1$FD)
  data1$FD <- ordered(data1$FD)
  data1$cut <- ordered(data1$cut)
  data1$year <- ordered(data1$year)
  data1$loc <- as.factor(data1$loc)
  data1$cons_days <- ordered(data1$cons_days)
  head(data1)
  
  M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                       random = ~ corgh(loc):ar1(cons_days):id(FD) + cut,
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))

  # M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
  #                      random = ~ us(loc):ar1(cut):id(FD),
  #                      data = data1, na.action = list(x = "include", y = "include"),
  #                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  # 
  # M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
  #                      random = ~ cut,
  #                      data = data1, na.action = list(x = "include", y = "include"),
  #                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  # 
  # 
  # W1 <- wald.asreml(M1)
  # ST06[[length(ST06)+1]] <- W1
  # 
  # W2 <- summary(M1)$varcomp
  # ST4[[length(ST4)+1]] <- W2
  
  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt, update = F)
  
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predictPlus(classify = lev6[j], 
                         asreml.obj = M1, 
                         wald.tab = current.asrt$wald.tab, 
                         present = c("FD","loc","cut"))
    
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST05[[length(ST05)+1]] <- ST2
}


length(ST05)
length(ST03)
length(ST06)
names(ST05) <- names(ST03)
names(ST06) <- names(ST03)

# ST05 <-rbindlist(ST05, use.names=TRUE, fill=TRUE, idcol="trait")
# ST06 <- split(ST05, ST05$trait)


setwd("~/Documents/git/Dreger_2022/BLUPs/")
write.csv(ST05, "ST05_FD.csv", quote = F, row.names = F)


###########

names(ST05)
data <- ST05[[13]]

data <- pvals
data$FD <- as.numfac(data$FD)
str(data)

preds <- predict.asreml(M1, classify="cut", vcov=TRUE, aliased = T)
# data$cut <- as.numeric(as.character(data$cut))
# data$FD <- as.numeric(as.character(data$FD))

names(ST03)
data <- ST03[[1]]
colnames(data)[9] <- "predicted.value"
data$FD <- as.factor(data$FD)
lm1 <- lm(predicted.value ~ FD * cut, data)
lm1 <- lm(predicted.value ~ cut, data)
anova(lm1)
emmeans(lm1, "cut")
lm2 <- emmeans(lm1, pairwise ~ FD | cut)

lm1 <- lm(predicted.value ~ FD * cut, data)

lm1 <- lm(predicted.value ~ FD, data)


lm2 <- emmeans(lm1, pairwise ~ FD | cut)
emmeans(lm1, "cut")
lm3 <- as.data.frame(lm2$emmeans)

data$FD <- as.numfac(data$FD)
summary(lm1)


summary(lm1)$r.squared 
anova(lm1)

ggscatter(data, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 2)
 


pvals$cut <- as.factor(pvals$cut)

ggscatter(data, x = "FD", y = "predicted.value", add = "reg.line", color = "cut", conf.int = F, point = T,  palette = "jco") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(aes(color = cut), label.x = 1)

ggscatter(data, x = "FD", y = "predicted.value", add = "reg.line", color = "cut", conf.int = T, point = T,  palette = "jco") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + scale_y_continuous(limits = c(1, 3), breaks = seq(1, 3, by = 1)) + stat_cor(aes(color = cut), label.x = 2.2)

ggscatter(data, x = "FD", y = "predicted.value", add = "reg.line", color = "cut", conf.int = T, point = T,  palette = "jco") + facet_wrap(~loc) + stat_cor(label.y = 2.5) + stat_regline_equation(aes(color = cut), label.x = 2) 


ggscatter(data, x = "FD", y = "predicted.value", add = "reg.line", color = "cut", size = 0.3,
          palette = "jco", conf.int = F, point = T, 
          facet.by = "loc") 

# + stat_cor(aes(color = loc), method = "spearman", label.x = 2)


##########
# wald
setwd("~/Documents/git/Dreger_2022/statistical_results/wald/")

lev3 <- names(ST06)

df2 <- list()
for (i in 1:length(ST06)) {
  df1 <- as.data.frame(ST06[[i]])
  colnames(df1) <- c("Df","Sum.of.Sq","Wald.statistic","Pr.Chisq.")
  df1 <- df1 %>% rownames_to_column("term")
  df1$Pr.Chisq.[df1$Pr.Chisq. < 0.001] <- "***"
  df1$Pr.Chisq.[df1$Pr.Chisq. > 0.05] <- "ns"
  df1$Pr.Chisq.[df1$Pr.Chisq. < 0.05 & df1$Pr.Chisq. > 0.01] <- "*"
  df1$Pr.Chisq.[df1$Pr.Chisq. < 0.01 & df1$Pr.Chisq. > 0.001] <- "**"
  df1 <- df1[,-4]
  df1$Sum.of.Sq <- round(df1$Sum.of.Sq, 2)
  df1$Pr.Chisq.
  df1 <- df1 %>% unite("Sum.of.Sq1", c(Sum.of.Sq, Pr.Chisq.), sep = " ", remove = T)
  df2[[length(df2)+1]] <- df1
}
names(df2) <- names(ST06)

df2 <- rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
df2 <- df2 %>% spread(key = "trait", value = "Sum.of.Sq1")

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")
write.csv(df2, "wald1.csv", quote = F, row.names = F)



#########
data1 <- ST05[["ADF"]][["FD"]][["predictions"]]



ggplot(data = data1, aes(x=FD, y=predicted.value, group = 1)) + geom_point(size = 1, alpha = 0.6) + geom_line(linewidth = 0.5, alpha = 0.6) + geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) 

# + geom_line(aes(linetype=FD, group = FD), alpha = 0.6) 

# + geom_point(aes(shape=FD), size = 1, alpha = 0.6)
#+ geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) 

# + theme_classic(base_family = "Arial", base_size = 12) 

# geom_point(aes(color = FD), size = 1, alpha = 0.6) +
#+ labs(y = "") + facet_grid(loc ~ .) 


