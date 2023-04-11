rm(list = ls())

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
library(dae)
library(emmeans)
library(ggplot2)

setwd("~/Documents/git/Dreger_2022/raw_data/")
# load("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")
# save.image("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")

b1 <- read.csv("cols_rows1.csv")
colnames(b1)
head(b1)

# b1.1 <- b1 %>% dplyr::filter(loc == "WA") %>% dplyr::select(gen, col, row) %>% spread(key = "row", value = "gen")
# b1.2 <- b1 %>% dplyr::filter(loc == "OR") %>% dplyr::select(gen, col, row) %>% spread(key = "row", value = "gen")
# b1.3 <- b1 %>% dplyr::filter(loc == "ID") %>% dplyr::select(gen, col, row) %>% spread(key = "row", value = "gen")
# 
# write.csv(b1.1, "~/Documents/git/Norberg_2020/spatial_distribution/WA_row_col.csv", quote = F, row.names = F)
# write.csv(b1.2, "~/Documents/git/Norberg_2020/spatial_distribution/OR_row_col.csv", quote = F, row.names = F)
# write.csv(b1.3, "~/Documents/git/Norberg_2020/spatial_distribution/ID_row_col.csv", quote = F, row.names = F)



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


# colnames(a5)
# colnames(a1)
# a5 <- a5[,c(8,4,2,3,5,1,9,10:24)]
# a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a5')

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

a7 <- a6 %>% dplyr::filter(!gen == "202")
a7 <- a7 %>% dplyr::filter(!trait == "DM")
a7 <- droplevels(a7)
summary(a7$gen)
dplyr::count(a7, gen, loc, cut)

a7 <- a7 %>% unite("env", c(loc, year, cut), sep = "_", remove = F)
a7$year <- as.factor(a7$year)
a7 <- inner_join(b2, a7, by = c("env","loc","year"))
head(a7)

a7$trait <- as.factor(a7$trait)
summary(a7$trait)

colnames(a7)
# a7 <- a6 %>% dplyr::filter(cut == "3") %>% dplyr::filter(loc == "ID") %>% dplyr::filter(year == "2018")
# head(a7)
# a7 <- split(a7, a7$trait)


lev1 <- colnames(a7)[3:21]
a7[,lev1] <- lapply(a7[,lev1], factor)
str(a7)
levels(a7$cons_days1)
summary(a7$gen)

# a6 <- split(a5[,-1], a5$env)
# names(a6)
# str(a6[[1]])
# levels(a6[[1]]$gen)

# save.image("~/Documents/git/big_files/tidy_Dreger1.RData")
# load("~/Documents/git/big_files/tidy_Dreger1.RData")

# c201 <- a5 %>% dplyr::filter(gen %in% c(201))
# c202 <- a5 %>% dplyr::filter(gen %in% c(202))
# # head(c201)

a8 <- a7 %>% unite("env2", c(loc, year, cut, trait), sep = "_", remove = F)
a8 <- a7 %>% unite("env2", c(loc, trait), sep = "_", remove = F)
a8 <- split(a8, a8$env2)
names(a8)
length(a8)

dplyr::count(a8[[1]], gen, year, cut)
dplyr::count(a8[[1]], year, cut)


a9 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  data <- data %>% group_by(gen)%>%summarise(Average=mean(raw))
  a9[[length(a9)+1]] <- data
}

names(a9) <- names(a8)
a9 <-rbindlist(a9, use.names=TRUE, fill=TRUE, idcol="trait")

a10 <- a9 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F) %>% unite("env2", c(loc, trait), sep = "_", remove = F) %>% inner_join(., b3, by = "gen")

a10 <- split(a10, a10$env2)
a10 <- split(a10, a10$trait)

names(a10)
nrow(a10[[1]])
# ST0
head(data)
data <- a10[[8]]
hist(data$Average)
str(data)
data$env <- as.factor(data$env)
data$year <- as.factor(data$year)
data$cut <- as.factor(data$cut)
data$FD <- as.factor(data$FD)
data$loc <- as.factor(data$loc)
data$FD
data$FD <- ordered(data$FD)
data$year <- ordered(data$year)
data$year

m1 <- asreml::asreml(fixed = Average ~ FD,
                     random = ~ ~corgh(loc):year + cut,
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m2 <- asreml::asreml(fixed = Average ~ FD, 
                     random = ~ loc * year * cut,
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m1 <- update.asreml(m1)
summary(m1)$varcomp
summary(m1)$aic

lrt(m1, m2)

current.asrt <- as.asrtests(m1, NULL, NULL)
current.asrt <- as.asrtests(m1, NULL, NULL, label = "Maximal model", IClikelihood = "full")

# current.asrt <- rmboundary.asrtests(current.asrt)


diffs <- predictPlus(classify = lev6[j], 
                     asreml.obj = m1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("FD","gen","loc"))

preds <- predict.asreml(m2, classify="FD:cut", vcov=TRUE, aliased = T) 
preds <- predict.asreml(m1, classify="cut", vcov=TRUE, aliased = T) 
pvals <- preds$pvals
pvals$FD <- as.numeric(as.character(pvals$FD))
pvals$cut <- as.numeric(as.character(pvals$cut))
str(pvals)

lm1 <- lm(predicted.value ~ FD, pvals)
lm1 <- lm(predicted.value ~ FD * cut, pvals)
lm1 <- lm(predicted.value ~ cut, pvals)

summary(lm1)
aov3 <- anova(lm1)
summary(aov3)
summary(lm1)$r.squared 

pvals$cut <- as.factor(pvals$cut)

ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 2, label.y = 10) + stat_regline_equation(label.x = 2, label.y = 12) 


ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", color = "cut", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1))



data <- data[order(data$row, data$col), ] # order data by row then col
data$check <- recode_factor(data$gen, "201" = "control", .default = "test")

m1 <- asreml::asreml(fixed = raw ~ 1 + at(check, "control"):gen, 
                     random = ~ + block + at(check, "test"):gen,
                     data = data, 
                     na.action = list(x = "include", y = "include"))




ST0 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  data <- na.omit(data)
  
  m3 <- asreml::asreml(fixed = raw ~ gen,
                       random = ~ block,
                       data = data,
                       na.action = list(x = "include", y = "include"))

  m3 <- update.asreml(m3)
  
  preds <- predict.asreml(m3, classify="gen", vcov=TRUE, aliased = T) 
  pvals <- preds$pvals
  sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value), ] <- 0
  vcov <- as.matrix(preds$vcov)
  vcov <- vcov[sel == 1, sel == 1]
  pvals$weight[sel == 1] <- diag(solve(vcov))
  ST0[[length(ST0)+1]] <- pvals

}

length(ST0)
length(a8)
names(ST0) <- names(a8)
ST01 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")

b3$gen <- as.factor(b3$gen)
ST01 <- ST01 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)  %>% unite("env2", c(loc, year), sep = "_", remove = F) %>% unite("env3", c(year, cut), sep = "_", remove = F) %>% inner_join(., b3, by = "gen")

ST01 <- inner_join(ST01, b2, by = c("env","loc","year"))

# ST01 <- ST01 %>% relocate(gen, .after=env3) %>% relocate(loc, .after=gen) %>% relocate(c("FD","Var1"), .after=gen)
head(ST01)
colnames(ST01)
ST01 <- as.data.frame(ST01)
lev2 <- colnames(ST01)[c(1:8,13,21)]
ST01[,lev2] <- lapply(ST01[,lev2], factor)
str(ST01)

ST02 <- split(ST01, ST01$trait)
names(ST02)

ST03 <- ST02[-c(7,8,10,11,15,16,22)]
names(ST03)
data <- ST03[[1]]


#############
# ST1
names(ST03)
data <- ST03[[12]] # starch
str(data)
lev2 <- colnames(data)[c(1:5,9,10)]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
head(data)
# data$env <- as.factor(data$env)

ST1 <- list()
ST3 <- list()
ST4 <- list()

data <- ST03[[12]]
data1 <- na.omit(data)

for (i in 1:(length(ST03))) {
  data <- ST03[[i]]
  data1 <- na.omit(data)
  head(data1)
  
  # M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
  #                      random = ~ us(loc):ar1(env3):id(gen) + year,
  #                      data = data1, na.action = list(x = "include", y = "include"),
  #                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
                       random = ~ us(loc):ar1(c_days):id(gen) + year,
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  
  W1 <- wald.asreml(M1)
  ST3[[length(ST3)+1]] <- W1
  
  W2 <- summary(M1)$varcomp
  ST4[[length(ST4)+1]] <- W2

  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt)
  
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predictPlus(classify = lev6[j], 
                         asreml.obj = M1, 
                         wald.tab = current.asrt$wald.tab, 
                         present = c("FD","gen","loc","env4"))
   
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST1[[length(ST1)+1]] <- ST2

}

length(ST1)
length(ST03)
names(ST1) <- names(ST03)
names(ST3) <- names(ST03)
names(ST4) <- names(ST03)


diffs <- predictPlus(classify = ,"FD:c_days",
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("FD","gen","loc","c_days"))

data <- ST1[[1]]
data1 <- data[[1]][[1]]
data2 <- data[[1]]
data1 <- data[[1]][[4]]
names(ST1[1])
names(ST1[[1]])
names(data[[1]])

data1 <- ST1[[1]][[11]]
##################
# p_differences

setwd("~/Documents/git/Dreger_2022/statistical_results/p_differences_melt1/")

for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST4 <- list()
  for (j in 1:length(lev5)) {
    data1 <- data[[j]][[4]]
    data1 <- melt(data1)
    data1 <- na.omit(data1)
    colnames(data1)[3] <- "p_value"
    ST4[[length(ST4)+1]] <- data1
  }
  names(ST4) <- lev5
  
  wb <- createWorkbook()
  saveWorkbook(wb, paste0(names(ST1[i]), '.p.differences_melt.xlsx'))
  lapply(names(ST4), function(x) write.xlsx(ST4[[x]], paste0(names(ST1[i]), '.p.differences_melt.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))
}

# ST1.1 <-rbindlist(ST1, use.names=TRUE, fill=TRUE, idcol="trait")
# head(ST1.1)

#################
# predictions

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")

names(ST1)

for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST4 <- list()
  for (j in 1:length(lev5)) {
    data1 <- data[[j]][[1]]
    ST4[[length(ST4)+1]] <- data1
  }
  names(ST4) <- lev5
  
  wb <- createWorkbook()
  saveWorkbook(wb, paste0(names(ST1[i]), '.predictions.xlsx'))
  lapply(names(ST4), function(x) write.xlsx(ST4[[x]], paste0(names(ST1[i]), '.predictions.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))
}

##########
# wald
setwd("~/Documents/git/Dreger_2022/statistical_results/wald/")

lapply(names(ST3), function(x) write.xlsx(ST3[[x]], 'dreger.wald.1.xlsx', sheetName=x, append=T, row.names=T, showNA = F))

names(ST3)
class(ST3[1])
df2 <- list()
for (i in 1:length(ST3)) {
  df1 <- as.data.frame(ST3[i])
  colnames(df1) <- c("Df","Sum.of.Sq","Wald.statistic","Pr.Chisq.")
  df1 <- df1 %>% rownames_to_column("term")
  df2[[length(df2)+1]] <- df1
}
names(df2) <- names(ST3)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/wald/wald1.csv", quote = F, row.names = F)


#########


df2 <- list()
for (i in 1:length(ST3)) {
  df1 <- as.data.frame(ST4[i])
  df1 <- df1[,c(1:3)]
  colnames(df1) <- c("Component.Var","std.error","z.ratio")
  df1 <- df1 %>% rownames_to_column("Trial")
  df1 <- df1[-9,]
  df1 <- df1[c(2,4,7,3,5,6,1,8),]
  df1$Trial <- gsub("loc:env4:gen!loc_","", df1$Trial)
  df1$Trial <- gsub("loc:env4:gen!env4!","cut:", df1$Trial)
  df2[[length(df2)+1]] <- df1
}

names(df2) <- names(ST3)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/wald/wald2.csv", quote = F, row.names = F)

##########
# all traits

# a1 <- read.csv("pheno1.csv")
# a2 <- read.csv("all_19.csv")
# a3 <- read.csv("all_18.csv")

colnames(a1) # 18 and 19 cut 1
a1 <- a1[,-c(2:6)]
colnames(a2) # 19
a2 <- a2[,-c(1,3)]
colnames(a3) # 18
a3 <- a3[,-c(3)]
ncol(a1)
ncol(a2)
ncol(a3)

c1 <- a1 %>% dplyr::filter(ID %in% common_ID) %>% gather(key = "trait", value = "raw", 3:16)
c2 <- a2 %>% dplyr::filter(ID %in% common_ID) %>% gather(key = "trait", value = "raw", 3:20)
c3 <- a3 %>% dplyr::filter(ID %in% common_ID) %>% gather(key = "trait", value = "raw", 4:18)

c4 <- rbind(c1, c2, c3)
nrow(c1) + nrow(c2) + nrow(c3)
nrow(c4)
a5 <- inner_join(b1, c4, by = "ID")
head(a5)
a5 <- a5 %>% na_if("") %>% na.omit

######
