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

setwd("~/Documents/git/Dreger_2022/raw_data/")
load("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")
save.image("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")

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

# colnames(a5)
# colnames(a1)
# a5 <- a5[,c(8,4,2,3,5,1,9,10:24)]
# a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a5')

a1 <- a1 %>% gather(key = "trait", value = "raw", 10:24)
a2 <- a2 %>% gather(key = "trait", value = "raw", 10:27)
a3 <- a3 %>% gather(key = "trait", value = "raw", 10:24)
a4 <- a4 %>% gather(key = "trait", value = "raw", 10:29)
a3 <- a3 %>% relocate(year, .after=Cutting)

head(a1)
head(a2)
head(a3)
head(a4)

common_ID <- unique(a2$ID)

a5 <- rbind(a1, a2, a3, a4)
a5 <- a5 %>% dplyr::filter(ID %in% common_ID)
a5 <- inner_join(FD2, a5, by = "ID")
head(a5)
colnames(a5)
# data$month <- fct_relevel(data$month, "2019_jun", after = 4)


# a5 <- a5 %>% unite("env", c(loc, Year, Cutting), sep = "_", remove = F) 
lev1 <- colnames(a5)[1:12]
a5[,lev1] <- lapply(a5[,lev1], factor)
str(a5)
levels(a5$trait)
summary(a5$gen)

a5 <- a5 %>% dplyr::filter(!gen %in% "22")
summary(a5$Var1)
a6 <- droplevels(a5)

# a6 <- split(a5[,-1], a5$env)
# names(a6)
# str(a6[[1]])
# levels(a6[[1]]$gen)

# save.image("~/Documents/git/big_files/tidy_Dreger1.RData")
# load("~/Documents/git/big_files/tidy_Dreger1.RData")

c201 <- a5 %>% dplyr::filter(gen %in% c(201))
c202 <- a5 %>% dplyr::filter(gen %in% c(202))
# head(c201)

c201 <- c201[,-c(3:7)]
colnames(c201)[6] <- "cov1"
c202 <- c202[,-c(3:7)]
colnames(c202)[6] <- "cov2"

# a6 <- a5 %>% inner_join(., c201, by= c("loc", "block", "Cutting", "Year", "trait")) 
# a6 <- a5 %>% inner_join(., c201, by= c("loc", "block", "Cutting", "Year", "trait")) %>% inner_join(., c202, by= c("loc", "block", "Cutting", "Year", "trait")) %>% unite("env", c(loc, Year, Cutting, trait), sep = "_", remove = F) %>% inner_join(., FD2, by = "gen")
head(a6)

a6 <- a6 %>% unite("env", c(loc, year, Cutting, trait), sep = "_", remove = F)
a6 <- split(a6[,-4], a6$env)
names(a6)
length(a6)


# ST0
data <- a6[[2]]
head(data)

ST0 <- list()
for (i in 1:length(a6)) {
  data <- a6[[i]]
  data <- data[order(data$row, data$col), ]
  # m3 <- asreml::asreml(fixed = raw ~ 1 + cov1 + cov2,
  #                      random = ~ block + gen,
  #                      data = data,
  #                      na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = raw ~ gen + block,
                       data = data,
                       na.action = list(x = "include", y = "include"))
  

  preds <- predict(m3, classify = "gen", vcov = TRUE)
  blup <- preds$pvals
  blup <- blup[1:3]
  blup$weight <- (1/blup$std.error)^2
  ST0[[length(ST0)+1]] <- blup

}
length(ST0)
length(a6)
names(ST0) <- names(a6)

ST0 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST0)
str(ST0)
ST01 <- ST0 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env1", c(loc, year, cut), sep = "_", remove = F)  %>% unite("env2", c(loc, year), sep = "_", remove = F) %>% unite("env3", c(year, cut), sep = "_", remove = F)%>% inner_join(., FD2, by = "gen")

ST01 <- ST01 %>% relocate(gen, .after=env3) %>% relocate(loc, .after=gen) %>% relocate(c("FD","Var1"), .after=gen)
head(ST01)
ST01 <- as.data.frame(ST01)
lev2 <- colnames(ST01)[1:10]
ST01[,lev2] <- lapply(ST01[,lev2], factor)

ST01$env4 <- recode_factor(ST01$env1,
                           "ID_2018_1"="2018_jul","ID_2018_2"="2018_aug","ID_2018_3"="2018_sep",
                           "ID_2019_1"="2019_may","ID_2019_2"="2019_jun",
                           "ID_2019_3"="2019_jul","ID_2019_4"="2019_aug","ID_2020_1"="2020_may",
                           "OR_2018_1"="2018_jul","OR_2018_2"="2018_aug","OR_2018_3"="2018_sep",
                           "OR_2019_1"="2019_may","OR_2019_2"="2019_jun",
                           "OR_2019_3"="2019_jul","OR_2019_4"="2019_aug",
                           "OR_2020_1"="2020_may","OR_2021_1"="2021_may",
                           "WA_2018_1"="2018_jul","WA_2018_2"="2018_aug","WA_2018_3"="2018_sep",
                           "WA_2019_1"="2019_may","WA_2019_2"="2019_jun",
                           "WA_2019_3"="2019_jul","WA_2019_4"="2019_aug","WA_2019_5"="2019_sep",
                           "WA_2020_1"="2020_may","WA_2021_1"="2021_may")
ST01$env5 <- recode_factor(ST01$env1,
                           "ID_2018_1"="jul","ID_2018_2"="aug","ID_2018_3"="sep",
                           "ID_2019_1"="may","ID_2019_2"="jun",
                           "ID_2019_3"="jul","ID_2019_4"="aug","ID_2020_1"="may",
                           "OR_2018_1"="jul","OR_2018_2"="aug","OR_2018_3"="sep",
                           "OR_2019_1"="may","OR_2019_2"="jun",
                           "OR_2019_3"="jul","OR_2019_4"="aug",
                           "OR_2020_1"="may","OR_2021_1"="may",
                           "WA_2018_1"="jul","WA_2018_2"="aug","WA_2018_3"="sep",
                           "WA_2019_1"="may","WA_2019_2"="jun",
                           "WA_2019_3"="jul","WA_2019_4"="aug","WA_2019_5"="sep",
                           "WA_2020_1"="may","WA_2021_1"="may")


ST01$env4  <- fct_relevel(ST01$env4, "2019_sep", after = 7)
ST01$env5 <- fct_relevel(ST01$env5, c("may", "jun", "jul", "aug", "sep"))

levels(ST01$env1)
levels(ST01$env2)
levels(ST01$env3)
levels(ST01$env4)
levels(ST01$env5)
levels(ST01$cut)
levels(ST01$FD)

ST01 <- ST01 %>% relocate(env4, .after=env3)  %>% relocate(env5, .after=env4) 

str(ST01)
summary(ST01$env1)

ST02 <- split(ST01[,-11], ST01$trait)
names(ST02)

# data <- droplevels(ST01[[18]]) # Starch
# data <- droplevels(ST01[[21]]) # TTNDFD
str(data)
levels(data$env3)

#############
lev6 <- c("FD","loc","year","env5",
          "FD:loc","FD:year","FD:env5","loc:year","loc:env5","year:env5",
          "FD:loc:year","FD:loc:env5","loc:year:env5","FD:loc:year:env5")

lev5 <- c("FD","loc","year","month",
          "FD_loc","FD_year","FD_month","loc_year","loc_month","year_month",
          "FD_loc_year","FD_loc_month","loc_year_month","FD_loc_year_month")

lev6 <- c("FD","loc","year","env4",
          "FD:loc","FD:year","FD:env4","loc:year","loc:env4",
          "FD:loc:year","FD:loc:env4")

lev5 <- c("FD","loc","year","year_month",
          "FD_loc","FD_year","FD_year_month","loc_year","loc_year_month",
          "FD_loc_year","FD_loc_year_month")

#############
# ST1

data <- ST01[[16]] # starch
str(data)
lev2 <- colnames(data)[c(1:5,9,10)]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
head(data)
# data$env <- as.factor(data$env)

ST1 <- list()
ST3 <- list()
ST4 <- list()
for (i in 1:(length(ST03))) {
  data <- ST03[[i]]
  data1 <- na.omit(data)
  head(data1)
  
  # M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
  #                      random = ~ us(loc):ar1(env3):id(gen) + year,
  #                      data = data1, na.action = list(x = "include", y = "include"),
  #                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
                       random = ~ us(loc):ar1(env4):id(gen) + year,
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



data <- ST1[[1]]
data1 <- data[[1]][[1]]
data2 <- data[[1]]
data1 <- data[[1]][[4]]
names(ST1[1])
names(data[[1]])

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
summary(US)
