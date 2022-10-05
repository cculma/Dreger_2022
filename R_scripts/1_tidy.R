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

setwd("~/Documents/git/Dreger_2022/raw_data/")
load("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")
save.image("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")

b1 <- read.csv("cols_rows1.csv")
head(b1)

a1 <- read.csv("pheno1.csv")
a2 <- read.csv("all_19.csv")
a3 <- read.csv("all_18.csv")

colnames(a1) # 18 and 19 cut 1
colnames(a2) # 19
colnames(a3) # 18

a1$Cutting <- 1
colnames(a1)[7] <- "Year"

Variety_a2 <- a2[,c(2,3)]
Variety_a3 <- a3[,c(2,3)]
common_ID <- intersect(Variety_a2$ID, Variety_a3$ID)
common_ID

a2$Year <- 2019
a3$Year <- 2018

a1 <- a1[,-c(2:6,19:21)]
a1 <- a1[,c(1,14,2,3:11,13,12)]
colnames(a1)

a2 <- a2[,-c(1,3,12:15,20:22)]
a2 <- a2[,c(1,2,14,3:13)]
colnames(a2)

a3 <- a3[,-c(3,12:15)]
a3 <- a3[,c(2,3,1,4:14)]
colnames(a3)
a4 <- rbind(a1, a2, a3)

a4 <- a4 %>% dplyr::filter(ID %in% common_ID) %>% gather(key = "trait", value = "raw", 4:14)
a5 <- inner_join(b1, a4, by = "ID") 
head(a5)

# a5 <- a5 %>% unite("env", c(loc, Year, Cutting), sep = "_", remove = F) 
lev1 <- colnames(a5)[1:10]
a5[,lev1] <- lapply(a5[,lev1], factor)
str(a5)
levels(a5$trait)
summary(a5$gen)

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

a6 <- a5 %>% inner_join(., c201, by= c("loc", "block", "Cutting", "Year", "trait")) %>% inner_join(., c202, by= c("loc", "block", "Cutting", "Year", "trait")) %>% unite("env", c(loc, Year, Cutting, trait), sep = "_", remove = F) %>% inner_join(., FD2, by = "gen")
head(a6)

# FD3 <- a6 %>% inner_join(., FD2, by = "gen")
# head(FD3)
# FD3 <- split(FD3[,-11], FD3$trait)


a6 <- split(a6[,-1], a6$env)
names(a6)

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

ST01 <- ST0 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F) %>% inner_join(., FD2, by = "gen")
head(ST01)


# ST01 <- split(ST01[,-2], ST01$env)
ST01 <- split(ST01[,-6], ST01$trait)
names(ST01)

#############
lev6 <- c("FD","loc","year","cut",
          "FD:loc","FD:year","FD:cut","loc:year","loc:cut","year:cut",
          "FD:loc:year","FD:loc:cut","loc:year:cut","FD:loc:year:cut")

lev5 <- c("FD","loc","year","cut",
          "FD_loc","FD_year","FD_cut","loc_year","loc_cut","year_cut",
          "FD_loc_year","FD_loc_cut","loc_year_cut","FD_loc_year_cut")


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
for (i in 1:(length(ST01))) {
  data <- ST01[[i]]
  lev2 <- colnames(data)[c(1:5,9,10)]
  data <- as.data.frame(data)
  data[,lev2] <- lapply(data[,lev2], factor)
  data1 <- na.omit(data)
  head(data1)
# 
#   FA_1 <- asreml::asreml(fixed = predicted.value ~ 1 + gen,
#                          random = ~ fa(env, 1):id(gen) + loc + year + cut,
#                          data = data1, na.action = list(x = "include", y = "include"),
#                          weights = weight, family = asreml::asr_gaussian(dispersion = 1))
#   FA_1 <- update.asreml(FA_1)
  
  US <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                       random = ~ idv(env):id(gen) + cut + year,
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))

  
  
  W1 <- wald.asreml(US)
  ST3[[length(ST3)+1]] <- W1

  current.asrt <- as.asrtests(US, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt)
  
  ST2 <- list()
  for (j in 1:length(lev6)) {
    diffs <- predictPlus(classify = lev6[j], 
                         asreml.obj = US, 
                         wald.tab = current.asrt$wald.tab, 
                         present = c("FD","gen","loc","year","cut"))
   
    ST2[[length(ST2)+1]] <- diffs
  }
  
  names(ST2) <- lev5
  ST1[[length(ST1)+1]] <- ST2

}

length(ST1)
length(ST01)
names(ST1) <- names(ST01)
names(ST3) <- names(ST01)

data <- ST1[[1]]
data1 <- data[[1]][[1]]
data2 <- data[[1]]
data1 <- data[[1]][[4]]
names(ST1[1])
names(data[[1]])

##################
# p_differences

setwd("~/Documents/git/Dreger_2022/statistical_results/p_differences_melt/")

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

lapply(names(ST3), function(x) write.xlsx(ST3[[x]], 'dreger.wald.xlsx', sheetName=x, append=T, row.names=T, showNA = F))

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
