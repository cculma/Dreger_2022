rm(list = ls())

library(tidyverse)
library(ggcorrplot)
library(patchwork)
library(Matrix)
library(asreml)
library(asremlPlus)

setwd("~/Documents/git/Dreger_2022/raw_data/")

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

c201 <- a5 %>% dplyr::filter(gen %in% c(201))
c202 <- a5 %>% dplyr::filter(gen %in% c(202))
head(c201)

c201 <- c201[,-c(3:7)]
colnames(c201)[6] <- "cov1"
c202 <- c202[,-c(3:7)]
colnames(c202)[6] <- "cov2"

a6 <- a5 %>% left_join(., c201, by= c("loc", "block", "Cutting", "Year", "trait")) %>% left_join(., c202, by= c("loc", "block", "Cutting", "Year", "trait")) %>% unite("env", c(loc, Year, Cutting, trait), sep = "_", remove = F) 
head(a6)

a6 <- split(a6[,-1], a6$env)
names(a6)

# ST0
data <- a6[[2]]
head(data)

ST0 <- list()
for (i in 1:length(a6)) {
  data <- a6[[i]]
  # data <- data[order(data$row, data$col), ]
  m3 <- asreml::asreml(fixed = raw ~ 1 + cov1 + cov2,
                       random = ~ block + gen,
                       data = data,
                       na.action = list(x = "include", y = "include"))

  preds <- predict(m3, classify = "gen", vcov = TRUE)
  blup <- preds$pvals
  blup <- blup[1:3]
  blup$weight <- (1/blup$std.error)^2
  ST0[[length(ST0)+1]] <- blup

}

names(ST0) <- names(a6)
length(ST0)
length(a6)

ST0 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST0)
ST01 <- ST0 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)
head(ST01)
ST01 <- split(ST01[,-2], ST01$env)
ST01 <- split(ST01[,-6], ST01$trait)

#######
# ST1

data <- ST01[[16]]
str(data)
lev2 <- colnames(data)[1:5]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
str(data)
data$env <- as.factor(data$env)

ST1 <- list()
ST2 <- list()
for (i in 1:(length(ST01))) {
  data <- ST01[[i]]
  lev2 <- colnames(data)[1:5]
  data <- as.data.frame(data)
  data[,lev2] <- lapply(data[,lev2], factor)
  data1 <- na.omit(data)
  head(data1)

  FA_1 <- asreml::asreml(fixed = predicted.value ~ 1 + gen,
                         random = ~ fa(env, 1):id(gen) + loc + year + cut,
                         data = data1, na.action = list(x = "include", y = "include"),
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- update.asreml(FA_1)
  
  BLUP1 <- predict.asreml(FA_1, classify='gen:env', vcov=TRUE, )$pvals
  BLUP1 <- BLUP1[,c(1:3)]
  # BLUP1$env <- paste0("ST1:", BLUP1$env)
  # BLUP1 <- BLUP1 %>% spread(key = env, value = 3)
  
  BLUP2 <- predict.asreml(FA_1, classify='gen:loc:year', vcov=TRUE, )$pvals
  BLUP2 <- BLUP2[,c(1:4)]
  BLUP2$loc <- paste0("ST2:", BLUP2$loc)
  BLUP2 <- BLUP2 %>% unite("env", c(loc, year), sep = "_", remove = T) %>% spread(key = env, value = 3)
   
  BLUP3 <- predict.asreml(FA_1, classify='gen:loc', vcov=TRUE, )$pvals
  BLUP3 <- BLUP3[,c(1:3)]
  BLUP3$loc <- paste0("ST3:", BLUP3$loc)
  BLUP3 <- BLUP3 %>% spread(key = loc, value = 3)
  
  BLUP4 <- predict.asreml(FA_1, classify='gen', vcov=TRUE, )$pvals
  BLUP4 <- BLUP4[,c(1:2)]
  colnames(BLUP4)[2] <- "ST4"
  # colnames(BLUP4)[2] <- paste0("ST4:", names(ST01[i]))
  
  BLUP5 <- BLUP2 %>% inner_join(., BLUP3, by = "gen") %>% inner_join(., BLUP4, by = "gen")
  lev3 <- ncol(BLUP5)
  BLUP6 <- BLUP5 %>% gather(key = "env", value = "BLUP", 2:all_of(lev3))

  ST1[[length(ST1)+1]] <- BLUP1
  ST2[[length(ST2)+1]] <- BLUP6

}

length(ST1)
length(ST01)
names(ST1) <- names(ST01)
names(ST2) <- names(ST01)

ST1.1 <-rbindlist(ST1, use.names=TRUE, fill=TRUE, idcol="trait")
head(ST1.1)


#######
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

