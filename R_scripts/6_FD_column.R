# add FD column
library(tidyverse)


Variety_a2 <- a2[,c(2,3)]
Variety_a3 <- a3[,c(2,3)]
common_ID <- intersect(Variety_a2$ID, Variety_a3$ID)
common_ID
ID1 <- as.data.frame(common_ID)
colnames(ID1) <- "ID"

Variety_a2$ID <- as.factor(Variety_a2$ID)
Variety_a3$ID <- as.factor(Variety_a3$ID)
ID1$ID <- as.factor(ID1$ID)

str(Variety_a2)
str(Variety_a3)
str(ID1)


Variety_a2 <- Variety_a2 %>% distinct(ID, .keep_all = T)
Variety_a3 <- Variety_a3 %>% distinct(ID, .keep_all = T)

FD1 <- ID1 %>% inner_join(., Variety_a3, by = "ID") %>% inner_join(., Variety_a2, by = "ID")
FD1 <- FD1[,-2]

write.csv(FD1, "~/Documents/git/Dreger_2022/raw_data/FD_raw1.csv", quote = F, row.names = F)
# manual editing
#~~~~~~~~~~~~~~~END

#Start here

FD2 <- read.csv("~/Documents/git/Dreger_2022/raw_data/FD_raw1.csv")
FD2 <- inner_join(b1, FD2, by = "ID") %>% select(5,9,10) %>% distinct(gen, .keep_all = T)
FD2[,colnames(FD2)] <- lapply(FD2[,colnames(FD2)], factor)

# join with ST1.1: BLUP values of quality traits
FD3 <- ST1.1 %>% inner_join(., FD2, by = "gen") %>% separate(3, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
head(FD3)

FD4 <- FD3 %>% dplyr::filter(env == "OR_2018_1") %>% dplyr::filter(trait == "ADF")
str(FD4)
FD4$FD <- as.numeric(FD4$FD)
model <- lm(FD ~ predicted.value, data = FD4)
plot(model)

#create scatterplot of raw data
plot(FD4$FD, FD4$predicted.value, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(model)
