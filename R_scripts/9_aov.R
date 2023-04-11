library(stringr)

str_detect("dsadsf", "^abc", negate = TRUE)


names(ST1[[1]][[11]])
data1 <- ST1[[1]][[11]]
data1 <- ST1[[1]][[11]][[1]]


data1 <- ST02[[1]]
head(data1)
colnames(data1)[5] <- "cut"
str(data1)

# data1 <- data1 %>% separate(3, c("year", "cut"), sep = "_", remove = T, convert = FALSE, extra = "merge") 
colnames(data1)
lev4 <- colnames(data1)[1:4]
data1[,lev4] <- lapply(data1[,lev4], factor)
str(data1)
data1 <- droplevels(data1)

mod6 = lm(predicted.value ~ FD + loc + year + cut
          + FD:loc + FD:year + FD:cut 
          + loc:year + loc:cut + year:cut
          + FD:loc:year + FD:loc:cut + FD:year:cut + loc:year:cut
          + FD:loc:year:cut, data = data1)

ao1 <- as.data.frame(anova(mod6))
ao1 <- ao1 %>% rownames_to_column("term")


ST05 <- ST02[-c(15,16)]


df2 <- list()
for (i in 1:length(ST05)) {
  data1 <- ST05[[1]]
  colnames(data1)[5] <- "cut"
  data1 <- data1 %>% dplyr::filter(!year %in% c("2021"))
  
  mod6 = lm(predicted.value ~ FD + loc + year 
            + FD:loc + FD:year 
            + loc:year + 
            + FD:loc:year, data = data1)
  
  ao1 <- as.data.frame(anova(mod6))
  ao1 <- ao1 %>% rownames_to_column("term")
  
  df2[[length(df2)+1]] <- ao1
}
names(df2) <- names(ST05)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/LM_AOV_all_18_20_no_cuts.csv", quote = F, row.names = F)



df2 <- list()
for (i in 1:length(ST05)) {
  data1 <- ST05[[i]]
  colnames(data1)[5] <- "cut"
  
  mod3 <- lmer(predicted.value ~ FD + loc + FD:loc
               + (1|year) + (1|cut) + (1|year:cut) 
               + (1|FD:year) + (1|loc:year)
               + (1|FD:cut) + (1|loc:cut) 
               + (1|FD:loc:year) + (1|FD:loc:cut) + (1|loc:year:cut)
               + (1|FD:loc:year:cut), data = data1)
  
  
  
  ls3 <- ls_means(mod3, pairwise = F, ddf="Satterthwaite")
  ls3 <- ls3 %>% rownames_to_column("term")
  df2[[length(df2)+1]] <- ls3
}
names(df2) <- names(ST05)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/ls3_all.csv", quote = F, row.names = F)



# ~~~~~~~~~~~~~~~~

str(data1)
df2 <- list()
for (i in 1:length(ST05)) {
  data1 <- ST05[[i]]
  colnames(data1)[5] <- "cut"
  data1 <- data1 %>% dplyr::filter(!year %in% c("2021"))
  mod4 <- lmer(predicted.value ~ FD * loc * year
               + (1|cut), data = data1)
  
  ls3 <- ls_means(mod4, pairwise = F, ddf="Satterthwaite")
  ls3 <- ls3 %>% rownames_to_column("term")
  df2[[length(df2)+1]] <- ls3
}
names(df2) <- names(ST05)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
# lev3 <- colnames(df2)[1:2]
# df2 <- as.data.frame(df2)
# df2[,lev3] <- lapply(df2[,lev3], factor)

write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/ls3_all_18_20_no_cuts.csv", quote = F, row.names = F)

anova(mod4)

lev7 <- subset(levels(df2$term),  str_detect(levels(df2$term), "^FD", negate = TRUE))

df3 <- df2 %>% dplyr::filter(term %in% lev7)

# ~~~~~~~~~~~~~

str(data1)
df2 <- list()
for (i in 1:length(ST05)) {
  data1 <- ST05[[i]]
  colnames(data1)[5] <- "cut"
  data1 <- data1 %>% dplyr::filter(!year %in% c("2021"))
  mod4 <- lmer(predicted.value ~ FD * loc * year
               + (1|cut), data = data1)
 
  ao1 <- as.data.frame(anova(mod4))
  ao1 <- ao1 %>% rownames_to_column("term") 

  df2[[length(df2)+1]] <- ao1
}
names(df2) <- names(ST05)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")


write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/ls_lmer_all_18_20_no_cuts.csv", quote = F, row.names = F)


