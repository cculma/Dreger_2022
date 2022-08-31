library(metan)
library(data.table)

# BASIC stats

FD4 <- split(FD3[,-1], FD3$trait)
names(FD4)

FD5 <- list()
for (i in 1:length(FD4)) {
  stat1 <- desc_stat(.data = FD4[[i]], stats = "main", hist = F)
  FD5[[length(FD5)+1]] <- stat1
}
names(FD5) <- names(FD4)
FD5 <-rbindlist(FD5, use.names=TRUE, fill=TRUE, idcol="trait")
FD5 <- FD5[,-2]

write.csv(FD5, "~/Documents/git/Dreger_2022/stats_1/desc_stats.csv", quote = F, row.names = F)

###############

# ANOVA
FD6 <- list()
for (i in 1:length(FD4)) {
  mod1 <- lm(predicted.value ~ FD + year + cut + loc + loc:cut, data = FD4[[i]])
  final <- anova(mod1)[,c(1,3,5)]
  rnames <- rownames(final)
  
  colnames(final) <- c("DF", "MS", "P-value")
  # colnames(final)[2] <- names(FD4[i])
  final <- as.data.frame(round(final, digits = 2))
  final$sign[final$`P-value` < 0.05] <- "*"
  final$sign[final$`P-value` < 0.01] <- "**"
  final$sign[final$`P-value` < 0.001] <- "***"
  final$sign[final$`P-value` > 0.05] <- "ns"
  final[[2]] <- paste(final[[2]], ifelse(is.na(final[[4]]), "", final[[4]]))
  final <- final[-c(3,4)]
  final <- final %>% rownames_to_column(var = "SOV")
  FD6[[length(FD6)+1]] <- final
}
names(FD6) <- names(FD4)
FD6.1 <- rbindlist(FD6, use.names=TRUE, fill=TRUE, idcol="trait")
FD6.1 <- FD6.1 %>% select(-3) %>% spread(key = trait, value = MS)

FD6.2 <- FD6[[1]][1:2]
FD6.3 <- FD6[[10]][1:2]
FD6.4 <- FD6[[16]][1:2]

FD6.5 <- inner_join(FD6.2, FD6.1, by = "SOV")
write.csv(FD6.5, "~/Documents/git/Dreger_2022/stats_1/AOV.csv", quote = F, row.names = F)

#~~~~~~~~~~ END

# plots ggplot

names(FD4)
head(FD4[[18]])

FD7 <- FD4[[18]] %>% dplyr::filter(year == "2019") %>% dplyr::filter(!cut == "5")
FD7 <- as.data.frame(FD7)
FD7 <- droplevels(FD7)
str(FD7)
lev4 <- c("gen","env","loc","year","cut","FD","Var1")
FD7[,lev4] <- lapply(FD7[,lev4], factor)

# coefs <- coef(lm(predicted.value ~ cut, data = dat))

ggplot(FD7, aes(x = gen, y = predicted.value, fill = loc)) +
  geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + facet_wrap(cut ~ loc) 

ggplot(FD7, aes(x = cut, y = predicted.value, fill = loc)) +
  geom_boxplot(alpha = 0.6, width=0.6, position = position_dodge(width=0.8, preserve = "single")) + theme_classic(base_size = 12)
 
ggplot(FD7, aes(x=cut, y=predicted.value, color = loc)) +  geom_point() + theme_classic(base_size = 12)
  # geom_line() + facet_wrap(gen ~ .)  

#  geom_abline(intercept = mod1[1], slope = mod1[2])
head(FD7)

#############

# Stats by gen loc
FD6 <- list()
for (i in 1:length(FD4)) {
  stat1 <- FD4[[i]] %>% dplyr::filter(year == "2019") %>% dplyr::filter(!cut == "5") %>% select(1,3,5,6) %>% unite("env", c(gen, loc), sep = "_", remove = T) %>% spread(key = env, value = predicted.value)
  stat2 <- desc_stat(.data = stat1, stats = "main", hist = F)
  FD6[[length(FD6)+1]] <- stat2
}
names(FD6) <- names(FD4)
FD6 <- rbindlist(FD6, use.names=TRUE, fill=TRUE, idcol="trait")
FD6.1 <- FD6 %>% separate(2, c("gen", "loc"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% inner_join(., FD2, by = "gen")
FD6.1 <- FD6.1[order(FD6.1$trait, FD6.1$FD), ]
colnames(FD6.1)
FD6.1 <- FD6.1[,c(1,13,2,12,3:11)]
write.csv(FD6.1, "~/Documents/git/Dreger_2022/stats_1/Stats2.csv", quote = F, row.names = F)

# Stats by env ID_1
FD7 <- list()
for (i in 1:length(FD4)) {
  stat1 <- FD4[[i]] %>% dplyr::filter(year == "2019") %>% select(1,3,5,6) %>% unite("env", c(loc, cut), sep = "_", remove = T) %>% spread(key = env, value = predicted.value)
  stat2 <- desc_stat(.data = stat1, stats = "main", hist = F)
  FD7[[length(FD7)+1]] <- stat2
}
names(FD7) <- names(FD4)
FD7 <- rbindlist(FD7, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(FD7, "~/Documents/git/Dreger_2022/stats_1/Stats3.csv", quote = F, row.names = F)

# Stats by env ID_2018_1
FD8 <- list()
for (i in 1:length(FD4)) {
  stat1 <- FD4[[i]] %>% select(1,2,6) %>% spread(key = env, value = predicted.value)
  stat2 <- desc_stat(.data = stat1, stats = "main", hist = F)
  FD8[[length(FD8)+1]] <- stat2
}
names(FD8) <- names(FD4)
FD8 <- rbindlist(FD8, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(FD8, "~/Documents/git/Dreger_2022/stats_1/Stats4.csv", quote = F, row.names = F)

# Stats by gen
FD5 <- list()
for (i in 1:length(FD4)) {
  stat1 <- FD4[[i]] %>% select(1,2,6) %>% spread(key = gen, value = predicted.value)
  stat2 <- desc_stat(.data = stat1, stats = "main", hist = F)
  FD5[[length(FD5)+1]] <- stat2
}
names(FD5) <- names(FD4)
FD5 <- rbindlist(FD5, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(FD5, "~/Documents/git/Dreger_2022/stats_1/Stats0.csv", quote = F, row.names = F)

