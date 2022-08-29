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