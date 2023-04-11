
b2 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/harvest_dates.csv")

head(b2)
b2$cons_days <- as.factor(b2$cons_days)
# b2 <- b2 %>% unite("harv_loc", c(9,3), sep = ",", remove = F)

list3 <- b2$harv_loc

ST9 <- list()

for (i in 1:length(ST1)) {
#  data <- ST1[[1]]
  data <- ST1[[i]]
  data2 <- data[[7]][[1]] # blup
  data2$predicted.value <- round(data2$predicted.value, 2)
  data2 <- inner_join(data2, b2, by = c("cons_days","loc"))
  data2 <- data2 %>% dplyr::select(c("FD","predicted.value","env"))
  data2 <- data2 %>% unite("FD_env", c(1,3), sep = "_", remove = T)
  ST9[[length(ST9)+1]] <- data2
  
}

names(ST9) <- names(ST1)
ST9 <- rbindlist(ST9, use.names=TRUE, fill=TRUE, idcol="trait")
ST9 <- ST9 %>% spread(key = "FD_env", value = "predicted.value") %>% column_to_rownames("trait")
ST9 <- t(ST9)
head(ST9)
P00 <- cor(ST9, use = "complete.obs")
P00 <- round(P00, 2)
P00[upper.tri(P00)] <- NA
setwd("~/Documents/git/Dreger_2022/statistical_results/cor/")
write.csv(P00, 'cor1.csv', row.names = T, quote = F)


# FQ 200 gen --------------------------------------------------------------

setwd("~/Documents/git/Dreger_2022/raw_data/")

b1 <- read.csv("cols_rows1.csv")
head(b1)

b4 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/Fall_Dormancy_CI.csv")

b4 <- b4 %>% dplyr::filter(env == "ID" | env == "OR" | env == "WA" ) %>% dplyr::select("gen","env","predicted.value")
colnames(b4) <- c("gen","loc","FD")
b4[,c("gen","loc")] <- lapply(b4[,c("gen","loc")], factor)

b4 <- b4 %>% dplyr::filter(env == "all") %>% dplyr::select("gen","predicted.value")
colnames(b4)[2] <- "FD"
b4$gen <- as.factor(b4$gen)


# b4$FD <- cut(b4$FD, breaks=c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6), labels= c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5", "5.5", "6"))
# b4$FD <- cut(b4$FD, breaks=c(0, 1.5, 2, 3, 4, 5, 6), labels= c("1", "2", "3", "4", "5", "6"))
# b4$FD <- round(b4$FD, 0)
# b4$FD <- as.factor(b4$FD)
# summary(b4$FD)

# b4 <- ST3
# b4 <- b4 %>% dplyr::select("gen", "FD", "FD_fc")
# b4$gen <- as.factor(b4$gen)
# hist(b4$FD)
# str(b4)
# summary(b4$FD_fc)


a1 <- read.csv("pheno1.csv")
a1 <- a1 %>% select(., -c("Name","DM"))
a4 <- read.csv("all_20_21.csv")
a4 <- a4 %>% select(., -c("DM"))

# to estimate RFV
a1$RFV <- ((88.9 - (.779 * a1$ADF)) * (120/a1$aNDF)) / 1.29
a4$RFV <- ((88.9 - (.779 * a4$ADF)) * (120/a4$aNDF)) / 1.29

colnames(a1) # 18 and 19 cut 1
colnames(a4) # 20 and 21 cut 1
colnames(b1)
a1 <- inner_join(a1, b1, by = c("ID","block","position","loc","gen")) 
a4 <- inner_join(a4, b1, by = c("block","position","loc","gen")) 

colnames(a1)
colnames(a4)

a1 <- a1 %>% gather(key = "trait", value = "raw", 8:22)
a4 <- a4 %>% gather(key = "trait", value = "raw", 7:26)

a6 <- rbind(a1, a4)
colnames(a6)[1:10]
a6[,colnames(a6)[1:10]] <- lapply(a6[,colnames(a6)[1:10]], factor)
a8 <- a6 %>% unite("env2", c(loc, year, cut, trait), sep = "_", remove = F)

a8 <- split(a8, a8$env2)
names(a8)
length(a8)

FQ0 <- list()
for (i in 1:length(a8)) {
  # data <- a8[[1]]
  data <- a8[[i]]
  data <- data[order(data$row, data$col), ] 
  m1 <- asreml::asreml(fixed = raw ~ gen, 
                       random = ~ + block,
                       residual = ~id(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = raw ~ gen, 
                       random = ~ + block,
                       residual = ~sar(row):sar(col),
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = raw ~ gen, 
                       random = ~ + block,
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  i1 <- infoCriteria.asreml(m1)
  i2 <- infoCriteria.asreml(m2)
  i3 <- infoCriteria.asreml(m3)
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE),
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE),
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE))))
  
  pvals <- blue$pvals
  sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value), ] <- 0
  vcov <- as.matrix(blue$vcov)
  vcov <- vcov[sel == 1, sel == 1]
  pvals$weight[sel == 1] <- diag(solve(vcov))
  
  colnames(pvals) <- c("gen", "BLUE", "std.error", "status","weight")
  pvals$weight1 <- (1/pvals$std.error)^2
  FQ0[[length(FQ0)+1]] <- pvals
  
}
length(FQ0)
length(a8)
names(FQ0) <- names(a8)

FQ1 <-rbindlist(FQ0, use.names=TRUE, fill=TRUE, idcol="env")
head(FQ1)

head(b2)
str(b2)
b2[,c("env","loc","year")] <- lapply(b2[,c("env","loc","year")], factor)
str(FQ2)

FQ2 <- FQ1 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F) %>% select(., -c("cut"))
FQ2[,c("env","loc","year","trait","gen")] <- lapply(FQ2[,c("env","loc","year","trait","gen")], factor) 
FQ2 <- inner_join(FQ2, b2, by = c("env","loc","year")) %>% inner_join(., b4, by = "gen")
FQ2 <- droplevels(FQ2)

FQ3 <- split(FQ2, FQ2$trait)


# FQ ST2 ------------------------------------------------------------------

data <- FQ3[[1]]
str(data)
data <- droplevels(data)
data1 <- na.omit(data)
head(data1)

M1 <- asreml::asreml(fixed = BLUE ~ loc,
                     random = ~corgh(loc):ar1(year):id(gen),
                     data = data1,
                     weights = weight,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))


M1 <- asreml::asreml(fixed = BLUE ~ FD + loc + gen,
                     random = ~corgh(loc):ar1(year):id(gen),
                     data = data1,
                     weights = weight,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))


M1 <- update.asreml(M1)
wald.asreml(M1)
current.asrt <- as.asrtests(M1, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt, update = F)

diffs <- predictPlus(classify = "gen:loc:year",
                     asreml.obj = M1,
                     wald.tab = current.asrt$wald.tab,
                     present = c("gen","loc","year"))

diffs <- predictPlus(classify = "gen:loc",
                     asreml.obj = M1,
                     wald.tab = current.asrt$wald.tab,
                     present = c("gen","loc","year"))

pred1 <- diffs[["predictions"]]

pred1 <- as.data.frame(pred1)

pred1 <- pred1 %>% inner_join(., b4, by = c("gen","loc"))

pred1 <- pred1 %>% inner_join(., b4, by = "gen")

str(pred1)


anova(lm1)

summary(lm1)
summary(lm1)$r.squared 
plot(lm1)

plot(predicted.value ~ FD, data = pred1)
abline(lm1)

ggplot(data = pred1, aes(x = FD, y = predicted.value)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")



ST2 <- list()
for (j in 1:length(lev6)) {
  diffs <- predictPlus(classify = "gen:loc:year",
                       asreml.obj = M1,
                       wald.tab = current.asrt$wald.tab,
                       present = c("gen","loc","year"))

  ST2[[length(ST2)+1]] <- diffs
}


head(diffs[["predictions"]])

ggplot(data = diffs[["predictions"]], aes(x=year, y=predicted.value, group=FD)) + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "")

ggplot(data = diffs[["predictions"]], aes(x=year, y=predicted.value, group=FD))  + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD), alpha = 0.6)  + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "") + facet_grid(loc ~ .) 


# + geom_linerange(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), alpha = 0.6)




# compare cor between FD and values reported
head(S_FD4)
S_FD5 <- inner_join(S_FD4, b4, by = "gen")
head(S_FD5)
S_FD5$FD_fc <- as.numeric(as.character(S_FD5$FD_fc))

ggplot(data = S_FD5, aes(x=FD.x, y=FD.y)) + theme_classic(base_family = "Arial", base_size = 12) + geom_point(size = 1, alpha = 0.6) 

hist(S_FD5$FD.y)
ggplot(data = S_FD5, aes(x=FD.x, y=FD_fc)) + theme_classic(base_family = "Arial", base_size = 12) + geom_point(size = 1, alpha = 0.6) 
cor(S_FD5$FD.x, S_FD5$FD.y)
cor(S_FD5$FD.x, S_FD5$FD_fc)



summary(lm1)
plot(lm1)

summary(lm2)
plot(lm1)

plot1 <- ggplot(data = S_FD5, aes(x=FD.x, y=FD.y)) + geom_point(size = 1, alpha = 0.6) + geom_smooth(method="lm", col="black") + stat_regline_equation(label.x = 3, label.y = 6) + theme_classic(base_family = "Arial", base_size = 12) + labs(title = "FD predicted vs reported", x = "FD reported", y = "FD BLUPs") + scale_y_continuous(breaks = seq(0:6)) + scale_x_continuous(breaks = seq(0:6)) 

# + scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) + scale_x_continuous(breaks = seq(0:6)) 

# + scale_y_continuous(breaks = seq(0:6)) + scale_x_continuous(breaks = seq(0:6)) 

setwd("~/Documents/git/Norberg_2020/figs/second-stage1/")
ggsave(filename = "lm_FD.jpg", plot = plot1, width = 4, height = 4)
