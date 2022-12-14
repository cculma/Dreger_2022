rm(list = ls())

library(metan)
library(data.table)
library(lme4) # LMM
library(car)
library(lmerTest) # ANOVA Table (replace it)
library(lsmeans)

# GLMM 
# Random and fixed
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
# lme model
names(FD4)
head(FD4[[1]])
str(FD4[[1]])
data2 <- FD4[[1]]
data2 <- as.data.frame(data2)
lev4 <- colnames(data2)[c(1:5,7:8)]
data2[,lev4] <- lapply(data2[,lev4], factor)
str(data2)
data2 <- droplevels(data2)

mod1 <- lmer(predicted.value ~ gen * loc * year + (1|cut) + (1|loc:year:cut), data = data2) ## Incorrect

mod1 <- lmer(predicted.value ~ FD * loc * (1|year) + (1|cut) + (1|loc:year:cut), data = data2) ##

mod2 <- lmer(predicted.value ~ FD * loc + (1|year) + (1|cut) + (1|loc:year:cut), REML=FALSE, data = data2)


mod3 <- lmer(predicted.value ~ FD + loc + FD:loc
             + (1|year) + (1|cut) + (1|year:cut) 
             + (1|FD:year) + (1|loc:year)
             + (1|FD:cut) + (1|loc:cut) 
             + (1|FD:loc:year) + (1|FD:loc:cut) + (1|loc:year:cut)
             + (1|FD:loc:year:cut), data = data2)

summary(mod3)
confint(mod3) # Computing profile confidence intervals

ls2 <- ls_means(mod3, pairwise = T)
ls3 <- ls_means(mod3, pairwise = F, ddf="Satterthwaite")
ls4 <- ls_means(mod3, pairwise = F, ddf="Kenward-Roger")


?ls_means
write.csv(ls3, "~/Documents/git/Dreger_2022/statistical_results/ls3.csv", quote = F, row.names = T)

ls_means(mod3, which = NULL, ddf="Kenward-Roger")

mod3
summary(mod3)
anova(mod3)
mod4 <- update(mod3, REML=FALSE, verbose = 1)
vc <- lme4::VarCorr(mod3)
# Variance-Covariance Matrix of fixed effects: 
vc_fixed <- as.matrix(vcov(mod3))

# Variance of fixed effects: 
var_fixed <- diag(vc_fixed)
var_fixed

se_fixed <- sqrt(var_fixed)
se_fixed


summary(mod3)
get_variance(mod3)
get_variance_fixed(mod3)
get_variance_residual(mod3)
get_residuals(mod3)
get_df(mod3)
get_deviance(mod3)
find_random(mod3)
get_random(mod3)
find_response(mod3)
is_mixed_model(mod3)
find_parameters(mod3)
# get all residuals.
# using GLMM using.
# separate the means summary of the model.


# mod4 <- lmer(predicted.value ~ FD + gen + loc + FD:loc + gen:loc + (1|year) + (1|cut)  + (1|loc/year/cut) + (1|FD:loc:year) + (1|gen:loc:year), data = data1)

mod5 <- lmer(predicted.value ~ FD * loc * year * cut
             + (1|year) + (1|cut) + (1|year:cut) 
             + (1|FD:year) + (1|loc:year)
             + (1|FD:cut) + (1|loc:cut) 
             + (1|FD:loc:year) + (1|FD:loc:cut) + (1|loc:year:cut)
             + (1|FD:loc:year:cut), data = data1)
mod4 <- update(mod5, REML=FALSE, verbose = 1)
anova(mod4)

mod6 <- lm(predicted.value ~ FD + loc + year + cut
           + FD:loc + FD:year + FD:cut 
           + loc:year + loc:cut + year:cut
           + FD:loc:year + FD:loc:cut + FD:year:cut + loc:year:cut
           + FD:loc:year:cut, data = data1)


drop1(mod3)
add1(mod5, test = "F")
anova(mod1)
anova(mod3, mod5)
anova(mod6) 
?lsmeans
lsmeans(mod6, "loc")
marginal = emmeans(mod6, ~ loc)

anova(mod5)
anova(mod3, ddf="Satterthwaite")
anova(mod3, ddf="Kenward-Roger")
anova(mod3, ddf="lme4")

ls_means(mod4)


mean(data1$predicted.value)

ls1 <- difflsmeans(mod3, which = NULL, ddf="Satterthwaite")
ls_means(mod6)

write.csv(ls1, "~/Documents/git/Dreger_2022/statistical_results/ls1.csv", quote = F, row.names = T)

(step_res <- step(mod3))
final <- get_model(step_res)
anova(final)
summary(mod3, ddf="lme4")
coef(summary(mod3))
summary(mod3)
lsmeansLT(mod3, which = NULL, ddf="Satterthwaite")

lsm <- ls_means(mod3)
plot(lsm, which=c("FD", "loc"))

show_tests(ls_means(mod3)) 

car::Anova(mod3)
anova(mod3, ddf = "lme4")

?car::Anova
?lsmeansLT
?ls_means
?lme4::pvalues
?difflsmeans
?anova
?lmerTest::anova
?anova.lmerModLmerTest
?emmeans
?drop1

library(emmeans)
emmeans(mod3, list(pairwise ~ FD), adjust = "tukey")
FD_a <- emmeans(mod3, "FD")
emmeans(mod3, pairwise ~ FD + loc)
contrast(FD_a)

anova(mod3)
lmerTest::ranova(mod3, reduce.terms = T)
ranova(mod4, reduce.terms = T)
ranova(mod5, reduce.terms = T)
coef(mod5)

final <- ranova(mod1)[,c(1,3,6)]
anova(mod1)[,c(1,3,6)]
ranova(mod1)[,c(1,3,6)]
rand(mod1)
final <- ranova(mod3)[,c(1,3,6)]

plot(typing.lsm[[2]])

mod3
summary(mod3)
summary(mod6)

anova(mod1,mod2,mod3,mod6)

# check models and find residuals
save.image("~/Documents/git/Dreger_2022/tidy_Dreger1.RData")


##############

final <- anova(mod2)[,c(1,3,6)]
rnames <- rownames(final)


# ANOVA lmer
FD6 <- list()
for (i in 1:length(FD4)) {
  # mod1 <- lmer(predicted.value ~ gen * loc *year + (1|cut) + (1|loc:year:cut), data = FD4[[i]])

  mod1 <- lmer(predicted.value ~ FD + gen + loc + FD:loc + gen:loc 
               + (1|year) + (1|cut)  + (1|year:cut) 
               + (1|FD:year) + (1|gen:year) + (1|loc:year) 
               + (1|FD:cut) + (1|gen:cut) + (1|loc:cut) 
               + (1|FD:loc:year) + (1|gen:loc:year), data = FD4[[i]])
  
  final <- anova(mod1)[,c(1,3,6)]
  rnames <- rownames(final)
  colnames(final) <- c("MS", "DF", "P-value")
  # colnames(final)[2] <- names(FD4[i])
  final <- as.data.frame(round(final, digits = 2))
  final$sign[final$`P-value` < 0.1] <- "."
  final$sign[final$`P-value` < 0.05] <- "*"
  final$sign[final$`P-value` < 0.01] <- "**"
  final$sign[final$`P-value` < 0.001] <- "***"
  final$sign[final$`P-value` > 0.1] <- "ns"
  final[[1]] <- paste(final[[1]], ifelse(is.na(final[[4]]), "", final[[4]]))
  final <- final[-c(3,4)]
  final <- final %>% rownames_to_column(var = "SOV")
  FD6[[length(FD6)+1]] <- final
}
names(FD6) <- names(FD4)

###
##############
# write in a table

FD6.1 <- rbindlist(FD6, use.names=TRUE, fill=TRUE, idcol="trait")
FD6.3 <- FD6.1 %>% select(-4) %>% spread(key = trait, value = MS)

FD6.2 <- FD6[[1]][c(1,3)]
FD6.5 <- inner_join(FD6.2, FD6.3, by = "SOV")

write.csv(FD6.5, "~/Documents/git/Dreger_2022/stats_1/AOV.1.csv", quote = F, row.names = F)

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

##############

# extras
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

