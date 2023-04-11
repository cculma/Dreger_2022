devtools::install_github("cardiomoon/ggiraphExtra")
require(moonBook)
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(sjPlot)

fit=lm(NTAV~age,data=radial)
summary(fit)
ggPredict(fit,se=TRUE,interactive=TRUE)

fit1=lm(NTAV~age+sex,data=radial)
summary(fit1)
ggPredict(fit1,se=TRUE,interactive=F)

fit2=lm(NTAV~age*DM,data=radial)
summary(fit2)
ggPredict(fit2,colorAsFactor = T, se = T, interactive = F)

fit4=lm(NTAV~age*weight*HBP,data=radial)
summary(fit4)
anova(fit4)
ggPredict(fit4,interactive = F)

str(pred1)
pred1$year <- as.integer(as.character(pred1$year))

# ~~~~~~~~
lm1 <- lm(predicted.value ~ FD , data = pred1)
lm1 <- lm(predicted.value ~ FD + year + loc, data = pred1)

lm1 <- lm(predicted.value ~ FD * year * loc, data = pred1)
summary(lm1)$r.squared
summary(lm1)$adj.r.squared

summary(lm1)
summary(lm1)$coefficient
plot_model(lm1, type = "pred", terms = c("FD", "loc", "year"), legend.title = NULL)
plot1 <- plot_model(lm1, type = "eff", terms = c("FD", "loc", "year"))
plot1 + theme_bw(base_family = "Arial", base_size = 14) + labs(title = "", y = lev2[[1]])
class(plot1)
?plot_model
# ~~~~~~~~

lev2 <- names(ST3)


# 
lm1 <- lm(predicted.value ~ FD * year * loc, data = pred1)

# lm1 <- lm(predicted.value ~ FD * year * loc + gen, data = pred1)

# lm1 <- lm(predicted.value ~ FD * year * loc + gen + gen:loc + gen:year, data = pred1)

# lm1 <- lm(predicted.value ~ FD * year * loc + gen * year * loc, data = pred1)
# lm1 <- lm(predicted.value ~ FD + year * loc, data = pred1)

# lm1 <- lm(predicted.value ~ FD * loc, data = pred1)


summary(lm1)
summary(lm1)$r.squared
anova(lm1)

ao1 <- as.data.frame(anova(lm1))
ao1 <- ao1 %>% rownames_to_column("term")
ao1$`Pr(>F)`[ao1$`Pr(>F)` < 0.001] <- "***"
ao1$`Pr(>F)`[ao1$`Pr(>F)` > 0.05] <- "ns"
ao1$`Pr(>F)`[ao1$`Pr(>F)` < 0.05 & ao1$`Pr(>F)` > 0.01] <- "*"
ao1$`Pr(>F)`[ao1$`Pr(>F)` < 0.01 & ao1$`Pr(>F)` > 0.001] <- "**"
ao1 <- ao1[,c(1,2,3,6)]
ao1$`Sum Sq` <- round(ao1$`Sum Sq`, 2)

write.csv(ao1, "~/Documents/git/Dreger_2022/statistical_results/Anova_FD_cofactor.csv", row.names = F, quote = F)


mod3.1 <- emmeans(lm1, pairwise ~ year | loc)

emmip(lm1, FD ~ loc, cov.reduce = range)


# ~~~~~~~~~~~~~~

ST2 <- list()
for (i in 1:length(FQ3)) {
  data <- FQ3[[i]]
  data1 <- na.omit(data)
  
  M1 <- asreml::asreml(fixed = BLUE ~ loc,
                       random = ~corgh(loc):ar1(year):id(gen),
                       data = data1,
                       weights = weight,
                       na.action = list(x = "include", y = "include"),
                       family = asreml::asr_gaussian(dispersion = 1))
  
  M1 <- update.asreml(M1)
  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt, update = F)
  diffs <- predictPlus(classify = "gen:loc:year",
                       asreml.obj = M1,
                       wald.tab = current.asrt$wald.tab,
                       present = c("gen","loc","year"))
  
  pred1 <- diffs[["predictions"]]
  pred1 <- as.data.frame(pred1)
  pred1 <- pred1 %>% inner_join(., b4, by = "gen")
  ST2[[length(ST2)+1]] <- pred1
}

length(ST2)
length(FQ3)
names(ST2) <- names(FQ3)

ST2 <-rbindlist(ST2, use.names=TRUE, fill=TRUE, idcol="env")
head(ST2)

ST3 <- split(ST2, ST2$env)

names(ST3)
ST4 <- list()
ST5 <- list()
for (i in 1:length(ST3)) {
  data1 <- ST3[[3]]
  lm1 <- lm(predicted.value ~ FD * year * loc, data = data1)
  
  ao1 <- as.data.frame(anova(lm1))
  ao1 <- ao1 %>% rownames_to_column("term")
  ao1$`Pr(>F)`[ao1$`Pr(>F)` < 0.001] <- "***"
  ao1$`Pr(>F)`[ao1$`Pr(>F)` > 0.05] <- "ns"
  ao1$`Pr(>F)`[ao1$`Pr(>F)` < 0.05 & ao1$`Pr(>F)` > 0.01] <- "*"
  ao1$`Pr(>F)`[ao1$`Pr(>F)` < 0.01 & ao1$`Pr(>F)` > 0.001] <- "**"
  ao1 <- ao1[,c(1,2,3,6)]
  ao1$`Sum Sq` <- round(ao1$`Sum Sq`, 2)
  
  ST4[[length(ST4)+1]] <- ao1
  ST5[[length(ST5)+1]] <- lm1

}
anova(lm1)

length(ST4)
length(ST3)
names(ST4) <- names(ST3)
names(ST5) <- names(ST3)

ST4 <-rbindlist(ST4, use.names=TRUE, fill=TRUE, idcol="trait")

ST4.1 <- ST4 %>% dplyr::filter(trait != "dNDF30",trait != "dNDF48",trait != "IVTDMD30",trait != "IVTDMD48",trait != "NDFD30",trait != "NDFD48", trait != "TTNDFD")

ST4.1 <- ST4.1 %>% unite("SumSq", 4:5, sep = " ", remove = T) %>% spread(key = trait, value = SumSq)

write.csv(ST4.1, "~/Documents/git/Dreger_2022/statistical_results/Anova_FD_cofactor.csv", row.names = F, quote = F)


names(ST5)
lev2[1]

rm(plot1)

for (i in 1:length(ST5)) {
  plot1 <- plot_model(ST5[[i]], type = "eff", terms = c("FD", "loc", "year"))
  plot1 <- plot1 + theme_bw(base_family = "Arial", base_size = 14) + labs(title = "", y = lev2[i])
  setwd("~/Documents/git/Norberg_2020/figs/lm_1/")
  ggsave(filename = paste0(lev2[i], ".jpg"), plot = plot1, width = 5, height = 5)
  # ggsave(filename = "1.jpg", plot = plot1, width = 5, height = 5)
}

summary(ST5[[3]])

names(ST5)
plot1 <- plot_model(ST5[[3]], type = "pred", terms = c("FD", "loc", "year"))
plot1 <- plot1 + theme_bw(base_family = "Arial", base_size = 14) + labs(title = "", y = lev2[[1]])

class(ST5[[3]])
ggPredict(ST5[[3]],interactive = F)

install.packages("predict3d")

if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/predict3d")
require(predict3d)
install.packages("rgl")
require(rgl)


names(ST5)

plot1 <- plot_model(ST5[[3]], type = "eff", terms = c("FD","year"))
plot1 <- plot_model(ST5[[3]], type = "eff", terms = c("FD","loc"))
plot1 <- plot_model(ST5[[3]], type = "eff", terms = c("FD"))
plot1 <- plot1 + theme_bw(base_family = "Arial", base_size = 14) + labs(title = "", y = lev2[[1]])

lm1 <- lm(predicted.value ~ FD  year * loc, data = data1)

lm1 <- lm(predicted.value ~ FD + year + loc + 
     FD:year + FD:loc, data = data1)


ggPredict(lm1,digits=1, show.error = TRUE, facet.modx = TRUE, show.text=FALSE)

ggPredict(lm1, show.point = FALSE,se=TRUE,xpos=0.5, facet.modx = F, facetbycol = FALSE)

ggPredict(ST5[[3]], facet.modx = F,add.modx.values = FALSE,xpos=0.5, show.point = FALSE)
ggPredict(ST5[[3]], pred=FD, modx=loc, show.point = FALSE,se=TRUE,xpos=0.5)


class(plot2)
?ggPredict
plot1 <- plot_model(ST5[[1]], type = "eff", terms = c("FD", "loc", "year"))
plot1 <- plot1 + theme_bw(base_family = "Arial", base_size = 14) + labs(title = "", y = lev2[[1]])

setwd("~/Documents/git/Norberg_2020/figs/lm_1/")
ggsave(filename = paste0(lev2[[1]], "jpg"), plot = plot1, width = 6, height = 5)
paste0(lev2[[1]], "jpg")


install.packages("visreg")
library(visreg)
visreg(ST5[[3]], "FD")
class(ST5[[3]])
visreg(ST5[[3]], "loc", gg=TRUE, ylab="predicted.value")

#load car package
library(car)

#produce added variable plots
avPlots(ST5[[3]])
