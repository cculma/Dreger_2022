rm(list = ls())

library(interactions)
library(jtools)
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
library(emmeans)
library(ggplot2)
library(ggpubr)
library(dae)

setwd("~/Documents/git/Dreger_2022/raw_data/")

b1 <- read.csv("cols_rows1.csv")
colnames(b1)
head(b1)

b2 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/harvest_dates.csv")
b2$date <- as.Date(b2$date, format = "%m/%d/%y")
b2$date_init <- as.Date(b2$date_init, format = "%m/%d/%y")
lev1 <- c("env","loc","year")
b2[,lev1] <- lapply(b2[,lev1], factor)
head(b2)
b2 <- b2[,-c(5,12)]

b3 <- read.csv("~/Documents/git/Dreger_2022/raw_data/FD_raw2.csv") # FD class

a1 <- read.csv("pheno1.csv")
a2 <- read.csv("all_19.csv")
a3 <- read.csv("all_18.csv")
a4 <- read.csv("all_20_21.csv")
a5 <- read.csv("all_20.csv")

colnames(a1) # 18 and 19 cut 1
colnames(a2) # 19 cuts 2-5
colnames(a3) # 18 cuts 2-3
colnames(a4) # 20 and 21 cut 1
colnames(a5) # 20 cuts 2-5

a1 <- a1 %>% select(., -c("Name"))
a2 <- a2 %>% select(., -c("Variety"))

a1 <- inner_join(b1, a1, by = c("ID","block","position","loc","gen")) 
a2 <- inner_join(b1, a2, by = c("loc","ID")) 
a3 <- inner_join(b1, a3, by = c("ID")) 
a4 <- inner_join(b1, a4, by = c("block","position","loc","gen")) 
a5 <- inner_join(b1, a5, by = c("ID")) 

# to estimate RFV

a1$RFV <- ((88.9 - (.779 * a1$ADF)) * (120/a1$aNDF)) / 1.29
a2$RFV <- ((88.9 - (.779 * a2$ADF)) * (120/a2$aNDF)) / 1.29
a3$RFV <- ((88.9 - (.779 * a3$ADF)) * (120/a3$aNDF)) / 1.29
a4$RFV <- ((88.9 - (.779 * a4$ADF)) * (120/a4$aNDF)) / 1.29
a5$RFV <- ((88.9 - (.779 * a5$ADF)) * (120/a5$aNDF)) / 1.29

a1 <- a1 %>% gather(key = "trait", value = "raw", 10:25)
a2 <- a2 %>% gather(key = "trait", value = "raw", 10:28)
a3 <- a3 %>% gather(key = "trait", value = "raw", 10:25)
a4 <- a4 %>% gather(key = "trait", value = "raw", 10:30)
a5 <- a5 %>% gather(key = "trait", value = "raw", 10:28)

head(a1)
head(a2)
head(a3)
head(a4)
head(a5)

a6 <- rbind(a1, a2, a3, a4, a5)
a6$cut <- as.factor(a6$cut)
a6$cut <- ordered(a6$cut)
a6$ID <- as.factor(a6$ID)
a6$trait <- as.factor(a6$trait)
a6$gen <- as.factor(a6$gen)
levels(a6$trait)
levels(a6$gen)
a6 <- a6 %>% dplyr::filter(cut == "1") %>% dplyr::filter(!year == "2018") %>% dplyr::filter(!trait %in% c("DM","NDFD30","NDFD48","TTNDFD"))

a6$gen <- as.factor(a6$gen)

a6 <- droplevels(a6)



a7 <- a6 %>% dplyr::filter(!gen == "202")
a7 <- droplevels(a7)
levels(a7$gen)

# ID block 1,4,1,6,2
# OR block 4,8,3,4,3
# WA block 8,4,2,8,11

data2 <- a6 %>% dplyr::filter(gen %in% c(201))
data2 <- data2 %>% dplyr::select(block, raw, cut, year, trait, loc)
colnames(data2)[2] <- "cov1"
data3 <- a6 %>% dplyr::filter(gen %in% c(202))
data3 <- data3 %>% dplyr::select(block, raw, cut, year, trait, loc)
colnames(data3)[2] <- "cov2"


a7.6 <- inner_join(a6, data2, by = c("block", "cut", "year", "trait", "loc")) %>% inner_join(., data3, by = c("block", "cut", "year", "trait", "loc"))

a7.6 <- droplevels(a7.6)
levels(a7.6$cut)

a7.6$trait <- as.factor(a7.6$trait)
levels(a7.6$trait)
summary(a7.6$trait)
a7.6$trait <- recode_factor(a7.6$trait, "Su_ESC" = "Su.ESC", "Su_WSC" = "Su.WSC")

a8 <- a7.6 %>% unite("env", c(trait, loc, year, cut), sep = "_", remove = F)

a8 <- split(a8, a8$env)
a8 <- split(a8, a8$trait)
names(a8)



# ST0
names(a8)
data <- a8[[8]] # "ID_2018_ADF"

# model to analyze data grouped as ID_2018_ADF
ST01 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  
  data$year <- as.factor(data$year)
  data$year <- ordered(data$year)
  data$loc <- as.factor(data$loc)
  data$env <- as.factor(data$env)
  data <- data[order(data$env), ]
  m3 <- asreml::asreml(fixed = raw ~ cov1 + cov2 + gen * loc,
                     random = ~  year + block,
                     residual = ~ dsum(~units | env),
                     data = data,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))
  
  m3 <- update.asreml(m3)
  W1 <- wald(m3)
  ST06[[length(ST06)+1]] <- W1

  pvals1 <- predict.asreml(m3, classify="gen", vcov=TRUE, aliased = T)$pval
  pvals2 <- predict.asreml(m3, classify="gen:loc", vcov=TRUE, aliased = T)$pval
  
  pvals <- preds$pvals
  pvals <- pvals[1:4]
  pvals$weight <- (1/pvals$std.error)^2
  ST01[[length(ST01)+1]] <- pvals
}

head(ST2)
preds2 <- ST2[,c(1:3)]
colnames(preds2)[3] <- "FD"

head(ST3)
head(preds)

preds2 <- ST3[,c(1,2)]
colnames(preds2)[2] <- "FD"


preds1 <- preds[,c(1,2)]
colnames(preds1)[2] <- "ADF"

preds1 <- preds[,c(1:3)]
colnames(preds1)[3] <- "ADF"

preds3 <- inner_join(preds1, preds2, by = "gen")

preds3 <- inner_join(preds1, preds2, by = c("gen","loc"))
data1 <- ST04[[5]]
cc <- dplyr::count(data1, gen)

lm1 <- lm(predicted.value ~ loc + FD, data = data1)
summary(lm1)
anova(lm1)
ST04[[1]]

lev2 <- names(ST04)

ST05 <- list()
for (i in 1:length(ST04)) {
  plot1 <- ggplot(data = ST04[[i]], aes(x = FD, y = predicted.value, 1)) + geom_point(alpha = 0.2) +
    stat_poly_line(formula = formula, color = "black", se = T) + theme_classic(base_family = "Arial", base_size = 12) +
    stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + labs(y = lev2[[i]], x = "FD") + facet_grid(. ~ loc) 
  ggsave(filename = paste0(lev2[[i]],"_200.pdf"), plot = plot1, dpi = 300, width = 8, height = 3, device = cairo_pdf)
  
#   ST05[[length(ST05)+1]] <- plot1
}

 ST05[[1]]

P19 <- ggarrange(ST05[[1]],ST05[[2]],ST05[[3]],ST05[[4]],ST05[[5]],ST05[[6]],ST05[[7]],
                 ST05[[8]],ST05[[9]],ST05[[10]],ST05[[11]],ST05[[12]],ST05[[13]],ST05[[14]],
                 ST05[[15]],ST05[[16]],ST05[[17]],ST05[[18]],
                 ncol = 3, nrow = 6)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "FD_200.pdf", plot = P19, dpi = 300, width = 6, height = 12, device = cairo_pdf)
ggsave(filename = "FD_200.jpg", plot = P19, dpi = 300, width = 6, height = 12)



# length(ST01)
# length(a10)
# names(ST01) <- names(a10)
# ST01 <-rbindlist(ST01, use.names=TRUE, fill=TRUE, idcol="trait")
# ST01 <- ST01 %>% separate(1, c("loc", "year", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)  %>% unite("env2", c(loc, year), sep = "_", remove = F) %>% unite("env3", c(year, cut), sep = "_", remove = F) %>% inner_join(., b3, by = "gen")
# ST01 <- inner_join(ST01, b2, by = c("env","loc","year"))
# head(ST01)


ST02 <- list()
for (i in 1:length(a8)) {
  data <- a8[[i]]
  data$check <- recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,c("row","col","gen","check","block")] <- lapply(data[,c("row","col","gen","check","block")], factor)
  data <- data[order(data$row, data$col), ] # order data by row then col
  
  # m1 <- asreml::asreml(fixed = raw ~ 1 + at(check, "control"):gen, 
  #                      random = ~ at(check, "test"):gen + block + units,
  #                      residual = ~ar1(row):ar1(col),
  #                      data = data,
  #                      na.action = list(x = "include", y = "include"))
  
  
  m1 <- asreml::asreml(fixed = raw ~ cov1 + cov2 + gen, 
                       random = ~ + block,
                       residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  
  m1 <- update.asreml(m1)
  blue <- predict.asreml(m1, classify="gen", vcov=TRUE) 
  
  pvals <- blue$pvals
  sel <- matrix(1, ncol = 1, nrow = length(pvals$predicted.value))
  sel[is.na(pvals$predicted.value), ] <- 0
  vcov <- as.matrix(blue$vcov)
  vcov <- vcov[sel == 1, sel == 1]
  pvals$weight[sel == 1] <- diag(solve(vcov))
  

  # Adding Weights
  # pvals <- preds$pvals
  # pvals <- pvals[1:3]
  # pvals$weight <- (1/pvals$std.error)^2
  ST02[[length(ST02)+1]] <- pvals
}

length(a8)
length(ST02)
names(ST02) <- names(a8)
ST02 <-rbindlist(ST02, use.names=TRUE, fill=TRUE, idcol="trait")

ST02$trait <- as.factor(ST02$trait)
levels(ST02$trait)



ST03 <- ST02 %>% separate(1, c("trait","loc","year","cut"), sep = "_", remove = T, convert = FALSE, extra = "merge")


ST03 <- ST03 %>% unite("env", c(loc, year, cut), sep = "_", remove = F)
ST03 <- ST03 %>% inner_join(., b2, by = c("loc","year","env"))

ST03 <- split(ST03, ST03$trait)

names(ST03)

# second stage ------------------------------------------------------------


lev6 <- c("FD","loc","cut",
          "FD:loc","FD:cut","FD:cut:loc",
          "FD:cons_days:loc")

lev5 <- c("FD","loc","cut",
          "FD_loc","FD_cut","FD_cut_loc",
          "FD_cons_days_loc")


# ST1
names(ST03)
data <- ST03[[8]] # starch
data <- as.data.frame(data)
data1 <- na.omit(data)
head(data1)
lev4 <- c("loc","cut","cons_days","gen")
data1[,lev4] <- lapply(data1[,lev4], factor)
hist(data1$predicted.value)
hist(data1$weight)
class(data1$cons_days)


ST04 <- list()
for (i in 1:(length(ST03))) {
  data <- ST03[[i]]
  # data1 <- na.omit(data)
  data$gen <- as.factor(data$gen)
  data$loc <- as.factor(data$loc)
  data$year <- as.factor(data$year)
  data$year <- ordered(data$year)
  data$env <- as.factor(data$env)
  data$cons_days <- as.factor(data$cons_days)
  data$cons_days <- ordered(data$cons_days)
  data <- data[order(data$env), ]
  data <- na.omit(data)
  
  # M1 <- asreml::asreml(fixed = predicted.value ~ loc,
  #                      random = ~ diag(loc):ar1(cons_days):id(gen),
  #                      data = data, na.action = list(x = "include", y = "include"),
  #                      weights = weight, family = asreml::asr_gaussian(dispersion = 1))

  M1 <- asreml::asreml(fixed = predicted.value ~ loc + gen,
                       random = ~corgh(loc):ar1(year):id(gen),
                       data = data,
                       weights = weight,
                       na.action = list(x = "include", y = "include"),
                       family = asreml::asr_gaussian(dispersion = 1))
  
  M1 <- update.asreml(M1)
  
  # W1 <- wald.asreml(M1)
  # ST3[[length(ST3)+1]] <- W1
  # 
  # random = ~ corgh(loc):ar1(year):id(gen),
  # residual = ~ dsum(~units | env),
  # W2 <- summary(M1)$varcomp
  # ST4[[length(ST4)+1]] <- W2
  
  pvals1 <- predict.asreml(M1, classify="gen", vcov=TRUE, aliased = T)$pval
  pvals1$loc <- "all"
  pvals2 <- predict.asreml(M1, classify="gen:loc", vcov=TRUE, aliased = T)$pval
  diffs <- rbind(pvals1, pvals2)
  diffs <- diffs[,c(1,2,5)]
  diffs <- inner_join(diffs, ST4, by = c("gen","loc"))
  
  # current.asrt <- as.asrtests(M1, NULL, NULL)
  # current.asrt <- rmboundary.asrtests(current.asrt)
  # ST2 <- list()
  # for (j in 1:length(lev6)) {
  #   diffs <- predictPlus(classify = lev6[j], 
  #                        asreml.obj = M1, 
  #                        wald.tab = current.asrt$wald.tab, 
  #                        present = c("FD","gen","loc","cons_days","cut"))
  #   
    
  ST04[[length(ST04)+1]] <- diffs
}

length(ST04)
length(ST03)
names(ST04) <- names(ST03)

#########
# var comp

df2 <- list()
for (i in 1:length(ST4)) {
  df1 <- as.data.frame(ST4[i])
  df1 <- df1[-5,c(1:3)]
  colnames(df1) <- c("Component.Var","std.error","z.ratio")
  df1 <- df1 %>% rownames_to_column("loc")
  df1$loc <- gsub("loc:cons_days:gen!loc_","", df1$loc)
  df1$loc <- gsub("loc:cons_days:gen!cons_days!", "", df1$loc)
  df2[[length(df2)+1]] <- df1
}

names(df2) <- names(ST3)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/wald/var_comp.csv", quote = F, row.names = F)

# Wald table

df2 <- list()
for (i in 1:length(ST3)) {
  df1 <- as.data.frame(ST3[i])
  colnames(df1) <- c("Df","Sum.of.Sq","Wald.statistic","Pr.Chisq.")
  df1 <- df1 %>% rownames_to_column("factor")
  df2[[length(df2)+1]] <- df1
}
names(df2) <- names(ST3)
df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/wald/wald1.csv", quote = F, row.names = F)


#################
# predictions

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")

names(ST1)

for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST5 <- list()
  for (j in 1:length(lev6)) {
    data1 <- data[[j]][[1]]
    ST5[[length(ST5)+1]] <- data1
  }
  names(ST5) <- lev5
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% inner_join(., b2, by = c("cons_days", "loc")) %>% dplyr::select(c(1:8,11,14,16))
  
  wb <- createWorkbook()
  saveWorkbook(wb, paste0(names(ST1[i]), '.predictions.xlsx'))
  lapply(names(ST5), function(x) write.xlsx(ST5[[x]], paste0(names(ST1[i]), '.predictions.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))
}


# plot tendencies ---------------------------------------------------------


lev1 <- names(ST1)

b2$cons_days <- as.factor(b2$cons_days)

for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST5 <- list()
  for (j in 1:length(lev6)) {
    data1 <- data[[j]][[1]]
    ST5[[length(ST5)+1]] <- data1
  }
  names(ST5) <- lev5
  
  data1 <- inner_join(data1, b2, by = c("cons_days","loc"))
  str(data1)
  data1$cons_days <- as.numeric(as.character(data1$cons_days))
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% inner_join(., b2, by = c("cons_days", "loc")) %>% dplyr::select(c(1:8,11,14,16))
  
  ST5[["FD_cons_days_loc"]]$harv_days3 <- as.numeric(as.character(ST5[["FD_cons_days_loc"]]$harv_days3))
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% dplyr::filter(!year == "2021")

  str(data[[7]][["predictions"]])
  data1 <- data[[7]][["predictions"]]
    
  # plot1 <- ggplot(data = ST5[["FD_loc"]], aes(x=loc, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic() + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(title = paste0(lev1[i], " two-stage"), y = "BLUPs", x = "Loc", color = "FD")
  # 
  # plot2 <- ggplot(data = ST5[["FD_cut_loc"]], aes(x=cut, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic() + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ .) + labs(title = paste0(lev1[i], " two-stage"), y = "BLUPs", x = "Cuts", color = "FD")
  # 
  # plot3 <- ggplot(data = ST5[["FD_cons_days_loc"]], aes(x=harv_days3, y=predicted.value, color = FD)) + labs(col="loc") + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + scale_x_continuous(limits = c(120, 300), breaks = seq(120, 300, by = 10)) + facet_grid(loc ~ year) + labs(title = paste0(lev1[i], " two-stage"), y = "BLUPs", x = "Days", color = "FD")
  
  setwd("~/Documents/git/Norberg_2020/figs/second-stage1/")
  ggsave(filename = paste0("1_", lev1[i], ".jpg"), plot = plot1, width = 6, height = 6)
  ggsave(filename = paste0("2_", lev1[i], ".jpg"), plot = plot2, width = 8, height = 6)
  ggsave(filename = paste0("3_", lev1[i], ".jpg"), plot = plot3, width = 10, height = 6)
  
}

# plot join
setwd("~/Documents/git/Norberg_2020/figs/second-stage_join/")
lev1 <- names(ST1)

for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST5 <- list()
  for (j in 1:length(lev6)) {
    data1 <- data[[j]][[1]]
    ST5[[length(ST5)+1]] <- data1
  }
  names(ST5) <- lev5
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% inner_join(., b2, by = c("cons_days", "loc")) %>% dplyr::select(c(1:8,11,14,16))
  
  ST5[["FD_cons_days_loc"]]$harv_days3 <- as.numeric(as.character(ST5[["FD_cons_days_loc"]]$harv_days3))
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% dplyr::filter(!year == "2021")
  
  plot1 <- ggplot(data = ST5[["FD_loc"]], aes(x=loc, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.key.size = unit(0.3, 'cm')) + labs(y = "", color = "FD")
  
  plot2 <- ggplot(data = ST5[["FD_cut"]], aes(x=cut, y=predicted.value, group=FD)) + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") + labs(y = "")
  
  plot3 <- ggplot(data = ST5[["FD_cut_loc"]], aes(x=cut, y=predicted.value, group=FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") + facet_grid(loc ~ .) + labs(y = "")
  
  plot4 <- ggplot(data = ST5[["FD_cons_days_loc"]], aes(x=harv_days3, y=predicted.value, color = FD)) + labs(col="loc") + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(color=FD), size = 1) + geom_line(aes(color=FD), linetype = "dashed") + geom_linerange(aes(color=FD, ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position = "none") + scale_x_continuous(limits = c(120, 300), breaks = seq(120, 300, by = 10)) + facet_grid(loc ~ year) + labs(y = "", x = "Days")
  
  p1 <- ggarrange(plot1, plot2, ncol = 1, nrow = 2, align = "v")
  
  p2 <- ggarrange(p1, plot3, ncol = 2, nrow = 1, align = "h")
  
  p3 <- ggarrange(p2, plot4, ncol = 1, nrow = 2, align = "h")
  ggsave(filename = paste0("join_", lev1[i], ".jpg"), plot = p3, width = 8, height = 8)
  
}

head(ST5[["FD_cons_days_loc"]])

##################
# p_differences
names(ST1)
data <- ST1[[5]]
names(data)
data[["FD_cut_loc"]]

class(ST1[[1]][[1]])
setwd("~/Documents/git/Dreger_2022/statistical_results/p_differences_melt1/")
for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST5 <- list()
  for (j in 1:length(lev5)) {
    data1 <- data[[j]][[4]]
    data1 <- data[["FD_cut_loc"]][[4]]
    data1 <- reshape2::melt(data1)
    data1 <- na.omit(data1)
    colnames(data1)[3] <- "p_value"
    ST5[[length(ST5)+1]] <- data1
  }
  names(ST5) <- lev5
  
  wb <- createWorkbook()
  saveWorkbook(wb, paste0(names(ST1[i]), '.p.differences_melt.xlsx'))
  lapply(names(ST5), function(x) write.xlsx(ST5[[x]], paste0(names(ST1[i]), '.p.differences_melt.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))
}

# ST1.1 <-rbindlist(ST1, use.names=TRUE, fill=TRUE, idcol="trait")
# head(ST1.1)

class(ST5)
data1 <- data1 %>% separate(2, c("FD2", "cut2"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD1", "cut1"), sep = ",", remove = T, convert = FALSE, extra = "merge")

data[["FD_cut_loc"]]
data1 <- data1 %>% separate(2, c("FD2", "cut2", "loc2"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD1", "cut1", "loc1"), sep = ",", remove = T, convert = FALSE, extra = "merge")


data1 <- data1 %>% dplyr::filter(cut1 == "5") %>% dplyr::filter(cut2 == "5")

data1 <- data1 %>% dplyr::filter(cut1 == "4") %>% dplyr::filter(cut2 == "4") %>% dplyr::filter(loc1 == "ID") %>% dplyr::filter(loc2 == "ID")

data1 <- data[["FD_cut_loc"]][["predictions"]] %>% dplyr::filter(loc == "ID") %>% dplyr::filter(cut == "4")

##########

# wald
setwd("~/Documents/git/Dreger_2022/statistical_results/wald/")

lapply(names(ST3), function(x) write.xlsx(ST3[[x]], 'dreger.wald.1.xlsx', sheetName=x, append=T, row.names=T, showNA = F))

names(ST3)
class(ST3[1])
df2 <- list()
for (i in 1:length(ST3)) {
  df1 <- as.data.frame(ST3[i])
  colnames(df1) <- c("Df","Sum.of.Sq","Wald.statistic","Pr.Chisq.")
  df1 <- df1 %>% rownames_to_column("term")
  df2[[length(df2)+1]] <- df1
}
names(df2) <- names(ST3)

df2 <-rbindlist(df2, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(df2, "~/Documents/git/Dreger_2022/statistical_results/wald/wald1.csv", quote = F, row.names = F)


# plot lev1:3 -------------------------------------------------------------

lev3 <- c("FD","loc","cut")
ST5 <- list()

lev6
lev5

names(data)
for (i in 1:length(ST1)) {
  data <- ST1[[1]]
  data1 <- data[[6]][[4]] # p.dif
  data2 <- data[[6]][[1]] # blup
  
  data2 <- data2 %>% dplyr::select(1:4) %>% dplyr::filter(cut == "5" & loc == "WA")
  let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
  let2 <- as.data.frame(print(let1)) %>% rownames_to_column(lev3[j])
  let2 <- let2[,c(1,2)]
  colnames(let2) <- c(lev3[j], "Letters")
  let2 <- let2 %>% separate(1, into = c("FD","cut","loc"), remove = T)
  data2$predicted.value <- round(data2$predicted.value, 2)
  
  blup <- inner_join(data2, let2, by = c("FD","cut","loc"))
  blup <- blup %>% unite("BLUP", 4:5, sep = " ", remove = T)
  blup <- blup[,-3]
  df_total <- rbind(df_total,blup)
  
}


for (i in 1:length(ST1)) {
  data <- ST1[[1]]
  df_total = data.frame()
  for (j in 1:length(lev3[1:3])) {
    data1 <- data[[j]][[4]] # p.dif
    data2 <- data[[j]][[1]] # blup
    
    let1 <- multcompLetters(data1, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    let2 <- as.data.frame(print(let1)) %>% rownames_to_column(lev3[j])
    let2 <- let2[,c(1,2)]
    colnames(let2) <- c(lev3[j], "Letters")
    blup <- inner_join(data2, let2, by = lev3[j])
    blup$class2 <- lev3[j]
    colnames(blup)[1] <- "class1"
    df_total <- rbind(df_total,blup)
  }
  ST5[[length(ST5)+1]] <- df_total
}  
names(ST5) <- names(ST1)



for (i in 1:length(ST5)) {
  data1 <- ST5[[i]]
  
  head(data1)
  p1 <- ggplot(data1, aes(x=class1, y=predicted.value)) + geom_bar(stat="identity", width=0.6) + theme_classic(base_family = "Arial", base_size = 12) + geom_linerange(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + labs(y = "", x = "") + geom_text(aes(label = Letters, y = upper.Confidence.limit), vjust = -0.5) + facet_grid(.~ class2, scales = "free_x", space = "free_x", switch = "x")
  
  setwd("~/Documents/git/Norberg_2020/figs/lev1_3")
  ggsave(filename = paste0("1_", lev1[i], ".jpg"), plot = p1, width = 6, height = 4)
}


########
# pearson cor
lev1 <- names(ST1)

P00 <- list()
for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST5 <- list()
  for (j in 1:length(lev6)) {
    data1 <- data[[j]][[1]]
    ST5[[length(ST5)+1]] <- data1
  }
  names(ST5) <- lev5
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% inner_join(., b2, by = c("cons_days", "loc")) %>% dplyr::select(c(1:8,11,14,16))
  
  ST5[["FD_cons_days_loc"]]$harv_days3 <- as.numeric(as.character(ST5[["FD_cons_days_loc"]]$harv_days3))
  
  ST5[["FD_cons_days_loc"]] <- ST5[["FD_cons_days_loc"]] %>% dplyr::filter(!year == "2021")
  head(ST5[["FD_cut_loc"]])
  ST5[["FD_cut_loc"]] <- ST5[["FD_cut_loc"]][,c(1:4)]
  
  P00[[length(P00)+1]] <- ST5[["FD_cut_loc"]]
}    

names(P00) <- names(ST1)
P00 <-rbindlist(P00, use.names=TRUE, fill=TRUE, idcol="trait")
P00 <- P00 %>% unite("env", c(FD, cut, loc), sep = "_", remove = T)
P00 <- P00 %>% spread(key = trait, value = predicted.value)
P00 <- P00 %>% column_to_rownames("env")
P00 <- t(P00)
P00 <- cor(P00, use = "complete.obs")
