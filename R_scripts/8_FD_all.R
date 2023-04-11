# stagewise
rm(list = ls())
library(StageWise)
library(asreml)
library(data.table)
library(tidyverse)
library(asremlPlus)
library(ggplot2)
library(ggrepel)
library(goeveg)
library(ggpubr)

S_FD4 <- read.csv("~/Documents/git/big_files/FD_scores.csv")
head(S_FD4)
S_FD4$gen <- as.factor(S_FD4$gen)

b2 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/harvest_dates.csv")
head(b2)
colnames(b2)
str(b2)
b2$date <- as.Date(b2$date, format = "%m/%d/%y")
b2$date_init <- as.Date(b2$date_init, format = "%m/%d/%y")
lev1 <- c("env","loc","year")
b2[,lev1] <- lapply(b2[,lev1], factor)
head(b2)
b2 <- b2[,-c(5,12)]

b3 <- read.csv("~/Documents/git/Dreger_2022/raw_data/FD_raw2.csv") # FD class
b3$gen <- as.factor(b3$gen)
b3 <- b3[-3,]

lev1 <- c("block", "gen", "row", "col")

setwd("~/Documents/git/Norberg_2020/BLUE_values/split_data/")
data_ar <- list.files(pattern = ".csv", full.names = T)
data_ar5 <- data_ar[c(7,21,26,31)] # 5_FD
data_ar6 <- data_ar[c(10,14,18)] # OR Height to FD



# FD_OR -------------------------------------------------------------------

FD_OR <- list()
FD_lm1 <- list()

for (i in 1:length(data_ar6)) {
  data <- read.csv(data_ar6[i])
  data <- data[,c(3,6,7,8,13,18,23)]
  head(data)
  
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  
  data1 <- data %>% dplyr::select(1:5) %>% na.omit() %>% dplyr::filter(gen %in% b3$gen) %>% group_by(gen) %>% summarise_at(vars(resp), list(name = mean)) %>% inner_join(., b3, by = "gen")
  
  FD_lm1[[length(FD_lm1)+1]] <- data1
  
  data2 <- lm(FD ~ name, data = data1)
  
  summary(data2)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  data$resp[data$resp < 0.1] <- 0.1 # replace values negative values to 0.1
  data$cov1 <- data2$coefficients[2] * data$cov1 + data2$coefficients[1]
  data$cov2 <- data2$coefficients[2] * data$cov2 + data2$coefficients[1]
  
  FD_OR[[length(FD_OR)+1]] <- data
}
names(FD_OR) <- gsub(".csv", "", gsub("./", "", data_ar6))
names(FD_lm1) <- gsub(".csv", "", gsub("./", "", data_ar6))

# FD_WA and ID -------------------------------------------------------------------

FD_WA <- list()
FD_lm2 <- list()

for (i in 1:length(data_ar5)) {
  data <- read.csv(data_ar5[i])
  data <- data[,c(3,6,7,8,15,20,25)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data[,lev1] <- lapply(data[,lev1], factor)
  
  data1 <- data %>% dplyr::select(1:6) %>% na.omit() %>% dplyr::filter(gen %in% b3$gen) %>% group_by(gen) %>% summarise_at(vars(resp), list(name = mean)) %>% inner_join(., b3, by = "gen")
  
  FD_lm2[[length(FD_lm2)+1]] <- data1
  
  data2 <- lm(FD ~ name, data = data1)
  data$resp <- data2$coefficients[2] * data$resp + data2$coefficients[1]
  data$resp[data$resp < 0.1] <- 0.1 # replace values negative values to 0.1
  data$cov1 <- data2$coefficients[2] * data$cov1 + data2$coefficients[1]
  data$cov2 <- data2$coefficients[2] * data$cov2 + data2$coefficients[1]

  FD_WA[[length(FD_WA)+1]] <- data
}
names(FD_WA) <- gsub(".csv", "", gsub("./", "", data_ar5))
names(FD_lm2) <- gsub(".csv", "", gsub("./", "", data_ar5))

FD_lm3 <- c(FD_lm1, FD_lm2)

# merge_FD ----------------------------------------------------------------

FD1 <- c(FD_OR, FD_WA)
FD2 <- list()
for (i in 1:length(FD1)) {
  data <- FD1[[i]]
  data <- data %>% group_by(gen) %>% summarise_at(vars(resp), list(name = mean))
  FD2[[length(FD2)+1]] <- data
}
names(FD2) <- names(FD1) 
FD2 <-rbindlist(FD1, use.names=TRUE, fill=TRUE, idcol="env")
colnames(FD2)[6] <- "FDR"

FD2 <- FD2 %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
write.csv(FD2, "~/Documents/git/Norberg_2020/BLUE_values/FDR_raw.csv", quote = F)
head(FD2)
FD2 <- inner_join(FD2, b2, by = c("env","loc","year"))



# P00 <- cor(FD2, use = "complete.obs")
# write.csv(P00, "~/Documents/git/Norberg_2020/BLUE_values/pearson_raw.csv", quote = F)

# fall dormancy classes (FDC) 1 to 11are designated.
# fall dormancy rating (FDR) based on the regression

# stats raw ---------------------------------------------------------------

FD7 <- FD2 %>% group_by(env) %>% summarise_at(vars(FDR), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
FD7 <- FD7 %>% column_to_rownames("env") %>% round(2)
FD7$range <- str_c(FD7$max,"-", FD7$min) 
FD7 <- FD7 %>% mutate(sd = paste0("(", sd, ")"))
FD7$mean_sd <- str_c(FD7$mean," ", FD7$sd) 

FD7 <- FD7 %>% dplyr::select(5:7) %>% rownames_to_column("env")
write.csv(FD7, "~/Documents/git/Norberg_2020/BLUE_values/stats_raw.csv", quote = F)

# ST0 ---------------------------------------------------------------------


FD3 <- list()
for (i in 1:length(FD1)) {
  data <- FD1[[i]]
  data <- data[order(data$row, data$col), ] # order data by row then col
  data$check <- recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  
  m1 <- asreml::asreml(fixed = resp ~ cov1 + gen, 
                       random = ~ + block,
                     residual = ~id(row):ar1v(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ cov1 + gen, 
                       random = ~ + block,
                     residual = ~sar(row):sar(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ cov1 + gen, 
                       random = ~ + block,
                     residual = ~ar1(row):ar1(col), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

  i1 <- infoCriteria.asreml(m1)
  i2 <- infoCriteria.asreml(m2)
  i3 <- infoCriteria.asreml(m3)
  
  # plot(m1)
  # plot(m2)
  # plot(m3)
# 
#   i1$model <- "ar1_id"
#   i2$model <- "sar_id"
#   i3$model <- "ar1_ar1"
# 
#   data3 <- rbind(i1, i2, i3)
#   M_He_FD[[length(M_He_FD)+1]] = data3
  
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
  
  # vcov <- as.matrix(blue$vcov)
  # vcov[is.na(vcov)] = 1
  # pvals$weight <- diag(solve(vcov))
  
  colnames(pvals) <- c("gen", "BLUE", "std.error", "status","weight")
  pvals$weight1 <- (1/pvals$std.error)^2
  FD3[[length(FD3)+1]] <- pvals

}

names(FD3) <- names(FD1)
FD3 <-rbindlist(FD3, use.names=TRUE, fill=TRUE, idcol="env")
head(FD3)

# weight is the diag(solve(vcov)) Damesa 2017
# weight1 is the inverse of std.error


FD4 <- FD3 %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE)
write.csv(FD4, "~/Documents/git/Norberg_2020/BLUE_values/FDR_ST0.csv", quote = F, row.names = F)

FD4 <- FD3 %>% dplyr::select(1:3) %>% spread(key = env, value = BLUE) %>% column_to_rownames("gen")
P00 <- cor(FD4, use = "complete.obs")
# write.csv(P00, "~/Documents/git/Norberg_2020/BLUE_values/pearson_st0.csv", quote = F)

summary(m3)
# ST1 ---------------------------------------------------------------------

FD5 <- FD3 %>% separate(1, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")
FD5 <- as.data.frame(FD5)
lev2 <- c("env","loc","year","cut","gen")
FD5[,lev2] <- lapply(FD5[,lev2], factor)
str(FD5)
FD5 <- inner_join(FD5, b2, by = c("env","loc","year"))

levels(FD5$year)
FD5$year <- ordered(FD5$year)
summary(FD5$year)
levels(FD5$gen)
levels(FD5$loc)
summary(FD5$loc)
head(FD5)

data1 <- na.omit(FD5)
head(data1)
data1$harv_days3 <- as.factor(data1$harv_days3)
data1$harv_days3 <- ordered(data1$harv_days3)
summary(data1$harv_days3)

lm1 <- lm(BLUE ~ gen * loc + gen*year, data = data1)
anova(lm1)

M1 <- asreml::asreml(fixed = BLUE ~ loc + gen,
                     random = ~corgh(loc):ar1(year):id(gen),
                     data = data1,
                     weights = weight,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))

M1 <- update.asreml(M1)
plot(M1)
wald.asreml(M1)
summary(M1)$varcomp
summary(M1)$aic
current.asrt <- as.asrtests(M1, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt, update = F)

# gen:env -----------------------------------------------------------------

diffs1 <- predictPlus(classify = "gen:env", 
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("gen","loc","harv_days3","env"))

ST1 <- diffs1[[1]]

# gen:loc -----------------------------------------------------------------

diffs2 <- predictPlus(classify = "gen:loc", 
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("gen","loc","year"))

ST2 <- diffs2[[1]]

# gen ---------------------------------------------------------------------

diffs3 <- predictPlus(classify = "gen", 
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("gen","loc","year"))

ST3 <- diffs3[[1]]
hist(ST3$predicted.value)
ST3$FD_fc <- round(ST3$predicted.value, 0)
ST3$FD_fc <- as.factor(ST3$FD_fc)
colnames(ST3)[2] <- "FD"
summary(ST3$FD_fc)

diffs3 <- predictPlus(classify = "loc", 
                      asreml.obj = M1, 
                      wald.tab = current.asrt$wald.tab, 
                      present = c("gen","loc","year"))

# data frame all ----------------------------------------------------------

colnames(ST1)[2] <- "loc"

ST3$loc <- "all"

ST4 <- rbind(ST2, ST3)
ST4 <- ST4[,c(1:3)]
colnames(ST4)[3] <- "FD"

ST4 <- rbind(ST1, ST2, ST3)

write.csv(ST4, "~/Documents/git/Norberg_2020/BLUE_values/FD_CI.csv", row.names = F)

ST5 <- ST4 %>% select(1:3) %>% spread(key = loc, value = predicted.value)
write.csv(ST5, "~/Documents/git/Norberg_2020/BLUE_values/FDR_ST1.csv", row.names = F) 

b4 <- inner_join(ST5, b3, by = "gen")

ST6 <- ST5 %>% column_to_rownames("gen")
P00 <- cor(ST6, use = "complete")
write.csv(P00, "~/Documents/git/Norberg_2020/BLUE_values/pearson_ST1.csv", quote = F)

# summarize ---------------------------------------------------------------

str(ST4)
FD6 <- ST4 %>% group_by(loc) %>% summarise_at(vars(predicted.value), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)

FD6 <- FD6 %>% column_to_rownames("loc") %>% round(2)
FD6$range <- str_c(FD6$max,"-", FD6$min) 
FD6 <- FD6 %>% mutate(sd = paste0("(", sd, ")"))
FD6$mean_sd <- str_c(FD6$mean," ", FD6$sd) 

FD6 <- FD6 %>% dplyr::select(5:7) %>% rownames_to_column("env")
FD6 <- FD6[,c(1,4,3,2)]

write.csv(FD6, "~/Documents/git/Norberg_2020/BLUE_values/stats_ST1.csv", quote = F, row.names = F)

FD7 <- summary(M1)$varcomp
FD7 <- FD7 %>% rownames_to_column("env") %>% dplyr::filter(bound == "P")
FD7$env <- gsub('env:gen!env_', '', FD7$env)
FD7 <- FD7[-1,-c(5,6)]

write.csv(FD7, "~/Documents/git/Norberg_2020/BLUE_values/var_comp.csv", quote = F, row.names = F)


# env                   component  std.error    z.ratio       bound %ch
# env:gen!env_ID_2019_4 0.45592464 0.07001864   6.5114748     P     0
# The ratio of VA (component) to its standard error (std.error)  is known as z.ratio. If z.ratio is considerably larger than 2 (i.e. the parameter estimate is more than 2 SEs from zero), this looks likely to be significant. 
head(FD7)

plot_A <- ggplot(FD7, aes(x = reorder(env, component), y = component, group=1)) + geom_point(size = 0.5) + geom_errorbar(aes(ymin = component-std.error, ymax = component+std.error), width=.2) + theme_bw(base_family = "Arial", base_size = 14) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "Genetic variance", x = "Enviroment") + geom_line(linetype = "dashed") 

setwd("~/Documents/git/Norberg_2020/figs/")
ggsave(filename = "variance.jpg", plot = plot_A, width = 10, height = 6)


# --------------------------------


ST4 <- ST3 %>% spread(key = loc, value = predicted.value)
BLUE_FD4 <- inner_join(ST4, S_FD4, by = "gen") %>% column_to_rownames("gen")
P00 <- cor(BLUE_FD4, use = "complete.obs")

write.csv(P00, "~/Documents/git/Norberg_2020/BLUE_values/pearson_FD_loc.csv", quote = F)

BLUE_FD4 <- inner_join(ST3, ST2, by = "gen")
write.csv(BLUE_FD4, "~/Documents/git/Norberg_2020/BLUE_values/FD_ST3.csv", quote = F, row.names = F)


ggplot(BLUE_FD4, aes(x=FD, y=WA_2020_5)) + geom_point() + labs(x = "FD reported", y = "FD in WA_2020")


# ~~~~~~~~~~~~~~~~~~
# plot CI


plot_B <- ggplot(ST4, aes(x = reorder(gen, -predicted.value), y = predicted.value)) + geom_point(size = 0.5) + geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + labs(title = "FD by loc with CI", x = "gen", y = "FD") + facet_wrap(. ~ loc, ncol = 4)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "FD_CI.jpg", plot = plot_B, width = 12, height = 5)

# ~~~~~~~~~~~~~~~~~~
ST5 <- ST4 %>% dplyr::filter(gen %in% c(b3$gen))
ST5 <- inner_join(ST4, b3, by = "gen")
head(ST5)
ST5 <- ST5 %>% relocate(FD, .after = gen) %>% unite("gen1", 1:2, remove = FALSE)

plot_C <- ggplot(ST5, aes(x = reorder(gen1, -predicted.value), y = predicted.value)) + geom_point(size = 0.5) + geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit)) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(title = "FD check varieties by loc with CI", x = "gen", y = "FD") + facet_wrap(. ~ loc, ncol = 6)

setwd("~/Documents/git/Norberg_2020/figs/")
ggsave(filename = "FD1_CI.jpg", plot = plot_C, width = 12, height = 5)

head(ST1)

ST5 <- inner_join(ST1, b3, by = "gen")
head(ST5)
ST5 <- ST5 %>% relocate(FD, .after = gen) %>% unite("gen1", 1:2, remove = FALSE)
ST5$gen1 <- as.factor(ST5$gen1)
levels(ST5$gen1)
ST5 <- ST5 %>% mutate(gen1 = fct_relevel(gen1, 
                            "61_6", "104_5", "44_4", 
                            "202_3", "144_3", "201_2", 
                            "112_1"))

plot_D <- ggplot(ST5, aes(x = loc, y = predicted.value, group =gen1)) + geom_point(aes(color=gen1), size = 1) + theme_bw(base_family = "Arial", base_size = 14) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "FDR", x = "Enviroment") + geom_line(aes(color=gen1), linetype = "dashed") + scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1))

setwd("~/Documents/git/Norberg_2020/figs/")
ggsave(filename = "FD2_CI.jpg", plot = plot_D, width = 8, height = 5)

# ~~~~~~~~~~~~~~~~~~

