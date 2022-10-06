library(hrbrthemes)
library(viridis)

names(ST01)
data <- ST01[[1]] # ADF
data <- ST01[[13]] # ligini
data <- data[,c(1:5,9,10,6:8)]
data <- data %>% unite("loc_year", c(loc, year), sep = "_", remove = F)
head(data)
str(data)
levels(data$env)
lev2 <- colnames(data)[c(1:7)]

# lev2 <- colnames(data)[c(1:6,10,11)]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
head(data)
levels(data$env)

data$cut1 <- recode_factor(data$env,
                           "ID_2018_1"="3","ID_2018_2"="4","ID_2018_3"="5",
                           "ID_2019_1"="1","ID_2019_2"="2","ID_2019_3"="3","ID_2019_4"="4",
                           "OR_2018_1"="3","OR_2018_2"="4","OR_2018_3"="5",
                           "OR_2019_1"="1","OR_2019_2"="2","OR_2019_3"="3","OR_2019_4"="4",
                           "WA_2018_1"="3","WA_2018_2"="4","WA_2018_3"="5",
                           "WA_2019_1"="1","WA_2019_2"="2","WA_2019_3"="3","WA_2019_4"="4","WA_2019_5"="5")

# data$year_cut <- recode_factor(data$year_cut,
#                            "2018_1"="1",
#                            "2018_2"="2",
#                            "2018_3"="3",
#                            "2019_1"="4",
#                            "2019_2"="5",
#                            "2019_3"="6",
#                            "2019_4"="7",
#                            "2019_5"="8")

# data$month <- recode_factor(data$env,
#                             "ID_2018_1"="2018_jul","ID_2018_2"="2018_aug","ID_2018_3"="2018_sep",
#                             "ID_2019_1"="2019_may","ID_2019_2"="2019_jul",
#                             "ID_2019_3"="2019_aug","ID_2019_4"="2019_sep",
#                             "OR_2018_1"="2018_jul","OR_2018_2"="2018_aug","OR_2018_3"="2018_sep",
#                             "OR_2019_1"="2019_may","OR_2019_2"="2019_jul",
#                             "OR_2019_3"="2019_aug","OR_2019_4"="2019_sep",
#                             "WA_2018_1"="2018_jul","WA_2018_2"="2018_aug","WA_2018_3"="2018_sep",
#                             "WA_2019_1"="2019_may","WA_2019_2"="2019_jun",
#                             "WA_2019_3"="2019_jul","WA_2019_4"="2019_aug","WA_2019_5"="2019_sep")

data$month <- recode_factor(data$env,
                            "ID_2018_1"="2018_jul","ID_2018_2"="2018_aug","ID_2018_3"="2018_sep",
                            "ID_2019_1"="2019_may","ID_2019_2"="2019_jun",
                            "ID_2019_3"="2019_jul","ID_2019_4"="2019_aug",
                            "OR_2018_1"="2018_jul","OR_2018_2"="2018_aug","OR_2018_3"="2018_sep",
                            "OR_2019_1"="2019_may","OR_2019_2"="2019_jun",
                            "OR_2019_3"="2019_jul","OR_2019_4"="2019_aug",
                            "WA_2018_1"="2018_jul","WA_2018_2"="2018_aug","WA_2018_3"="2018_sep",
                            "WA_2019_1"="2019_may","WA_2019_2"="2019_jun",
                            "WA_2019_3"="2019_jul","WA_2019_4"="2019_aug","WA_2019_5"="2019_sep")


# data$cons <- recode_factor(data$env,
#                            "ID_2018_1"="1","ID_2018_2"="2","ID_2018_3"="3",
#                            "ID_2019_1"="4","ID_2019_2"="5","ID_2019_3"="6","ID_2019_4"="7",
#                            "OR_2018_1"="1","OR_2018_2"="2","OR_2018_3"="3",
#                            "OR_2019_1"="4","OR_2019_2"="5","OR_2019_3"="6","OR_2019_4"="7",
#                            "WA_2018_1"="1","WA_2018_2"="2","WA_2018_3"="3",
#                            "WA_2019_1"="4","WA_2019_2"="5","WA_2019_3"="6","WA_2019_4"="7","WA_2019_5"="8")


levels(data$month)
levels(data$cut1)
# data$month <- fct_relevel(data$month, "2019_jun", after = 4)
data$cut1 <- fct_relevel(data$cut1, c("1", "2", "3", "4", "5"))


# data$cons <- as.factor(data$cons)
# levels(data$cons)

data1 <- na.omit(data)
head(data1)
str(data1)

M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ fa(loc_year, 1):id(gen) + month,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

wald(M1)
summary(M1)$varcomp
current.asrt <- as.asrtests(M1, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)

diffs <- predictPlus(classify = "FD:month", 
                     asreml.obj = M1, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("FD","gen","loc","month"))


M2 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ diag(loc):ar1(year):ar1(cut1):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))


wald(M2)
summary(M2)$varcomp
current.asrt <- as.asrtests(M2, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)

diffs <- predictPlus(classify = "FD:cut1:loc", 
                     asreml.obj = M2, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("FD","gen","loc","year", "cut1"))


M3 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ diag(loc):ar1(month):id(gen) + year + cut1,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))
wald(M3)
summary(M3)$varcomp
current.asrt <- as.asrtests(M3, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)
diffs <- predictPlus(classify = "FD:loc:month", 
                     asreml.obj = M3, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("FD","gen","loc","year", "cut1", "month"))

levels(data1$month)
levels(data1$year)
levels(data1$cut1)

M4 <- asreml::asreml(fixed = predicted.value ~ FD * loc * cut1,
                     random = ~ fa(loc, 1):ar1(month):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))


infoCriteria.asreml(M2)
infoCriteria.asreml(M3)
infoCriteria.asreml(M4)

wald(M4)
summary(M4)$varcomp
current.asrt <- as.asrtests(M4, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)
diffs <- predictPlus(classify = "month:FD:loc", 
                     asreml.obj = M4, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("FD","gen","loc","cut1", "month"))


#~~~~~~~~~~~~~~~~~~~~~


M5 <- asreml::asreml(fixed = predicted.value ~ 1,
                     random = ~ diag(env):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

M6 <- asreml::asreml(fixed = predicted.value ~ 1,
                     random = ~ rr(env, 1):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))





levels(data1$env)

m3 <- asreml::asreml(fixed = raw ~ gen + block,
                     data = data,
                     na.action = list(x = "include", y = "include"))

m3 <- asreml::asreml(fixed = raw ~ 1,
                     random = ~ gen + block,
                     data = data,
                     na.action = list(x = "include", y = "include"))

summary(m3)
wald(m3)
diag(16)



library(ggplot2)
names(diffs)
df1 <- as.data.frame(diffs[[4]])
df <- as.data.frame(diffs[[1]])
levels(df$month)
# levels(df$cut1)
# df$cut1 <- fct_relevel(df$cut1, c("1", "2", "3", "4", "5"))
head(df)
str(df)
df <- df %>% separate(1, c("year", "month1"), sep = "_", remove = F, convert = FALSE, extra = "merge")


ggplot(df, aes(x=month, y=predicted.value, group=FD, color=FD)) + 
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6)+
  geom_errorbar(aes(ymin=predicted.value - standard.error,ymax=predicted.value+standard.error), width = 0.1, alpha = 0.6) + theme_bw(base_family = "Arial", base_size = 14) + facet_grid(loc ~ year, scales = "free_x", space = "free") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "Predicted Values Lignin", x = "")


ggplot(df, aes(x=cut1, y=predicted.value, group=FD, color=FD)) + 
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6)+
  geom_errorbar(aes(ymin=predicted.value - standard.error,ymax=predicted.value+standard.error), width = 0.1, alpha = 0.6) + theme_minimal() + facet_grid(. ~ loc, scales = "free", space = "free") 


