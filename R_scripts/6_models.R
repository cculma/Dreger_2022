
data <- ST01[[1]] # ADF
data <- ST01[[16]] # starch
data <- data %>% unite("year_cut", c(year, cut), sep = "_", remove = F)
head(data)
str(data)
lev2 <- colnames(data)[c(1:6,10,11)]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
head(data)
data$year_cut <- recode_factor(data$year_cut,
                           "2018_1"="1",
                           "2018_2"="2",
                           "2018_3"="3",
                           "2019_1"="4",
                           "2019_2"="5",
                           "2019_3"="6",
                           "2019_4"="7",
                           "2019_5"="8")
levels(data$year_cut)
# data <- data %>% dplyr::filter(loc %in% c("WA"))

summary(data$year_cut)


data1 <- na.omit(data)
head(data1)

M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ fa(env, 1):id(gen) + cut + year,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

wald(M1)
summary(M1)$varcomp
current.asrt <- as.asrtests(M1, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)

M2 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ idv(env):id(gen) + cut + year,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

wald(M2)
summary(M2)$varcomp

M3 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ corh(env):id(gen) + cut + year,
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))
wald(M3)
summary(M3)$varcomp

head(data1)
M4 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ ante(cut, 2):id(gen) + us(loc):id(gen),
                     data = data1, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

wald(M4)
summary(M4)$varcomp

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
