install.packages('augmentedRCBD', dependencies=TRUE)
library(augmentedRCBD)
library(agricolae)

a1 <- read.csv("~/Documents/git/Dreger_2022/raw_data/example_Scoot.csv")
colnames(a1)
lev1 <- c("Entry","Block")
lev2 <- c("Sibley","Hardin","Weber","Kato")
a1[,lev1] <- lapply(a1[,lev1], factor)
levels(a1$Entry)

# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
data <- data.frame(blk, trt, y1, y2)
# Convert block and treatment to factors
data$blk <- as.factor(data$blk)
data$trt <- as.factor(data$trt)
str(data)
# Results for variable y1 (checks inferred)

out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE)

summary(out1)
describe.augmentedRCBD(out1)
gva.augmentedRCBD(out1)



out3 <- augmentedRCBD(data$block, data$gen, data$raw, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE)

m2 <- out3$Means
m2 <- m2[,c(1,8)]
colnames(m2)[1] <- "gen"



m3 <- asreml::asreml(fixed = raw ~ gen + block,
                     data = data,
                     na.action = list(x = "include", y = "include"))

summary(m3)
wald(m3)

m5 <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals
m5 <- m5[,c(1:2)]
m6 <- inner_join(m2, m5, by = "gen")
cor(m6$`Adjusted Means`, m6$predicted.value)

# ASReml
data <- FD3[[1]]
head(data)
data <- droplevels(data)
data$env <- as.factor(data$env)
str(data)
lev2 <- colnames(data)[1:5]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
data <- na.omit(data)
head(data)


m3 <- asreml::asreml(fixed = raw ~ gen + block,
                     data = data,
                     na.action = list(x = "include", y = "include"))



DIAG <- asreml::asreml(fixed = raw ~ gen + block,
                       random = ~ diag(env):id(gen) + loc + Year + Cutting,
                       data = data, na.action = list(x = "include", y = "include"),
                       family = asreml::asr_gaussian(dispersion = 1))


US <- asreml::asreml(fixed = raw ~ gen + block,
                     random = ~ idv(env):id(gen) + loc + Year + Cutting,
                     data = data, na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))

infoCriteria.asreml(DIAG)
infoCriteria.asreml(US)

BLUP1 <- predict.asreml(US, classify='gen:loc:Year', vcov=TRUE)$pvals
current.asrt <- as.asrtests(DIAG, NULL, NULL)
diffs <- predictPlus(classify = "loc:cut", 
                     asreml.obj = US, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("gen", "loc", "year","cut"))



m2 <- asreml::asreml(fixed = raw ~ 1 + gen + cov1 + cov2, 
                     random = ~ + block + spl(row), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m3 <- asreml::asreml(fixed = raw ~ gen + block,
                     data = data,
                     na.action = list(x = "include", y = "include"))

m4 <- asreml::asreml(fixed = raw ~ gen,
                     random = ~ block ,
                     data = data,
                     na.action = list(x = "include", y = "include"))

m5 <- asreml::asreml(fixed = raw ~ 1 + gen + cov1 + cov2, 
                     random = ~ block, 
                     data = data, 
                     na.action = list(x = "include", y = "include"))

infoCriteria.asreml(m2)
infoCriteria.asreml(m3)
infoCriteria.asreml(m4)



summary(m3)
summary(m2)

# Yi ----------------------------------------------------------------------

lev1 <- c("block", "gen", "row", "col","check")

Yield_BLUE <- list()

for (i in 1:length(data_ar4)) {
  data <- read.csv(data_ar4[i])
  data <- data[,c(3,6,7,8,14,19,24)]
  colnames(data) <- c("block", "gen", "row", "col", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[,lev1] <- lapply(data[,lev1], factor)
  Yield_BLUE[[length(Yield_BLUE)+1]] = data
}  
names(Yield_BLUE) <- list_4

names(Yield_BLUE)
data <- Yield_BLUE[["ID_2018_1"]]
data <- na.omit(data)

out3 <- augmentedRCBD(data$block, data$gen, data$resp, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE)

summary(out3)
describe.augmentedRCBD(out1)
gva.augmentedRCBD(out1)

?gva.augmentedRCBD
hist(out3$Means$`Adjusted Means`)

m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                     random = ~ + block + spl(row), 
                     data = data, 
                     na.action = list(x = "include", y = "include"))






m1 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                     random = ~ block + at(check, "test"):gen, 
                     residual = ~ ~idv(units),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m2 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                     random = ~ block + at(check, "test"):gen, 
                     residual = ~ id(row):id(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                     random = ~ block + at(check, "test"):gen, 
                     residual = ~ ar1(row):id(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m4 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                     random = ~ block + at(check, "test"):gen, 
                     residual = ~ sar(row):id(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))

m5 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                     random = ~ block + at(check, "test"):gen, 
                     residual = ~ ar1(row):ar1(col),
                     data = data, 
                     na.action = list(x = "include", y = "include"))


lrt(m1, m2, boundary = F)
lrt(m2, m3, boundary = F)
lrt(m3, m4, boundary = F)
lrt(m3, m5, boundary = F)

summary(m1)$aic
summary(m2)$aic
summary(m3)$aic
summary(m4)$aic
summary(m5)$aic

m1 <- update.asreml(m1)
m2 <- update.asreml(m2)
m3 <- update.asreml(m3)
m4 <- update.asreml(m4)
m5 <- update.asreml(m5)

m1$vparameters
m2$vparameters
m3$vparameters
m4$vparameters
m5$vparameters

vpredict(m1, h2 ~ V2 / (V1 + V2 + V3))
vpredict(m2, h2 ~ V2 / (V1 + V2 + V3))
vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
vpredict(m4, h2 ~ V2 / (V1 + V2 + V3 + V4))

vpredict(m2, h2 ~ V2 / (V1 + V2 + V3 + V4 + V5))
vpredict(m2, h2 ~ V2 / (V2 + V3))
vpredict(m2, h2 ~ V2 / (V1 + V2 + V3))
vpredict(m2, h2 ~ V2 / (V1 + V2 + V3 + V4))


df1 <- as.data.frame(m5$vparameters)



# current.asrt <- as.asrtests(m2, NULL, NULL)
# current.asrt <- rmboundary.asrtests(current.asrt, update = F)



summary(m2)
summary(m2)$varcomp
m2$vparameters
?vpredict



m3$vparameters
vg <- vpredict(m3, VG ~ V2)
h2 <- vpredict(m4, h2 ~ V2 / (V1 + V2 + V3 + V4))

rbind(vg, h2)


library(inti)
library(knitr)

dt <- potato
hr <- H2cal(data = dt
            , trait = "stemdw"
            , gen.name = "geno"
            , rep.n = 5
            , fixed.model = "0 + (1|bloque) + geno"
            , random.model = "1 + (1|bloque) + (1|geno)"
            , emmeans = TRUE
            , plot_diag = TRUE
            , outliers.rm = TRUE)

hr$tabsmr %>% knitr::kable(caption = "Variance component table")
?H2cal

hr <- H2cal(data = data
            , trait = "resp"
            , gen.name = "gen"
            , rep.n = 1
            , fixed.model = "0 + (1|block) + gen"
            , random.model = "1 + (1|block) + (1|gen)"
            , emmeans = TRUE
            , plot_diag = TRUE
            , outliers.rm = TRUE)
