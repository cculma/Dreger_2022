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
