library(stats)
library(metan)


data <- ST03[[1]]

data <- data %>% dplyr::filter(loc == "OR")
data1 <- anova_joint(.data = data, env = cut, gen = FD, rep = year, resp = predicted.value)

get_model_data(data1, "details")

data1 <- anova_ind(.data =  data, env = cut, gen = FD, rep = year, resp = predicted.value)
get_model_data(data1)
get_model_data(data1, "predicted.value")

colnames(data)
data1 <- data[,c(3,6,14)]

data3 <- data1 %>% dplyr::filter(gen %in% c(201))
data4 <- data1 %>% dplyr::filter(!gen %in% c(201))
t2 <- data.frame(gen = "201", t1 = mean(data3$raw))

b1.1 <- data1 %>% dplyr::filter(gen %in% c(201))
colnames(b1.1)[3] <- "cv"
b1.1 <- b1.1[,c(2,3)]

data2 <- inner_join(data4, b1.1, by = "block")
str(data2)
data2$block <- as.factor(data2$block)
data2$m_201 <- mean(data3$raw)
data2$t1 <- (data2$m_201*(data2$raw/data2$cv))
data2 <- data2 %>% dplyr::select(gen, t1)
data2 <- rbind(data2, t2)

names(a8)
ST0 <- list()
for (i in 1:length(a8)) {
  
  data <- a8[[i]]
  data1 <- data[,c(3,6,14)]
  data2 <- data1 %>% dplyr::filter(gen %in% c(201))
  m1 <- mean(data2$raw)
  t2 <- data.frame(gen = "201", predicted.value = m1)
  colnames(data2)[3] <- "cv"
  data2 <- data2[,c(2,3)]
  
  data3 <- data1 %>% dplyr::filter(!gen %in% c(201))
  data4 <- inner_join(data3, data2, by = "block")
  data4$predicted.value <- (m1*(data4$raw/data4$cv))
  data4 <- data4 %>% dplyr::select(gen, predicted.value)
  data4 <- rbind(data4, t2)
  
  ST0[[length(ST0)+1]] <- data4
}

length(ST0)
length(a8)
names(ST0) <- names(a8)
ST01 <-rbindlist(ST0, use.names=TRUE, fill=TRUE, idcol="trait")

ST01 <- ST01 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)  %>% unite("env2", c(loc, year), sep = "_", remove = F) %>% unite("env3", c(year, cut), sep = "_", remove = F) %>% inner_join(., b3, by = "gen")

names(ST03)
data <- ST03[[8]]

data$loc <- as.factor(data$loc)
data$FD <- as.factor(data$FD)
data$cut <- as.factor(data$cut)
data$year <- as.factor(data$year)
data$FD <- ordered(data$FD)
data$cut <- ordered(data$cut)
data$env <- as.factor(data$env)
data <- data[order(data$FD, data$env), ]

dplyr::count(data, env)

m3 <- asreml::asreml(fixed = predicted.value ~ FD * loc * cut,
                     random = ~  year ,
                     residual = ~ dsum(~ env | FD),
                     data = data,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))


m3 <- asreml::asreml(fixed = raw ~ cv + FD * loc * cut,
                     random = ~ year,
                     residual = ~ ar1(loc):ar1(year),
                     data = data,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))


m3 <- update.asreml(m3)
preds <- predict.asreml(m3, classify="FD", vcov=TRUE, aliased = T)$pvals 


data2 <- aggregate(raw ~ gen , FUN = mean, data = data)
data2[6,2]
data3 <- data %>% group_by(block) %>% summarise_at(vars(resp), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)




data <- a8[[1]]

data <- ST03[[2]]
data <- data %>% dplyr::filter(!env %in% c("ID_2018_3","ID_2020_2","ID_2020_4")) 

M1 <- asreml::asreml(fixed = predicted.value ~ FD * loc,
                     random = ~ us(loc):ar1(cut):id(FD),
                     data = data, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))


data <- data %>% dplyr::select(env, FD, predicted.value) %>% spread(key = env, value = predicted.value)

data <- data %>% dplyr::select(-c(ID_2018_3,ID_2020_2,ID_2020_4))

data <- data %>% column_to_rownames("FD")
data <- t(data)

cor(data)

# colnames(data)[9] <- "predicted.value"
data$FD <- as.factor(data$FD)
data <- data %>% dplyr::filter(!year == "2018") %>% dplyr::filter(!cut == "5")


ggplot(data = data, aes(x=FD, y=predicted.value, color = cut)) + geom_point(size = 1, alpha = 0.6) + geom_line(linewidth = 0.5, alpha = 0.6) + facet_wrap(~loc) 

ggplot(data = data, aes(x=FD, y=predicted.value, color = cut)) + geom_boxplot() + facet_wrap(~loc) 

cc <- dplyr::count(data, FD, cut, loc)
cc <- dplyr::count(data, year, cut, loc)


data %>% dplyr::filter(FD == "1") %>% dplyr::filter(cut == "2") %>% dplyr::filter(loc == "ID")


# geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) 


data <- data %>% dplyr::filter(!cut == "5")
data <- data %>% dplyr::filter(year %in% c("2019","2020")) %>% dplyr::filter(!cut == "5")
class(data)
data$check <- recode_factor(data$gen, "201" = "control", .default = "test")

data$block <- as.factor(data$block)
data$FD <- ordered(data$FD)
data$cut <- ordered(data$cut)
data$year <- as.factor(data$year)
data$year <- ordered(data$year)

data$loc <- as.factor(data$loc)
data$FD <- as.factor(data$FD)



# m3 <- asreml::asreml(fixed = raw ~ FD,
#                      random = ~ block,
#                      data = data,
#                      na.action = list(x = "include", y = "include"), 
#                      family = asreml::asr_gaussian(dispersion = 1))


m3 <- asreml::asreml(fixed = raw ~ cv + FD + loc,
                     random = ~ block + year + cut:year,
                     data = data,
                     na.action = list(x = "include", y = "include"),
                     family = asreml::asr_gaussian(dispersion = 1))

summary(m3)$varcomp
summary(M1)$aic


m3 <- update.asreml(m3)
wald(m3)
# 
# m3 <- asreml::asreml(fixed = raw ~ at(check, "control"):gen,
#                      random = ~ at(check, "test"):gen ,
#                      data = data,
#                      na.action = list(x = "include", y = "include"))


# m3 <- asreml::asreml(fixed = raw ~ FD * loc + cut,
#                      random = ~ us(loc):ar1(cut):id(FD),
#                      data = data, na.action = list(x = "include", y = "include"),
#                      family = asreml::asr_gaussian(dispersion = 1))




wald.asreml(m3)

m3 <- update.asreml(m3)

current.asrt <- as.asrtests(m3, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt, update = F)


diffs <- predictPlus(classify = "cut", 
                     asreml.obj = m3, 
                     wald.tab = wald(m3), 
                     present = c("FD","loc","cut"))

preds <- predict.asreml(m3, classify="cut", vcov=T, aliased = T)$pvals
preds <- predict.asreml(m3, classify="FD", vcov=TRUE, aliased = T)$pvals
preds <- predict.asreml(m3, classify="loc", vcov=TRUE, aliased = T)$pvals

pvals <- preds$pvals

names(a8)
ST05 <- list()
for (i in 1:length(a8)) {
  data <- a8[[5]]
  data <- na.omit(data)
  
  data$loc <- as.factor(data$loc)
  data$FD <- as.factor(data$FD)
  data$cut <- as.factor(data$cut)
  data$year <- as.factor(data$year)
  data$FD <- ordered(data$FD)
  data$cut <- ordered(data$cut)
  head(data)
  
  lm1 <- lm(raw ~ cv + FD * loc * cut, data)
  
  anova(lm1)
  emmeans(lm1, "cut")
  emmeans(lm1, "FD")
  
  
}


lm1 <- lm(predicted.value ~ FD * loc * cut, data)
# lm1 <- lm(raw ~ cv + FD * loc * cut, data)
summary(lm1)
anova(lm1)
emmeans(lm1, "cut")
emmeans(lm1, "FD")
lm2 <- emmeans(lm1, pairwise ~ FD | cut)
lm3 <- as.data.frame(lm2$emmeans)
lm2 <- emmeans(lm1, "FD")
lm2 <- emmeans(lm1, ~ cut)

contrast(lm2)


pvals <- ST05[[5]][[5]][[1]]
str(pvals)
pvals$cut <- as.numfac(pvals$cut)

fit1 <- lm(predicted.value ~cut, data=pvals)
fit2 <- lm(predicted.value~poly(cut,2,raw=TRUE), data=pvals)
fit3 <- lm(predicted.value~poly(cut,3,raw=TRUE), data=pvals)
fit4 <- lm(predicted.value~poly(cut,4,raw=TRUE), data=pvals)
fit5 <- lm(predicted.value~poly(cut,5,raw=TRUE), data=pvals)

plot(pvals$cut, pvals$predicted.value, pch=19, xlab='x', ylab='y')

xaxis <- seq(1, 4, length=4)
lines(xaxis, predict(fit1, data.frame(x=xaxis)), col='green')
lines(xaxis, predict(fit2, data.frame(x=xaxis)), col='red')
lines(xaxis, predict(fit3, data.frame(x=xaxis)), col='blue')
lines(xaxis, predict(fit4, data.frame(x=xaxis)), col='pink')
lines(xaxis, predict(fit5, data.frame(x=xaxis)), col='yellow')


summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared



pvals <- ST05[[1]][[5]]
head(pvals)
pvals$cut <- as.numfac(pvals$cut)
pvals$predicted.value


ggplot(data = pvals, aes(x = cut, y = predicted.value)) +
  geom_point() +
  geom_smooth(method = "lm",  formula = y ~ x + I(x^2), se = F)


formula <- y ~ x + I(x^2)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_fit_deviations(formula = formula, colour = "red") +
  stat_poly_line(formula = formula) +
  stat_poly_eq(use_label(c("eq", "adj.R2")), formula = formula)

ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(use_label(c("eq", "adj.R2")), formula = formula)

ggplot(data = pvals, aes(x = cut, y = predicted.value)) +
  geom_point() +
  
  stat_poly_line(formula = formula, se = F, fm.values = T) +
  stat_poly_eq(use_label(c("eq", "adj.R2")), formula = formula) + stat_fit_tb(method = "lm",
            method.args = list(formula = formula),
            tb.type = "fit.anova",
            tb.vars = c(Effect = "term", 
                        "df",
                        "M.S." = "meansq", 
                        "italic(F)" = "statistic", 
                        "italic(P)" = "p.value"),
            tb.params = c(x = 1, "x^2" = 2),
            label.y.npc = "top", label.x.npc = "left",
            size = 2.5,
            parse = TRUE)

# stat_poly_eq(use_label(c("eq", "adj.R2", "p.value.label")), formula = formula)