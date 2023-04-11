library(ggpmisc)

# Stats
head(ST01)
ST04 <- list()
for (i in 1:(length(ST03))) {
  data_new <- ST03[[i]] %>% group_by(env) %>% summarise_at(vars(predicted.value), list(min = min, max = max, mean = mean, sd = sd, cv = cv), na.rm = TRUE)
  data_new[,c(2:6)] <- round(data_new[,c(2:6)], 2)
  
  data_new <- data_new %>% unite("range", c(min, max), sep = " - ", remove = T) %>% unite("mean", c(mean, sd), sep = " \u00b1 ", remove = T)
  data_new <- data_new %>% column_to_rownames("env")
  data_new <- t(data_new)
  data_new <- as.data.frame(data_new)
  data_new <- data_new %>% rownames_to_column("env")
  ST04[[length(ST04)+1]] <- data_new
}
names(ST04) <- names(ST03)
ST04 <-rbindlist(ST04, use.names=TRUE, fill=TRUE, idcol="trait")

setwd("~/Documents/git/Dreger_2022/BLUPs/")
write.csv(ST04, "ST04.csv", quote = F, row.names = F)


# paste0("mean","\u00b1")
# "\u00b1"
#############

lev2 <- names(FD_lm3)

FD_lm4 <- list()
for (i in 1:(length(FD_lm3))) {
  data1 <- FD_lm3[[i]]
  
  plot1 <- ggscatter(data1, x = "FD", y = "name", conf.int = T, point = T, add = "reg.line") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = max(data1$name + 3)) + stat_regline_equation(label.x = 1, label.y = max(data1$name + 1)) + labs(y = lev2[[i]], x = "FD")

  FD_lm4[[length(FD_lm4)+1]] <- plot1
  
}
names(FD_lm4) <- names(FD_lm3)

P19 <- ggarrange(FD_lm4[[4]],FD_lm4[[1]],FD_lm4[[2]],FD_lm4[[3]],FD_lm4[[5]],FD_lm4[[6]],FD_lm4[[7]], ncol = 2, nrow = 4)
P19

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "FD_lm.pdf", plot = P19, dpi = 300, width = 4, height = 9, device = cairo_pdf)
ggsave(filename = "FD_lm.jpg", plot = P19, dpi = 300, width = 4, height = 9)





plot1 <- ggplot(data = data1, aes(x = FD, y = name)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = T) + theme_classic(base_family = "Arial", base_size = 12) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_poly_eq(aes(label = paste("atop(", after_stat(p.value.label), ",", after_stat(eq.label), ")", sep = "")), formula = formula) + labs(y = lev2[[i]], x = "FD")
pvals <- FD_lm3[[1]]
ggplot(data = pvals, aes(x = FD, y = name)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = T) + theme_classic(base_family = "Arial", base_size = 12) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_poly_eq(aes(label = paste("atop(", after_stat(p.value.label), ",", after_stat(eq.label), ")", sep = "")), formula = formula)


# stat_poly_eq(use_label(c("adj.R2", "p.value.label","eq")), formula = formula)
# stat_poly_eq(aes(label = paste("atop(", after_stat(p.value.label), ",", after_stat(eq.label), ")", sep = "")), formula = formula)


?ggscatter
?stat_poly_eq
ggscatter(data1, x = "FD", y = "name", conf.int = T, point = T, add = "reg.line") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 17) + stat_regline_equation(label.x = 1, label.y = 15) + labs(y = "PH ID_2018_3", x = "FD")

#add = "reg.line"
#add = "loess"
##################

names(ST05)
str(pvals)
pvals <- ST05[[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 31) + labs(y = "ADF", x = "FD")

pvals <- ST05[[2]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P2 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 36.5) + labs(y = "aNDF", x = "FD")

pvals <- ST05[[3]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P3 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 9.8) + labs(y = "Ash", x = "FD")

pvals <- ST05[[4]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P4 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 1.5) + labs(y = "Ca", x = "FD")

pvals <- ST05[[5]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P5 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 24) + labs(y = "Cprot", x = "FD")

pvals <- ST05[[6]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P6 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 15.5) + labs(y = "dNDF30", x = "FD")

pvals <- ST05[[7]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P7 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 17.5) + labs(y = "dNDF48", x = "FD")

pvals <- ST05[[8]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P8 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 2.1) + labs(y = "Fat", x = "FD")

pvals <- ST05[[9]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P9 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 2.7) + labs(y = "K", x = "FD")

pvals <- ST05[[10]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P10 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 6) + labs(y = "Lignin", x = "FD")

pvals <- ST05[[11]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P11 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 0.38) + labs(y = "Mg", x = "FD")

pvals <- ST05[[12]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P12 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 0.35) + labs(y = "P", x = "FD")

pvals <- ST05[[13]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P13 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 192) + labs(y = "RFV", x = "FD")

pvals <- ST05[[14]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P14 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 2.9) + labs(y = "Starch", x = "FD")

pvals <- ST05[[15]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P15 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 7.2) + labs(y = "Su_ESC", x = "FD")

pvals <- ST05[[16]][[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P16 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 8.9) + labs(y = "Su_WSC", x = "FD")

P19 <- ggarrange(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16, ncol = 4, nrow = 4)
P19

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "lm1.pdf", plot = P19, dpi = 300, width = 9, height = 9, device = cairo_pdf)
ggsave(filename = "lm1.jpg", plot = P19, dpi = 300, width = 9, height = 9)


# FD cut ------------------------------------------------------------------
names(ST05)
pvals <- ST05[[8]][[3]][[1]]
pvals$FD <- as.numfac(pvals$FD)

P1 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T, facet.by = "cut") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + ylim(25, 33) + stat_cor(label.x = 1, label.y = 26) + labs(y = "ADF", x = "FD")

# ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", color = "cut", conf.int = T, point = T) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1))

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "ADF_cut.jpg", plot = P1, dpi = 300, width = 4, height = 4)

P8 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T, facet.by = "cut") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 1.8) + labs(y = "Fat", x = "FD")

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "Fat_cut.jpg", plot = P8, dpi = 300, width = 4, height = 4)

# FD loc ------------------------------------------------------------------

names(ST05)
pvals <- ST05[[1]][[2]][[1]]
pvals$FD <- as.numfac(pvals$FD)

P1 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T, facet.by = "loc") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + ylim(25, 33) + stat_cor(label.x = 1, label.y = 26) + labs(y = "ADF", x = "FD")

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "ADF_loc.jpg", plot = P1, dpi = 300, width = 6, height = 3)

pvals <- ST05[[8]][[2]][[1]]
pvals$FD <- as.numfac(pvals$FD)

P8 <- ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", conf.int = T, point = T, facet.by = "loc") + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + stat_cor(label.x = 1, label.y = 1.8) + labs(y = "Fat", x = "FD")

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "Fat_cut.jpg", plot = P8, dpi = 300, width = 4, height = 4)

pvals <- ST05[[1]][[4]][[1]]
pvals$FD <- as.numfac(pvals$FD)

ggscatter(pvals, x = "FD", y = "predicted.value", add = "reg.line", color = "loc", conf.int = T, conf.int.level = 0.95, point = T, facet.by = "cut", palette = c("#00AFBB", "#E7B800", "#FC4E07"),) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1))

?ggscatter
names(ST05)


ggscatter(
  data = data,
  x = "total.tillers", 
  y = "kernal.yield",
  xlab = "Number of tillers",
  ylab = "Kernal yield in rice",
  add = "reg.line",
  conf.int = TRUE,
  conf.int.level = 0.95,
  add.params = list(color = "black",
                    size = 1,
                    linetype = 1,
                    fill = "lightgray"),
  cor.coef = TRUE, cor.method = "pearson",
  cor.coeff.args = list(label.sep = "\n"), # Correlation and probability value
  color = "priming",                       # Custom color palettes
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  ggtheme = theme_bw()                     # Customize themes
)



# FD1  --------------------------------------------------------------------
names(ST05)

formula <- y ~ x
formula1 <- y ~ x + I(x^2)
formula2 <- y ~ x + I(x^2) + I(x^3)

pvals <- ST05[[1]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "ADF", x = "FD")

pvals <- ST05[[2]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P2 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "aNDF", x = "FD")

pvals <- ST05[[3]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P3 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Ash", x = "FD")

pvals <- ST05[[4]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P4 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Ca", x = "FD")

pvals <- ST05[[5]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P5 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula2, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula2) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Cprot", x = "FD")

pvals <- ST05[[6]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P6 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "dNDF30", x = "FD")

pvals <- ST05[[7]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P7 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "dNDF48", x = "FD")

pvals <- ST05[[8]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P8 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Fat", x = "FD")

pvals <- ST05[[9]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P9 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "IVTDMD30", x = "FD")

pvals <- ST05[[10]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P10 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "IVTDMD48", x = "FD")

pvals <- ST05[[11]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P11 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "K", x = "FD")

pvals <- ST05[[12]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P12 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Lignin", x = "FD")

pvals <- ST05[[13]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P13 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Mg", x = "FD")

pvals <- ST05[[14]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P14 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "P", x = "FD")

pvals <- ST05[[15]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P15 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "RFV", x = "FD")

pvals <- ST05[[16]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P16 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Starch", x = "FD")

pvals <- ST05[[17]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P17 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Su_ESC", x = "FD")

pvals <- ST05[[18]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P18 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Su_WSC", x = "FD")


P19 <- ggarrange(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18, ncol = 4, nrow = 5)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "lm1.pdf", plot = P19, dpi = 300, width = 9, height = 10, device = cairo_pdf)
ggsave(filename = "lm1.jpg", plot = P19, dpi = 300, width = 9, height = 9)
