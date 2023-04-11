# cut

names(ST05)
pvals$std.error
pvals <- ST05[[1]][[5]]
pvals$cut <- as.numfac(pvals$cut)
# Standard error of the mean
ggplot(pvals, aes(x=cut, y=predicted.value, 1)) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  geom_line() +
  geom_point()

# cut2 --------------------------------------------------------------------

formula <- y ~ x
formula1 <- y ~ x + I(x^2)
formula2 <- y ~ x + I(x^2) + I(x^3)


pvals <- ST05[[1]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P1 <- ggplot(data = pvals, aes(x = cut, y = predicted.value, 1)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "ADF", x = "cut")

pvals <- ST05[[2]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P2 <- ggplot(data = pvals, aes(x = cut, y = predicted.value, 1)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "aNDF", x = "cut")

pvals <- ST05[[3]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P3 <- ggplot(data = pvals, aes(x = cut, y = round(predicted.value, 1))) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Ash", x = "cut")

pvals <- ST05[[4]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P4 <- ggplot(data = pvals, aes(x = cut, y = round(predicted.value, 1))) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Ca", x = "cut")

pvals <- ST05[[5]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P5 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Cprot", x = "cut")

pvals <- ST05[[6]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P6 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "dNDF30", x = "cut")

pvals <- ST05[[7]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P7 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "dNDF48", x = "cut")

pvals <- ST05[[8]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P8 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Fat", x = "cut")

pvals <- ST05[[9]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P9 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "IVTDMD30", x = "cut")

pvals <- ST05[[10]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P10 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "IVTDMD48", x = "cut")

pvals <- ST05[[11]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P11 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "K", x = "cut")

pvals <- ST05[[12]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P12 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Lignin", x = "cut")

pvals <- ST05[[13]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P13 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Mg", x = "cut")

pvals <- ST05[[14]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P14 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "P", x = "cut")

pvals <- ST05[[15]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P15 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "RFV", x = "cut")

pvals <- ST05[[16]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P16 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Starch", x = "cut")

pvals <- ST05[[17]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P17 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Su_ESC", x = "cut")

pvals <- ST05[[18]][[5]]
pvals$cut <- as.numfac(pvals$cut)
P18 <- ggplot(data = pvals, aes(x = cut, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Su_WSC", x = "cut")


names(ST05)
P19 <- ggarrange(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18, ncol = 4, nrow = 5)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "lm2.pdf", plot = P19, dpi = 300, width = 9, height = 9, device = cairo_pdf)
ggsave(filename = "lm2.jpg", plot = P19, dpi = 300, width = 9, height = 9)
