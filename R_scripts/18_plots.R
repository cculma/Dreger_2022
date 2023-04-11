


# geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
names(ST05)

pvals <- ST05[[8]][[3]] # FD_cut
pvals <- ST05[[15]][[4]] # FD_cut_loc
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula2, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula2) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "RFV", x = "FD") + facet_grid(cut~loc)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "RFV.pdf", plot = P1, dpi = 300, width = 6, height = 6, device = cairo_pdf)
ggsave(filename = "RFV.jpg", plot = P1, dpi = 300, width = 6, height = 6)

pvals <- ST05[[5]][[4]] # FD_cut_loc
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula2, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula2) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "CP", x = "FD") + facet_grid(cut~loc)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "CP.pdf", plot = P1, dpi = 300, width = 6, height = 6, device = cairo_pdf)
ggsave(filename = "CP.jpg", plot = P1, dpi = 300, width = 6, height = 6)

pvals <- ST05[[1]][[4]] # FD_cut_loc
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula2, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula2) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "ADF", x = "FD") + facet_grid(cut~loc)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "ADF.pdf", plot = P1, dpi = 300, width = 6, height = 6, device = cairo_pdf)
ggsave(filename = "ADF.jpg", plot = P1, dpi = 300, width = 6, height = 6)

pvals <- ST05[[8]][[4]] # FD_cut_loc
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Fat", x = "FD") + facet_grid(cut~loc)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "Fat.pdf", plot = P1, dpi = 300, width = 6, height = 6, device = cairo_pdf)
ggsave(filename = "Fat.jpg", plot = P1, dpi = 300, width = 6, height = 6)

pvals <- ST05[[12]][[4]] # FD_cut_loc
pvals$FD <- as.numfac(pvals$FD)
P1 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula2, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula2) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Lignin", x = "FD") + facet_grid(cut~loc)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "Lignin.pdf", plot = P1, dpi = 300, width = 6, height = 6, device = cairo_pdf)
ggsave(filename = "Lignin.jpg", plot = P1, dpi = 300, width = 6, height = 6)


pvals <- ST05[[12]][[4]] # FD_cut_loc
pvals$cut <- as.numfac(pvals$cut)
P1 <- ggplot(data = pvals, aes(x = cut, y = predicted.value, 1)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + 
  geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 4), breaks = seq(1, 4, by = 1)) + labs(y = "Lignin", x = "cut") + facet_grid(FD~loc)

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "Lignin1.pdf", plot = P1, dpi = 300, width = 5, height = 7, device = cairo_pdf)
ggsave(filename = "Lignin1.jpg", plot = P1, dpi = 300, width = 5, height = 7)

# 


pvals <- ST05[[8]][[3]]
pvals$FD <- as.numfac(pvals$FD)

ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) + geom_errorbar(aes(ymin=predicted.value-std.error, ymax=predicted.value+std.error), width=.1) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "ADF", x = "FD") + facet_wrap(~cut)




pvals <- ST05[[2]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P2 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "aNDF", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[3]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P3 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Ash", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[4]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P4 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Ca", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[5]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P5 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula2, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula2) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Cprot", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[6]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P6 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "dNDF30", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[7]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P7 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "dNDF48", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[8]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P8 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Fat", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[9]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P9 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "IVTDMD30", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[10]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P10 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "IVTDMD48", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[11]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P11 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "K", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[12]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P12 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Lignin", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[13]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P13 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Mg", x = "FD") + facet_wrap(~loc)

pvals <- ST05[[14]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P14 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "P", x = "FD")

pvals <- ST05[[15]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P15 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula1, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula1) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "RFV", x = "FD")

pvals <- ST05[[16]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P16 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Starch", x = "FD")

pvals <- ST05[[17]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P17 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Su_ESC", x = "FD")

pvals <- ST05[[18]][[1]]
pvals$FD <- as.numfac(pvals$FD)
P18 <- ggplot(data = pvals, aes(x = FD, y = predicted.value)) + geom_point() +
  stat_poly_line(formula = formula, color = "black", se = F) + theme_classic(base_family = "Arial", base_size = 12) +
  stat_poly_eq(use_label(c("adj.R2", "p.value.label")), formula = formula) + scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + labs(y = "Su_WSC", x = "FD")


names(ST05)
P19 <- ggarrange(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18, ncol = 4, nrow = 5)
P19

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "lm1.pdf", plot = P19, dpi = 300, width = 9, height = 10, device = cairo_pdf)
ggsave(filename = "lm1.jpg", plot = P19, dpi = 300, width = 9, height = 9)
