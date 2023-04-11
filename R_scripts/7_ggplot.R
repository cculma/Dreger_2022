library(patchwork)
library(ggpubr)
library(ggplot2)
library("lubridate")

names(ST1)
names(ST1[[13]])
df1 <- as.data.frame(ST1[[13]][[14]][[1]])
head(df1)
ggplot(df1, aes(x=env4, y=predicted.value, group=FD, color=FD)) + 
  geom_line(alpha = 0.6) +
  geom_point(aes(color = FD), size = 1, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + 
  theme_bw(base_family = "Arial", base_size = 14) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ ., scales = "free_x", space = "free")

df1 <- as.data.frame(ST1[[13]][[11]][[1]])
head(df1)
ggplot(df1, aes(x=year, y=predicted.value, group=FD, color=FD)) + 
  geom_line(alpha = 0.6) +
  geom_point(aes(color = FD), size = 1, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + 
  theme_bw(base_family = "Arial", base_size = 14) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(loc ~ ., scales = "free_x", space = "free")


df1 <- as.data.frame(ST1[[13]][[1]][[1]])
head(df1)

# A
A <- ggplot(df1, aes(x=FD, y=predicted.value)) + 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + theme_classic(base_family = "Arial", base_size = 12) + labs(y = "", x = "FD")

# B
df1 <- as.data.frame(ST1[[13]][[2]][[1]])
head(df1)
B <- ggplot(df1, aes(x=loc, y=predicted.value)) + 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + 
  theme_classic(base_family = "Arial", base_size = 12) + labs(y = "", x = "Loc")


# FD_loc
# C
df1 <- as.data.frame(ST1[[13]][[5]][[1]])
head(df1)

# C
C <- ggplot(df1, aes(x=FD, y=predicted.value, group = loc)) + 
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.08, na.rm=TRUE, position=position_dodge(width=0.8)) + 
  geom_point(size = 1, aes(shape=loc, color = loc), na.rm=TRUE, position=position_dodge(width=0.8)) +
  scale_shape_manual(values = c(15, 16, 18)) +
  theme_classic(base_family = "Arial", base_size = 12) +
  theme(legend.position="none") + labs(y = "", x = "FD by Loc")

# FD loc env4 (year_month)

df1 <- as.data.frame(ST1[[13]][[11]][[1]])
head(df1)

# D

D <- ggplot(df1, aes(x=env4, y=predicted.value, group=loc)) + 
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.08, na.rm=TRUE, position=position_dodge(width=0.8)) + 
  geom_point(size = 1, aes(shape=loc, color = loc), na.rm=TRUE, position=position_dodge(width=0.8)) +
  scale_shape_manual(values = c(15, 16, 18)) +
  theme_classic(base_family = "Arial", base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), legend.position="none") +
  facet_grid(. ~ FD, scales = "free_x", space = "free") + labs(y = "", x = "Harvest")

A + B + C + D + plot_layout(ncol = 2)

ggarrange(A, B, widths = c(1, 0.5), ncol = 2, nrow = 1, align = "h")

ggarrange(ggarrange(A, B, widths = c(1, 0.5), ncol = 2, nrow = 1), C, ncol = 2, nrow = 1)

E <- ggarrange(ggarrange(ggarrange(A, B, widths = c(1, 0.5), ncol = 2), C, ncol = 2, nrow = 1), D, ncol = 1, nrow = 2, heights = c(0.7, 1), align = "v")

setwd("~/Documents/git/Dreger_2022/figs/")
ggsave(filename = "Lignin.jpg", plot = E, width = 15, height = 6)

F1 <- ggplot(df1, aes(x=env4, y=predicted.value, group=FD, color=FD)) + 
  geom_line(alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.08, na.rm=TRUE, alpha = 0.6) + 
  geom_point(size = 1, na.rm=TRUE, alpha = 0.6) +
  scale_shape_manual(values = c(15, 16, 18)) +
    theme_classic(base_family = "Arial", base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
  facet_grid(. ~ loc, scales = "free_x", space = "free") + labs(y = "predicted.value.Lignin", x = "Harvest")

  setwd("~/Documents/git/Dreger_2022/figs/")
  ggsave(filename = "Lignin2.jpg", plot = F1, width = 15, height = 4)
  
names(ST1) 
df1 <- as.data.frame(ST1[[5]][[11]][[1]])
head(df1)  

G <- ggplot(df1, aes(x=env4, y=predicted.value, group=FD, color=FD)) + 
  geom_line(alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.08, na.rm=TRUE, alpha = 0.6) + 
  geom_point(size = 1, na.rm=TRUE, alpha = 0.6) +
  scale_shape_manual(values = c(15, 16, 18)) +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
  facet_grid(. ~ loc, scales = "free_x", space = "free") + labs(y = "predicted.value.Cprot", x = "Harvest")

ggsave(filename = "Cprot.jpg", plot = G, width = 15, height = 4)

names(ST02)
data1 <- ST02[["Cprot"]]

levels(data1$env1)
data1 <- data1 %>% dplyr::filter(!env1 %in% c("OR_2021_1", "WA_2021_1"))

ggplot(data1, aes(x=date, y=predicted.value, color = gen, group=gen)) + geom_line(alpha = 0.6) + xlab("Harvest date") + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle=90, hjust = 1)) + facet_grid(. ~ loc, scales = "free_x", space = "free") + scale_x_date(date_labels="%b %y",date_breaks  ="1 month")


# MH_ML -------------------------------------------------------------------

ST03 <- ST02[-c(7,8,10,11,15,16,22)]
lev1 <- names(ST03)
setwd("~/Documents/git/Norberg_2020/figs/one-stage/")
for (i in 1:length(ST03)) {
  data <- ST03[[i]]
  data <- data %>% relocate(FD, .after = gen) %>% unite("gen1", 1:2, remove = FALSE)
  data$gen1 <- as.factor(data$gen1)
  levels(data$gen1)
  data <- data %>% dplyr::filter(!year == "2021")
  data <- droplevels(data)
  data <- data %>% mutate(gen1 = fct_relevel(gen1, 
                                             "61_6", "104_5", "44_4", 
                                             "202_3", "144_3", "201_2", 
                                             "112_1"))
  
  data <- na.omit(data)
  plot1 <- ggplot(data = data, aes(x=harv_days3, y=predicted.value, group=gen1)) + labs(col="loc") + theme_classic() + geom_point(aes(color=gen1), size = 1) + geom_line(aes(color=gen1), linetype = "dashed") + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + scale_x_continuous(limits = c(120, 300), breaks = seq(120, 300, by = 10)) + facet_grid(loc ~ year) + labs(title =  paste0(lev1[i], " single-stage"), y = "BLUE", x = "Days") 
  
  ggsave(filename = paste0("plot_", lev1[i], ".jpg"), plot = plot1, width = 10, height = 6)
}


# harv_time ---------------------------------------------------------------

b2 <- read.csv("~/Documents/git/Norberg_2020/Raw_data/harvest_dates.csv")
b2$date <- as.Date(b2$date, format = "%m/%d/%y")
b2$date_init <- as.Date(b2$date_init, format = "%m/%d/%y")
lev1 <- c("env","loc","year")
b2[,lev1] <- lapply(b2[,lev1], factor)
head(b2)
# b2 <- b2[,-c(5,12)]

b2$y_pos

plot_B <- ggplot(b2, aes(x=harv_days3,y=0, col=loc, label=loc)) + labs(col="loc") + theme_classic() + geom_segment(data=b2, aes(x = 120, y = -0.2, xend = 300, yend = -0.2), color='black', size=0.2, linetype=2) + geom_segment(data=b2, aes(x = 120, y = 0, xend = 300, yend = 0), color='black', size=0.2, linetype=2) + geom_segment(data=b2, aes(x = 120, y = 0.2, xend = 300, yend = 0.2), color='black', size=0.2, linetype=2) + geom_point(data = b2, aes(y=y_pos), size=3) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.line.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) + facet_grid(year ~ .) + ylim(-0.5, 0.5) + scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 10))


plot_B <- ggplot(b2, aes(x=harv_days3,y=0, label=loc)) + geom_segment(data=b2, aes(x = 120, y = -0.2, xend = 300, yend = -0.2), color='black', size=0.2, linetype=2) + geom_segment(data=b2, aes(x = 120, y = 0, xend = 300, yend = 0), color='black', size=0.2, linetype=2) + geom_segment(data=b2, aes(x = 120, y = 0.2, xend = 300, yend = 0.2), color='black', size=0.2, linetype=2) + geom_point(data = b2, aes(y=y_pos, shape = loc), size = 2) + theme_classic(base_family = "Arial", base_size = 12) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.line.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank(), legend.key.size = unit(0.3, 'cm'), legend.position = c(0.9, 0.5)) + facet_grid(year ~ ., scales = "free_x", space = "free_x", switch = "y") + ylim(-0.5, 0.5) + scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 10)) + labs(x = "Days in a year")


setwd("~/Documents/git/Norberg_2020/figs/fd_plots/")
ggsave(filename = "harv_time.jpg", plot = plot_B, width = 6, height = 3.5)



