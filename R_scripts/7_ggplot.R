
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
ggplot(df1, aes(x=FD, y=predicted.value)) + 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + theme_classic(base_family = "Arial", base_size = 12)

df1 <- as.data.frame(ST1[[13]][[2]][[1]])
head(df1)
ggplot(df1, aes(x=loc, y=predicted.value)) + 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + 
  theme_classic(base_family = "Arial", base_size = 12)

df1 <- as.data.frame(ST1[[13]][[5]][[1]])
head(df1)
ggplot(df1, aes(x=FD, y=predicted.value)) + 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) +
  theme_classic(base_family = "Arial", base_size = 12) +
  facet_grid(. ~ loc, scales = "free_x", space = "free")

df1 <- as.data.frame(ST1[[13]][[11]][[1]])
head(df1)
ggplot(df1, aes(x=year, y=predicted.value, group=FD, color=FD)) + 
  geom_line(linetype="twodash") +
  geom_point(aes(color = FD), size = 1, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) + 
  theme_classic(base_family = "Arial", base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + facet_grid(. ~ loc, scales = "free_x", space = "free") + 
  scale_color_brewer(palette = "Dark2") 

ggplot(df1, aes(x=FD, y=predicted.value)) + 
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, alpha = 0.6) +
  theme_classic(base_family = "Arial", base_size = 12) +
  facet_grid(. ~ loc, scales = "free_x", space = "free") + 
  facet_grid(loc ~ year, scales = "free_x", space = "free")


ggplot(df1, aes(x=FD, y=predicted.value, group=loc)) + 
  geom_point(size = 1, aes(shape=loc), na.rm=TRUE, position=position_dodge(width=0.5)) +
  scale_shape_manual(values = c(2, 0, 1)) +
  geom_errorbar(aes(ymin = lower.Confidence.limit, ymax = upper.Confidence.limit), width = 0.1, na.rm=TRUE, position=position_dodge(width=0.5)) +
  theme_classic(base_family = "Arial", base_size = 12) +
  facet_grid(. ~ year, scales = "free_x", space = "free")


# df1 <- as.data.frame(ST1[[19]][[7]][[1]])
# head(df1)
