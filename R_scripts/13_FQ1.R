

# FQ without FD -----------------------------------------------------------

head(data)
data$cons_days <- as.factor(data$cons_days)
summary(data$cons_days)
data$year <- as.factor(data$year)
summary(data$year)
data$loc <- as.factor(data$loc)


FQ4 <- list()
for (i in 1:(length(FQ3))) {
  # data <- FQ3[[1]]
  data <- FQ3[[i]]
  data$cons_days <- as.factor(data$cons_days)
  data$year <- as.factor(data$year)
  data$loc <- as.factor(data$loc)
  data1 <- na.omit(data)
  head(data1)
  
  M1 <- asreml::asreml(fixed = BLUE ~ loc * year,
                       random = ~ corgh(loc):ar1(year):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt)
  
  diffs <- predictPlus(classify = "gen:loc:year",
                       asreml.obj = M1,
                       wald.tab = current.asrt$wald.tab,
                       present = c("gen","loc","year"))
  
  blup <- diffs[["predictions"]]
  FQ4[[length(FQ4)+1]] <- blup
  
}

length(FQ3)
length(FQ4)
names(FQ4) <- names(FQ3)
names(FQ4)
FQ5 <- FQ4[-c(7,8,10,11,15,16,22)]
names(FQ5)

FQ5 <-rbindlist(FQ5, use.names=TRUE, fill=TRUE, idcol="trait")
FQ5 <- FQ5 %>% dplyr::select("trait","gen","loc","year","predicted.value")
FQ6 <- FQ5 %>% spread(key = trait, value = predicted.value)
str(FQ6)
FQ6 <- as_tibble(FQ6)
head(FQ6)
FQ6 <- na.omit(FQ6)





FQ7 <- as_tibble(FQ4[["NDFD48"]])
summary(FQ7$year)
summary(FQ7$loc)
FQ7 %>% dplyr::count(loc, year)
str(FQ7)


FQ5 <- FQ5 %>% dplyr::select("trait","gen","loc","year","predicted.value") %>% unite("env", c(gen, loc, year), sep = "_", remove = T)
FQ5 <- FQ5 %>% spread(key = trait, value = predicted.value) %>% column_to_rownames("env")
colnames(FQ5)
P00 <- cor(FQ5, use = "complete.obs")
P00 <- round(P00, 2)
P00[upper.tri(P00)] <- NA
setwd("~/Documents/git/Dreger_2022/statistical_results/cor/")
write.csv(P00, 'cor2.csv', row.names = T, quote = F)


heatmap.2(P00, keysize=2, revC = TRUE, density.info = "none", trace = "none", dendrogram = "col") 

mds2 <- -cmdscale(UScitiesD)
plot(mds2, type="n", axes=FALSE, ann=FALSE)
text(mds2, labels=rownames(mds2), xpd = NA)

hcity.D  <- hclust(UScitiesD, "ward.D") # "wrong"
hcity.D2 <- hclust(UScitiesD, "ward.D2")
opar <- par(mfrow = c(1, 2))
plot(hcity.D,  hang=-1)
plot(hcity.D2, hang=-1)
par(opar)

# PCA ---------------------------------------------------------------------
library(gplots)
library(ggfortify)
library(metan)

FQ6 <- na.omit(FQ5)
FQ6 <- t(FQ6)
PCs <- prcomp(FQ6, scale=T)
head(PCs)
class(PCs)
PCA1 <- autoplot(PCs, data = FQ6, label = T)
PCA1 + theme_bw()
PCs <- princomp(FQ6, cor = T, scores = T)


# GT biplot for all numeric variables
dim(FQ6)

mod <- gtb(.data = FQ6, gen = gen, scaling = 1, centering = 2, svp = 1) # best

plot(mod)
plot1 <- plot(mod, type = 4, col.env = "black", col.line = "gray", col.alpha = 0.5, title = F) + theme_bw(base_family = "Arial", base_size = 12) # best


plot1 <- plot(mod, type = 10, col.env = "black", col.line = "gray", col.alpha = 0.5) + theme_bw(base_family = "Arial", base_size = 12)
setwd("~/Documents/git/Norberg_2020/figs/fd_plots/")
ggsave(filename = "discri_biplot.jpg", plot = plot1, width = 4, height = 3)


# FQ with FD --------------------------------------------------------------

lev8 <- c("FD_fc","gen","loc","year","FD_fc:loc","FD_fc:year","FD_fc:loc:year","gen:loc","gen:year","gen:loc:year")
lev7 <- c("FD_fc","gen","loc","year","FD_fc_loc","FD_fc_year","FD_fc_loc_year","gen_loc","gen_year","gen_loc_year")


FQ2 <- FQ1 %>% separate(1, c("loc", "year", "cut", "trait"), sep = "_", remove = T, convert = FALSE, extra = "merge") %>% unite("env", c(loc, year, cut), sep = "_", remove = F)
FQ2 <- inner_join(FQ2, b2, by = c("env","loc","year")) %>% inner_join(., b4, by = "gen")
FQ2 <- droplevels(FQ2)

FQ3 <- split(FQ2, FQ2$trait)
names(FQ3)
FQ3 <- FQ3[-c(7,8,10,11,15,16,22)]

FQ4 <- list()
for (i in 1:(length(FQ3))) {
  # data <- FQ3[[1]]
  data <- FQ3[[i]]
  data[,c("cons_days","year","loc")] <- lapply(data[,c("cons_days","year","loc")], factor)
  data1 <- na.omit(data)
  head(data1)
  
  M1 <- asreml::asreml(fixed = BLUE ~ FD_fc * loc * year,
                       random = ~ corgh(loc):ar1(year):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  M1 <- update.asreml(M1)
  current.asrt <- as.asrtests(M1, NULL, NULL)
  current.asrt <- rmboundary.asrtests(current.asrt)
  current.asrt$wald.tab

  ST2 <- list()
  for (j in 1:length(lev8)) {
    diffs <- predictPlus(classify = lev8[j],
                         asreml.obj = M1,
                         wald.tab = current.asrt$wald.tab,
                         present = c("FD_fc","gen","loc","year"))

    ST2[[length(ST2)+1]] <- diffs
  }
  names(ST2) <- lev7
  FQ4[[length(FQ4)+1]] <- ST2
}
names(FQ4) <- names(FQ3)

data <- FQ4[["ADF"]][["gen"]][["predictions"]]
data1 <- FQ4[["ADF"]][["gen"]][["p.differences"]]

# data <- ST1[[n]]
# df_total = data.frame()
pdif <- FQ4[["ADF"]][["gen"]][["p.differences"]] # p.diff
pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]

data2 <- FQ4[["ADF"]][["gen"]][["predictions"]] # blup
data2 <- data2 %>% dplyr::select(1:2)
data2$predicted.value <- round(data2$predicted.value, 2)

let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
let2 <- as.data.frame(print(let1)) %>% rownames_to_column("cut")
let2 <- let2[,c(1,2)]
colnames(let2) <- c("cut", "Letters")
let2 <- let2 %>% separate(1, into = c("FD","cut","loc"), remove = T)

blup <- inner_join(data2, let2, by = c("FD","cut","loc"))
blup <- blup %>% unite("BLUP", 4:5, sep = " ", remove = T)
blup <- blup[,-3]
df_total <- rbind(df_total,blup)


############
ggplot(data = diffs[["predictions"]], aes(x=year, y=predicted.value, group=FD_fc))  + theme_classic(base_family = "Arial", base_size = 12) + geom_point(aes(shape=FD_fc), size = 1, alpha = 0.6) + geom_line(aes(linetype=FD_fc), alpha = 0.6)  + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) + labs(y = "") + facet_grid(loc ~ .) 
