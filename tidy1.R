rm(list = ls())

# quality
library(data.table)
library(tidyverse)
library(ggpubr)
library(StageWise)
library(asreml)
library(asremlPlus)
library(ggrepel)
library(ggpubr)



setwd("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022")
setwd("~/Documents/git/Dreger_2022/")

data_qual <- list.files(pattern = ".csv", full.names = T)

a2 <- read.csv("qual1.1.csv")
A2018_1 <- a2
dim(A2018_1)
# qual1 contain all entries 2018 2019
# qual2 contain 201 & 202 2018
# qual3 contain 201 & 202 2019 ID
# qual4 contain 201 & 202 2019 OR
# qual5 contain 201 & 202 2019 WA
# qual6 contain all entries 2018 2019 2020 2021 NIRS

qual1 <- list()
for (i in 1:length(data_qual)) {
  data <- read.csv(data_qual[i])
  qual1[[length(qual1)+1]] <-  data
}

a1 <- read.csv("~/Documents/git/Norberg_2020/spatial_distribution/cols_rows1.csv")
head(a1)

# A2018_1 <- qual1[[1]]

A2018_1 <- A2018_1[1:(length(A2018_1)-5)]
colnames(A2018_1)
colnames(A2018_1)[3] <- "Treatment"
A2018_1 <- A2018_1[,-c(4)]
A2018_1 <- left_join(a1, A2018_1, by = c("ID", "Treatment"))
# A2018_1$Year <- "18"
A2018_1$Cutting <- 1
A2018_1 <- A2018_1 %>% gather(key = "trait", value = "raw", 9:75)



A2018_2 <- qual1[[2]]
A2018_2 <- A2018_2[,-c(3:6)]
colnames(A2018_2)
A2018_2 <- inner_join(a1, A2018_2, by = "ID")
A2018_2 <- A2018_2 %>% gather(key = "trait", value = "raw", 10:24)


# ID2019 <- qual1[[3]]
# ID2019 <- ID2019[,-c(2,3,5)]
# colnames(ID2019)
# ID2019.2 <- ID2019 %>% dplyr::filter(Cutting %in% c(2))
# ID2019.2 <- inner_join(a1, ID2019.2, by = "ID")

ID2019 <- qual1[[3]]
OR2019 <- qual1[[4]]
WA2019 <- qual1[[5]]
colnames(ID2019)
colnames(OR2019)
colnames(WA2019)

ID2019 <- ID2019[,-c(1,2,3,5)]
OR2019 <- OR2019[,-c(1,2,3,5)]
WA2019 <- WA2019[,-c(1,2,3,5,6)]

A2019 <- rbind(ID2019, OR2019, WA2019)
A2019$Cutting <- as.factor(A2019$Cutting)
levels(A2019$Cutting)
A2019 <- inner_join(a1, A2019, by = "ID")
colnames(A2019)
A2019$Year <- "19"
A2019 <- A2019 %>% gather(key = "trait", value = "raw", 9:26)


N2018_1 <- qual1[[8]]
N2018_1 <- read.csv("~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022/qual6.csv")
colnames(N2018_1)
head(N2018_1)
head(a1)

N2018_1 <- left_join(a1, N2018_1, by = c("Location", "Treatment", "Block", "Position"))
N2018_1 <- N2018_1 %>% gather(key = "trait", "raw", 9:33)
N2018_1$Cutting <- 1
N2018_1[,lev1] <- lapply(N2018_1[,lev1], factor)
str(N2018_1)
colnames(N2018_1)[11] <- "Cut"
colnames(N2018_1)[5] <- "gen"
colnames(N2018_1)[10] <- "resp"
colnames(N2018_1)[2] <- "block"
N2018_1 <- N2018_1[,c(1:8,11,9,10)]

N201 <- N2018_1 %>% dplyr::filter(gen %in% c(201))
N202 <- N2018_1 %>% dplyr::filter(gen %in% c(202))
head(N201)
N201 <- N201[,-c(3:7)]
colnames(N201)[6] <- "cov1"
N202 <- N202[,-c(3:7)]
colnames(N202)[6] <- "cov2"

N1.3 <- left_join(N2018_1, N201, by= c("Location", "Year", "Cut", "block", "trait")) %>% left_join(., N202, by= c("Location", "Year", "Cut", "block",  "trait"))
head(N1.3)
write.csv(N1.3, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022/N1.3.csv", row.names = F, quote = F)



colnames(A2018_1)
head(A2018_1)
dim(A2018_1)
lev1 <- c("Location", "Block", "Position", "ID", "Treatment", "row", "col", "Year", "Cutting", "trait")
A2018_1[lev1] <- lapply(A2018_1[lev1], factor)
A2018_1$raw <- as.numeric(A2018_1$raw)
str(A2018_1)

colnames(A2018_2)
colnames(A2019)
head(A2019)

qual2 <- rbind(A2018_2, A2019)
qual2 <- rbind(A2018_1, A2018_2, A2019)
lev1 <- colnames(qual2)
lev1 <- lev1[-11]
qual2[,lev1] <- lapply(qual2[,lev1], factor)
str(qual2)
colnames(qual2)[9] <- "Cut"
colnames(qual2)
summary(qual2)
levels(qual2$trait)
summary(qual2$trait)
summary(qual2$Treatment)

qual3 <- qual2 %>% unite("merged", c(Location, Year, Cut, trait), sep = "_", remove = F)
qual3 <- qual2 %>% unite("merged", c(Location, Year, Cut), sep = "_", remove = F)
qual3$merged <- as.factor(qual3$merged)
str(qual3)
qual4 <- split(qual3, qual3$merged)
length(qual4)
head(qual4[[1]])
str(qual4[[1]])
nlevels(qual4[[1]]$trait)
nrow(count(qual4[[1]], Location, trait))
nrow(count(qual4[[2]], Location, trait))
nrow(count(qual4[[3]], Location, trait))

names(qual4)
sapply(qual4, nrow)

data <- data.frame(n_trait = rep(NA, length(qual4)))
data1 <- list()
for (i in 1:(length(qual4))) {
  data <- nrow(count(qual4[[i]], Location, trait))
  data <- as.data.frame(data)
  data1[[length(data1)+1]] <- data
}
names(data1) <- names(qual4)
data1 <- rbindlist(data1, use.names=TRUE, fill=TRUE, idcol="trait")
write.csv(data1, "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022/n_trait.csv", row.names = F, quote = F)


names(qual4)
cc1 <- count(qual4[["WA_19_4_Starc"]], Treatment)
cc2 <- count(qual4[["WA_19_4_aNDF"]], Treatment)
cc3 <- count(qual4[["OR_19_4_aNDF"]], Treatment)
cc4 <- count(qual4[["OR_19_4_Starc"]], Treatment)

levels(qual4[["ID_18_2"]]$trait)
levels(qual4[["ID_18_3"]]$trait)
cc5 <- count(qual4[["ID_18_2"]], Treatment, trait)
cc5 %>% spread(key = trait, value = n)

qual6 <- list()
for (i in 1:(length(qual4))) {
  cc5 <- count(qual4[[i]], Treatment, trait)
  cc5 <- cc5 %>% spread(key = trait, value = n)
  qual6[[length(qual6)+1]] <- cc5
}
names(qual6) <- names(qual4)

colnames(qual6[["ID_18_2"]])
colnames(qual6[["ID_19_3"]])



A2018_1[,lev1] <- lapply(A2018_1[,lev1], factor)
str(A2018_1)
colnames(A2018_1)[9] <- "Cut"
colnames(A2018_1)[5] <- "gen"
colnames(A2018_1)[11] <- "resp"
colnames(A2018_1)[2] <- "block"

colnames(A2018_1)
head(A2018_1)
# A2018_1 <- na.omit(A2018_1)
summary(A2018_1)
levels(A2018_1$trait)
summary(A2018_1$trait)
summary(A2018_1$gen)

c201 <- A2018_1 %>% dplyr::filter(gen %in% c(201))
c202 <- A2018_1 %>% dplyr::filter(gen %in% c(202))
c201 <- c201[,-c(3:7)]
colnames(c201)[6] <- "cov1"
c202 <- c202[,-c(3:7)]
colnames(c202)[6] <- "cov2"

b1.3 <- left_join(A2018_1, c201, by= c("Location", "Year", "Cut", "block", "trait")) %>% left_join(., c202, by= c("Location", "Year", "Cut", "block",  "trait"))
head(b1.3)
nlevels(qual5$merged)
nlevels(qual5$Location)
nlevels(qual5$Year)
nlevels(qual5$trait)

dim(b1.3)
data_subset <- b1.3[ , "cov1"]
b1.3 <- b1.3[complete.cases(data_subset), ]
which(is.na(b1.3$resp))

qual5 <- b1.3 %>% unite("merged", c(Location, trait, Year), sep = "_", remove = F)
qual5$merged <- as.factor(qual5$merged)
str(qual5)
qual6 <- split(qual5, qual5$merged)
qual6[[145]]
str(qual6[["ID_ADF_18"]])

qual7 <- qual6[-c(149,189)]
names(qual6)
head(qual6[[189]])
# WA_T_Milk48_NEG
# WA_Lig_NEL
#######################
# ST0
qual_AIC <- list()
qual_BLUE <- list()
for (i in 1:length(qual7)) {
  data <- qual6[[189]]
  data <- data[order(data$row, data$col), ]
  
  m1 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~sar(row):sar(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m2 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):id(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  m3 <- asreml::asreml(fixed = resp ~ 1 + gen + cov1 + cov2, 
                       random = ~ + block, residual = ~ar1(row):ar1(col), 
                       data = data, 
                       na.action = list(x = "include", y = "include"))
  
  info1 <- infoCriteria.asreml(m1)
  info2 <- infoCriteria.asreml(m2)
  info3 <- infoCriteria.asreml(m3)
  
  info1$model <- "sar_sar"
  info2$model <- "ar1_id"
  info3$model <- "ar1_ar1"
  
  data1 <- rbind(info1, info2)
  data1 <- rbind(info1, info2, info3)
  qual_AIC[[length(qual_AIC)+1]] = data1
  
  ifelse(info1$AIC < info2$AIC && info1$AIC < info3$AIC, 
         blue <- predict.asreml(m1, classify='gen', vcov=TRUE)$pvals,
         ifelse(info2$AIC < info1$AIC && info2$AIC < info3$AIC,
                blue <- predict.asreml(m2, classify='gen', vcov=TRUE)$pvals,
                ifelse(info3$AIC < info1$AIC && info3$AIC < info2$AIC,
                       blue <- predict.asreml(m3, classify='gen', vcov=TRUE)$pvals)))
  
  
  colnames(blue) <- c("gen", "BLUE", "std.error", "status")
  blue$weight <- (1/blue$std.error)^2
  qual_BLUE[[length(qual_BLUE)+1]] = blue
  
}

names(qual_BLUE) <- names(qual7)
names(qual_AIC) <- names(qual7)

qual_AIC1 <- c(list('WA_Lig_NEL'=data1),qual_AIC)
qual_BLUE1 <- c(list('WA_Lig_NEL'=blue), qual_BLUE)
qual_AIC1 <- c(list('WA_T_Milk48_NEG'=data1),qual_AIC1)
qual_BLUE1 <- c(list('WA_T_Milk48_NEG'=blue), qual_BLUE1)

qual_AIC2 <-rbindlist(qual_AIC1, use.names=TRUE, fill=TRUE, idcol="trait")
qual_AIC2[ , .SD[which.min(AIC)], by = trait]

qual_BLUE2 <-rbindlist(qual_BLUE1, use.names=TRUE, fill=TRUE, idcol="trait")


qual_BLUE3 <- qual_BLUE2 %>% dplyr::select(1:3) %>% spread(key = trait, value = BLUE, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
colnames(qual_BLUE3)[2:length(qual_BLUE3)] <- gsub("^", "ST0_", colnames(qual_BLUE3)[2:length(qual_BLUE3)])
str()
colnames(qual_BLUE3)

qual_BLUE4 <- qual_BLUE2 %>% separate(1, c("loc", "trait"), sep = "_", remove = F, convert = FALSE, extra = "merge")


###############
# ST1
list_5.1 <- c("env", "gen", "trait")

class(qual_BLUE4)
qual_BLUE4 <- as.data.frame(qual_BLUE4)
colnames(qual_BLUE4)[2] <- "env" 
qual_BLUE4[list_5.1] <- lapply(qual_BLUE4[list_5.1], factor)
str(qual_BLUE4)  
head(qual_BLUE4)
qual_BLUE5 <- split(qual_BLUE4, qual_BLUE4$trait)
class(qual_BLUE5[[1]])
names(qual_BLUE5)
data <- qual_BLUE5[[1]]

qual_BLUP1 <- list()
qual_BLUP2 <- list()
qual_AIC3 <- list()

for (i in 1:(length(qual_BLUE5))) {
  data <- qual_BLUE5[[i]]
  data <- data[order(data$gen, data$env), ]
  data1 <- na.omit(data)
  head(data1)
  
  Diag <- asreml::asreml(fixed = BLUE ~ 1 + env, 
                         random = ~ + diag(env):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  US <- asreml::asreml(fixed = BLUE ~ 1 + env,
                       random = ~ + idv(env):id(gen),
                       data = data1, na.action = list(x = "include", y = "include"),
                       weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  FA_1 <- asreml::asreml(fixed = BLUE ~ 1, 
                         random = ~ + fa(env, 1):id(gen),
                         data = data1, na.action = list(x = "include", y = "include"), 
                         weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  FA_1 <- update.asreml(FA_1)
  
  # FA_2 <- asreml::asreml(fixed = BLUE ~ 1 +  env, 
  #                        random = ~ + fa(env, 2):id(gen),
  #                        data = data1, na.action = list(x = "include", y = "include"), 
  #                        weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  # FA_2 <- update.asreml(FA_2)

  CORGH <- asreml::asreml(fixed = BLUE ~ 1 + env,
                        random = ~ + corgh(env):id(gen),
                        data = data1, na.action = list(x = "include", y = "include"),
                        weights = weight, family = asreml::asr_gaussian(dispersion = 1))
  
  i1 <- infoCriteria.asreml(Diag)
  i2 <- infoCriteria.asreml(US)
  i3 <- infoCriteria.asreml(FA_1)
  i4 <- infoCriteria.asreml(CORGH)

  i1$model <- "Diag"
  i2$model <- "US"
  i3$model <- "FA_1"
  i4$model <- "CORGH"
  
  data2 <- rbind(i1, i2, i3, i4)
  qual_AIC3[[length(qual_AIC3)+1]] = data2
  
  ifelse(i1$AIC < i2$AIC && i1$AIC < i3$AIC && i1$AIC < i4$AIC, 
         c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "gen"))$predictions,
           BLUP4 <- predictPlus(classify = "gen", asreml.obj = Diag, 
                                wald.tab = NULL, 
                                present = c("env", "gen"))$predictions),
         ifelse(i2$AIC < i1$AIC && i2$AIC < i3$AIC && i2$AIC < i4$AIC,
                c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "gen"))$predictions,
                  BLUP4 <- predictPlus(classify = "gen", asreml.obj = US, 
                                       wald.tab = NULL, 
                                       present = c("env", "gen"))$predictions),
                ifelse(i3$AIC < i1$AIC && i3$AIC < i2$AIC && i3$AIC < i4$AIC, 
                       c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env", "gen"))$predictions,
                         BLUP4 <- predictPlus(classify = "gen", asreml.obj = FA_1, 
                                              wald.tab = NULL, 
                                              present = c("env", "gen"))$predictions),
                       ifelse(i4$AIC < i1$AIC && i4$AIC < i2$AIC && i4$AIC < i3$AIC, 
                              c(BLUP3 <- predictPlus(classify = "env:gen", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env", "gen"))$predictions,
                                BLUP4 <- predictPlus(classify = "gen", asreml.obj = CORGH, 
                                                     wald.tab = NULL, 
                                                     present = c("env", "gen"))$predictions)
                       ))))
  
BLUP3 <- BLUP3[,c(1:3)]
BLUP4 <- BLUP4[,c(1:2)]
BLUP3$env <- gsub("^", "ST1_", BLUP3.1$env)
BLUP3$env <- paste(BLUP3.1$env, names(qual_BLUE5[i]), sep = "_", collapse = NULL)
BLUP4$env <- paste("ST2", names(qual_BLUE5[i]), sep = "_", collapse = NULL)
qual_BLUP1[[length(qual_BLUP1)+1]] <- BLUP3
qual_BLUP2[[length(qual_BLUP2)+1]] <- BLUP4

}

names(qual_BLUP1) <- names(qual_BLUE5)
names(qual_BLUP2) <- names(qual_BLUE5)
names(qual_AIC3) <- names(qual_BLUE5)


qual_BLUP1.1 <-rbindlist(qual_BLUP1, use.names=TRUE, fill=TRUE, idcol="trait")
qual_BLUP2.1 <-rbindlist(qual_BLUP2, use.names=TRUE, fill=TRUE, idcol="trait")
qual_AIC3 <-rbindlist(qual_AIC3, use.names=TRUE, fill=TRUE, idcol="trait")

head(qual_BLUP1.1)
head(qual_BLUP2.1)

qual_BLUP1.1 <- qual_BLUP1.1 %>% dplyr::select(2:4) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

qual_BLUP2.1 <- qual_BLUP2.1 %>% dplyr::select(2:4) %>% spread(key = env, value = predicted.value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)

qual_BLUP3 <- qual_BLUE3 %>% inner_join(., qual_BLUP1.1, by = "gen") %>% inner_join(., qual_BLUP2.1, by = "gen")

colnames(qual_BLUP3)
class(qual_BLUP3)


##########

qual_BLUP4 <- as.data.frame(qual_BLUP3)

lev0 <- subset(colnames(qual_BLUP3),  grepl("^ST0", colnames(qual_BLUP3)))
lev1 <- subset(colnames(qual_BLUP3),  grepl("^ST1", colnames(qual_BLUP3)))
lev2 <- subset(colnames(qual_BLUP3),  grepl("^ST2", colnames(qual_BLUP3)))

library(stringr)
nchar(lev1)
lev1 <- subset(colnames(qual_BLUP3),  grepl("^ST1", colnames(qual_BLUP3)))
str_sub(lev1, 7, 10) <- ""
length(lev1)
lev1

colnames(qual_BLUP4)[203:403] <- lev1 
colnames(qual_BLUP4)

subset(colnames(qual_BLUP4),  grepl("ST0_ID_ADF", colnames(qual_BLUP4)))
subset(colnames(qual_BLUP4),  grepl("ST0_OR_ADF", colnames(qual_BLUP4)))
subset(colnames(qual_BLUP4),  grepl("ST0_WA_ADF", colnames(qual_BLUP4)))



install.packages("GGally")

library(GGally)
library(airway)
data(airway)

colData(airway)[,1:3]
df <- as.data.frame(assays(airway)$counts[,1:4])
class(df)
class(qual_BLUP5)

ggpairs(qual_BLUP5) + theme_bw()


library(stringr)
library(caret)
library(pheatmap)

qual_BLUP5 <- qual_BLUP4[,lev0]
qual_BLUP6 <- qual_BLUP4[,lev1]
qual_BLUP7 <- qual_BLUP4[,lev2]

qual_BLUP5 <- cor(qual_BLUP5, use = "complete.obs")
qual_BLUP6 <- cor(qual_BLUP6, use = "complete.obs")
qual_BLUP7 <- cor(qual_BLUP7, use = "complete.obs")

H0 <- pheatmap(qual_BLUP5, fontsize_number = 8, legend = T, show_rownames = F, show_colnames = F, annotation_col = metadata0)
pheatmap(qual_BLUP6, fontsize_number = 8, legend = T, show_rownames = F, show_colnames = F, annotation_col = metadata1)

# library(gplots)
myheatcol <- redgreen(75)
# Creates heatmap for entire data set
pheatmap(qual_BLUP6, legend = T, show_rownames = T, show_colnames = F, annotation_col = metadata1, cexCol=0.6, labRow=NA)

metadata0 <- data.frame(row.names = colnames(qual_BLUP5), "Env" = substr(colnames(qual_BLUP5), 5, 6))
metadata1 <- data.frame(row.names = colnames(qual_BLUP6), "Env" = substr(colnames(qual_BLUP6), 5, 6))

substring(colnames(qual_BLUP5), 8)
substring(colnames(qual_BLUP6), 8)

result = substring(month, 4)
print(result)

library(pheatmap)
res <- pheatmap(mtcars)
clust <- cbind(qual_BLUP5, 
                      cluster = cutree(H0$tree_row, 
                                       k = 5))


##########
data <- replicate(20, rnorm(50))
rownames(data) <- paste("Gene", c(1:nrow(data)))
colnames(data) <- paste("Sample", c(1:ncol(data)))

metadata <- data.frame(
  c(rep("case", ncol(data)/2), rep("control", ncol(data)/2)),
  c(rep("cond1", ncol(data)/4), rep("cond2", ncol(data)/4), rep("cond3", ncol(data)/4), rep("cond4", ncol(data)/4)),
  row.names=colnames(data))
colnames(metadata) <- c("casecontrol","condition")

metadata_gene <- data.frame(
  c(rep("Tcell", nrow(data)/2), rep("Bcell", nrow(data)/2)),
  row.names=rownames(data))
colnames(metadata_gene) <- c("Cell")

out <- pheatmap(data, 
                show_rownames=F, cluster_cols=T, cluster_rows=T, scale="row",
                cex=1, clustering_distance_rows="euclidean", cex=1,
                clustering_distance_cols="euclidean", clustering_method="complete", border_color=FALSE,
                annotation_col=metadata,
                annotation_row=metadata_gene)

##########


P01 <- findCorrelation(qual_BLUP5, cutoff = 0.7, verbose = T, names = T, exact = T)
P02 <- findCorrelation(qual_BLUP6, cutoff = 0.7, verbose = T, names = T, exact = T)
P03 <- findCorrelation(qual_BLUP7, cutoff = 0.7, verbose = T, names = T, exact = T)

library(ggplot2)
library(ggpubr)




ggplot(a2, aes(x = a2[,1], y = a2[,3], fill = a2[,2])) + geom_boxplot(alpha = 0.6, outlier.shape = NA)  + scale_fill_manual(values = mycolors) + theme_ipsum(base_family = "Arial", base_size = 12) + theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 0, hjust = 0.95, vjust = 0.2)) + facet_grid( ~ a2[,2], scales = "free", space = "free") + labs(y = "FD_raw WA", x = "Allele dosage") + theme(panel.spacing = unit(0.3, "lines"))


hc0 <- hclust(as.dist(1-qual_BLUP6), method = "complete")
hc1 <- hclust(as.dist(1-qual_BLUP6), method = "complete")

G_dendro <- ggdendrogram(hc1, segments = T) + theme_ipsum(base_family = "Arial", base_size = 6) + theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2), axis.title.x=element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + abline(v=0.35)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022/dendro.png", plot = G_dendro, dpi = 300, width = 17, height = 4)

S_E <- 7.880138
S_a <- 34.94159
S_e <- 43.10000

S_a/S_e + S_a
sqrt(1-(S_E)^2/(S_a))

sqrt(0.08) 


library(tidyverse)
colnames(qual_BLUP4)

qual_BLUP8 <- qual_BLUP4[,c("gen", "ST0_ID_ADF","ST0_OR_ADF","ST0_WA_ADF", "ST1_ID_ADF","ST1_OR_ADF","ST1_WA_ADF")]
qual_BLUP8 <- qual_BLUP4[,c("gen", "ST0_ID_ADF","ST0_OR_ADF","ST0_WA_ADF")]
qual_BLUP8 <- qual_BLUP4[,c("gen", "ST1_ID_ADF","ST1_OR_ADF","ST1_WA_ADF")]

qual_BLUP8 <- qual_BLUP8 %>% gather(key = "trait", value = "BLUP", 2:7) %>% separate(col = 2, into = c("stage", "loc", "trait"), sep = "_", remove = T)
qual_BLUP8 <- qual_BLUP8 %>% gather(key = "trait", value = "BLUP", 2:4) %>% separate(col = 2, into = c("stage", "loc", "trait"), sep = "_", remove = T)

head(qual_BLUP8)


qual_BLUP9 <- qual_BLUP8 %>% dplyr::filter(gen %in% c(201))
qual_BLUP10 <- qual_BLUP8 %>% dplyr::filter(gen %in% c(202)) 
qual_BLUP8 <- qual_BLUP8 %>% dplyr::filter(!gen %in% c(201, 202)) 

qual_BLUP9$entry <- "201"
qual_BLUP10$entry <- "202"
qual_BLUP8$entry <- "new"
qual_BLUP8 <- rbind(qual_BLUP8, qual_BLUP9, qual_BLUP10)
head(qual_BLUP8)
tail(qual_BLUP8)

dim(qual_BLUP8)
str(qual_BLUP8)


highlight_df <- qual_BLUP8 %>% dplyr::filter(gen %in% c(201, 202)) 


ST0_ADF <- qual_BLUP8 %>% ggplot (aes(x = loc, y = BLUP)) + geom_boxplot(alpha = 0.6, outlier.shape = NA) + geom_point(data = highlight_df, aes(x = loc, y = BLUP, color=entry)) + scale_color_manual(values = c("201" = "purple", "202"="orange")) + theme_classic(base_family = "Arial", base_size = 12) + facet_wrap(.~ stage)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022/ADF2.png", plot = ST0_ADF, width = 8, height = 4, dpi = 300)

ST1_ADF <- qual_BLUP8 %>% ggplot (aes(x = loc, y = BLUP)) + geom_boxplot(alpha=0.2) + geom_point(data = highlight_df, aes(x = loc, y = BLUP, color=entry)) + scale_color_manual(values = c("201" = "purple", "202"="orange")) + theme_classic(base_family = "Arial", base_size = 12) 


qual_BLUP8 %>% ggplot(aes(x=loc, y=BLUP, fill=stage)) + geom_boxplot() + geom_jitter(width=0.1,alpha=0.2)


qual_BLUP8 %>% ggplot(aes(x=loc, y=BLUP, fill=stage)) + geom_boxplot(alpha = 0.6, outlier.shape = NA) + labs(fill = "Stage") + geom_point(color = dplyr::case_when(qual_BLUP8$entry == "201" ~ "#1b9e77", 
                                   qual_BLUP8$entry == "202" ~ "#d95f02"), position=position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), alpha=0.8) + theme_classic(base_family = "Arial", base_size = 12) 



qual_BLUP8 %>% ggplot(aes(x=loc, y=BLUP, fill=stage)) + geom_boxplot(alpha = 0.6, outlier.shape = NA) + labs(fill = "Stage") + geom_point(data = highlight_df, aes(x = loc, y = BLUP, color=entry), position=position_jitterdodge(dodge.width = 1, jitter.width = 0.5)) + scale_color_manual(values = c("201" = "purple", "202"="orange"))

geom_point(data = highlight_df, aes(x = loc, y = BLUP, color=entry))
MyPlot <- qual_BLUP8 %>% ggplot(aes(x=loc, y=BLUP, fill=stage)) + geom_boxplot(alpha = 0.6, outlier.shape = NA)

MuPlot <- MyPlot + geom_pointline(data = highlight_df, aes(y=Optimum, colour="Optimum"), stroke="black") + scale_colour_manual(values('Optimum'='green'))



levels(qual_BLUP8$entry) 
# qual_BLUP8$entry <- as.factor(qual_BLUP8$entry)



qual_BLUP8 %>% ggplot(aes(x=loc,y=BLUP, fill=stage)) + geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) + xlab("Location") + facet_wrap(~stage) + theme(axis.text.x = element_text(angle = 45, hjust = 1))




myplot1 <- ggarrange(ST0_ADF, ST1_ADF, 
                     labels = c("a", "b"), ncol = 1, nrow = 2)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/yield_FD/figures/GWAS/MS_manhattan.png", plot = P1, dpi = 300, width = 8, height = 8)



qual_BLUP8 %>% group_by(stage) %>%
  ggplot(aes(x = loc, y = BLUP, fill = stage)) + 
  scale_fill_brewer(palette = "Blues") +
  geom_boxplot(position=position_dodge(0.8))+
  geom_point(data=subset(qual_BLUP8, entry == 201 | entry == 202), aes(x = loc, y = BLUP, color=entry), position=position_dodge(0.8), show.legend = T)

#subset(qual_BLUP8, entry == 201 | entry == 202)

library(ggstatsplot)


ggbetweenstats(data = qual_BLUP8, x = loc, y = BLUP,
               type = "robust", xlab = "Location", outlier.tagging = TRUE, outlier.label = gen, 
               outlier.coef     = 2, ggsignif.args    = list(textsize = 4, tip_length = 0.01),
               p.adjust.method  = "bonferroni",
               palette          = "default_jama",
               package          = "ggsci",
               plotgrid.args    = list(nrow = 1), ggtheme = ggplot2::theme_classic())

levels(qual_BLUP8$loc)
head(qual_BLUP8)
qual_BLUP8$loc <- as.factor(qual_BLUP8$loc)

ggbetweenstats(data = qual_BLUP8, x = loc, y = BLUP,
               label.var = gen, label.expression = entry == "201" | entry == "202",
               point.label.args = list(alpha = 0.7, size = 4, color = "grey50"))

ggbetweenstats(data = qual_BLUP8, x = loc, y = BLUP,
  plot.type = "box",
  type = "p",
  conf.level = 0.99,
  package = "ggsci",
  palette = "nrc_npg", 
  ggtheme = ggplot2::theme_classic(),
  outlier.tagging = TRUE,  outlier.label = gen)

ggsave(filename = "~/OneDrive - Washington State University (email.wsu.edu)/Sen_2020/quality_2022/ADF3.png", plot = P2, dpi = 300, width = 6, height = 4)


grouped_ggbetweenstats(data = qual_BLUP8, x = loc, y = BLUP,
  grouping.var = stage,
  xlab = "Env",
  ylab = "BLUP",
  pairwise.display = "significant", ## display only significant pairwise comparisons
  p.adjust.method = "fdr", ## adjust p-values for multiple tests using this method
  ggtheme = ggplot2::theme_classic(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
  outlier.label = gen,
  ## arguments relevant for combine_plots
  annotation.args = list(title = "ADF ST0 & ST1"),
  plotgrid.args = list(nrow = 2)
)



?ggbetweenstats
# label.var = title, ## variable to use for labeling data points
# label.expression = rating < 5 & budget > 100, ## expression for deciding which points to label
# point.label.args = list(alpha = 0.7, size = 4, color = "grey50"),


qual_BLUP9 <- qual_BLUP4[,c("gen", "ST0_ID_ADF","ST1_ID_ADF")]
qual_BLUP9 <- qual_BLUP4[,c("gen", "ST1_WA_ADF","ST1_ID_ADF")]
head(qual_BLUP9)


str(qual_BLUP9)
ggscatterstats(
  data = qual_BLUP9, ## dataframe from which variables are taken
  x = ST1_WA_ADF, ## predictor/independent variable
  y = ST1_ID_ADF, ## dependent variable
  label.var = gen, ## variable to use for labeling data points
  label.expression = gen == 201 | gen == 202, ## expression for deciding which points to label
  point.label.args = list(alpha = 0.7, size = 4, color = "grey50"),
  xfill = "#CC79A7", ## fill for marginals on the x-axis
  yfill = "#009E73", ## fill for marginals on the y-axis
)



