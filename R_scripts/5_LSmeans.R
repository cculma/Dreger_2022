library(xlsx)
library(reshape2)
library(asremlPlus)

names(ST01)
data <- ST01[[1]] # ADF

data <- ST01[[16]] # starch

lev2 <- colnames(data)[c(1:5,9,10)]
data <- as.data.frame(data)
data[,lev2] <- lapply(data[,lev2], factor)
head(data)
str(data)

lev6 <- c("FD","loc","year","cut",
          "FD:loc","FD:year","FD:cut","loc:year","loc:cut","year:cut",
          "FD:loc:year","FD:loc:cut","loc:year:cut","FD:loc:year:cut")

lev5 <- c("FD","loc","year","cut",
          "FD_loc","FD_year","FD_cut","loc_year","loc_cut","year_cut",
          "FD_loc_year","FD_loc_cut","loc_year_cut","FD_loc_year_cut")


# A01 = FD
# A02 = loc
# A03 = year
# A04 = cut
# A05 = FD:loc
# A06 = FD:year
# A07 = FD:cut
# A08 = loc:year
# A09 = loc:cut
# A10 = year:cut
# A11 = FD:loc:year
# A12 = FD:loc:cut
# A13 = FD:year:cut
# A14 = loc:year:cut
# A15 = FD:loc:year:cut

# 4 tables single factor 
# plus 6 tables two interactions
# plus 4 tables three interactions
# plus 1 table of four interactions
# names(ST01) # 18 analytes
# plus TTNDFD, this trait is in a single cut by year.
# BLUP1 <- predict.asreml(US, classify='gen:env', vcov=TRUE, )$pvals
# BLUP1 <- BLUP1[,c(1:3)] %>% inner_join(., FD2, by = "gen") %>% separate(2, c("loc", "year", "cut"), sep = "_", remove = F, convert = FALSE, extra = "merge")

US <- asreml::asreml(fixed = predicted.value ~ FD * loc ,
                     random = ~ idv(env):id(gen) + year + cut,
                     data = data, na.action = list(x = "include", y = "include"),
                     weights = weight, family = asreml::asr_gaussian(dispersion = 1))

current.asrt <- as.asrtests(US, NULL, NULL)
current.asrt <- rmboundary.asrtests(current.asrt)
rm(current.asrt)


diffs <- predictPlus(classify = lev6[3], 
                     asreml.obj = US, 
                     wald.tab = current.asrt$wald.tab, 
                     present = c("env","FD","gen","loc","year","cut"))


# ggplot(data, aes(x = loc, y = predicted.value, fill =loc)) + geom_boxplot() + theme_ipsum(base_family = "Arial", base_size = 12)  

summary(US)

diff1 <- list()
for (j in 1:length(lev6)) {
  diffs <- predictPlus(classify = lev6[j], 
                       asreml.obj = US, 
                       wald.tab = current.asrt$wald.tab, 
                       present = c("FD","gen","loc","year","cut"))
  diff1[[length(diff1)+1]] <- diffs
}
length(diff1) 
names(diff1) <- lev5

diff2 <- list()
for (i in 1:length(diff1)) {
  R3 <- as.data.frame(diff1[[i]]$p.differences)
  diff2[[length(diff2)+1]] <- R3
}
length(diff2)
names(diff2) <- lev5


diff3 <- list()
for (i in 1:length(diff1)) {
  R3 <- as.data.frame(diff1[[i]]$predictions)
  diff3[[length(diff3)+1]] <- R3
}
length(diff3)
names(diff3) <- lev5

diff4 <- list(diff2, diff3)
names(diff4) <- c("p.differences","predictions")

R6 <- wald.asreml(US)
R7 <- summary(US)$varcomp
current.asrt <- rmboundary.asrtests(current.asrt)
current.asrt$asreml.obj


R1 <- as.data.frame(diffs$predictions)
R2 <- as.data.frame(diffs$differences)
R3 <- as.data.frame(diffs$p.differences)
R4 <- as.data.frame(diffs$sed)

class(R2)
class(R1)


write.xlsx(R1, file="~/Documents/git/Dreger_2022/statistical_results/ADF.p.differences.xlsx", sheetName="predictions", row.names=F)
for (variable in vector) {
  
}

?write.xlsx
write.xlsx(R4, file="~/Documents/git/Dreger_2022/statistical_results/assay.xlsx", sheetName="differences", append=T, row.names=T, showNA = F)
write.xlsx(R3, file="~/Documents/git/Dreger_2022/statistical_results/assay.xlsx", sheetName="p.differences", append=T, row.names=T)
write.xlsx(R4, file="~/Documents/git/Dreger_2022/statistical_results/assay.xlsx", sheetName="diffs$sed", append=T, row.names=T)


setwd("~/Documents/git/Dreger_2022/statistical_results/")
library(dplyr)
library(xlsx)
example <- data.frame('Group' = c('building 1', 'building 1', 
                                  'building 2', 'building 2'),
                      'Subgroup' = c('Active','Inactive','Active','Inactive'),
                      'Value' = c('abc','def','ghi','jkl'))

E1 <- example %>% 
  mutate_if(is.factor, as.character) %>% 
  split(., .$Group) %>% 
  lapply(., function(x) split(x, x$Subgroup)) %>% 
  lapply(., function(dat) 
    lapply(dat, function(dat.sub) 
      write.xlsx(dat.sub, 
                 file = paste0(as.character(unique(dat.sub$Group)), ".xlsx"),
                 sheetName = paste0(as.character(unique(dat.sub$Subgroup))), 
                 append = TRUE, row.names = FALSE)))

E1$
lapply(E1, function(dat) 
  lapply(dat, function(dat.sub) 
    write.xlsx(dat.sub, 
               file = paste0(as.character(unique(dat.sub$Group)), ".xlsx"),
               sheetName = paste0(as.character(unique(dat.sub$Subgroup))), 
               append = TRUE, row.names = FALSE)))
E1$`building 1`$Active

diff3$
lapply(diff4, function(dat) 
  lapply(dat, function(dat.sub) 
    write.xlsx(dat.sub, 
               file = paste0(as.character(unique(dat.sub$p.differences)), ".xlsx"),
               sheetName = paste0(as.character(unique(dat.sub$Subgroup))), 
               append = TRUE, row.names = FALSE)))



R2 <- melt(diffs$p.differences)
class(R2)
R3 <- R2 %>% dplyr::filter(value < 0.05)

R3 <- as.data.frame(diffs$p.differences)

for (i in 1:length(diff2)) {
  write.xlsx(diff2[i,,], file="table.xlsx", sheetName = countrylist[i],
             col.names=FALSE, row.names=FALSE, append = T)  
}
names(diff2)

lev5
wb <- createWorkbook()
saveWorkbook(wb, 'ADF_output.xlsx')
lapply(names(diff2), function(x) write.xlsx(diff2[[x]], 'output.xlsx', sheetName=x, append=T, row.names=T, showNA = F))

# blue <- predict(US, classify="FD", levels=levels(data1$FD), vcov=TRUE,aliased = T) # get the lsmeans


