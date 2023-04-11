library(multcompView)

##################
# p_differences
names(ST1)
data <- ST1[[5]]
names(data)
data[["FD_cut_loc"]]


# p_differences 1-3 -------------------------------------------------------


class(ST1[[1]][[1]])
setwd("~/Documents/git/Dreger_2022/statistical_results/p_differences_1_3/")
ST6 <- list()

data <- ST1[[1]]
for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  ST5 <- list()
  for (j in 1:length(lev5[1:3])) {
    data1 <- data[[j]][[4]]
    pdif <- data1
    pdif[pdif < 0.001] <- "***"
    pdif[pdif > 0.05] <- "ns"
    pdif[pdif < 0.05 & pdif > 0.01] <- "*"
    pdif[pdif < 0.01 & pdif > 0.001] <- "**"
    pdif[upper.tri(pdif)] <- NA
    pdif <- reshape2::melt(pdif, na.rm = T)
    pdif <- pdif[,c(2,1,3)]
    head(pdif)
    colnames(pdif) <- c("A","B","p.signif")
    pdif <- pdif %>% unite("A_B", c(A, B), sep = " - ", remove = T)
    ST5[[length(ST5)+1]] <- pdif
  }
  names(ST5) <- lev5[1:3]
  
  wb <- createWorkbook()
  saveWorkbook(wb, paste0(names(ST1[i]), '.p.differences_melt.xlsx'))
  lapply(names(ST5), function(x) write.xlsx(ST5[[x]], paste0(names(ST1[i]), '.p.differences_melt.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))
  
  ST6[[length(ST6)+1]] <- ST5
}
names(ST6) <- names(ST1)


# p-signif cut or loc -----------------------------------------------------

ST7 <- list()
for (i in 1:length(ST1)) {
  data <- ST1[[1]]
  pdif <- data[[4]][[4]]
  
  pdif[pdif < 0.001] <- "***"
  pdif[pdif > 0.05] <- "ns"
  pdif[pdif < 0.05 & pdif > 0.01] <- "*"
  pdif[pdif < 0.01 & pdif > 0.001] <- "**"
  pdif[upper.tri(pdif)] <- NA
  pdif <- reshape2::melt(pdif, na.rm = T)
  pdif <- pdif[,c(2,1,3)]
  colnames(pdif) <- c("A","B","p.signif") 
  pdif$A <- paste0("'", pdif$A)
  
  pdif <- pdif %>% separate(2, c("FD1", "loc1"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD2", "loc2"), sep = ",", remove = T, convert = FALSE, extra = "merge")
  
  data1 <- pdif %>% dplyr::filter(loc2 == "WA") %>% dplyr::filter(loc1 == "WA") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(loc = "WA")
  
  data2 <- pdif %>% dplyr::filter(loc2 == "OR") %>% dplyr::filter(loc1 == "OR") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(loc = "OR")
  
  data3 <- pdif %>% dplyr::filter(loc2 == "ID") %>% dplyr::filter(loc1 == "ID") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(loc = "ID")
  data4 <- rbind(data3, data2, data1) %>% spread(key = "loc", value = "p.signif")
  
  ST7[[length(ST7)+1]] <- data4

}
names(ST7) <- names(ST1)


ST8 <- list()
for (i in 1:length(ST1)) {
  data <- ST1[[i]]
  pdif <- data[[5]][[4]]
  pdif[pdif < 0.001] <- "***"
  pdif[pdif > 0.05] <- "ns"
  pdif[pdif < 0.05 & pdif > 0.01] <- "*"
  pdif[pdif < 0.01 & pdif > 0.001] <- "**"
  pdif[upper.tri(pdif)] <- NA
  pdif <- reshape2::melt(pdif, na.rm = T)
  pdif <- pdif[,c(2,1,3)]
  colnames(pdif) <- c("A","B","p.signif") 
  pdif$A <- paste0("'", pdif$A)

  head(pdif)
  pdif <- pdif %>% separate(2, c("FD1", "cut1"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD2", "cut2"), sep = ",", remove = T, convert = FALSE, extra = "merge")
  
  data1 <- pdif %>% dplyr::filter(cut2 == "1") %>% dplyr::filter(cut1 == "1") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut = "cut1")

  data2 <- pdif %>% dplyr::filter(cut2 == "2") %>% dplyr::filter(cut1 == "2") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut = "cut2")

  data3 <- pdif %>% dplyr::filter(cut2 == "3") %>% dplyr::filter(cut1 == "3") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut = "cut3")

  data4 <- pdif %>% dplyr::filter(cut2 == "4") %>% dplyr::filter(cut1 == "4") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut = "cut4")

  data5 <- pdif %>% dplyr::filter(cut2 == "5") %>% dplyr::filter(cut1 == "5") %>% dplyr::select(1,3,5) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut = "cut5")

  data6 <- rbind(data1, data2,data3, data4, data5) %>% spread(key = "cut", value = "p.signif")
  
  ST8[[length(ST8)+1]] <- data6

}
names(ST8) <- names(ST1)

setwd("~/Documents/git/Dreger_2022/statistical_results/p_differences_4_5/")

for (i in 1:length(ST8)) {
  da3 <- inner_join(ST8[[i]], ST7[[i]], by = "FD")
  write.csv(da3, paste0(names(ST1[i]), '.p.differences_melt.csv'), row.names = F, quote = F)
}


# p-signif cut and loc ----------------------------------------------------

# ID

ST7 <- list()
for (n in 1:length(ST1)) {
  ST6 <- list()
  for (j in 1:4) {
    my_vec <- character()
    k <- paste0(j,",ID")
    for (i in 1:6) {
      l <- paste0(i,",",k)
      my_vec <- c(my_vec, l)
    }
    data <- ST1[[n]]
    pdif <- data[[6]][[4]]
    colnames(pdif)
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    pdif[pdif < 0.001] <- "***"
    pdif[pdif > 0.05] <- "ns"
    pdif[pdif < 0.05 & pdif > 0.01] <- "*"
    pdif[pdif < 0.01 & pdif > 0.001] <- "**"
    
    pdif[upper.tri(pdif)] <- NA
    pdif <- reshape2::melt(pdif, na.rm = T)
    # pdif <- pdif[,c(2,1,3)]
    colnames(pdif) <- c("FD_A","FD_B","p.signif")
    
    pdif <- pdif %>% separate(2, c("FD2", "cut2", "loc2"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD1", "cut1", "loc1"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% dplyr::select(1,4,7) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut_loc = paste0("cut",j,"_","ID"))
    
    ST6[[length(ST6)+1]] <- pdif
  }
  names(ST6) <- seq(1:4)
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut")
  ST6 <- ST6 %>% dplyr::select(-1) %>% spread(key = "cut_loc", value = "p.signif")
  ST7[[length(ST7)+1]] <- ST6
  
}
names(ST7) <- names(ST1)

# OR

ST8 <- list()
for (n in 1:length(ST1)) {
  ST6 <- list()
  for (j in 1:4) {
    my_vec <- character()
    k <- paste0(j,",OR")
    for (i in 1:6) {
      l <- paste0(i,",",k)
      my_vec <- c(my_vec, l)
    }
    data <- ST1[[1]]
    data <- ST1[[n]]
    pdif <- data[[6]][[4]]
    colnames(pdif)
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    pdif[pdif < 0.001] <- "***"
    pdif[pdif > 0.05] <- "ns"
    pdif[pdif < 0.05 & pdif > 0.01] <- "*"
    pdif[pdif < 0.01 & pdif > 0.001] <- "**"
    
    pdif[upper.tri(pdif)] <- NA
    pdif <- reshape2::melt(pdif, na.rm = T)
    # pdif <- pdif[,c(2,1,3)]
    colnames(pdif) <- c("FD_A","FD_B","p.signif")
    
    pdif <- pdif %>% separate(2, c("FD2", "cut2", "loc2"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD1", "cut1", "loc1"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% dplyr::select(1,4,7) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut_loc = paste0("cut",j,"_","OR"))
    
    ST6[[length(ST6)+1]] <- pdif
  }
  names(ST6) <- seq(1:4)
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut")
  ST6 <- ST6 %>% dplyr::select(-1) %>% spread(key = "cut_loc", value = "p.signif")
  ST8[[length(ST8)+1]] <- ST6
  
}
names(ST8) <- names(ST1)

# WA

ST9 <- list()
for (n in 1:length(ST1)) {
  ST6 <- list()
  for (j in 1:5) {
    my_vec <- character()
    k <- paste0(j,",WA")
    for (i in 1:6) {
      l <- paste0(i,",",k)
      my_vec <- c(my_vec, l)
    }
    data <- ST1[[n]]
    pdif <- data[[6]][[4]]
    colnames(pdif)
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    pdif[pdif < 0.001] <- "***"
    pdif[pdif > 0.05] <- "ns"
    pdif[pdif < 0.05 & pdif > 0.01] <- "*"
    pdif[pdif < 0.01 & pdif > 0.001] <- "**"
    
    pdif[upper.tri(pdif)] <- NA
    pdif <- reshape2::melt(pdif, na.rm = T)
    # pdif <- pdif[,c(2,1,3)]
    colnames(pdif) <- c("FD_A","FD_B","p.signif")
    
    pdif <- pdif %>% separate(2, c("FD2", "cut2", "loc2"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% separate(1, c("FD1", "cut1", "loc1"), sep = ",", remove = T, convert = FALSE, extra = "merge") %>% dplyr::select(1,4,7) %>% unite("FD", c(FD2, FD1), sep = " - ", remove = T) %>% mutate(cut_loc = paste0("cut",j,"_","WA"))
    
    ST6[[length(ST6)+1]] <- pdif
  }
  names(ST6) <- seq(1:4)
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut")
  ST6 <- ST6 %>% dplyr::select(-1) %>% spread(key = "cut_loc", value = "p.signif")
  ST9[[length(ST9)+1]] <- ST6
  
}
names(ST9) <- names(ST1)

setwd("~/Documents/git/Dreger_2022/statistical_results/p_differences_6/")

for (i in 1:length(ST1)) {
  ST6 <- ST7[[i]] %>% inner_join(., ST8[[i]]) %>% inner_join(., ST9[[i]])
  
  write.csv(ST6, paste0(names(ST1[i]), '.p.differences_melt.csv'), row.names = F, quote = F)
}


wb <- createWorkbook()
saveWorkbook(wb, paste0(names(ST1[i]), '.p.differences_melt.xlsx'))
lapply(names(ST6), function(x) write.xlsx(ST6[[x]], paste0(names(ST1[i]), '.p.differences_melt.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))


# blup_CLD ----------------------------------------------------------------

# ID
# ST1 = ST05

ST7 <- list()
for (n in 1:length(ST05)) {
  ST6 <- list()
  for (j in 1:4) {
    my_vec <- character()
    k <- paste0(j,",ID")
    for (i in 1:6) {
      l <- paste0(i,",",k)
      my_vec <- c(my_vec, l)
    }
    data <- ST05[[n]]
    df_total = data.frame()
    pdif <- data[[4]][[4]] # p.dif
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    
    data2 <- data[[4]][[1]] # blup
    data2 <- data2 %>% dplyr::select(1:4) %>% dplyr::filter(cut == j & loc == "ID")
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
    
    ST6[[length(ST6)+1]] <- df_total
  }
  names(ST6) <- seq(1:4)
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut1")
  ST6 <- ST6 %>% dplyr::select(-1)
  ST6$loc <- "ID"
  ST7[[length(ST7)+1]] <- ST6
  
}
names(ST7) <- names(ST05)
ST7 <-rbindlist(ST7, use.names=TRUE, fill=TRUE, idcol="trait")
ST7 <- ST7 %>% spread(key = "trait", value = "BLUP")

# OR

ST8 <- list()
for (n in 1:length(ST05)) {
  ST6 <- list()
  for (j in 1:4) {
    my_vec <- character()
    k <- paste0(j,",OR")
    for (i in 1:6) {
      l <- paste0(i,",",k)
      my_vec <- c(my_vec, l)
    }

    data <- ST05[[n]]
    df_total = data.frame()
    pdif <- data[[4]][[4]] # p.dif
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    
    data2 <- data[[4]][[1]] # blup
    data2 <- data2 %>% dplyr::select(1:4) %>% dplyr::filter(cut == j & loc == "OR")
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
    
    ST6[[length(ST6)+1]] <- df_total
  }
  names(ST6) <- seq(1:4)
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut1")
  ST6 <- ST6 %>% dplyr::select(-1)
  ST6$loc <- "OR"
  ST8[[length(ST8)+1]] <- ST6
  
}
names(ST8) <- names(ST05)
ST8 <-rbindlist(ST8, use.names=TRUE, fill=TRUE, idcol="trait")
ST8 <- ST8 %>% spread(key = "trait", value = "BLUP")

# WA
ST9 <- list()
for (n in 1:length(ST05)) {
  ST6 <- list()
  for (j in 1:4) {
    my_vec <- character()
    k <- paste0(j,",WA")
    for (i in 1:6) {
      l <- paste0(i,",",k)
      my_vec <- c(my_vec, l)
    }
    data <- ST05[[n]]
    df_total = data.frame()
    pdif <- data[[4]][[4]] # p.dif
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    
    data2 <- data[[4]][[1]] # blup
    data2 <- data2 %>% dplyr::select(1:4) %>% dplyr::filter(cut == j & loc == "WA")
    
    let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    let2 <- as.data.frame(print(let1)) %>% rownames_to_column("cut")
    let2 <- let2[,c(1,2)]
    colnames(let2) <- c("cut", "Letters")
    let2 <- let2 %>% separate(1, into = c("FD","cut","loc"), remove = T)
    data2$predicted.value <- round(data2$predicted.value, 2)
    
    blup <- inner_join(data2, let2, by = c("FD","cut","loc"))
    blup <- blup %>% unite("BLUP", 4:5, sep = " ", remove = T)
    blup <- blup[,-3]
    df_total <- rbind(df_total,blup)
    
    ST6[[length(ST6)+1]] <- df_total
  }
  names(ST6) <- seq(1:4)
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut1")
  ST6 <- ST6 %>% dplyr::select(-1)
  ST6$loc <- "WA"
  ST9[[length(ST9)+1]] <- ST6
  
}
names(ST9) <- names(ST05)

ST9 <-rbindlist(ST9, use.names=TRUE, fill=TRUE, idcol="trait")
ST9 <- ST9 %>% spread(key = "trait", value = "BLUP")


ST10 <- rbind(ST7, ST8, ST9)
ST10 <- ST10[order(ST10$loc, ST10$cut, ST10$FD), ]

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")
write.csv(ST10, "FD.cut.loc.csv", quote = F, row.names = F)

# ST6 <- list()
# for (i in 1:length(ST1)) {
#   data1 <- rbind(ST7[[i]], ST8[[i]], ST9[[i]])
#   data1 <- data1 %>% relocate("loc", .before = "FD")
#   ST6[[length(ST6)+1]] <- data1
#   
# }

names(ST6) <- names(ST1)
setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")

wb <- createWorkbook()
saveWorkbook(wb, paste0(names(ST6[i]), '.dreger.FD.cut.loc.xlsx'))
lapply(names(ST6), function(x) write.xlsx(ST6[[x]], paste0(names(ST6[i]), '.dreger.FD.cut.loc.xlsx'), sheetName=x, append=T, row.names=F, showNA = F))



# CDL FD_loc --------------------------------------------------------------

list3 <- c("ID","OR","WA")
ST9 <- list()
for (n in 1:length(ST05)) {
  ST6 <- list()
  for (j in 1:length(list3)) {
    my_vec <- character()
    for (i in 1:6) {
      l <- paste0(i,",",list3[j])
      my_vec <- c(my_vec, l)
    }
    data <- ST05[[n]]
    df_total = data.frame()
    
    data2 <- data[[2]][[1]] # blup
    data2 <- data2 %>% dplyr::select(1:3) %>% dplyr::filter(loc == list3[j])
    data2$predicted.value <- round(data2$predicted.value, 2)
    
    pdif <- data[[2]][[4]] # p.dif
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    let2 <- as.data.frame(print(let1)) %>% rownames_to_column("cut")
    let2 <- let2[,c(1,2)]
    colnames(let2) <- c("cut", "Letters")
    let2 <- let2 %>% separate(1, into = c("FD","loc"), remove = T)
    
    blup <- inner_join(data2, let2, by = c("FD","loc"))
    blup <- blup %>% unite("BLUP", 3:4, sep = " ", remove = T)
    #  blup <- blup[,-2]
    
    df_total <- rbind(df_total,blup)
    ST6[[length(ST6)+1]] <- df_total
  }
  names(ST6) <- list3
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut1")
  ST6 <- ST6 %>% dplyr::select(-1)
  ST9[[length(ST9)+1]] <- ST6
}
names(ST9) <- names(ST05)
ST9 <-rbindlist(ST9, use.names=TRUE, fill=TRUE, idcol="trait")
ST9 <- ST9 %>% spread(key = "trait", value = "BLUP")
ST9 <- ST9[order(ST9$loc, ST9$FD), ]

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")
write.csv(ST9, "FD.loc.csv", quote = F, row.names = F)


# FD_cut ------------------------------------------------------------------


list3 <- c(1:4)
ST9 <- list()
for (n in 1:length(ST05)) {
  ST6 <- list()
  for (j in 1:length(list3)) {
    my_vec <- character()
    for (i in 1:6) {
      l <- paste0(i,",",list3[j])
      my_vec <- c(my_vec, l)
    }
    data <- ST05[[n]]
    df_total = data.frame()
    
    data2 <- data[[3]][[1]] # blup
    data2 <- data2 %>% dplyr::select(1:3) %>% dplyr::filter(cut == list3[j])
    data2$predicted.value <- round(data2$predicted.value, 2)
    
    pdif <- data[[3]][[4]] # p.dif
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    let2 <- as.data.frame(print(let1)) %>% rownames_to_column("cut")
    let2 <- let2[,c(1,2)]
    colnames(let2) <- c("cut", "Letters")
    let2 <- let2 %>% separate(1, into = c("FD","cut"), remove = T)
    
    blup <- inner_join(data2, let2, by = c("FD","cut"))
    blup <- blup %>% unite("BLUP", 3:4, sep = " ", remove = T)
    #  blup <- blup[,-2]
    
    df_total <- rbind(df_total,blup)
    ST6[[length(ST6)+1]] <- df_total
  }
  names(ST6) <- list3
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut1")
  ST6 <- ST6 %>% dplyr::select(-1)
  # ST6 <- ST6 %>% dplyr::select(-1) %>% spread(key = "cut", value = "BLUP")
  ST9[[length(ST9)+1]] <- ST6
}
names(ST9) <- names(ST05)

ST9 <-rbindlist(ST9, use.names=TRUE, fill=TRUE, idcol="trait")

ST9 <- ST9 %>% spread(key = "trait", value = "BLUP")
ST9 <- ST9[order(ST9$cut, ST9$FD), ]

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")
write.csv(ST9, "FD.cut.csv", quote = F, row.names = F)


# FD ----------------------------------------------------------------------


ST9 <- list()
for (n in 1:length(ST05)) {
    data <- ST05[[n]]
    
    data2 <- data[[1]][[1]] # blup
    data2 <- data2 %>% dplyr::select(1:2)
    data2$predicted.value <- round(data2$predicted.value, 2)
    
    pdif <- data[[1]][[4]] # p.dif
    let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    let2 <- as.data.frame(print(let1)) %>% rownames_to_column("FD")
    let2 <- let2[,c(1,2)]
    colnames(let2) <- c("FD", "Letters")
    
    blup <- inner_join(data2, let2, by = "FD")
    blup <- blup %>% unite("BLUP", 2:3, sep = " ", remove = T)
 
    
  ST9[[length(ST9)+1]] <- blup
}
names(ST9) <- names(ST05)

ST9 <-rbindlist(ST9, use.names=TRUE, fill=TRUE, idcol="trait")

ST9 <- ST9 %>% spread(key = "trait", value = "BLUP")

setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")
write.csv(ST9, "FD.csv", quote = F, row.names = F)


# CDL FD_loc_date ---------------------------------------------------------


head(b2)
b2$cons_days <- as.factor(b2$cons_days)
b2 <- b2 %>% unite("harv_loc", c(9,3), sep = ",", remove = F)

list3 <- b2$harv_loc

ST9 <- list()
for (n in 1:length(ST1)) {
  ST6 <- list()
  for (j in 1:length(list3)) {
    my_vec <- character()
    df_total = data.frame()
    for (i in 1:6) {
      l <- paste0(i,",",list3[j])
      my_vec <- c(my_vec, l)
    }
    data <- ST1[[n]]
    
    data2 <- data[[7]][[1]] # blup
    data2$predicted.value <- round(data2$predicted.value, 2)
    data2 <- data2 %>% unite("harv_loc", 2:3, sep = ",", remove = F)
    data2 <- inner_join(data2, b2, by = c("cons_days","loc","harv_loc"))
#    data2$cons_days <- as.numeric(as.character(data2$cons_days))
    data2 <- data2 %>% dplyr::select(c("FD","predicted.value","cons_days","harv_loc","env")) %>% dplyr::filter(harv_loc == list3[j])
    
   
    pdif <- data[[7]][[4]] # p.dif
    pdif <- pdif[rownames(pdif) %in% my_vec, colnames(pdif) %in% my_vec]
    let1 <- multcompLetters(pdif, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    let2 <- as.data.frame(print(let1)) %>% rownames_to_column("cut")
    let2 <- let2[,c(1,2)]
    colnames(let2) <- c("cut", "Letters")
    
    let2 <- let2 %>% separate(1, into = c("FD","harv_loc"), remove = T, extra = "merge")
    
    blup <- inner_join(data2, let2, by = c("FD","harv_loc"))
    blup <- blup %>% dplyr::select(c("FD","predicted.value","Letters","env")) %>% unite("BLUP", 2:3, sep = " ", remove = T)
    
    df_total <- rbind(df_total,blup)
    ST6[[length(ST6)+1]] <- df_total
  }
  
  ST6 <-rbindlist(ST6, use.names=TRUE, fill=TRUE, idcol="cut1")
  ST6 <- ST6 %>% dplyr::select(-1) %>% spread(key = "env", value = "BLUP")
  
  ST9[[length(ST9)+1]] <- ST6
}

names(ST9) <- names(ST1)
ST9 <-rbindlist(ST9, use.names=TRUE, fill=TRUE, idcol="trait")


setwd("~/Documents/git/Dreger_2022/statistical_results/predictions/")
write.csv(ST9, 'dreger.FD.env.csv', row.names = F, quote = F)
