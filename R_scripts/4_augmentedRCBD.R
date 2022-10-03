install.packages('augmentedRCBD', dependencies=TRUE)
library(augmentedRCBD)

a1 <- read.csv("~/Documents/git/Dreger_2022/raw_data/example_Scoot.csv")
colnames(a1)
lev1 <- c("Entry","Block")
lev2 <- c("Sibley","Hardin","Weber","Kato")
a1[,lev1] <- lapply(a1[,lev1], factor)
levels(a1$Entry)

# Example data
blk <- c(rep(1,7),rep(2,6),rep(3,7))
trt <- c(1, 2, 3, 4, 7, 11, 12, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 8, 6, 10)
y1 <- c(92, 79, 87, 81, 96, 89, 82, 79, 81, 81, 91, 79, 78, 83, 77, 78, 78,
        70, 75, 74)
y2 <- c(258, 224, 238, 278, 347, 300, 289, 260, 220, 237, 227, 281, 311, 250,
        240, 268, 287, 226, 395, 450)
data <- data.frame(blk, trt, y1, y2)
# Convert block and treatment to factors
data$blk <- as.factor(data$blk)
data$trt <- as.factor(data$trt)
str(data)
# Results for variable y1 (checks inferred)

out1 <- augmentedRCBD(data$blk, data$trt, data$y1, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE)


out2 <- augmentedRCBD(a1$Block, a1$Entry, a1$Yi, method.comp = "lsd",
                      alpha = 0.05, group = TRUE, console = TRUE)

out2$Means


