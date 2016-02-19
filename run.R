# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
  if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
  dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)


## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
  result <- paste(x1,x2,sep="")
  return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
  if (x %in% rownames(installed.packages())) { 
    print(concat("package already installed: ", x))
  }
  else { 
    install.packages(x) 
  }
  library(x, character.only=TRUE)
}

# get some data
data <- mtcars
# how many are n/a?
sum(is.na(data))
head(which(is.na(data)))
# how many are NULL?
sum(is.null(data))
# how many are blank?
length(which(data == ""))
str(data)
data$cyl <- as.factor(data$cyl)

# PCA
pca <- prcomp(data[,-2], center=TRUE, scale=TRUE)
prop.pca <- pca$sdev^2/sum(pca$sdev^2)

# Linear Discriminant Analysis with Jacknifed Prediction
load_package("MASS")
lda <- lda(cyl ~ mpg + disp + hp + drat + wt, data=data, prior = c(1,1,1)/3)
prop.lda = lda$svd^2/sum(lda$svd^2)
plda <- predict(object = lda,
                newdata = data)
dataset = data.frame(cyl = data[,"cyl"],
                     pca = pca$x, lda = plda$x)

load_package("ggplot2")
load_package("scales")
p1 <- ggplot(dataset) + 
  geom_point(aes(lda.LD1, lda.LD2, col = cyl, shape = cyl), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

p2 <- ggplot(dataset) + 
  geom_point(aes(pca.PC1, pca.PC2, col = cyl, shape = cyl), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

load_package("gridExtra")
grid.arrange(p1, p2)

# Exploratory Graph for LDA or QDA
load_package("klaR")
par(mfrow=c(1,1))
partimat(cyl ~ mpg + disp + hp + drat + wt,data=data,method="lda")

# Scatterplot for 3 Group Problem 
pairs(data[c("mpg","disp","hp","wt","drat")], 
      main="Scatterplot Matrix for MTCARS Data", 
      pch=22, 
      bg=c("red", "yellow", "blue")[unclass(data$cyl)])
plot(lda)
