# Library
library(psych)
library(GPArotation)
library(clValid)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyverse)
library(car)
library(readxl)

#memanggil data
data = read_excel("E:/skripsyen/data bab 4 fix/tpak2018.xlsx")

#standarisasi data
datastand <- scale(data[,2:2])
datastand

# Hitung matriks jarak
jarak <- dist(data, method = "euclidean")
# Konversi matriks jarak ke data frame
jarak_df <- as.data.frame(as.matrix(jarak))
# Tampilkan data frame
print(jarak_df)

#hirarki ward
hierward <- hclust(dist(data[,2:2]), method = "ward.D")
#korelasi cophenetic
d1 <- dist(data[,2:2])
hc <- hclust(d1,"ward.D")
d2 <- cophenetic(hc)
corward <- cor(d1,d2)
corward

#dendogram
hierward <- hclust(dist(scale(data[,2:2])), method = "ward.D")
hierward
plot(hierward, labels = data$daerah, hang = 1, col = "black", 
     main = "Cluster Dendogram", sub = "dendogram 2018 ", 
     xlab = "kabupaten/kota", ylab = "Jarak")

#validitas klaster
data <- datastand
inval <- clValid(datastand, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "ward")
summary(inval)
