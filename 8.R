library(rattle)
library(factoextra)
library(cluster)
library(ggplot2)

normalize <- function(data){
  (data - min(data))/(max(data)-min(data))
}


wine <- wine
wine_data <- wine[, -1]            
wine_norm <- as.data.frame(lapply(wine_data, normalize))

wine_pca <- prcomp(wine_norm, scale. = TRUE)
wine_data <- as.data.frame(wine_pca$x[,1:2])

fviz_nbclust(wine_data,kmeans,method = "wss")
fviz_nbclust(wine_data,kmeans,method = "silhouette")


set.seed(100)

wine_kmeans <- kmeans(wine_data,centers = 3,nstart = 25)
wine_data$cluster <- as.factor(wine_kmeans$cluster)


ggplot(wine_data, aes(x = PC1, y = PC2, color = cluster))+
  geom_point(size=3)
