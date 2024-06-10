# Load required packages
library(readxl)
library(cluster)
library(factoextra)
library(gridExtra)
library(NbClust)

# Read the data
mall_customers <- read_excel("Mall_Customers-1.xls")
mall_customers

# Subset the clustering variables
df <- mall_customers[, c("Age", "Income", "Spend_Score")]

# Hierarchical Clustering
d <- dist(df, method = "euclidean")  # Compute distance matrix

# Try different linkage methods
m <- c("average", "single", "complete", "ward")
ac <- sapply(m, function(x) agnes(df, method = x)$ac)  # Compute agglomerative coefficients

# Determine optimal number of clusters
res.nbclust <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index = "all")
optimal_clusters <- res.nbclust$Best.nc[1, 1]

# Perform hierarchical clustering using the optimal method
hc <- hclust(d, method = "ward.D2")  # Use ward.D2 as it tends to work well
groups <- cutree(hc, k = optimal_clusters)  # Cut dendrogram into clusters

# Analyze cluster characteristics
cluster_means <- aggregate(df, by = list(Cluster = groups), FUN = mean)
print(cluster_means)

# K-Means Clustering
set.seed(123)  # For reproducibility
kclusters <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
optimal_k <- kclusters$Best.nc[1, 1]

final_kmeans <- kmeans(df, centers = optimal_k, nstart = 25)
fviz_cluster(final_kmeans, data = df)  # Visualize clusters

# Profile clusters using ANOVA
anova_results <- aov(final_kmeans$cluster ~ Age + Income + Spend_Score, data = mall_customers)
summary(anova_results)

# Write cluster descriptions
for (i in 1:optimal_k) {
  cat(paste0("Cluster ", i, ":\n"))
  cluster_data <- mall_customers[final_kmeans$cluster == i, c("Age", "Income", "Spend_Score")]
  print(summary(cluster_data))
  cat("\n")
}

