library(tidyverse)
library(cluster)
library(plotly)

# helpers -----------------------------------------------------------------
elbow_plot <- function(data, k_values, k_means_results = NULL) {
  wss <- numeric(length(k_values))  # Initialize vector for Within-Cluster-Sum of Squared Errors
  
  for (k in k_values) {
    if (is.null(k_means_results)) {
      kmeans_result <- kmeans(data, centers = k, nstart = 10)
    } else {
      kmeans_result <- k_means_results[[k - 1]]
    }
    wss[k - 1] <- sum(kmeans_result$withinss)  # Store WSS for each k
  }
  
  # Plot the elbow method
  plot(
    k_values, wss, type = "b", pch = 19, frame = FALSE,
    xlab = "Number of Clusters (k)", ylab = "Within-Cluster-Sum of Squared Errors",
    main = "WSS"
  )
}

# Function for the Silhouette Method
silhouette_plot <- function(
  data, k_values, k_means_results = NULL, dist = dist(data)
) {
  silhouette_scores <- numeric(length(k_values))
  
  for (k in k_values) {
    print(k)
    if (is.null(k_means_results)) {
      kmeans_result <- kmeans(data, centers = k, nstart = 10)
    } else {
      kmeans_result <- k_means_results[[k - 1]]
    }
    silhouette_scores[k - 1] <- mean(
      silhouette(kmeans_result$cluster, dist)[,3] # third column is the silhouette width
    )
  }
  
  # Plot silhouette scores
  plot(
    k_values, silhouette_scores, type = "b", pch = 19, frame = FALSE,
    xlab = "Number of Clusters (k)", ylab = "Average Silhouette Width",
    main = "Silhouette Width"
  )
}

extract_vectors <- function(vector_char) {
  map(vector_char, function(vc) {
    vc %>%
      str_remove_all("\n") %>%
      str_remove("\\[") %>%
      str_remove("\\]") %>%
      trimws() %>%
      str_replace_all("\\s+", " ") %>%
      str_split(" ") %>%
      .[[1]] %>%
      as.numeric()
  })
}

# run ---------------------------------------------------------------------
capability_vectors <- read_csv("results/capability_vectors.csv")
esco_vectors <- read_csv("results/skill_vectors.csv")

capability_vectors <- capability_vectors %>%
  distinct(capability, .keep_all = TRUE) %>%
  mutate(
    vector_nums = extract_vectors(vector)
  ) %>%
  select(-vector) %>%
  unnest(vector_nums) %>%
  group_by(capability) %>%
  mutate(names = paste0("v", 1:n())) %>%
  pivot_wider(
    id_cols = capability, 
    names_from = names, 
    values_from = vector_nums
  )

esco_vectors <- esco_vectors %>%
  distinct(skill, .keep_all = TRUE) %>%
  mutate(
    vector_nums = extract_vectors(vector)
  ) %>%
  select(-vector) %>%
  unnest(vector_nums) %>%
  group_by(skill) %>%
  mutate(names = paste0("v", 1:n())) %>%
  pivot_wider(
    id_cols = skill, 
    names_from = names, 
    values_from = vector_nums
  )  

# choose k for k-means
set.seed(5005)
k_values <- 2:50
k_means_list <- list()
tictoc::tic()
for (k in k_values) {
  print(k)
  k_means_list[[k - 1]] <- kmeans(
    capability_vectors[, -1], centers = k, nstart = 10
  )
}
tictoc::toc()

dataDist = dist(capability_vectors[,-1])
elbow_plot(capability_vectors, 2:50, k_means_list)
silhouette_plot(
  capability_vectors, 2:50, k_means_list, 
  dist = dataDist
)

k_means_result <- k_means_list[[7]] # k = 8 based on elbow plot and silhouette score

# plot low dimensional representation
capability_vectors$cluster <- as.factor(k_means_result$cluster)
pca_result <- capability_vectors %>%
  ungroup() %>%
  select(-c(cluster,capability)) %>%
  prcomp(center = TRUE, scale. = TRUE)

# select first 3 principal components
pca_result_df <- pca_result$x[,1:3] %>%
  as_tibble() %>%
  mutate(cluster = capability_vectors$cluster)

# plot clusters
pca_result_df %>%
  ggplot(aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Clusters of Capability Vectors")

# plot 3D
plot_ly(data = pca_result_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster, colors = RColorBrewer::brewer.pal(8, "Set1"), type = "scatter3d", mode = "markers") %>%
  layout(title = "3D Clusters of Capability Vectors", scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  ))


# connect centroids to skills ---------------------------------------------
centroids <- k_means_result$centers %>%
  as_tibble() %>%
  mutate(cluster = 1:nrow(.))

skill_matrix <- esco_vectors %>%
  ungroup() %>%
  select(-skill) %>%
  as.matrix()

skills_by_cluster <- tibble()

n_skills <- 1

for (i in 1:nrow(centroids)) {
  centroid <- as.numeric(centroids[i, -ncol(centroids)])
  norm_centroid <- centroid / sqrt(sum(centroid^2))
  
  # calculate dot product between centroid and each skill
  dotprods <- skill_matrix %*% as.matrix(centroid) 
  
  # normalize by skill and centroid
  norm_dotprods <- dotprods / (sqrt(rowSums(skill_matrix^2)) * sqrt(sum(centroid^2)))
  
  # get top N skills
  skills_by_cluster <- bind_rows(
    skills_by_cluster,
    tibble(
      cluster = centroids$cluster[i],
      skill = esco_vectors$skill[order(norm_dotprods, decreasing = TRUE)[1:n_skills]]
    )
  )
}
