# Load necessary libraries
library(ggplot2)
library(reshape2)

# Read the data from the file (replace with the actual file path)
file_path <- "heat_map_data_group_time.unknown"
data_matrix <- as.matrix(read.table(file_path, header = TRUE, row.names = 1))

# Exclude the first two rows as they contain group and time information
data_matrix <- data_matrix[-c(1, 2), ]

# Convert data to numeric
data_matrix <- apply(data_matrix, 2, as.numeric)

# Create a row dendrogram based on "group"
group_dendro <- as.dendrogram(hclust(dist(data_matrix, method = "euclidean")))

# Create the heatmap with clustering by rows (group) and columns (time)
heatmap(
  data_matrix,
  col = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Clustered Heatmap",
  Rowv = group_dendro,  # Cluster rows by time
  Colv = NA,            # Do not cluster columns by time
  xlab = "Sample",
  ylab = "group",
  labRow = NULL,  # Row labels (group, time)
  labCol = NULL  # No column labels for this example
)
