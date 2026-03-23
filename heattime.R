# Load the necessary packages
#library(heatmap)
# load required packages
library(ggplot2)
library(reshape2)
# Read the data from the file
file_path <- "E:/Mahmoud/Work/NohaOsman/heattime/heat_map_data_group_time.unknown"  # Update with the actual file path

#time_matrix <- data_matrix <- as.matrix(read.table(file_path, header = TRUE, row.names = 1))

#time_sample_matrix <- time_matrix[1:2,]

# Get unique values
#unique_values <- unique(as.vector(time_sample_matrix))

# Create a mapping between unique values and unique numeric labels
#value_to_label <- setNames(seq_along(unique_values), unique_values)

# Convert the matrix to numeric labels
#numeric_labels_matrix <- sapply(time_sample_matrix, function(x) value_to_label[x])

# Remove row and column names from the numeric labels matrix
#rownames(numeric_labels_matrix) <- NULL
#colnames(numeric_labels_matrix) <- NULL

#numeric_labels_matrix <- matrix(numeric_labels_matrix, nrow = 2, byrow = TRUE)


# Create the heatmap using base R heatmap
#heatmap(numeric_labels_matrix, 
#        col = colorRampPalette(c("blue", "white", "red"))(100),
#        main = "Heatmap",
#        xlab = "Sample",
#        ylab = "Time")

data_matrix <- as.matrix(read.table(file_path, header = TRUE, row.names = 1))

# Convert the data to numeric (handling non-numeric entries)
#data_matrix <- apply(data_matrix, 2, as.numeric)

data_matrix <- data_matrix[-c(1, 2), ]
#rownames(data_matrix) <- NULL
#colnames(numeric_labels_matrix) <- NULL
# Replace the first two rows in data_matrix_values with numeric_labels_matrix
#data_matrix_values <- data_matrix
#data_matrix_values[1:2, ] <- numeric_labels_matrix

#print(data_matrix_values)

# Remove row names
#rownames(data_matrix_values) <- NULL

#data_matrix
#numeric_data <- as.numeric(data_matrix)
# convert data to long format
melted_data <- melt(data_matrix)
head(data_matrix)
# Create the clustermap
heatmap(numeric_data, 
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Clustermap",
         cluster_rows = TRUE,
         cluster_cols = FALSE,
         breaks = seq(-5, 5, length.out = 101)
        )
