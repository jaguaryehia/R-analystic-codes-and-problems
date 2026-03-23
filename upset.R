library(RColorBrewer)
library(UpSetR)

extract_genes <- function(files) {
  gene_sets <- list()  # Initialize an empty list to store gene sets for each file
  colors <- brewer.pal(3, "Set1")  # Generate 3 unique colors
  color_map <- setNames(colors, sapply(files, function(file) sub("\\.txt$", "", basename(file))))  # Create color map
  
  for (i in 1:length(files)) {
    file <- files[i]
    file_name <- sub("\\.txt$", "", basename(file))  # Extract the file name without the extension
    data <- read.table(file, header = TRUE, sep = "\t")
    gene_vector <- data$Gene  # Extract the gene vector
    gene_sets[[file_name]] <- gene_vector  # Store the gene vector as a list of genes
    
    # Cycle through the colors for each file
    color_map[[file_name]] <- colors[i %% 3 + 1]
  }
  
  return(list(gene_sets = gene_sets, color_map = color_map))
}

# Example usage:
path <- "E:/Year 4/New folder (2)/New folder/upset"
file_names <- list.files(path, pattern = "\\.txt$", full.names = TRUE)
result <- extract_genes(file_names)

# Retrieve the colors in the order of set names and sort them
sorted_colors <- result$color_map[order(names(result$gene_sets))]

# Extract the sorted colors
sorted_set_bar_colors <- as.vector(sorted_colors)

# Use the sorted colors for the sets.bar.color parameter
UpSetR::upset(
  fromList(result$gene_sets),
  sets = names(result$gene_sets),
  order.by = "freq",
  group.by = "sets",
  point.size = 5,
  matrix.color = 'black',
  main.bar.color = result$color_map,
  sets.bar.color = sorted_set_bar_colors,# Use the sorted colors for set.bar.color
  keep.order = TRUE
)