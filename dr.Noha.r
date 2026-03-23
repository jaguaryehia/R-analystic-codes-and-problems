# Load the required libraries
library(EnhancedVolcano)
library(ggplot2)
library(ggrepel)
library(tibble)

# Read the data from the specified file
df1 <- read.table(
  file = "C:/Users/jaguar/Downloads/DRUG2_DRUG1_NEW_1.txt",
  header = TRUE,
  sep = "\t",
)

# Create a new column to categorize points based on significance and direction
df1$diffexpressed<- "NO"
# if log2Foldchange > 0.9 and pvalue < 0.05, set as "UP" 
df1$diffexpressed[df1$log2FC > 0.9 & df1$PValue < 0.05] <- "UP"
# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
df1$diffexpressed[df1$log2FC < -0.6 & df1$PValue < 0.05] <- "DOWN"
mycolors <- c("blue", "red", "grey")
sizes <- c("UP" = 3, "DOWN" = 3, "NO" = 1)
names(mycolors) <- c("DOWN", "UP", "NO")
makearrows<- df1[df1$diffexpressed %in% c("UP", "DOWN"), ]
# updown<-df1[df1$diffexpressed %in% c("UP", "DOWN"),]
# Create the volcano plot using ggplot2
# volcano_plot <- ggplot(data = df1, aes(x = log2FC, y = -log10(PValue))) +
#   geom_point(aes(size = -log10(PValue), color = diffexpressed), alpha = 0.6, shape = 16) +
#   scale_size_continuous(range = c(0.5, 2)) +
#   geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   labs(
#     title = "Volcano Plot (First File)",
#     x = "log2 Fold Change",
#     y = "-log10 P-Value"
#   ) +
#   scale_color_manual(values = mycolors) +  # Specify custom colors
#   theme_minimal() +
#   geom_text_repel(data= makearrows,
#                   aes(label = Gene),
#                   size = 3,
#                   box.padding = unit(.9, "lines"),hjust= 0.30,
#                   # point.padding = 3,
#                   # segment.size = 0.2,
#                   # segment.color = "black",
#                   # max.overlaps = Inf
#                   
#   )
volcano_plot <- ggplot(data = df1, aes(x = log2FC, y = -log10(PValue))) +
  geom_point(aes(size = diffexpressed, color = diffexpressed), alpha = 0.6, shape = 16) +
  scale_size_manual(values = sizes) +  # Specify custom sizes
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Volcano Plot (First File)",
    x = "log2 Fold Change",
    y = "-log10 P-Value"
  ) +
  scale_color_manual(values = mycolors) +
  theme_minimal() +
  geom_text_repel(data = makearrows,
                  aes(label = Gene),
                  size = 3,
                  box.padding = unit(1.05, "lines"), hjust = 0.10
                  
                  
  )
# Show the plot
print(volcano_plot)


