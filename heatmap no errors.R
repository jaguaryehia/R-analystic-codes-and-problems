# #If not already installed
# install.packages("gplots")
# install.packages("devtools")

#Load necessary packages
library("gplots")


#Create a fake dataset for demonstration purposes
prob_matrix=read.table('heat_map_data_group_time.unknown')


rownames(prob_matrix)<-prob_matrix$V1
prob_matrix$V1<-NULL
prob_matrix

#Create fake color side bars
drugclass_colors=sample(c("darkorchid","darkred"), length(prob_matrix['group',]), replace = TRUE, prob = NULL)

drugcategory_colors=sample(c("green","darkgreen"), length(prob_matrix['time',]), replace = TRUE, prob = NULL)

subtype_colors=sample(c("red","blue","cyan","pink","yellow","green"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
subtype_colors
Mcolors=sample(c("black","white","grey"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
Ncolors=sample(c("black","white","grey"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
Tcolors=sample(c("black","white","grey"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
HER2colors=sample(c("black","white","grey"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
PRcolors=sample(c("black","white","grey"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
ERcolors=sample(c("black","white","grey"), length(prob_matrix['sample',]), replace = TRUE, prob = NULL)
rlab=t(cbind(drugclass_colors,drugcategory_colors))
clab=cbind(subtype_colors,Mcolors,Ncolors,Tcolors,HER2colors,PRcolors,ERcolors)
rownames(rlab)=c("Class","Category")
colnames(clab)=c("Subtype","M","N","T","HER2","PR","ER")

#Define custom dist and hclust functions for use with heatmaps
mydist=function(c) {dist(c,method="euclidian")}
myclust=function(c) {hclust(c,method="average")}

#Create heatmap using custom heatmap.3 source code loaded above
pdf(file="heatmap3_example.pdf")
main_title="Drug Response Predictions"
par(cex.main=1)
heatmap.3(prob_matrix, hclustfun=myclust, distfun=mydist, na.rm = TRUE, scale="none", dendrogram="both", margins=c(6,12),
          Rowv=TRUE, Colv=TRUE, ColSideColors=clab, RowSideColors=rlab, symbreaks=FALSE, key=TRUE, symkey=FALSE,
          density.info="none", trace="none", main=main_title, labCol=FALSE, labRow=drug_names, cexRow=1, col=rev(heat.colors(75)),
          ColSideColorsSize=7, RowSideColorsSize=2, KeyValueName="Prob. Response")
legend("topright",legend=c("Basal","LumA","LumB","Her2","Claudin","Normal","","Positive","Negative","NA","","Targeted","Chemo","","Approved","Experimental"),
       fill=c("red","blue","cyan","pink","yellow","green","white","black","white","grey","white","darkorchid","darkred","white","green","darkgreen"), border=FALSE, bty="n", y.intersp = 0.7, cex=0.7)
dev.off()

#Example to show that it now also works with just a single column or single row
mat <- matrix(1:100, byrow=T, nrow=10)
column_annotation <- sample(c("red", "blue", "green"), 10, replace=T)
column_annotation <- as.matrix(column_annotation)
colnames(column_annotation) <- c("Variable X")

row_annotation <- sample(c("red", "blue", "green"), 10, replace=T)
row_annotation <- as.matrix(t(row_annotation))
rownames(row_annotation) <- c("Variable Y")

heatmap.3(mat, RowSideColors=row_annotation, ColSideColors=column_annotation)