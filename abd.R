# Version info: R 4.2.2, Biobase 2.58.0, GEOquery 2.66.0, limma 3.54.0, DESeq2 1.38.3
################################################################
#   Differential expression analysis with limma
library(GEOquery)
library(limma)
library(umap)

# load series and platform data from GEO

gset <- getGEO("GSE41850", GSEMatrix =TRUE, AnnotGPL=TRUE)
if (length(gset) > 1) idx <- grep("GPL16209", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# make proper column names to match toptable 
fvarLabels(gset) <- make.names(fvarLabels(gset))

# group membership for all samples
gsms <- paste0("00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "00000000000000000000000000000000001111111111111111",
               "11111111111111111111111111111111111111111111111111",
               "11111111111111111111111111111111111111111111111111",
               "11111111111111111111111111111111111111111111111111",
               "111111111111111")
sml <- strsplit(gsms, split="")[[1]]

# log2 transformation
ex <- exprs(gset)
qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
LogC <- (qx[5] > 100) ||
  (qx[6]-qx[1] > 50 && qx[2] > 0)
if (LogC) { ex[which(ex <= 0)] <- NaN
exprs(gset) <- log2(ex) }

exprs(gset) <- normalizeBetweenArrays(exprs(gset)) # normalize data

# assign samples to groups and set up design matrix
gs <- factor(sml)
groups <- make.names(c("MS","Control"))
levels(gs) <- groups
gset$group <- gs
design <- model.matrix(~group + 0, gset)
colnames(design) <- levels(gs)

gset <- gset[complete.cases(exprs(gset)), ] # skip missing values

# calculate precision weights and show plot of mean-variance trend
v <- vooma(gset, design, plot=T)
# OR weights by group
# v <- voomaByGroup(gset, group=groups, design, plot=T, cex=0.1, pch=".", col=1:nlevels(gs))
v$genes <- fData(gset) # attach gene annotations

# fit linear model
fit  <- lmFit(v)

# set up contrasts of interest and recalculate model coefficients
cts <- c(paste(groups[1],"-",groups[2],sep=""))
cont.matrix <- makeContrasts(contrasts=cts, levels=design)
fit2 <- contrasts.fit(fit, cont.matrix)

# compute statistics and table of top significant genes
fit2 <- eBayes(fit2, 0.01)
tT <- topTable(fit2, adjust="fdr", sort.by="B",number=19000)

tT <- subset(tT, select=c("ID","adj.P.Val","P.Value","t","B","logFC","RANGE_GB","RANGE_STRAND","RANGE_START","gene_assignment","mrna_assignment"))
tT
# write.table(tT, file="C:/Users/hp/Desktop/mdata2.csv", row.names=T, sep="\t")
#write.csv(tT, "C:/Users/jaguar/Desktop/New folder (8)/my_data1.csv", row.names = FALSE,sep="\t")

library(readxl)

# my_data2<-read.csv("C:/Users/hp/Desktop/mdata2.csv",sep = "\t")
# abd<-read_excel("C:/Users/hp/Desktop/DATA4.xlsx")

propsite1<-my_data2$ID

prop2<-abd$PROPSETID

# Subset rows of propsite1 where IDs match with prop2
matches <- propsite1[propsite1 %in% prop2]
matched_rows <- my_data2[my_data2$ID %in% matches, ]
matched_rows
sorted_mydata <- matched_rows[order(matched_rows$ID), ]
sorted_mydata
write.csv(sorted_mydata, "C:/Users/hp/Desktop/newDATA4.csv", row.names = FALSE)



matched_rows_gene<-sorted_mydata$gene_assignment

abd$gene_assignment<-matched_rows_gene


matched_rows_mrna<-sorted_mydata$mrna_assignment

abd$mrna_assignment<-matched_rows_mrna

write.csv(abd, "C:/Users/jaguar/Desktop/New folder (8)/New folder/DATA4.csv", row.names = FALSE)

abd<-read.csv("C:/Users/jaguar/Desktop/New folder (8)/New folder/DATA1.csv")
propsite1 <- abd$gene_assignment
propsite1




# Visualize and quality control test results.
# Build histogram of P-values for all genes. Normal test
# assumption is that most genes are not differentially expressed.
tT2 <- topTable(fit2, adjust="fdr", sort.by="B", number=Inf)
hist(tT2$adj.P.Val, col = "grey", border = "white", xlab = "P-adj",
     ylab = "Number of genes", main = "P-adj value distribution")

# summarize test results as "up", "down" or "not expressed"
dT <- decideTests(fit2, adjust.method="fdr", p.value=0.05, lfc=0)

# Venn diagram of results
vennDiagram(dT, circle.col=palette())

# create Q-Q plot for t-statistic
t.good <- which(!is.na(fit2$F)) # filter out bad probes
qqt(fit2$t[t.good], fit2$df.total[t.good], main="Moderated t statistic")

# volcano plot (log P-value vs log fold change)
colnames(fit2) # list contrast names
ct <- 1        # choose contrast of interest
volcanoplot(fit2, coef=ct, main=colnames(fit2)[ct], pch=20,
            highlight=length(which(dT[,ct]!=0)), names=rep('+', nrow(fit2)))

# MD plot (log fold change vs mean log expression)
# highlight statistically significant (p-adj < 0.05) probes
plotMD(fit2, column=ct, status=dT[,ct], legend=F, pch=20, cex=1)
abline(h=0)

################################################################
# General expression data analysis
ex <- exprs(gset)

# box-and-whisker plot
dev.new(width=3+ncol(gset)/6, height=5)
ord <- order(gs)  # order samples by group
palette(c("#1B9E77", "#7570B3", "#E7298A", "#E6AB02", "#D95F02",
          "#66A61E", "#A6761D", "#B32424", "#B324B3", "#666666"))
par(mar=c(7,4,2,1))
title <- paste ("GSE41850", "/", annotation(gset), sep ="")
boxplot(ex[,ord], boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, col=gs[ord])
legend("topleft", groups, fill=palette(), bty="n")
dev.off()

# expression value distribution
par(mar=c(4,4,2,1))
title <- paste ("GSE41850", "/", annotation(gset), " value distribution", sep ="")
plotDensities(ex, group=gs, main=title, legend ="topright")

# UMAP plot (dimensionality reduction)
ex <- na.omit(ex) # eliminate rows with NAs
ex <- ex[!duplicated(ex), ]  # remove duplicates
ump <- umap(t(ex), n_neighbors = 15, random_state = 123)
par(mar=c(3,3,2,6), xpd=TRUE)
plot(ump$layout, main="UMAP plot, nbrs=15", xlab="", ylab="", col=gs, pch=20, cex=1.5)
legend("topright", inset=c(-0.15,0), legend=levels(gs), pch=20,
       col=1:nlevels(gs), title="Group", pt.cex=1.5)
library(maptools)  # point labels without overlaps
pointLabel(ump$layout, labels = rownames(ump$layout), method="SANN",cex=0.6)
