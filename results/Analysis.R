# Load libraries -------------------
setwd('~/Desktop/training')
library(tidyverse)
library(edgeR)



# Load data here ----------------------
# Load each file with a meaningful variable name.

exp<- read.csv('GSE60450_GeneLevel_Normalized(CPM.and.TMM)_data.csv', row.names = 1)
metadata<- read.csv('GSE60450_filtered_metadata.csv',row.names = 1)


# Inspect the data -------------------------

# What are the dimensions of each data set? (How many rows/columns in each?)
## Expression data
dim(exp)

## Metadata
dim(metadata)

# Prepare/combine the data for plotting ------------------------
# How can you combine this data into one data.frame?

#' save the gene symbol alone
gene_sym<- exp %>% select('gene_symbol')

#' make a dge obj
identical(colnames(exp)[-1], rownames(metadata))
dge<- DGEList(exp[,-1], samples = metadata)
dge$genes<- gene_sym


# Plot the data --------------------------
## Plot the expression by cell type
## Can use boxplot() or geom_boxplot() in ggplot2

#' gene a gene of interest
gene_of_interest <- "Gnai3"

#' get the gene's expression value
expr <- dge$counts[rownames(dge$genes)[dge$genes$gene_symbol == gene_of_interest], ]

# prepare df for plotting
df <- data.frame(
  Expression = as.numeric(expr),
  CellType = dge$samples$immunophenotype,  # or whichever column defines your groups
  Sample = colnames(dge)
)

#' plot out
p<- ggplot(df, aes(x = CellType, y = Expression, fill = CellType)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6, size = 4) +  # add individual points
  theme_classic(base_size = 14) +
  scale_fill_manual(values = c('#649BC8', "#CFEFA6"))+
  theme(  axis.text.x = element_text(face = "bold", color = "black"),
          axis.title.x = element_text(face = "bold", color = "black"),
          axis.text.y = element_text(face = "bold", color = "black"),
          axis.title.y = element_text(face = "bold", color = "black"),
          legend.position = "none",
          plot.title = element_text(face = 'bold', color = 'black', hjust = 0.5))+
  labs(title = paste("Expression of", gene_of_interest ,'Gene'),
       y = "Expression (counts)", x = "Cell Type")


## Save the plot
### Show code for saving the plot with ggsave() or a similar function
pdf(paste("Expression of", gene_of_interest ,'Gene', 'boxplot.pdf'), width = 6, height = 5)
p
graphics.off()
