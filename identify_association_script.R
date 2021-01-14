################################################################################################
##
##
##            IDENTIFICATION OF CATEGORICAL CLINICAL FACTORS ASSOCIATED
##            WITH IMMUNE CELL CLUSTERS
##
##
################################################################################################


load("/Users/jameskelly/Downloads/ucec.raw_counts.RData") ##file with raw counts
CLINICAL_INFO <- ucec_cdr_immune_clin                     ## and associated clinical data
CLINICAL_INFO <- CLINICAL_INFO[,-1] 
CLINICAL_INFOCHI <- CLINICAL_INFO                     

## Select only clinical info that is categorical
CLINICAL_INFOCHI <- CLINICAL_INFOCHI[ , c(1,5,6,7,47,65,67,68,88,89)]

##Read in the deconvoluted data
Immune_proportionCHI <- read.csv("/Users/jameskelly/Documents/IMMUNEESTIMATION/IMMUNE_PROPORTION.csv")
Immune_proportionCHI <- as.data.frame(Immune_proportionCHI)
rownames(Immune_proportionCHI) <- Immune_proportionCHI[,1] ##Set sample ID as rownames
Immune_proportionCHI <- Immune_proportionCHI[,-1]

CLINICAL_INFOCHI$sample == rownames(Immune_proportionCHI) ##to check if in same order
all_clin_n_deconv <- cbind(CLINICAL_INFOCHI, Immune_proportionCHI) ## Now I can cbind
names(all_clin_n_deconv)[names(all_clin_n_deconv) == "P.value"] <- "PVALUES" 


all_clin_n_deconv <-  fltr_pval(all_clin_n_deconv) ## Function I wrote in 
## DECONVOLUTION_CLUSTERING_ANNOTATEDHEATMAP repository

names(Immune_proportionCHI)[names(Immune_proportionCHI) == "P.value"] <- "PVALUES" 
Immune_proportionCHI <- fltr_pval(Immune_proportionCHI)


all_clin_n_deconv$sample == rownames(Immune_proportionCHI)
clustergrp <- kmeans(Immune_proportionCHI, 2) ##make a column to associate each sample with a cluster
clustergrp <- clustergrp$cluster
all_clin_n_deconv$CLUSTER <- clustergrp 

##subset the categoricals and the associated cluster
clinical_n_cluster <- all_clin_n_deconv[, c(2:10,36)]


##I use the 'lapply' method to run chi square test to test
##if there is an association between any of the categorical clinical
##parameters and the immune cell clusters
##All P values are computed and then only significant ones are kept.
##This quickly computes which clinical paramaters may be associated with clusters.
Cluster_Associated_w_Category <- lapply(clinical_n_cluster[1:length(clinical_n_cluster)], function(x) chisq.test(table(x, clinical_n_cluster$CLUSTER))$p.value)
myTests[myTests<.05]

