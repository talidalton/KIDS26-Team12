library(shiny)
library(VizModules)

# ========================================================================================================
# Goal: show methylation-derived HRD alongside genome HRD, plus a QC panel for the purity/tissue confound
# What we have right now:
#   - Genome HRD scores (HRD.sum, HRD_LOH, LST, TAI) in 'master_samples.tsv'
#   - Raw methylation values for HRD/DDR genes in 'beta_hrd_genes.tsv'

# What we're still waiting for:
#   - the actual epi-HRD score (methylation-derived classifier output) - not yet available.
#     Build against placeholder/fake values for now so you're not blocked; swap in the 
#     real column once it lands
# ========================================================================================================


source("/Users/tdalton/KIDS26-Team12/R/adapters/adapt_ddr_scores.R")
ddr_data<- adapt_ddr_scores()
sample_data<- unique(ddr_data[, .(sample_id, cancer_type, HRDsum, purity, ploidy)])

set.seed(1)
sample_data$epi_HRD <- sample_data$HRDsum + rnorm(nrow(sample_data), 0, 2)