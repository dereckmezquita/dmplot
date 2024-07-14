box::use(dt = data.table)
box::use(dmplot[ Pca, Comparison ])
data(feature_counts, package = "dmplot")

data <- feature_counts[GeneBiotype == "protein_coding", ]

colnames(data)[1] <- "feature"

comp_table <- data.frame(
    group = c("A", "A", "A", "A", "B", "B", "B", "B"),
    sample = c("T64552", "T64553", "T64554", "T64555", "T64546", "T64548", "T64549", "T64550")
)

comp <- Comparison$new(
    comparison_name	= "A_over_B",
    group_order = c("B", "A"),
    comparison_table = comp_table
)

pca_obj <- Pca$new(data, comp)
pca_obj$prcomp()

pca_obj$data
pca_obj$prcomp_results
pca_obj$prcomp_refined

pca_obj$plot_scree()
pca_obj$plot_scatter()
