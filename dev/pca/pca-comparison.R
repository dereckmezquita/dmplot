box::use(dt = data.table)
box::use(dmplot[ Pca, Comparison ])
data(feature_counts, package = "dmplot")

data <- feature_counts[GeneBiotype == "protein_coding", ]

colnames(data)[1] <- "feature"

comp_table <- dt$fread(
"group	sample
A	T64552
A	T64553
A	T64554
A	T64555
B	T64546
B	T64548
B	T64549
B	T64550"
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
