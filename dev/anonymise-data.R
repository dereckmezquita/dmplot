box::use(dt = data.table)

# Function to change last 3 digits of GeneID
change_gene_id <- function(id) {
    prefix <- substr(id, 1, nchar(id) - 3)
    suffix <- sprintf("%03d", sample(0:999, 1))
    return(paste0(prefix, suffix))
}

# Anonymize the data
#' @export
anonymise_data <- function(data) {
    # Create a copy of the original data
    dt_anon <- dt$copy(data)
    
    # Change last 3 digits of GeneID
    dt_anon[, GeneID := sapply(GeneID, change_gene_id)]
    
    # Get the names of numeric columns (excluding GeneID, GeneSymbol, and GeneBiotype)
    numeric_cols <- setdiff(
        names(dt_anon)[sapply(dt_anon, is.numeric)], 
        c("GeneID", "GeneSymbol", "GeneBiotype")
    )
    
    # Randomly add 1 or 2 to some cells in numeric columns
    for (col in numeric_cols) {
        dt_anon[, (col) := get(col) + sample(c(0, 1, 2), .N, replace = TRUE, prob = c(0.85, 0.1, 0.05))]
    }
    
    return(dt_anon[])
}

