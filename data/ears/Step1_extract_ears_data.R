
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tabulizer)

# Directories
indir <- "data/ears/data"
outdir <- "data/ears/data/raw"

# PDF path
pdf_path <- file.path(getwd(), indir, "IS_DRI_2006_summary_tables.pdf")

# Extract tables
pages_with_tables <- 2:13
table_list <- extract_tables(pdf_path, pages = 7, method="stream")

# Loop through and export
for(i in 1:length(table_list)){
  
  df <- table_list[[i]]
  write.csv(df, file=file.path(outdir, paste0("DRI_summary_table_raw", i, ".csv")), row.names=F)
  
}
