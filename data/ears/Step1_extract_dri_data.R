
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tabulizer)

# Directories
datadir <- "data/ears/new/raw"

# PDF path
# Ths file was downloaded here: https://www.nal.usda.gov/sites/default/files/fnic_uploads/recommended_intakes_individuals.pdf
pdf_path <- file.path(getwd(), datadir, "recommended_intakes_individuals.pdf")

# Extract tables
table_list <- extract_tables(pdf_path, method="stream")

# Loop through and export
for(i in 1:length(table_list)){
  
  df <- table_list[[i]]
  write.csv(df, file=file.path(datadir, paste0("table", i, ".csv")), row.names=F)
  
}
