# pipeline
devtools::install_github('Telogen/ASNJ')


library(ASNJ)

online_data <- read.csv('../Phenomics/weekly_online_paper_metrices/SearchResults.csv')
online_data <- filter(online_data,Content.Type != 'Journal')

all_Phenomics_paper_metrics <- get_all_Phenomics_paper_metrics(online_data)
all_Phenomics_paper_metrics$update_time <- NULL
rownames(all_Phenomics_paper_metrics) <- NULL

writexl::write_xlsx(all_Phenomics_paper_metrics,'../ASNJ_data/Phenomics/Phenomics_paper_metrics.xlsx')


