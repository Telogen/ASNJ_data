# pipeline
devtools::install_github('Telogen/ASNJ')


library(ASNJ)
homepage <- get_journal_homepage('Cell Discovery')    # Cell Research, Cell Discovery, STTT
homepage

article_content_urls <- get_all_content_urls(homepage,'article')
article_paper_urls               <- get_all_paper_urls(article_content_urls)
article_paper_metrics         <- get_all_paper_metrics(article_paper_urls)
article_paper_metrics$orig_type                    <- 'article'

review_content_urls <- get_all_content_urls(homepage,'review')
review_paper_urls               <- get_all_paper_urls(review_content_urls)
review_paper_metrics         <- get_all_paper_metrics(review_paper_urls)
review_paper_metrics$orig_type                    <- 'review'

comment_content_urls <- get_all_content_urls(homepage,'comment')
comment_paper_urls               <- get_all_paper_urls(comment_content_urls)
comment_paper_metrics         <- get_all_paper_metrics(comment_paper_urls)
comment_paper_metrics$orig_type                    <- 'comment'

all_paper_metrics <- rbind(article_paper_metrics,
                           review_paper_metrics,
                           comment_paper_metrics)

dim(all_paper_metrics)

table(all_paper_metrics$type)
table(all_paper_metrics$orig_type)
all_paper_metrics$orig_type <- factor(all_paper_metrics$orig_type,
                                      labels = c('Article','Letter','Review Article')) %>% as.character()
NA_type_idx <- which(all_paper_metrics$type == 'NA')
all_paper_metrics[NA_type_idx,]$type <- all_paper_metrics[NA_type_idx,]$orig_type
table(all_paper_metrics$type)
table(all_paper_metrics$orig_type)

writexl::write_xlsx(all_paper_metrics,'../ASNJ_data/STTT/STTT_paper_metrics.xlsx')


