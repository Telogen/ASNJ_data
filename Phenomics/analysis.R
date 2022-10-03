# analysis

library(ASNJ)
library(dplyr)
library(ggplot2)

all_paper_metrics <- readxl::read_excel('../ASNJ_data/Phenomics/paper_metrics.xlsx')

all_paper_metrics <- filter(all_paper_metrics,type != 'Correction')
all_paper_metrics$mytype <- factor(all_paper_metrics$type,
                                   levels = c('Article','Review','Commentary','Correspondence','Editorial','Meeting Report'),
                                   labels = c('Article','Review','Others','Others','Others','Others'))
head(all_paper_metrics)


# 发文量
ggplot(all_paper_metrics,aes(x = year)) +
  geom_bar(width = 0.8, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_x_continuous(breaks=c(2021,2022)) +
  scale_fill_discrete(name = "Article type") +
  ggtitle('Phenomics articles') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        plot.title = element_text(hjust = 0.5)) +
  ylab('Number')




# citations
all_paper_metrics$citation_range <- '0'
all_paper_metrics[which(all_paper_metrics$citation > 0 & all_paper_metrics$citation <= 5),]$citation_range <- '1-5'
all_paper_metrics[which(all_paper_metrics$citation > 5 & all_paper_metrics$citation <= 10),]$citation_range <- '6-10'

table(all_paper_metrics$citation_range)
all_paper_metrics$citation_range <- factor(all_paper_metrics$citation_range,
                                levels = c('0','1-5','6-10','11-50','51-100','101-500','>500'))

ggplot(all_paper_metrics,aes(x = citation_range)) +
  geom_bar(width = 0.8, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  ggtitle("Phenomics paper citation distribution") +
  theme_bw() +
  xlab('Citation ranges') +
  ylab('Number') +
  theme(axis.text.x = element_text(size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))










