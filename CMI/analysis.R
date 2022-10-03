# analysis

library(ASNJ)
library(dplyr)
library(ggplot2)

all_paper_metrics <- readxl::read_excel('../ASNJ_data/CMI/CMI_paper_metrics.xlsx')
all_paper_metrics$mytype <- factor(all_paper_metrics$orig_type,
                                   levels = c('Article','Review','Others'))
head(all_paper_metrics)

# View(all_paper_metrics)
# https://www.scijournal.org/impact-factor-of-cell-mol-immunol.shtml
all_paper_metrics$IF <- NA
all_paper_metrics[which(all_paper_metrics$year == 2022),]$IF <- 22.096
all_paper_metrics[which(all_paper_metrics$year == 2021),]$IF <- 13.845
all_paper_metrics[which(all_paper_metrics$year == 2020),]$IF <- 4.993
all_paper_metrics[which(all_paper_metrics$year == 2019),]$IF <- 4.003
all_paper_metrics[which(all_paper_metrics$year == 2018),]$IF <- 5.72
all_paper_metrics[which(all_paper_metrics$year == 2017),]$IF <- 6.028
all_paper_metrics[which(all_paper_metrics$year == 2016),]$IF <- 4.076
all_paper_metrics[which(all_paper_metrics$year == 2015),]$IF <- 3.557
all_paper_metrics[which(all_paper_metrics$year == 2014),]$IF <- 3.315
all_paper_metrics[which(all_paper_metrics$year == 2013),]$IF <- 3.732
all_paper_metrics[which(all_paper_metrics$year == 2012),]$IF <- 3.752
all_paper_metrics[which(all_paper_metrics$year == 2011),]$IF <- 3.108
all_paper_metrics[which(all_paper_metrics$year == 2010),]$IF <- 2.395
all_paper_metrics[which(all_paper_metrics$year == 2009),]$IF <- 2.891
all_paper_metrics[which(all_paper_metrics$year == 2008),]$IF <- 3.353
all_paper_metrics[which(all_paper_metrics$year == 2007),]$IF <- 2.312
all_paper_metrics[which(all_paper_metrics$year == 2006),]$IF <- 0.824
all_paper_metrics[which(all_paper_metrics$year == 2005),]$IF <- 0.045
all_paper_metrics[which(all_paper_metrics$year == 2004),]$IF <- 0

# 发文量
ggplot(all_paper_metrics,aes(x = year)) +
  geom_bar(width = 0.8, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_point(aes(y = IF)) +
  geom_line(aes(y = IF)) +
  geom_text(aes(y = IF,label = IF),vjust = -.6,size = 3) +
  scale_x_continuous(breaks=c(2008:2022)) +
  scale_fill_discrete(name = "Article type") +
  ggtitle('CMI articles') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        plot.title = element_text(hjust = 0.5)) +
  ylab('Number')




# subjects
all_paper_metrics_high_cited <- all_paper_metrics[which(all_paper_metrics$citation > 10),]
all_subjects <- strsplit(all_paper_metrics_high_cited$subjects,', ') %>% unlist(use.names = F)
plot_data_all <- table(all_subjects) %>% sort(T) %>% as.data.frame()
head(plot_data_all)
colnames(plot_data_all) <- c('Subject','Frequency')
plot_data <- head(plot_data_all,20)

ggplot(plot_data, aes(x = Subject, y = Frequency,color = Subject,fill = Subject)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8, preserve = "single"), width = 0.7) +
  geom_text(aes(label = round(Frequency,2)), size = 5, position = position_dodge(0.8), vjust = -0.5) +
  ggtitle("CMI highly cited (citations > 10) papers' subjects distribution") +
  ylim(0,34) +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(.2,.2,.2,1.2),units = 'cm'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))


# citations
all_paper_metrics$citation_range <- '0'
all_paper_metrics[which(all_paper_metrics$citation > 0 & all_paper_metrics$citation <= 5),]$citation_range <- '1-5'
all_paper_metrics[which(all_paper_metrics$citation > 5 & all_paper_metrics$citation <= 10),]$citation_range <- '6-10'
all_paper_metrics[which(all_paper_metrics$citation > 10 & all_paper_metrics$citation <= 50),]$citation_range <- '11-50'
all_paper_metrics[which(all_paper_metrics$citation > 50 & all_paper_metrics$citation <= 100),]$citation_range <- '51-100'
all_paper_metrics[which(all_paper_metrics$citation > 100 & all_paper_metrics$citation <= 500),]$citation_range <- '101-500'
all_paper_metrics[which(all_paper_metrics$citation > 500),]$citation_range <- '>500'

table(all_paper_metrics$citation_range)
all_paper_metrics$citation_range <- factor(all_paper_metrics$citation_range,
                                levels = c('0','1-5','6-10','11-50','51-100','101-500','>500'))

ggplot(all_paper_metrics,aes(x = citation_range)) +
  geom_bar(width = 0.8, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  ggtitle("CMI paper citation distribution") +
  theme_bw() +
  xlab('Citation ranges') +
  ylab('Number') +
  theme(axis.text.x = element_text(size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))










