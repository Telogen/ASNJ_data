# analysis

library(ASNJ)
library(dplyr)
library(ggplot2)

all_paper_metrics <- readxl::read_excel('../ASNJ_data/CellRes/CellRes_paper_metrics.xlsx')
all_paper_metrics$mytype <- factor(all_paper_metrics$orig_type,
                                   levels = c('Article','Review Article','Others'),
                                   labels = c('Article','Review','Others'))
head(all_paper_metrics)

table(all_paper_metrics$year)
all_paper_metrics$myyear <- all_paper_metrics$year
all_paper_metrics$myyear[which(all_paper_metrics$myyear < 2000)] <- 1999
table(all_paper_metrics$myyear)

# View(all_paper_metrics)
# https://www.scijournal.org/impact-factor-of-cell-res.shtml
all_paper_metrics$IF <- NA
all_paper_metrics[which(all_paper_metrics$myyear == 2022),]$IF <- 46.297
all_paper_metrics[which(all_paper_metrics$myyear == 2021),]$IF <- 21.404
all_paper_metrics[which(all_paper_metrics$myyear == 2020),]$IF <- 10.301
all_paper_metrics[which(all_paper_metrics$myyear == 2019),]$IF <- 9.644
all_paper_metrics[which(all_paper_metrics$myyear == 2018),]$IF <- 8.736
all_paper_metrics[which(all_paper_metrics$myyear == 2017),]$IF <- 8.255
all_paper_metrics[which(all_paper_metrics$myyear == 2016),]$IF <- 7.824
all_paper_metrics[which(all_paper_metrics$myyear == 2015),]$IF <- 7.293
all_paper_metrics[which(all_paper_metrics$myyear == 2014),]$IF <- 6.515
all_paper_metrics[which(all_paper_metrics$myyear == 2013),]$IF <- 7.734
all_paper_metrics[which(all_paper_metrics$myyear == 2012),]$IF <- 7.232
all_paper_metrics[which(all_paper_metrics$myyear == 2011),]$IF <- 5.858
all_paper_metrics[which(all_paper_metrics$myyear == 2010),]$IF <- 7.598
all_paper_metrics[which(all_paper_metrics$myyear == 2009),]$IF <- 6.506
all_paper_metrics[which(all_paper_metrics$myyear == 2008),]$IF <- 3.868
all_paper_metrics[which(all_paper_metrics$myyear == 2007),]$IF <- 4.173
all_paper_metrics[which(all_paper_metrics$myyear == 2006),]$IF <- 3.575
all_paper_metrics[which(all_paper_metrics$myyear == 2005),]$IF <- 2.391
all_paper_metrics[which(all_paper_metrics$myyear == 2004),]$IF <- 2.22
all_paper_metrics[which(all_paper_metrics$myyear == 2003),]$IF <- 1.718
all_paper_metrics[which(all_paper_metrics$myyear == 2002),]$IF <- 1.945
all_paper_metrics[which(all_paper_metrics$myyear == 2001),]$IF <- 2.102
all_paper_metrics[which(all_paper_metrics$myyear == 2000),]$IF <- 1.677
all_paper_metrics[which(all_paper_metrics$myyear == 1999),]$IF <- 0
all_paper_metrics$IF <- round(all_paper_metrics$IF,1)

# 发文量
ggplot(all_paper_metrics,aes(x = myyear)) +
  geom_bar(width = 0.8, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_point(aes(y = IF)) +
  geom_line(aes(x = myyear,y = IF)) +
  geom_text(aes(y = IF,label = IF),vjust = -.6,size = 4) +
  scale_x_continuous(breaks=c(1999:2022),labels = c('1990s',2000:2022)) +
  scale_fill_discrete(name = "Article type") +
  ggtitle('Cell Research articles') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,color = 'black',angle = 45,vjust = 1,hjust = 1),
        axis.text.y = element_text(size = 12,color = 'black'),
        plot.title = element_text(hjust = 0.5)) +
  ylab('Number') +
  xlab('year')





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
  ggtitle("Cell Research highly cited (citations > 10) papers' subjects distribution") +
  ylim(0,260) +
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
  ggtitle("Cell Research paper citation distribution") +
  theme_bw() +
  xlab('Citation ranges') +
  ylab('Number') +
  theme(axis.text.x = element_text(size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))










