# analysis

library(ASNJ)
library(dplyr)
library(ggplot2)

all_paper_metrics <- readxl::read_excel('../ASNJ_data/CellDis/CellDis_paper_metrics.xlsx')
all_paper_metrics$mytype <- factor(all_paper_metrics$orig_type,
                                   levels = c('Article','Review Article','Correspondence'),
                                   labels = c('Article','Review','Others'))
head(all_paper_metrics)

# View(all_paper_metrics)
# https://www.scijournal.org/impact-factor-of-cell-discovery.shtml
all_paper_metrics$year <- as.numeric(all_paper_metrics$year)
all_paper_metrics$IF <- NA
all_paper_metrics[which(all_paper_metrics$year == 2015),]$IF <- 0
all_paper_metrics[which(all_paper_metrics$year == 2016),]$IF <- 2.207
all_paper_metrics[which(all_paper_metrics$year == 2017),]$IF <- 4.714
all_paper_metrics[which(all_paper_metrics$year == 2018),]$IF <- 4.582
all_paper_metrics[which(all_paper_metrics$year == 2019),]$IF <- 4.852
all_paper_metrics[which(all_paper_metrics$year == 2020),]$IF <- 5.787
all_paper_metrics[which(all_paper_metrics$year == 2021),]$IF <- 18.08
all_paper_metrics[which(all_paper_metrics$year == 2022),]$IF <- 38.079
all_paper_metrics$IF <- round(all_paper_metrics$IF,1)

# 发文量
ggplot(all_paper_metrics,aes(x = year)) +
  geom_bar(width = 0.8, aes(fill = mytype)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  geom_point(aes(y = IF)) +
  geom_line(aes(y = IF)) +
  geom_text(aes(y = IF,label = IF),vjust = -.6,size = 5) +
  scale_x_continuous(breaks=c(2015,2016,2017,2018,2019 , 2020, 2021,2022)) +
  scale_fill_discrete(name = "Article type") +
  ggtitle('Cell Discovery articles') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,color = 'black'),
        axis.text.y = element_text(size = 12,color = 'black'),
        plot.title = element_text(hjust = 0.5),legend.title = ) +
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
  ggtitle("Cell Discovery highly cited (citations > 10) papers' subjects distribution") +
  ylim(0,40) +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(.2,.2,.2,1.8),units = 'cm'),
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
  ggtitle("Cell Discovery paper citation distribution") +
  theme_bw() +
  xlab('Citation ranges') +
  ylab('Number') +
  theme(axis.text.x = element_text(size = 12,colour = 'black'),
        axis.text.y = element_text(size = 12,colour = 'black'),
        plot.title = element_text(hjust = 0.5))










