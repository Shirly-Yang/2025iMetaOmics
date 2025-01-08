library('readxl')
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)

#*(D)
data.cross <- read.table('mldist_list.txt',header = TRUE)
my_comparisons = list( c("MAGs", "REF"))
p <- ggboxplot(data.cross,x = "type",y = "value",color = "type",add = "jitter") 
p2<-p + stat_compare_means(comparisons = my_comparisons,
                       # label = "p.signif",
                       method = "t.test")

#*(E)
df_hmp<- read_xlsx('hmp.xlsx')
summary_df <- df_hmp %>%
  group_by(BGC, Type) %>%
  summarise(count = n()) %>%
  ungroup()
custom_type_order <- c("cyclic-lactone-autoinducer", "RRE-containing", 
                       "cyclic-lactone-autoinducer.RRE-containing",'RiPP-like',
                       'LAP.thiopeptide','T3PKS','siderophore') 
summary_df$Type <- factor(summary_df$Type, levels = custom_type_order)
custom_type_order2 <- c("RiPPs", "PKS", "Others") 
summary_df$BGC <- factor(summary_df$BGC, levels = custom_type_order2)
#custom_colors <- c('#64B6EA','#9BCDFF', "#61D4D5", '#99FFCC', '#CCFF9A','#F8D889', '#FFAFD7')
custom_colors <- c('#C095E4','#E49995', "#B8E495", '#95E0E4', '#F8D889','#FFFF77', '#FFAFD7')
p_his <- ggplot(summary_df, aes(x = BGC, y = count, fill = Type)) +
  #geom_bar(stat = "identity", position = position_dodge2(width = 0.8), width = 0.8) +
  geom_bar(data = filter(summary_df, BGC == "RiPPs"), stat = "identity", position = position_dodge2(width = 0.8), width = 1.1) + # RiPPs 组的柱子
  geom_bar(data = filter(summary_df, BGC == "PKS"), stat = "identity", position = position_dodge2(width = 0.8), width = 1.1/5) + # PKS 组的柱子，宽度是 RiPPs 的五分之一
  geom_bar(data = filter(summary_df, BGC == "Others"), stat = "identity", position = position_dodge2(width = 0.8), width = 1.1/5) + # PKS 组的柱子，宽度是 RiPPs 的五分之一
  scale_fill_manual(values = custom_colors) +  # 自定义颜色
  labs(title = " ",
       x = "BGC",
       y = "Number") +
  scale_y_continuous(expand = c(0, 0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(hjust = 1),
        legend.position =  c(0.7, 0.7),
        axis.line = element_line(color = "black"), 
  )
ggsave('hmp_hist_BGC.svg',p_his,width = 6,height = 6,units = 'in',dpi = 600)

#*(F)
summary_genus <- df_hmp %>%
  group_by(PHYLUM, GENUS) %>%
  summarise(count = n()) %>%
  ungroup()

GENUS_order <- c("G__Bacteroides", "G__Phocaeicola", 
                 "G__Parabacteroides",'G__Agathobacter','G__Blautia','G__Lachnoanaerobaculum','G__Streptococcus') 
summary_genus$GENUS <- factor(summary_genus$GENUS, levels = GENUS_order)
PHYLUM_order <- c("P__Bacteroidota", "P__Bacillota") 
summary_genus$PHYLUM <- factor(summary_genus$PHYLUM, levels = PHYLUM_order)
GENUS_colors <- c('#DF97BE','#BEDF97', "#97BEDF", '#979ADF', '#DC97DF','#DFB897', '#9ADF97')
p_genus_his <- ggplot(summary_genus, aes(x = PHYLUM, y = count, fill = GENUS)) +
  #geom_bar(stat = "identity", position = position_dodge2(width = 0.8), width = 0.8) +
  geom_bar(data = filter(summary_genus, PHYLUM == "P__Bacteroidota"), stat = "identity", position = position_dodge2(width = 0.8), width = 0.6) + 
  geom_bar(data = filter(summary_genus, PHYLUM == "P__Bacillota"), stat = "identity", position = position_dodge2(width = 0.8), width = 0.8) + 
  scale_fill_manual(values = GENUS_colors, name = "") + 
  labs(title = " ",
       x = "Phylum",
       y = "Number") +
  scale_y_continuous(expand = c(0, 0))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(hjust = 1),
        legend.position =  c(0.7, 0.7),
        axis.line = element_line(color = "black"), 
        )
ggsave('hmp_hist_GENUS.svg',p_genus_his,width = 8,height = 8,units = 'in',dpi = 600)
