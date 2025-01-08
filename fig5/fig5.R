#*(A)
library(ggstatsplot)
library("ggbreak")

df_transrate$phylum.merge <- ifelse(df_transrate$phylum.x %in% c("Bacillota", "Bacteroidota"), df_transrate$phylum.x, "Others")
tab <- table(df_transrate$phylum.x,df_transrate$HTBGC)

tab <- matrix(c(50, 23738, 31, 8028,0,3075), ncol = 2, byrow = TRUE,
              dimnames = list(c("Bacillota", "Bacteroidota","Others"), c("Count1", "Count2")))
total_obs <- sum(tab)  
row_margins <- margin.table(tab, 1)  
col_margins <- margin.table(tab, 2)  
expected <- outer(row_margins, col_margins) / total_obs  
all(expected >= 5)

p_trans_phylum2 <- ggbarstats(df_transrate, 
                              HTBGC, phylum.merge, 
                              perc.k	= 2)+
  scale_fill_manual(values = c('#F9BEBB','#89C9C8'))
extract_stats(p_trans_phylum2)
ggsave("p_phylum_rate.SVG", p_trans_phylum2, width = 6, height = 4, units = "in")

df_jieduan <- data.frame(
  Category = c("Bacillota", "Bacteroidota", "Others"),
  HTBGC = c(50, 31, 0),
  NOT_HTBGC = c(23738, 8028, 3075))
df_jieduan$total <- rowSums(df_jieduan[, -1])
df_jieduan[, -1] <- df_jieduan[, -1] / df_jieduan$total 
df_jieduan_long <- tidyr::pivot_longer(df_jieduan, 
                                 cols = ends_with("BGC"), 
                                 names_to = "Value_BGC", 
                                 values_to = "Value")
df_jieduan_long$Value_BGC <- factor(df_jieduan_long$Value_BGC, levels = c('NOT_HTBGC', 'HTBGC'))
p_jieduan <- ggplot(df_jieduan_long, aes(x = Category, y = Value, fill = Value_BGC)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal()+
  scale_fill_manual(values = c('#F9BEBB','#89C9C8'))
p_jieduan2 <- p_jieduan + scale_y_break(c(0.005, 0.95),scales = "free",
                         expand=expansion(add = c(0, 0.002)))
ggsave("p_phylum_jieduan.SVG", p_jieduan2, width = 6, height = 4, units = "in")

#*(B)
tab <- table(df_transrate$genus.x,df_transrate$HTBGC)
df_transrate$genus.merge <- ifelse(df_transrate$genus.x %in% c("Agathobacter", "Wujia","Streptococcus", 
                                                               "Simiaoa","Segatella", "Ruminococcus","Roseburia",
                                                               "Phocaeicola", "Paraprevotella",'Parabacteroides',
                                                               'Megamonas','Mediterraneibacter','Lachnospira',
                                                               'Faecalibacterium','Enterococcus','Enterocloster',
                                                               'Dorea','Catenibacterium','Butyrivibrio',
                                                               'Blautia','Bacteroides','Anaerostipes','Anaerobutyricum'),
                                   df_transrate$genus.x, "Others")
#分析是否可用卡方检验
mat <- as.matrix(table(df_transrate$genus.merge, df_transrate$HTBGC)) 
total_obs <- sum(mat)  
row_margins <- margin.table(mat, 1)  
col_margins <- margin.table(mat, 2) 
expected <- outer(row_margins, col_margins) / total_obs 
all(expected >= 5)
#不符合卡方分析要求，应用fisher确切概率法
result <- fisher.test(mat,simulate.p.value = TRUE)
print(result)

df_jieduan2 <- as.data.frame(as.matrix(table(df_transrate$genus.merge, df_transrate$HTBGC)))
df_jieduan2$Var2 <- factor(df_jieduan2$Var2, levels = c('NOT', 'HTBGC'))

group_totals <- df_jieduan2 %>% group_by(Var1) %>% summarise(total = sum(Freq))
df_jieduan2 <- left_join(df_jieduan2, group_totals, by = "Var1")
df_jieduan2 <- df_jieduan2 %>% mutate(Rate = Freq / total)
bianliang1 <- filter(df_jieduan2, Var2 == 'HTBGC')
bianliang1 <- bianliang1 %>% arrange(desc(Rate)) 
df_jieduan2$Var1 <- factor(df_jieduan2$Var1, levels = bianliang1$Var1)

p_jieduan_genus <- ggplot(df_jieduan2, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) + # 设置y轴为百分比格式
  theme_minimal()+
  scale_fill_manual(values = c('#F9BEBB','#89C9C8'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_jieduan_genus2 <- p_jieduan_genus + scale_y_break(c(0.03, 0.98),scales = "free",
                                        expand=expansion(add = c(0, 0.001)))
ggsave("p_genus_jieduan.SVG", p_jieduan_genus2, width = 10, height = 4, units = "in")

#*(C)
df_transrate$BGC_ripp <- ifelse(grepl("RiPP", df_transrate$BGC.x), 'RiPP', 'non-RiPP')
p_trans_BGC_ripp<- ggbarstats(df_transrate, HTBGC, BGC_ripp, palette = 'Set1',perc.k	= 2)
p_trans_BGC_ripp
ggsave("p_BGC_ripp.SVG", p_trans_BGC_ripp, width = 6, height = 4, units = "in")

df_jieduanBGC_ripp <- as.data.frame(as.matrix(table(df_transrate$BGC_ripp, df_transrate$HTBGC)))
df_jieduanBGC_ripp$Var2 <- factor(df_jieduanBGC_ripp$Var2, levels = c('NOT', 'HTBGC'))
df_jieduanBGC_ripp$Var1 <- factor(df_jieduanBGC_ripp$Var1, levels = c('RiPP', 'non-RiPP'))

p_jieduan_BGC_ripp <- ggplot(df_jieduanBGC_ripp, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(position = "fill") +
  #labs(title = "Stacked Bar Chart (Percentage)", x = "Category", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal()+
  scale_fill_manual(values = c('#F9BEBB','#89C9C8'))+
  scale_y_break(c(0.004, 0.98),scales = "free",expand=expansion(add = c(0, 0.0001)))
tab <- table(p_jieduan_BGC_ripp$phylum.x,df_transrate$HTBGC)
ggsave("p_jieduan_BGC_ripp.SVG", p_jieduan_BGC_ripp, width = 6, height = 4, units = "in")

#*(D)
df_transrate$BGC_cla <- ifelse(grepl("cyclic-lactone-autoinducer", df_transrate$type.x), 'cyclic-lactone-autoinducer', 'non-cyclic-lactone-autoinducer')
p_trans_BGC_cla<- ggbarstats(df_transrate, HTBGC, BGC_cla, palette = 'Set1',perc.k	= 2)
ggsave("p_BGC_cla.SVG", p_trans_BGC_cla, width = 6, height = 4, units = "in")

df_jieduanBGC_cla <- as.data.frame(as.matrix(table(df_transrate$BGC_cla, df_transrate$HTBGC)))
df_jieduanBGC_cla$Var2 <- factor(df_jieduanBGC_cla$Var2, levels = c('NOT', 'HTBGC'))

p_jieduan_BGC_cla <- ggplot(df_jieduanBGC_cla, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(position = "fill") +
  #labs(title = "Stacked Bar Chart (Percentage)", x = "Category", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal()+
  scale_fill_manual(values = c('#F9BEBB','#89C9C8'))+
  scale_y_break(c(0.01, 0.98),scales = "free",expand=expansion(add = c(0, 0.001)))
tab_jieduan_BGC_cla <- table(df_transrate$BGC_cla,df_transrate$HTBGC)
ggsave("p_jieduan_BGC_cla.SVG", p_jieduan_BGC_cla, width = 6, height = 4, units = "in")




