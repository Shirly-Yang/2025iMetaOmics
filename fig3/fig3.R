library(ggpubr)
library(readxl)
library(pROC)
library(ggplot2)
library(caret)
library(ggpubr)

#*(A)
data_ori <- read.csv("final_reads_bins.csv")
data <- data_ori[data_ori$ACTURAL != 'na', ]
data$group <- NA  
data$group[grepl("50M", data$genome)] <- "50M"
data$group[grepl("100M", data$genome)] <- "100M"
data$group[grepl("200M", data$genome)] <- "200M"
data$genome_name <- sub("_.*", "", data$genome)  
data$actual <- ifelse(data$ACTURAL == "1" , 1, 0)
data$predicted <- ifelse(data$HTBGC == TRUE, 1, 0)
data$predicted <- as.numeric(data$predicted)
head(data)

roc_curve <- roc(data$actual, data$predicted)
ci <- ci.auc(roc_curve, conf.level = 0.95)
roc_plot <- ggroc(roc_curve,alpha = 0.5, colour = "red", linetype = 1, size = 1,legacy.axes = TRUE)+
  theme_bw() + ggtitle(" ") + 
  xlab("False Positive Rate ( FPR )") + ylab("True Positive Rate ( TPR )") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size = 1, color="darkgrey", linetype="dashed")+
  annotate("text", x = 0.7, y = 0.3, label = paste("AUC =", round(roc_curve$auc, 3)),alpha = 0.5, colour = "red", size = 8) 
ggsave("roc_curve.svg", plot = roc_plot, width = 8, height = 6, dpi = 300)


#*(B)
f1_scores <- data.frame(group = character(), genome_name = character(), F1_score = numeric())
for (group in unique(data$group)) {
  for (genome in unique(data$genome_name[data$group == group])) {
    subset_data <- data[data$group == group & data$genome_name == genome, ]    
    if (nrow(subset_data) > 0) {
      cm <- confusionMatrix(factor(subset_data$predicted, levels = c(0, 1)),
                            factor(subset_data$actual, levels = c(0, 1)))
      f1 <- cm$byClass['F1']
      f1_scores <- rbind(f1_scores, data.frame(group = group, genome_name = genome, F1_score = f1))
    }
  }
}
write.xlsx(f1_scores, "f1_scores.xls")
f1_scores2 <- subset(f1_scores, !is.na(F1_score))[]

f1_scores2$group <-factor(f1_scores2$group ,ordered=TRUE,levels=c("50M","100M",'200M')) 
pF1score <- ggplot(f1_scores2, aes(x = group, y = F1_score, fill = group)) + 
  geom_boxplot() +  
  geom_jitter(color = "red", size = 1, height=0,width = 0.1) +  
  #geom_jitter(width = 0.1,shape = 16, size = 1, color = "black") +  
  scale_fill_manual(values = c("50M" = "lightblue", "100M" = "lightgreen", "200M" = "lightcoral")) +
  labs(title = "", 
       x = "Library Size (in millions) ", y = "F1 Score") +
  theme_bw() +  
  theme(axis.text = element_text(angle = 0, hjust = 2,color = "black",size = 10))  +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 1.2)) 
  #stat_compare_means(method = "kruskal.test", label = "p.signif")

# 不满足正态分布，使用 Kruskal-Wallis 检验
kruskal_result <- kruskal.test(F1_score ~ group, data = f1_scores)
print(kruskal_result)

dunn_result <- dunn.test(f1_scores$F1_score, f1_scores$group, 
                         kw = TRUE, label = TRUE, 
                         wrap = TRUE, list = TRUE)
ggsave("F1_box.svg", plot = pF1score, width = 8, height = 6, dpi = 300)
