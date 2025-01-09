library("ggplot2")
library("reshape")
library("tidyr")
library("ComplexHeatmap")
library("circlize")
library(RColorBrewer)

data.cross <- read.table('ANI_result_strain.txt')
data.cross <- data.cross[,1:3,drop=FALSE]
data.cross$V3 <- as.numeric(data.cross$V3)
df_molten_dat <- spread(data.cross,V2,V3)
df_molten_dat <- df_molten_dat %>%
  tibble::column_to_rownames("V1")
df_molten_matrix<- as.matrix(df_molten_dat)
col_fun <- colorRamp2(
  c(70,85 , 100), 
  c(brewer.pal(11, 'RdYlBu')[9], brewer.pal(11, 'RdYlBu')[6], brewer.pal(11, 'RdYlBu')[2])
)
col <- rev(colorRampPalette(brewer.pal(10, "RdYlBu"))(256))
p<-Heatmap(df_molten_dat,name='ANI',col =col)
svg("ANI.svg", width=20, height=20)
p
dev.off()
p<-pheatmap(df_molten_dat, 
            cluster_rows = T,
            cluster_cols = T,
            show_rownames = T,
            show_colnames = T,
            fontsize_row=8,
            fontsize_col=8)

