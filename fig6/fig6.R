#FIG6####
install.packages('circlize')
library(circlize)
df_circle <- read_xlsx('humangut_htBGC_donor.xlsx')
df_circle2 <- df_circle[,c('phylum','genus','dophylum','dogenus')]
df_circle2$dogenus <- ifelse(df_circle2$dogenus == 'unclassfied',
                             paste(df_circle2$dophylum, 'G_unclassified', sep = '_'),
                             df_circle2$dogenus)
df_circle2$genus <- paste('Re',df_circle2$genus,  sep = '_')
df_circle2$phylum <- paste('Re',df_circle2$phylum,  sep = '_')


matrix_genus <- as.matrix(table(df_circle2$dogenus, df_circle2$genus))
nrow(matrix_genus)
nm_genus <- unique(c(df_circle2$genus, df_circle2$dogenus))  

named_vec <- character(length(nm_genus))

for (i in seq_along(nm_genus)) {
  if (nm_genus[i] %in% df_circle2$genus) {
    named_vec[i] <- df_circle2$phylum[match(nm_genus[i], df_circle2$genus)]
  }
  else if (nm_genus[i] %in% df_circle2$dogenus) {
    named_vec[i] <- df_circle2$dophylum[match(nm_genus[i], df_circle2$dogenus)]
  }
  else {
    named_vec[i] <- nm_genus[i]
  }
}
print(named_vec)

group_circle = structure( named_vec,names = nm_genus)

group <- factor(
  group_circle[sample(length(group_circle), length(group_circle))], 
  levels = c("Re_P__Bacillota", "Re_P__Bacteroidota", 
             'P__Bacillota',"P__Bacteroidota",'P__Actinomycetota','P__Thermodesulfobacteriota','unclassfied'))

table_circle <- as.matrix(table(df_circle2$genus, df_circle2$phylum))

grid.col <-  c(
  '#fb8072','#C1C976','#C1C976','#927FD3','#fb8072','#fb8072',
  '#fdb462','#fb8072','#fb8072','#fb8072','#fb8072','#C1C976',
  '#fb8072','#fb8072','#fb8072','#fb8072','#fb8072','#fb8072',
  '#C1C976','#719FFB','#F6C4E6','#F6C4E6','#F6C4E6','#80b1d3',
  '#F6C4E6','#F6C4E6','#F6C4E6','#F6C4E6','#F6C4E6','#F6C4E6',
  '#F6C4E6','#F6C4E6','#F6C4E6','#F6C4E6','#80b1d3','#80b1d3',
  '#80b1d3','#F6C4E6','#F6C4E6','#80b1d3','#F6C4E6','#F6C4E6','#F6C4E6')

pdf(file ="P6.pdf", width = 15, height =10)
circos.clear()
circos.par(start.degree = 280, clock.wise = FALSE)

p_circle <- chordDiagram(matrix_genus,
                         grid.col = grid.col,
                         group = group,
                         annotationTrack = "grid", 
                         preAllocateTracks = list(
                           track.height = max(strwidth(unlist(dimnames(mat)))))))

circos.track(
  track.index = 1, panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, CELL_META$ylim[1], 
      CELL_META$sector.index,  facing = "clockwise", 
      niceFacing = TRUE, adj = c(0, 0.5)
    )
  }, bg.border = NA
)

legend("right",pch=20,legend=unique(c(df_circle2$phylum,df_circle2$dophylum)),
       col=grid.col,bty="n",
       cex=0.5,pt.cex=0.5,border="black",ncol=1)
circos.info() 
dev.off()
