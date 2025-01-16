library(ggplot2)
library(readxl)
library(ggalluvial)
library(RColorBrewer)
df_humanall<- read_xlsx('humangut_all.xlsx')
df_humanhgt <- read_xlsx('humangut_htBGC.xlsx')
DFhumanhgt <- df_humanhgt[,c("phylum","genus","BGC","type")]#BGC大类，type小类

#*(A)
df <- to_lodes_form(DFhumanhgt[,1:ncol(DFhumanhgt)],axes = 1:ncol(DFhumanhgt),id = "value")
cString_sanky <- c(
  "#D8D68C", "#A6D1B7", "#A1B7D6", "#E1A9C4", 
  "#F0A5B9", "#E6B59F", "#A1D5C3", "#A8C3D9", 
  "#A1D3A0", "#E4C7D1", "#D3E0A2", "#B6A4D8", 
  "#A3D2D4", "#E1A6A1", "#D1C8F0", "#E4A9C9", 
  "#A3D1A3", "#E2C17A", "#8F9BDA", "#B9D7A9", 
  "#7BA6E3", "#A3C0A3", "#D2A6E9", "#A9C7D8", 
  "#C69FCD", "#E2D79A", "#A8C7E6", "#BCC7A8", 
  "#84A9E0", "#C3D9D1", "#A4F2E4", "#E3B0A5", 
  "#F1D0C2", "#A1A4D8", "#C0E6A8", "#B1C1E0", 
  "#9B6F97", "#A2B1C8", "#B4E8D3", "#D2F1A8")
p2 <- ggplot(df, aes(x = x, fill=stratum, label=stratum,
                     stratum = stratum, alluvium  = value))+
  geom_flow(linewidth = 0.3,
            curve_type = "sine",
            alpha = 0.5,
            color = 'white',
            size = 0.1)+
  geom_stratum(width = 0.28)+
  #geom_text(stat = 'stratum', size = 2, color = 'black')+
  scale_fill_manual(values = cString3)+
  #scale_fill_manual('Set3')+
  theme_void()+
  theme(legend.position = 'right',
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.5, "lines"))
ggsave(p2,filename = "sankey_humangut_htBGC3.svg",width = 15, height = 15, units = "in",dpi = 300)

#*(B)
dat <- aggregate(DFhumanhgt$phylum, by = list(DFhumanhgt$phylum,DFhumanhgt$genus), FUN = length)
names(dat) <- c('phylum','genus','value')
dat$genus <- factor(dat$genus, levels = c("G__Agathobacter","G__Anaerobutyricum",'G__Anaerostipes','G__Blautia','G__Butyrivibrio',
                                          'G__Catenibacterium', "G__Dorea", "G__Enterocloster", 
                                          "G__Enterococcus", "G__Faecalibacterium",
                                          "G__Lachnospira", "G__Mediterraneibacter", "G__Megamonas",
                                          "G__Roseburia",'G__Ruminococcus','G__Simiaoa','G__Streptococcus','G__Wujia',
                                          'G__Bacteroides','G__Parabacteroides','G__Paraprevotella',
                                          'G__Phocaeicola','G__Segatella'))

dat1 = aggregate(dat$value, by = list(dat$phylum), FUN = sum)
dat1$per1 = dat1$x / sum(dat1$x)
for (i in seq(nrow(dat1), 1)) {
  if (i == nrow(dat1)) {
    dat1$per.y1[i] = dat1$per1[i] / 2
  }else{
    dat1$per.y1[i] = sum(dat1$per1[(i + 1):nrow(dat1)]) + dat1$per1[i] / 2
  }
}
dat1$label1 = paste(dat1$Group.1,'(',round(dat1$per1*100, 2),'%',')', sep = '')
dat = merge(dat, dat1[,c(1,3,4,5)], by.x = 'phylum', by.y = 'Group.1')
dat2 = aggregate(dat$value, by = list(dat$genus), FUN = sum)
dat2$per2 = dat2$x / sum(dat2$x)
for (i in seq(nrow(dat2), 1)) {
  if (i == nrow(dat2)) {
    dat2$per.y2[i] = dat2$per2[i] / 2
  }else{
    dat2$per.y2[i] = sum(dat2$per2[(i + 1):nrow(dat2)]) + dat2$per2[i] / 2
  }
}
dat2$label2 = paste(dat2$Group.1,'(',round(dat2$per2*100, 2),'%',')', sep = '')
dat = merge(dat, dat2[,c(1,3,4,5)], by.x = 'genus', by.y = 'Group.1')
mycol <- c(
  "#A4B8F0", "#F8D0C3", "#A6A9D4", "#F6C4E6", "#80B1D3", 
  "#719FFB", "#B7D9A4", "#D4A1E8", "#F2A2B8", "#B1B9F3", 
  "#D5D9C0", "#C4A6F1", "#E8B8D3", "#A3D3F2", "#F9C8B3", 
  "#C1D1F2", "#E4B1A2", "#A8D8F2", "#D1A3C9", "#F1D3F2", 
  "#A9C1D3", "#F2A7C3", "#C2D9F5", "#A1D4A1", "#D1F2A8")
p_pie_phylum <- ggplot(dat) +
  geom_bar(aes(x=3, 
               ifelse(phylum == 'P__Bacillota', per1/18, per1/5),
               fill = phylum), 
           stat = 'identity', width = 2.5) +
  #geom_text(aes(3, as.numeric(per.y1),label = label1),size = 2.5, color = 'black') +
  geom_bar(aes(x= 5, per2, fill = genus),stat = 'identity', width = 1.5) +
  #geom_text(aes(x=5, as.numeric(per.y2), label = label2),size =2.5, color = 'black') +
  scale_y_continuous(labels = scales::percent,limits = c(0, 1)) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = mycol)+ 
  theme(legend.position = 'right')
p_pie_phylum
ggsave("p_pie_phylum2.svg",p_pie_phylum,width = 15, height = 15, units = "in",dpi = 300)

#*(C)
dat_bgc <- aggregate(DFhumanhgt$BGC, by = list(DFhumanhgt$BGC,DFhumanhgt$type), FUN = length)
names(dat_bgc) <- c('bgc','type','value')
dat_bgc$type <- factor(dat_bgc$type, levels = c("arylpolyene", 
                                                "cyclic-lactone-autoinducer.betalactone",
                                                "resorcinol",
                                                'cyclic-lactone-autoinducer','RRE-containing',
                                                'cyclic-lactone-autoinducer.lanthipeptide-class-ii',
                                                'lanthipeptide-class-i','lanthipeptide-class-ii','lanthipeptide-class-iv',
                                                'ranthipeptide','RiPP-like','thiopeptide'))

dat_bgc1 = aggregate(dat_bgc$value, by = list(dat_bgc$bgc), FUN = sum)
dat_bgc1$per1 = dat_bgc1$x / sum(dat_bgc1$x)
for (i in seq(nrow(dat_bgc1), 1)) {
  if (i == nrow(dat_bgc1)) {
    dat_bgc1$per.y1[i] = dat_bgc1$per1[i] / 2
  }else{
    dat_bgc1$per.y1[i] = sum(dat_bgc1$per1[(i + 1):nrow(dat_bgc1)]) + dat_bgc1$per1[i] / 2
  }
}
dat_bgc1$label1 = paste(dat_bgc1$Group.1,'(',round(dat_bgc1$per1*100, 2),'%',')', sep = '')
dat_bgc = merge(dat_bgc, dat_bgc1[,c(1,3,4,5)], by.x = 'bgc', by.y = 'Group.1')
dat_bgc2 = aggregate(dat_bgc$value, by = list(dat_bgc$type), FUN = sum)
dat_bgc2$per2 = dat_bgc2$x / sum(dat_bgc2$x)
for (i in seq(nrow(dat_bgc2), 1)) {
  if (i == nrow(dat_bgc2)) {
    dat_bgc2$per.y2[i] = dat_bgc2$per2[i] / 2
  }else{
    dat_bgc2$per.y2[i] = sum(dat_bgc2$per2[(i + 1):nrow(dat_bgc2)]) + dat_bgc2$per2[i] / 2
  }
}
dat_bgc2$label2 = paste(dat_bgc2$Group.1,'(',round(dat_bgc2$per2*100, 2),'%',')', sep = '')
dat_bgc = merge(dat_bgc, dat_bgc2[,c(1,3,4,5)], by.x = 'type', by.y = 'Group.1')
mybgccol <- c(
  "#D8B4D2", "#B6A9E0", "#80B1D3", 
  "#B3A4C9", 
  "#B0D6F6", "#A8B9D6", "#F4B2B2","#F2C6C6",
  "#B3D7D1", "#D8A5A6", "#B9C2FF", "#F1A8A8", 
  "#98D59A", "#A9E0A1"
)
p_pie_BGC <- ggplot(dat_bgc) +
  geom_bar(aes(x=3, 
               ifelse(bgc == 'Others', per1/3, per1/9),
               fill = bgc), 
           stat = 'identity', width = 2.5) +
  #geom_text(aes(3, as.numeric(per.y1),label = label1),size = 2.5, color = 'black') +
  geom_bar(aes(x= 5, per2, fill = type),stat = 'identity', width = 1.5) +
  #geom_text(aes(x=5, as.numeric(per.y2), label = label2),size =2.5, color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  coord_polar(theta = "y") + 
  theme_void() +
  scale_fill_manual(values = mybgccol)+  
  theme(legend.position = 'right') 
p_pie_BGC
ggsave("p_pie_BGC2.svg",p_pie_BGC,width = 15, height = 15, units = "in",dpi = 300)

