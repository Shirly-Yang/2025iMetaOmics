library(ggplot2)
library(readxl)
library(ggalluvial)
library(RColorBrewer)
df_humanall<- read_xlsx('humangut_all.xlsx')
df_humanhgt <- read_xlsx('humangut_htBGC.xlsx')
DFhumanhgt <- df_humanhgt[,c("phylum","genus","BGC","type")]#BGC大类，type小类

#*(A)
df <- to_lodes_form(DFhumanhgt[,1:ncol(DFhumanhgt)],axes = 1:ncol(DFhumanhgt),id = "value")
cString3 <- c('#8770E0' ,'#FF9DCE', 
              '#4DE890', '#2178B8', '#77A2E8', '#f97b81', '#26C4B8', '#0094C5',
              '#5AE7E4', '#2E9F79', '#3638AE', '#FF7F00', '#FA9B97', '#30A02D',
              '#B0E188', '#2077B5', '#05B9C7', '#A8CBE4', '#F5FFB3', '#BEECAF',
              '#FFA1C4', '#9D9DFF', '#01AFEE', '#4574C6', '#FDC100', 
              "#CCFF9A", '#99FFCC',
              '#A589C6', '#FD91A0', '#F2E9DA', '#DFE384', '#39BFCB', '#A6E3E8', 
              '#CC99FF', '#FFAFD7', '#9BCDFF', '#FFD0A1', '#93FFFF', '#9CFF9C')
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
mycol <- c("#F8E58C", "#8CF8AF", "#8C9FF8", "#F88CD5", "#E5F88C", "#8CF8C6", "#8CC7F8", "#F8A98C", 
           "#F5DA5C", "#8CE5F8", "#BA8CF8", "#F88C98", "#D1F88C", "#8CF8DD", "#8C7CF8", "#F8BD8C", 
           "#F8E5E3", "#8CF89F", "#B18CF8", "#F8CA8C", "#78DBFE", "#AFF88C",
           '#B0E188','#71B7F1' ,'#DF97B1')
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
mybgccol <- c("#F5DA5C", "#8CE5F8", "#BA8CF8", "#D1F88C", "#8CF8DD","#F88C98",  "#8C7CF8", "#F8BD8C", 
              "#F8E5E3", "#8CF89F",  "#78DBFE", "#AFF88C",
              "#CC9AFF", "#FF9ACC")
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

