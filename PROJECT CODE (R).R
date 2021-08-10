#Atur direktori
getwd()
setwd("C:/Users/vania frederica/Documents/Semester 4/Analisis Data Eksploratif/EAS PROJECT")

#Baca data
df=read.csv('Placement_Data_Full_Class.csv', sep=',', header=T)
df=df[,c(1,2,8,10,11,12,13,14,15)]

#Preprocessing data
df1<- within(df, {
  gender<- factor(gender, labels = c("Perempuan", "Laki-Laki"))
  workex <- factor(workex,labels=c("Ya","Tidak")) 
  specialisation<- factor(specialisation)
  status <- factor(status,labels=c("Tidak ditempatkan","Ditempatkan"))  
})

#Mereplace nilai missing pada salary dengan nilai 0 karena artinya belum bekerja
df1$salary[is.na(df1$salary)]<-0

#Data sudah lengkap dan siap untuk di eksplor lebih lanjut
str(df1)

#Memanggil Library
packages = c('CCA','dplyr','ggplot2','RColorBrewer','tidyverse','ggthemes','forcats','tidyr','patchwork','plyr', 'psych','dplyr', 'vcd')
lapply(packages, library, character.only = T)

#membagi salary dalam ribuan supaya y axis plot dapat tervisualisasi dengan baik
df1$salary=(df1$salary/1000)
str(df1)

#tema
tema=function() {
  theme(plot.title = element_text(size = 24, face = 'bold'),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 11, color = 'blue'),
        axis.text.x  = element_text(size = 12, margin = margin(12, b = 12), color = 'black'),
        axis.title.y = element_text(size = 14, face = 'bold', vjust = 1, margin = margin(l = 12,r=12)),
        axis.title.x = element_text(size = 14, face = 'bold', vjust = 1),
        axis.text.y = element_text(size = 11, color = 'black'),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"))
}

#VISUALISASI 1 PAIRS PANEL
pairs.panels(df1[ ,c(3,7,5)], 
             method = 'pearson', # correlation method
             hist.col = '#fa4b84',
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main = 'Pair plot')

df_2=df1
df_2$status=factor(df_2$status,labels=c("0","1"))
df_2$status=as.numeric(df_2$status)
cor.test( df_2$status, df_2$degree_p)

df_2$status=factor(df_2$status,labels=c("0","1"))
df_2$status=as.numeric(df_2$status)
cor.test( df_2$status, df_2$mba_p)

#Visualisasi 2 COUNT PLOT 
df2=df1%>%
  group_by(status,gender) %>% 
  dplyr :: summarise(etest_p = round(mean(etest_p),2))

ggplot(df2,aes(x=status,y=etest_p,fill=gender,label=etest_p))+
  geom_bar(stat="identity", width=0.6) +
  theme_minimal() +
  tema()+
  labs(x = 'Status Penempatan',
       y = 'Rata-Rata Nilai Tes Kelayakan Kerja',
       title = 'Rata-Rata Tes Kelayakan Kerja',
       subtitle = 'Berdasarkan Status Penempatan dan Gender',
       caption = 'Source: studentperformance, kaggle.com')+
  geom_text(size = 6, position = position_stack(vjust = 0.5),color="black",fontface="bold") +
  scale_fill_manual(values=c('#fa4b84','#fbb146')) +
  theme(legend.position = "bottom")

df_2=df1
df_2$status=factor(df_2$status,labels=c("0","1"))
df_2$status=as.numeric(df_2$status)
cor.test( df_2$status, df_2$etest_p)


#Visualisasi 3 dan 4  facet staked bar plot
df2=df1%>%
  group_by(workex,status,gender) %>% 
  dplyr :: count(workex,sort=TRUE)


ggplot(df2,aes(x=workex,y=n,fill=status,label=n))+geom_bar(stat="identity")+facet_grid(.~gender) +
  theme_igray() + 
  labs(x = 'Pengalaman Kerja',
       y = 'Frekuensi',
       title = 'Jumlah Status Penempatan',
       subtitle = 'Berdasarkan Pengalaman Kerja dan Jenis Kelamin',
       caption = 'Source: studentperformance, kaggle.com') +
  scale_y_continuous('Frekuensi', 
                     breaks = seq(0, 100, 10),
                     limits = c(0, 100)) + 
  scale_fill_manual(values=c('#fa4b84','#fbb146')) + theme_bw()+
  tema()+geom_text(size = 4.5, position = position_stack(vjust = 0.5),fontface="bold",color="black") +
  theme(legend.position = "bottom")

data_xtabs1 = xtabs(~df1$gender + df1$status)
data_xtabs1
assocstats(data_xtabs1)

data_xtabs = xtabs(~df1$workex + df1$status)
data_xtabs
assocstats(data_xtabs)

#Visualisasi 5 PIE CHART
df4=df1%>%
  filter(specialisation=="Mkt&Fin")%>%
  group_by(status) %>% 
  dplyr :: count(status,sort=TRUE)%>%
  mutate(persen=round((n/sum(n))*100,2))

pie1=ggplot(df4, aes(x = "", y = persen, fill = status)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = cumsum(persen) - 0.5*persen ,label = paste(persen,"%")), color = "white")+
  scale_fill_manual(values = c('#fa4b84','#fbb146')) +
  theme_void()+theme(legend.position="bottom") +labs(title="Marketing and Finance")+
  theme(plot.title = element_text( size=20, face="bold"))



df5=df1%>%
  filter(specialisation=="Mkt&HR")%>%
  group_by(status) %>% 
  dplyr :: count(status,sort=TRUE)%>%
  mutate(persen=round((n/sum(n))*100,2))

pie2=ggplot(df5, aes(x = "", y = persen, fill = status)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = cumsum(persen) - 0.5*persen ,label = paste(persen,"%")), color = "white")+
  scale_fill_manual(values = c('#fa4b84','#fbb146')) +
  theme_void()+theme(legend.position="none") +labs(title="Marketing and HR")+
  theme(plot.title = element_text( size=20, face="bold"))


pie1+pie2

data_xtabs2 = xtabs(~df1$specialisation + df1$status)
data_xtabs2
assocstats(data_xtabs2)



#Visualisasi 6  violin plot

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


df1%>%
  filter(df1$salary!=0)%>%
  ggplot(aes(x=specialisation, y=salary,fill=gender, color=gender)) + 
  geom_split_violin(trim=FALSE) +  scale_fill_manual(values=c('#fa4b84','#fbb146'))+ scale_color_manual(values=c('black','black'))+
  geom_boxplot(width=0.1,notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0 )+
  theme_minimal() +
  tema() +
  labs(x = 'Spesialisasi', y = 'Gaji', title = "Distribusi Gaji",
       subtitle = 'Berdasarkan Spesialisasi dan Jenis Kelamin',
       caption = 'Source: studentperformance, kaggle.com') +
  scale_y_continuous('Gaji', 
                     breaks = seq(100, 1000, 100),
                     limits = c(100, 1000)) +
  theme(legend.position = "bottom")


df_2=df1
df_2$status=factor(df_2$status,labels=c("0","1"))
df_2$status=as.numeric(df_2$status)
cor.test( df_2$status, df_2$salary)

df_2$gender=factor(df_2$gender,labels=c("0","1"))
df_2$gender=as.numeric(df_2$gender)
cor.test( df_2$gender, df_2$salary)


