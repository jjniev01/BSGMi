require(ggplot2)
require(rgdal)
require(data.table)
require(gridExtra)




####  BOXPLOTS OF UNIT LEVEL METRICS BY YEAR  ----------------------------------
iso3 <- c("PAN","VNM","CHE", "UGA")
root <- "D:/Research/BSGMiv1a/"

model_tag <- "2000-2005-2010-2015"
##  Dimensions in mm:
width = 95
height = 50 
##Resolution in DPI:
res = 500

##  For every country:
for(i in iso3){
  ##  Set out directory:
  out <- paste0(root,"Figures/",i,"_GUF+/")
  
  ##  Read in the unit level contingency data from all the countries:
  dt <- readRDS(file = paste0(root,"/output/prj_",model_tag,"_",i,
                              "/derived/AdminLvl_Contingencies_MultiModels_",
                              i,".RDS"))
  ##  Go ahead and calculate percent difference in quantity:
  # dt[,PER.QUANT.DIFF:={PRED-OBS}/OBS*100,by=list(GID,YEAR)]
  
  ##  Calculate a grouping factor:
  dt[,MPGROUP := paste(MODEL, PERIOD,sep = " ")]
  
  ##  F1 Plot  ----
  tiff(filename=paste0(out,i,"_F1_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL==model_tag])
  ##    Remove any null records where data is undefined:
  count_dt <- count_dt[!is.na(F1) & !is.na(F1.NULL)]
  ##  Get the count of admin IDs:
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  print(ggplot(dt[ISO==i&MODEL == model_tag & !is.na(F1)&!is.na(F1.NULL)],
               aes(as.factor(YEAR), F1))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("F1 Score")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(F1)&!is.na(F1.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=F1.NULL), color = "red", 
                       na.rm = T)+
          facet_wrap( ~ PERIOD, scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))        
  dev.off()
  
  
  tiff(filename=paste0(out,i,"_Recall_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)])
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  
  print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)],
               aes(as.factor(YEAR),RECALL))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("Recall")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=RECALL.NULL), color = "red", 
                       na.rm=T)+
          facet_wrap( ~ PERIOD,scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))        
  dev.off()
  
  
  tiff(filename=paste0(out,i,"_Precision_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)])
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  
  print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)],
               aes(as.factor(YEAR),PRECISION))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("Precision")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=PRECISION.NULL), color = "red",
                       na.rm=T)+
          facet_wrap( ~ PERIOD,scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))        
  dev.off()
  
  
  
  tiff(filename=paste0(out,i,"_QuantDis_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)&!is.na(QUANT.NULL)])
  count_dt <- count_dt[!is.na(QUANTDIS)]
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)&!is.na(QUANT.NULL)], 
               aes(as.factor(YEAR),QUANTDIS))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("Quantity Disagreement")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)&!is.na(QUANT.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=QUANT.NULL), color = "red")+
          facet_wrap( ~ PERIOD,scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))
  dev.off()
  
  
  tiff(filename=paste0(out,i,"_AllocDis_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(ALLOCDIS)&!is.na(ALLOC.NULL)])
  count_dt <- count_dt[!is.na(ALLOCDIS)]
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(ALLOCDIS)&!is.na(ALLOC.NULL)], aes(as.factor(YEAR),ALLOCDIS))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("Allocation Disagreement")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(ALLOCDIS)&!is.na(ALLOC.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=ALLOC.NULL), color = "red")+
          facet_wrap( ~ PERIOD,scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))      
  dev.off()
  
  
  # tiff(filename=paste0(out,i,"_QuantPerDiff_Boxplots_",model_tag,".tiff"),
  #      width=width,
  #      height=height,
  #      units="mm",
  #      compression="lzw",
  #      bg="white",
  #      res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  # count_dt <- copy(dt[ISO==i&MODEL==model_tag&!is.na(PER.QUANT.DIFF)])
  # count_dt[,COUNT:=.N,by = list(YEAR)]
  # setkey(count_dt, YEAR)
  # cols <- c("YEAR","COUNT","PERIOD")
  # count_dt <- count_dt[,..cols]
  # count_dt <- unique(count_dt)
  # 
  # print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(PER.QUANT.DIFF)],
  #              aes(as.factor(YEAR),PER.QUANT.DIFF))+
  #         geom_hline(yintercept = 0)+
  #         geom_boxplot(outlier.shape = NA)+
  #         geom_text(data = count_dt,
  #                   aes(x = as.factor(YEAR),y = -150, label = COUNT),
  #                   size = 2,
  #                   inherit.aes = F)+
  #         xlab("Year of Prediction")+
  #         ylab("Percent Difference")+
  #         coord_cartesian(ylim = c(-150,200))+
  #         facet_wrap( ~PERIOD,scales = "free_x")+
  #         theme_bw()+
  #         ggtitle(i)+
  #         theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
  #               axis.title.x = element_text(size=8),
  #               axis.title.y = element_text(size=8),
  #               axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
  #               axis.text.y = element_text(size = 7),
  #               strip.text = element_text(size = 8)))
  # 
  # dev.off()
}

##  COUNTRY WIDE COMPARISON METRIC FIGURES
iso3 <- c("PAN","VNM","CHE", "UGA")
root <- "D:/Research/BSGMiv1a/"
out <- paste0(root,"Figures/")
model_tag <- "2000-2005-2010-2015"
##  Dimensions in mm:
width = 90
height = 75
##Resolution in DPI:
res = 500

##  Read in the data:
comp <- read.csv(paste0(root,"CountryLevelPixelComparisons.csv"))
##  Creating a grouping variable:
comp$MPGROUP <- paste0(comp$ISO," ",
                       ifelse(comp$MODEL == "2000-2005-2010-2015",
                       "ESA","GUF+"), " ",
                       ifelse(comp$YEAR < 2005 | comp$MODEL == "2000-2015_GUF+",
                              "1",
                              ifelse(comp$YEAR < 2010 & comp$YEAR > 2005,
                                     "2","3")))
##  Create another grouping variable:
comp$MODGROUP <- paste0(comp$ISO," ",
                        ifelse(comp$MODEL == "2000-2005-2010-2015",
                               "ESA","GUF+"))
##  Create a data table of it:
comp <- as.data.table(comp)
##  Define shapes we want to use:
shapes <- c(22,21,24,25,7)
names(shapes) <- unique(comp$MODGROUP)

tiff(filename=paste0(out,"All_F1_DotPlot_Pixel_Level_CountryWide_",
                     model_tag,".tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

ggplot(as.data.frame(comp),
       aes(x=as.factor(YEAR),
           y=F1, 
           shape = MODGROUP, 
           group = MPGROUP))+
  geom_line(size = 0.2)+
  geom_line(data = as.data.frame(comp),
            aes(x = as.factor(YEAR), 
                y = FSCORE.500, 
                group = MPGROUP),color = "red",
            inherit.aes = F, size = 0.2)+
  geom_point(data = comp,
             aes(x=as.factor(YEAR),y=F1,
                 shape = MODGROUP, group = MPGROUP),size = 1,
             fill="white")+
  geom_point(data = as.data.frame(comp),
             aes(x = as.factor(YEAR), y = FSCORE.500,
                 shape = MODGROUP, group = MPGROUP),
             size = 1, fill="white",
             inherit.aes = F, color = "red", show.legend = F)+
  scale_shape_manual(values = shapes)+
  theme_bw()+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  labs(shape = "Model", x = "Year", y = "F1 Score")

dev.off()


tiff(filename=paste0(out,"All_Quant_DotPlot_Pixel_Level_CountryWide_",
                     model_tag,".tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

ggplot(as.data.frame(comp),
       aes(x=as.factor(YEAR),
           y=QUANTDIS, 
           shape = MODGROUP, 
           group = MPGROUP))+
  geom_line(size=0.2)+
  geom_line(data = as.data.frame(comp),
            aes(x = as.factor(YEAR), 
                y = QUANT.500, 
                group = MPGROUP),color = "red",
            inherit.aes = F, size = 0.2)+
  geom_point(data = comp,
             aes(x=as.factor(YEAR),y=QUANTDIS,
                 shape = MODGROUP, group = MPGROUP),size = 1, fill = "white")+
  geom_point(data = as.data.frame(comp),
             aes(x = as.factor(YEAR), y = QUANT.500,
                 shape = MODGROUP, group = MPGROUP),
             size = 1, fill = "white",
             inherit.aes = F, color = "red", show.legend = F)+
  scale_shape_manual(values = shapes)+
  theme_bw()+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  labs(shape = "Model", x = "Year", y = "Quantity Disagreement")

dev.off()


tiff(filename=paste0(out,"All_AllocationDis_DotPlot_Pixel_Level_CountryWide_",
                     model_tag,".tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

ggplot(as.data.frame(comp),
       aes(x=as.factor(YEAR),
           y=ALLOCDIS, 
           shape = MODGROUP, 
           group = MPGROUP))+
  geom_line(size = 0.2)+
  geom_line(data = as.data.frame(comp),
            aes(x = as.factor(YEAR), 
                y = ALLOC.500, 
                group = MPGROUP),color = "red",
            inherit.aes = F, size = 0.2)+
  geom_point(data = comp,
             aes(x=as.factor(YEAR),y=ALLOCDIS,
                 shape = MODGROUP, group = MPGROUP),size = 1, fill = "white")+
  geom_point(data = as.data.frame(comp),
             aes(x = as.factor(YEAR), y = ALLOC.500,
                 shape = MODGROUP, group = MPGROUP),
             size = 1,
             inherit.aes = F, color = "red", show.legend = F, fill = "white")+
  scale_shape_manual(values = shapes)+
  theme_bw()+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  labs(shape = "Model", x = "Year", y = "Allocation Disagreement")

dev.off()




#####  GUF+ BOXPLOT FIGURES  ---------------------------------------------------
iso3 <- c("VNM")
root <- "D:/Research/BSGMiv1a/"

model_tag <- "2000-2015"
##  Dimensions in mm:
width = 90
height = 50 
##Resolution in DPI:
res = 500


for(i in iso3){
  out <- paste0(root,"Figures/",i,"_GUF+/")
  dt <- readRDS(file = paste0(root,"/output/prj_",model_tag,"_",i,
                              "/derived/AdminLvl_Contingencies_MultiModels_",
                              i,".RDS"))
  #dt[, PER.QUANT.DIFF:={PRED-OBS}/OBS*100,by=list(GID,YEAR)]
  dt[, MPGROUP := paste(MODEL, PERIOD,sep = " ")]
  
  ##  F1 Plot  ----
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL==model_tag])
  count_dt <- count_dt[!is.na(F1) & !is.na(F1.NULL)]
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  f1plot <- ggplot(dt[ISO==i&MODEL == model_tag & !is.na(F1)&!is.na(F1.NULL)],
                   aes(as.factor(YEAR), F1))+
    geom_boxplot(outlier.shape = NA)+
    xlab("")+
    ylab("F1 Score")+
    geom_text(data = count_dt,
              aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
              size = 2,
              inherit.aes = F)+
    stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(F1)&!is.na(F1.NULL)],
                 fun.y = median, geom= "point", shape=4,size = 3,
                 aes(x=as.factor(YEAR), y=F1.NULL), color = "red", 
                 na.rm = T)+
    theme_bw()+
    ylim(-0.11,0.6)+
    theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size=8),
          axis.title.y = element_text(size=8),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 8))        

  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)&!is.na(QUANT.NULL)])
  count_dt <- count_dt[!is.na(QUANTDIS)]
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  quantplot <- ggplot(dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)&!is.na(QUANT.NULL)], 
                      aes(as.factor(YEAR),QUANTDIS))+
    geom_boxplot(outlier.shape = NA)+
    xlab("Prediction Year")+
    ylab("Quantity Disagreement")+
    geom_text(data = count_dt,
              aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
              size = 2,
              inherit.aes = F)+
    stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)&!is.na(QUANT.NULL)],
                 fun.y = median, geom= "point", shape=4,size = 3,
                 aes(x=as.factor(YEAR), y=QUANT.NULL), color = "red")+
    theme_bw()+
    ylim(-0.11,0.6)+
    theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size=8),
          axis.title.y = element_text(size=8),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 8))
  

  
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(ALLOCDIS)&!is.na(ALLOC.NULL)])
  count_dt <- count_dt[!is.na(ALLOCDIS)]
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  allocplot <- ggplot(dt[ISO==i&MODEL == model_tag&!is.na(ALLOCDIS)&!is.na(ALLOC.NULL)], aes(as.factor(YEAR),ALLOCDIS))+
    geom_boxplot(outlier.shape = NA)+
    xlab("")+
    ylab("Allocation Disagreement")+
    geom_text(data = count_dt,
              aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
              size = 2,
              inherit.aes = F)+
    stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(ALLOCDIS)&!is.na(ALLOC.NULL)],
                 fun.y = median, geom= "point", shape=4,size = 3,
                 aes(x=as.factor(YEAR), y=ALLOC.NULL), color = "red")+
    theme_bw()+
    ylim(-0.11,0.6)+
    theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
          axis.title.x = element_text(size=8),
          axis.title.y = element_text(size=8),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 8))
  
  tiff(filename=paste0(out,i,"_GUF+_F1_Quant_Alloc_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  print(grid.arrange(f1plot,quantplot,allocplot,nrow=1))
  
  dev.off()
  
  
  
  
  tiff(filename=paste0(out,i,"_Recall_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)])
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  
  print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)],
               aes(as.factor(YEAR),RECALL))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("Recall")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=RECALL.NULL), color = "red", 
                       na.rm=T)+
          facet_wrap( ~ PERIOD,scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))        
  dev.off()
  
  
  tiff(filename=paste0(out,i,"_Precision_Boxplots_",model_tag,".tiff"),
       width=width,
       height=height,
       units="mm",
       compression="lzw",
       bg="white",
       res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)])
  count_dt[,COUNT:=.N,by = list(YEAR)]
  setkey(count_dt, YEAR)
  cols <- c("YEAR","COUNT","PERIOD")
  count_dt <- count_dt[,..cols]
  count_dt <- unique(count_dt)
  
  
  print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)],
               aes(as.factor(YEAR),PRECISION))+
          geom_boxplot(outlier.shape = NA)+
          xlab("Year of Prediction")+
          ylab("Precision")+
          geom_text(data = count_dt,
                    aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
                    size = 2,
                    inherit.aes = F)+
          stat_summary(data=dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)],
                       fun.y = median, geom= "point", shape=4,size = 3,
                       aes(x=as.factor(YEAR), y=PRECISION.NULL), color = "red",
                       na.rm=T)+
          facet_wrap( ~ PERIOD,scales = "free_x")+
          theme_bw()+
          ggtitle(i)+
          theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
                axis.title.x = element_text(size=8),
                axis.title.y = element_text(size=8),
                axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 7),
                strip.text = element_text(size = 8)))        
  dev.off()
}




####  BOXPLOT OF ALL YEARS AND MODELS
iso3 <- c("PAN","VNM","CHE", "UGA")
root <- "D:/Research/BSGMiv1a/"

model_tag <- "2000-2005-2010-2015"
##  Dimensions in mm:
width = 180
height = 90 
##Resolution in DPI:
res = 500

out <- paste0(root,"Figures/")

##  Bring in the subnational level data for each country and put it in one 
##  big data.table:
for(i in 1:length(iso3)){
  ##  Bring in the subnational level info:
  dt <- readRDS(file = paste0(root,"/output/prj_",model_tag,"_",iso3[i],
                              "/derived/AdminLvl_Contingencies_MultiModels_",
                              iso3[i],".RDS"))
  #dt[,PER.QUANT.DIFF:={PRED-OBS}/OBS*100,by=list(GID,YEAR)]
  ##  Create a grouping variable:
  dt[,MPGROUP := paste(MODEL, PERIOD,sep = " ")]
  
  ##  If we are working with Vietnam, make sure to grab the GUF+ model run as 
  ##  well which doesn't meet the above file name configuration:
  if(iso3[i]=="VNM"){
    foodt <- readRDS(file = paste0(root,"/output/prj_2000-2015_",iso3[i],
                                   "/derived/AdminLvl_Contingencies_MultiModels_",
                                   iso3[i],".RDS"))
    foodt[,PER.QUANT.DIFF:={PRED-OBS}/OBS*100,by=list(GID,YEAR)]
    foodt[,MPGROUP := paste(MODEL, PERIOD,sep = " ")]
    ##  Attach to the main dt table:
    dt <- rbind(dt,copy(foodt))
  }
  
  if(i == 1){
    ##  If the first country, put the dt as the carryover dt:
    aggdt <- copy(dt)
  }else{
    ##  Just append the data:
    aggdt <- rbind(aggdt,copy(dt))
  }
}
##  Make sure the factors are factors:
aggdt$ISO <- as.factor(aggdt$ISO)
aggdt$MODEL <- as.factor(aggdt$MODEL)
aggdt$PERIOD <- as.factor(aggdt$PERIOD)
aggdt$MPGROUP <- as.factor(aggdt$MPGROUP)



##  Pixel level data  ----
##  Bring in all pixel level data that already has annual data calculated:
pixdt <- as.data.table(read.csv(file=paste0(root,
                                            "output/CountryLevelPixelComparisons.csv")))
  # readRDS(file=paste0(root, 
  #                    "output/All_Pixel_Level_Contingency_Data_CHE_PAN_UGA_VNM_",
  #                    "2018-10-23.RDS"))
pixdt$ISO <- as.factor(pixdt$ISO)
pixdt$MODEL <- as.factor(pixdt$MODEL)
##  Create a grouping factor:
pixdt$UNIGROUP <- as.factor(paste0(pixdt$ISO," ",pixdt$MODEL))
##  Rename the levels:
levels(pixdt$UNIGROUP)<- c("CHE ESA","PAN ESA","UGA ESA","VNM ESA","VNM GUF Evo.")

##  Aggregate by YEAR and Model:
pixdt <- pixdt[,list(TP = sum(TP, na.rm=T),
                     FP = sum(FP, na.rm=T),
                     FN = sum(FN, na.rm=T),
                     TN = sum(TN, na.rm=T)),
               by=list(UNIGROUP,YEAR)]
##  Ensure numeric:
pixdt$TP <- as.numeric(pixdt$TP)
pixdt$FP <- as.numeric(pixdt$FP)
pixdt$TN <- as.numeric(pixdt$TN)
pixdt$FN <- as.numeric(pixdt$FN)

##  Calculate metrics:
pixdt[, RECALL := TP/{TP+FN}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, PRECISION := TP/{TP+FP}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, F1 := {2*(PRECISION*RECALL)/(PRECISION+RECALL)}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, SPECIFICITY := {TN/(FP+TN)}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, OVERACC := {{TN+TP}/(TP+FN+FP+TN)}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, QUANTDIS.0 := {abs({FN-FP}/(TP+FP+FN+TN))}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, QUANTDIS.1 := {abs({FP-FN}/(TP+FP+FN+TN))}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, QUANTDIS := {{QUANTDIS.0+QUANTDIS.1}/2}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, ALLOCDIS.0 := {2*min({FP/{TP+TN+FP+FN}}, {FN/{TP+TN+FP+FN}})},
      by=list(UNIGROUP,YEAR)]
pixdt[, ALLOCDIS.1 := {2*min({FN/{TP+TN+FP+FN}}, {FP/{TP+TN+FP+FN}})},
      by=list(UNIGROUP,YEAR)]
pixdt[, ALLOCDIS := {ALLOCDIS.0+ALLOCDIS.1}/2, 
      by=list(UNIGROUP,YEAR)]
pixdt[, PROP.CORRECT := {{TN+TP}/{TP+TN+FP+FN}}, 
      by=list(UNIGROUP,YEAR)]


##  Create a dataset in long format, for graphic, disagreement metrics (Pontius)
dt <- copy(pixdt[UNIGROUP != "CNM GUF Evo."])
##  Melt it:
dt <- melt(dt, id.vars = c("UNIGROUP","YEAR"),
           measure.vars = c("QUANTDIS","ALLOCDIS"))
levels(dt$variable) <- c("Quantity","Allocation")

##  Get null values:
nulldt <- as.data.table(read.csv(file=paste0(root,"output/CountryLevelPixelComparisons.csv")))
nulldt$UNIGROUP <- as.factor(paste0(nulldt$ISO," ",nulldt$MODEL))
levels(nulldt$UNIGROUP)<- c("CHE ESA","PAN ESA","UGA ESA","VNM ESA", "VNM GUF Evo")
nulldt <- nulldt[,list(UNIGROUP=UNIGROUP,YEAR=YEAR,QUANT.500=QUANT.500,ALLOC.500=ALLOC.500)]
nulldt<-nulldt[UNIGROUP!="VNM GUF Evo."]
nullmelt<-melt(nulldt,id.vars = c("UNIGROUP","YEAR"),
               measure.vars = c("QUANT.500","ALLOC.500"))
levels(nullmelt$UNIGROUP)<- c("CHE ESA","PAN ESA","UGA ESA","VNM ESA", "VNM GUF Evo.")
levels(nullmelt$variable)<-c("Quantity, Null","Allocation, Null")

dt <- rbind(dt,copy(nullmelt))
dt[variable == "Quantity, Null"|variable =="Allocation, Null",GROUP:=1]
dt[variable != "Quantity, Null"&variable !="Allocation, Null",GROUP:=2]


##  Go through all the model types to create a custom gridded map
che_plot <- ggplot()+
  geom_col(data = dt[UNIGROUP == "CHE ESA"],
           aes(x=GROUP,
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.5, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3"))

pan_plot <- ggplot()+
  geom_col(data = dt[UNIGROUP == "PAN ESA"],
           aes(x=GROUP,
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.5, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3"))
  
uga_plot <- ggplot()+
  geom_col(data = dt[UNIGROUP == "UGA ESA"],
           aes(x=GROUP,
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.5, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3"))      

vnm_plot <- ggplot()+
  geom_col(data = dt[UNIGROUP == "VNM ESA"],
           aes(x=GROUP,
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.5, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3"))

vnm_g_plot <- ggplot()+
  geom_col(data = dt[UNIGROUP == "VNM GUF Evo."],
           aes(x=GROUP,
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.5, show.legend = T)+
  facet_wrap( ~ YEAR, ncol = 12)+
  theme_bw()+
  labs(fill = "Disagreement Type")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.height = unit(2, "mm"),
        legend.key.width = unit(3,"mm"),
        legend.spacing = unit(1,"mm"),
        legend.text = element_text(size =6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3"))

tiff(filename=paste0(out,"All_ISO_Years_Pixel_Disagreement Bars.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

grid.arrange(che_plot,pan_plot,uga_plot,vnm_plot,#vnm_g_plot,
             layout_matrix=rbind(c(1,1,1,1,1,1,1,1,1,1,1,1),
                                 c(2,2,2,2,2,2,2,2,2,2,2,2),
                                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                                 c(4,4,4,4,4,4,4,4,4,4,4,4)
                                 ))


dev.off()


##  Pixel Level F1 Bar Chart ----
##  Bring in all pixel level data that already has annual data calculated:
pixdt <- as.data.table(read.csv(file=paste0(root,"output/CountryLevelPixelComparisons.csv")))
# readRDS(file=paste0(root, 
#                    "output/All_Pixel_Level_Contingency_Data_CHE_PAN_UGA_VNM_",
#                    "2018-10-23.RDS"))
pixdt$ISO <- as.factor(pixdt$ISO)
pixdt$MODEL <- as.factor(pixdt$MODEL)
pixdt$UNIGROUP <- as.factor(paste0(pixdt$ISO," ",pixdt$MODEL))
##  Rename the levels:
levels(pixdt$UNIGROUP)<- c("CHE ESA","PAN ESA","UGA ESA","VNM ESA","VNM GUF Evo.")

##  Aggregate by YEAR:
pixdt <- pixdt[,list(TP = sum(TP, na.rm=T),
                     FP = sum(FP, na.rm=T),
                     FN = sum(FN, na.rm=T),
                     TN = sum(TN, na.rm=T)),
               by=list(UNIGROUP,YEAR)]
pixdt$TP <- as.numeric(pixdt$TP)
pixdt$FP <- as.numeric(pixdt$FP)
pixdt$TN <- as.numeric(pixdt$TN)
pixdt$FN <- as.numeric(pixdt$FN)

pixdt[, RECALL := TP/{TP+FN}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, PRECISION := TP/{TP+FP}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, F1 := {2*(PRECISION*RECALL)/(PRECISION+RECALL)}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, SPECIFICITY := {TN/(FP+TN)}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, OVERACC := {{TN+TP}/(TP+FN+FP+TN)}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, QUANTDIS.0 := {abs({FN-FP}/(TP+FP+FN+TN))}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, QUANTDIS.1 := {abs({FP-FN}/(TP+FP+FN+TN))}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, QUANTDIS := {{QUANTDIS.0+QUANTDIS.1}/2}, 
      by=list(UNIGROUP,YEAR)]
pixdt[, ALLOCDIS.0 := {2*min({FP/{TP+TN+FP+FN}}, {FN/{TP+TN+FP+FN}})},
      by=list(UNIGROUP,YEAR)]
pixdt[, ALLOCDIS.1 := {2*min({FN/{TP+TN+FP+FN}}, {FP/{TP+TN+FP+FN}})},
      by=list(UNIGROUP,YEAR)]
pixdt[, ALLOCDIS := {ALLOCDIS.0+ALLOCDIS.1}/2, 
      by=list(UNIGROUP,YEAR)]
pixdt[, PROP.CORRECT := {{TN+TP}/{TP+TN+FP+FN}}, 
      by=list(UNIGROUP,YEAR)]


##  Create a dataset of long format disagreement metrics (Pontius)
dt <- copy(pixdt[UNIGROUP !="VNM GUF Evo."])

##  Get null values:
nulldt <- as.data.table(read.csv(file=paste0(root,"output/CountryLevelPixelComparisons.csv")))
nulldt$UNIGROUP <- as.factor(paste0(nulldt$ISO," ",nulldt$MODEL))
levels(nulldt$UNIGROUP)<- c("CHE ESA","PAN ESA","UGA ESA","VNM ESA", "VNM GUF Evo.")
nulldt <- nulldt[,list(UNIGROUP=UNIGROUP,YEAR=YEAR,FSCORE.500=FSCORE.500)]
names(nulldt)[3]<-"F1.NULL"
nulldt <- nulldt[UNIGROUP!="VNM GUF Evo."]
setkey(nulldt,UNIGROUP,YEAR)
setkey(dt,UNIGROUP,YEAR)
dt <- dt[nulldt, names(nulldt)[3]:=nulldt[,.SD,.SDcols=3]]
dtmelt <- melt(dt,id.vars = c("UNIGROUP","YEAR"),
               measure.vars = c("F1","F1.NULL"))
levels(dtmelt$variable) <- c("BSGM","Null Model")

##  Go through all the model types to create a custom gridded map
che_plot <- ggplot()+
  geom_col(data = dtmelt[UNIGROUP == "CHE ESA"],
           aes(x=as.factor(YEAR),
               y=value,
               fill=variable),
           position = "dodge",color="grey20", show.legend = F)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("F1 Score")+ 
  scale_fill_manual(values = c("#8DD3C7","#FFFFB3"))

pan_plot <- ggplot()+
  geom_col(data = dtmelt[UNIGROUP == "PAN ESA"],
           aes(x=as.factor(YEAR),
               y=value,
               fill=variable),
           position = "dodge",color="grey20", show.legend = F)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("F1 Score")+ 
  scale_fill_manual(values = c("#8DD3C7","#FFFFB3"))

uga_plot <- ggplot()+
  geom_col(data = dtmelt[UNIGROUP == "UGA ESA"],
           aes(x=as.factor(YEAR),
               y=value,
               fill=variable),
           position = "dodge",color="grey20", show.legend = F)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("F1 Score")+ 
  scale_fill_manual(values = c("#8DD3C7","#FFFFB3"))

vnm_plot <- ggplot()+
  geom_col(data = dtmelt[UNIGROUP == "VNM ESA"],
           aes(x=as.factor(YEAR),
               y=value,
               fill=variable),
           position = "dodge",color="grey20", show.legend = F)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("F1 Score")+ 
  scale_fill_manual(values = c("#8DD3C7","#FFFFB3"))

# vnm_g_plot <- ggplot()+
#   geom_col(data = dtmelt[UNIGROUP == "VNM GUF Evo."],
#            aes(x=as.factor(YEAR),
#                y=value,
#                fill=variable),
#            position = "dodge",color="grey20", show.legend = T)+
#   theme_bw()+
#   labs(fill = "Model")+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_text(size=6),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.y = element_text(size = 6),
#         strip.text = element_text(size = 6),
#         legend.title = element_text(size = 7),
#         legend.key.height = unit(2, "mm"),
#         legend.key.width = unit(3,"mm"),
#         legend.spacing = unit(1,"mm"),
#         legend.text = element_text(size =6),
#         strip.background=element_rect(fill = "white"))+
#   xlab("Year")+
#   ylab("F1 Score")+ 
#   scale_fill_manual(values = c("#8DD3C7","#FFFFB3"))

tiff(filename=paste0(out,"All_ISO_Years_Pixel_F1_Bars.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

grid.arrange(che_plot,pan_plot,uga_plot,vnm_plot,#vnm_g_plot,
             layout_matrix=rbind(c(1,1,1,1,1,1,1,1,1,1,1,1),
                                 c(2,2,2,2,2,2,2,2,2,2,2,2),
                                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                                 c(4,4,4,4,4,4,4,4,4,4,4,4)))


dev.off()