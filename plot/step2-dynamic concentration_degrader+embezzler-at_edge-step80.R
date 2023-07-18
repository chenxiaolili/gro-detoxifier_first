
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path1 <-getwd()

folderpath <- paste(as.character(path1), "/","data11-dynamic concentraion at edge/", sep="")
setwd(folderpath) 

datacon_de<-read.csv("plot-Concenlist_time-at_edge-degrader.csv",header = T)
datacon_em<-read.csv("plot-Concenlist_time-at_edge-embezzler.csv",header = T)

datacon_de$strain <- c("de")
datacon_em$strain <- c("em")
datacon<-rbind(datacon_de,datacon_em)
write.csv(datacon,"plot-Concenlist_time-at_edge-all.csv", row.names=FALSE)

library(Rmisc)
library(ggplot2)
library(viridis)
mass_list <-c("Sout","Sin","Iout","Iin","Pout","Pin","mu","cell_num","Stotal","Itotal","Ptotal")

for(m in 1:length(mass_list)){
  df<-datacon[which(datacon$mass==mass_list[[m]]),]
  par(mar=c(5,7,2,2))
  p <-ggplot(data=df, aes(x=rela_time, y=concentration, colour = as.factor(substrate),fill=as.factor(substrate),linetype=as.factor(strain))) + 
    geom_line(size=1)+
    geom_ribbon(aes(ymin = concentration - sd,
                    ymax = concentration + sd,fill=as.factor(substrate)),
                alpha=0.2,linetype = 0)+
    scale_fill_manual(values = c('#8cadeb',"#de9a7e",'#50bdaf','#cf99d3'),
                      limits = c("5","10","15","20"))+
    scale_color_manual(values = c('#8cadeb',"#de9a7e",'#50bdaf','#cf99d3'),
                       limits = c("5","10","15","20"))+
    labs(x="Relative time",y="Concentration (C-mM)")+
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'))+
    theme(axis.text= element_text(size=14,color="black"),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))#+
  #theme(legend.position = 'none')
  dev.off()
  mass_name <- paste("Concenlist_time-at_edge-all-", mass_list[[m]],".pdf",sep="")
  ggsave(mass_name,height=6,width=8)
  dev.new()
}




datacon_de_1<-read.csv("Concenlist_time-at_edge-degrader.csv",header = T)
datacon_em_1<-read.csv("Concenlist_time-at_edge-embezzler.csv",header = T)
datacon_de_2<-datacon_de_1[,5:(ncol(datacon_de_1))]
datacon_em_2<-datacon_em_1[,5:(ncol(datacon_em_1))]
datacon_merge <- merge(datacon_de_2,datacon_em_2,by=c("time", "rela_time","substrate","replicate"),all=T)

names(datacon_merge)[5:ncol(datacon_merge)] <- c("Sout_de","Sin_de","Iout_de","Iin_de","Pout_de","Pin_de","mu_de","cell_num_de","Stotal_de","Itotal_de","Ptotal_de","Sout_em","Sin_em","Iout_em","Iin_em","Pout_em","Pin_em","mu_em","cell_num_em","Stotal_em","Itotal_em","Ptotal_em")
datacon_merge <- data.frame(apply(datacon_merge,2,function(x) as.numeric(as.character(x))))
datacon_merge_ratio <- NULL
datacon_merge_ratio<-cbind(datacon_merge_ratio,
                           time=datacon_merge$time,
                           rela_time=datacon_merge$rela_time,
                           substrate=datacon_merge$substrate,
                           replicate=datacon_merge$replicate,
                           Sout_ratio= round((datacon_merge$Sout_de)/(datacon_merge$Sout_em),4),
                           Sin_ratio= round((datacon_merge$Sin_de)/(datacon_merge$Sin_em),4),
                           Iout_ratio= round((datacon_merge$Iout_de)/(datacon_merge$Iout_em),4),
                           Iin_ratio= round((datacon_merge$Iin_de)/(datacon_merge$Iin_em),4),
                           Pout_ratio= round((datacon_merge$Pout_de)/(datacon_merge$Pout_em),4),
                           Pin_ratio= round((datacon_merge$Pin_de)/(datacon_merge$Pin_em),4),
                           mu_ratio= round((datacon_merge$mu_de)/(datacon_merge$mu_em),4),
                           Stotal_ratio= round((datacon_merge$Stotal_de)/(datacon_merge$Stotal_em),4),
                           Itotal_ratio= round((datacon_merge$Itotal_de)/(datacon_merge$Itotal_em),4),
                           Ptotal_ratio= round((datacon_merge$Ptotal_de)/(datacon_merge$Ptotal_em),4),
                           de_abundance=round((datacon_merge$cell_num_de)/((datacon_merge$cell_num_de)+(datacon_merge$cell_num_em)),4)
)	

datacon_merge_ratio<- as.data.frame(datacon_merge_ratio)
write.csv(datacon_merge_ratio,"Concenratio_list-time-at_edge.csv", row.names=FALSE)

ratio_list <- c("Sout_ratio","Sin_ratio","Iout_ratio","Iin_ratio","Pout_ratio","Pin_ratio","mu_ratio","Stotal_ratio","Itotal_ratio","Ptotal_ratio","de_abundance")
df_ratio_ave <-NULL
library(Rmisc)
library(ggplot2)
for(m in 1:length(ratio_list)){
  df_ratio<-summarySE(datacon_merge_ratio,measurevar=ratio_list[[m]],groupvars=c("rela_time","substrate"))
  par(mar=c(5,7,2,2))
  p <-ggplot(data=df_ratio, aes(x=rela_time, y=df_ratio[,ratio_list[[m]]], colour = as.factor(substrate),fill=as.factor(substrate))) + 
    geom_line(size=1)+
    geom_ribbon(aes(ymin = df_ratio[,ratio_list[[m]]] - sd,
                    ymax = df_ratio[,ratio_list[[m]]] + sd,fill=as.factor(substrate)),
                alpha=0.2,linetype = 0)+
    scale_fill_manual(values = c('#8cadeb',"#de9a7e",'#50bdaf','#cf99d3'),
                      limits = c("5","10","15","20"))+
    scale_color_manual(values = c('#8cadeb',"#de9a7e",'#50bdaf','#cf99d3'),
                       limits = c("5","10","15","20"))+
    labs(x="Relative time",y="degrader/consumer")+
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'))+
    theme(axis.text= element_text(size=14,color="black"),
          axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))#+
  #theme(legend.position = 'none')
  dev.off()
  mass_name <- paste("Concenratio_list-time-at_edge-", ratio_list[[m]],".pdf",sep="")
  ggsave(mass_name,height=6,width=8)
  dev.new()
  names(df_ratio)[4:5] <-c("ratio_ave","ratio_sd")
  df_ratio_ave <- rbind(df_ratio_ave,cbind(df_ratio[1:5],mass=ratio_list[[m]]))
}
write.csv(df_ratio_ave,"plot-Concenratio_list-time-at_edge.csv", row.names=FALSE)



