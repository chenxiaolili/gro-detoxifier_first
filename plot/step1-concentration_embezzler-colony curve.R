
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path1 <-getwd()

library(tidyr)
library(dplyr)
Alldata <- NULL
substrate_list <- c("5","10","15","20")
for(j in 1:length(substrate_list)){
folderpath <- paste(as.character(path1), "/","data11/",as.character(substrate_list[[j]]),sep="")
setwd(folderpath) 
for(k in 1:3){
##position
datap_de_name <- paste("data1p",k,".csv",sep="")
datap_de<-read.table(datap_de_name,header = F)
datap_de_final<-datap_de[nrow(datap_de),]
datap_de_final_1 <-data.frame(datap_de_final)
#拆分
datap_de_final_2 <-separate(datap_de_final_1,datap_de_final, c("time", "pos"),",{{")
datap_de_final_3<-data.frame(datap_de_final_2[,2])
colnames(datap_de_final_3)<-c("pos")
datap_de_final_4<-separate_rows(datap_de_final_3,pos,sep="},{",convert = F)
datap_de_final_5 <-separate(datap_de_final_4,pos, c("ID", "pos1","pos2","pos3"),",")
datap_de_final_5$pos3<-gsub("}}}","",datap_de_final_5$pos3)
##mass concentration
datacon_de_name <- paste("dataE",k,".txt",sep="")
datacon_de<-read.delim(datacon_de_name,header = F,sep="\t",row.names=NULL)
datacon_de_final<-datacon_de[nrow(datacon_de),]
datacon_de_final_1 <-data.frame(datacon_de_final)

datacon_de_final_2 <-separate(datacon_de_final_1,datacon_de_final, c("time", "pos"),",{{")
datacon_de_final_3<-data.frame(datacon_de_final_2[,2])
colnames(datacon_de_final_3)<-c("pos")
datacon_de_final_4<-separate_rows(datacon_de_final_3,pos,sep="},{",convert = F)
datacon_de_final_5 <-separate(datacon_de_final_4,pos, c("ID", "Sout","Sin","Iout","Iin","Pout","Pin","mu"),",")
datacon_de_final_5$mu<-gsub("}}}","",datacon_de_final_5$mu)

data_de <-merge(datap_de_final_5,datacon_de_final_5,by="ID",all=T)

data_de <- data.frame(lapply (data_de,as.numeric))
step_length=40
radius <- sqrt((data_de$pos1)^2+(data_de$pos2)^2)

for (q in 1:(840/step_length)) {
  data_de_q <-data_de[which(radius>(q-1)*step_length & radius<=(q)*step_length),] 
  ave_col<-as.data.frame(colMeans(data_de_q))
#  sd_col<-colsd(data_de_q)
# Alldata <- rbind(Alldata, cbind(ave_col, sd_col))
  Alldata <- rbind(Alldata, cbind(t(ave_col), radius=q, replicate=k,substrate=substrate_list[[j]],cell_num=nrow(data_de_q)))
  }
}
#colnames(Alldata)<-c("ID_ave", "pos1_ave","pos2_ave","pos3_ave" ,"Sout_ave","Sin_ave","Iout_ave","Iin_ave","Pout_ave","Pin_ave","mu_ave",
#                     "pos1_sd","pos2_sd","pos3_sd" "Sout_sd","Sin_sd","Iout_sd","Iin_sd","Pout_sd","Pin_sd","mu_sd")
}

Alldata <- data.frame(apply(Alldata,2,function(x) as.numeric(as.character(x))))
Alldata$Stotal <- Alldata$Sin +Alldata$Sout
Alldata$Itotal <- Alldata$Iin +Alldata$Iout
Alldata$Ptotal <- Alldata$Pin +Alldata$Pout
con_name <- paste("Concenlist_diameter-embezzler", ".csv",sep="")
folderpath2 <- paste(as.character(path1), "/","data11-concentration curve/",sep="")
setwd(folderpath2) 
write.csv(Alldata,con_name, row.names=FALSE)

Alldata<-read.csv("Concenlist_diameter-embezzler.csv",header = T)

library(Rmisc)
library(ggplot2)
library(viridis)
mass_list <-c("Sout","Sin","Iout","Iin","Pout","Pin","mu","cell_num","Stotal","Itotal","Ptotal")
df_all <-NULL
for(m in 1:length(mass_list)){
df<-summarySE(Alldata,measurevar=mass_list[[m]],groupvars=c("radius","substrate"))
df1 <-df[which(df$radius>=8),]
par(mar=c(5,7,2,2))
p <-ggplot(data=df1, aes(x=radius, y=as.numeric(df1[,mass_list[[m]]]), colour = as.factor(substrate),fill=as.factor(substrate))) + 
  geom_line(size=1)+
  geom_ribbon(aes(ymin = as.numeric(df1[,mass_list[[m]]]) - sd,
                  ymax = as.numeric(df1[,mass_list[[m]]]) + sd,fill=as.factor(substrate)),
              alpha=0.2,linetype = 0)+
  scale_fill_manual(values = c('#8cadeb',"#de9a7e",'#50bdaf','#cf99d3'),
                    limits = c("5","10","15","20"))+
  scale_color_manual(values = c('#8cadeb',"#de9a7e",'#50bdaf','#cf99d3'),
                     limits = c("5","10","15","20"))+
  labs(x="Distance from center (R)",y="Concentration (C-mM)")+
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'))+
  theme(axis.text= element_text(size=14,color="black"),
        axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))#+
#theme(legend.position = 'none')
dev.off()
mass_name <- paste("Concenlist_diameter-embezzler-", mass_list[[m]],".pdf",sep="")
ggsave(mass_name,height=6,width=8)
dev.new()
colnames(df)[4] <-c("concentration")
df_all <- rbind(df_all,cbind(df,mass=rep(mass_list[[m]],times=nrow(df))))
}
write.csv(df_all,"plot-Concenlist_diameter-average-embezzler.csv", row.names=FALSE)