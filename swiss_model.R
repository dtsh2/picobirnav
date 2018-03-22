library("ggplot2")
data<-read.csv("SwissModelData.csv")
#data$Virus<-c(rep('Picobirnavirus',38),rep('dsRNA',17))
data$Virus<-c(rep('Picobirnavirus I',33),rep('Picobirnavirus II',5),rep('dsRNA',17))

p <-ggplot(data, aes(NAME, QMEAN))
p<-p +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,QMEAN),fill=Virus)+xlab('Virus')
pdf('qmean.pdf',width=15,height = 5)
p
dev.off()

p1 <-ggplot(data, aes(NAME, GMQE))
p1<-p1 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,GMQE),fill=Virus)+xlab('Virus')
pdf('gmqe.pdf',width=15,height = 5)
p1
dev.off()

p2 <-ggplot(data, aes(NAME, Seq_Similarity))
p2<-p2 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')
pdf('Seq_Similarity.pdf',width=15,height = 5)
p2
dev.off()

# same seq similarity

p0 <-ggplot(data, aes(NAME, QMEAN))
p0<-p0 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')
pdf('qmean_same_seq_sim.pdf',width=15,height = 5)
p0
dev.off()

p10 <-ggplot(data, aes(NAME, GMQE))
p10<-p10 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')
pdf('gmqe_same_seq_sim.pdf',width=15,height = 5)
p10
dev.off()

p20 <-ggplot(data, aes(NAME, Seq_Similarity))
p20<-p20 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')
pdf('Seq_Similarity.pdf',width=15,height = 5)
p20
dev.off()

## same axis
scaleFUN <- function(x) sprintf("%.2f", x)
scaleFUNqmean <- function(x) sprintf("%.1f", x)


p00 <-ggplot(data, aes(NAME, Seq_Similarity))
p00<-p00 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ guides(fill=FALSE)+
   scale_y_continuous(labels=scaleFUN)

p100 <-ggplot(data, aes(NAME, GMQE))
p100<-p100 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')+ guides(fill=FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels=scaleFUN)

p200 <-ggplot(data, aes(NAME, QMEAN))
p200<-p200 +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq_Similarity),fill=Virus)+xlab('Virus')+ guides(fill=guide_legend(title="Classification"))+
  scale_y_continuous(labels=scaleFUNqmean)+ theme(legend.position = c(0.9, -1.1))

library(gridExtra)

pdf('swiss_model.pdf',width=12)
grid.arrange(p00,p100,p200,nrow = 3,heights = c(1,1,2))
dev.off()

data_spin<-read.csv("Spinareovirinae.csv")
data_spin$NAME<-paste(data_spin$Virus.Type,data_spin$Accession)

p00_spin <-ggplot(data_spin, aes(NAME, Seq.Similarity))
p00_spin<-p00_spin +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME, Seq.Similarity),fill=Virus.Type)+xlab('Virus')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ theme(legend.position = c(0.3, 0.6))+ guides(fill=guide_legend(title="Classification", ncol = 2))+
  scale_y_continuous(labels=scaleFUN)

p100_spin <-ggplot(data_spin, aes(NAME, GMQE))
p100_spin<-p100_spin +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq.Similarity),fill=Virus.Type)+xlab('Virus')+ guides(fill=FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels=scaleFUN)

p200_spin <-ggplot(data_spin, aes(NAME, QMEAN))
p200_spin<-p200_spin +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq.Similarity),fill=Virus.Type)+xlab('Virus')+scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(labels=scaleFUNqmean)+
  scale_x_discrete(labels=data_birn$Accession)


data_birn<-read.csv("Birnaviridae.csv")
data_birn$NAME<-paste(data_birn$Virus.Type,data_birn$Accession)

p00_birn <-ggplot(data_birn, aes(NAME, Seq.Similarity))
p00_birn<-p00_birn +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME, Seq.Similarity),fill=Virus.Type)+xlab('Virus')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ guides(fill=FALSE)+
  scale_y_continuous(labels=scaleFUN)

p100_birn <-ggplot(data_birn, aes(NAME, GMQE))
p100_birn<-p100_birn +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq.Similarity),fill=Virus.Type)+xlab('Virus')+ guides(fill=FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_y_continuous(labels=scaleFUN)

p200_birn <-ggplot(data_birn, aes(NAME, QMEAN))
p200_birn<-p200_birn +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  aes(x=reorder(NAME,Seq.Similarity),fill=Virus.Type)+xlab('Virus')+ 
  theme(legend.position = c(0.75, 0.5))+ guides(fill=guide_legend(title="Classification"))+
  scale_y_continuous(labels=scaleFUNqmean)+
  scale_x_discrete(labels=data_birn$Accession)

pdf('swiss_model_spin_birn.pdf',width=12)
grid.arrange(p00_spin,p00_birn,p100_spin,p100_birn,p200_spin,p200_birn,
             nrow = 3,ncol=2,heights = c(1,1,2), widths = c(1,1.5))
dev.off()
