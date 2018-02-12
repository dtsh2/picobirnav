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
