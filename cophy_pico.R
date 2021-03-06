rm(list = ls())
library(phytools)
library(ape) 
library(paco)
# source("https://bioconductor.org/biocLite.R")
# biocLite("ggtree")
# library("Biostrings")
library("ggplot2")
library("ggtree")
library(ggrepel)

## final trees

treeA<-read.newick("Pico A.nwk")
treeB<-read.newick("Pico B.nwk")
treeC<-read.newick("Pico C.nwk")
treeD<-read.newick("Pico D.nwk")
treeE<-read.newick("Pico E.nwk")

treeE$tip.label<-sub("_.*", "", treeE$tip.label)
treeE$tip.label[91:95]<-'Picobirnavirus 2'
treeE$tip.label[96:length(treeE$tip.label)]<-'Picobirnavirus 1'

treeE$node.label<-1:length(treeE$node.label)
##

## to find tips and nodes
ggtree(treeE) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab()

gzoom(treeE, grep("Picobirnavirus", treeE$tip.label)) 

## 
groupInfo <- split(treeE$tip.label, gsub("_\\w+", "", treeE$tip.label))
treeE <- groupOTU(treeE, groupInfo)

ggtree(treeE, layout='rectangular') +  geom_treescale(fontsize = 3,offset = 2) +# geom_tree()+
  ggtitle("dsRNA phlylogeny")+
  geom_tiplab(aes(color=group),size = 2)+
  scale_x_continuous(expand = c(0,0), limits = c(0,8))


# # #These are the tips to drop
 prunetips <- c(1:8,10:11,13:16,25:31,38:68,70:89,91:length(treeE$tip.label)) # to just drop those that are not picob
 treeE$tip.label[prunetips]<-'' # to just drop those that are not picob
# # 
# # #But get the tips to keep
#  keeptips <- treeE$tip.label[!treeE$tip.label %in% prunetips]
# # 
# # #Group the tips to keep
#  prunetree <- groupOTU(treeE, focus=keeptips)
# # 
# # #And plot
#  ggtree(prunetree, aes()) + drop.tip(prunetips)
#  # ggtree(prunetree, aes(color=group))+
# #   scale_color_manual(values=c("lightgrey","black"))+
# #   geom_tiplab()

 # ggtree(treeE, aes())+ ggtitle("dsRNA phlylogeny")+theme_tree2() +
 #   #geom_tiplab()+ 
 #   geom_cladelabel(node=199, label="test label", align=T, color='red') 

 # treeE <- groupOTU(treeE, focus=c("Picobirnavirus 1", "Picobirnavirus 2"))
 # ggtree(treeE)+ ggtitle("dsRNA phlylogeny")+theme_tree2() +
#   geom_tiplab(size=2.5, aes(color=group))+ geom_tippoint(aes(color=group))#
# #theme(legend.position="left")
# 
 p<-ggtree(treeE, aes(color=group), layout='rectangular') +  geom_treescale(fontsize = 3,offset = 2) 
 g <- ggplot_build(p)

ggtree(treeE, aes(color=group), layout='rectangular') +  geom_treescale(fontsize = 3,offset = 2) +
   ggtitle("dsRNA phlylogeny")+
   geom_hilight(node=218, fill="gold",alpha = .1)+ 
   geom_cladelabel(node=219, label="Picobirnavirus 2", #align=T,
                   color='black',fontsize = 3)+ 
   geom_cladelabel(node=223, label="Picobirnavirus 1", #align=T,
                   color='black',fontsize = 3)+
   geom_tiplab(size = 3) + 
  #theme(legend.position="left")+
  # guides(colour = guide_legend(override.aes = list(size=3,linetype=0),title = 'Virus'))+
    geom_point(aes(colour = group), size = 0) +
   geom_cladelabel(node=131, label="Chrysovirus", #align=T,
                   color=(g$data[[1]]$colour[131]),fontsize = 2)+ 
   geom_cladelabel(node=138, label="Chrysovirus", #align=T,
                   color=(g$data[[1]]$colour[138]),fontsize = 2)+ 
   geom_cladelabel(node=142, label="Totivirus", #align=T,
                   color=(g$data[[1]]$colour[142]),fontsize = 2)+ 
   geom_cladelabel(node=153, label="Victorivirus", #align=T,
                   color=(g$data[[1]]$colour[153]),fontsize = 2)+ 
   geom_cladelabel(node=167, label="Aquabirnavirus", #align=T,
                   color=(g$data[[1]]$colour[167]),fontsize = 2)+ 
   geom_cladelabel(node=176, label="Avibirnavirus", #align=T,
                   color=(g$data[[1]]$colour[176]),fontsize = 2)+ 
   geom_cladelabel(node=187, label="Orbivirus", #align=T,
                   color=(g$data[[1]]$colour[187]),fontsize = 2)+ 
   geom_cladelabel(node=199, label="Orthoreovirus", #align=T,
                   color=(g$data[[1]]$colour[199]),fontsize = 2)+ 
   geom_cladelabel(node=208, label="Rotavirus", #align=T,
                   color=(g$data[[1]]$colour[208]),fontsize = 2)+
   scale_x_continuous(expand = c(0,0), limits = c(0,8))
  
##

ggtree(treeE, layout='rectangular') +  geom_treescale(fontsize = 3,offset = 2) +
  ggtitle("dsRNA phlylogeny")+
  geom_hilight(node=218, fill="gold",alpha = .1)+ 
  geom_cladelabel(node=219, label="Picobirnavirus 2", #align=T,
                  color='black',fontsize = 3)+ 
  geom_cladelabel(node=223, label="Picobirnavirus 1", #align=T,
                  color='black',fontsize = 3)+
  geom_tiplab(aes(color = group),size = 3) + 
## comment out the next 3 lines to remove the labels
  #  theme(legend.position="left")+
  #  guides(colour = guide_legend(override.aes = list(size=3,linetype=0),title = 'Virus'))+
  # geom_point(aes(colour = group), size = 0) +
  geom_cladelabel(node=131, label="Chrysovirus", #align=T,
                  # nb change all font sizes to 2 if want smaller
                  color=(g$data[[1]]$colour[131]),fontsize = 3)+ 
  geom_cladelabel(node=138, label="Chrysovirus", #align=T,
                  color=(g$data[[1]]$colour[138]),fontsize = 3)+ 
  geom_cladelabel(node=142, label="Totivirus", #align=T,
                  color=(g$data[[1]]$colour[142]),fontsize = 3)+ 
  geom_cladelabel(node=153, label="Victorivirus", #align=T,
                  color=(g$data[[1]]$colour[153]),fontsize = 3)+ 
  geom_cladelabel(node=167, label="Aquabirnavirus", #align=T,
                  color=(g$data[[1]]$colour[167]),fontsize = 3)+ 
  geom_cladelabel(node=176, label="Avibirnavirus", #align=T,
                  color=(g$data[[1]]$colour[176]),fontsize = 3)+ 
  geom_cladelabel(node=187, label="Orbivirus", #align=T,
                  color=(g$data[[1]]$colour[187]),fontsize = 3)+ 
  geom_cladelabel(node=199, label="Orthoreovirus", #align=T,
                  color=(g$data[[1]]$colour[199]),fontsize = 3)+ 
  geom_cladelabel(node=208, label="Rotavirus", #align=T,
                  color=(g$data[[1]]$colour[208]),fontsize = 3)+
  scale_x_continuous(expand = c(0,0), limits = c(0,8))

# change labels

library(stringr)
str_sub(treeA$tip.label,1,2)

treeA$tip.label<-paste('PBV',str_sub(treeA$tip.label,1,2))
treeB$tip.label<-paste('PBV',str_sub(treeB$tip.label,1,2))
treeC$tip.label<-paste('PBV',str_sub(treeC$tip.label,1,2))
treeD$tip.label<-paste('PBV',str_sub(treeD$tip.label,1,2))
#treeE$tip.label<-paste('PBV',str_sub(treeE$tip.label,1,2))

# paco
mat_co<-diag(x = 1, nrow=length(treeA$tip.label), ncol=length(treeA$tip.label))
row.names(mat_co)<-(treeA$tip.label)
colnames(mat_co)<-(treeA$tip.label)
treeAt<-cophenetic(treeA)
treeBt<-cophenetic(treeB)
D<-prepare_paco_data(H=treeAt,P=treeBt,HP=mat_co)
D<-add_pcoord(D, correction = 'cailliez')
D<-PACo(D,nperm=1000,seed=12,method = 'r0',symmetric = T)
D<-paco_links(D)
res<-residuals_paco(D$proc)
assoc <- data.frame(pol=rownames(mat_co)[which(mat_co==1, arr.ind=TRUE)[,'row']], 
                    pla=colnames(mat_co)[which(mat_co==1, arr.ind=TRUE)[,'col']])
weight <- (res^-2)/50

cophyloplot(treeA, treeB, assoc, show.tip.label=T, use.edge.length=FALSE,
            lwd=weight, col='steelblue', length.line=0, gap=5, space=5)

#

objAB<-cophylo(treeA,treeB,rotate = T)
plot(objAB,cex=0.01,show.tip.label=F)
objAC<-cophylo(treeA,treeC,rotate = T)
plot(objAC,cex=0.01)
objDC<-cophylo(treeD,treeC,rotate = T)
plot(objDC,cex=0.01)

# plot together

# left<-rep("blue",nrow(objAB$trees[[1]]$edge))
# nodes<-getDescendants(objAB$trees[[1]],36)
# left[sapply(nodes,function(x,y) which(y==x),y=objAB$trees[[1]]$edge[,2])]<-
#   "blue"
# nodes<-getDescendants(objAB$trees[[1]],43)
# left[sapply(nodes,function(x,y) which(y==x),y=objAB$trees[[1]]$edge[,2])]<-
#   "forestgreen"
# right<-rep("blue",nrow(objAB$trees[[2]]$edge))
# nodes<-getDescendants(objAB$trees[[2]],36)
# right[sapply(nodes,function(x,y) which(y==x),y=objAB$trees[[2]]$edge[,2])]<-
#   "red"
# nodes<-getDescendants(objAB$trees[[2]],43)
# right[sapply(nodes,function(x,y) which(y==x),y=objAB$trees[[2]]$edge[,2])]<-
#   "forestgreen"
# edge.col<-list(left=left,right=right)
# edge.col


pdf('combined_trees.pdf', height = 12, width = 8)
par(mfrow=c(2,1))
# plot(objAB,fsize=0.8,edge.col=edge.col)
plot(objAB,fsize=0.8,link.col=c(rep('blue',5),rep(51,33)))
legend("topright", c("Picobirnavirus I","Picobirnavirus II"), pch=16, inset = .05, col = c("forestgreen","blue"),
       bty = "n")
text(-0.5, 1, "a",
     cex = 1)
tiplabels.cophylo(pch=16,frame="none",col=c(rep('blue',5),rep('forestgreen',33)),cex=1.5)
tiplabels.cophylo(pch=16,frame="none",col=c(rep('blue',5),rep('forestgreen',33)),cex=1.5, which = 'right')
plot(objAC,fsize=0.8,link.col=c(rep('blue',5),rep(51,33)))
legend("topright", c("Picobirnavirus I","Picobirnavirus II"), pch=16, inset = .05, col = c("forestgreen","blue"),
       bty = "n")
text(-0.5, 1, "b",
     cex = 1)
tiplabels.cophylo(pch=16,frame="none",col=c(rep('blue',5),rep('forestgreen',33)),cex=1.5)
tiplabels.cophylo(pch=16,frame="none",col=c(rep('blue',5),rep('forestgreen',33)),cex=1.5, which = 'right')

#plot(objDC,fsize=0.8)
dev.off()
# cophyloplot(treeA, treeB, assoc, show.tip.label=T, use.edge.length=FALSE,
#             lwd=weight, col='steelblue', length.line=0, gap=15, space=5)
