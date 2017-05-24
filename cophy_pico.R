rm(list = ls())
library(phytools)
library(ape) 

tree1<-read.newick("Picobirnavirus complete.newick")
tree2<-read.newick("Picobirnavirus short.newick")

tt<-charmatch(tree1$tip.label,tree2$tip.label)

tree1$tip.label<-LETTERS[1:length(tree1$tip.label)]
tree2$tip.label<-LETTERS[tt]

obj<-cophylo(tree1,tree2,rotate = F)
plot(obj,cex=0.01)

pdf("cophy_pico.pdf",width = 7,height = 7)
obj<-cophylo(tree1,tree2,rotate = T)
plot(obj,cex=0.01)
dev.off()

tree1<-read.newick("ML tree of NaturePico.newick")
tree2<-read.newick("ML tree of NaturePicoTrimmed.newick")

tt<-charmatch(tree1$tip.label,tree2$tip.label)

tree1$tip.label<-LETTERS[1:length(tree1$tip.label)]
tree2$tip.label<-LETTERS[tt]

obj<-cophylo(tree1,tree2,rotate = F)
plot(obj,cex=0.01)

pdf("cophy_pico_nature_trimmed.pdf",width = 7,height = 7)
obj<-cophylo(tree1,tree2,rotate = T)
plot(obj,cex=0.01)
dev.off()

