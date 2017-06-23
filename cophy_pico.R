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

##

tree1<-read.newick("ML tree of NucleotideAlignmentTrimmedSHORT.newick")
tree2<-read.newick("ML tree of NucleotideAlignmentTrimmed.newick")

tt<-charmatch(tree1$tip.label,tree2$tip.label)

x<-tree1$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree1$tip.label<-x1

x<-tree2$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree2$tip.label<-x1

obj<-cophylo(tree1,tree2,rotate = F)
plot(obj,cex=0.01)

pdf("cophy_pico_NA_TrimAI.pdf",width = 7,height = 7)
obj<-cophylo(tree1,tree2,rotate = T)
plot(obj,cex=0.01)
dev.off()

#### protein

tree1<-read.newick("ML tree of ProteinAlignmentTrimmedSHORT.newick")
tree2<-read.newick("ML tree of ProteinAlignmentTrimmed.newick")

tt<-charmatch(tree1$tip.label,tree2$tip.label)

x<-tree1$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree1$tip.label<-x1

x<-tree2$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree2$tip.label<-x1

obj<-cophylo(tree1,tree2,rotate = F)
plot(obj,cex=0.01)

pdf("cophy_pico_Prot_TrimAI.pdf",width = 7,height = 7)
obj<-cophylo(tree1,tree2,rotate = T)
plot(obj,cex=0.01)
dev.off()

#### trimmed

tree1<-read.newick("ML tree of ProteinAlignmentTrimmedSHORT.newick")
tree2<-read.newick("ML tree of NucleotideAlignmentTrimmedSHORT.newick")

tt<-charmatch(tree1$tip.label,tree2$tip.label)

x<-tree1$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree1$tip.label<-x1

x<-tree2$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree2$tip.label<-x1

obj<-cophylo(tree1,tree2,rotate = F)
plot(obj,cex=0.01)

pdf("cophy_pico_TRIMMED_SHORT_TrimAI.pdf",width = 7,height = 7)
obj<-cophylo(tree1,tree2,rotate = T)
plot(obj,cex=0.01)
dev.off()

tree1<-read.newick("ML tree of ProteinAlignmentTrimmed.newick")
tree2<-read.newick("ML tree of NucleotideAlignmentTrimmed.newick")

tt<-charmatch(tree1$tip.label,tree2$tip.label)

x<-tree1$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree1$tip.label<-x1

x<-tree2$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[1]))
tree2$tip.label<-x1

obj<-cophylo(tree1,tree2,rotate = F)
plot(obj,cex=0.01)

pdf("cophy_pico_TRIMMED_TrimAI.pdf",width = 7,height = 7)
obj<-cophylo(tree1,tree2,rotate = T)
plot(obj,cex=0.01)
dev.off()
