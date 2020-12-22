library(phytools)
library(geiger)
library(phangorn)
library(diversitree)
library(coda)
library(ape)
library(maps)
library(TreeSim)
library(hisse)  
#import tree
Naesiotus.tree<-read.nexus(file="Named_snails.nex")
plotTree(Naesiotus.tree, fsize=0.6)
#import data file
NaesiotusData<-read.csv("SantaCruz.csv", row.names=1)
td<- treedata(Naesiotus.tree, NaesiotusData)
tree.pruned <-td$phy
data.pruned <- td$data
plotTree(tree.pruned, fsize=0.6)

#Is your tree ultrametric?
is.ultrametric(tree.pruned)

#If tree is not ultrametric.. make it be. Create fuction to do this.
force.ultrametric<-function(tree,method=c("nnls","extend")){
  method<-method[1]
  if(method=="nnls") tree<-nnls.tree(cophenetic(tree),tree,
                                     rooted=TRUE,trace=0)
  else if(method=="extend"){
    h<-diag(vcv(tree))
    d<-max(h)-h
    ii<-sapply(1:Ntip(tree),function(x,y) which(y==x),
               y=tree$edge[,2])
    tree$edge.length[ii]<-tree$edge.length[ii]+d
  } else 
    cat("method not recognized: returning input tree\n\n")
  tree
}

#Apply to tree and save it as a new object.
tree.pruned <- force.ultrametric(tree.pruned)

overlap<-read.csv("Overlap.csv", row.names=1)

x <- age.range.correlation(tree.pruned, overlap, tri = "lower", n = 1000)

# plot average niche overlap versus node age
plot(x$age.range.correlation)
# add a regression line
abline(x$linear.regression$coefficients)


#looks weird...maybe I need to remove some of the species without data (wolfi 3, olla, cf. wolfi)
Naesiotus.tree2<-read.nexus(file="Named_snails.nex")
plotTree(Naesiotus.tree2, fsize=0.6)

#import data file
NaesiotusData2<-read.csv("SantaCruz2.csv", row.names=1)
td<- treedata(Naesiotus.tree2, NaesiotusData2)
tree.pruned2 <-td$phy
data.pruned2 <- td$data
plotTree(tree.pruned2, fsize=0.6)

#Apply to tree and save it as a new object.
tree.pruned2 <- force.ultrametric(tree.pruned2)

overlap2<-read.csv("Overlap2.csv", row.names=1)

x <- age.range.correlation(tree.pruned2, overlap2, tri = "lower", n = 3000)
x
# plot average niche overlap versus node age
par(mfcol = c(2, 2))
plot(x$age.range.correlation)
# add a regression line
abline(x$linear.regression$coefficients)
