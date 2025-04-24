####################
####################
####################
#Workshop "Estimating and interpreting psychological networks in R"
#
#Code for example
#
####################
####################
####################

##########
####Get started

###set your working directory
setwd("C:/Users/lange/OneDrive/Zusatz/Konferenzen/DGPs 2022/Workshop")

#R version 4.2.1
###packages
library(qgraph)            #plotting networks; version 1.9.2
library(bootnet)           #estimating networks; version 1.5
library(psych)             #psych statistics; version 2.2.5
library(CliquePercolation) #network structure and data set; version 0.3.0
library(psychonetrics)     #(confirmatory) network modeling; version 0.10
library(dplyr)             #helpful function; version 1.0.10

###load data - ratings of Obama
data(Obama)
data <- Obama

###Items
##Beliefs - scale from 1 to 5
#Mor - "Is moral"
#Led - "Would provide strong leadership"
#Car - "Really cares about people like you"
#Kno - "Is knowledgeable"
#Int - "Is intelligent"
#Hns - "Is honest"
##Feelings - scale from 1 to 5
#Ang - "Angry"
#Hop - "Hopeful"
#Afr - "Afraid of him"
#Prd - "Proud"

##########
####Descriptive Statistics

describe(data)
#-> several missing values
#-> no obvious skewness or kurtosis

#compute correlations
correlations <- cor(data, use = "pairwise.complete.obs")
correlations

###plotting correlations with qgraph
cor_graph <- qgraph(correlations)

#+ using Fruchterman-Reingold
cor_graph <- qgraph(correlations, layout = "spring")

#+ change for color-blind people
cor_graph <- qgraph(correlations, layout = "spring", theme = "colorblind")

#+ change shape of nodes
cor_graph <- qgraph(correlations, layout = "spring", theme = "colorblind",
                    shape = "triangle")

#+ make correlations with absolute value < .70 less important
cor_graph <- qgraph(correlations, layout = "spring", theme = "colorblind",
                    shape = "triangle", cut = .7)

#for list of features see options in qgraph help page
?qgraph

##########
####Network Estimation - Gaussian Graphical Model (Continuous Data)

###estimate regularized partial correlation network
###EBICglasso (gLASSO with EBIC model selection)
###correlations determined via cor
###pairwise deletion of missing values
GGM_net <- estimateNetwork(data, default = "EBICglasso", corMethod = "cor",
                           missing = "pairwise")

###plot network with qgraph
GGM_graph <- plot(GGM_net)

###declare beliefs and feelings separate groups
groups <- c(rep("Beliefs",6), rep("Feelings",4))

###plot network with groups; no legend
GGM_graph <- plot(GGM_net, layout = "spring", theme = "colorblind",
                  groups = groups, legend = FALSE)

#save layout of the graph
layout <- GGM_graph$layout

###get weights matrix
Wmat_GGM <- getWmat(GGM_graph)
Wmat_GGM

##########
###Node Centrality

#determine values
cent <- centrality(GGM_net)

#get values
cent$OutDegree
cent$Closeness

#plot values
centralityPlot(GGM_net, scale = "raw", include = c("Strength","Closeness"))

##########
###Network Stability and Difference Tests

###non-parametric bootstrap
#-> for edge stability and edge as well as centrality difference tests
#run bootstrap
# set.seed(4186)
# boot1 <- bootnet(GGM_net, statistics = c("edge","Strength","Closeness"),
#                  nboots = 1000, nCores = 2, type = "nonparametric")
# save(boot1, file = "boot_edges.RData")
load("boot_edges.RData")

###case-dropping bootstrap
#-> for centrality stability
#run bootstrap
# set.seed(4186)
# boot2 <- bootnet(GGM_net, statistics = c("Strength","Closeness"),
#                  nboots = 1000, nCores = 2, type = "case")
# save(boot2, file = "boot_centrality.RData")
load("boot_centrality.RData")

###stability
#plot edge CIs
plot(boot1, statistics = "edge", labels = TRUE, order = "sample")
#plot edge CIs + make labels visible by creating large pdf
pdf("edge_stability.pdf", height = 10)
plot(boot1, statistics = "edge", labels = TRUE, order = "sample")
dev.off()
#plot centrality stability
plot(boot2, statistics = c("Strength","Closeness"))
#check stability of centrality indices
corStability(boot2)
#-> strength and closeness can be interpreted

###difference tests
#plot edge weights difference test
plot(boot1, statistics = "edge", plot = "difference",
     onlyNonZero = TRUE, order = "sample")
#plot strength difference test
plot(boot1, statistics = "strength", plot = "difference",
     order = "sample")
#plot closeness difference test
plot(boot1, statistics = "closeness", plot = "difference",
     order = "sample")

##########
###Network Estimation - Ising Model (Binary Data)

#can be estimated via estimateNetwork
#variables automatically binarized at median
#here, feeling variables better binarized at 2 because 1 indicates "no, I never felt this"
split <- describe(data)$median
split[which(groups == "Feelings")] <- 2

#estimate regularized logistic node-wise regression network
#define where to binarize variables
#eLASSO (LASSO with EBIC model selection)
#listwise deletion of missing values (pairwise not possible for regressions)
Ising_net <- estimateNetwork(data, default = "IsingFit", split = split,
                             missing = "listwise", rule = "OR")

#extract thresholds
thresholds <- Ising_net$intercepts
thresholds

#extract weights matrix
Wmat_Ising <- getWmat(Ising_net)
Wmat_Ising

###compare GGM and Ising model
#correlating weights matrices
cor.test(Wmat_GGM[upper.tri(Wmat_GGM)], Wmat_Ising[upper.tri(Wmat_Ising)]) #.93

####centrality indices, stability, and difference tests are performed as before

##########
###Other Model Selection

###thresholding with significance (bootnet)
GGM_net_thres <- estimateNetwork(data, default = "pcor",
                                 threshold = "sig", alpha = .01)
plot(GGM_net_thres, layout = layout)

###pruning (psychonetrics)
GGM_net_prune <- ggm(data) %>% runmodel %>% prune(alpha = .01)
Wmat_GGM_prune <- getmatrix(GGM_net_prune, "omega")
qgraph(Wmat_GGM_prune, theme = "colorblind",
       layout = layout, labels = names(data))

###model search with modelsearch (psychonetrics)
GGM_net_modelsearch <- ggm(data) %>% runmodel %>% prune %>% modelsearch
Wmat_GGM_modelsearch <- getmatrix(GGM_net_modelsearch, "omega")
qgraph(Wmat_GGM_modelsearch, theme = "colorblind",
       layout = layout, labels = names(data))

###compare networks
#-> get maximum edge across all networks
Wmat_GGM_thres <- getWmat(GGM_net_thres)
max_edge <- max(Wmat_GGM, Wmat_GGM_thres,
                Wmat_GGM_prune, Wmat_GGM_modelsearch)
#-> plot networks next to each other
layout(matrix(c(1,2,
                3,4), nrow = 2, ncol = 2, byrow = TRUE))
plot(GGM_net, maximum = max_edge)
title("EBICglasso", adj = 1)
plot(GGM_net_thres, layout = layout, maximum = max_edge)
title("Thresholdng", adj = 1)
qgraph(Wmat_GGM_prune, theme = "colorblind",
       layout = layout, labels = names(data),
       maximum = max_edge)
title("Pruning", adj = 1)
qgraph(Wmat_GGM_modelsearch, theme = "colorblind",
       layout = layout, labels = names(data),
       maximum = max_edge)
title("Modelsearch", adj = 1)
dev.off()
