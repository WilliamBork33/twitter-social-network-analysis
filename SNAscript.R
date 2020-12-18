# Prepare Working Environment ---------------------------------------------
## Load libraries
library("tidyverse")
library("visNetwork")
library("amen")
library("igraph")

## Load pre-built node list file and edge list file
nodes_retweets <- as_tibble(read_csv(file = "nodes_retweets_pseudonym.csv"))
edges_retweets <- as_tibble(read_csv(file = "edges_retweets_pseudonym.csv"))


# Interactive network graphs with visNetwork ----------------
## Scale edges because visNetwork doesn't take the "weight" variable natively
edges_retweets <- mutate(edges_retweets, width = weight * 5 - 4)

## A Frozen Plot
frozenPlot_retweets <- visNetwork(nodes_retweets, edges_retweets, main = "Network Graph for a Twitter Hashtag") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(color = list(background = "lightblue", border = "black", highlight = "orange")) %>% 
  visEdges(arrows = "to", color = list(highlight = "orange")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 12)
frozenPlot_retweets

## An Interactive Plot
interactivePlot_retweets <- visNetwork(nodes_retweets, edges_retweets, main = "Network Graph for a Twitter Hashtag") %>% 
  visNodes(color = list(background = "lightblue", border = "black", highlight = "orange")) %>% 
  visEdges(arrows = "to", color = list(highlight = "orange")) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>% 
  visLayout(randomSeed = 12)
interactivePlot_retweets


# Load Pre-Built Models --------------------------------------------------------------
## Load model fits from workspace to view summary
load(file = "fit_attribute1_retweets_Xr.RData")
load(file = "fit_attribute1_retweets_Xc.RData")
load(file = "fit_attribute1_retweets_Xd.RData")
load(file = "fit_attribute1_retweets_Xrc.RData")
load(file = "fit_attribute1_retweets_Xdrc.RData")

load(file = "fit_attribute2_retweets_Xr.RData")
load(file = "fit_attribute2_retweets_Xc.RData")
load(file = "fit_attribute2_retweets_Xd.RData")
load(file = "fit_attribute2_retweets_Xrc.RData")
load(file = "fit_attribute2_retweets_Xdrc.RData")

load(file = "fit_attribute3_retweets_Xr.RData")
load(file = "fit_attribute3_retweets_Xc.RData")
load(file = "fit_attribute3_retweets_Xd.RData")
load(file = "fit_attribute3_retweets_Xrc.RData")
load(file = "fit_attribute3_retweets_Xdrc.RData")

load(file = "fit_attribute4_retweets_Xr.RData")
load(file = "fit_attribute4_retweets_Xc.RData")
load(file = "fit_attribute4_retweets_Xd.RData")
load(file = "fit_attribute4_retweets_Xrc.RData")
load(file = "fit_attribute4_retweets_Xdrc.RData")

load(file = "fit_attribute5_retweets_Xr.RData")
load(file = "fit_attribute5_retweets_Xc.RData")
load(file = "fit_attribute5_retweets_Xd.RData")
load(file = "fit_attribute5_retweets_Xrc.RData")
load(file = "fit_attribute5_retweets_Xdrc.RData")


## Show summary of each model fit
summary(fit_attribute1_retweets_Xr)
summary(fit_attribute1_retweets_Xc)
summary(fit_attribute1_retweets_Xd)
summary(fit_attribute1_retweets_Xrc)
summary(fit_attribute1_retweets_Xdrc)

summary(fit_attribute2_retweets_Xr)
summary(fit_attribute2_retweets_Xc)
summary(fit_attribute2_retweets_Xd)
summary(fit_attribute2_retweets_Xrc)
summary(fit_attribute2_retweets_Xdrc)

summary(fit_attribute3_retweets_Xr)
summary(fit_attribute3_retweets_Xc)
summary(fit_attribute3_retweets_Xd)
summary(fit_attribute3_retweets_Xrc)
summary(fit_attribute3_retweets_Xdrc)

summary(fit_attribute4_retweets_Xr)
summary(fit_attribute4_retweets_Xc)
summary(fit_attribute4_retweets_Xd)
summary(fit_attribute4_retweets_Xrc)
summary(fit_attribute4_retweets_Xdrc)

summary(fit_attribute5_retweets_Xr)
summary(fit_attribute5_retweets_Xc)
summary(fit_attribute5_retweets_Xd)
summary(fit_attribute5_retweets_Xrc)
summary(fit_attribute5_retweets_Xdrc)


# Re-Build Models *(if desired) to cross check against existing loaded models --------------------------------------------------------------
## Create a matrix of node variables/attributes *(selecting out node labels)
nodeMatrix_retweets <- nodes_retweets %>% 
  select(-label) %>% 
  as.matrix()
nodeMatrix_retweets

## Create a matrix of the edgelist
networkMatrix_retweets <- edges_retweets %>% 
  select(from, to, weight) %>% 
  rename("sender" = "from", "receiver" = "to")
networkMatrix_retweets <- as.matrix(get.adjacency(graph.data.frame(networkMatrix_retweets)))
networkMatrix_retweets

## Put node attributes into a matrix called allAttributesMatrix
allAttributesMatrix_retweets <- nodeMatrix_retweets[, 2:ncol(nodeMatrix_retweets)]
allAttributesMatrix_retweets

## Separate out individual variables/attributes to model each individually
attribute1_retweets <- c(allAttributesMatrix_retweets[,1])
attribute2_retweets <- c(allAttributesMatrix_retweets[,2])
attribute3_retweets <- c(allAttributesMatrix_retweets[,3])
attribute4_retweets <- c(allAttributesMatrix_retweets[,4])
attribute5_retweets <- c(allAttributesMatrix_retweets[,5])

## Obtain absolute values for each variable/attribute for where needed in building models
ab_value_attribute1_retweets <- as.matrix(dist(attribute1_retweets, method = "manhattan"))
ab_value_attribute2_retweets <- as.matrix(dist(attribute2_retweets, method = "manhattan"))
ab_value_attribute3_retweets <- as.matrix(dist(attribute3_retweets, method = "manhattan"))
ab_value_attribute4_retweets <- as.matrix(dist(attribute4_retweets, method = "manhattan"))
ab_value_attribute5_retweets <- as.matrix(dist(attribute5_retweets, method = "manhattan"))


## Estimate amen model
fit_attribute1_retweets_Xr    <- ame(networkMatrix_retweets, Xr = attribute1_retweets, print = FALSE)
fit_attribute1_retweets_Xc    <- ame(networkMatrix_retweets, Xc = attribute1_retweets, print = FALSE)
fit_attribute1_retweets_Xd    <- ame(networkMatrix_retweets, Xd = ab_value_attribute1_retweets, print = FALSE)
fit_attribute1_retweets_Xrc   <- ame(networkMatrix_retweets, Xr = attribute1_retweets, Xc = attribute1_retweets, print = FALSE)
fit_attribute1_retweets_Xdrc  <- ame(networkMatrix_retweets, Xd = ab_value_attribute1_retweets, Xr = attribute1_retweets, Xc = attribute1_retweets, print = FALSE)

fit_attribute2_retweets_Xr    <- ame(networkMatrix_retweets, Xr = attribute2_retweets, print = FALSE)
fit_attribute2_retweets_Xc    <- ame(networkMatrix_retweets, Xc = attribute2_retweets, print = FALSE)
fit_attribute2_retweets_Xd    <- ame(networkMatrix_retweets, Xd = ab_value_attribute2_retweets, print = FALSE)
fit_attribute2_retweets_Xrc   <- ame(networkMatrix_retweets, Xr = attribute2_retweets, Xc = attribute2_retweets, print = FALSE)
fit_attribute2_retweets_Xdrc  <- ame(networkMatrix_retweets, Xd = ab_value_attribute2_retweets, Xr = attribute2_retweets, Xc = attribute2_retweets, print = FALSE)

fit_attribute3_retweets_Xr    <- ame(networkMatrix_retweets, Xr = attribute3_retweets, print = FALSE)
fit_attribute3_retweets_Xc    <- ame(networkMatrix_retweets, Xc = attribute3_retweets, print = FALSE)
fit_attribute3_retweets_Xd    <- ame(networkMatrix_retweets, Xd = ab_value_attribute3_retweets, print = FALSE)
fit_attribute3_retweets_Xrc   <- ame(networkMatrix_retweets, Xr = attribute3_retweets, Xc = attribute3_retweets, print = FALSE)
fit_attribute3_retweets_Xdrc  <- ame(networkMatrix_retweets, Xd = ab_value_attribute3_retweets, Xr = attribute3_retweets, Xc = attribute3_retweets, print = FALSE)

fit_attribute4_retweets_Xr    <- ame(networkMatrix_retweets, Xr = attribute4_retweets, print = FALSE)
fit_attribute4_retweets_Xc    <- ame(networkMatrix_retweets, Xc = attribute4_retweets, print = FALSE)
fit_attribute4_retweets_Xd    <- ame(networkMatrix_retweets, Xd = ab_value_attribute4_retweets, print = FALSE)
fit_attribute4_retweets_Xrc   <- ame(networkMatrix_retweets, Xr = attribute4_retweets, Xc = attribute4_retweets, print = FALSE)
fit_attribute4_retweets_Xdrc  <- ame(networkMatrix_retweets, Xd = ab_value_attribute4_retweets, Xr = attribute4_retweets, Xc = attribute4_retweets, print = FALSE)

fit_attribute5_retweets_Xr    <- ame(networkMatrix_retweets, Xr = attribute5_retweets, print = FALSE)
fit_attribute5_retweets_Xc    <- ame(networkMatrix_retweets, Xc = attribute5_retweets, print = FALSE)
fit_attribute5_retweets_Xd    <- ame(networkMatrix_retweets, Xd = ab_value_attribute5_retweets, print = FALSE)
fit_attribute5_retweets_Xrc   <- ame(networkMatrix_retweets, Xr = attribute5_retweets, Xc = attribute5_retweets, print = FALSE)
fit_attribute5_retweets_Xdrc  <- ame(networkMatrix_retweets, Xd = ab_value_attribute5_retweets, Xr = attribute5_retweets, Xc = attribute5_retweets, print = FALSE)


## Save models *(if desired) so they don't have to re-build them later*
## WILL OVERWRITE PRE-LOADED MODELS!!
save(fit_attribute1_retweets_Xr, file   = "fit_attribute1_retweets_Xr.RData")
save(fit_attribute1_retweets_Xc, file   = "fit_attribute1_retweets_Xc.RData")
save(fit_attribute1_retweets_Xd, file   = "fit_attribute1_retweets_Xd.RData")
save(fit_attribute1_retweets_Xrc, file  = "fit_attribute1_retweets_Xrc.RData")
save(fit_attribute1_retweets_Xdrc, file = "fit_attribute1_retweets_Xdrc.RData")

save(fit_attribute2_retweets_Xr, file   = "fit_attribute2_retweets_Xr.RData")
save(fit_attribute2_retweets_Xc, file   = "fit_attribute2_retweets_Xc.RData")
save(fit_attribute2_retweets_Xd, file   = "fit_attribute2_retweets_Xd.RData")
save(fit_attribute2_retweets_Xrc, file  = "fit_attribute2_retweets_Xrc.RData")
save(fit_attribute2_retweets_Xdrc, file = "fit_attribute2_retweets_Xdrc.RData")

save(fit_attribute3_retweets_Xr, file   = "fit_attribute3_retweets_Xr.RData")
save(fit_attribute3_retweets_Xc, file   = "fit_attribute3_retweets_Xc.RData")
save(fit_attribute3_retweets_Xd, file   = "fit_attribute3_retweets_Xd.RData")
save(fit_attribute3_retweets_Xrc, file  = "fit_attribute3_retweets_Xrc.RData")
save(fit_attribute3_retweets_Xdrc, file = "fit_attribute3_retweets_Xdrc.RData")

save(fit_attribute4_retweets_Xr, file   = "fit_attribute4_retweets_Xr.RData")
save(fit_attribute4_retweets_Xc, file   = "fit_attribute4_retweets_Xc.RData")
save(fit_attribute4_retweets_Xd, file   = "fit_attribute4_retweets_Xd.RData")
save(fit_attribute4_retweets_Xrc, file  = "fit_attribute4_retweets_Xrc.RData")
save(fit_attribute4_retweets_Xdrc, file = "fit_attribute4_retweets_Xdrc.RData")

save(fit_attribute5_retweets_Xr, file   = "fit_attribute5_retweets_Xr.RData")
save(fit_attribute5_retweets_Xc, file   = "fit_attribute5_retweets_Xc.RData")
save(fit_attribute5_retweets_Xd, file   = "fit_attribute5_retweets_Xd.RData")
save(fit_attribute5_retweets_Xrc, file  = "fit_attribute5_retweets_Xrc.RData")
save(fit_attribute5_retweets_Xdrc, file = "fit_attribute5_retweets_Xdrc.RData")

