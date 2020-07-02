library(claremontrun)
library(tidyverse)
library(ggsci)
library(ggthemes)
library(dendextend)
library(ggdendro)
library(ape)

characters_tot <- characters %>% group_by(character) %>%
  summarize_if(is.numeric, sum) %>% select(issue, character, rendered_unconcious:number_of_kills_non_humans)

names_xmen <- characters_tot$character
characters_df <- as.data.frame(characters_tot %>% select(rendered_unconcious:number_of_kills_non_humans))
rownames(characters_df) <- names_xmen

#hierarchical clustering
characters_scaled <- as.data.frame(scale(characters_df))
hclust.xmen <- hclust(dist(characters_scaled), method="complete")
cut.xmen <- cutree(hclust.xmen, k=5)
characters_scaled$cluster <- cut.xmen

#summary of clusters
xmen_summary_clust <- characters_scaled %>% group_by(cluster) %>% summarise_all(mean) %>%
  pivot_longer(rendered_unconcious:number_of_kills_non_humans)

#plot summary of clusters
ggplot(xmen_summary_clust, aes(x=name, y=value, col=as.factor(cluster), group=cluster)) + 
  geom_line() + scale_color_manual(values=c("deepskyblue3", "purple2","darkgoldenrod","red3","green4")) + theme_few() + coord_flip() + 
  facet_wrap(~cluster, nrow=1) +
  theme(legend.position = "none") +
  labs(title="Attributes of X-Men clusters (scaled)", y="Mean scaled value", x="", caption="Source: Malcom Barret's claremontrun package") 

#dendogram
dend_xmen <- as.dendrogram(hclust.xmen)
dend_xmen_col <- color_branches(dend_xmen, k=5)

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "black")

#par(pin=c(6,3))
#dev.new(width=10, height=10)
plot(dend_xmen_col, horiz=T, type="triangle",
     xlab="Height", nodePar = nodePar,
     label.offset=0.9, main="Fight profile clusters of X-Men characters")
