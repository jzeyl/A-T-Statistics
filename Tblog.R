library(ggtree)
library(patchwork)

# a tree
#set.seed(1338)tr <- rtree(10)
# and some dummy data
#df1 <- tibble(
#  # only some labels match
#  label = c(tr$tip.label[sample(6, 6)], "u9", "v9"),
#  value = label %>% str_sub(2) %>% as.numeric)
#df2 <- tibble(
#  label = rep(tr$tip.label, 4),
#  category = rep(1:4, each=10),
#  value = rnorm(40, 0, 3))

no_legend <- function() theme(legend.position="none")

# plot the tree,
#gg_tr <- ggtree(tr) + geom_tiplab(align=TRUE) +
#  scale_x_continuous(expand=expand_scale(0.2)) # make more room for the labels
## the data points, the histogram and the heatmap
#gg_hist <- ggplot(df1, aes(label, value)) +
#  geom_col(aes(fill=substr(label, 1, 1))) + no_legend()
#gg_heat <- ggplot(df2, aes(category, label)) + geom_tile(aes(fill=value)) +
#  scale_fill_gradient2() + no_legend()
#
#gg_tr + gg_hist + gg_heat + plot_annotation(tag_levels="A")

tree_y <-  function(ggtree, data){
  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")
  left_join(select(data, label), select(ggtree$data, label, y)) %>%
    pull(y)
}

# replot histogram and heatmap, match the y-coords to the tree
#gg_hist <- ggplot(df1, aes(tree_y(gg_tr, df1), value)) +
#  geom_col(aes(fill=substr(label, 1, 1))) + no_legend() +
#  coord_flip() # flip this plot
#gg_heat <- ggplot(df2, aes(category, y=tree_y(gg_tr, df2))) +
#  geom_tile(aes(fill=value)) +
#  scale_fill_gradient2() + no_legend()
#
#gg_tr + gg_hist + gg_heat + plot_annotation(tag_levels="A")
#
#ggsave("img/ggtree-composite-2.png", type='cairo', width=8, height=4)


# overwrite the default expand for continuous scales
scale_y_tree <- function(expand=expand_scale(0, 0.6), ...){
  scale_y_continuous(expand=expand, ...)
}

# get the range of the ggtree y-axis data
tree_ylim <- function(ggtree){
  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")
  range(ggtree$data$y)
}

# plot data next to a ggtree aligned by shared labels
ggtreeplot <- function(ggtree, data = NULL, mapping = aes(), flip=FALSE,
                       expand_limits=expand_scale(0,.6), ...){
  
  if(!inherits(ggtree, "ggtree"))
    stop("not a ggtree object")
  
  # match the tree limits
  limits <- tree_ylim(ggtree)+c(-5,0)#JZ added
  limits[1] <- limits[1] + (limits[1] * expand_limits[1]) - expand_limits[2]
  limits[2] <- limits[2] + (limits[2] * expand_limits[3]) + expand_limits[4]
  
  if(flip){
    mapping <- modifyList(aes_(x=~x), mapping)
    data <- mutate(data, x=tree_y(ggtree, data))
    gg <- ggplot(data=data, mapping = mapping, ...) +
      scale_x_continuous(limits=limits, expand=c(0,0))
  }else{
    mapping <- modifyList(aes_(y=~y), mapping)
    data <- mutate(data, y=tree_y(ggtree, data))
    gg <- ggplot(data=data, mapping = mapping, ...) +
      scale_y_continuous(limits=limits, expand=c(0,0))
  }
  gg
}

# get rid of superfluous axis - this works after coord_flip, so it also works
# for the rotated histogram
no_y_axis <- function () 
  theme(axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#ggsave("img/ggtree-composite-1.png", type='cairo', width=8, height=4)
#
#gg_tr <- ggtree(tr) + geom_tiplab(align=TRUE) +
#  scale_x_continuous(expand=expand_scale(0.2)) + # make more room for the labels
#  scale_y_tree()
#gg_hist <- ggtreeplot(gg_tr, df1, aes(y=value), flip=TRUE) +
#  geom_col(aes(fill=substr(label, 1, 1))) + no_legend() +
#  coord_flip() + no_y_axis()
#gg_heat <- ggtreeplot(gg_tr, df2, aes(x=category)) + geom_tile(aes(fill=value)) +
#  scale_fill_gradient2() + no_legend() + no_y_axis() 
#
#gg_tr + gg_hist + gg_heat + plot_annotation(tag_levels="A")
#
#ggsave("img/ggtree-composite-3.png", type='cairo', width=8, height=4)