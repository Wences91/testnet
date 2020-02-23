#' two_mode
#' 
#' @param edges edges data.frame
#' @param size node sizes
#' @param degree_mode which degree calculates (in, out, total)
#' @param label_size text size
#' @param min_weight minimum edge weight by which to filter
#' @param min_degree minimum node degree by which to filter
#' @export
#' @importFrom igraph graph_from_data_frame degree cluster_louvain layout_nicely as.undirected V E "V<-"
#' 

two_mode <- function(edges, size=1, label_size=1, degree_mode='in', min_weight=0, min_degree=0){
  # create two-mode network
  g <- igraph::graph_from_data_frame(edges[which(edges$weight>min_weight),], directed=TRUE)
  
  g$indegree <- igraph::degree(g, mode = degree_mode)
  g$nsize <- size * (g$degree/sum(g$degree))
  
  igraph::V(g) <- igraph::V(g)[g$degree >= min_degree]
  
  g <- giant_component(g)
  
  communities <- igraph::cluster_louvain(igraph::as.undirected(g))
  cat('Modularity:', communities$modularity)
  
  igraph::V(g)$colors <- ifelse(substr(igraph::V(g)$name, 1, 2) == 'S-', '#add8e6', '#da86c5')
  
  plot(communities, g, layout=igraph::layout_nicely,
       vertex.size=g$nsize, col=igraph::V(g)$colors, vertex.label.cex=label_size,
       edge.width=igraph::E(g)$weight, edge.arrow.size=0.1, edge.arrow.width=0.2, edge.color='#00000020')
}
