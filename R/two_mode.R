#' two_mode
#' 
#' @param edges edges data.frame
#' @export
#' @importFrom igraph graph_from_data_frame degree cluster_louvain layout_nicely as.undirected V E "V<-"
#' 

two_mode <- function(edges){
  # create two-mode network
  g <- igraph::graph_from_data_frame(edges, directed = TRUE)
  g <- giant_component(g)
  
  g$indegree <- igraph::degree(g, mode = 'in')
  
  communities <- igraph::cluster_louvain(igraph::as.undirected(g))
  
  igraph::V(g)$colors <- ifelse(substr(igraph::V(g)$name, 1, 2) == 'S-', '#add8e6', '#da86c5')
  
  plot(communities, g, layout=igraph::layout_nicely,
       vertex.size=5, col=igraph::V(g)$colors,
       edge.width=igraph::E(g)$weight, edge.arrow.size=0.1, edge.arrow.width=0.2, edge.color='#00000020')
}
