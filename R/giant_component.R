#' giant_component
#' 
#' @param graph igraph graph
#' @export
#' @importFrom igraph clusters induced.subgraph
#' 

giant_component <- function(graph){
  cl <- igraph::clusters(graph)
  return(igraph::induced.subgraph(graph, which(cl$membership == which.max(cl$csize))))
}
