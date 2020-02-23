#' co_fails
#' 
#' @param edges edges data.frame
#' @param size node sizes
#' @param degree_mode which degree calculates (in, out, total)
#' @param label_size text size
#' @param min_weight minimum edge weight by which to filter
#' @param min_degree minimum node degree by which to filter
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' @importFrom igraph graph_from_data_frame degree cluster_louvain layout_nicely as.undirected V E "V<-" delete_vertices
#' 

co_fails <- function(edges, size=1, label_size=1, degree_mode='total', min_weight=0, min_degree=0){
  
  # create co-fails matrix
  cofails <- dplyr::inner_join(x=edges, y=edges, by='source')
  cofails <- cofails[which(!(cofails$target.x == cofails$target.y)),]
  
  cofails <- dplyr::mutate(cofails,
                           Source = pmin(target.x, target.y),
                           Target = pmax(target.x, target.y))
  cofails <- cofails[, which(!(names(cofails) %in% c('target.x', 'target.y', 'weight.x', 'weight.y')))]
  
  # remove
  cofails <- dplyr::distinct(cofails, .keep_all = TRUE)
  
  # change data.frame names
  names(cofails)[names(cofails) == 'source'] <- 'Weight'
  
  # group and sum mentions
  cofails$Weight <- 1
  cofails <- dplyr::group_by(cofails, Source, Target)
  cofails <- dplyr::summarise(cofails, Weight=sum(Weight))
  cofails <- as.data.frame(cofails, stringsAsFactors = FALSE)
  
  # create co-fails network
  g <- igraph::graph_from_data_frame(cofails[which(cofails$Weight>min_weight),], directed = FALSE)
  
  g$degree <- igraph::degree(g, mode='total')
  g$nsize <- size * (g$degree/sum(g$degree))
  
  igraph::delete_vertices(g, igraph::V(g)[g$degree >= min_degree])
  g <- giant_component(g)
  
  communities <- igraph::cluster_louvain(g, weights = E(g)$Weights)
  cat('Modularity:', communities$modularity)
  
  plot(communities, g,
       layout=igraph::layout_nicely,
       vertex.size=g$nsize, vertex.color=V(g)$colors, vertex.label.cex=label_size,
       edge.width=E(g)$weight)
}
