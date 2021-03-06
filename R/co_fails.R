#' co_fails
#' 
#' @param edges edges data.frame
#' @param size node sizes
#' @param edge_size edge sizes
#' @param degree_mode which degree calculates (in, out, total)
#' @param label_size text size
#' @param min_weight minimum edge weight by which to filter
#' @param min_degree minimum node degree by which to filter
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' @importFrom igraph graph_from_data_frame degree cluster_louvain layout_nicely as.undirected V E "V<-" delete_vertices
#' 

co_fails <- function(edges, size=1, edge_size=1, label_size=1, degree_mode='total', min_weight=0, min_degree=0){
  
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
  
  igraph::V(g)$degree <- igraph::degree(g, mode = degree_mode)
  igraph::V(g)$nsize <- size * (igraph::V(g)$degree/sum(igraph::V(g)$degree))
  
  igraph::E(g)$edge_size <- edge_size * (igraph::E(g)$Weight/sum(igraph::E(g)$Weight))
  
  g <- igraph::delete_vertices(g, which(igraph::V(g)$degree < min_degree))
  g <- giant_component(g)
  
  communities <- igraph::cluster_louvain(g, weights = E(g)$Weights)
  cat(length(unique(communities$membership)), 'communities | Modularity:', max(communities$modularity), '\n')
  
  l <- igraph::layout_nicely(g)
  
  plot(communities, g, layout=l,
       vertex.size=igraph::V(g)$nsize, vertex.color=V(g)$colors, vertex.label.cex=label_size,
       edge.width=igraph::E(g)$edge_size,
       rescale=FALSE, ylim=c(min(l[,2]), max(l[,2])), xlim = c(min(l[,1]), max(l[,1])))
}
