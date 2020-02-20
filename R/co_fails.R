#' co_fails
#' 
#' @param edges edges data.frame
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' @importFrom igraph graph_from_data_frame degree cluster_louvain layout_nicely as.undirected V E "V<-"
#' 

co_fails <- function(edges){
  
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
  g <- igraph::graph_from_data_frame(cofails[which(cofails$Weight>9),], directed = FALSE)
  g <- giant_component(g)
  
  g$degree <- igraph::degree(g)
  
  communities <- igraph::cluster_louvain(g, weights = E(g)$Weights)
  
  plot(communities, g,
       layout=igraph::layout_nicely,
       vertex.size=g$degree, vertex.color=V(g)$colors,
       edge.width=E(g)$weight)
}
