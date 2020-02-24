#' fails_dist
#' 
#' @param edges edges data.frame
#' @param by the subject of the distribution (questions or students)
#' @export
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme element_text
#' 

fails_dist <- function(edges, by='questions'){
  if(by=='questions'){
    ggplot2::ggplot(data=edges, ggplot2::aes(reorder(target, -weight, sum), weight)) +
      ggplot2::geom_col(fill='#72bcd4') +
      ggplot2::labs(x='Answer', y='Count') +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  }else if(by=='students'){
    ggplot2::ggplot(data=edges, ggplot2::aes(reorder(source, -weight, sum), weight)) +
      ggplot2::geom_col(fill='#d4728b') +
      ggplot2::labs(x='Student', y='Count') +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  }else{
    cat('Error, by parameter is no valid')
  }
  
}
