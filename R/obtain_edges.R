#' obtain_edges
#' 
#' @param answers response data.frame
#' @param c_answers response template
#' @param avoid questions to avoid
#' @export
#' 

obtain_edges <- function(answers, c_answers, avoid=NA){
  
  # transform questions into numbers
  invisible(sapply(1:dim(answers)[2], function(x){
    names(answers)[x] <<- strsplit(names(answers)[x], '\\.')[[1]][1]
    c_answers[x,1] <<- strsplit(c_answers[x,1], '\\.')[[1]][1]
  }))
  
  # set students names
  row.names(answers) <- paste0('S-',row.names(answers))
  
  # remove some questions
  answers <- answers[,which(!(names(answers) %in% avoid)),]
  c_answers <- c_answers[which(!(c_answers[,1] %in% avoid)),]
  
  # check
  all(names(answers) %in% c_answers[,1])
  
  # correct the responses
  corrected <- answers
  sapply(names(answers), function(x){
    corrected[,x] <<- as.integer(corrected[,x] == c_answers[which(c_answers[,1] == x),2])
  })
  
  # obtain the edges
  edges <- data.frame(source = integer(), target = character(), weight = integer(), stringsAsFactors = FALSE)
  
  sapply(1:dim(corrected)[1], function(x){
    sapply(1:dim(corrected)[2], function(y){
      if(corrected[x, y] == 0){
        edges <<- rbind.data.frame(edges, data.frame(source = rownames(corrected)[x], target = names(corrected)[y], weight = 1, stringsAsFactors = FALSE))
      }
    })
  })
  
  return(edges)
}
