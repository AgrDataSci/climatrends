#' @method print clima_df
#' @export
print.clima_df <- function(x, ...){
  
  x <- as.data.frame(x, stringAsFactor = FALSE)
  
  classes <- lapply(x, function(y){
    class(y)
  })
  
  classes <- as.vector(unlist(classes))
  class_abb = c(list = "<list>", integer = "<int>", numeric = "<dbl>", 
                character = "<char>", Date = "<Date>", complex = "<cplx>", 
                factor = "<fctr>", POSIXct = "<POSc>", logical = "<lgcl>", 
                IDate = "<IDat>", integer64 = "<i64>", raw = "<raw>", 
                expression = "<expr>", ordered = "<ord>")
  
  abbs <- unname(class_abb[classes])
  
  nc <- dim(x)[[2]]
  nr <- dim(x)[[1]]
  
  x[1:nc] <- lapply(x, function(y){
    if (is.numeric(y)) round(y, 2)
  })
  
  if (nr <= 10L) {
    
    toprint <- rbind(abbs, x)
    
    rownames(toprint) <- c("", rownames(x))
    
  }
  
  if (nr > 10L) {
    
    he <- head(x, 5L)
    
    ta <- tail(x, 5L)
    
    toprint <- rbind(abbs, 
                     he, 
                     rep("", dim(x)[[2]]), 
                     ta, 
                     names(x))
    
    rownames(toprint) <- c("",
                           row.names(he),
                           "---",
                           rownames(ta),
                           " ")
    
    
  }
  
  print(toprint)
  
}
