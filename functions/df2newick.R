# designed by Dr.Martin Turjak (PhD in computational phylogenetics), 
#see StackOverflow Post here:https://stackoverflow.com/questions/15343338/convert-a-data-frame-to-a-tree-structure-object-such-as-dendrogram
df2newick <- function(df, innerlabel = FALSE){
  traverse <- function(a, i, innerl){
    if(i < (ncol(df))){
      alevelinner <- as.character(
        unique(df[which(as.character(df[,i]) == a), i + 1])
      )
      desc <- NULL
      for(b in alevelinner) 
        desc <- c(desc, traverse(b, i + 1, innerl))
      il <- NULL
      if(innerl==TRUE) 
        il <- paste0(",", a)
      (newickout <- paste("(", paste(desc,collapse = ","), ")", il, 
                          sep=""))
    }
    else { 
      (newickout <- a) 
    }
  }
  
  alevel <- as.character(unique(df[,1]))
  newick <- NULL
  for(x in alevel) 
    newick <- c(newick, traverse(x, 1, innerlabel))
  (newick <- paste("(", paste(newick, collapse = ","), ");", sep=""))
}