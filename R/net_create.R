#' Create the network for visualizing from the JAGS model
#'
#' @param c_matrix Matrix of network connectivity. This is the output from the JAGS run
#' @param node.names Node names of the connectivity model. These need to be in the same order of the JAGS run.
#' @param margin Float value
#' @param connected_tol Float value
#' @return
#' @export
#'
#'
net_create <- function(c_matrix,node.types=c("B","W"),
                                       node.names=list(),
                                       margin=0.05,
                                       connected_tol=0.0001)
{
  net<-list()
  if(length(node.names) < 1)
    node.names[[1]]<-paste0(node.types[1],1:nrow(c_matrix))
  if(length(node.names) < 2)
    node.names[[2]]<-paste0(node.types[2],1:ncol(c_matrix))

  display_width<-1
  display_height<-1

  bnodes<-data.frame(name=factor(node.names[[1]],levels=node.names[[1]]),
                     x=seq(0+display_width*margin,
                           1-display_width*margin,
                           length=nrow(c_matrix)),
                     y=1-display_height*2*margin,
                     type=factor(node.types[1],levels=node.types),
                     size=rowSums(c_matrix))

  wnodes<-data.frame(name=factor(node.names[[2]],levels=node.names[[2]]),
                     x=seq(0+display_width*margin,
                           1-display_width*margin,
                           length=ncol(c_matrix)),
                     y=0+display_height*2*margin,
                     type=factor(node.types[2],levels=node.types),
                     size=colSums(c_matrix))
  net$nodes<-rbind(bnodes,wnodes)

  mat_edges<-which(c_matrix>connected_tol,arr.ind = 2)
  net$edges<-data.frame(from=mat_edges[,1],
                        to=mat_edges[,2],
                        size=apply(mat_edges,MARGIN=1,function(x){c_matrix[x[1],x[2]]}))

  net$connectivity<-as.data.frame(100*c_matrix/sum(c_matrix))
  rownames(net$connectivity)<-node.names[[1]]
  names(net$connectivity)<-node.names[[2]]
  net$display_par<-list(node_size_scale=c(1,10),
                        edge_size_scale=c(1,10),
                        bnode_colors="red",
                        wnode_colors="blue",
                        bnode_shape=21,
                        wnode_shape=21)


  return(net)
}
