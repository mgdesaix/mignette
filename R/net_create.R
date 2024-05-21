#' Visualize the network from the JAGS model
#'
#' @param network_model Network model output from mignette::run_network_model()
#' @param node_types Character vector of the 2 stages of annual cycle: e.g., "BR" for breeding, "NB" for nonbreeding
#' @param margin Float value
#' @param connected_tol Float value
#' @return Returns a network
#' @export
#'
#'
net_create <- function(network_model,
                       node_types=c("BR","NB"),
                       margin=0.05,
                       connected_tol=0.001){

  c_matrix <- network_model$mean$conn_g
  node_names <- list(network_model[["brnode_names"]], network_model[["nbnode_names"]])

  net<-list()
  if(length(node_names) < 1)
    node_names[[1]]<-paste0(node_types[1],1:nrow(c_matrix))
  if(length(node_names) < 2)
    node_names[[2]]<-paste0(node_types[2],1:ncol(c_matrix))

  display_width<-1
  display_height<-1

  bnodes<-data.frame(name=factor(node_names[[1]],levels=node_names[[1]]),
                     x=seq(0+display_width*margin,
                           1-display_width*margin,
                           length=nrow(c_matrix)),
                     y=1-display_height*2*margin,
                     type=factor(node_types[1],levels=node_types),
                     size=rowSums(c_matrix))

  wnodes<-data.frame(name=factor(node_names[[2]],levels=node_names[[2]]),
                     x=seq(0+display_width*margin,
                           1-display_width*margin,
                           length=ncol(c_matrix)),
                     y=0+display_height*2*margin,
                     type=factor(node_types[2],levels=node_types),
                     size=colSums(c_matrix))
  net$nodes<-rbind(bnodes,wnodes)

  mat_edges<-which(c_matrix>connected_tol,arr.ind = TRUE)
  net$edges<-data.frame(from=mat_edges[,1],
                        to=mat_edges[,2],
                        size=apply(mat_edges,MARGIN=1,function(x){c_matrix[x[1],x[2]]}))

  net$connectivity<-as.data.frame(100*c_matrix/sum(c_matrix))
  rownames(net$connectivity)<-node_names[[1]]
  names(net$connectivity)<-node_names[[2]]
  net$display_par<-list(node_size_scale=c(1,10),
                        edge_size_scale=c(1,10),
                        bnode_colors="red",
                        wnode_colors="blue",
                        bnode_shape=21,
                        wnode_shape=21)


  return(net)
}
