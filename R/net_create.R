#' Create a "net" object from the JAGS model for visualization
#'
#' @param network_model Network model output from mignette::run_network_model()
#' @param margin Float value
#' @param connected_tol Float value to set threshold of connectivity estimates to set to 0
#' @param replace_overlap Logical value for changing connectivity estimates to 0 that have 95% credible intervals bounded by 0
#' @return Returns a net object to be used in `net_draw` function
#' @export
#'
#'
net_create <- function(network_model,
                     margin = 0.05,
                     connected_tol = 0.001,
                     replace_overlap = FALSE){

  # create the net object
  c_matrix <- network_model$jags_out$mean$conn
  if(replace_overlap == TRUE){
    c_matrix[network_model$jags_out$overlap0$conn] <- 0
  }
  node_names <- list(network_model[["brnode_names"]], network_model[["nbnode_names"]])
  node_types <- c("BR", "NB")
  net<-list()
  if(length(node_names) < 1)
    node_names[[1]]<-paste0(node_types[1],1:nrow(c_matrix))
  if(length(node_names) < 2)
    node_names[[2]]<-paste0(node_types[2],1:ncol(c_matrix))

  display_width<-1
  display_height<-1

  brnodes<-data.frame(name=factor(node_names[[1]],levels=node_names[[1]]),
                      x=seq(0+display_width*margin,
                            1-display_width*margin,
                            length=nrow(c_matrix)),
                      y=1-display_height*2*margin,
                      type=factor(node_types[1],levels=node_types),
                      size=rowSums(c_matrix))

  nbnodes<-data.frame(name=factor(node_names[[2]],levels=node_names[[2]]),
                      x=seq(0+display_width*margin,
                            1-display_width*margin,
                            length=ncol(c_matrix)),
                      y=0+display_height*2*margin,
                      type=factor(node_types[2],levels=node_types),
                      size=colSums(c_matrix))
  net$nodes<-rbind(brnodes,nbnodes)

  mat_edges<-which(c_matrix>connected_tol,arr.ind = TRUE)
  net$edges<-data.frame(from=mat_edges[,1],
                        to=mat_edges[,2],
                        size=apply(mat_edges,MARGIN=1,function(x){c_matrix[x[1],x[2]]}))

  net$connectivity<-as.data.frame(100*c_matrix/sum(c_matrix))
  rownames(net$connectivity)<-node_names[[1]]
  names(net$connectivity)<-node_names[[2]]
  net$display_par<-list(node_size_scale=c(1,10),
                        edge_size_scale=c(1,10),
                        brnode_colors="red",
                        nbnode_colors="blue",
                        brnode_shape=21,
                        nbnode_shape=21)
  return(net)
}
