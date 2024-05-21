#' Visualize the network from the JAGS model
#'
#' @param network_model Network model output from mignette::run_network_model()
#' @param margin Float value
#' @param connected_tol Float value
#' @return Returns a ggplot of the network
#' @export
#'
#'
net_draw <- function(network_model,
                            margin=0.05,
                            connected_tol=0.001){

  # create the net object
  c_matrix <- network_model$mean$conn_g
  node_names <- list(network_model[["brnode_names"]], network_model[["nbnode_names"]])
  node_types <- c("BR", "NB")
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
  # Plot the network

  nb<-sum(network$nodes$type=="BR")
  nw<-sum(network$nodes$type=="NB")
  network$edges$from.x<-network$nodes$x[network$edges$from]
  network$edges$to.x<-network$nodes$x[network$edges$to+nb]
  network$edges$from.y<-network$nodes$y[network$edges$from]
  network$edges$to.y<-network$nodes$y[network$edges$to+nb]


  g<-ggplot2::ggplot()
  g<-g + ggplot2::geom_segment(data=network$edges,ggplot2::aes(x=from.x,
                                                      y=from.y,
                                                      xend=to.x,
                                                      yend=to.y,
                                                      size=size),
                               col="grey30") +
    ggplot2::scale_size_continuous(range = network$display_par$edge_size_scale)+
    ggnewscale::new_scale("size")

  g <- g + ggplot2::geom_point(data = network$nodes %>% dplyr::filter(type=="BR"),
                               ggplot2::aes(x=x,y=y,size=size,fill=name),
                               shape=network$display_par$bnode_shape,
                               color="black") +
    ggplot2::geom_text(data=network$nodes %>% dplyr::filter(type=="BR"),
                       ggplot2::aes(x=x,y=y,label=name)) +
    ggplot2::scale_fill_manual(values=rep(network$display_par$bnode_colors,length=nb))+
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(data=network$nodes %>% dplyr::filter(type=="NB"),
                        ggplot2::aes(x=x,y=y,size=size,fill=name),
                        shape=network$display_par$wnode_shape,color="black")+
    ggplot2::geom_text(data=network$nodes %>% dplyr::filter(type=="NB"),
                       ggplot2::aes(x=x,y=y,label=name)) +
    ggplot2::scale_fill_manual(values=rep(network$display_par$wnode_colors,length=nw))+
    ggplot2::xlim(c(0,1))+
    ggplot2::ylim(c(0,1))+
    ggplot2::scale_size_continuous(range = network$display_par$node_size_scale)+
    ggplot2::theme(legend.position =  "none")+
    ggplot2::theme(panel.background = ggplot2::element_rect(color="black",fill="white"),
                   panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  return(g)
}
