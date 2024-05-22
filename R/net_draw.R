#' Visualize the network from the JAGS model
#'
#' @param net_object Net object produced from `net_create`
#' @return Returns a ggplot of the network
#' @export
#'
#'
net_draw <- function(net_object){
  # Plot the network
  nb<-sum(net_object$nodes$type=="BR")
  nw<-sum(net_object$nodes$type=="NB")
  net_object$edges$from.x<-net_object$nodes$x[net_object$edges$from]
  net_object$edges$to.x<-net_object$nodes$x[net_object$edges$to+nb]
  net_object$edges$from.y<-net_object$nodes$y[net_object$edges$from]
  net_object$edges$to.y<-net_object$nodes$y[net_object$edges$to+nb]


  g<-ggplot2::ggplot()
  g<-g + ggplot2::geom_segment(data=net_object$edges,ggplot2::aes(x=from.x,
                                                      y=from.y,
                                                      xend=to.x,
                                                      yend=to.y,
                                                      size=size),
                               col="grey30") +
    ggplot2::scale_size_continuous(range = net_object$display_par$edge_size_scale)+
    ggnewscale::new_scale("size")

  g <- g + ggplot2::geom_point(data = net_object$nodes %>% dplyr::filter(type=="BR"),
                               ggplot2::aes(x=x,y=y,size=size,fill=name),
                               shape=net_object$display_par$brnode_shape,
                               color="black") +
    ggplot2::geom_text(data=net_object$nodes %>% dplyr::filter(type=="BR"),
                       ggplot2::aes(x=x,y=y,label=name)) +
    ggplot2::scale_fill_manual(values=rep(net_object$display_par$brnode_colors,length=nb))+
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(data=net_object$nodes %>% dplyr::filter(type=="NB"),
                        ggplot2::aes(x=x,y=y,size=size,fill=name),
                        shape=net_object$display_par$nbnode_shape,color="black")+
    ggplot2::geom_text(data=net_object$nodes %>% dplyr::filter(type=="NB"),
                       ggplot2::aes(x=x,y=y,label=name)) +
    ggplot2::scale_fill_manual(values=rep(net_object$display_par$nbnode_colors,length=nw))+
    ggplot2::xlim(c(0,1))+
    ggplot2::ylim(c(0,1))+
    ggplot2::scale_size_continuous(range = net_object$display_par$node_size_scale)+
    ggplot2::theme(legend.position =  "none")+
    ggplot2::theme(panel.background = ggplot2::element_rect(color="black",fill="white"),
                   panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  return(g)
}
