#' Plot the network
#'
#' @param network The network created from `net_create`
#' @return
#' @export
#'
#'
net_draw<-function(network)
{
  nb<-sum(network$nodes$type=="B")
  nw<-sum(network$nodes$type=="W")
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

  g <- g + ggplot2::geom_point(data = network$nodes %>% dplyr::filter(type=="B"),
                               ggplot2::aes(x=x,y=y,size=size,fill=name),
                               shape=network$display_par$bnode_shape,
                               color="black") +
    ggplot2::geom_text(data=network$nodes %>% dplyr::filter(type=="B"),
                       ggplot2::aes(x=x,y=y,label=name)) +
    ggplot2::scale_fill_manual(values=rep(network$display_par$bnode_colors,length=nb))+
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(data=network$nodes %>% dplyr::filter(type=="W"),
                        ggplot2::aes(x=x,y=y,size=size,fill=name),
                        shape=network$display_par$wnode_shape,color="black")+
    ggplot2::geom_text(data=network$nodes %>% dplyr::filter(type=="W"),
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
