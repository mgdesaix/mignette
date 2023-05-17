#' Set up the data for the migratory network model in JAGS
#'
#' @param abundance Tibble of abundance data across populations (model nodes)
#' @param assignment Tibble of assignment among the populations
#' @param bnode_names Names of breeding populations/nodes
#' @param wnode_names Names of nonbreeding populations/nodes
#' @return A list of of input data for the JAGS model
#' @export
#'
#'
get_jags_data <- function(abundance, assignment, bnode_names, wnode_names){
  stopifnot("`bnode_names` must correspond to values in first column of `abundance` tibble" = bnode_names %in% dplyr::pull(abundance, 1))
  stopifnot("`wnode_names` must correspond to values in first column of `abundance` tibble" = wnode_names %in% dplyr::pull(abundance, 1))
  stopifnot("`bnode_names` must correspond to values in first column of `assignment` tibble" = bnode_names %in% dplyr::pull(assignment, 1))
  stopifnot("`wnode_names` must be in column names of `assignment` tibble, starting with column 2" = wnode_names %in% colnames(assignment))

  pop.b <- abundance %>%
    dplyr::filter(.[[1]] %in% bnode_names) %>%
    dplyr::arrange(factor(.[[1]], levels = bnode_names)) %>%
    dplyr::mutate(rel_ab = as.numeric(round(100*.[[2]]/sum(.[[2]])))) %>%
    dplyr::pull(rel_ab)

  pop.w <- abundance %>%
    dplyr::filter(.[[1]] %in% wnode_names) %>%
    dplyr::arrange(factor(.[[1]], levels = wnode_names)) %>%
    dplyr::mutate(rel_ab = as.numeric(round(100*.[[2]]/sum(.[[2]])))) %>%
    dplyr::pull(rel_ab)

  dta_conn_x <- assignment %>%
    dplyr::arrange(factor(.[[1]], levels = bnode_names)) %>%
    tibble::column_to_rownames(colnames(assignment)[1]) %>%
    dplyr::select(dplyr::all_of(wnode_names)) %>%
    as.matrix()

  obs_bnode_n<-nrow(dta_conn_x)
  obs_wnode_n<-ncol(dta_conn_x)
  wnode_gendat<-(1:obs_wnode_n)
  bnode_gendat<-(1:obs_bnode_n)

  jags.data3 <- list(obs_bnode_n=obs_bnode_n,
                     obs_wnode_n=obs_wnode_n,
                     dta_conn_x=dta_conn_x,
                     dta_conn_y=dta_conn_x,
                     dta_conn_colsum=colSums(dta_conn_x),
                     dta_conn_rowsum=rowSums(dta_conn_x),
                     dta_conn_effort=colSums(dta_conn_x)/sum(dta_conn_x), #genetic effort
                     dta_conn_nb.est.mn= pop.b,
                     dta_conn_nb.est.sum= sum(pop.b),
                     dta_conn_nw.est.mn= pop.w,
                     dta_conn_nw.est.sum= sum(pop.w),
                     known_connected=arrayInd(which(dta_conn_x>0),.dim=dim(dta_conn_x)),
                     known_n=sum(dta_conn_x>0),
                     unknown_connected=arrayInd(which(dta_conn_x==0),.dim=dim(dta_conn_x)),
                     unknown_n=sum(dta_conn_x==0)
  )
  return(jags.data3)
}
