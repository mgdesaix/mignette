#' Set up the data for the migratory network model in JAGS
#'
#' @param abundance Tibble of abundance data across populations (model nodes)
#' @param nb2br_assign Tibble of assignment for known nonbreeding (columns) to inferred breeding populations (rows)
#' @param br2nb_assign Tibble of assignment for known breeding (columns) to inferred nonbreeding populations (rows)
#' @param bnode_names Names of breeding populations/nodes
#' @param wnode_names Names of nonbreeding populations/nodes
#' @param model Select integer value of assignment model type (1 = known nonbreeding/inferred breeding, 2 = known breeding/inferred nonbreeding, 3 = both types of data provided)
#' @return A list of of input data for the JAGS model
#' @export
#'
#'
get_jags_data <- function(abundance, bnode_names, wnode_names, model,
                          nb2br_assign = NULL, br2nb_assign = NULL){
  stopifnot("`bnode_names` must correspond to values in first column of `abundance` tibble" = bnode_names %in% dplyr::pull(abundance, 1))
  stopifnot("`wnode_names` must correspond to values in first column of `abundance` tibble" = wnode_names %in% dplyr::pull(abundance, 1))
  stopifnot("Not a valid assignment model choice - must be integers 1, 2, or 3" = model %in% c(1,2,3))
  if (model == 1 | model == 3){
    stopifnot("No assignment file of nonbreeding to breeding (`nb2br_assign`) provided!" = !is.null(nb2br_assign))
    stopifnot("First column of `nb2br_assign` tibble must correspond to values in `bnode_names`" = dplyr::pull(nb2br_assign, 1) %in% bnode_names)
    stopifnot("Column names of `nb2br_assign` tibble starting with column 2 must be in `wnode_names`" = colnames(nb2br_assign)[2:length(colnames(nb2br_assign))] %in% wnode_names)
  }
  if (model == 2 | model == 3){
    stopifnot("No assignment file of breeding to nonbreeding (`br2nb_assign`) provided!" = !is.null(br2nb_assign))
    stopifnot("First column of `br2nb_assign` tibble must correspond to values in `wnode_names`" = dplyr::pull(br2nb_assign, 1) %in% wnode_names)
    stopifnot("Column names of `br2nb_assign` tibble starting with column 2 must be in `bnode_names`" = colnames(br2nb_assign)[2:length(colnames(br2nb_assign))] %in% bnode_names)
  }

  ## Abundance stats used in all models
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
  ### Model 1:
  ## Known/sampled nonbreeding, inferred breeding origin (e.g. genetics)
  if (model == 1) {
    dta_conn_x <- nb2br_assign %>%
      dplyr::arrange(factor(.[[1]], levels = bnode_names)) %>%
      tibble::column_to_rownames(colnames(nb2br_assign)[1]) %>%
      dplyr::select(dplyr::all_of(wnode_names)) %>%
      as.matrix()

    obs_bnode_n<-length(bnode_names)
    obs_wnode_n<-length(wnode_names)
    wnode_gendat<-which(colnames(dta_conn_x) %in% wnode_names)
    bnode_gendat<-which(rownames(dta_conn_x) %in% bnode_names)

    jags.data <- list(obs_bnode_n=obs_bnode_n,
                      obs_wnode_n=obs_wnode_n,
                      wnode_gendat=wnode_gendat,
                      bnode_gendat=bnode_gendat,
                       dta_conn_x=dta_conn_x,
                       dta_conn_y=dta_conn_x,
                       dta_conn_colsum=colSums(dta_conn_x),
                       dta_conn_rowsum=rowSums(dta_conn_x),
                       dta_conn_effort=colSums(dta_conn_x)/sum(dta_conn_x),
                       dta_conn_nb.est.mn= pop.b,
                       dta_conn_nb.est.sum= sum(pop.b),
                       dta_conn_nw.est.mn= pop.w,
                       dta_conn_nw.est.sum= sum(pop.w),
                       known_connected=arrayInd(which(dta_conn_x>0),.dim=dim(dta_conn_x)),
                       known_n=sum(dta_conn_x>0),
                       unknown_connected=arrayInd(which(dta_conn_x==0),.dim=dim(dta_conn_x)),
                       unknown_n=sum(dta_conn_x==0)
    )
  } else if (model == 2){
    ### Model 2:
    ## Known/sampled breeding, inferred nonbreeding origin (e.g. geolocators)
    dta_conn_glx <- br2nb_assign %>%
      dplyr::arrange(factor(.[[1]], levels = wnode_names)) %>%
      tibble::column_to_rownames(colnames(nb2br_assign)[1]) %>%
      dplyr::select(dplyr::all_of(bnode_names)) %>%
      as.matrix()

    obs_bnode_n<-length(bnode_names)
    obs_wnode_n<-length(wnode_names)
    wnode_gldat<-which(rownames(dta_conn_glx) %in% wnode_names)
    bnode_gldat<-which(colnames(dta_conn_glx) %in% bnode_names)

    jags.data <- list(obs_bnode_n=obs_bnode_n,
                      obs_wnode_n=obs_wnode_n,
                      bnode_gldat=bnode_gldat,
                      wnode_gldat=wnode_gldat,
                      dta_conn_glx=dta_conn_glx,
                      dta_conn_gly=dta_conn_glx,
                      dta_conn_glcolsum=colSums(dta_conn_glx),
                      dta_conn_glrowsum=rowSums(dta_conn_glx),
                      dta_conn_gleffort=colSums(dta_conn_glx)/sum(dta_conn_glx),
                      dta_conn_nb.est.mn= pop.b,
                      dta_conn_nb.est.sum= sum(pop.b),
                      dta_conn_nw.est.mn= pop.w,
                      dta_conn_nw.est.sum= sum(pop.w),
                      known_connected=arrayInd(which(dta_conn_glx>0),.dim=dim(dta_conn_glx)),
                      known_n=sum(dta_conn_glx>0),
                      unknown_connected=arrayInd(which(dta_conn_glx==0),.dim=dim(dta_conn_glx)),
                      unknown_n=sum(dta_conn_glx==0)
    )
  } else {
    ### Model 3:
    ## Known/sampled nonbreeding, inferred breeding origin (e.g. genetics) AND
    ## data for known/sampled breeding, inferred nonbreeding origin (e.g. geolocators)
    dta_conn_x <- nb2br_assign %>%
      dplyr::arrange(factor(.[[1]], levels = bnode_names)) %>%
      tibble::column_to_rownames(colnames(nb2br_assign)[1]) %>%
      dplyr::select(dplyr::all_of(wnode_names)) %>%
      as.matrix()

    wnode_gendat<-which(colnames(dta_conn_x) %in% wnode_names)
    bnode_gendat<-which(rownames(dta_conn_x) %in% bnode_names)

    dta_conn_glx <- br2nb_assign %>%
      dplyr::arrange(factor(.[[1]], levels = wnode_names)) %>%
      tibble::column_to_rownames(colnames(nb2br_assign)[1]) %>%
      dplyr::select(dplyr::all_of(bnode_names)) %>%
      as.matrix()

    wnode_gldat<-which(rownames(dta_conn_glx) %in% wnode_names)
    bnode_gldat<-which(colnames(dta_conn_glx) %in% bnode_names)

    obs_bnode_n<-length(bnode_names)
    obs_wnode_n<-length(wnode_names)

    jags.data <- list(obs_bnode_n=obs_bnode_n,
                      obs_wnode_n=obs_wnode_n,
                      wnode_gendat=wnode_gendat,
                      bnode_gendat=bnode_gendat,
                      bnode_gldat=bnode_gldat,
                      wnode_gldat=wnode_gldat,
                      dta_conn_x=dta_conn_x,
                      dta_conn_y=dta_conn_x,
                      dta_conn_glx=dta_conn_glx,
                      dta_conn_gly=dta_conn_glx,
                      dta_conn_effort=colSums(dta_conn_x)/sum(dta_conn_x), #genetic effort
                      dta_conn_colsum=colSums(dta_conn_x),
                      dta_conn_rowsum=rowSums(dta_conn_x),
                      dta_conn_gleffort=rowSums(dta_conn_glx)/sum(dta_conn_glx), #gl effort
                      dta_conn_glcolsum=colSums(dta_conn_glx),
                      dta_conn_glrowsum=rowSums(dta_conn_glx),
                      dta_conn_nb.est.mn= pop.b,
                      dta_conn_nb.est.sum= sum(pop.b),
                      dta_conn_nw.est.mn= pop.w,
                      dta_conn_nw.est.sum= sum(pop.w),
                      known_connected=arrayInd(which(dta_conn_x>0),.dim=dim(dta_conn_x)),
                      known_n=sum(dta_conn_x>0),
                      unknown_connected=arrayInd(which(dta_conn_x==0),.dim=dim(dta_conn_x)),
                      unknown_n=sum(dta_conn_x==0)
    )
  }
  return(jags.data)
}
