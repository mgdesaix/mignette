#' Set up the data for the migratory network model for JAGS
#'
#' @param abundance Tibble of abundance data across populations (model nodes)
#' @param nb2br_assign Tibble of assignment for encounter season as nonbreeding (columns) and recovery season as breeding populations (rows)
#' @param br2nb_assign Tibble of assignment for encounter season as breeding (columns) to recovery season as nonbreeding populations (rows)
#' @param brnode_names Character vector of names of breeding populations/nodes
#' @param nbnode_names Character vector of names of nonbreeding populations/nodes
#' @param model Character string of assignment model type ("BR" = encounter nonbreeding/recovery breeding, NB = encounter breeding/recovery nonbreeding, BR-NB = both types of data provided)
#' @param base_filename Character string of file name for the model .txt file that will be saved. Model type added as suffix to name.
#' @param iter.increment Integer value of jagsUI::autojags() parameter iter.increment
#' @param n.thin Integer value of jagsUI::autojags() parameter n.thin
#' @param n.burnin Integer value of jagsUI::autojags() parameter n.burnin
#' @param n.chains Integer value of jagsUI::autojags() parameter n.chains
#' @param parallel Logical value of jagsUI::autojags() paramter parallel
#' @return A list of 1) "conn" is a dataframe of the connectivity matrix with node names, 2) "jags_out" is a list of the full standard jagsUI::autojags() output
#' @export
#'
#'
run_network_model <- function(abundance, brnode_names, nbnode_names,
                          model, base_filename = "jags",
                          nb2br_assign = NULL, br2nb_assign = NULL,
                          iter.increment = 500000, n.thin = 4,
                          n.burnin = 100000, n.chains = 3, parallel = FALSE){
  stopifnot("`brnode_names` must correspond to values in first column of `abundance` tibble" = brnode_names %in% dplyr::pull(abundance, 1))
  stopifnot("`nbnode_names` must correspond to values in first column of `abundance` tibble" = nbnode_names %in% dplyr::pull(abundance, 1))
  stopifnot("Not a valid assignment model choice - must be BR, NB, or FULL" = model %in% c("BR", "NB", "BR-NB"))

  if (model == "BR" | model == "BR-NB"){
    stopifnot("No assignment file of nonbreeding to breeding (`nb2br_assign`) provided!" = !is.null(nb2br_assign))
    stopifnot("First column of `nb2br_assign` tibble must correspond to values in `brnode_names`" = dplyr::pull(nb2br_assign, 1) %in% brnode_names)
    stopifnot("Column names of `nb2br_assign` tibble starting with column 2 must be in `wbode_names`" = colnames(nb2br_assign)[2:length(colnames(nb2br_assign))] %in% nbnode_names)
  }
  if (model == "NB" | model == "BR-NB"){
    stopifnot("No assignment file of breeding to nonbreeding (`br2nb_assign`) provided!" = !is.null(br2nb_assign))
    stopifnot("First column of `br2nb_assign` tibble must correspond to values in `brnode_names`" = dplyr::pull(br2nb_assign, 1) %in% brnode_names)
    stopifnot("Column names of `br2nb_assign` tibble starting with column 2 must be in `wbode_names`" = colnames(br2nb_assign)[2:length(colnames(br2nb_assign))] %in% nbnode_names)
  }

  ## Abundance stats used in all models
  pop.b <- abundance %>%
    dplyr::filter(.[[1]] %in% brnode_names) %>%
    dplyr::arrange(factor(.[[1]], levels = brnode_names)) %>%
    dplyr::mutate(rel_ab = as.numeric(round(100*.[[2]]/sum(.[[2]])))) %>%
    dplyr::pull(rel_ab)

  pop.w <- abundance %>%
    dplyr::filter(.[[1]] %in% nbnode_names) %>%
    dplyr::arrange(factor(.[[1]], levels = nbnode_names)) %>%
    dplyr::mutate(rel_ab = as.numeric(round(100*.[[2]]/sum(.[[2]])))) %>%
    dplyr::pull(rel_ab)
  ### Model 1:
  ## Known/sampled nonbreeding, inferred breeding origin (e.g. genetics)
  if (model == "BR"){

    dta_conn_x <- nb2br_assign %>%
      dplyr::arrange(factor(.[[1]], levels = brnode_names)) %>%
      tibble::column_to_rownames(colnames(nb2br_assign)[1]) %>%
      dplyr::select(nbnode_names[which(nbnode_names %in% colnames(nb2br_assign)[-1])]) %>%
      as.matrix()
    # error for rows and columns if they have all 0s
    stopifnot("Please remove columns from nb2br_assign with all 0s" = length(which(colSums(dta_conn_x) == 0)) == 0)
    stopifnot("Please remove rows from nb2br_assign with all 0s" = length(which(rowSums(dta_conn_x) == 0)) == 0)
    # error if mismatch in number of nodes and names
    stopifnot("Number of breeding nodes different in brnode_names and nb2br_assign rows" = length(brnode_names) == nrow(dta_conn_x))
    stopifnot("Number of nonbreeding nodes different in nbnode_names and nb2br_assign columns" = length(nbnode_names) == ncol(dta_conn_x))

    obs_brnode_n<-length(brnode_names)
    obs_nbnode_n<-length(nbnode_names)
    nbnode_gendat<-which(colnames(dta_conn_x) %in% nbnode_names)
    brnode_gendat<-which(rownames(dta_conn_x) %in% brnode_names)

    jags.data <- list(obs_brnode_n=obs_brnode_n,
                      obs_nbnode_n=obs_nbnode_n,
                      nbnode_gendat=nbnode_gendat,
                      brnode_gendat=brnode_gendat,
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
  } else if (model == "NB"){
    ### Model 2:
    ## Known/sampled breeding, inferred nonbreeding origin (e.g. geolocators)
    dta_conn_glx <- br2nb_assign %>%
      dplyr::arrange(factor(.[[1]], levels = brnode_names)) %>%
      tibble::column_to_rownames(colnames(br2nb_assign)[1]) %>%
      dplyr::select(nbnode_names[which(nbnode_names %in% colnames(br2nb_assign)[-1])]) %>%
      as.matrix()
    # error for rows and columns if they have all 0s
    stopifnot("Please remove columns from br2nb_assign with all 0s" = length(which(colSums(dta_conn_glx) == 0)) == 0)
    stopifnot("Please remove rows from br2nb_assign with all 0s" = length(which(rowSums(dta_conn_glx) == 0)) == 0)
    # error if mismatch in number of nodes and names
    stopifnot("Number of breeding nodes different in brnode_names and br2nb_assign rows" = length(brnode_names) == nrow(dta_conn_glx))
    stopifnot("Number of nonbreeding nodes different in nbnode_names and br2nb_assign columns" = length(nbnode_names) == ncol(dta_conn_glx))

    obs_brnode_n<-length(brnode_names)
    obs_nbnode_n<-length(nbnode_names)
    nbnode_gldat<-which(colnames(dta_conn_glx) %in% nbnode_names)
    brnode_gldat<-which(rownames(dta_conn_glx) %in% brnode_names)

    jags.data <- list(obs_brnode_n=obs_brnode_n,
                      obs_nbnode_n=obs_nbnode_n,
                      brnode_gldat=brnode_gldat,
                      nbnode_gldat=nbnode_gldat,
                      dta_conn_glx=dta_conn_glx,
                      dta_conn_gly=dta_conn_glx,
                      dta_conn_glcolsum=colSums(dta_conn_glx),
                      dta_conn_glrowsum=rowSums(dta_conn_glx),
                      dta_conn_gleffort=rowSums(dta_conn_glx)/sum(dta_conn_glx),
                      dta_conn_nb.est.mn=pop.b,
                      dta_conn_nb.est.sum=sum(pop.b),
                      dta_conn_nw.est.mn=pop.w,
                      dta_conn_nw.est.sum=sum(pop.w),
                      known_connected=arrayInd(which(dta_conn_glx>0),.dim=dim(dta_conn_glx)),
                      known_n=sum(dta_conn_glx>0),
                      unknown_connected=arrayInd(which(dta_conn_glx==0),.dim=dim(dta_conn_glx)),
                      unknown_n=sum(dta_conn_glx==0)
    )
  } else {
    ### Model 3:
    ## Known/sampled nonbreeding, inferred breeding origin (e.g. genetics) AND
    ## data for known/sampled breeding, inferred nonbreeding origin (e.g. geolocators)
    dta_conn_x_temp <- nb2br_assign %>%
      dplyr::arrange(factor(.[[1]], levels = brnode_names)) %>%
      tibble::column_to_rownames(colnames(nb2br_assign)[1]) %>%
      dplyr::select(nbnode_names[which(nbnode_names %in% colnames(nb2br_assign)[-1])]) %>%
      as.matrix()

    stopifnot("Please remove columns from nb2br_assign with all 0s" = length(which(colSums(dta_conn_x_temp) == 0)) == 0)
    stopifnot("Please remove rows from nb2br_assign with all 0s" = length(which(rowSums(dta_conn_x_temp) == 0)) == 0)

    dta_conn_x <- matrix(0, nrow = length(brnode_names), ncol = length(nbnode_names))
    rownames(dta_conn_x) <- brnode_names
    colnames(dta_conn_x) <- nbnode_names
    dta_conn_x[rownames(dta_conn_x_temp), colnames(dta_conn_x_temp)] <- dta_conn_x_temp

    # br2nb_assign data
    dta_conn_glx_temp <- br2nb_assign %>%
      dplyr::arrange(factor(.[[1]], levels = brnode_names)) %>%
      tibble::column_to_rownames(colnames(br2nb_assign)[1]) %>%
      dplyr::select(nbnode_names[which(nbnode_names %in% colnames(br2nb_assign)[-1])]) %>%
      as.matrix()
    # error for rows and columns if they have all 0s
    stopifnot("Please remove columns from br2nb_assign with all 0s" = length(which(colSums(dta_conn_glx_temp) == 0)) == 0)
    stopifnot("Please remove rows from br2nb_assign with all 0s" = length(which(rowSums(dta_conn_glx_temp) == 0)) == 0)

    dta_conn_glx <- matrix(0, nrow = length(brnode_names), ncol = length(nbnode_names))
    rownames(dta_conn_glx) <- brnode_names
    colnames(dta_conn_glx) <- nbnode_names
    dta_conn_glx[rownames(dta_conn_glx_temp), colnames(dta_conn_glx_temp)] <- dta_conn_glx_temp

    # get node indices
    nbnode_gendat<- which(nbnode_names %in% colnames(dta_conn_x_temp))
    brnode_gendat<- which(brnode_names %in% rownames(dta_conn_x_temp))

    nbnode_gldat <- which(nbnode_names %in% colnames(dta_conn_glx_temp))
    brnode_gldat <- which(brnode_names %in% rownames(dta_conn_glx_temp))

    obs_brnode_n<-length(brnode_names)
    obs_nbnode_n<-length(nbnode_names)

    jags.data <- list(obs_brnode_n=obs_brnode_n,
                      obs_nbnode_n=obs_nbnode_n,
                      nbnode_gendat=nbnode_gendat,
                      brnode_gendat=brnode_gendat,
                      nbnode_gldat=nbnode_gldat,
                      brnode_gldat=brnode_gldat,
                      #add array with breeding nodes with data-genetic + array of brnodes with gl data
                      dta_conn_x=dta_conn_x,
                      dta_conn_y=dta_conn_x,
                      dta_conn_glx=dta_conn_glx,
                      dta_conn_gly=dta_conn_glx,
                      dta_conn_colsum=colSums(dta_conn_x),
                      dta_conn_rowsum=rowSums(dta_conn_x),
                      dta_conn_glcolsum=colSums(dta_conn_glx),
                      dta_conn_glrowsum=rowSums(dta_conn_glx),
                      dta_conn_effort=colSums(dta_conn_x)/sum(dta_conn_x), #genetic effort
                      dta_conn_gleffort=rowSums(dta_conn_glx)/sum(dta_conn_glx), #gl effort
                      dta_conn_nb.est.mn= pop.b,
                      dta_conn_nb.est.sum= sum(pop.b),
                      dta_conn_nw.est.mn= pop.w,
                      dta_conn_nw.est.sum= sum(pop.w),
                      known_connected=arrayInd(which(dta_conn_x>0 | dta_conn_glx>0),.dim=dim(dta_conn_x)),
                      known_n=sum(dta_conn_x>0 | dta_conn_glx>0),
                      unknown_connected=arrayInd(which(dta_conn_x==0 & dta_conn_glx==0),.dim=dim(dta_conn_x)),
                      unknown_n=sum(dta_conn_x==0 & dta_conn_glx==0)
    )
  }
  # run get_jags_model
  jags_model_name <- mignette::get_jags_model(base_filename = base_filename, model = model)

  # run JAGS model

  parameters <- c("conn", "FT.obs", "FT.rep", "bpval")

  jags_out <- jagsUI::autojags(data = jags.data,
                               inits = NULL,
                               parameters.to.save = parameters,
                               model.file = jags_model_name,
                               n.chains = n.chains, n.thin = n.thin, iter.increment = iter.increment,
                               max.iter = iter.increment*50+n.burnin, n.burnin = n.burnin,
                               n.adapt = NULL, parallel = parallel)

  conn <- as.data.frame(jags_out$mean$conn)
  colnames(conn) <- nbnode_names
  conn.tib <- conn %>%
    round(5) %>%
    tibble::add_column("Breeding" = brnode_names, .before = 1)

  network_model <- list()
  network_model[["conn"]] <- conn.tib
  network_model[["jags_out"]] <- jags_out
  network_model[["brnode_names"]] <- brnode_names
  network_model[["nbnode_names"]] <- nbnode_names

  return(network_model)
}
