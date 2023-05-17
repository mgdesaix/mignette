#' Create the migratory network model to be run in JAGS
#'
#' @param base_filename Character string of file name for the model .txt file that will be saved.
#' @param model_type Integer value, where 1 indicates genetic data being used, 2 indicates genetic and geolocator data
#' @return Returns the file name of the model and saves the model as a text file
#' @export
#'
#'
get_jags_model <- function(base_filename = "jags_genetic_model", model_type = 1){
  stopifnot("`type` must be 1 or 2, where 1 is for only genetic data, 2 is for geolocator and genetic data" = model_type %in% c(1,2))

  if(model_type == 1){
    ################################################################################
    # 1.  jags model
    ############################################################################
    # Connectivity Model - using genetic assignment data as a breeding x winter matrix
    # connectivity data in :
    # dta_conn_x and dta_conn_y- these are exact copies of each other
    # dta_conn_colsum - col sums
    # dta_conn_rowsum - row sums
    # dta_conn_effort -relative trapping effort at each winter node
    # dta_conn_nb.est.mn - mean population indices for b nodes, must be integers
    # dta_conn_nb.est.sum - sum of dta_conn_nb.est.mn
    # dta_conn_nw.est.mn - mean population indices for w nodes,  must be integers
    # dta_conn_nw.est.sum - sum of dta_conn_nw.est.mn
    # known_connected - matrix with rows of (row, col) of each known connected (> 0 in asssignment matrix)
    # unknown_connected - matrix withrow, col of each unknown connected (== 0 in asssignment matrix)
    # known_n - number of known connected
    # unknown_n - number of unknown connected

    ## OPERATION: use autojags to run until convergence
    ############################################################################
    ################################################################################

    sink(file=paste0(base_filename,".txt"))
    cat("
        model{


        #---------------------------------------------------------------------------
        # Priors
        #---------------------------------------------------------------------------
        for(k in 1:known_n)
        {
          psi[known_connected[k,1],known_connected[k,2]]<-1
        }
        for(k in 1:unknown_n)
        {
          psi[unknown_connected[k,1],unknown_connected[k,2]] ~ dunif(0,1)
        }
        for(j in 1:obs_wnode_n)
        {
          for(i in 1:obs_bnode_n){
              connected[i,j] ~ dbern(psi[i,j])
              conn_g[i,j] <- alpha2[i,j]/sum(alpha2)
              alpha2[i,j]<-alpha[i,j]*connected[i,j]
              alpha[i,j] ~ dpois(lambda[i,j])
              lambda[i,j] ~ dunif(0,1000)
            }
        }



        #---------------------------------------------------------------------------
        # Part 1. The data in colums are the number of individuals trapped at each
        # non-breeding node. these are assumed to be multinomial.
        #---------------------------------------------------------------------------
        for(j in 1:obs_wnode_n)
        {
            dta_conn_y[1:obs_bnode_n,j] ~ dmulti(conn_g[,j], dta_conn_colsum[j])
        }
        #---------------------------------------------------------------------------
        # Part 2 The data in rows can also be assumed to be mutinomial but we have to
        # adjust for the fact that there was different effort at each winter node so
        # the probabilities of the multinomial are multiplied by relative effort
        # which is  estimated as proportion of total no. of birds sampled. Have to use a
        # different variable name, dta_conn_x which is an exact copy of dta_conn_y
        #---------------------------------------------------------------------------
        for (i in 1:obs_bnode_n){
            dta_conn_x[i,1:obs_wnode_n] ~ dmulti(px[i,], dta_conn_rowsum[i])
            for (j in 1:obs_wnode_n){
                px[i,j] <- conn_g[i,j]*dta_conn_effort[j]
            } #j
        } #i

        #---------------------------------------------------------------------------
        # Part 3. Assume the breeding population indices (mut be integers) are mutinom
        # dta_conn_nb.est.sum is the sum of dta_conn_nb.est.mn
        #---------------------------------------------------------------------------
        for(i in 1:obs_bnode_n)
        {
            pb[i]<-sum(conn_g[i,])
        }
        dta_conn_nb.est.mn ~ dmulti(pb,dta_conn_nb.est.sum)

        #---------------------------------------------------------------------------
        # Part 4 Assume the winter population indices (mut be integers) are mutinom
        #---------------------------------------------------------------------------
        for(j in 1:obs_wnode_n)
        {
            pw[j]<-sum(conn_g[,j])
         }

        dta_conn_nw.est.mn ~ dmulti(pw,dta_conn_nw.est.sum)



        }
        ", fill=TRUE)
    sink()
  } else{

  }
  return(base_filename)
}
