#' Create the migratory network model to be run in JAGS
#'
#' @param base_filename Character string of file name for the model .txt file that will be saved. Model integer added as suffix to name.
#' @param model Select assignment model type ("BR" = encounter nonbreeding/recovery breeding, NB = encounter breeding/recovery nonbreeding, FULL = both types of data provided)
#' @return Returns the file name of the model and saves the model as a text file
#' @export
#'
#'
get_jags_model <- function(base_filename = "jags", model){

  if(model == "BR"){
    base_filename <- paste0(base_filename, ".model_BR.txt")
    ################################################################################
    # JAGS model 1
    ############################################################################
    # Connectivity Model -
    # using assignment data as a breeding x nonbreeding matrix, with the inferred nodes as rows (e.g. genetic assignment)
    # connectivity data in :
    # obs_brnode_n = number of breeding nodes
    # obs_nbnode_n = number of nonbreeding nodes
    # nbnode_gendat = indices for nonbreeding node in connectivity matrix with data
    # brnode_gendat  = indices for breeding node in connectivity matrix with data
    # dta_conn_x and dta_conn_y - these are exact copies of each other
    # dta_conn_colsum = column sums of connectivity (nonbreeding)
    # dta_conn_rowsum = row sums of connectivity (breeding)
    # dta_conn_effort = relative trapping effort at each nonbreeding node
    # dta_conn_nb.est.mn = mean population indices for breeding nodes, must be integers
    # dta_conn_nb.est.sum = sum of dta_conn_nb.est.mn
    # dta_conn_nw.est.mn = mean population indices for nonbreeding nodes,  must be integers
    # dta_conn_nw.est.sum = sum of dta_conn_nw.est.mn
    # known_connected = matrix with rows of (row, col) of each known connected (> 0 in asssignment matrix)
    # known_n = number of known connected
    # unknown_connected = matrix withrow, col of each unknown connected (== 0 in asssignment matrix)
    # unknown_n = number of unknown connected

    ## OPERATION: use autojags to run until convergence
    ############################################################################
    ################################################################################

    sink(file=base_filename)
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
        for(j in 1:obs_nbnode_n)
        {
          for(i in 1:obs_brnode_n){
              connected[i,j] ~ dbern(psi[i,j])
              conn[i,j] <- alpha2[i,j]/sum(alpha2)
              alpha2[i,j]<-alpha[i,j]*connected[i,j]
              alpha[i,j] ~ dpois(lambda[i,j])
              lambda[i,j] ~ dunif(0,1000)
            }
        }



        #---------------------------------------------------------------------------
        # Part 1. The data in colums are the number of individuals trapped at each
        # non-breeding node. these are assumed to be multinomial.
        #---------------------------------------------------------------------------
        for(j in 1:obs_nbnode_n)
        {
            dta_conn_y[1:obs_brnode_n,j] ~ dmulti(conn[,j], dta_conn_colsum[j])
        }
        #---------------------------------------------------------------------------
        # Part 2 The data in rows can also be assumed to be mutinomial but we have to
        # adjust for the fact that there was different effort at each nonbreeding node so
        # the probabilities of the multinomial are multiplied by relative effort
        # which is  estimated as proportion of total no. of birds sampled. Have to use a
        # different variable name, dta_conn_x which is an exact copy of dta_conn_y
        #---------------------------------------------------------------------------
        for (i in 1:obs_brnode_n){
            dta_conn_x[i,1:obs_nbnode_n] ~ dmulti(px[i,], dta_conn_rowsum[i])
            dta_conn_x.exp[i,1:obs_nbnode_n] <- px[i,]/sum(px[i,]) * dta_conn_rowsum[i] # expected value
            for (j in 1:obs_nbnode_n){
                px[i,j] <- conn[i,j]*dta_conn_effort[j]
                dta_conn_x.rep[i,j] ~ dbinom(px[i,j] / sum(px[i,]), dta_conn_rowsum[i]) # random samples, avoid incomplete multinomial
                FT.obs[i,j] <- pow(pow(dta_conn_x[i,j], 0.5) - pow(dta_conn_x.exp[i,j], 0.5), 2) # Freeman-Tukey observed
                FT.rep[i,j] <- pow(pow(dta_conn_x.rep[i,j], 0.5) - pow(dta_conn_x.exp[i,j], 0.5), 2) # Freeman-Tukey simulated
            } #j
        } #i

        fit <- sum(FT.obs[,])
        fit.rep <- sum(FT.rep[,])
        fit.diff <- fit - fit.rep
        bpval <- step(fit.diff) # Bayesian p-value

        #---------------------------------------------------------------------------
        # Part 3. Assume the breeding population indices (mut be integers) are mutinom
        # dta_conn_nb.est.sum is the sum of dta_conn_nb.est.mn
        #---------------------------------------------------------------------------
        for(i in 1:obs_brnode_n)
        {
            pb[i]<-sum(conn[i,])
        }
        dta_conn_nb.est.mn ~ dmulti(pb,dta_conn_nb.est.sum)

        #---------------------------------------------------------------------------
        # Part 4 Assume the nonbreeding population indices (mut be integers) are mutinom
        #---------------------------------------------------------------------------
        for(j in 1:obs_nbnode_n)
        {
            pw[j]<-sum(conn[,j])
         }

        dta_conn_nw.est.mn ~ dmulti(pw,dta_conn_nw.est.sum)



        }
        ", fill=TRUE)
    sink()
  } else if(model == "NB"){
    base_filename <- paste0(base_filename, ".model_NB.txt")

    ################################################################################
    #JAGS model 2
    ############################################################################
    # Connectivity Model -
    # using assignment data as a nonbreeding x breeding matrix, with the inferred nodes as rows (e.g. geolocator assignment of breeding individuals to nonbreeding populations)
    # obs_brnode_n = number of breeding nodes
    # obs_nbnode_n = number of nonbreeding nodes
    # nbnode_gldat = indices for nonbreeding node in connectivity matrix with data
    # brnode_gldat  = indices for breeding node in connectivity matrix with data
    # dta_conn_glx and dta_conn_gly - these are exact copies of each other
    # dta_conn_glcolsum = column sums of connectivity (sampled nodes)
    # dta_conn_glrowsum = row sums of connectivity (inferred nodes)
    # dta_conn_gleffort = relative trapping effort at each nonbreeding node, i.e. colSums(dta_conn_glx)/sum(dta_conn_glx)
    # dta_conn_nb.est.mn = mean population indices for breeding nodes, must be integers
    # dta_conn_nb.est.sum = sum of dta_conn_nb.est.mn
    # dta_conn_nw.est.mn = mean population indices for nonbreeding nodes,  must be integers
    # dta_conn_nw.est.sum = sum of dta_conn_nw.est.mn
    # known_connected = matrix with rows of (row, col) of each known connected (> 0 in asssignment matrix)
    # known_n = number of known connected
    # unknown_connected = matrix withrow, col of each unknown connected (== 0 in asssignment matrix)
    # unknown_n = number of unknown connected

    ## OPERATION: use autojags to run until convergence
    ############################################################################
    ################################################################################

    sink(file=base_filename)
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
    for(j in 1:obs_nbnode_n)
    {
      for(i in 1:obs_brnode_n){
          connected[i,j] ~ dbern(psi[i,j])
          conn[i,j] <- alpha2[i,j]/sum(alpha2)
          alpha2[i,j]<-alpha[i,j]*connected[i,j]
          alpha[i,j] ~ dpois(lambda[i,j])
          lambda[i,j] ~ dunif(0,1000)
        }
    }

    #---------------------------------------------------------------------------
    # Part 1_gl. Nonbreeding nodes. The data in colums are the number of individuals
    # *inferred to each non-breeding node. These are assumed to be multinomial but we have to
    # adjust for the fact that there was different effort at each *sampled* breeding node so
    # the probabilities of the multinomial are multiplied by relative effort
    # which is  estimated as proportion of total no. of birds sampled.
    #---------------------------------------------------------------------------

    #add array for which node has data, replace 1:obs_nbnode_n with array name

    for(j in nbnode_gldat)
    {
        dta_conn_gly[1:obs_brnode_n,j] ~ dmulti(pglx[,j], dta_conn_glcolsum[j])
        dta_conn_gly.exp[1:obs_brnode_n,j] <- pglx[,j]/sum(pglx[,j]) * dta_conn_glcolsum[j] # expected value

        for (i in 1:obs_brnode_n){
            pglx[i,j] <- conn[i,j]*dta_conn_gleffort[i]

            dta_conn_gly.rep[i,j] ~ dbinom(pglx[i,j] / sum(pglx[,j]), dta_conn_glcolsum[j]) # random samples, avoid incomplete multinomial
            FT.obs[i,j] <- pow(pow(dta_conn_gly[i,j], 0.5) - pow(dta_conn_gly.exp[i,j], 0.5), 2) # Freeman-Tukey observed
            FT.rep[i,j] <- pow(pow(dta_conn_gly.rep[i,j], 0.5) - pow(dta_conn_gly.exp[i,j], 0.5), 2) # Freeman-Tukey simulated

        } #i

    } #j

    fit <- sum(FT.obs[,])
    fit.rep <- sum(FT.rep[,])
    fit.diff <- fit - fit.rep
    bpval <- step(fit.diff) # Bayesian p-value

    #---------------------------------------------------------------------------
    # Part 2_gl The data in rows can also be assumed to be mutinomial. Have to use a
    # different variable name, dta_conn_glx which is an exact copy of dta_conn_gly
    #---------------------------------------------------------------------------
   #add array for which node has data, replace 1:obs_nbnode_n with array name

    for (i in brnode_gldat){
        dta_conn_glx[i,1:obs_nbnode_n] ~ dmulti(conn[i,], dta_conn_glrowsum[i])
    } #i

    #---------------------------------------------------------------------------
    # Part 3. Assume the breeding population indices (mut be integers) are mutinom
    # dta_conn_nb.est.sum is the sum of dta_conn_nb.est.mn
    #---------------------------------------------------------------------------
    for(i in 1:obs_brnode_n)
    {
        pb[i]<-sum(conn[i,])
    }
    dta_conn_nb.est.mn ~ dmulti(pb,dta_conn_nb.est.sum)

    #---------------------------------------------------------------------------
    # Part 4 Assume the nonbreeding population indices (mut be integers) are mutinom
    #---------------------------------------------------------------------------
    for(j in 1:obs_nbnode_n)
    {
        pw[j]<-sum(conn[,j])
     }

    dta_conn_nw.est.mn ~ dmulti(pw,dta_conn_nw.est.sum)



    }
    ", fill=TRUE)
    sink()
  } else{
    base_filename <- paste0(base_filename, ".model_BR-NB.txt")
    ################################################################################
    #JAGS model 3
    ############################################################################
    # Connectivity Model -
    # using assignment data as a breeding x nonbreeding matrix (e.g. genetic assignment of nonbreeding individuals to breeding populations)
    # AND using assignment data as a nonbreeding x breeding matrix (e.g. geolocator assignment of breeding individuals to nonbreeding populations)
    # connectivity data in :
    # dta_conn_x and dta_conn_y- these are exact copies of each other
    # dta_conn_colsum - col sums
    # dta_conn_rowsum - row sums
    # dta_conn_effort -relative trapping effort at each nonbreeding node
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

    sink(file=base_filename)
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
    for(j in 1:obs_nbnode_n)
    {
      for(i in 1:obs_brnode_n){
          connected[i,j] ~ dbern(psi[i,j])
          conn[i,j] <- alpha2[i,j]/sum(alpha2)
          alpha2[i,j]<-alpha[i,j]*connected[i,j]
          alpha[i,j] ~ dpois(lambda[i,j])
          lambda[i,j] ~ dunif(0,1000)
        }
    }



    #---------------------------------------------------------------------------
    # Part 1. The data in colums are the number of individuals trapped at each
    # non-breeding node. these are assumed to be multinomial.
    #---------------------------------------------------------------------------
    #add array for which node has data, replace 1:obs_nbnode_n with array name
    for(j in nbnode_gendat)
    {
        dta_conn_y[1:obs_brnode_n,j] ~ dmulti(conn[,j], dta_conn_colsum[j])
    }
    #---------------------------------------------------------------------------
    # Part 2 The data in rows can also be assumed to be mutinomial but we have to
    # adjust for the fact that there was different effort at each nonbreeding node so
    # the probabilities of the multinomial are multiplied by relative effort
    # which is  estimated as proportion of total no. of birds sampled. Have to use a
    # different variable name, dta_conn_x which is an exact copy of dta_conn_y
    #---------------------------------------------------------------------------

    #add array for which node has data, replace 1:obs_nbnode_n with array name

    for (i in brnode_gendat){
        dta_conn_x[i,1:obs_nbnode_n] ~ dmulti(px[i,], dta_conn_rowsum[i])
        dta_conn_x.exp[i,1:obs_nbnode_n] <- px[i,]/sum(px[i,]) * dta_conn_rowsum[i] # expected value
        for (j in 1:obs_nbnode_n){
            px[i,j] <- conn[i,j]*dta_conn_effort[j]

            dta_conn_x.rep[i,j] ~ dbinom(px[i,j] / sum(px[i,]), dta_conn_rowsum[i]) # random samples, avoid incomplete multinomial
            FT.obs[i,j] <- pow(pow(dta_conn_x[i,j], 0.5) - pow(dta_conn_x.exp[i,j], 0.5), 2)
            FT.rep[i,j] <- pow(pow(dta_conn_x.rep[i,j], 0.5) - pow(dta_conn_x.exp[i,j], 0.5), 2)
        } #j
    } #i

    fit <- sum(FT.obs[,])
    fit.rep <- sum(FT.rep[,])
    fit.diff <- fit - fit.rep
    bpval <- step(fit.diff) # Bayesian p-value

    #---------------------------------------------------------------------------
    # Part 1_gl. The data in colums are the number of individuals trapped at each
    # non-breeding node. these are assumed to be multinomial.
    #---------------------------------------------------------------------------

    #add array for which node has data, replace 1:obs_nbnode_n with array name

    for(j in nbnode_gldat)
    {
        dta_conn_gly[1:obs_brnode_n,j] ~ dmulti(pglx[,j], dta_conn_glcolsum[j])

               for (i in 1:obs_brnode_n){
            pglx[i,j] <- conn[i,j]*dta_conn_gleffort[i]
        } #i

    }

    #pglx
    #dta_conn_glcolsum

    #---------------------------------------------------------------------------
    # Part 2_gl The data in rows can also be assumed to be mutinomial. Have to use a
    # different variable name, dta_conn_glx which is an exact copy of dta_conn_gly
    #---------------------------------------------------------------------------
   #add array for which node has data, replace 1:obs_nbnode_n with array name

    for (i in brnode_gldat){
        dta_conn_glx[i,1:obs_nbnode_n] ~ dmulti(conn[i,], dta_conn_glrowsum[i])
    } #i

    #---------------------------------------------------------------------------
    # Part 3. Assume the breeding population indices (mut be integers) are mutinom
    # dta_conn_nb.est.sum is the sum of dta_conn_nb.est.mn
    #---------------------------------------------------------------------------
    for(i in 1:obs_brnode_n)
    {
        pb[i]<-sum(conn[i,])
    }
    dta_conn_nb.est.mn ~ dmulti(pb,dta_conn_nb.est.sum)

    #---------------------------------------------------------------------------
    # Part 4 Assume the nonbreeding population indices (mut be integers) are mutinom
    #---------------------------------------------------------------------------
    for(j in 1:obs_nbnode_n)
    {
        pw[j]<-sum(conn[,j])
     }

    dta_conn_nw.est.mn ~ dmulti(pw,dta_conn_nw.est.sum)



    }
    ", fill=TRUE)
    sink()

  }
  return(base_filename)
}
