#==============================================================================
# This is a script for ENVE 727 group project on bayesion belief networks
#
# Tyler Bradley
# 2018-02-06
#==============================================================================

update_prior <- function(p, l){
  pX_update <- (p$X*l$X)/(p$X * l$X + p$J * l$J + p$O * l$O)
  pJ_update <- (p$J*l$J)/(p$X * l$X + p$J * l$J + p$O * l$O)
  pO_update <- (p$O*l$O)/(p$X * l$X + p$J * l$J + p$O * l$O)
  
  output <- list(
    X = pX_update,
    J = pJ_update,
    O = pO_update
  )
  
  return(output)
}


diag_ben <- function(p, l){
  dbX <- (l$X)/((p$J*l$J + p$O*l$O)/(1-p$X))
  dbJ <- (l$J)/((p$X*l$X + p$O*l$O)/(1-p$J))
  dbO <- (l$O)/((p$X*l$X + p$J*l$J)/(1-p$O))
  
  output <- list(
    X = dbX,
    J = dbJ, 
    O = dbO
  )
  
  return(output)
}

p_prior <- list(X = 0.1, J = 0.01, O = 0.89)
lent_car <- list(X = 0.1, J = 0.05, O = 0.02)
ask_ride <- list(X = 0.5, J = 0.05, O = 0.05)

p_post1 <- update_prior(p = p_prior, l = lent_car)
db1 <- diag_ben(p = p_prior, l = lent_car)

p_post2 <- update_prior(p = p_post1, l = ask_ride)
diag_ben(p = p_post1, l = ask_ride)
