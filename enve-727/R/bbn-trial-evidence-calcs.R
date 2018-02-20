#==============================================================================
# This is a script for ENVE 727 group project on bayesion belief networks
#
# Tyler Bradley
# 2018-02-06
#==============================================================================

# This function will take the prior probability that each of the 
# three possible suspects (X, J, and O (other)) are guilty as a named 
# list `p` when the names are X, J, and O respectively.
# Additionally, it will take the likelihood, `l`, that a given piece of 
# evidence is true given that a given suspect is guilty. This 
# should also be given as a named list with the same names 
# (X, J, O) as p. 
# This will return a named list by the same convention 
# with the posterior probabilites of guilt for each of the three
# suspects
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

# This function will take the prior probability that each of the 
# three possible suspects (X, J, and O (other)) are guilty as a named 
# list `p` when the names are X, J, and O respectively.
# Additionally, it will take the likelihood, `l`, that a given piece of 
# evidence is true given that a given suspect is guilty. This 
# should also be given as a named list with the same names 
# (X, J, O) as p. 
# This will return a named list by the same convention 
# with the diagnostic benefit for the given piece of evidence 
# on the probability of guilt for each suspect
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

p_prior <- list(X = c(0.1, 0.15), J = c(0.01, 0.05), O = c(0.89, 0.8))
lent_car <- list(X = c(0.1, 0.08), J = c(0.05, 0.06), O = c(0.02, 0.04))
ask_ride <- list(X = 0.5, J = 0.05, O = 0.05)

p_post1 <- update_prior(p = p_prior, l = lent_car)
db1 <- diag_ben(p = p_prior, l = lent_car)

p_post1
# $X
# [1] 0.3533569
# 
# $J
# [1] 0.01766784
# 
# $O
# [1] 0.6289753

db1
# $X
# [1] 4.918033
# 
# $J
# [1] 1.780576
# 
# $O
# [1] 0.2095238

p_post2 <- update_prior(p = p_post1, l = ask_ride)
db2 <- diag_ben(p = p_post1, l = ask_ride)


p_post2
# $X
# [1] 0.8453085
# 
# $J
# [1] 0.004226543
# 
# $O
# [1] 0.1504649

db2
# $X
# [1] 10
# 
# $J
# [1] 0.2359932
# 
# $O
# [1] 0.1044776