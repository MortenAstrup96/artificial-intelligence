myFunction = function(moveInfo,
                      readings,
                      positions,
                      edges,
                      probs) {
  #Initial state probability for each node
  if (moveInfo$mem$status == 0) {
    prob_vector <- initialize_probvector(positions)
    
    #Append new vector to mem with probabilities
    moveInfo$mem[["probs"]] <- prob_vector
    
    initialize_transmatrix(edges)
  }
  
  calculate_emission_vector(readings, probs)
}

calculate_emission_vector = function(readings, probs) {
  emission_vector = rep(0, 40)
  
  for (i in 1:40) {
    salinity_mean = probs$salinity[[i, 1]]
    salinity_std = probs$salinity[[i, 2]]
    
    phosphate_mean = probs$phosphate[[i, 1]]
    phosphate_std = probs$phosphate[[i, 2]]
    
    nitrogen_mean = probs$nitrogen[[i, 1]]
    nitrogen_std = probs$nitrogen[[i, 2]]
    
    result = dnorm(readings[[1]], salinity_mean, salinity_std) * dnorm(readings[[2]], phosphate_mean, phosphate_std) * dnorm(readings[[3]], nitrogen_mean, nitrogen_std)
    
    emission_vector[i] = result
  }
  print(emission_vector)
}

#######################
### Initializations ###
#######################
#If first round set all prob except backpacker location
initialize_probvector = function(pos) {
  prob_vector <- rep(1 / 38, 40)
  prob_vector[pos[1]] <- 0
  prob_vector[pos[2]] <- 0
  return (prob_vector)
}

initialize_transmatrix = function(edges) {
  trans_matrix <- matrix(0, nrow = 40, ncol = 40)
  
  #setting 1 values for counting adjacent nodes: to be counted
  for (i in 1:nrow(edges)) {
    a = edges[i, 1]
    b = edges[i, 2]
    trans_matrix[a, b] = 1
    trans_matrix[b, a] = 1
  }
  
  for (i in 1:40) {
    #changes diagonal values to 1
    trans_matrix[i, i] = 1
    #count how many times 1 is in column: edges adjacent to node
    freqof1percol <- length(which(trans_matrix[, i] == 1))
    for (j in 1:40) {
      #update t matrix with prob to each adjacent node
      if (trans_matrix[j, i] == 1) {
        trans_matrix[j, i] = 1 / freqof1percol
      }
    }
  }
  
  return(trans_matrix)
}
