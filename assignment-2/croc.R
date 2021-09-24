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
    
    transition_matrix = initialize_transmatrix(edges)
    
  }
  emission_vector = calculate_emission_vector(readings, probs)
  
  prob_vector = moveInfo$mem$probs
  new_prob_vector = rep(0, 40)
  
  
  for(i in 1:40) {
    sum = 0
    for(j in 1:nrow(transition_matrix)) {
      sum = sum + (prob_vector[[j]] * transition_matrix[[j,i]])
    }
    result = sum * emission_vector[[i]]
    new_prob_vector[[i]] = result
  }
  
  moveInfo$mem$probs = new_prob_vector
  
  goalNode = which.max(new_prob_vector)
  

  ## Calculate shortest path
  moveInfo$moves = c(1, 2)
  
  make_graph(transition_matrix)
  return(moveInfo)
}

make_graph = function(transition_matrix) {
  graph = list()
  
  for (i in 1:ncol(transition_matrix)) {
    neighbour_vec = c()
    for (j in 1:ncol(transition_matrix)) {
      if(transition_matrix[j,i]!=0) {
        neighbour_vec = append(neighbour_vec,j) 
      }
    }
    graph[[i]] = neighbour_vec
  }
  
  str(graph)
  
  return(graph)
}

get_shortest_path = function(start, end, edges) {

  
  
  
  
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
    
    result =  dnorm(readings[[1]], salinity_mean, salinity_std) * 
              dnorm(readings[[2]], phosphate_mean, phosphate_std) * 
              dnorm(readings[[3]], nitrogen_mean, nitrogen_std)
    
    emission_vector[i] = result
  }
  return (emission_vector)
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
