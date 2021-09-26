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
    moveInfo$mem[["isSwedeAlive"]] = c(T, T)
  }
  
  # Calculate emission matrix based on readings / probs
  emission_vector = calculate_emission_vector(readings, probs)
  
  # Calculate temporary edges matrix with swedes positions removed
  modified_edges = calculate_modified_edges(edges, moveInfo$mem[["isSwedeAlive"]], positions)
  transition_matrix = calculate_transmatrix(modified_edges)
  
  # Calculate new probability vector based on previous readings (HMM)
  moveInfo$mem$probs = calculate_probability_vector(transition_matrix, moveInfo$mem$probs, emission_vector)
  
  # Goal node is the node with highest probability in vector
  goalNode = which.max(moveInfo$mem$probs)

  print(goalNode)
  ## Calculate shortest path
  moveInfo$moves = c(1, 2)
  
  make_graph(transition_matrix)
  return(moveInfo)
}

calculate_modified_edges = function(edges, isSwedeAlive, positions) {
  modified_edges = edges
  for(i in 1:2) {
    for (j in 1:nrow(modified_edges)) {
      if((isSwedeAlive[[1]] && modified_edges[[j, i]] == positions[[1]]) || (isSwedeAlive[[2]] && modified_edges[[j, i]] == positions[[2]])) {
        modified_edges[j,] = Inf
      }
    }
  }
  
  for (j in nrow(modified_edges):1) {
    if(modified_edges[j,] == Inf) {
      modified_edges = modified_edges[-j,]
    }
  }
  
  return (modified_edges)
}

calculate_probability_vector = function(transition_matrix, prob_vector, emission_vector) {
  new_prob_vector = rep(0, 40)
  
  # Loop through each column (each node)
  for(i in 1:40) {
    sum = 0
    
    # For each column (node) calculate the probabilities to go here from previous round 
    # based on transition & emission matrix
    for(j in 1:nrow(transition_matrix)) {
      sum = sum + (prob_vector[[j]] * transition_matrix[[j,i]])
    }
    result = sum * emission_vector[[i]]
    
    new_prob_vector[[i]] = result
  }
  
  return (new_prob_vector)
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

calculate_transmatrix = function(edges) {
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
