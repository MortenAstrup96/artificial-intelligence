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
  }
  
  # Calculate emission matrix based on readings / probs
  emission_vector = calculate_emission_vector(readings, probs)
  
  # Calculate temporary edges matrix with swedes positions removed
  modified_edges = calculate_modified_edges(edges, positions)
  transition_matrix = calculate_transmatrix(modified_edges)
  
  # Calculate new probability vector based on previous readings (HMM)
  moveInfo$mem$probs = calculate_probability_vector(transition_matrix, moveInfo$mem$probs, emission_vector)
  
  # Goal node is the node with highest probability in vector
  goalNode = which.max(moveInfo$mem$probs)

  ## Calculate shortest path
  moveInfo$moves = c(1, 2)
  
  graph = make_graph(transition_matrix)
  
  shortest = get_shortest_path(positions[[3]], goalNode, graph)
  
  print("Found shortest")
  print(shortest)
  
  ## If we are standing on the spot
  if(length(shortest) == 1) {
    moveInfo$moves = c(0, 0,)
    
  ## If we predict croc is next to our current node
  } else if(length(shortest) == 2) {
    moveInfo$moves = c(shortest[[2]], 0)
  
  ## We will move towards croc
  } else {
    moveInfo$moves = c(shortest[[2]], shortest[[3]])  
  }
  print(moveInfo$moves)
  return(moveInfo)
}

# Inspired from https://www.r-bloggers.com/2020/10/finding-the-shortest-path-with-dijkstras-algorithm/
get_shortest_path = function(start, end, graph, path = c()) {
  if(is.null(graph[[start]])) return(NULL)
  
  path = c(path, start)
  
  # If we are already on the correct spot
  if(start == end) {
    return (path)
  }

  shortest = NULL
  
  # Loop through each node connected to our current node
  for(node in graph[[start]]) {
    
    ## If the node is NOT in our current path, run shortest path on it
    if(!(node %in% path)) {
      newPath = get_shortest_path(node, end, graph, path)
      
        ## If the path we just found is horter than our current, mark it as shortest
        if(path_length(newPath) < path_length(shortest)) {
          shortest = newPath
        }
      }
  }
  return (shortest)
}

path_length <- function(path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
 # print(length(path))
  return(length(path))
}


# Calculates every round all edges, but removes the connections between swede positions
calculate_modified_edges = function(edges, positions) {
  isSwedeOneAlive = !is.na(positions[[1]])
  isSwedeTwoAlive = !is.na(positions[[2]])
  
  modified_edges = edges
  for(i in 1:2) {
    for (j in 1:nrow(modified_edges)) {
      if((isSwedeOneAlive && modified_edges[[j, i]] == positions[[1]]) || (isSwedeTwoAlive && modified_edges[[j, i]] == positions[[2]])) {
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
