myFunction = function(moveInfo, readings, positions, edges, probs) {

  #Initial state probability for each node
  if(moveInfo$mem$status == 0) {
    prob_vector <- initialize(positions)
    
    #Append new vector to mem with probabilities
    moveInfo$mem[["probs"]] <- prob_vector
  }
  str(moveInfo)
  #print(moveInfo$mem)
  
  
  
  
  
}

#If first round set all prob except backpacker location
initialize = function(pos) {
  prob_vector <- rep(1/38,40)
  prob_vector[pos[1]] <- 0
  prob_vector[pos[2]] <- 0
  return (prob_vector)
}


