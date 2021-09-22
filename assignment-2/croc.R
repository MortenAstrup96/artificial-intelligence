myFunction = function(moveInfo, readings, positions, edges, probs) {
    print("test")
  
  #Initial state probability for each node
  #prob_vector <- moveInfo$mem
  
  if(moveInfo$mem$status == 0) {prob_vector <- initialize(positions)}
  print(prob_vector)
  
}

#If first round set all prob except backpacker location
initialize = function(pos) {
  prob_vector <- rep(1/38,40)
  prob_vector[pos[1]] <- 0
  prob_vector[pos[2]] <- 0
  return (prob_vector)
}


