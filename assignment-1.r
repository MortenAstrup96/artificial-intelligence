# File:         demo.r
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson

# Install the package
#install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
# ?testDM

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if (carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix,
                                   carInfo,
                                   packageMatrix)
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3, 4)]
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[, 1] - carInfo$x)) +
    ((packageMatrix[, 2] - carInfo$y))
  distanceVector[packageMatrix[, 5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1, 2)])
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
  carPos <- c(carInfo$x, carInfo$y)
  destination <- c(carInfo$mem$goal[1], carInfo$mem$goal[2])
  calcAstar(carPos, destination, trafficMatrix)
}


calcAstar <- function(carPos, dest, trafficMatrix) {
  # 1. fronttier queue
  frontierList <- list()
  # Step 0: put first frontier (your position) to queue
  frontierList <- append(
    frontierList,
    list(
      nodePos = carPos,
      edgecost = 0,
      manhattan = 0,
      travelcost = 0,
      path = list()
    )
  )
  # 2. loop (A* Algorithm)
  # loop while ( !empty & goalDest)
  # currentFrontier = get first frontier
  scores = sapply(frontierList, function(item)
    item$travelcost)
  best_index = which.min(scores)
  
  # get cheapest frontier out of the list
  expandedFrontier = frontierList[[best_index]]
  # remove the cheapest frontier
  frontierList = frontierList[-best_index]
  
  # ToDo: check if the neighbor is in the grid
  # get neighbors
  # up-neighbor
  newPos = expandedFrontier$nodePos + c(0, 1)
  newedgecost = expandedFrontier$edgecost + trafficMatrix$vroad(expandedFrontier$nodePos)
  newmanhattan = 0
  newtravelcost = newedgecost + newmanhattan
  newpath = append(expandedFrontier$path, newPos)
  frontierList = append(
    frontierList,
    list(
      nodePos = newPos,
      edgecost = newedgecost,
      manhattan = newmanhattan,
      travelcost = newtravelcost,
      path = newpath
    )
  )
  # right-neighbora
  #
  #create frpmtoer --> add own costs to the  frontier cost
  # + also include path in array
  # 3. return cheap path (next step)
  
}
