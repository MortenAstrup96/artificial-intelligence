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
  destination <- c(carInfo$mem$goal[1], carInfo$mem$goal[2])
  nextCoordinates <- calcAstar(carInfo, destination, trafficMatrix)
  ## if(carX - destX != 0)
}


calcAstar <- function(carInfo, dest, trafficMatrix) {
  # 1. fronttier queue
  frontierList <- list()
  
  
  manhattan <- calcManhattanDist(carInfo$x, carInfo$y, dest[1], dest[2])
  # Step -1: put first frontier (your position) to queue
  firstNode <- list(
    x = carInfo$x,
    # pos x
    y = carInfo$y,
    # pos y
    g = 0,
    # Edge Cost
    h = manhattan,
    # Manhattan Cost
    f = manhattan,
    # Total Cost (Score)
    path = list()
  ) # Next move (Vector in the list))
  
  frontierList <- append(frontierList,
                         list(firstNode))

  ## --- Loop here ---
  while (length(frontierList) != 0) {
    ## Find best score in frontiers
    scores <- sapply(frontierList, function(item)
      (item[[5]]))
    best_index <- which.min(scores)
    
    ## Pop best node from frontierlist
    expandedFrontier <- frontierList[[best_index]]
    frontierList <- frontierList[-best_index]
    

    if(expandedFrontier[[4]] == 0) {
      #print(dest)
      str()
      return (expandedFrontier[[6]][[2]])
    }
    
    up <-
      findNeighbor(expandedFrontier, trafficMatrix, frontierList, 0, 1, 0, 0,  dest)  #up
    right <-
      findNeighbor(expandedFrontier, trafficMatrix, frontierList, 1, 0, 0, 0, dest)  #right
    down <-
      findNeighbor(expandedFrontier, trafficMatrix, frontierList, 0,-1, 0,-1, dest) #down
    left <-
      findNeighbor(expandedFrontier, trafficMatrix, frontierList,-1, 0,-1, 0, dest) #left
    
    
    if (isInsideGame(up, trafficMatrix))
      frontierList <- append(frontierList, list(up))
    if (isInsideGame(right, trafficMatrix))
      frontierList <- append(frontierList, list(right))
    if (isInsideGame(down, trafficMatrix))
      frontierList <- append(frontierList, list(down))
    if (isInsideGame(left, trafficMatrix))
      frontierList <- append(frontierList, list(left))
    
    #create frpmtoer --> add own costs to the  frontier cost
    # + also include path in array
    # 3. return cheap path (next step)
    
    
  }
  return (5)
}


calcManhattanDist <- function(a, b, x, y) {
  return(abs(x-a)+abs(y-b))
}

isInsideGame <- function(node, trafficMatrix) {
  dim <- NCOL(trafficMatrix$hroads)
  if ((node[[1]] > 0 &&
       node[[1]] <= dim) && (node[[2]] > 0 && node[[2]] <= dim)) {
    return (TRUE)
  }
  return (FALSE)
}

findNeighbor <-
  function(expandedFrontier,
           trafficMatrix,
           frontierList,
           offsetX,
           offsetY,
           edgeOffsetX,
           edgeOffsetY,
           dest
           ) {
    newX <- expandedFrontier[[1]] + offsetX
    newY <- expandedFrontier[[2]] + offsetY
    
    ## If x == 0 then we know that we look only up/down (a bit hacky)
    if (offsetX == 0) {
      newG <-
        expandedFrontier[[3]] + trafficMatrix$vroads[expandedFrontier[[1]], expandedFrontier[[2]] + edgeOffsetY]
    } else {
      newG <-
        expandedFrontier[[3]] + trafficMatrix$hroads[expandedFrontier[[1]] + edgeOffsetX, expandedFrontier[[2]]]
    }
    newH <- calcManhattanDist(newX, newY, dest[1], dest[2])
    newF <- newG + newH
    newpath <- append(expandedFrontier[[6]], list(c(newX, newY)))
    newNode <- list(x <- newX, #1
                    y <- newY, #2
                    g <- newG, #3
                    h <- newH, #4
                    f <- newF, #5
                    path <- newpath) #6
    return (newNode)
  }
