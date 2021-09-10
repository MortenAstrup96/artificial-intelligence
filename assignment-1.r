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
  
  a = nextCoordinates[1]
  b = nextCoordinates[2]
  
  x = carInfo$x
  y = carInfo$y
  

  if(x-a == 0 && y-b == -1) {return (8)} #up
  else if(x-a == -1 && y-b == 0) {return (6)} #right
  else if(x-a == 0 && y-b == 1) {return (2)} #down
  else if(x-a == 1 && y-b == 0) {return (4)} #left
  
  else if(x-a == 0 && y-b == 0) { #wait
    print("waiting")
    return (5)
    } # on dest: wait
  else {
    print("ERROR")
    print(x-a)
    print(y-b)
  }
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
    #print(length(frontierList))
    ## Find best score in frontiers
    scores <- sapply(frontierList, function(item)
      (item[[5]]))
    best_index <- which.min(scores)
    
    ## Pop best node from frontierlist
    expandedFrontier <- frontierList[[best_index]]
    frontierList <- frontierList[-best_index]
    
    #if found the dest, return first node in path
    if(expandedFrontier[[4]] == 0) {
      if(length(expandedFrontier[[6]]) > 2) return (expandedFrontier[[6]][[1]]) 
    }
    
    
    if(isInsideGame2(expandedFrontier,0,1,trafficMatrix)) {
      up <-
        findNeighbor(expandedFrontier, trafficMatrix, frontierList, 0, 1, 0, 0,  dest)  #up
        if(!is.null(up)) {
          frontierList <- append(frontierList, list(up))
        }
    }
    
    if(isInsideGame2(expandedFrontier,1,0,trafficMatrix)) {
      right <-
        findNeighbor(expandedFrontier, trafficMatrix, frontierList, 1, 0, 0, 0, dest)  #right
      if(!is.null(right)) {
      frontierList <- append(frontierList, list(right))
      }
      
    }
    if(isInsideGame2(expandedFrontier,0,-1,trafficMatrix)) {
      down <-
        findNeighbor(expandedFrontier, trafficMatrix, frontierList, 0,-1, 0,-1, dest) #down
      if(!is.null(down)) {
      frontierList <- append(frontierList, list(down))
      }
      
    }
    
    if(isInsideGame2(expandedFrontier,-1,0,trafficMatrix)) {
      left <-
        findNeighbor(expandedFrontier, trafficMatrix, frontierList,-1, 0,-1, 0, dest) #left
      if(!is.null(left)) {
      frontierList <- append(frontierList, list(left))
      }
    }


    #create frpmtoer --> add own costs to the  frontier cost
    # + also include path in array
    # 3. return cheap path (next step)
    
    
  }
  #print("outside loop")
  return (5)
}

isInsideGame2 <- function(node, offsetX, offsetY, trafficMatrix) {
  dim <- NCOL(trafficMatrix$hroads)
  if ((node[[1]]+offsetX > 0 &&
       node[[1]]+offsetX <= dim) && (node[[2]]+offsetY > 0 && node[[2]]+offsetY <= dim)) {
    return (TRUE)
  }
  return (FALSE)
}

calcManhattanDist <- function(a, b, x, y) {
  return(abs(x-a)+abs(y-b))
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
    
    for(item in frontierList) {
      if((newX == item[[1]]) && (newY == item[[2]])) {
        return (NULL)
      }
    }
    
    ## If x == 0 then we know that we look only up/down (a bit hacky)
    if (offsetX == 0) {
      newG <-
        expandedFrontier[[3]] + trafficMatrix$vroads[expandedFrontier[[1]], expandedFrontier[[2]] + edgeOffsetY]
    } else {
      newG <-
        expandedFrontier[[3]] + trafficMatrix$hroads[expandedFrontier[[1]] + edgeOffsetX, expandedFrontier[[2]]]
    }
    newH <- calcManhattanDist(newX, newY, dest[1], dest[2])
    newF <- newG + (2* newH)
    newpath <- append(expandedFrontier[[6]], list(c(newX, newY)))
    newNode <- list(x <- newX, #1
                    y <- newY, #2
                    g <- newG, #3
                    h <- newH, #4
                    f <- newF, #5
                    path <- newpath) #6
    return (newNode)
  }
