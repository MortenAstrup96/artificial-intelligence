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
waitCounter <- 0

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
     distanceVector <- c()
     i <- 1
     while (i <= length(packageMatrix[, 1])) {
          #firstpack
          node <-
               calcAstar(
                    carInfo = carInfo,
                    dest = c(packageMatrix[i, 1], packageMatrix[i, 2]),
                    trafficMatrix = trafficMatrix
               )
          distanceVector <- append(distanceVector, node[[3]])
          i <- i + 1
     }
     distanceVector[packageMatrix[, 5] != 0] = Inf
     return(packageMatrix[which.min(distanceVector), c(1, 2)])
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
     destination <- c(carInfo$mem$goal[1], carInfo$mem$goal[2])
     goalNode <- calcAstar(carInfo, destination, trafficMatrix)
     
     #check if the path is empty:
     if (length(goalNode[[6]]) < 1) {
          a = carInfo$x
          b = carInfo$y
     } # Coordinates of current car
     else{
          nextCoordinates <- goalNode[[6]][[1]]
          a = nextCoordinates[1]
          b = nextCoordinates[2]
     }
     
     x = carInfo$x
     y = carInfo$y
     
     
     if (x - a == 0 && y - b == -1) {
          return (8)
     } #up
     else if (x - a == -1 && y - b == 0) {
          return (6)
     } #right
     else if (x - a == 0 && y - b == 1) {
          return (2)
     } #down
     else if (x - a == 1 && y - b == 0) {
          return (4)
     } #left
     
     else if (x - a == 0 && y - b == 0) {
          # print("wait")
          return (5)
     }
     
}

## Returns the goalNode
calcAstar <- function(carInfo, dest, trafficMatrix) {
     # 1. fronttier queue
     frontierList <- list()
     
     manhattan <-
          calcManhattanDist(carInfo$x, carInfo$y, dest[1], dest[2])
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
     
     frontierList <- append(frontierList, list(firstNode))
     
     ## --- Loop here ---
     while (length(frontierList) != 0) {
          ## Find best score in frontiers
          scores <- sapply(frontierList, function(item)
               (item[[5]]))
          best_index <- which.min(scores)
          
          ## Finding nodes that are equal in travel cost bust cheaper in manhattan cost
          potentialExpantionNode <- frontierList[[best_index]]
          
          i = 1
          while (i < length(frontierList)) {
               # str(potentialExpantionNode)
               if (frontierList[[i]][[5]] == potentialExpantionNode[[5]] &&
                   frontierList[[i]][[3]] < potentialExpantionNode[[3]]) {
                    potentialExpantionNode = frontierList[[i]]
                    best_index = i
               }
               i = i + 1
          }
          
          ## Pop best node from frontierlist
          expandedFrontier <- potentialExpantionNode
          frontierList <- frontierList[-best_index]
          
          #if found the dest, return first node in path
          # str(potentialExpantionNode)
          if (expandedFrontier[[4]] == 0) {
               return (expandedFrontier)
          }
          
          
          if (isInsideGame2(expandedFrontier, 0, 1, trafficMatrix)) {
               up <-
                    findNeighbor(expandedFrontier,
                                 trafficMatrix,
                                 frontierList,
                                 0,
                                 1,
                                 0,
                                 0,
                                 dest)  #up
               if (!is.null(up)) {
                    frontierList <- append(frontierList, list(up))
               }
          }
          
          if (isInsideGame2(expandedFrontier, 1, 0, trafficMatrix)) {
               right <-
                    findNeighbor(expandedFrontier,
                                 trafficMatrix,
                                 frontierList,
                                 1,
                                 0,
                                 0,
                                 0,
                                 dest)  #right
               if (!is.null(right)) {
                    frontierList <- append(frontierList, list(right))
               }
               
          }
          if (isInsideGame2(expandedFrontier, 0,-1, trafficMatrix)) {
               down <-
                    findNeighbor(expandedFrontier,
                                 trafficMatrix,
                                 frontierList,
                                 0,-1,
                                 0,-1,
                                 dest) #down
               if (!is.null(down)) {
                    frontierList <- append(frontierList, list(down))
               }
          }
          
          if (isInsideGame2(expandedFrontier,-1, 0, trafficMatrix)) {
               left <-
                    findNeighbor(expandedFrontier,
                                 trafficMatrix,
                                 frontierList,-1,
                                 0,-1,
                                 0,
                                 dest) #left
               if (!is.null(left)) {
                    frontierList <- append(frontierList, list(left))
               }
          }
          
     }
     #print("outside loop")
     return (5)
}

isInsideGame2 <- function(node, offsetX, offsetY, trafficMatrix) {
     dim <- NCOL(trafficMatrix$hroads)
     if ((node[[1]] + offsetX > 0 &&
          node[[1]] + offsetX <= dim) &&
         (node[[2]] + offsetY > 0 && node[[2]] + offsetY <= dim)) {
          return (TRUE)
     }
     return (FALSE)
}

## To optimise: Create a table with all manhattan distances at start of program
calcManhattanDist <- function(a, b, x, y) {
     return(abs(x - a) + abs(y - b))
}


findNeighbor <-
     function(expandedFrontier,
              trafficMatrix,
              frontierList,
              offsetX,
              offsetY,
              edgeOffsetX,
              edgeOffsetY,
              dest) {
          newX <- expandedFrontier[[1]] + offsetX
          newY <- expandedFrontier[[2]] + offsetY
          
          ## Check if this neighbor is already in frontierList - Return null if it is
          for (item in frontierList) {
               if ((newX == item[[1]]) && (newY == item[[2]])) {
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
          newF <- newG + newH
          newpath <- append(expandedFrontier[[6]], list(c(newX, newY)))
          newNode <- list(x <- newX, y <-newY, g <- newG, h <-newH, f <- newF, path <- newpath)
          return (newNode)
     }
