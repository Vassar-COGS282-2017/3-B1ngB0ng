# model parameters ####
rows <- 50 
cols <- 50
dist <- function(x) {
  return(x)
}
proportion.group.1 <- .5 # proportion of red agents
empty <- .2 # proportion of grid that will be empty space
min.similarity <- 5/8 # minimum proportion of neighbors that are the same type to not move

# create.grid ####
# generates a rows x column matrix and randomly places the initial population
# values in the matrix are either 0, 1, or 2
# if 0, the space is empty
# 1 and 2 represent the two different groups
create.grid <- function(rows, cols, proportion.group.1, empty){
  pop.size.group.1 <- (rows*cols)*(1-empty)*proportion.group.1
  pop.size.group.2 <- (rows*cols)*(1-empty)*(1-proportion.group.1)
  
  initial.population <- sample(c(
    rep(1, pop.size.group.1), 
    rep(2, pop.size.group.2), 
    rep(0, (rows*cols)-pop.size.group.1-pop.size.group.2)
  ))
  grid <- matrix(initial.population, nrow=rows, ncol=cols)
}

# visualize.grid ####
# outputs a visualization of the grid, with red squares representing group 1,
# blue squares group 2, and black squares empty locations.
visualize.grid <- function(grid){
  image(grid, col=c('black','red','blue'), xaxs=NULL, yaxs=NULL, xaxt='n', yaxt='n')
}

# empty.locations ####
# returns all the locations in the grid that are empty
# output is an N x 2 array, with N equal to the number of empty locations
# the 2 columns contain the row and column of the empty location.
empty.locations <- function(grid){
  return(which(grid==0, arr.ind=T))
}

# similarity.to.center ####
# takes a grid and the center.val of that grid and returns
# the proportion of cells that are the same as the center,
# ignoring empty cells. the center.val must be specified
# manually in case the grid has an even number of rows or 
# columns
similarity.to.center <- function(grid.subset, center.val){
  if(center.val == 0){ return(NA) }
  same <- sum(grid.subset==center.val) - 1
  not.same <- sum(grid.subset!=center.val) - sum(grid.subset==0)
  return(same/(same+not.same))
}

# segregation ####
# computes the proportion of neighbors who are from the same group
segregation <- function(grid){
  same.count <- 0
  diff.count <- 0
  for(row in 1:(nrow(grid)-1)){
    for(col in 1:(ncol(grid)-1)){
      if(grid[row,col] != 0 && grid[row+1,col+1] != 0){
        if(grid[row,col] != grid[row+1,col+1]){
          diff.count <- diff.count + 1
        } else {
          same.count <- same.count + 1
        }
      }
    }
  }
  return(same.count / (same.count + diff.count))
}

# unhappy.agents ####
# takes a grid and a minimum similarity threshold and computes
# a list of all of the agents that are unhappy with their 
# current location. the output is N x 2, with N equal to the
# number of unhappy agents and the columns representing the 
# location (row, col) of the unhappy agent in the grid
unhappy.agents <- function(grid, min.similarity){
  grid.copy <- grid
  for(row in 1:rows){
    for(col in 1:cols){
      similarity.score <- similarity.to.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      if(is.na(similarity.score)){
        grid.copy[row,col] <- NA
      } else {
        grid.copy[row,col] <- similarity.score >= min.similarity
      }
    }
  }
  return(which(grid.copy==FALSE, arr.ind = T))
}

# Assign property cost
property.cost <- function(center, current.index, dist) {
  #calculate the center
  rowDif <- abs((center[1] - current.index[1]))
  colDif <- abs((center[2] - current.index[2]))
  totalDif <- rowDif + colDif
  #return function of the total difference
  return(dist(totalDif))
}
  
#Create property matrix
create.property.matrix <- function(rows, cols) {
  #initialize the property grid
  property.grid <<- matrix(nrow=rows, ncol=cols)
  #calculate the center
  center <- c((rows%/%2),(cols%/%2))
  for(i in 1:rows) {
    for(j in 1:cols) {
      property.grid[i,j] <<- property.cost(center, c(i,j), dist)
    }
  }
}



# one.round ####
# runs a single round of the simulation. the round starts by finding
# all of the unhappy agents and empty spaces. then unhappy agents are randomly
# assigned to a new empty location. a new grid is generated to reflect all of
# the moves that took place.
one.round <- function(grid, min.similarity){
  #find all the empty spaces
  empty.spaces <- empty.locations(grid)
  #find all the unhappy agents
  unhappy <- unhappy.agents(grid, min.similarity)
  #shuffle empty spaces to create random assignement
  empty.spaces <- empty.spaces[sample(1:nrow(empty.spaces)), ]
  #go through the empty spaces list and assign an agent to each
  for(i in 1:nrow(empty.spaces)) {
    #if we run out of unhappy dudes we wanna end the for loop then
    if(i > nrow(unhappy)){
      break;
    }
    #make the switch by copying an unhappy index from grid to the corresponding
    #index in empty spaces. 
    grid[empty.spaces[i, 1], empty.spaces[i,2]] <- grid[unhappy[i,1], unhappy[i,2]]
    #
    grid[unhappy[i,1], unhappy[i,2]] <- 0
  }
  return(grid)
}

# running the simulation ####
done <- FALSE # a variable to keep track of whether the simulation is complete
grid <- create.grid(rows, cols, proportion.group.1, empty)
seg.tracker <- c(segregation(grid)) # keeping a running tally of the segregation scores for each round
while(!done){
  new.grid <- one.round(grid, min.similarity) # run one round of the simulation, and store output in new.grid
  seg.tracker <- c(seg.tracker, segregation(grid)) # calculate segregation score and add to running list
  if(all(new.grid == grid)){ # check if the new.grid is identical to the last grid
    done <- TRUE # if it is, simulation is over -- no agents want to move
  } else {
    grid <- new.grid # otherwise, replace grid with new.grid, and loop again.
  }
}
layout(1) # change graphics device to have two plots
visualize.grid(grid) # show resulting grid
plot(seg.tracker) # plot segregation over time
