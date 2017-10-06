######################################################
#DISCUSSION
######################################################
#My variation of the Schelling model adds property cost
#and income inequality to the mix. The model assumes that
#property in the center of the city is most expensive, and 
#that cost decreases as you move outwards. It introduces 
#several new parameters, but most of these exist to ensure 
#there will be equilibrium, and have been tuned accordingly.

#For the most part, the innequality.ratio and the min.similarity
#are the parameters worth tweaking.

#What is most interesting about this model is that even a slight
#income advantage given to one color causes it to dominate the center
#assuming a min.similarity of 3/8. This model can help explain the way
#that income interacts with bais to enforce segregation and supress
#social mobility. 


######################################################
#PARAMETERS
######################################################
rows <- 50 #number of rows in matrix
cols <- 50 #number of columns in matrix

#a distribution that determines how sharply property prices
#decay as you expand outward from the center of the map
property.dist <- function(x) {
  return(x^(0.6))
}

proportion.group.1 <- .5 # proportion of red agents
empty <- .2 # proportion of grid that will be empty space
min.similarity <- 3/8 # minimum proportion of neighbors that are the same type to not move


inequality.ratio <- c(3,2) #a simple metric to set the income ineqality.
#Keep the first digit higher than the second to give red a higher income

#income distribution of the red population
distgrp1 <- quote(rbeta(1, inequality.ratio[1], inequality.ratio[2]))
#income distribution of the blue population
distgrp2 <- quote(rbeta(1, inequality.ratio[2], inequality.ratio[1])) 

overprice.tolerance <- 0.2 #number between 0 and 1, determines how far above someone's means
# they are willing to live
underprice.tolerance <- 0.5 #number between 0 and 1, determines how far below someone's means
# they are willing to live

######################################################
#FUNCTIONS
######################################################

#_________create.grid_________###
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

#_________visualize.grid_________####
# outputs a visualization of the grid, with red squares representing group 1,
# blue squares group 2, and black squares empty locations.
visualize.grid <- function(grid){
  image(grid, col=c('black','red','blue'), xaxs=NULL, yaxs=NULL, xaxt='n', yaxt='n')
}

#_________empty.locations_________####
# returns all the locations in the grid that are empty
# output is an N x 2 array, with N equal to the number of empty locations
# the 2 columns contain the row and column of the empty location.
empty.locations <- function(grid){
  return(which(grid==0, arr.ind=T))
}

#_________similarity.to.center_________####
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

#_________segregation_________####
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


#_________match.means_________####
#This is a fucntion that determines whether or not an individual
#will want to move because they are living either too far above
#or too far below their means
match.means <- function(row, col) {
  return((property.grid[row,col] - overprice.tolerance < wealth.grid[row,col])&(wealth.grid[row,col] < property.grid[row,col] + underprice.tolerance))
}

#_________unhappy.agents_________####
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
        #also call the match.means function to see if they need to move
        grid.copy[row,col] <- similarity.score >= min.similarity & match.means(row,col)
      }
    }
  }
  return(which(grid.copy==FALSE, arr.ind = T))
}

#_________Assign property cost_________###
#This function creates the spread of property 
#values assuming the property in the center is more 
#expensive than the property on the outskirts
assign.property.cost <- function(rows, cols, current.index, dist) {
  #calculate the center
  center <- c((rows%/%2),(cols%/%2))
  
  rowDif <- abs((center[1] - current.index[1]))
  colDif <- abs((center[2] - current.index[2]))
  unscalledDif <- rowDif + colDif
  scalledDif <- 1- (dist(unscalledDif)/(dist(cols)))
  #return function of the total difference
  return(scalledDif)
}
  
#_________Create property matrix_________###
#assigns property cost to every square and stores 
#it in a matrix
create.property.matrix <- function(rows, cols) {
  #initialize the property grid
  property.grid <<- matrix(nrow=rows, ncol=cols)
  #mutate it
  for(i in 1:rows) {
    for(j in 1:cols) {
      property.grid[i,j] <<- assign.property.cost(rows, cols, c(i,j), property.dist)
    }
  }
}



#Create a function to assign the disposable income based 
#on the income distribution of the group
assign.wealth <- function(distgrp1, distgrp2, type) {
  if(type==0){return(0)} 
  if(type==1){return(distgrp1)}
  if(type==2){return(distgrp2)}
}


#Create income matrix
create.income.matrix <- function(distgrp1, distgrp2, matrix) {
  #initialize the wealth grid
  wealth.grid <<- matrix
  for(i in 1:rows) {
    for(j in 1:cols) {
    wealth.grid[i,j] <<- assign.wealth(eval(distgrp1), eval(distgrp2), matrix[i,j])
    }
  }
}

#_________one.round_________####
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
    #make sure the wealth grid stays synced with the grid (sloppy code...mutate the global envrt)
    #would have done differenctly if I was starting this now
    wealth.grid[empty.spaces[i, 1], empty.spaces[i,2]] <<- wealth.grid[unhappy[i,1], unhappy[i,2]]
    #
    grid[unhappy[i,1], unhappy[i,2]] <- 0
    #keep wealth grid synced
    wealth.grid[unhappy[i,1], unhappy[i,2]] <<- 0
  }
  return(grid)
}

######################################################
#RUNNING THE SIMULATION
######################################################

done <- FALSE # a variable to keep track of whether the simulation is complete
grid <- create.grid(rows, cols, proportion.group.1, empty)
seg.tracker <- c(segregation(grid)) # keeping a running tally of the segregation scores for each round
create.property.matrix(rows, cols)
create.income.matrix(distgrp1, distgrp2, grid)


while(!done){
  new.grid <- one.round(grid, min.similarity) # run one round of the simulation, and store output in new.grid
  seg.tracker <- c(seg.tracker, segregation(grid)) # calculate segregation score and add to running list
  if(all(new.grid == grid)){ # check if the new.grid is identical to the last grid
    done <- TRUE # if it is, simulation is over -- no agents want to move
  } else {
    grid <- new.grid # otherwise, replace grid with new.grid, and loop again.
  }
}
layout(1:2) # change graphics device to have two plots
visualize.grid(grid) # show resulting grid
plot(seg.tracker) # plot segregation over time


