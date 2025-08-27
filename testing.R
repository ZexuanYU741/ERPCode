source( "MarkovChainMixtureEM.R" ) # EM algorithm stuff
traj.list <- grouped_classes
str(traj.list)
####################################################################
#	Given a trajectory - a sequence of states drawn from
# {1, .., n.states} - build a matrix C whose i,j entry counts
# the number of times that the trajectory includes a transition 
# from state i to state j.
####################################################################
test.traj <- traj.list[[1]]
test.traj1 <- traj.list[[500]]
count.transitions( traj.list[[1]] )
count.transitions( traj.list[[500]] )
count.transitions( traj.list[[1000]] )

####################################################################
#	Given a count matrix of the sort produced by count.transitions()
# and a matrix of transition probabilities, compute a log-likelihood.
# Test whether an impossible transition happens.
####################################################################
small.trans.mat <- matrix( rep( 0.5, 4), nrow=2 ) ;
test.loglike <- traj.loglike( count.transitions( test.traj, 2 ), small.trans.mat )
test.loglike == log(0.5) * (length(test.traj) - 1)

small.trans.mat <- matrix( c(1, 0, 0, 1), nrow=2 ) ;
traj.impossibleQ( count.transitions( test.traj, 2 ), small.trans.mat )

small.trans.mat1 <- matrix( c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3 ) ;
traj.impossibleQ( count.transitions( test.traj1, 3 ), small.trans.mat1 )

####################################################################
#	Given a matrix gamma whose i,j entry is the probability
# that the i-th subject belongs to the j-th class, compute a 
# vector pi whose j-th entry is the probability that a 
# randomly-selected subject belongs to class j.
####################################################################
traj_mat <- do.call(rbind, traj.list)  
dim(traj_mat)
traj_mat_normalized <- traj_mat / rowSums(traj_mat)
estimate.mixture.weights( traj_mat_normalized )

####################################################################
#	Given a matrix gamma as described above and a list
# of count matrices of the sort produced by count.transitions(),
# estimate the transition matrices for all the classes.
####################################################################
test.count.mat <- count.transitions( traj.list[[4050]], 7 )
test.count.mat
test.count.mats = list( test.count.mat, test.count.mat, t(test.count.mat))
estimated.trans.mats <- estimate.transition.matrices( test.gamma.mat, test.count.mats )
estimated.trans.mats

traj.list1 <- traj.list[1:290]
EM.result1 <- MarkovChainMixtureEM( 
  traj.list, 
  n.classes = 2, 
  n.states = 7,
  max.cycles = 500,
  tol = 1.0e-8
)
EM.result1

EM.result2 <- MarkovChainMixtureEM( 
  traj.list, 
  n.classes = 3, 
  n.states = 7,
  max.cycles = 500,
  tol = 1.0e-8
)
EM.result2

EM.result3 <- MarkovChainMixtureEM( 
  traj.list, 
  n.classes = 4, 
  n.states = 7,
  max.cycles = 500,
  tol = 1.0e-8
)
EM.result3

EM.result4 <- MarkovChainMixtureEM( 
  traj.list, 
  n.classes = 5, 
  n.states = 7,
  max.cycles = 500,
  tol = 1.0e-8
)
EM.result4

EM.result5 <- MarkovChainMixtureEM( 
  traj.list, 
  n.classes = 6, 
  n.states = 7,
  max.cycles = 500,
  tol = 1.0e-8
)
EM.result5

assignment1 <- apply( EM.result1$gamma.mat, MARGIN=1, FUN=which.max)
assignment2 <- apply( EM.result2$gamma.mat, MARGIN=1, FUN=which.max)
assignment3 <- apply( EM.result3$gamma.mat, MARGIN=1, FUN=which.max)
assignment4 <- apply( EM.result4$gamma.mat, MARGIN=1, FUN=which.max)
assignment5 <- apply( EM.result5$gamma.mat, MARGIN=1, FUN=which.max)

my.k <- EM.result3$n.classes
n.locations <- 254
traj.per.location <- 29
assignment.mat <- matrix( rep(0, n.locations*EM.result3$n.classes), nrow=n.locations )
for( location in 1:n.locations ) {
  min.idx <- 1 + traj.per.location*(location-1)
  max.idx <- traj.per.location*location
  assignment.mat[location,] <- tabulate( assignment3[min.idx:max.idx], nbins=my.k )
}
assignment.mat

grouped_assignment <- split(assignment3, 
                         ceiling(seq_along(assignment) / 29))
# Select the rows with 29
count_rows_with_29 <- sum(rowSums(assignment.mat == 29) > 0)
print(count_rows_with_29)
## Select the rows with 21
row_indices <- which(apply(assignment.mat, 1, function(row) any(row == 21)))
print(row_indices)
grouped_assignment[[18]]
grouped_assignment[[47]]
grouped_assignment[[117]]
grouped_assignment[[138]]
grouped_assignment[[149]]
grouped_assignment[[155]]
grouped_assignment[[161]]
grouped_assignment[[174]]
grouped_assignment[[182]]

