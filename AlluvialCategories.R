 ################################################################
#	Alluvial diagrams with plotly in R
# There are *lots* of R packages that draw alluvial diagrams,
# but here I've used plotly 
#
#	https://plotly.com/r/sankey-diagram/
#
# to allow for comparison with the approach in Alluvial.ipynb
#
# mrm: 14 August 2025 
# zy: 27 August 2025 
################################################################

library(plotly)
library(RColorBrewer) # pleasing colours

# Ensure that there is a Figures directory
fig.subdir <- "Figures"
if( !file.exists( fig.subdir ) ) {
	dir.create( fig.subdir )
}

# Make a note of the numbers of clusters for which we have data
n.classes <- 2:6
n.clusterings <- length( n.classes )

# Read the data and arrange it in the form we need
orig.data.df <- read.csv( "D:/DS/ERP/result.csv", header=TRUE )
n.fares <- nrow( orig.data.df ) 
for( j in 1:ncol(orig.data.df) ) {
	tmp.df <- data.frame(
		N = rep(n.classes[j], n.fares),
		class = orig.data.df[,j]
	)
	
	if( j==1 ) {
		cluster.df <- tmp.df
	} else {
		cluster.df <- rbind( cluster.df, tmp.df )
	}
}

# Prepare colours 
cluster.pal <- brewer.pal( 6, "Set1" )

# A function to describe semi-transparent colours 
# in a form that plotly recognises
semi.transparent.alpha <- 0.6
plotly.semi.transparent <- function( r.color, alpha )
{
	rgb.vals <- col2rgb( r.color )
	rgb.str <- paste( rgb.vals, collapse="," )
	plotly.color.str <- paste( 
		"rgba(", rgb.str, ",", alpha, ")", sep=""
	)
	
	return( plotly.color.str )
}

############################################################
#   Assemble lists of node and link properties for a
# Sankey plot that includes all 5 clusterings.
#
# N.B. Plotly numbers nodes and links from 0, while R 
# starts arrays at 1. Thus in some of the code below 
# we have both node.r and node.plotly = node.r - 1.
############################################################

# Nodes are labelled by cluster numbers.
node.label <- c()
node.color <- c()
for( j in 1:n.clusterings ) {
	crnt.n.classes <- n.classes[j]
	node.label <- c( node.label, 1:crnt.n.classes )
	node.color <- c( node.color, cluster.pal[1:crnt.n.classes] )
}
n.nodes <- length( node.label )

# Compute offsets in the node list
node.offset <- rep( 0, n.clusterings)
for( j in 2:n.clusterings ) {
	prev.n.classes <- n.classes[j-1]
	node.offset[j] <- node.offset[j-1] + prev.n.classes
}

# Assemble the attributes of the list of links
n.links <- 0
for( j in 1:(n.clusterings-1) ) {
	crnt.n.classes <- n.classes[j]
	n.links <- n.links * (n.links + 1)
}

link.from.node <- rep( 0, n.links )
link.to.node <- rep( 0, n.links )
link.value <- rep( 0, n.links )
link.color <- rep( "", n.links )

link.num = 1
for( j in 1:(n.clusterings-1) ) {
	# Links run from one clustering to the next
	from.N <- n.classes[j]
	to.N <- from.N + 1
	for( from.class in 1:from.N ) {
		from.node.r <- node.offset[j] + from.class
		from.node.ploty <- from.node.r - 1
		for( to.class in 1:to.N ) {
			to.node.r <- node.offset[j+1] + to.class
			to.node.plotly <- to.node.r - 1

			# Count the number of trajectories that are in from.class
			# in the j-th clustering and to.class in the (j+1)th.
			from.df <- subset( cluster.df, N==from.N )
			to.df <- subset( cluster.df, N==to.N )
			should.count <- (from.df$class == from.class) & (to.df$class == to.class)
			link.value[link.num] <- as.integer(sum(ifelse(should.count, 1, 0)))

			# Set remaining attributes of the link
			link.from.node[link.num] <- from.node.ploty
			link.to.node[link.num] <- to.node.plotly
			link.color[link.num] <- plotly.semi.transparent( 
				node.color[from.node.r], semi.transparent.alpha 
			)
			
			link.num <- link.num + 1
		}
	}
}

############################################################
# Find the positions of the midpoints of the nodes in
# a coordinate system where both x and y run 0 to 1.
############################################################

# Set various sizes in pixels
sankey.width <- 1000
sankey.height <- 600
node.width <- 20
node.padding <- 3

# Get fractions of the vertical column devoted to padding and
# the total length of the bars
node.padding.frac = node.padding / sankey.height
total.vbar.frac = (sankey.height - n.classes*node.padding) / sankey.height

# Get fractions of the horizontal extent occupied by bars.
node.width.frac = node.width / sankey.width
total.hbar.frac = (sankey.width - node.width) / sankey.width

# Build a matrix called class.counts such that 
# class.counts[c, j] is the number of trajectories that
#  were assigned to class c in the clustering with n.class[j] classes.
class.counts = matrix( rep(0, 5*6), nrow=6 )
for( j in 1:n.clusterings ) {
	crnt.N <- n.classes[j]
	for( c in 1:n.classes[j] ) {
		tmp.df <- subset( cluster.df, N==crnt.N & class==c )
		class.counts[c,j] <- nrow( tmp.df )
	}
}

# As a sanity check, print the matrix, then check that the 
# columns sum to the number of users
class.counts
all( colSums( class.counts ) == n.fares )

# Finally, work out the positions
class.bar.frac = (class.counts / n.fares) * total.vbar.frac 

node.x = rep( 0, n.nodes )
node.y = rep( 0, n.nodes )

node.num = 1
for( j in 1:n.clusterings ) {
	crnt.x <- (node.width.frac / 2) + ((j - 1) / (n.clusterings - 1)) * total.hbar.frac
	crnt.y <- 0.0
	for( c in 1:n.classes[j] ) {
		crnt.y <- crnt.y + class.bar.frac[c, j] / 2
		
		# Record the results
		node.x[node.num] <- crnt.x
		node.y[node.num] <- crnt.y
		node.num <- node.num + 1

		# Account for the vertical padding between bars
		crnt.y <- crnt.y + class.bar.frac[c, j] / 2
		crnt.y <- crnt.y + node.padding.frac
	}
}
   
############################################################ 
# Now, draw the thing. 
# See https://plotly.com/r/sankey-diagram/
############################################################

fig <- plot_ly(
	type = "sankey",
	width = sankey.width,
	height = sankey.height,

	# Node properties
	node = list(
		pad = node.padding,
		thickness = node.width,
		line = list( color = "black", width = 0.5 ),
		label = node.label,
		color = node.color,
 		x = node.x,
 		y = node.y
	),

	# Link properties
	link = list(
		source = link.from.node,
		target = link.to.node,
		value = link.value,
		color = link.color
	)
) 

# Add a title
fig <- layout( fig,
	title = list(
		automargin = TRUE,
		text = paste( "Class assignments:", n.fares, "taxi fares" ),
		font = list( size = 16 )
	),
	
	# Make background transparent
	paper_bgcolor = 'rgba(0,0,0,0)',
	plot_bgcolor = 'rgba(0,0,0,0)'
)

# Add day numbers beneath the columns of nodes.
# The code here follows an example at
# https://plotly.com/r/text-and-annotations/
for( j in 1:n.clusterings ){
	# Assemble a list describing one of the bits of text
	# we want to add.
	my.annotation <- list(
		text = paste( n.classes[j], "Classes" ),
		font = list(size=14, color="black"),
		x = ((j - 1) / (n.clusterings - 1)) * total.hbar.frac + (node.width.frac / 2),
		y = -0.05,
		xanchor = 'center',
		showarrow = FALSE,
		xref = "paper",
		yref = "paper"
    )
    
    #   Add it to the figure
    fig <- layout( fig, annotations = my.annotation )
}

# Display the result
fig


