###########################################################################
# Plot a heatmap representing the entries in a matrix
#
# mrm: Man Uni, 29 July 2025
# zy: Man Uni, 27 August 2025
###########################################################################
rm( list=ls() ) # Wipe the slate clean

library( gtools )		# fro rdirichlet()
library( plot.matrix )	# To dow he plotting
library( RColorBrewer ) # Various useful palettes: see https://colorbrewer2.org/

##########################################################################
#   Generate a random transition matrix 
##########################################################################

mat.dim <- 8
trans.mat <- rdirichlet( mat.dim, rep(0.5, mat.dim))

##########################################################################
#   Plot a heatmap of the matrix entries
##########################################################################

# Set up a continuous palette based on the Brewer palettes
spectral.pal <- brewer.pal( 11, "Spectral" )
my.color.func <- colorRampPalette( spectral.pal )

data1 <- matrix(c(
  6.722260e-01, 0.2214451799, 7.261291e-02, 0.033452547, 2.633438e-04, 0.000000000, 0.000000e+00,
  4.986485e-01, 0.2861640681, 1.242944e-01, 0.089339187, 1.553872e-03, 0.000000000, 0.000000e+00,
  3.601966e-01, 0.2985754966, 1.703224e-01, 0.162520296, 8.385260e-03, 0.000000000, 0.000000e+00,
  1.443796e-01, 0.1769210685, 1.557666e-01, 0.369326939, 1.505686e-01, 0.003037164, 0.000000e+00,
  5.507037e-04, 0.0024203992, 5.025217e-03, 0.080334802, 6.709003e-01, 0.240684228, 8.438564e-05,
  9.744888e-05, 0.0001948978, 2.157531e-121, 0.002462046, 2.777186e-01, 0.703528645, 1.599839e-02,
  0.000000e+00, 0.0000000000, 0.000000e+00, 0.000000000, 1.743178e-187, 0.058934097, 9.410659e-01
), nrow = 7, byrow = TRUE)


data2 <- matrix(c(
  0.581554748, 0.224616438, 0.0982703119, 0.0913745868, 0.004183915, 0.0000000000, 0.0000000000,
  0.314738584, 0.259752446, 0.1689653418, 0.2402197796, 0.016207436, 0.0001164128, 0.0000000000,
  0.163695691, 0.211838202, 0.1935434112, 0.3871808882, 0.043597177, 0.0001446307, 0.0000000000,
  0.047592732, 0.093054915, 0.1188816179, 0.5287583466, 0.209546393, 0.0021659956, 0.0000000000,
  0.002527203, 0.006736926, 0.0148593431, 0.2352917216, 0.627720548, 0.1121128967, 0.0007513608,
  0.000000000, 0.000000000, 0.0001660009, 0.0022302563, 0.089843024, 0.8072207249, 0.1005399939,
  0.000000000, 0.000000000, 0.0000000000, 0.0001158838, 0.001854137, 0.2788970073, 0.7191329720
), nrow = 7, byrow = TRUE)


data3 <- matrix(c(
  8.923664e-01, 0.09407394, 0.01176674, 0.001792961, 2.202882e-150, 0.0000000, 0.00000000,
  7.963401e-01, 0.16807681, 0.02891066, 0.006672412, 0.000000e+00, 0.0000000, 0.00000000,
  7.133520e-01, 0.21470016, 0.05679783, 0.015150038, 3.799094e-16, 0.0000000, 0.00000000,
  5.460765e-01, 0.25109798, 0.10896441, 0.075469967, 1.839112e-02, 0.0000000, 0.00000000,
  2.411836e-68, 0.07849075, 0.07161686, 0.110862363, 3.889130e-01, 0.3209066, 0.02921045,
  0.000000e+00, 0.00000000, 0.00000000, 0.005039127, 4.287694e-02, 0.7144456, 0.23763837,
  0.000000e+00, 0.00000000, 0.00000000, 0.000000000, 1.269118e-02, 0.5975378, 0.38977098
), nrow = 7, byrow = TRUE)


data4 <- matrix(c(
  0.983089233, 1.615600e-02, 0.000750133, 4.635555e-06, 0.00000000, 0.000000000, 0.000000e+00,
  0.943777580, 5.494335e-02, 0.001279073, 1.226250e-185, 0.00000000, 0.000000000, 0.000000e+00,
  0.854549224, 5.753644e-02, 0.016099831, 5.277445e-02, 0.01904006, 0.000000000, 0.000000e+00,
  0.006424526, 2.619277e-221, 0.064546651, 5.742633e-01, 0.34788945, 0.006876106, 0.000000e+00,
  0.000000000, 1.473814e-289, 0.003053143, 1.463725e-01, 0.53154412, 0.319030285, 2.726293e-84,
  0.000000000, 0.000000e+00, 0.000000000, 6.756188e-04, 0.15339427, 0.804454286, 4.147582e-02,
  0.000000000, 0.000000e+00, 0.000000000, 0.000000e+00, 0.00000000, 0.292964558, 7.070354e-01
), nrow = 7, byrow = TRUE)

orig.margins <- par(mar=c(3.1, 4.3, 2.1, 4.9))
plot( 
  data1,
  col = my.color.func,
  breaks = c(0, 1),
  fmt.key = "%.2f",
  axis.row = list(side=2, line=0.39),
  axis.col = list(side=1, line=-4.45),
  asp = TRUE,
  main = "",
  xlab = "",
  ylab = ""
)
title(main="Random Transition Matrix", line=-2.75, adj=0.375)
title(xlab="To state", line=-2.0, adj=0.4175)
title(ylab="From state")

pdf( "D:/DS/ERP/Heatmap/1-1.pdf", width=9.0, height=9.0  )
orig.margins <- par(mar=c( 3.1, 4.3, 2.1, 4.9 )) # modify margins
plot( 
	data1,
	col = my.color.func,
	breaks = c(0, 1), # Fix the range of the scale
	fmt.key = "%.2f",
	axis.row = list(side=2, line=0.39 ),
	axis.col = list(side=1, line=-4.45 ),
	asp = TRUE, # force the cells to be square
	# Suppress automatic titles
	main = "",
	xlab = "",
	ylab = ""
)

# Draw titles in such a way as to control their position
# Changing line moves them up and down, changing adj moves the left and right
title( main="Transition Matrix Heatmap for Class 1", line=-2.75, adj=0.375)
title( xlab="To state", line=-2.0, adj=0.4175 )
title( ylab="From state" )

dev.off()

pdf( "D:/DS/ERP/Heatmap/2.pdf", width=9.0, height=9.0  )
orig.margins <- par(mar=c( 3.1, 4.3, 2.1, 4.9 )) # modify margins
plot( 
  data2,
  col = my.color.func,
  breaks = c(0, 1), # Fix the range of the scale
  fmt.key = "%.2f",
  axis.row = list(side=2, line=0.39 ),
  axis.col = list(side=1, line=-4.45 ),
  asp = TRUE, # force the cells to be square
  # Suppress automatic titles
  main = "",
  xlab = "",
  ylab = ""
)

# Draw titles in such a way as to control their position
# Changing line moves them up and down, changing adj moves the left and right
title( main="Transition Matrix Heatmap for Class 2", line=-2.75, adj=0.375)
title( xlab="To state", line=-2.0, adj=0.4175 )
title( ylab="From state" )

dev.off()

pdf( "D:/DS/ERP/Heatmap/3.pdf", width=9.0, height=9.0  )
orig.margins <- par(mar=c( 3.1, 4.3, 2.1, 4.9 )) # modify margins
plot( 
  data3,
  col = my.color.func,
  breaks = c(0, 1), # Fix the range of the scale
  fmt.key = "%.2f",
  axis.row = list(side=2, line=0.39 ),
  axis.col = list(side=1, line=-4.45 ),
  asp = TRUE, # force the cells to be square
  # Suppress automatic titles
  main = "",
  xlab = "",
  ylab = ""
)

# Draw titles in such a way as to control their position
# Changing line moves them up and down, changing adj moves the left and right
title( main="Transition Matrix Heatmap for Class 3", line=-2.75, adj=0.375)
title( xlab="To state", line=-2.0, adj=0.4175 )
title( ylab="From state" )

dev.off()

pdf( "D:/DS/ERP/Heatmap/4.pdf", width=9.0, height=9.0  )
orig.margins <- par(mar=c( 3.1, 4.3, 2.1, 4.9 )) # modify margins
plot( 
  data4,
  col = my.color.func,
  breaks = c(0, 1), # Fix the range of the scale
  fmt.key = "%.2f",
  axis.row = list(side=2, line=0.39 ),
  axis.col = list(side=1, line=-4.45 ),
  asp = TRUE, # force the cells to be square
  # Suppress automatic titles
  main = "",
  xlab = "",
  ylab = ""
)

