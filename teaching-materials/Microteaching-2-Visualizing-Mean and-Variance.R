########### Microteaching Session 2 - MATH 900 ###########
###########            Ziye Lin                ###########
########### Topic: Expected Value and Variance ###########
##############################################################################
##############################################################################

### Here, we will be visualizing the expected value and variance 
### using bivariate normal distributions 
library(MASS)
library(circlize)

##############################################################################
##############################################################################

### Create a dartboard
dartboard <- function(title=NA){
  ### Draw a dartboard
  
  par(mar = c(3, 3, 3, 3))  # Adjust margins to fit both plots
  plot(1, type = "n", xlim = c(-10, 10), ylim = c(-10, 10), 
       xlab = "", ylab = "", axes = F, main=title)
  
  ### Draw a dart board
  
  fcts <- 1:20
  
  # Graphical parameters
  circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0),
             start.degree = 360/40, track.margin = c(0, 0), "clock.wise" = FALSE)
  
  # Create the circle
  circos.initialize(factors = fcts, xlim = c(0, 10))
  circos.trackPlotRegion(ylim = c(0, 10), factors = fcts, bg.col = "black",
                         track.height = 0.15)
  
  # Adding the numbers
  circos.trackText(rep(5, 20), rep(5, 20),  rep(5, 20),
                   labels = c(13, 4, 18, 1, 20, 5, 12, 9, 14, 11, 8, 16, 7, 19, 3, 17, 2, 15, 10, 6),
                   factors = fcts, col = "#EEEEEE", font = 2,
                   facing = "downward")
  
  # Double point
  circos.trackPlotRegion(ylim = c(0, 5), factors = fcts,
                         bg.col = rep(c("#df2623", "#11a551"), 10), bg.border = "#EEEEEE", 
                         track.height = 0.05)
  
  # Region between double and triple point
  circos.trackPlotRegion(ylim = c(0, 5), factors = fcts,
                         bg.col = rep(c("black", "#e6cda5"), 10), bg.border = "#EEEEEE", 
                         track.height = 0.275)
  
  # Triple point
  circos.trackPlotRegion(ylim = c(0, 5), factors = fcts,
                         bg.col = rep(c("#df2623", "#11a551"), 10), bg.border = "#EEEEEE",
                         track.height = 0.05)
  
  # Region between triple point and bullseye
  circos.trackPlotRegion(ylim = c(0, 5), factors = fcts, 
                         bg.col = rep(c("black", "#e6cda5"), 10), bg.border = "#EEEEEE",
                         track.height = 0.375)
  
  # 25 points
  draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360,
              rou1 = 0.1, col = "#11a551", border = "#EEEEEE")
  
  # 50 points
  draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360,
              rou1 = 0.05, col = "#df2623", border = "#EEEEEE")
  
  title(title)
  circos.clear()
  
  ### Use dartboard as the background
  par(new = TRUE)  # Allows overlaying another plot
}

### Generate points from bivariate normal distribution and plot them
set.seed(900)  # for reproducibility
n <- c(50, 100, 200, 500)
rho_set <- c(0.001, 0.01, 0.1, sqrt(2))
mu1 <- as.vector(c(0,0))
mu2 <- as.vector(c(0.5,0.5))
mu3 <- as.vector(c(-0.5,0.3))
mu4 <- as.vector(c(-0.4,-0.6))
mu5 <- as.vector(c(0.1,-0.7))
mu_set<-cbind(mu1,mu2,mu3,mu4,mu5)
cov.Sigma <- function(rho){
  return(diag(rho, nrow=2))
}
colour_set <- c("yellow", "violetred", "blue", "red")

##############################################################################
##############################################################################

### Mean = (0,0), Variance = 0.0001
dartboard("Mean = (0,0), Variance = 0.0001")
plot(NA, xlim=c(-1.1,1.1),ylim=c(-1.1,1.1))
hits1 <- mvrnorm(n[2],mu1,cov.Sigma(rho_set[1]))
points(hits1,col=colour_set[1], pch=13, cex=2)


### Mean = (0,0), Variance = 0.01
dartboard("Mean = (0,0), Variance = 0.01")
plot(NA, xlim=c(-1.1,1.1),ylim=c(-1.1,1.1))
hits2 <- mvrnorm(n[2],mu1,cov.Sigma(rho_set[2]))
points(hits2,col=colour_set[2], pch=13, cex=2)


### Mean = (0,0), Variance = 0.1
dartboard("Mean = (0,0), Variance = 0.1")
plot(NA, xlim=c(-1.1,1.1),ylim=c(-1.1,1.1))
hits3 <- mvrnorm(n[2],mu1,cov.Sigma(rho_set[3]))
points(hits3,col=colour_set[3], pch=13, cex=2)

##############################################################################
##############################################################################

### Play darts
dartboard("Mean = (0,0)")
plot(NA, xlim=c(-1.1,1.1),ylim=c(-1.1,1.1))
points(hits3,col=colour_set[3], pch=13, cex=2)
points(hits2,col=colour_set[2], pch=13, cex=2)
points(hits1,col=colour_set[1], pch=13, cex=2)
legend("topright",legend=c("Variance = 0.0001", 
                           "Variance = 0.01", "Variance = 0.1"),
       col = c("yellow", "violetred", "blue"), pch=13, cex=1)

### Hide dartboard
plot(NA, xlim=c(-1.1,1.1),ylim=c(-1.1,1.1), main="Mean = (0,0)")
points(hits3,col=colour_set[3], pch=13, cex=2)
points(hits2,col=colour_set[2], pch=13, cex=2)
points(hits1,col=colour_set[1], pch=13, cex=2)
legend("topright",legend=c("Variance = 0.0001", 
                           "Variance = 0.01", "Variance = 0.1"),
       col = c("yellow", "violetred", "blue"), pch=13, cex=1)

##############################################################################
##############################################################################

### If time permitted, show dartboard with different means
hits4 <- mvrnorm(n[2],mu2,cov.Sigma(rho_set[2]))
hits5 <- mvrnorm(n[2],mu3,cov.Sigma(rho_set[2]))
hits6 <- mvrnorm(n[2],mu4,cov.Sigma(rho_set[2]))
hits7 <- mvrnorm(n[2],mu5,cov.Sigma(rho_set[2]))

### Different means, Variance = 0.01
dartboard("Different Means, Variance = 0.01")
plot(NA, xlim=c(-1.1,1.1),ylim=c(-1.1,1.1))
points(hits4,col=colour_set[1], pch=13, cex=2)
points(mu2[1],mu2[2], col="gray", pch=17, cex=1.5)
points(mu2[1],mu2[2], col="black", pch=24, cex=1.6)
points(hits5,col=colour_set[2], pch=13, cex=2)
points(mu3[1],mu3[2], col="gray", pch=17, cex=1.5)
points(mu3[1],mu3[2], col="black", pch=24, cex=1.6)
points(hits6,col=colour_set[3], pch=13, cex=2)
points(mu4[1],mu4[2], col="gray", pch=17, cex=1.5)
points(mu4[1],mu4[2], col="black", pch=24, cex=1.6)
points(hits7,col=colour_set[4], pch=13, cex=2)
points(mu5[1],mu5[2], col="gray", pch=17, cex=1.5)
points(mu5[1],mu5[2], col="black", pch=24, cex=1.6)
legend("topright",legend=c("Mean=(0.5,0.5)", "Mean=(-0.5,0.3)", 
                           "Mean=(-0.4,-0.6)", "Mean=(0.1,-0.7)",
                           "Means"),
       pch=c(13,13,13,13,17), col=c(colour_set,"gray"))


##############################################################################
##############################################################################
### Final Plot
# par(mfrow=c(1,2))
# dartboard("Common mean, different variances")
# points(hits3,col=colour_set[3], pch=13, cex=2)
# points(hits2,col=colour_set[2], pch=13, cex=2)
# points(hits1,col=colour_set[1], pch=13, cex=2)
# # legend("topright",legend=c("Variance = 0.0001", 
# #                            "Variance = 0.01", "Variance = 0.1"),
# #        col = c("yellow", "violetred", "blue"), pch=13, cex=1)
# dartboard("Different means, common variance")
# points(hits4,col=colour_set[1], pch=13, cex=2)
# points(mu2[1],mu2[2], col="gray", pch=17, cex=1.5)
# points(mu2[1],mu2[2], col="black", pch=24, cex=1.6)
# points(hits5,col=colour_set[2], pch=13, cex=2)
# points(mu3[1],mu3[2], col="gray", pch=17, cex=1.5)
# points(mu3[1],mu3[2], col="black", pch=24, cex=1.6)
# points(hits6,col=colour_set[3], pch=13, cex=2)
# points(mu4[1],mu4[2], col="gray", pch=17, cex=1.5)
# points(mu4[1],mu4[2], col="black", pch=24, cex=1.6)
# points(hits7,col=colour_set[4], pch=13, cex=2)
# points(mu5[1],mu5[2], col="gray", pch=17, cex=1.5)
# points(mu5[1],mu5[2], col="black", pch=24, cex=1.6)
# legend("topright",legend=c("Mean=(0.5,0.5)", "Mean=(-0.5,0.3)", 
#                            "Mean=(-0.4,-0.6)", "Mean=(0.1,-0.7)",
#                            "Mean"),
#        pch=c(13,13,13,13,17), col=c(colour_set,"gray"))
# layout(1)

