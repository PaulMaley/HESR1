
# Note the "#" character tells R to ignore everything 
# that comes after it until the next line. 

# Data frame - With missing data as in the slides
raw <- data.frame(Package=factor(c(1,2,3,4)),
                  S1 = c(11,12,23,27),
                  S2 = c(17,10,20,33),
                  S3 = c(16,15,18,22),
                  S4 = c(14,19,17,26),
                  S5 = c(15,11,NA,28))

# Data needs to be reshaped!
# look at "raw" and "df" to see what this next function
# does - it "reshapes" the data
df <- reshape(raw, 
              v.names="Sales",
              idvar = "Package",
              direction = "long", 
              timevar="Store", 
              varying = c("S1","S2","S3","S4","S5"))

# Perform the analysis and look at the results
boxplot(Sales ~ Package, df)
model <- aov(Sales ~ Package, df)
plot(model)

# We'll look at this next week
TukeyHSD(model)

# Here is almost the same data, but I've
# added in a number where it was missing before
# to make the code simpler
raw <- data.frame(Package=factor(c(1,2,3,4)),
                  S1 = c(11,12,23,27),
                  S2 = c(17,10,20,33),
                  S3 = c(16,15,18,22),
                  S4 = c(14,19,17,26),
                  S5 = c(15,11,15,28))

# Data needs to be reshaped!
df <- reshape(raw, 
              v.names="Sales",
              idvar = "Package",
              direction = "long", 
              timevar="Store", 
              varying = c("S1","S2","S3","S4","S5"))

#
# Calculations (Very explicit)
#

# Separate the package designs into different samples
xs1 <- df$Sales[df$Package == 1]
xs2 <- df$Sales[df$Package == 2]
xs3 <- df$Sales[df$Package == 3]
xs4 <- df$Sales[df$Package == 4]

# Calculate the mean for each
xb1 <- mean(xs1)
xb2 <- mean(xs2)
xb3 <- mean(xs3)
xb4 <- mean(xs4)
            
# Calculate grand mean (xbb)
xs <- c(xs1, xs2, xs3, xs4)
xbb <- mean(xs)

# Calculate SSW
SSW <- sum(c( (xs1-xb1)^2, 
              (xs2-xb2)^2, 
              (xs3-xb3)^2, 
              (xs4-xb4)^2))

# Calculate SBB (5 is th number of data points
# in each package design sample)
SSB <- sum(5*c((xb1-xbb)^2, 
               (xb2-xbb)^2, 
               (xb3-xbb)^2, 
               (xb4-xbb)^2))

# Calculate F
MSW <- SSW/(length(xs)-4)
MSB <- SSB/(4-1)

F = MSB/MSW

# The critical value for F (alpha=1%):
alpha <- 0.01
Fc <- qF(0.99, 4-1, 20-4)

# Compare F and Fc