# Data frame
raw <- data.frame(Package=factor(c(1,2,3,4)),
                  S1 = c(11,12,23,27),
                  S2 = c(17,10,20,33),
                  S3 = c(16,15,18,22),
                  S4 = c(14,19,17,26),
                  S5 = c(15,11,15,28))
#                  S5 = c(15,11,NA,28))

# Data needs to be reshaped!
df <- reshape(raw, 
              v.names="Sales",
              idvar = "Package",
              direction = "long", 
              timevar="Store", 
              varying = c("S1","S2","S3","S4","S5"))

boxplot(Sales ~ Package, df)

#
# Calculations (Very explicit)
#

# Separate out the packages
xs1 <- df$Sales[df$Package == 1]
xs2 <- df$Sales[df$Package == 2]
xs3 <- df$Sales[df$Package == 3]
xs4 <- df$Sales[df$Package == 4]

# Calculate the mean for each
xb1 <- mean(xs1)
xb2 <- mean(xs2)
xb3 <- mean(xs3)
xb4 <- mean(xs4)
            
# Calculate Total
xs <- c(xs1, xs2, xs3, xs4)
xbb <- mean(xs)
SSB <- sum((xs-xbb)^2)

# Calculate Within
# Th
SSW <- sum(c( (xs1-xb1)^2, 
              (xs2-xb2)^2, 
              (xs3-xb3)^2, 
              (xs4-xb4)^2))

# Calculate Between
SSB <- sum(5*c((xb1-xbb)^2, 
               (xb2-xbb)^2, 
               (xb3-xbb)^2, 
               (xb4-xbb)^2))

# Calculate F
MSW <- SSW/(length(xs)-4)
MSB <- SSB/(4-1)

F = MSB/MSW


