library(strucchange)

# Bai & Perron (2003)
# US ex-post real interest rate: 
# the three-month treasury bill deflated by the CPI inflation rate.
data("RealInt")

## estimate breakpoints
bp.ri <- breakpoints(RealInt ~ 1, h = 15)
#x11(); 
plot(bp.ri)
summary(bp.ri)

## fit segmented model with two breaks from minimized BIC
fac.ri <- breakfactor(bp.ri, breaks = 2, label = "seg")
fm.ri <- lm(RealInt ~ 0 + fac.ri)
summary(fm.ri)

## Visualization
#x11(); 
plot(RealInt)
lines(as.vector(time(RealInt)), fitted(fm.ri), col = 4)
