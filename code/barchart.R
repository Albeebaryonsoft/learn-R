
counts <- c(24,178,1)

barplot(counts,main = "Treatarm B",names.arg = c("Alive,no AE","AE","SCT/TKI without prior AE"))


# P(N<30)
ppois(29, lambda = 50)

# P(N>=30)
1-ppois(29, lambda = 50)

# P(N=30)
dpois(30, lambda = 50)


# P(X≥10)=1−P(X≤9)
1-ppois(9, lambda = 3)

#test test 
