
counts <- c(24,178,1)

barplot(counts,main = "Treatarm B",names.arg = c("Alive,no AE","AE","SCT/TKI without prior AE"))


ppois(180, lambda = 400)
