# This is our code which gets stocks data from Yahoo Finance through the quantmod package and performs
# analysis on the data.

install.packages("igraph")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
library(igraph)
library(quantmod)
library(PerformanceAnalytics)

tickers <- c(
  # Energy
  "ENB.TO", "TRP.TO", "SU.TO", "CNQ.TO", "CVE.TO",
  "IMO.TO", "PPL.TO", "GEI.TO", "KEY.TO", "FNV.TO", 
  "BTE.TO", "CEU.TO", "VET.TO", "BIR.TO", "CEGS.TO",
  # Mining
  "ABX.TO", "NTR.TO", "FM.TO", "LUN.TO",
  "K.TO", "AEM.TO", "WPM.TO", "ELD.TO",
  "AGI.TO", "CCO.TO", "CS.TO", "OR.TO",
  "BTO.TO", "PAAS.TO", "IVN.TO", "ORA.TO", 
  "CG.TO", "EDV.TO", "GMIN.TO"
)
safeGet <- function(t) {
  tryCatch(getSymbols(t, src="yahoo", auto.assign=FALSE),
           error=function(e) NULL)
}
results <- lapply(tickers, safeGet)
available <- tickers[!sapply(results, is.null)]
available

prices <- list()

for (t in available) {
  data <- getSymbols(t, src="yahoo", auto.assign=FALSE, from="2010-01-01")
  prices[[t]] <- Ad(data)  # Adjusted Close
}

# merging all prices into a big xts object according to the date
all_prices <- do.call(merge, prices)
head(all_prices)

# removing any companies which have all days' values as NA
all_prices <- all_prices[, colSums(!is.na(all_prices)) > 0] 

# keeping only companies which have less than 10% of their values as NA
all_prices <- all_prices[, colMeans(is.na(all_prices)) < 0.1]

# storing energy and mining companies' names in separate vectors
energy_companies_count <- 15   # count of energy tickers in your tickers vector

# creating energy vector with ".Adjusted" appended
energy <- paste0(tickers[1:energy_companies_count], ".Adjusted")

# Create mining vector with ".Adjusted" appended
mining <- paste0(tickers[(n_energy+1):length(tickers)], ".Adjusted")

#filtering energy and mining vectiors tio only include those that are in the all_prices xts object
energy <- energy[energy %in% colnames(all_prices)]
mining <- mining[mining %in% colnames(all_prices)]

#getting the return value for the prices
returns <- na.omit(Return.calculate(all_prices))
head(returns)

# calculating the means of energy and mining prices respectively
energy_mean <- xts(rowMeans(returns[, energy], na.rm = TRUE), order.by = index(returns))
mining_mean <- xts(rowMeans(returns[, mining], na.rm = TRUE), order.by = index(returns))

# plot for energy and mining means
plot(energy_mean, main="Energy vs Mining Average Returns", col="red")
lines(mining_mean, col="blue")
legend("topright", legend=c("Energy", "Mining"), col=c("red","blue"), lty=1)


#TRIAL 1

cor(energy_mean, mining_mean)

cor_mat <- cor(coredata(returns), use="pairwise.complete.obs")
adj <- (abs(cor_mat) > 0.5) * cor_mat  # weight = corr (or 1)
g <- graph_from_adjacency_matrix(adj, mode="undirected", weighted=TRUE, diag=FALSE)
V(g)
V(g)$sector <- ifelse(names(V(g)) %in% energy, "energy","mining")

# centralities
deg <- degree(g)
deg
eig <- eigen_centrality(g)$vector
eig
betw <- betweenness(g)
betw

V(g)$color <- ifelse(V(g)$sector == "energy", "tomato", "skyblue")
plot(g,
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     layout = layout_with_fr,
     main = "Energy vs Mining Stock Correlation Network")

comm <- cluster_louvain(g)

#TRIAL 2

cor(energy_mean, mining_mean)

cor_mat <- cor(coredata(returns), use="pairwise.complete.obs")
threshold <- 0.3
adj <- (abs(cor_mat) > threshold) * cor_mat  # weight = corr (or 1)
g <- graph_from_adjacency_matrix(adj, mode="undirected", weighted=TRUE, diag=FALSE)
V(g)
V(g)$sector <- ifelse(names(V(g)) %in% energy, "energy","mining")

V(g)$color <- ifelse(V(g)$sector == "energy", "tomato", "skyblue")
plot(g,
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     layout = layout_with_fr,
     main = "Energy vs Mining Stock Correlation Network")

degree_of_nodes <- degree(g)
degree_of_nodes

eigenvector_centrality <- eigen_centrality(g)$vector
eigenvector_centrality
