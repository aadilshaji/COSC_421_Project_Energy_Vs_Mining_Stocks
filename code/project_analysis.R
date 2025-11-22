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
mining <- paste0(tickers[(energy_companies_count+1):length(tickers)], ".Adjusted")

#filtering energy and mining vectors to only include those that are in the all_prices xts object
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


# TRIAL 1
# As of now, DO NOT RUN Trial 1 and instead RUN TRIAL 2. I did not remove it from the file just in case we 
# need it for any reason in the future.

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

cor_mat <- cor(returns, use="pairwise.complete.obs")
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
V(g)$degree <- degree_of_nodes

eigenvector_centrality <- eigen_centrality(g)$vector
eigenvector_centrality
V(g)$eigenvector_centrality <- eigenvector_centrality

V(g)$sector
vertex_attr(g)

nodes_with_attributes <- as.data.frame(vertex_attr(g))
nodes_with_attributes

nodes_eigenvector_centrality_descending <- nodes_with_attributes[order(-nodes_with_attributes$eigenvector_centrality),]
nodes_eigenvector_centrality_descending

cor_mat
correlation_df <- as.data.frame(as.table(cor_mat))
correlation_df
correlation
correlation_df <- correlation_df[correlation_df$Var1 < correlation_df$Var2, ]
correlation_df

# results storing the details of each node, including the same sector and opposite sector degree of each
# node
results <- data.frame( node = V(g)$name, sector = V(g)$sector, node_degree = degree(g), 
                       same_sector_degree = integer(vcount(g)), 
                       opposite_sector_degree = integer(vcount(g)))

for(i in seq_len(vcount(g)))
{
  #current node is v 
  v <- V(g)[i]
  neighbours <- neighbors(g, v) # neighbours of v in g
  
  v_sector <- V(g)$sector[v] # sector of current node v
  
  neighbours_sectors <- V(g)$sector[neighbours] # sectors of neighbours
  
  same_sector_neighbours_degree <- sum(neighbours_sectors == v_sector)
  opposite_sector_neighbours_degree <- sum(neighbours_sectors != v_sector)
  
  results$same_sector_degree[i] <- same_sector_neighbours_degree
  results$opposite_sector_degree[i] <- opposite_sector_neighbours_degree
  
}

# printing results. notice how some of the nodes have higher degree in the opposite sector compared to 
# their own sector.
results

# ===================================================
# Question 2: Has the correlation changed over time?
# ===================================================

# 1. Define time periods (you can tweak these dates if you want)
periods <- list(
  period1 = c("2010-01-01", "2014-12-31"),
  period2 = c("2015-01-01", "2019-12-31"),
  period3 = c("2020-01-01", "2025-12-31")
)
avg_pairwise_corr <- data.frame(
  period = character(),
  start = character(),
  end = character(),
  avg_energy_mining_corr = numeric(),
  stringsAsFactors = FALSE
)

for (nm in names(periods)) {
  start_date <- periods[[nm]][1]
  end_date   <- periods[[nm]][2]
  
  # subset returns to this window
  sub_ret <- returns[paste0(start_date, "/", end_date)]
  
  if (nrow(sub_ret) < 10) next  # skip if too few days
  
  # full correlation matrix for this window
  cm <- cor(sub_ret, use = "pairwise.complete.obs")
  
  # all energy × mining pairwise correlations
  em_corrs <- cm[energy, mining, drop = FALSE]
  
  # average of those correlations
  avg_val <- mean(em_corrs, na.rm = TRUE)
  
  avg_pairwise_corr <- rbind(
    avg_pairwise_corr,
    data.frame(
      period = nm,
      start = start_date,
      end = end_date,
      avg_energy_mining_corr = avg_val,
      stringsAsFactors = FALSE
    )
  )
}
# helper function to compute sector–sector correlation in one period
sector_corr_in_period <- function(start_date, end_date) {
  # subset returns for this window
  sub_ret <- returns[paste0(start_date, "/", end_date)]
  
  # if there are not enough rows, return NA
  if (nrow(sub_ret) < 10) return(NA_real_)
  
  # average daily returns for each sector in this window
  energy_mean_p <- xts(rowMeans(sub_ret[, energy], na.rm = TRUE),
                       order.by = index(sub_ret))
  mining_mean_p <- xts(rowMeans(sub_ret[, mining], na.rm = TRUE),
                       order.by = index(sub_ret))
  
  # correlation between sector averages
  as.numeric(cor(energy_mean_p, mining_mean_p, use = "pairwise.complete.obs"))
}

# 2. loop over periods and compute correlations
corr_results <- data.frame(
  period = character(),
  start = character(),
  end = character(),
  energy_mining_corr = numeric(),
  stringsAsFactors = FALSE
)

for (nm in names(periods)) {
  start_date <- periods[[nm]][1]
  end_date   <- periods[[nm]][2]
  corr_value <- sector_corr_in_period(start_date, end_date)
  
  corr_results <- rbind(
    corr_results,
    data.frame(
      period = nm,
      start = start_date,
      end = end_date,
      energy_mining_corr = corr_value,
      stringsAsFactors = FALSE
    )
  )
}

corr_results
avg_pairwise_corr

# ============================================
# Question 4: remove overall market movement
# ============================================

# Step 1. get TSX Composite index as the market factor
mkt_data <- getSymbols("^GSPTSE",
                       src = "yahoo",
                       auto.assign = FALSE,
                       from = "2010-01-01")

mkt_prices <- Ad(mkt_data)
mkt_ret <- na.omit(Return.calculate(mkt_prices))

# align dates between stock returns and market returns
common_dates <- intersect(index(returns), index(mkt_ret))
returns_aligned <- returns[common_dates, ]
mkt_ret_aligned <- mkt_ret[common_dates, 1]

# Step 2. regress each stock on the market and keep residuals
resid_mat <- matrix(NA_real_,
                    nrow = nrow(returns_aligned),
                    ncol = ncol(returns_aligned))

colnames(resid_mat) <- colnames(returns_aligned)
rownames(resid_mat) <- common_dates

for (j in seq_len(ncol(returns_aligned))) {
  y <- as.numeric(returns_aligned[, j])
  x <- as.numeric(mkt_ret_aligned)
  
  fit <- lm(y ~ x)        # return_i = alpha + beta * market + error
  resid_mat[, j] <- residuals(fit)   # store error (idiosyncratic return)
}

resid_xts <- xts::xts(resid_mat, order.by = common_dates)

# Step 3. build a new correlation network from residuals
cor_resid <- cor(resid_xts, use = "pairwise.complete.obs")

adj_resid <- (abs(cor_resid) > threshold) * cor_resid

g_resid <- graph_from_adjacency_matrix(adj_resid,
                                       mode = "undirected",
                                       weighted = TRUE,
                                       diag = FALSE)

V(g_resid)$sector <- ifelse(names(V(g_resid)) %in% energy,
                            "energy", "mining")
V(g_resid)$color <- ifelse(V(g_resid)$sector == "energy",
                           "tomato", "skyblue")

# Step 4. compare original and market neutral networks
par(mfrow = c(1, 2))

plot(g,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     layout = layout_with_fr,
     main = "Original returns network")

plot(g_resid,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     layout = layout_with_fr,
     main = "Network after removing market")

par(mfrow = c(1, 1))

cat("Original edges:", gsize(g), "\n")
cat("Residual edges:", gsize(g_resid), "\n")

cat("Original average degree:", mean(degree(g)), "\n")
cat("Residual average degree:", mean(degree(g_resid)), "\n")

mod_orig  <- cluster_louvain(g)
mod_resid <- cluster_louvain(g_resid)

cat("Original modularity:", modularity(mod_orig), "\n")
cat("Residual modularity:", modularity(mod_resid), "\n")

