# This is our code which gets stocks data from Yahoo Finance through the quantmod package and performs
# analysis on the data.

install.packages("igraph")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
library(igraph)
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)

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

# Pre-processing of data

# removing data which are null
available <- tickers[!sapply(results, is.null)]
available

prices <- list()

for (t in available) {
  data <- getSymbols(t, src="yahoo", auto.assign=FALSE, from="2010-01-01")
  prices[[t]] <- Ad(data)  # Adjusted Close
}

# remove tickers whose data starts after the date below since most companies have data from that date 
# onwards and only a few do not.
cutoff <- as.Date("2010-01-04")

starts <- sapply(prices, function(x) start(x))
tickers_before_cutoff <- names(starts[as.Date(starts) <= cutoff])
prices <- prices[tickers_before_cutoff] 

removed_tickers <- setdiff(names(starts), tickers_before_cutoff)
cat("Removed tickers due to late start:\n")
print(removed_tickers)

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

# Creating correlation matrix and graph based on it

cor(energy_mean, mining_mean)

cor_mat <- cor(returns, use="pairwise.complete.obs")
threshold <- 0.3

# adjacency matrix based on correlation matrix with a threshold of 0.3 for an edge to exist between the 
# 2 nodes
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
V(g)$degree <- degree_of_nodes
cat("Degree of nodes: ", degree_of_nodes)

# computing eigenvector centrality and storing it as an attribute
eigenvector_centrality <- eigen_centrality(g)$vector
cat("Eigenvector centrality of nodes: ", eigenvector_centrality)
V(g)$eigenvector_centrality <- eigenvector_centrality

# Betweenness centrality of the nodes in descending order
betweenness_centrality <- betweenness(g)
V(g)$betweenness_centrality <- betweenness_centrality

closeness_centrality <- closeness(g)
V(g)$closeness_centrality <- closeness_centrality

V(g)$sector
vertex_attr(g)

#nodes with all attributes so far
nodes_with_attributes <- as.data.frame(vertex_attr(g))
nodes_with_attributes

# eigenvector centrality of nodes in descending order
nodes_eigenvector_centrality_descending <- nodes_with_attributes[order(-nodes_with_attributes$eigenvector_centrality),]
nodes_eigenvector_centrality_descending

# WPM, PAAS, ABX, AEM, and K have the highest eigenvector centralities which could indicate that they are 
# some of the most influential companies in this network.

# degree of nodes in descending order
nodes_degree_descending <- nodes_with_attributes[order(-nodes_with_attributes$degree),]
nodes_degree_descending

#SU, CNQ, CVE, IMO, and VET have the highest degrees, each with 15.

nodes_betweenness_centrality_descending <- nodes_with_attributes[order(-nodes_with_attributes$betweenness_centrality),]
nodes_betweenness_centrality_descending

#PAAS, CS, LUN, FM, and BIR have the highest betweennness centralities.

nodes_closeness_centrality_descending <- nodes_with_attributes[order(-nodes_with_attributes$closeness_centrality),]
nodes_closeness_centrality_descending

#FM, LUN, PAAS, WPM, and CS have the highest closeness centralities.

# Below code determines the degree each node has for nodes of the same sector and opposite sector
# This will help us in answering our 1st research question, do energy companies stocks affect that of 
# mining companies and also some insight for the third question, how interconnected are the 2 sectors and 
# which companies are most influential

# results storing the details of each node, including the same sector and opposite sector degree of each
# node

# ==========================================================================================
# Question 1 - In Canada, do energy stocks significantly impact mining stocks or vice versa?
# ==========================================================================================

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

# The energy companies which have a higher degree in the opposite sector are: FNV (0-10), FM (5-9), 
# LUN (5-8), CCO (2-5), and CS (3-6). The numbers in the brackets are the degrees for same sector and 
# opposite sectors respectively.

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
# correlation results by period: 2010-2014 was 0.5318576, 2015-2019 was 0.3410688, and 2020-2025 was 
# 0.3803875.

avg_pairwise_corr
# average pairwise correlation results by period: 2010-2014 was 0.2178981, 2015-2019 was 0.1469825, and 
# 2020-2025 was 0.2126215.

# =================================================================================================
# Question 3: How interconnected are Canada's mining and energy companies? Which companies are most
# connected/influential in the network?
# =================================================================================================

adjacency_matrix_edge_existing_or_not <- abs(cor_mat) > threshold
energy_to_mining_edges <- sum(adjacency_matrix_edge_existing_or_not[energy, mining])
total_possible_energy_to_mining_edges <- length(energy) * length(mining)

energy_to_mining_edges
total_possible_energy_to_mining_edges

edge_density_between_sectors <- energy_to_mining_edges/total_possible_energy_to_mining_edges
edge_density_between_sectors

# So on a scale of 0 to 1, only 0.2087912 of edges between any energy company with any mining company 
# exists. So this means that around 20.9% of the possible strong correlation edges between 2 companies 
# of opposite sectors exist.

cor_mat
correlation_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
colnames(correlation_df)[3] <- "correlation_value"

correlation_df
#dropping the self correlations of companies
correlation_df <- correlation_df[correlation_df$Var1 != correlation_df$Var2, ]

#removing repeat pairs of same 2 nodes
correlation_df <- correlation_df[apply(correlation_df, 1, function(node) node[1] < node[2]), ]

#removing correlations = NA
correlation_df <- correlation_df[!is.na(correlation_df$correlation_value), ]

correlation_df

# creating a companies data frame with the company name and the corresponding sector from the graph's 
# vertices
companies <- data.frame(
  company = V(g)$name,
  sector = V(g)$sector,
  stringAsFactors = FALSE
)

correlation_df <- correlation_df %>%
  left_join(companies %>% select(company, sector), by = c("Var1" = "company")) %>%
  rename(sector1 = sector) %>%
  left_join(companies %>% select(company, sector), by = c("Var2" = "company")) %>%
  rename(sector2 = sector)

# getting only correlations between 2 companies of opposite sectors
energy_and_mining_correlations <- correlation_df %>%
  filter(
    (sector1 == "energy" & sector2 == "mining") |
      (sector1 == "mining" & sector2 == "energy")
  )

energy_and_mining_correlations <- energy_and_mining_correlations %>%
  arrange(desc(correlation_value))

energy_and_mining_correlations

# On printing energy_and_mining_correlations in descending order of correlation value, we can see that the
# top 10 correlations are above 0.5 on the correlation scale of -1 to +1. This is a significant finding as
# that is a good number of correlations between companies of opposite sectors that is above 0.5 with the
# highest being 0.72.

pagerank_centrality <- page.rank(g)$vector
pagerank_centrality

V(g)$pagerank_centrality <- pagerank_centrality

nodes_with_attributes <- as.data.frame(vertex_attr(g))
nodes_with_attributes

nodes_pagerank_centrality_descending <- nodes_with_attributes[order(-nodes_with_attributes$pagerank_centrality),]
nodes_pagerank_centrality_descending

# The top 5 nodes by pagerank centrality are CNQ, SU, CVE, VET, and IMO. This shows their 
# importance/influence in the network. WPM, PAAS, ABX, AEM, and K were the nodes with the 
# highest eigenvector centrality. So there are no common nodes in the two lists. This shows 
# that they are separately considered important by two different types of centrality. 

# Average degree
deg_vec <- degree(g)
avg_degree <- mean(deg_vec)
cat("Average degree:", avg_degree, "\n")

# Clustering coefficient (transitivity)
global_clustering_coef <- transitivity(g, type = "global")
cat("Clustering coefficient:", global_clustering_coef, "\n")

# K-core
kcore_index <- coreness(g)

node_stats <- data.frame(
  sector                  = V(g)$sector,
  degree                  = deg_vec,
  k_core                  = kcore_index,
  eigenvector_centrality,
  betweenness_centrality,
  closeness_centrality,
  stringsAsFactors        = FALSE
)

node_stats_ordered <- node_stats[
  order(-node_stats$degree, -node_stats$k_core),
]

head(node_stats_ordered, 25)

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

# Merge returns and market returns, keeping only dates that exist in both
aligned_data <- merge(returns, mkt_ret, join="inner")  

# Extract returns and market separately
returns_aligned <- aligned_data[, colnames(returns)]
mkt_ret_aligned <- aligned_data[, colnames(mkt_ret)]

# Use the row index of returns_aligned as the proper time vector
aligned_dates <- index(returns_aligned)

# Step 2. regress each stock on the market and keep residuals
resid_mat <- matrix(NA_real_,
                    nrow = nrow(returns_aligned),
                    ncol = ncol(returns_aligned))

colnames(resid_mat) <- colnames(returns_aligned)

for (j in seq_len(ncol(returns_aligned))) {
  y <- as.numeric(returns_aligned[, j])
  x <- as.numeric(mkt_ret_aligned)
  
  fit <- lm(y ~ x)        # return_i = alpha + beta * market + error
  resid_mat[, j] <- residuals(fit)   # store error (idiosyncratic return)
}

resid_xts <- xts::xts(resid_mat, order.by = aligned_dates)

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

# Original edges = 159, residual edges = 87

cat("Original average degree:", mean(degree(g)), "\n")
cat("Residual average degree:", mean(degree(g_resid)), "\n")

# Original average degree = 11.77778, residual average degree = 6.444444

mod_orig  <- cluster_louvain(g)
mod_resid <- cluster_louvain(g_resid)

cat("Original modularity:", modularity(mod_orig), "\n")
cat("Residual modularity:", modularity(mod_resid), "\n")

# Original modularity = 0.4708674, residual modularity = 0.4441319

