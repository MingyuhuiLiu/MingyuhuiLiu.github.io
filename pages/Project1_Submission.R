setwd("C:\\Users\\Jane Liu\\Documents\\GWU Spring2018\\BigData\\Project1\\Project1")

# Get the library:
library(igraph)

# Merge --------------------------------------------------------------------
# Merge edges files to create a networkd file contains all nodes:
a = list.files("Edges");
dir = paste("./Edges/",a,sep="");   
n = length(dir);
# Merge edges into dataframe:
for (i in 1:n){
  if(!exists("merge.data"))
    merge.data <- read.table(dir[i],header = FALSE)
  if(exists("merge.data"))
    merge.data=rbind(merge.data,read.table(dir[i],header = FALSE))
}

# igraph Object -------------------------------------------------------------
# Create igraph class.
g <- graph_from_data_frame(merge.data,directed = TRUE)

# Define vertices and edges for simplified g:
V(g)$label <- NA
E(g)$arrow.size <- .2

#5a. Method 1: central person: as the one who with the most degrees 
#without double counting in and out edges.:
g <- simplify(g, remove.multiple = TRUE)
central_original <- V(g)$name[degree(g)==max(degree(g))]

#5a. Method 2:
# Followed central person:
dg = degree(g,mode = "in",loops = FALSE)
head(sort(dg,decreasing = TRUE))
# Following central person:
dg = degree(g,mode = "out",loops = FALSE)
head(sort(dg,decreasing = TRUE))

# 5b. The longest path:
# Method 1:
get_diameter(g, directed = TRUE, unconnected = TRUE)
# Method 2:
farthest.nodes(g)

# 5c. Find the largest clique:
cliques_original <- largest_cliques(g)
cliques_original

# 5d. Ego:
# For the whole data, if one is Ego, it has to be the central person as well.
# Thus, the rationale here is to check if the central person has the connections to all the people directly,
# aka, with a step length of 1. 


# 5e. i. Boncich power centralities of positions
power_centrality(g, nodes = V(g), loops = FALSE, exponent = 1,
                 rescale = FALSE, tol = 1e-07, sparse = TRUE)


# 5e. ii. Betweeness.
betweenness(g, v = V(g), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)

# Question 5 -----------------------------------------------------------------

# Simplification ---------------------------------------------------------
# Simplify with filters: drop any nodes with degrees less or equal to 400:
g <- graph_from_data_frame(merge.data,directed = TRUE)
dg = degree(g)
g = induced_subgraph(g,which(dg>400))

# Define vertices and edges for simplified g:
V(g)$label <- NA
E(g)$arrow.size <- .2

#5a. Method 1: central person: as the one who with the most degrees 
#without double counting in and out edges.:
g <- simplify(g, remove.multiple = TRUE)
central_filter <- V(g)$name[degree(g)==max(degree(g))]

#5a. Method 2:
# Followed central person:
dg = degree(g,mode = "in",loops = FALSE)
head(sort(dg,decreasing = TRUE))
# Following central person:
dg = degree(g,mode = "out",loops = FALSE)
head(sort(dg,decreasing = TRUE))

# 5b. The longest path:
# Method 1:
get_diameter(g, directed = TRUE, unconnected = TRUE)
# Method 2:
farthest.nodes(g)

# 5c. Find the largest clique:
cliques_filter <- largest_cliques(g)
cliques_filter

# 5e. i. Boncich power centralities of positions
power_centrality(g, nodes = V(g), loops = FALSE, exponent = 1,
                 rescale = FALSE, tol = 1e-07, sparse = TRUE)

# 5e. ii. Betweeness.
betweenness(g, v = V(g), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)

# Simplification ---------------------------------------------------------
# Simplify with filters and concatenation:
g <- graph_from_data_frame(merge.data,directed = TRUE)
dg = degree(g)
g = induced_subgraph(g,which(dg>400))
g <- simplify(g, edge.attr.comb="concat")

# Define vertices and edges for simplified g:
V(g)$label <- NA
E(g)$arrow.size <- .2

#5a. Method 1: central person: as the one who with the most degrees 
#without double counting in and out edges.:
g <- simplify(g, remove.multiple = TRUE)
central_both <- V(g)$name[degree(g)==max(degree(g))]

#5a. Method 2:
# Followed central person:
dg = degree(g,mode = "in",loops = FALSE)
head(sort(dg,decreasing = TRUE))
# Following central person:
dg = degree(g,mode = "out",loops = FALSE)
head(sort(dg,decreasing = TRUE))

# 5b. The longest path:
# Method 1:
get_diameter(g, directed = TRUE, unconnected = TRUE)
# Method 2:
farthest.nodes(g)

# 5c. Find the largest clique:
cliques_both <- largest_cliques(g)
cliques_both

# 5e. i. Boncich power centralities of positions
power_centrality(g, nodes = V(g), loops = FALSE, exponent = 1,
                 rescale = FALSE, tol = 1e-07, sparse = TRUE)

# 5e. ii. Betweeness.
betweenness(g, v = V(g), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)

farthest.nodes(g)

# Question 4 ------------------------------------------------------------
g <- graph_from_data_frame(merge.data,directed = TRUE)
dg = degree(g)
g = induced_subgraph(g,which(dg>400))
g <- simplify(g, edge.attr.comb="concat")
V(g)$label <- NA
E(g)$arrow.size <- .2

# Function 1: mean_distance: http://igraph.org/r/doc/distances.html
mean_distance(g, directed = TRUE, unconnected = TRUE)

# Function 2: knn: http://igraph.org/r/doc/knn.html
knn(g, vids = V(g), weights = NULL)

# Function 3: which_mutual: http://igraph.org/r/doc/which_mutual.html
which_mutual(g, es = E(g))

# Function 4: radius: http://igraph.org/r/doc/radius.html
radius(g, mode = c("all"))

# Function 5: 
random_walk(g, "106514999", 3, mode = c("all"),
            stuck = c("return", "error"))

# Function 6:
edge_density(g, loops = FALSE)

# Function 7:
count_triangles(g, vids = "263838766")

# Function 8: are_adjacent: 
are_adjacent(g, "28192145", "22682436")

# Function 9: gsize:
gsize(g)

# Function 10: eccentricity
eccentricity(g, vids = "370244456", mode = c("all"))

# Function 11:
incident(g, "113859309", mode = c('in'))

# Function 12: 
articulation_points(g)

# Ego: ego should be the one directly connected to every single node in the network.
ego(g, 1, nodes = '3359851', mode = c("all"),
    mindist = 0)


# Circles Plotting ----------------------------------------------------------------
# Step 1: Merge all circles.
a = list.files("Circles");
dir = paste("./Circles/",a,sep=""); 
setwd("./Circles")
for (file in a){
    con = file(file, "r")
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      arr <- strsplit(line,split = "\t")
      arr=arr[[1]]
      n =length(arr)
      new <- rep(strsplit(file,split = "\\.")[[1]][1],n-1)
      new1 <- rep(arr[1],n-1)
      arr <- arr[!arr %in% arr[1]]
      if(!exists("result"))
      { 
        result <-cbind(new,arr)
        result <-cbind(result,new1)
      }
      if(exists("result")){
        current=cbind(new,arr)
        current <-cbind(current,new1)
        result <-rbind(result,current)
      }
    }

  close(con)
}

# Step 2: Plot all circles: -----------------------------------------------------------------
#colors() is a function which will get a vector of colors. 
#Because the first color is white, we skip the first one and begin with mycols[2]. 
#We set a new attribute of E(g) according to the circle name. 
#So the different color means different circle name.
ver<- graph.data.frame(result)
dg = degree(ver)
V(ver)$size <- .1
V(ver)$label <- NA
E(ver)$arrow.size <- .2
E(ver)$circle <- result[,3]
mycols <- colors()
E(ver)$color <- mycols[as.numeric(E(ver)$circle)+2]
simplify(ver,remove.multiple = TRUE,remove.loops = TRUE,edge.attr.comb="concat")
plot(ver)
