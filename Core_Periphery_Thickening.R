#Core thickening function
library(igraph)

#The core thickening function has to be used carefully. Indeed, if you suppose the rich-club to be too big
#let's say 50% of the network (does it make sense?) or if you have a very large network the function may
#be too slow or create memory issues, compatibly with the memory size of your computer. Indeed, this function
#generates all the possible links realizable within the rich-club and then picks up the required amount.
#Therefore it generates choose(richclubnodes,2) couples. It follows that if richclubnodes is too big the
#function may cause problems. Improvements are welcome :)

Core_thickening <- function(g, percent_richclub, required_density){
  V(g)$name <- V(g) #set the names of the nodes so that the subgraph is properly made
  size_richclub <- round(vcount(g)*(percent_richclub/100)) #size of rich-club in number of nodes
  richclub_nodes <- order(degree(g), decreasing = T)[1:size_richclub]# the chosen richclub nodes 
  #so that they are the first X with highest degree 
  richclub <- induced.subgraph(g,vids = richclub_nodes)#
 linktoadd_richclub <- round(choose(size_richclub,2)*required_density)-ecount(richclub)# number of links to add
      D <- combn(sort(richclub_nodes),2) #there is a sort because in igraph elemnents the edgelist is
      #generally order so that a link exists between smaller_id_node -- higher_degree_node when 
      #undirected graphs are considered. MAKE SURE THAT YOUR EDGELIST IS ORDERED THIS WAY.
      #dim(D): 2 rows; X columns.
    #D contains all the possible couples of links within the rich-club subgraph
    
    doppi <- which(duplicated(rbind(get.edgelist(richclub, names = T),t(D))))
    #in order to generate simple graphs the variable doppi countes the couples of nodes that
    # are already connected (the links that are already present in the original graph)
    doppi <- doppi- ecount(richclub) #this operation is technical and it just sets the indices in the 
    #right oreder by subtracting the quantity ecount(initial_richclub).
    #More simply the links in doppi are indexed with the summation of the number of links in richclub
    if(length(doppi)!=0){D <- D[,-doppi]}
     #remove doppi from D (i.e. remove already present couples)
    s <- D[,sample(ncol(D),linktoadd_richclub)] 
    #from the clean version of D I sample the links that will be added in order to rich the desired 
    #rich-club density. The final rich-club density takes into account the initial number of links in
    #the subgraph therefore the number of links mights changes for different realization of the same
    #degree sequence
    
    #richclub_denso <- add_edges(richclub,s)# the dense rich-club
    #print(graph.density(richclub_denso))
    g <-  add_edges(g, s)
    #.print(ecount(g))
    return(g)
   
}

#The periphery thickening function is not a elengant one. The approach is correct but it 
#is "quick and dirty" thus, it's better to use such a function on sparse graphs and for
#large periphery size. Indeed, differently from the core thickening case, it's not possible
#to generate all the possible couples of edge endpoints and then choose among them. 
#Therefore I generate a "sufficiently large" amount of couples and choose among them. 
#The function returns a message that tells you wheter it was able to place the required
#amount of links while keeping the considered graph a simple graph.
#
#Conceptually the periphery tickens doesn't make the periphery as dense as the rich-club but
#it adds up to the periphery the amount of links that is necessary in order to have a richclub
#of the disidered size.

Periphery_thickening <- function(g, percent_richclub, required_density){
  V(g)$name <- V(g) #set the names of the nodes so that the subgraph is properly made
  size_richclub <- round(vcount(g)*(percent_richclub/100)) #size of rich-club in number of nodes
  richclub_nodes <- order(degree(g), decreasing = T)[1:size_richclub]# the chosen richclub nodes 
  #so that they are the first X with highest degree 
  richclub <- induced.subgraph(g,vids = richclub_nodes)#
  linktoadd_poorclub <- round(choose(size_richclub,2)*required_density)-ecount(richclub)
  # actual number of links to add in order to add the same links both in the core and in the per.
  poorclub_nodes <- order(degree(g), decreasing = T)[(size_richclub+1):vcount(g)]# the richclub nodes choosen
  #so that they are NOT the first X with highest degree 
  
  #I create a matrix from which I sample all the links to add to the periphery.
  #The matrix is oversized round(1.5*linktoadd_poorclub) in order to avoid (in theory) problems
  #related to double links and already present ones
  couples <- matrix(NA, 2, round(1.5*linktoadd_poorclub)) 
  for(j in 1:ncol(couples)){
    n1 <- sample((poorclub_nodes[1:(length(poorclub_nodes)-1)]),1)#choose the 1 endpoint
    if(length(poorclub_nodes[poorclub_nodes>n1])!=1){n2 <- sample(poorclub_nodes[poorclub_nodes>n1],1)
    }else{n2 <- sample(c(poorclub_nodes[poorclub_nodes>n1],poorclub_nodes[poorclub_nodes>n1]),1)}
    #choose the 2 endpoint with the index greater than the first (orderd couple) and then I avoid
    #the bias in the sample function when you sample a only integer
    couples[c(1,2),j] <- c(n1,n2) #assignment into couples
  }
  couples <- couples[,!duplicated(couples, MARGIN = 2)]#remove double columns
  couples <- rbind(couples, rep(NA, ncol(couples)))
  couples[3,] <- apply(couples, 2, function(x) are_adjacent(g, x[1],x[2]))
  #print(ncol(couples))
  couples <- couples[,which(couples[3,]==0)]#I choose only non-existing links
  #print(ncol(couples))
  #table(couples[3,])
  if(ncol(couples)>=linktoadd_poorclub){
    g <-  add_edges(g, as.vector(couples[c(1:2),c(1:linktoadd_poorclub)]))
    print("OK,")}else{print("You have to run the function again")}
  if(is.simple(g)){print("The graph is simple")}
  
  return(g)
}


##############################################################################################
#TEST AND EXAMPLE OF APPLICATION

library(igraph)
repeat{grandom <- erdos.renyi.game(100, 450, type = "gnm", directed = F)
       if(is.connected(grandom)){break}}
#we consider the richclubsize = 10, i.e. 1% of the nodes
actual_richclub <- induced.subgraph(grandom, order(degree(grandom), decreasing = T)[1:10])
graph.density(actual_richclub)

dense_core_grandom <- Core_thickening(grandom, 10, 0.8)
denser_richclub <- induced.subgraph(dense_core_grandom, order(degree(dense_core_grandom), decreasing = T)[1:10])
graph.density(denser_richclub)

dense_periphery_grandom <- Periphery_thickening(grandom, 10, 0.8)

ecount(dense_core_grandom) == ecount(dense_periphery_grandom)

ecount(dense_core_grandom) >= ecount(grandom)
##############################################################################################
