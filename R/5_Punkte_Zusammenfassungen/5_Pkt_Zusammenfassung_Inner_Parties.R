# install.packages("tidyverse")
# install.packages("igraph")
# install.packages("tidygraph")
# install.packages("ggraph")
# install.packages("readr")


require(tidyverse)
require(igraph)
require(tidygraph)
require(ggraph)
require(readr)
require(dplyr)

densities <- c()
components <- c()
diameters <- c()
transitivities <- c()
average_path_lengths <- c()
max_cores = c()
mean_k_cores = c()


parties=c("AfD", "CDU", "CSU", "SPD", "FDP", "GRUENE", "DIE LINKE")  
for(i in parties){
  #Filtere Tweets raus die sich selbst retweeten    
  rt_party_data_filtered <-rt_data%>%
    filter(party.x==party.y)%>%
    filter(party.x==i)%>%
    filter(twitter.handle.x != twitter.handle.y)
  
  #Baue Kanten zwischen den Nodes auf
  rt_edges_party <- rt_party_data_filtered%>%group_by(twitter.handle.x, twitter.handle.y) %>% mutate("amount" = 1)
  rt_edges_party <- rt_edges_party%>%summarise(amount=sum(amount))
  rt_nodes_party <- dplyr::distinct(twitter_data%>%select(twitter.handle, party))
  
  rt_net_party <- graph_from_data_frame(d=rt_edges_party, rt_nodes_party, directed = TRUE)
  
  #Entferne Self-Retweets
  rt_net_party <- simplify(rt_net_party, remove.multiple = FALSE)
  
  #Entferne isolierte Nodes
  rt_net_party <- delete_vertices(rt_net_party,degree(rt_net_party)==0)
  
  #Size/Größe
  # Density
  # L/k*(k-1) L=#observedTies k=#ofNods
  # Je näher der Wert bei 1 ist desto stärker sind die Knoten miteinander verbunden
  # Man kann es per Hand ausrechnen
  length(E(rt_net_party))/(length(V(rt_net_party))*(length(V(rt_net_party))-1))
  #Man kann aber auch die igraph-Funktion nutzen
  densities <- c(densities, edge_density(rt_net_party))
  #Components
  # Aus wievielen Einzelteilen ist das Netzwerk aufgebaut
  comp <- c(components(rt_net_party))
  components <- c(components, length(comp$membership))
  
  #Diameter/Geodesic
  # Nur für Netzwerke mit einer Komponente
  # Der längste aller kürzesten Pfade (zwischen Knoten A und B)
  # von allen Knotenpaaren.
  diameters <- c(diameters, diameter(rt_net_party,directed=TRUE))
  #get_diameter(party_net,directed=TRUE)
  #farthest_vertices(party_net,directed=TRUE)
  #Clustering Coefficient
  # Transitivität/Anzahl der geschloßenen Dreieck
  # Wenn A und B und A und C Freunde sind dann auch B und C ?
  # Verhältnis aus allen geschloßenen Triangles
  # zu allen/offenen Dreiecken
  # Betrachtet nur ungerichtete Graphen
  transitivities <- c(transitivities, transitivity(rt_net_party))
  
  #Average path length
  average_path_lengths = c(average_path_lengths, mean_distance(rt_net_party,directed = TRUE))
  
  #K-Cores
  curr_Coreness <- coreness(rt_net_party)
  max_cores <- c(max_cores,head(sort(curr_Coreness, decreasing=TRUE), 1))
  curr_Coreness <- as.data.frame(curr_Coreness)
  mean_k_cores <- c(mean_k_cores, dplyr::summarise(curr_Coreness, avg=mean(curr_Coreness)))
  rm(curr_Coreness)
  
  rt_net_party_tbl<-as_tbl_graph(rt_net_party)
  
  rt_net_party_tbl<-rt_net_party_tbl%>%mutate(in_degree=centrality_degree( mode = "in"),
                                        out_degree=centrality_degree(mode = "out"),
                                        between=centrality_betweenness(),
                                        page_rank=centrality_pagerank())
  #Betrachten wir die Werte indem wir die Nodes in einem data.frame speichern
  rt_nodes_party<-as.data.frame(rt_net_party_tbl%>%activate(nodes))
  
  party_colors<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")
  
  #Plot des Graphen mit ggraph
  ggraph(rt_net_party_tbl,layout = "graphopt")+geom_edge_link(aes(color="#6f6f6f"))+
    geom_node_point(aes(size=in_degree, color=as.factor(party)), show.legend = TRUE)+
    scale_color_manual(values = party_colors)+
    #geom_node_text(aes(label=name))+
    theme_graph()
}
mean_k_cores <- unlist(mean_k_cores)

rt_data_frame <- data.frame(parties, densities, components, diameters, transitivities, average_path_lengths, max_cores, mean_k_cores)
rt_data_frame <- rt_data_frame%>%remove_rownames%>%column_to_rownames(var = "parties")

#rm(rt_edges_party, rt_nodes_party, rt_net_party, rt_net_party_tbl)
#rm(parties, densities, components, diameters, transitivities, average_path_lengths,  max_cores, mean_k_cores, i)

