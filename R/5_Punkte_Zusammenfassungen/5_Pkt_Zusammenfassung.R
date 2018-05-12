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

density <- c()
components <- c()
diameter <- c()
transitivity <- c()
average_path_length <- c()
max_cores = c()
mean_k_cores = c()

rt_summary_tbl <- rt_data%>%count(party.y, party.x)

rt_summary_tbl_tidy <- rt_data%>%count(party.y, party.x)%>%spread(party.x,n)
rt_summary_tbl_tidy <- rt_summary_tbl_tidy%>%gather(party.x, n, AfD:CDU:CSU:`DIE LINKE`:FDP:GRUENE:SPD)
rt_summary_tbl_tidy[is.na(rt_summary_tbl_tidy)]  <- 0
rt_summary_tbl_tidy <- rt_summary_tbl_tidy %>% group_by(party.x) %>% mutate(fraq=n/sum(n))

#Generiere Heatmap
ggplot(rt_summary_tbl_tidy, aes(party.x, party.y, fill=fraq))+
  geom_tile()+
  geom_text(aes(label = format(round(fraq,3), nsmall = 3)), colour= "white")+
  labs(x="Retweet", y="Ausgangstweet", fill = "% Tweets")+
  theme_bw(base_size = 14)

#Filtere Tweets raus die sich selbst retweeten    
rt_data_filtered <-rt_data%>%filter(twitter.handle.x != twitter.handle.y)

#Baue Kanten zwischen den Nodes auf
rt_edges <- rt_data_filtered%>%group_by(twitter.handle.x, twitter.handle.y) %>% mutate("amount" = 1)
rt_edges <- rt_edges%>%summarise(amount=sum(amount))
rt_nodes <- dplyr::distinct(twitter_data%>%select(twitter.handle, party))

rt_net <- graph_from_data_frame(d=rt_edges, rt_nodes, directed = TRUE)

#Entferne Self-Retweets
rt_net <- simplify(rt_net, remove.multiple = FALSE)

#Entferne isolierte Nodes
rt_net <- delete_vertices(rt_net,degree(rt_net)==0)

#Size/Größe
# Density
# L/k*(k-1) L=#observedTies k=#ofNods
# Je näher der Wert bei 1 ist desto stärker sind die Knoten miteinander verbunden
# Man kann es per Hand ausrechnen
density <- length(E(rt_net))/(length(V(rt_net))*(length(V(rt_net))-1))
#Man kann aber auch die igraph-Funktion nutzen
#densitiy <- edge_density(rt_net)
#Components
# Aus wievielen Einzelteilen ist das Netzwerk aufgebaut
comp <- components(rt_net)
components <- c(components, length(comp$membership))

#Diameter/Geodesic
# Nur für Netzwerke mit einer Komponente
# Der längste aller kürzesten Pfade (zwischen Knoten A und B)
# von allen Knotenpaaren.
diameter <- diameter(rt_net,directed=TRUE)
get_diameter(rt_net,directed=TRUE)
farthest_vertices(rt_net,directed=TRUE)
#Clustering Coefficient
# Transitivität/Anzahl der geschloßenen Dreieck
# Wenn A und B und A und C Freunde sind dann auch B und C ?
# Verhältnis aus allen geschloßenen Triangles
# zu allen/offenen Dreiecken
# Betrachtet nur ungerichtete Graphen
transitivity <- transitivity(rt_net)

#Average path length
average_path_length <- mean_distance(rt_net,directed = TRUE)


#K-Cores
curr_Coreness <- coreness(rt_net)
max_cores <- c(max_cores,head(sort(curr_Coreness, decreasing=TRUE), 1))
curr_Coreness <- as.data.frame(curr_Coreness)
mean_k_cores <- c(mean_k_cores, dplyr::summarise(curr_Coreness, avg=mean(curr_Coreness)))
rm(curr_Coreness)

rt_net_tbl<-as_tbl_graph(rt_net)

rt_net_tbl<-rt_net_tbl%>%mutate(in_degree=centrality_degree( mode = "in"),
                                out_degree=centrality_degree(mode = "out"),
                                between=centrality_betweenness(),
                                page_rank=centrality_pagerank())
#Betrachten wir die Werte indem wir die Nodes in einem data.frame speichern
rt_nodes<-as.data.frame(rt_net_tbl%>%activate(nodes))

party_colors<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")

#Plot des Graphen mit ggraph
ggraph(rt_net_tbl,layout = "graphopt")+geom_edge_link(aes(color="#6f6f6f"))+
  geom_node_point(aes(size=in_degree, color=as.factor(party)), show.legend = TRUE)+
  scale_color_manual(values = party_colors)+
  #geom_node_text(aes(label=name))+
  theme_graph()

mean_k_cores <- unlist(mean_k_cores)

rt_data_frame_total <- data.frame(parties = "All Parties", densities=density, components, diameters=diameter, 
                                  transitivities=transitivity, average_path_lengths=average_path_length, max_cores, mean_k_cores)
rt_data_frame_total <- rt_data_frame_total%>%remove_rownames%>%column_to_rownames(var = "parties")
#rm(density, components, diameter, transitivity, average_path_length, k_core, comp)
#rm(rt_edges, rt_nodes, rt_net, rt_net_tbl)
