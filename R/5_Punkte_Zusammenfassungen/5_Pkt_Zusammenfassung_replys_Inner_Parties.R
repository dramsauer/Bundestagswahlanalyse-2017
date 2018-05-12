# install.packages(tidyverse)
# install.packages(igraph)
# install.packages(tidygraph)
# install.packages("ggraph")
# install.packages(readr)

require(tidyverse)
require(igraph)
require(tidygraph)
require(ggraph)
require(readr)

densities <- c()
components <- c()
diameters <- c()
transitivities <- c()
average_path_lengths <- c()
max_cores = c()
mean_k_cores = c()


parties=c("AfD", "CDU", "CSU", "SPD", "FDP", "GRUENE", "DIE LINKE")  
for(i in parties){
  
  #Selektiere alle Replys
  twitter_replys_party <- twitter_data%>%
    filter(party==i)%>%
    filter(!inReplyToUser == "null")%>%select(twitter.handle, party, userID, positveSentimentScore, negativeSentimentScore, inReplyToUser)%>%mutate(senti_value = negativeSentimentScore + positveSentimentScore)
  
  #Selektiere alle Politiker der Partei i
  twitter_users_party <- dplyr::distinct(twitter_data%>%select(twitter.handle, party, userID))
  
  #Finde passende Politiker zu den Replys
  twitter_replys_between_politi_party <- left_join(twitter_replys_party, twitter_users_party, by = c("inReplyToUser" = "userID"))
  
  #Filtere Replys welche zu keinem anderen Politker gehören raus
  twitter_replys_between_politi_party <- twitter_replys_between_politi_party%>%filter(!twitter.handle.y == "NA")
  
  #Eine Kante entsteht in die Richtung des Politikers auf den geantwortet wurde z.B Spaehn antwortet Schulz => Spaehn -> Schulz    
  reply_edges_party <- twitter_replys_between_politi_party%>%group_by(twitter.handle.y, twitter.handle.x)%>%mutate("amount" =1)
  reply_edges_party <- reply_edges_party%>%summarise(amount=sum(amount))
  reply_net_party <- graph_from_data_frame(d=reply_edges_party, twitter_users_party, directed = TRUE)
  
  #Remove loops (e.g. self reply)
  reply_net_party <- simplify(reply_net_party, remove.multiple = FALSE)
  
  #Remove isolated nodes
  reply_net_party <- delete_vertices(reply_net_party,degree(reply_net_party)==0)
  
  
  #Size/Größe
  # Density
  # L/k*(k-1) L=#observedTies k=#ofNods
  # Je näher der Wert bei 1 ist desto stärker sind die Knoten miteinander verbunden
  # Man kann es per Hand ausrechnen
  #length(E(rt_net))/(length(V(reply_net))*(length(V(reply_net))-1))
  #Man kann aber auch die igraph-Funktion nutzen
  densities = c(densities, edge_density(reply_net_party))
  
  #Components
  # Aus wievielen Einzelteilen ist das Netzwerk aufgebaut
  comp = c(components(reply_net_party))
  components = c(components, length(comp$membership))
  
  #Diameter/Geodesic
  # Nur für Netzwerke mit einer Komponente
  # Der längste aller kürzesten Pfade (zwischen Knoten A und B)
  # von allen Knotenpaaren.
  diameters = c(diameters, diameter(reply_net_party,directed=TRUE))
  #get_diameter(reply_net,directed=TRUE)
  #farthest_vertices(reply_net,directed=TRUE)
  #Clustering Coefficient
  # Transitivität/Anzahl der geschloßenen Dreieck
  # Wenn A und B und A und C Freunde sind dann auch B und C ?
  # Verhältnis aus allen geschloßenen Triangles
  # zu allen/offenen Dreiecken
  # Betrachtet nur ungerichtete Graphen
  transitivities = c(transitivities, transitivity(reply_net_party))
  
  #K-Cores
  curr_Coreness <- coreness(reply_net_party)
  max_cores <- c(max_cores,head(sort(curr_Coreness, decreasing=TRUE), 1))
  curr_Coreness <- as.data.frame(curr_Coreness)
  mean_k_cores <- c(mean_k_cores, dplyr::summarise(curr_Coreness, avg=mean(curr_Coreness)))
  rm(curr_Coreness)
  
  #Average path length
  average_path_lengths = c(average_path_lengths, mean_distance(reply_net_party,directed = TRUE))
  
  reply_net_tbl_party<-as_tbl_graph(reply_net_party)
  
  reply_net_tbl_party<-reply_net_tbl_party%>%mutate(in_degree=centrality_degree( mode = "in"),
                                        out_degree=centrality_degree(mode = "out"),
                                        between=centrality_betweenness(),
                                        page_rank=centrality_pagerank())
  #Betrachten wir die Werte indem wir die Nodes in einem data.frame speichern
  twitter_users<-as.data.frame(reply_net_tbl_party%>%activate(nodes))
  
  party_colors<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")
  
  #Plot des Graphen mit ggraph
  ggraph(reply_net_tbl_party,layout = "graphopt")+geom_edge_link(aes(color="#6f6f6f"))+
    geom_node_point(aes(size=in_degree, color=as.factor(party)), show.legend = TRUE)+
    scale_color_manual(values = party_colors)+
    #geom_node_text(aes(label=name))+
    theme_graph()
}
mean_k_cores <- unlist(mean_k_cores)

reply_data_frame <- data.frame(parties, densities, components, diameters, transitivities, average_path_lengths, max_cores, mean_k_cores)
reply_data_frame <- reply_data_frame%>%remove_rownames%>%column_to_rownames(var = "parties")

#rm(reply_net_party, reply_net_tbl_party, reply_edges_party, twitter_replys_party, twitter_replys_between_politi_party)
#rm(parties, densities, components, diameters, transitivities, average_path_lengths, max_cores, mean_k_cores, i)


