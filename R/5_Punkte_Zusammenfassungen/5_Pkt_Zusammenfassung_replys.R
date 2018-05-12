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

density <- c()
components <- c()
diameter <- c()
transitivity <- c()
average_path_length <- c()
max_cores = c()
mean_k_cores = c()


#Selektiere alle Replys
twitter_replys <- twitter_data%>%filter(!inReplyToUser == "null")%>%select(twitter.handle, party, userID, positveSentimentScore, negativeSentimentScore, inReplyToUser)%>%mutate(senti_value = negativeSentimentScore + positveSentimentScore)

#Selektiere alle Politiker
twitter_users <- dplyr::distinct(twitter_data%>%select(twitter.handle, party, userID))

#Finde passende Politiker zu den Replys
twitter_replys_between_politi <- left_join(twitter_replys, twitter_users, by = c("inReplyToUser" = "userID"))

#Filtere Replys welche zu keinem anderen Politker gehören raus
twitter_replys_between_politi <- twitter_replys_between_politi%>%filter(!twitter.handle.y == "NA")

#Übersichtstabelle (Edge hin zu Politiker auf den geantwortet wird x->y)
reply_summary_tbl <- twitter_replys_between_politi%>%count(party.x, party.y)

reply_summary_tbl_tidy <- twitter_replys_between_politi%>%count(party.x, party.y)%>%spread(party.y,n)
reply_summary_tbl_tidy <- reply_summary_tbl_tidy%>%gather(party.y, n, AfD:CDU:CSU:`DIE LINKE`:FDP:GRUENE:SPD)
reply_summary_tbl_tidy[is.na(reply_summary_tbl_tidy)]  <- 0
reply_summary_tbl_tidy <- reply_summary_tbl_tidy %>% group_by(party.y) %>% mutate(fraq=n/sum(n))

reply_heatmap <- ggplot(reply_summary_tbl_tidy, aes(party.x, party.y, fill=fraq))+
  geom_tile()+
  geom_text(aes(label = format(round(fraq,3), nsmall = 3)), colour= "white")+
  labs(x="Antwort", y="Ausgangstweet", fill = "% Replys")+
  theme_bw(base_size = 14)

#Eine Kante entsteht in die Richtung des Politikers auf den geantwortet wurde z.B Spaehn antwortet Schulz => Spaehn -> Schulz    
reply_edges <- twitter_replys_between_politi%>%group_by(twitter.handle.y, twitter.handle.x)%>%mutate("amount" =1)
reply_edges <- reply_edges%>%summarise(amount=sum(amount))
reply_net <- graph_from_data_frame(d=reply_edges, twitter_users, directed = TRUE)

#Remove loops (e.g. self reply)
reply_net <- simplify(reply_net, remove.multiple = FALSE)

#Remove isolated nodes
reply_net <- delete_vertices(reply_net,degree(reply_net)==0)


#Size/Größe
# Density
# L/k*(k-1) L=#observedTies k=#ofNods
# Je näher der Wert bei 1 ist desto stärker sind die Knoten miteinander verbunden
# Man kann es per Hand ausrechnen
length(E(reply_net))/(length(V(reply_net))*(length(V(reply_net))-1))
#Man kann aber auch die igraph-Funktion nutzen
density <- edge_density(reply_net)
#Components
# Aus wievielen Einzelteilen ist das Netzwerk aufgebaut
comp <- components(reply_net)
components <- c(components, length(comp$membership))
#Diameter/Geodesic
# Nur für Netzwerke mit einer Komponente
# Der längste aller kürzesten Pfade (zwischen Knoten A und B)
# von allen Knotenpaaren.
diameter <- diameter(reply_net,directed=TRUE)
get_diameter(reply_net,directed=TRUE)
farthest_vertices(reply_net,directed=TRUE)
#Clustering Coefficient
# Transitivität/Anzahl der geschloßenen Dreieck
# Wenn A und B und A und C Freunde sind dann auch B und C ?
# Verhältnis aus allen geschloßenen Triangles
# zu allen/offenen Dreiecken
# Betrachtet nur ungerichtete Graphen
transitivity <-transitivity(reply_net)

#K-Cores
curr_Coreness <- coreness(reply_net)
max_cores <- c(max_cores,head(sort(curr_Coreness, decreasing=TRUE), 1))
curr_Coreness <- as.data.frame(curr_Coreness)
mean_k_cores <- c(mean_k_cores, dplyr::summarise(curr_Coreness, avg=mean(curr_Coreness)))
rm(curr_Coreness)

#Average path length
average_path_length <- mean_distance(reply_net,directed = TRUE)

reply_net_tbl<-as_tbl_graph(reply_net)

reply_net_tbl<-reply_net_tbl%>%mutate(in_degree=centrality_degree( mode = "in"),
                                      out_degree=centrality_degree(mode = "out"),
                                      between=centrality_betweenness(),
                                      page_rank=centrality_pagerank())
#Betrachten wir die Werte indem wir die Nodes in einem data.frame speichern
twitter_users<-as.data.frame(reply_net_tbl%>%activate(nodes))

party_colors<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")

#Plot des Graphen mit ggraph
ggraph(reply_net_tbl,layout = "graphopt")+geom_edge_link(aes(color="#6f6f6f"))+
  geom_node_point(aes(size=in_degree, color=as.factor(party)), show.legend = TRUE)+
  scale_color_manual(values = party_colors)+
  #geom_node_text(aes(label=name))+
  theme_graph()


mean_k_cores <- unlist(mean_k_cores)

reply_data_frame_total <- data.frame(parties = "All Parties", densities=density, components, diameters=diameter, 
                                     transitivities=transitivity, average_path_lengths=average_path_length, max_cores, mean_k_cores)
reply_data_frame_total <- reply_data_frame_total%>%remove_rownames%>%column_to_rownames(var = "parties")

#rm(density, components, diameter, transitivity, average_path_length,  max_cores, mean_k_core, comp)
#rm(reply_net_tbl, reply_net, reply_edges, twitter_users)
