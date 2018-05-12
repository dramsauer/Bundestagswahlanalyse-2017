require(tidyverse)
require(igraph)
require(tidygraph)
require(ggraph)
require(readr)
require(dplyr)
require(gridExtra)

hashtag_pattern <- "#([[:alnum:]]|[_])+"

stopwords <- read_csv("stopwords.txt", col_names = FALSE)

#Filtere alle Tweets ohne Hashtags raus und säubere den Text
#twitter_data_clean <- twitter_data%>%filter(hashtagCount != 0)%>%select(ID, text, twitter.handle, party)%>%mutate(text = cleanTweetText(text))

#Filtere alle Hashtags aus den Tweets
hashtag_occurences <- twitter_data%>%select(text,twitter.handle, party)%>%
  mutate(hashtags=str_extract_all(text,hashtag_pattern))%>%select(twitter.handle, party, hashtags)

#Filtere alle leeren Hashtags raus
hashtag_occurences <- hashtag_occurences%>%filter(hashtags != "character(0)")

#Zähle Occurences
hashtag_occurences <- hashtag_occurences%>%mutate(hashtags=cleanHashtags(hashtags))%>%
  group_by(hashtags)%>%summarise(hashtagsFreqLog=n())%>%
  ungroup

#Nur auftreten von mindestens zwei Hashtags
hashtag_co_occurences <- hashtag_occurences%>%filter(grepl(",", hashtags))

#Splitte alle Hashtags in einzelne Spalten
hashtag_edges <- hashtag_co_occurences%>%separate(hashtags, c( "node" ,"edge1", "edge2", "edge3", "edge4", "edge5", "edge6", "edge7", "edge8", "edge9", "edge10", "edge11", "edge12", "edge13"), sep = ",")

#Tidy mit regex ToDo: kombiniere alles in ein Regexstatement
hashtag_edges <- hashtag_edges%>%mutate_all(funs(gsub("c\\(", "", .)))%>%mutate_all(funs(gsub("\\)", "", .)))%>%mutate_all(funs(gsub("\"", "", .)))
 # mutate_all(funs(gsub("btw2017", "btw17",.)))

#Gruppiere Kanten abhängig von deren Ausgangspunkt
hashtag_edges <- hashtag_edges%>%group_by(node)%>%gather(names(node), edge, edge1:edge13, na.rm = TRUE)%>%select(node, edge, hashtagsFreqLog)

#Entferne leere Knoten oder Kanten
hashtag_edges <- hashtag_edges%>%filter(node != "")%>%filter(edge != " ")%>%rename(amount = hashtagsFreqLog)

#Zähle doppelte Rows zu einer zusammen
hashtag_edges <- hashtag_edges%>%mutate_at("amount", funs(as.numeric(.)))%>%group_by(node, edge)%>%summarise(amount = sum(amount))

hashtag_edges <- hashtag_edges%>%anti_join(stopwords, by = c("node" = "X1"))

hashtag_edges<-hashtag_edges%>%mutate_at("edge", funs(gsub(" ", "", .)))

hashtag_edges <- hashtag_edges%>%anti_join(stopwords, by = c("edge" = "X1"))

hashtag_net <- graph_from_data_frame(d=hashtag_edges, directed = FALSE)


## Community structure
hashtag_com <- cluster_louvain(hashtag_net)


hashtag_com$degree <- (degree(hashtag_net)[hashtag_com$names])

hashtag_com$cluster <- unname(ave(hashtag_com$degree, hashtag_com$membership, 
                            FUN=function(x)names(x)[which.max(x)])
)

V(hashtag_net)$name <- hashtag_com$cluster

E(hashtag_net)$weight <- 1
V(hashtag_net)$weight <- 1
community_graph <- contract.vertices(hashtag_net, hashtag_com$membership, 
                          vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))

community <- data.frame(hashtag_com$names, hashtag_com$cluster, hashtag_com$degree)

# Simplify edges
community_graph <- simplify(community_graph, edge.attr.comb = list(weight = "sum", function(x)length(x)))

community_subgraph <- induced.subgraph(community_graph, V(community_graph)$weight > 650)
V(community_subgraph)$degree <- unname(degree(community_subgraph))

cum_graph_tbl <- as.data.frame(as_tbl_graph(community_subgraph))

cl <- colors(distinct = TRUE)
set.seed(11111)# to set random generator seed
mycols <- sample(cl, 1)


ggraph(community_subgraph, layout = "grid")+
  #geom_edge_link(aes(color="#6f6f6f"))+
  geom_node_point(aes(size = V(community_subgraph)$weight, color = mycols), show.legend = FALSE )+
geom_node_text(aes(label=name), nudge_y = -0.2)+
theme_graph()

curr_community <- community%>%filter(hashtag_com.cluster == "g20")%>%top_n(6, hashtag_com.degree)

community_subgraph_df <- as.data.frame(as_tbl_graph(community_subgraph))

topHashtags = community%>%group_by(hashtag_com.cluster)%>%top_n(6, hashtag_com.degree)

community_subgraph_df <- community_subgraph_df%>%left_join(topHashtags, by= c("name" = "hashtag_com.cluster"))

community_subgraph_df <- community_subgraph_df%>%rename(topHashtags = hashtag_com.names)

community_subgraph_df <- community_subgraph_df%>%group_by(name)%>%summarise_at(topHashtags, funs(list(.)))

#ToDo: Add top 5 hashtags to each cluster
#grid.table(community_subgraph_df%>%arrange(desc(weight))%>%select(name))