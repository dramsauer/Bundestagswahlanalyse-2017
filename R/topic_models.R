#install.packages("tidytext")
#install.packages("topicmodels")
#install.packages("tm")

require("tidyverse")
require("tidytext")
require("topicmodels")
require("tm")

#Wir laden die Funktion um sie in mutate zu verwenden
source("cleanTweetText.R")
#Wir lesen die Stopwortliste ein
stopwords <- read_csv("stopwords.txt", col_names = FALSE)

prepare_for_LDA<-twitter_data%>%filter(tokenCount>2)%>%
  #author topic model / wir nehmen alle Tweets eines Autors 
  group_by(twitter.handle)%>%
  summarise(newDoc=paste0(text,collapse=" "))%>%
  ungroup()%>%mutate(newDoc=cleanTweetText(newDoc))%>%filter(str_count(newDoc)>3)

prepare_for_LDA_tokens<-prepare_for_LDA%>%
          unnest_tokens(input=newDoc,output=tokens,
                token=stringr::str_split,pattern=" ")                    

# Lösche GERMAN Stopwords Lösche ENGLISH Stopwords
prepare_for_LDA_tokens<-prepare_for_LDA_tokens%>%
                anti_join(stopwords,by=c("tokens"="X1"))

prepare_for_LDA_tokens <- prepare_for_LDA_tokens%>%filter(tokens != "")

#Wir zählen alle Terme in den Dokumenten
prepare_for_LDA_tokens<-prepare_for_LDA_tokens%>%
                          count(twitter.handle,tokens,sort=TRUE)

#Sortiere wenig benutze Terme raus
prepare_for_LDA_tokens <- prepare_for_LDA_tokens%>%filter(n > 10)

# Wir konvertieren unseren tidy df in ein docterm-objekt
tokens_tm<-prepare_for_LDA_tokens%>%
              cast_dtm(twitter.handle,tokens,n)

#Wir betrachten das DT-Objekt
tokens_tm

gc()
#Wir trainieren unser Topic Model mit 50 topics und VEM
tweets_topic_model<-LDA(tokens_tm,method = "Gibbs",k=50,control = list(seed = 1234))


#Wir konvertieren das LDA-Objekt zurück via tidy
tidy_tweets_topic_model<-tidy(tweets_topic_model)

#Wir holen uns die top 5 Terme 
#(= am wahrscheinlichsten von diesem Topic) eines jeden Topics
top_terms_tweets_topic_model<-tidy_tweets_topic_model%>%
                                group_by(topic)%>%
                                  top_n(5, beta)%>%
                                    ungroup()%>%
                                  arrange(topic, -beta)



lda_gamma <- tidy(tweets_topic_model, matrix = "gamma")
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))


plot_topics<-top_terms_tweets_topic_model%>%
  mutate(term=reorder(term,beta))%>%
  group_by(topic,term)%>%
  arrange(desc(beta))%>%ungroup%>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__"))))


ggplot(data=plot_topics, mapping=aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 5, scales = "free")


#https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
#Welche Anzahl an Topics ist optimal?


require(ldatuning)

result <- FindTopicsNumber(
  tokens_tm,
  topics = seq(from = 25, to = 50, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 3L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)




