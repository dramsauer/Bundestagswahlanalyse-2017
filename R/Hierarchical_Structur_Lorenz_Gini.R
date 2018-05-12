# install.packages(tidyverse)
# install.packages(igraph)
# install.packages(tidygraph)
# install.packages(ggraph)
# install.packages(readr)
# install.packages(ineq)
require(tidyverse)
require(igraph)
require(tidygraph)
require(ggraph)
require(stringr)
require(readr)
require(ineq)
require(gridExtra)


#rt_nodes <- dplyr::distinct(rt_data%>%select(twitter.handle.y, party.y))


#################
# Computing
#################

# Producing FDP data.frame with Lorenz- and Gini-values as lorenz_df and gini_df
afd_distr <- rt_nodes %>% filter(party=="AfD") %>% select(name, in_degree)
afd_distr <-setNames(as.numeric(afd_distr$in_degree),afd_distr$name)
gini_df <- data.frame(Party="AfD",'Gini-Coeff'=ineq(afd_distr, type = "Gini"))
afd_distr <- Lc(afd_distr, n = rep(1,length(afd_distr)), plot =F)
p <- afd_distr[1]
L <- afd_distr[2]
lorenz_df <- data.frame(p,L,party="AfD")

# Producing SPD data.frame with Lorenz- and Gini-values appending to lorenz_df
spd_distr <- rt_nodes %>% filter(party=="SPD") %>% select(name, in_degree)
spd_distr <-setNames(as.numeric(spd_distr$in_degree),spd_distr$name)
gini_df <- rbind(gini_df, data.frame(Party="SPD",'Gini-Coeff'=ineq(spd_distr, type = "Gini")))
spd_distr <- Lc(spd_distr, n = rep(1,length(spd_distr)), plot =F)
p <- spd_distr[1]
L <- spd_distr[2]
lorenz_df <- rbind(lorenz_df, data.frame(p,L,party="SPD"))

# Producing FDP data.frame with Lorenz- and Gini-values appending to lorenz_df
fdp_distr <- rt_nodes %>% filter(party=="FDP") %>% select(name, in_degree)
fdp_distr <-setNames(as.numeric(fdp_distr$in_degree),fdp_distr$name)
gini_df <- rbind(gini_df, data.frame(Party="FDP",'Gini-Coeff'=ineq(fdp_distr, type = "Gini")))
fdp_distr <- Lc(fdp_distr, n = rep(1,length(fdp_distr)), plot =F)
p <- fdp_distr[1]
L <- fdp_distr[2]
lorenz_df <- rbind(lorenz_df, data.frame(p,L,party="FDP"))

# Producing CDU data.frame with Lorenz- and Gini-values appending to lorenz_df
cdu_distr <- rt_nodes %>% filter(party=="CDU") %>% select(name, in_degree)
cdu_distr <-setNames(as.numeric(cdu_distr$in_degree),cdu_distr$name)
gini_df <- rbind(gini_df, data.frame(Party="CDU",'Gini-Coeff'=ineq(cdu_distr, type = "Gini")))
cdu_distr <- Lc(cdu_distr, n = rep(1,length(cdu_distr)), plot =F)
p <- cdu_distr[1]
L <- cdu_distr[2]
lorenz_df <- rbind(lorenz_df, data.frame(p,L,party="CDU"))

# Producing CSU data.frame with Lorenz- and Gini-values appending to lorenz_df
csu_distr <- rt_nodes %>% filter(party=="CSU") %>% select(name, in_degree)
csu_distr <-setNames(as.numeric(csu_distr$in_degree),csu_distr$name)
gini_df <- rbind(gini_df, data.frame(Party="CSU",'Gini-Coeff'=ineq(csu_distr, type = "Gini")))
csu_distr <- Lc(csu_distr, n = rep(1,length(csu_distr)), plot =F)
p <- csu_distr[1]
L <- csu_distr[2]
lorenz_df <- rbind(lorenz_df, data.frame(p,L,party="CSU"))

# Producing GRUENE data.frame with Lorenz- and Gini-values appending to lorenz_df
gruene_distr <- rt_nodes %>% filter(party=="GRUENE") %>% select(name, in_degree)
gruene_distr <-setNames(as.numeric(gruene_distr$in_degree),gruene_distr$name)
gini_df <- rbind(gini_df, data.frame(Party="GRUENE",'Gini-Coeff'=ineq(gruene_distr, type = "Gini")))
gruene_distr <- Lc(gruene_distr, n = rep(1,length(gruene_distr)), plot =F)
p <- gruene_distr[1]
L <- gruene_distr[2]
lorenz_df <- rbind(lorenz_df, data.frame(p,L,party="GRUENE"))

# Producing 'DIE LINKE' data.frame with Lorenz- and Gini-values appending to lorenz_df
linke_distr <- rt_nodes %>% filter(party=="DIE LINKE") %>% select(name, in_degree)
linke_distr <-setNames(as.numeric(linke_distr$in_degree),linke_distr$name)
gini_df <- rbind(gini_df, data.frame(Party="DIE LINKE",'Gini-Coeff'=ineq(linke_distr, type = "Gini")))
linke_distr <- Lc(linke_distr, n = rep(1,length(linke_distr)), plot =F)
p <- linke_distr[1]
L <- linke_distr[2]
lorenz_df <- rbind(lorenz_df, data.frame(p,L,party="DIE LINKE"))


#Deleting of temporal-necessary data.frames
rm(p,L, afd_distr, spd_distr, fdp_distr, cdu_distr, csu_distr,gruene_distr, linke_distr)

#party_colors<-c(AfD="#1eaac2",CDU="#000000",CSU="#1804f6",`DIE LINKE`="#f402b9",FDP="#ffff4d",GRUENE="#3fba16",SPD="#f60410")


#str(lorenz_df)
#str(gini_df)



#################
# Plots
#################

# Showing table with Gini coefficients
gini_df <- dplyr::arrange(gini_df, desc(Gini.Coeff))
#Rename to german
gini_df <- gini_df%>%rename(Gini = Gini.Coeff, Partei = Party)
lorenz_df <- lorenz_df%>%rename(Partei = party)

gini_table <-tableGrob(gini_df)
grid.arrange(gini_table)

# Plotting Lorenz curves
lorenz_plot <-ggplot(data=lorenz_df)+
  #geom_point(aes(x=p, y=L, color=party)) +
  geom_line(aes(x=p, y=L, color=Partei)) +
  scale_color_manual(values = party_colors)+
  scale_x_continuous(name="Anteil an In-Degree", limits=c(0,1)) + 
  scale_y_continuous(name="Anteil an Nutzern", limits=c(0,1)) +
  geom_abline()+
  theme_bw(base_size = 14)
lorenz_plot

# Show a combining view of Lorenz curves and Gini coefficients
grid.arrange(lorenz_plot, gini_table)
