# install.packages("tidyverse")
# install.packages("igraph")
# install.packages("tidygraph")
# install.packages("ggraph")
# install.packages("readr")
# install.packages("ineq")
require(tidyverse)
require(igraph)
require(tidygraph)
require(ggraph)
require(stringr)
require(readr)
require(ineq)
require(gridExtra)
require(csv)

#######
#Vor dem Ausführen dieser Datei sollten zuerst alle anderen Scripte ausgeführt werden.
#######

rt_summary <- rbind(rt_data_frame, rt_data_frame_total)
rt_summary_grid <-tableGrob(rt_summary)
grid.arrange(rt_summary_grid)
write.csv(rt_summary, "rt_summary.csv")



reply_summary <- rbind(reply_data_frame, reply_data_frame_total)
reply_summary_grid <-tableGrob(reply_summary)
grid.arrange(reply_summary_grid)
write.csv(reply_summary, "reply_summary.csv")

