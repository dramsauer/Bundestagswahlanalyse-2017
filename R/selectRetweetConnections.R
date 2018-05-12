require("tidyverse")
rt_summary <- rt_data%>%count(party.y, party.x)