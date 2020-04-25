# Run this file second
# How votingworks: https://www.njleg.state.nj.us/legislativepub/Rules/SenRules.pdf


seq(2010,2018, by = 2 ) %>% walk(.f=function(YEAR){
  
  p <- Votes_all_years %>% 
    filter(Y <= 80, A <= 80, N <= 80) %>% 
    distinct () %>% 
    filter( Year == YEAR) %>% 
    ggplot(aes(x = Y, y = N, color = Bucco, shape = Bucco)) + 
    geom_jitter(aes(text = Bill_Synopsis)) +
    labs(x = "Yes Votes", y = "No Votes") + 
    ggtitle(paste0("NJ Assembly Voting Record for Session ", YEAR, "-", YEAR +1)) + 
    theme_bw() + 
    theme(legend.position = "bottom")
  
  ggp <- ggplotly(p, tooltip = "Bill_Synopsis") 
  htmlwidgets::saveWidget(ggp, paste0("Widget_",YEAR,".html"))
  
})

