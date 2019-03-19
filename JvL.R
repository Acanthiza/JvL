

  library("tidyverse")
  
  dat <- read_csv("pred.csv", col_names = FALSE) %>%
    setNames(paste0("pred",1:ncol(.))) %>%
    tidyr::gather(prediction,team,pred1:ncol(.)) %>%
    dplyr::mutate(rank = parse_number(team)
                  , team = gsub("\\d+","",team)
                  , team = gsub("\\. ","",team)
                  , team = gsub("\\.","",team)
                  , team = gsub(" $","",team)
                  , team = gsub("^ ","",team)
                  , team = gsub("GWS","Greater Western Sydney",team)
                  , team = gsub("[^[:alnum:][:blank:]?&/\\-]", "",team)
                  , team = gsub("U00..", "",team)
                  , team = fct_reorder(team,rank,.fun=sum)
                  )
  
  ggplot(dat, aes(fct_rev(team),rank)) +
    geom_boxplot() +
    coord_flip() +
    scale_colour_viridis_d()
    
  