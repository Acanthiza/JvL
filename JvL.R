

  library("tidyverse")
  
  mung_data <- function(path) {
    
    read_csv(path) %>%
      setNames(paste0("pred",1:ncol(.))) %>%
      tidyr::gather(prediction,team,pred1:ncol(.)) %>%
      dplyr::group_by(prediction) %>%
      dplyr::mutate(rank = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(team = gsub("\\d+","",team)
                    , team = gsub("\\. ","",team)
                    , team = gsub("\\.","",team)
                    , team = gsub(" $","",team)
                    , team = gsub("^ ","",team)
                    , team = gsub("GWS","Greater Western Sydney",team)
                    , team = gsub("[^[:alnum:][:blank:]?&/\\-]", "",team)
                    , team = gsub("U00..", "",team)
                    )
    
  }
  
  
  dat <- tibble(path = dir(pattern = "\\d{4}\\.csv")) %>%
    dplyr::mutate(year = parse_number(path)) %>%
    dplyr::mutate(data = map(path,mung_data)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::group_by(year,team) %>%
    dplyr::mutate(yearMed = median(rank)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(yearMed = if_else(year == max(year),yearMed,0)
                  , team = fct_reorder(team,yearMed,.fun = mean)
                  )
  
  p <- ggplot(dat, aes(fct_rev(team),rank)) +
    geom_boxplot() +
    coord_flip() +
    scale_colour_viridis_d() +
    facet_wrap(~year)
  
  ggsave("JvL.png")
    
  