library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)

search_lepoint = function(search, page_max) 
{
  for (i in 1:page_max) 
  {
    html <- "http://www.lepoint.fr/recherche/index.php?sort=date_desc&query=" %>%
    str_c(search,"&page=",i) %>%
    read_html()
    
    result <- result %>%
    bind_rows(bind_cols(html_nodes(html, css = "div.col.pls") %>%
                          html_nodes(css = "a") %>%
                          html_text() %>%
                          str_trim() %>%
                          str_replace_all(pattern = "[\\s+|>]",replacement = " ") %>%
                          str_replace_all(pattern = "[\\s+]",replacement = " ") %>%
                          tibble(text = .)%>%
                          filter(str_count(text) > 30), # On enlève ceux qui ont peu de caractères => erreurs
                        html_nodes(html, css = "span.art-author.inbl.vm") %>%
                          html_text() %>% # Origine
                          tibble(source = .),
                        html_nodes(html, css = "span.search-breadcrumb.inbl.vm") %>%
                          html_text() %>%
                          str_replace_all(pattern = ">", replacement = "") %>%
                          tibble(sujet = .),
                        html_nodes(html, css = "footer.search-footer.mtt") %>%
                          html_text() %>%
                          str_replace_all(pattern = "\\s+","") %>%
                          tibble(value = .) %>%
                          separate(value,into = c("Date","Heure","Auteur"), sep = "-")))
  }
}

