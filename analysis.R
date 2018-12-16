library(rentrez)
library(dplyr)
library(ggplot2)
library(pbapply)

httr::set_config(httr::config(http_version = 0))

search_year <- function(quotation, year){
  query <- paste(quotation, "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}

get_n_pubs <- function(quotation, year_range = 1990:2018) {
  n_pubs <- pbsapply(year_range, search_year, quotation = quotation)
  data.frame(year = year_range, n_pub = n_pubs)
}

hdx_df <- get_n_pubs("HDX-MS[TIAB] OR HDX Mass Spectrometry[TIAB] OR Hydrogen-Deuterium Exchange Mass Spectrometry[TIAB] OR Hydrogen-Deuterium Exchange MS[TIAB] OR Hydrogen/Deuterium Exchange Mass Spectrometry[TIAB] OR Hydrogen/Deuterium Exchange MS[TIAB]")

p_df <- get_n_pubs("proteomics[TIAB]")


rbind(mutate(p_df, query = "proteomics"),
      mutate(hdx_df, query = "HDX-MS")) %>% 
  ggplot(aes(x = year, y = n_pub, fill = query)) +
  geom_col() +
  geom_smooth(method = "loess") +
  scale_y_continuous("Number of publications") +
  scale_x_continuous(limits = c(1995, 2018)) +
  theme_bw() +
  facet_wrap(~ query, scales = "free_y") +
  theme(legend.position = "bottom")

pdf(file = "hdx-pub.pdf", height = 5, width = 6)
rbind(mutate(p_df, query = "proteomics"),
      mutate(hdx_df, query = "HDX-MS (and similar)")) %>% 
  group_by(query) %>% 
  mutate(n_pub = n_pub/sum(n_pub)) %>% 
  ggplot(aes(x = year, y = n_pub, fill = query)) +
  geom_col(position = "dodge") +
  #geom_smooth(method = "loess") +
  scale_x_continuous("Year", limits = c(2000, 2018)) +
  scale_y_continuous("Fraction of publications (PubMed query result)", labels = scales::percent) +
  scale_fill_manual("PubMed query", values = c("deepskyblue", "tomato1")) +
  theme_bw() +
  #facet_wrap(~ query, scales = "free_y") +
  theme(legend.position = "bottom")
dev.off()

rbind(mutate(p_df, query = "proteomics[TIAB]"),
      mutate(hdx_df, query = "HDX-MS[TIAB] OR HDX Mass Spectrometry[TIAB] OR Hydrogen-Deuterium Exchange Mass Spectrometry[TIAB] OR Hydrogen-Deuterium Exchange MS[TIAB] OR Hydrogen/Deuterium Exchange Mass Spectrometry[TIAB] OR Hydrogen/Deuterium Exchange MS[TIAB]")) %>% 
  group_by(query) %>% 
  summarise(total = sum(n_pub)) %>% 
  write.csv(file = "hdx-pub.csv", row.names = FALSE)
