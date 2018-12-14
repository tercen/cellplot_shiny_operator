# library(dplyr)
# 
# library(tidyr)
# 
# main.tbl = read.csv('DAVIDws.CLL.cellplot.FunctionalAnnotation.398ecca.tsv',
#                     header = TRUE, 
#                     sep = '\t',
#                     stringsAsFactors=FALSE) %>%
#   mutate(Rids=seq(from=0, to= nrow(.)-1), Genes = sapply(strsplit(Genes, ','), trimws)) 
# 
# join.tbl = main.tbl %>% 
#   select(Rids , Genes) %>% 
#   tidyr::unnest(Genes)
# 
# main.tbl = main.tbl %>% select(-Genes)
# 
# write.csv(main.tbl, file = "DAVIDws.CLL.cellplot.FunctionalAnnotation.398ecca.main.csv", row.names=FALSE)
# write.csv(join.tbl , file = "DAVIDws.CLL.cellplot.FunctionalAnnotation.398ecca.genes.join.csv", row.names=FALSE)