#===============================================================================
# This script will find all of the taxonomies related to each of the genes in the
# NCBI database
# 
# Tyler Bradley 
# 2019-05-08
#===============================================================================

library(tidyverse)
library(rentrez)
library(XML)

# nitro_red_search <- entrez_search("nucleotide", term = "nitroreductase", 
#                                   use_history = TRUE, retmax = 99999)
# 
# nitro_red_tax <- entrez_link("nucleotide", 
#                              # web_history = nitro_red_search$web_history,
#                              # web_history = "NCID_1_51352919_130.14.18.97_9001_1557336360_197551744_0MetA0_S_MegaStore",
#                              id = nitro_red_search$ids[1:300],
#                              db = "taxonomy")
# 
# 
# nitro_tax <- entrez_fetch("taxonomy", 
#                           id = nitro_red_tax$links$nuccore_taxonomy, 
#                           rettype = "xml")
# 
# nitro_red <- entrez_fetch(db = "nucleotide", web_history = nitro_red_search$web_history, 
#                           rettype = "fasta")
# 
# 
# tmp <- entrez_search("nucleotide", term = "nitroreductase", 
#                      retmax = 100, retstart = 805385)



genes_tbl <- tribble(
  ~name, ~records,
  "nitroreductase", 805384,
  "CODH", 18228,
  "Hydrogenase", 433329,
  "hydroxylating", 287404,
  "OxyR", 124301
)

gene_species <- genes_tbl %>% 
  # filter(name == "nitroreductase") %>%
  pmap_dfr(~{
    # browser()
    gene <- ..1
    n_records <- ..2
    
    rec_idx <- seq(1, n_records, by = 99998)
    
    nuc_ids <- map(rec_idx, ~{
      ent_search <- entrez_search("nucleotide", term = gene, 
                                  retmax = 99999, retstart = .x)
      
      return(ent_search$ids)
    }) %>% 
      flatten() %>% 
      unique()
    
    tax_idx <- seq(1, length(nuc_ids), by = 300)
    
    out <- map_dfr(tax_idx, ~{
      ent_links <- entrez_link("nucleotide", 
                               id = nuc_ids[.x:(.x+299)],
                               db = "taxonomy")
      
      ent_tax <- entrez_fetch(db = "taxonomy", 
                              id = ent_links$links$nuccore_taxonomy, 
                              rettype = "xml")
      
      tax_parse <- ent_tax %>% 
        xmlParse() %>% 
        xmlToDataFrame() %>% 
        as_tibble()
      
      return(tax_parse)
    })
    
    out <- out %>% 
      mutate(gene_name = gene)
    
    return(out)
  })



write_rds(gene_species, "saeed-request/gene-tax-relations.rds", 
          compress = "xz", compression = 9L)