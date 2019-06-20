#===============================================================================
# This scrupt performs blast of Saeed's selected proteins against a reference
# database. The correct databse is not yet identified
# 
# Tyler Bradley 
# 2019-06-17
#===============================================================================


library(rBLAST)
library(tidyverse)
library(ShortRead)
library(Biostrings)

prot_locs <- list.files("saeed-request/prot-seqs/", full.names = TRUE)
prot_seqs <- readAAStringSet(prot_locs)

download.file("ftp://ftp.ncbi.nlm.nih.gov/blast/db/refseq_protein.52.tar.gz",
              "saeed-request/refseq_protein.52.tar.gz", mode='wb')
untar("saeed-request/refseq_protein.52.tar.gz", exdir = "saeed-request/refseq_protein")

bl <- blast(db = "saeed-request/refseq_protein/refseq_protein.52", type = "blastp")

cl <- predict(bl, prot_seqs)
