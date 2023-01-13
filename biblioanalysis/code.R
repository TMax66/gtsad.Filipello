library(tidyverse)
library(bibliometrix)
library(here)

biblio <- here("biblioanalysis", c("b1.bib", "b2.bib","b3.bib","b4.bib"))
biblio <- convert2df(biblio, dbsource = "wos", format = "bibtex")

