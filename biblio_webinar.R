## ===========================================================================
## Webinar: An introduction to bibliometric analysis using R
## Author: Tengku Hanis
## Date: 24-09-2020
## ===========================================================================

## Packages
library(bibliometrix)
library(tidyverse)

## See  tags
data("bibtag"); View(bibtag)

## Data
link <- "https://raw.githubusercontent.com/tengku-hanis/
webinar_biblio24-09-2020/master/scopus_acanthoma.bib"
dat <- convert2df(file = link, dbsource = "scopus", format = "bibtex")
names(dat)
dim(dat)

## Duplicate
anyDuplicated(dat$DI) # can use DOI/pubmed id
dat[duplicated(dat$DI), "DI"]

## Descriptive
result <- biblioAnalysis(dat)
summary(result, k=10)
P <- plot(result, k=10)
P[1]
P[2]
P[3]
P[4]
P[5]

## Analysis of cited references
dat$CR[1] # separator is ;

# Frequently cited papers
cited_paper <- citations(dat, field = "article", sep = ";")
cbind(cited_paper$Cited[1:10])

# Frequently cited first authors
cited_author <- citations(dat, field = "author", sep = ";")
cbind(cited_author$Cited[1:10])

# Authors' dominance ranking 
# (no of first authored paper/no of multi-authored paper)
dominance(result, k=10) 

## Top-authors's productivity over time
authorProdOverTime(dat, k=10)

## Top institutions based on affiliation 
first_auth <- result$FirstAffiliation # first author affiliate
co_auth <- result$Affiliations # co-author affiliate

first_auth <- first_auth %>% table() %>% as.data.frame()
names(first_auth) <- c("institution", "no_author")

co_auth <- co_auth %>% as.data.frame()
names(co_auth) <- c("institution", "no_coAuthor")
  # Merge 
total_aff <- full_join(first_auth, co_auth, by = "institution")
dim(total_aff)

# Top institution based on first author affiliation
total_aff %>% select(-no_author) %>% arrange(desc(no_coAuthor)) %>% head(10)
# Top institution based on co-author affiliation
total_aff %>% select(-no_coAuthor) %>% arrange(desc(no_author)) %>% head(10)

##----------------------------------------------------------------------------
## Visualization @ network plot

# Country collaboration network
MT <- metaTagExtraction(dat, Field = "AU_CO", sep = ";")
net_country <- biblioNetwork(MT, analysis = "collaboration", 
                             network = "countries", sep = ";")

networkPlot(net_country, n = 30, Title = "Country Collaboration", 
            type = "auto", size=TRUE, remove.multiple=T,
            labelsize=0.7,cluster="none")

## Institution collaboration
net_insti <- biblioNetwork(MT, analysis = "collaboration", 
                           network = "universities", sep = ";")

networkPlot(net_insti, n = 30, Title = "Institution Collaboration", 
            type = "auto", size=TRUE, remove.multiple=T,
            labelsize=0.7,cluster="louvain")


## Thematic map
theme_map <- thematicMap(dat, field = "ID", n = nrow(dat), minfreq = 30,
                   stemming = FALSE, size = 0.5, n.labels=3, repel = T)
plot(theme_map$map)

##----------------------------------------------------------------------------
## Laws

## Lotka's law 
L <- lotka(result)
L
  # GOF of Lotka's law (r^2)
L$R2
  # P value of K-S two sample test
L$p.value # no sig diff between observed and theoretical distribution
  # Observed distribution
Observed <- L$AuthorProd[,3]
  # Theoretical distribution with Beta = 2
Theoretical <- 10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1], Theoretical, type = "l", col = "red", ylim = c(0, 1), 
     xlab = "Articles", ylab = "Freq. of Authors", 
     main = "Scientific Productivity")
lines(L$AuthorProd[,1], Observed, col = "blue")
legend(x = "topright", c("Theoretical (B=2)","Observed"),
       col = c("red","blue"), lty = c(1,1,1),cex = 0.6, bty = "n")  

# Bradford's law @ core journals
core_journal <- bradford(dat)
core_journal <- core_journal$table %>% as.data.frame()
zone_all <- core_journal %>% select(SO, Freq, Zone) %>% group_by(Zone) %>% 
  summarise(journal = length(SO), article = sum(Freq))
zone_all

core_journal %>% filter(Zone == "Zone 1") # zone 1 journals
