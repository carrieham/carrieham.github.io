options(repos = list(CRAN="http://cran.rstudio.com/"))
install.packages("pacman")
library(pacman)

p_load(bookdown,
       data.table,
       # DataTables,
       dplyr, 
       DT,
       htmlwidgets,
       forcats,
       plotly,
       quanteda,
       stopwords,
       tictoc,
       tidyverse, 
       tidytext,
       tokenizers,
       vroom)

load(verbose=TRUE,"data/enviroLaborSpeeches.Rda")

# enviroLaborSpeeches <- vroom("data/enviroLaborSpeeches.csv",
#                         show_col_types = FALSE) %>%
enviroLaborSpeeches <- enviroLaborSpeeches %>%
  as_tibble() %>%
  select(year,
         date,
         chamber,
         environment,
         labor,
         speech_id,
         speech,
         # last_name,
         # state,
         total_annual_speeches) #%>%
# filter(year>=1900) #%>%
# mutate(speech=str_to_lower(speech))


# enviroLaborSpeeches <- enviroLaborSpeeches %>%
#   filter(is.na(year)==FALSE)
# save(enviroLaborSpeeches,file="data/enviroLaborSpeeches.Rda")
# fwrite(enviroLaborSpeeches,"data/enviroLaborSpeeches.csv")


stopwords_iso <- stopwords("en", source = "stopwords-iso") 
stopwords_manual <- c("absent","adjourn", "ask", "can", "chairman",
                      "committee","con","democrat","etc","gentleladies",
                      "gentlelady", "gentleman", "gentlemen", "gentlewoman",
                      "gentlewomen","hereabout","hereafter","hereat", "hereby",
                      "herein", "hereinafter", "hereinbefore", "hereinto", "hereof",
                      "hereon", "hereto", "heretofore", "hereunder", "hereunto",
                      "hereupon", "herewith", "month", "mr", "mrs", "nai", "nay",
                      "none", "now", "part", "per", "pro", "republican",
                      "say", "senator", "shall", "sir", "speak", "speaker",
                      "tell", "thank", "thereabout", "thereafter", "thereagainst",
                      "thereat", "therebefore", "therebeforn", "thereby", "therefor",
                      "therefore", "therefrom", "therein", "thereinafter", "thereof",
                      "thereon", "thereto", "theretofore", "thereunder", "thereunto",
                      "thereupon", "therewith", "therewithal", "today", "whereabouts",
                      "whereafter", "whereas", "whereat", "whereby","wherefore",
                      "wherefrom", "wherein", "whereinto", "whereof", "whereon","whereto",
                      "whereunder", "whereupon", "wherever", "wherewith", 
                      "wherewithal", "will", "yea", "yes", "yield")

stopwords_manual2 <- c("white house","white houses",
                       "avenue","street","nw","northwest","suite",
                       "number","numeric","sequence","sequences",
                       "add","addition","additions","adding","added",
                       "amend","amended","amendment","amendments","amending","amends",
                       "america","american","americas",
                       "bill","bills",
                       "chapter","chapters",
                       "code","codes",
                       "country","countrys", 
                       "congress","congressional",
                       "district of columbia","district","districts",
                       "enact","enacted","enacting","enacts",
                       "federal", 
                       "senate","senates", 
                       "house of representatives","representative","representatives","the house",
                       "insert","inserting","inserted","inserts",
                       "law","laws",
                       "legislation","legislative","legislator",
                       "paragraph","paragraphs","sentence","sentences",
                       "percent","percentage","percentages",
                       "president","presidents","presidential", 
                       "print","printing","printed","prints",
                       "record","recording","recorded","records",
                       "section","sections",
                       "united states",
                       "state","states",
                       "subchapter","subchapters",
                       "subsection","subsections",
                       "supreme court","supreme courts","court","courts",
                       "title","titles",
                       "unanimous consent","consent","consenting",
                       "united","u.s.","us",
                       "urge","urging","urged","urges",
                       "colleague","colleagues",
                       "washington", "d.c.")

stopwords <- c(stopwords_manual2,stopwords_manual,stopwords_iso)


# ##### BIGRAMS #####
tic()
enviroLaborBigrams <- enviroLaborSpeeches %>%
  filter(environment=="Yes" & labor=="Yes") %>%
  corpus(docid_field="speech_id",
         text_field="speech") %>%
  tokenize_word_stems(stopwords=stopwords) %>%
  tokens(remove_punct=FALSE, #CREATE TOKEN OBJECT
         split_tags=TRUE,
         split_elisions=TRUE,
         remove_symbols=TRUE,
         remove_numbers=TRUE,
         remove_separators=TRUE,
         padding=FALSE,
         xptr=TRUE,
         verbose=TRUE) %>%
  tokens_remove("[:digit:]+(th|st|rd)", valuetype = "regex",
               padding=FALSE,
               verbose=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", #REMOVE PUNCT
                padding=TRUE, #REPLACE W/ EMPTY STRINGS
                verbose=TRUE) %>%
  # tokens_remove(stopwords, #REMOVE STOP WORDS BEFORE TRIPLES CREATED
  #               padding=FALSE,
  #               verbose=TRUE) %>%
  tokens_ngrams(n=2) %>% #CREATE BIGRAMS
  dfm() %>% #CREATE DOC-TERM MATRIX
  tidy() %>%
  mutate(speech_id=as.double(document)) %>%
  left_join(enviroLaborSpeeches %>% select(year,date,speech_id,environment,labor),by=join_by(speech_id==speech_id)) %>%
  as_tibble()
toc()
#
# ##### SAVE FILE
save(enviroLaborBigrams,file="data/R/enviroLaborBigrams.Rda")

enviroLaborBigrams_top25 <- enviroLaborBigrams %>%
  as_tibble() %>%
  count(term,wt=count) %>%
  top_n(25) %>%
  arrange(desc(n))
save(enviroLaborBigrams_top25,file="data/R/tables/enviroLaborBigrams_top25.Rda")

map_enviroLaborBigrams_top25 <- ggplot() +
  geom_col(data=enviroLaborBigrams_top25,
           aes(x=fct_reorder(term,n),
               y=n,
               text=paste("<b>",term,":</b> ",n,sep="")),
           #color="blueviolet",
           fill="darkslateblue") +
  coord_flip() +
  theme_minimal() +
  theme(axis.line=element_line(color="#32127a"),
        axis.ticks=element_line(color="#32127a"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


##### make interactive
ggplotly(map_enviroLaborBigrams_top25, tooltip="text") %>%
  style(hoverlabel=list(font=list(family="IBM Plex Mono",
                                  size=11))) %>%
  layout(font=list(color="#32127a",family="IBM Plex Mono"),
         xaxis=list(color="#32127a",
                    linecolor="#32127a",
                    tickfont=list(color="#32127a")),
         yaxis=list(color="#32127a",
                    linecolor="#32127a",
                    tickfont=list(color="#32127a"))) %>%
  partial_bundle() %>%
  saveWidget("figures/enviroLabor_bigrams_top25.html", selfcontained = F, libdir = "lib")


######ENVIRO
# ##### BIGRAMS #####
tic()
enviroBigrams <- enviroLaborSpeeches %>%
  filter(environment=="Yes") %>%
  corpus(docid_field="speech_id",
         text_field="speech") %>%
  tokenize_word_stems(stopwords=stopwords) %>%
  tokens(remove_punct=FALSE, #CREATE TOKEN OBJECT
         split_tags=TRUE,
         split_elisions=TRUE,
         remove_symbols=TRUE,
         remove_numbers=TRUE,
         remove_separators=TRUE,
         padding=FALSE,
         xptr=TRUE,
         verbose=TRUE) %>%
  tokens_remove("[:digit:]+(th|st|rd)", valuetype = "regex",
               padding=FALSE,
               verbose=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", #REMOVE PUNCT
                padding=TRUE, #REPLACE W/ EMPTY STRINGS
                verbose=TRUE) %>%
  # tokens_remove(stopwords, #REMOVE STOP WORDS BEFORE TRIPLES CREATED
  #               padding=FALSE,
  #               verbose=TRUE) %>%
  tokens_ngrams(n=2) %>% #CREATE BIGRAMS
  dfm() %>% #CREATE DOC-TERM MATRIX
  tidy() %>%
  mutate(speech_id=as.double(document)) %>%
  left_join(enviroLaborSpeeches %>% select(year,date,speech_id,environment,labor),by=join_by(speech_id==speech_id)) %>%
  as_tibble()
toc()
#
# ##### SAVE FILE
save(enviroBigrams,file="data/R/enviroBigrams.Rda")



enviroBigrams_top25 <- enviroBigrams %>%
  as_tibble() %>%
  count(term,wt=count) %>%
  top_n(25) %>%
  arrange(desc(n))
save(enviroBigrams_top25,file="data/R/tables/enviroBigrams_top25.Rda")

# load(verbose=TRUE,file="data/R/tables/enviroBigrams_top25.Rda")

map_enviroBigrams_top25 <- ggplot() +
  geom_col(data=enviroBigrams_top25,
           aes(x=fct_reorder(term,n),
               y=n,
               text=paste("<b>",term,":</b> ",n,sep="")),
           fill="aquamarine4") +
  coord_flip() +
  theme_minimal() +
  theme(axis.line=element_line(color="#32127a"),
        axis.ticks=element_line(color="#32127a"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



##### make interactive
ggplotly(map_enviroBigrams_top25, tooltip="text") %>%
  style(hoverlabel=list(font=list(family="IBM Plex Mono",
                                  size=11))) %>%
  layout(font=list(color="#32127a",family="IBM Plex Mono"),
         xaxis=list(color="#32127a",
                    linecolor="#32127a",
                    tickfont=list(color="#32127a")),
         yaxis=list(color="#32127a",
                    linecolor="#32127a",
                    tickfont=list(color="#32127a"))) %>%
  partial_bundle() %>%
  saveWidget("figures/enviro_bigrams_top25.html", selfcontained = F, libdir = "lib")


###### LABOR
laborBigrams <- enviroLaborSpeeches %>%
  filter(labor=="Yes") %>%
  corpus(docid_field="speech_id",
         text_field="speech") %>%
  tokenize_word_stems(stopwords=stopwords) %>%
  tokens(remove_punct=FALSE, #CREATE TOKEN OBJECT
         split_tags=TRUE,
         split_elisions=TRUE,
         remove_symbols=TRUE,
         remove_numbers=TRUE,
         remove_separators=TRUE,
         padding=FALSE,
         xptr=TRUE,
         verbose=TRUE) %>%
  tokens_remove("[:digit:]+(th|st|rd)", valuetype = "regex",
               padding=FALSE,
               verbose=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", #REMOVE PUNCT
                padding=TRUE, #REPLACE W/ EMPTY STRINGS
                verbose=TRUE) %>%
  # tokens_remove(stopwords, #REMOVE STOP WORDS BEFORE TRIPLES CREATED
  #               padding=FALSE,
  #               verbose=TRUE) %>%
  tokens_ngrams(n=2) %>% #CREATE BIGRAMS
  dfm() %>% #CREATE DOC-TERM MATRIX
  tidy() %>%
  mutate(speech_id=as.double(document)) %>%
  left_join(enviroLaborSpeeches %>% select(year,date,speech_id,environment,labor),by=join_by(speech_id==speech_id)) %>%
  as_tibble()
toc()
#
# ##### SAVE FILE
save(laborBigrams,file="data/R/laborBigrams.Rda")


laborBigrams_top25 <- laborBigrams %>%
  as_tibble() %>%
  count(term,wt=count) %>%
  top_n(25) %>%
  arrange(desc(n))
save(laborBigrams_top25,file="data/R/tables/laborBigrams_top25.Rda")

# load(verbose=TRUE,file="data/R/tables/laborBigrams_top25.Rda")

map_laborBigrams_top25 <- ggplot() +
  geom_col(data=laborBigrams_top25,
           aes(x=fct_reorder(term,n),
               y=n,
               text=paste("<b>",term,":</b> ",n,sep="")),
           #color="blueviolet",
           fill="blueviolet") +
  coord_flip() +
  theme_minimal() +
  theme(axis.line=element_line(color="#32127a"),
        axis.ticks=element_line(color="#32127a"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


##### make interactive
ggplotly(map_laborBigrams_top25, tooltip="text") %>%
  style(hoverlabel=list(font=list(family="IBM Plex Mono",
                                  size=11))) %>%
  layout(font=list(color="#32127a",family="IBM Plex Mono"),
         xaxis=list(color="#32127a",
                    linecolor="#32127a",
                    tickfont=list(color="#32127a")),
         yaxis=list(color="#32127a",
                    linecolor="#32127a",
                    tickfont=list(color="#32127a"))) %>%
  partial_bundle() %>%
  saveWidget("figures/labor_bigrams_top25.html", selfcontained = F, libdir = "lib")











###### TABLES
enviroLabor_bigrams_by_year <- enviroLaborBigrams %>%
  group_by(year) %>%
  count(term,wt=count) %>%
  top_n(10) %>%
  arrange(desc(year),desc(n))
save(enviroLabor_bigrams_by_year,file="data/R/tables/enviroLabor_bigrams_by_year.Rda")

enviro_bigrams_by_year <- enviroBigrams %>%
  group_by(year) %>%
  count(term,wt=count) %>%
  top_n(10) %>%
  arrange(desc(year),desc(n))
save(enviro_bigrams_by_year,file="data/R/tables/enviro_bigrams_by_year.Rda")

labor_bigrams_by_year <- laborBigrams %>%
  group_by(year) %>%
  count(term,wt=count) %>%
  top_n(10) %>%
  arrange(desc(year),desc(n))
save(labor_bigrams_by_year,file="data/R/tables/labor_bigrams_by_year.Rda")


