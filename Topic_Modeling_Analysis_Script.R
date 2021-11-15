mode="reviews" # toggle between reviews, evidence, gr
#############################################################################

      ### Data cleaning and preparation for analysis

#############################################################################

library(tidyverse)
library(tidytext)
library(topicmodels)
library(forcats)
library(tidytext)
library(tidyverse)
library(topicmodels)
library(ldatuning)
library(reshape2)
#load screened results of literature search
# filename = '/Users/chenyuyan/journal_access-main/review_text.csv
if (mode=="reviews"){
filename = './abstract/cleaned/review_abstract_cleaned.csv'
data = read.csv(filename)
data$all_text<-data$Abstract
ntopic=8
}
if (mode=="evidence"){
  filename = './abstract/cleaned/evidence_abstract_cleaned.csv'
  data = read.csv(filename)
  data$all_text<-data$Abstract
  ntopic=8
}

if (mode=="gr"){
  filename = './text/gr_canada_cleaned.csv'
  data = read.csv(filename)
  filename2 = './text/gr_usa_cleaned.csv'
  data2 = read.csv(filename2)
  colnames(data2)<-colnames(data)
  data=rbind(data, data2)
  ntopic=9
}


#create and remove custom stop words that do not contribute to 
#understanding distinct topics within the resilient protected
#area literature


custom_stop_words = tribble(
  ~word, ~lexicon,
  "et", "CUSTOM",
  "al", "CUSTOM", 
  "north", "CUSTOM",
  "america","CUSTOM",
  "2010", "CUSTOM",
  "2014", "CUSTOM",
  "2015", "CUSTOM",
  "2012", "CUSTOM",
  "2017", "CUSTOM",
  "2006", "CUSTOM",
  "2007", "CUSTOM",
  "2009", "CUSTOM",
  "2011", "CUSTOM",
  "2008", "CUSTOM",
  "2013", "CUSTOM",
  "2016", "CUSTOM",
  "2018", "CUSTOM",
  "2019", "CUSTOM",
  "2007", "CUSTOM",
  "1999", "CUSTOM",
  "2022", "CUSTOM",
  "1996", "CUSTOM",
  "24", "CUSTOM",
  "cm", "CUSTOM",
  "00e9", "CUSTOM",
  "mean", "CUSTOM",
  "years", "CUSTOM",
  "table", "CUSTOM",
  "sites", "CUSTOM",
  "using", "CUSTOM",
  "10", "CUSTOM",
  "trees", "CUSTOM",
  "forests", "CUSTOM",
  "study", "CUSTOM",
  "data", "CUSTOM",
  "fig", "CUSTOM",
  "figure", "CUSTOM",
  "doi", "CUSTOM",
  "doi.org", "CUSTOM",
  "plots", "CUSTOM",
  "https", "CUSTOM",
  "0", "CUSTOM",
  "1", "CUSTOM",
  "2", "CUSTOM",
  "3", "CUSTOM",
  "4", "CUSTOM",
  "5", "CUSTOM",
  "6", "CUSTOM",
  "7", "CUSTOM",
  "8", "CUSTOM",
  "9", "CUSTOM",
  # gr_canada
  "used", "CUSTOM", 
  "using", "CUSTOM", 
  "e.g", "CUSTOM",
  # gr_usa
  "201c", "CUSTOM",
  "201d", "CUSTOM",
  "00b0", "CUSTOM",
  #emma added
  "species", "CUSTOM",
  "tree", "CUSTOM",
  "stands", "CUSTOM",
  "management", "CUSTOM",
  "efforts", "CUSTOM",
  "results", "CUSTOM",
  "invasion", "CUSTOM",
  "due", "CUSTOM",
  "site", "CUSTOM",
  "rate", "CUSTOM",
  "nonnative", "CUSTOM",
  "significant", "CUSTOM",
  "health", "CUSTOM",
  "loss", "CUSTOM",
  "increased", "CUSTOM",
  "infested", "CUSTOM",
  "found", "CUSTOM",
  "understanding", "CUSTOM",
  "including", "CUSTOM",
  "compared", "CUSTOM",
  "lower", "CUSTOM",
  "rates", "CUSTOM",
  "infestation", "CUSTOM",
  "forest", "CUSTOM",
  "exotic", "CUSTOM",
  "invasive", "CUSTOM",
  "major", "CUSTOM",
  "control", "CUSTOM",
  "mortality", "CUSTOM",
  "findings", "CUSTOM",
  "introduced", "CUSTOM",
  "springer", "CUSTOM",
  "conducted", "CUSTOM",
  "alien", "CUSTOM",
  "stand", "CUSTOM",
  "estimate", "CUSTOM",
  "estimated", "CUSTOM",
  "affect", "CUSTOM",
  "significantly", "CUSTOM", 
  "low", "CUSTOM",
  "methods", "CUSTOM",
  "introduction", "CUSTOM",
  "authors", "CUSTOM",
  "information", "CUSTOM",
  "impact", "CUSTOM",
  "impacts", "CUSTOM",
  "assessed", "CUSTOM",
  "provide", "CUSTOM",
  "total", "CUSTOM",
  "effects", "CUSTOM",
  "affected", "CUSTOM",
  "causing", "CUSTOM",
  "spp", "CUSTOM",
  "evaluate", "CUSTOM",
  "multiple", "CUSTOM",
  #Emma reviews
  "by", "CUSTOM",
  "role", "CUSTOM",
  "highly", "CUSTOM",
  "causes", "CUSTOM",
  "documented", "CUSTOM",
  "review", "CUSTOM",
  "approach", "CUSTOM",
  "result", "CUSTOM",
  "framework", "CUSTOM",
  "continue", "CUSTOM",
  "effective", "CUSTOM",
  "common", "CUSTOM",
  "suggests", "CUSTOM",
  "conditions", "CUSTOM",
  "caused", "CUSTOM",
  "studies", "CUSTOM",
  "improve", "CUSTOM",
  "limited", "CUSTOM",
  "factors", "CUSTOM",
  "process", "CUSTOM",
  "processes", "CUSTOM",
  "managing", "CUSTOM",
  "levels", "CUSTOM",
  "managers", "CUSTOM",
  "bv", "CUSTOM",
  "determine", "CUSTOM",
  "reviews", "CUSTOM",
  "risk", "CUSTOM",
  "risks", "CUSTOM",
  "strategies", "CUSTOM",
  "programs", "CUSTOM",
  "reserved", "CUSTOM",
  "successful", "CUSTOM",
  "consequences", "CUSTOM",
  "responses", "CUSTOM",
  "damage", "CUSTOM",
  "threat", "CUSTOM",
  "address", "CUSTOM",
  "literature", "CUSTOM",
  "invaded", "CUSTOM",
  "discuss", "CUSTOM",
  "extensive", "CUSTOM", 
  "elsevier", "CUSTOM",
  "increasing", "CUSTOM",
  "resulting", "CUSTOM",
  "research", "CUSTOM",
  "based", "CUSTOM",
  "century", "CUSTOM",
  "concern", "CUSTOM",
  "applied", "CUSTOM",
  "increase", "CUSTOM",
  "manage", "CUSTOM",
  "support", "CUSTOM",
  "focused", "CUSTOM",
  "invasions", "CUSTOM",
  "reduce", "CUSTOM",
  "illustrate", "CUSTOM",
  "component", "CUSTOM",
  "assess", "CUSTOM",
  "infestations", "CUSTOM",
  "threatens", "CUSTOM",
  "losses", "CUSTOM",
  "tactics", "CUSTOM",
  "quality", "CUSTOM",
  "articles", "CUSTOM",
  "exist", "CUSTOM",
  "focus", "CUSTOM",
  "mitigate", "CUSTOM",
  "type", "CUSTOM",
  "step", "CUSTOM",
  "ngs", "CUSTOM",
  "provided", "CUSTOM",
  "maintain", "CUSTOM",
  "reducing", "CUSTOM",
  "lack", "CUSTOM",
  "previously", "CUSTOM",
  "andor", "CUSTOM",
  "created", "CUSTOM",
  "primary", "CUSTOM",
  "unique", "CUSTOM",
  "considerations", "CUSTOM",
  "eliminate", "CUSTOM",
  "approaches", "CUSTOM",
  "understood", "CUSTOM",
  "scientific", "CUSTOM",
  "developing", "CUSTOM",
  "developed", "CUSTOM",
  "keystone", "CUSTOM",
  "required", "CUSTOM",
  "include", "CUSTOM",
  "central", "CUSTOM",
  #emma added gr_canada
  "00bb", "CUSTOM",
  "red", "CUSTOM",
  "red", "CUSTOM",
  "1984", "CUSTOM",
  "wa", "CUSTOM",
  "ca", "CUSTOM",
  "pra", "CUSTOM",
  "bole", "CUSTOM",
  "mm", "CUSTOM",
  "er", "CUSTOM",
  "ma", "CUSTOM",
  "ha", "CUSTOM",
  "la", "CUSTOM",
  "mo", "CUSTOM",
  "st", "CUSTOM",
  "2000", "CUSTOM",
  "fish", "CUSTOM",
  "2004", "CUSTOM",
  "f06f", "CUSTOM",
  "strategy", "CUSTOM",
  "loosestrife", "CUSTOM",
  "inf", "CUSTOM",
  "taylor", "CUSTOM",
  "386", "CUSTOM",
  "rep", "CUSTOM",
  "journal", "CUSTOM",
  "hall", "CUSTOM",
  "25", "CUSTOM",
  "system", "CUSTOM",
  "1998", "CUSTOM",
  "ch", "CUSTOM",
  "1983", "CUSTOM",
  "centre", "CUSTOM",
  "resources", "CUSTOM",
  "tools", "CUSTOM",
  "aquatic", "CUSTOM",
  "water", "CUSTOM",
  "lakes", "CUSTOM",
  "lake", "CUSTOM",
  "ia", "CUSTOM",
  "1993", "CUSTOM",
  "attack", "CUSTOM",
  "view", "CUSTOM",
  "response", "CUSTOM",
  "ballast", "CUSTOM",
  "1985", "CUSTOM",
  "km", "CUSTOM",
  "harris", "CUSTOM",
  "report", "CUSTOM",
  "university", "CUSTOM",
  "purple", "CUSTOM",
  "00ad", "CUSTOM",
  "lawrence", "CUSTOM",
  #Emma added gr_usa
  "20", "CUSTOM",
  "12", "CUSTOM",
  "32", "CUSTOM",
  "15", "CUSTOM",
  "60", "CUSTOM",
  "70", "CUSTOM",
  "ecol", "CUSTOM",
  "seybold", "CUSTOM",
  "2212", "CUSTOM",
  "rs", "CUSTOM",
  "b4", "CUSTOM",
  "index", "CUSTOM",
  "index", "CUSTOM",
  "environ", "CUSTOM",
  "14", "CUSTOM",
  "b4", "CUSTOM",
  "variables", "CUSTOM",
  "50", "CUSTOM",
  "19", "CUSTOM",
  "40", "CUSTOM",
  "17", "CUSTOM",
  "22", "CUSTOM",
  "11", "CUSTOM",
  "80", "CUSTOM",
  "paine", "CUSTOM",
  "decline", "CUSTOM",
  "product", "CUSTOM",
  "identify", "CUSTOM",
  "lee", "CUSTOM",
  "pontius", "CUSTOM",
  "gtr", "CUSTOM",
  
  
  )


#add custom stop words to common stop words available
#in the tidytext package

stop_words_2 <- stop_words %>%
  bind_rows(custom_stop_words)

#give each paper a unique identifier
data$id<- seq(1:length(data$all_text))

#tokenize the data - separate into individual words
tidy_data <- data %>%
  unnest_tokens(word, all_text) %>%
  #remove stop words
  anti_join(stop_words_2)
#tidy_data<-tidy_data[,c(1,3:4)]
#tidy_data<-tidy_data[-grep("[0-9]", tidy_data$word),]

#for (i in 1:nrow(tidy_data)){
 # tidy_data$word[i]<-gsub("(.*)s$","\\1", tidy_data$word[i]) } # remove plurals? but also removes trailing s on other words

#cast to document-term matrix

tidy_dtm <- tidy_data %>%
  count(word, id) %>%
  cast_dtm(id, word, n)

#############################################################################

       ### Model Selection - determining number of topics

#############################################################################

#lda tuning method
#https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
# 


# result <- FindTopicsNumber(
#   tidy_dtm,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 42),
#   verbose = TRUE
# )

#Plot to determine appropriate range of topic numbers
#based on 4 metrics
# 
# FindTopicsNumber_plot(result)


#############################################################################

        ### Topic Model

#############################################################################


#LDA
lda_topics <- LDA(
  tidy_dtm,
  k = ntopic,      ###determined using range of appropriate values above + qualitative assessment of topic coherence
  method = "Gibbs",
  control = list(burnin = 4000, iter = 8000, thin = 1000)
) 
lda_topics5<-lda_topics%>%
  tidy(matrix = "beta")

results<-topicmodels::posterior(lda_topics)
topic_output<-results$topics
#plot the top 20 words for each topic, found together with the highest probability

word_probs <- lda_topics5 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

plot <- ggplot(word_probs, aes(term2, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "Word", y = "Distribution")+ theme(axis.text=element_text(size=6),  axis.title=element_text(size=8,face="bold"))
pdf(paste0('./result_abstract/',mode,'_plot_topics.pdf'))
# plot
# dev.off()
#order of topics not relevant, reordered for publication for ease of discussion
#rerunning the model will result in different combinations, considered stable if
#topics reflect the same themes regardless of seed


# visualize topics as word cloud
par(oma=c(0,0,0,0))
par(mar=c(2,2,2,2))
for (i in 1:ntopic){
topicToViz <- i # change for your own topic of interest

top40terms <- sort(results$terms[topicToViz,], decreasing=TRUE)[1:20]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(results$terms[topicToViz,], decreasing=TRUE)[1:20]
# visualize the terms as wordcloud
library(RColorBrewer)
library(wordcloud)
mycolors <- brewer.pal(8, "Dark2")
# 
# pdf(paste0('./result_abstract/',mode,'_fig_',i,'.pdf'))
# wordcloud(words, probabilities, random.order = FALSE, color = mycolors)
# dev.off()
}



# topics are probability distribtions over the entire vocabulary
beta <- results$terms   # get beta from results
# for every document we have a probaility distribution of its contained topics
theta <- results$topics 
exampleIds = c(1:3)
N <- length(exampleIds)
topicProportionExamples <- theta[exampleIds,]

k = 1
topKtermsPerTopic <- terms(lda_topics, k)
# topicNames <- apply(topKtermsPerTopic, 2, paste, collapse=" ")
topicNames <-  terms(lda_topics, k)

colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  # coord_flip() +
  facet_wrap(~ document, ncol = N)

# get the most frequent topic
rankcount = integer(8)
# get topic for each document
document_topic = integer(dim(theta)[1])
for(r in 1:dim(theta)[1]){
  max_index <- 1
  max_value <- theta[r, max_index]
  for (c in 1:8){
    if (theta[r, c] > max_value){
      max_index <- c
      max_value <- theta[r, c]
    }
  }
  rankcount[max_index] = rankcount[max_index] + 1
  document_topic[r] = max_index
}


df <- data.frame(topic = c(1:8), number = rankcount)
ggplot(data=df, aes(x=topic, y=number, group=1)) +
  geom_line()+
  geom_point()
