
#############################################################################

      ### Data cleaning and preparation for analysis

#############################################################################

library(tidyverse)
library(tidytext)

#load screened results of literature search
# filename = '/Users/chenyuyan/journal_access-main/review_text.csv
filename = '/Users/chenyuyan/journal_access-main/text.csv'
data = read.csv(filename)
# data <- read.csv("Literature_Search_Results_clean.csv")


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
# library(topicmodels)
# library(ldatuning)
# 
# 
# result <- FindTopicsNumber(
#   tidy_dtm,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 42),
#   verbose = TRUE
# )
# 
# #Plot to determine appropriate range of topic numbers
# #based on 4 metrics
# 
# FindTopicsNumber_plot(result)



#############################################################################

        ### Topic Model

#############################################################################

library(topicmodels)
library(forcats)
library(tidytext)
library(tidyverse)

#LDA
lda_topics5 <- LDA(
  tidy_dtm,
  k = 5,      ###determined using range of appropriate values above + qualitative assessment of topic coherence
  method = "Gibbs",
  control = list(burnin = 4000, iter = 8000, thin = 1000)
) %>%
  tidy(matrix = "beta")


#plot the top 20 words for each topic, found together with the highest probability

word_probs <- lda_topics5 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

plot <- ggplot(word_probs, aes(term2, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "Word", y = "Distribution") 


#order of topics not relevant, reordered for publication for ease of discussion
#rerunning the model will result in different combinations, considered stable if
#topics reflect the same themes regardless of seed


