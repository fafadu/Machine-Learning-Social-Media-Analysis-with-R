Sys.setenv(GITHUB_PAT = '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')
Sys.getenv('GITHUB_PAT')
#install.packages("remotes")
#library(remotes)
#remotes::install_github("vosonlab/vosonSML")
#remotes::install_github("vosonlab/VOSONDash")


# Part 1: YouTube User Analysis ----

# Load packages required for this session into library

library(vosonSML)
library(igraph)


# Set up YouTube authentication variables

my_api_key <- Sys.getenv("YOUTUBE_API_KEY")

# Authenticate to YouTube and collect data

yt_auth <- Authenticate("youtube", apiKey = my_api_key)

video_url <- c("https://www.youtube.com/watch?v=gPCCYMeXin0") #Meghan Trainor - Made You Look

yt_data <- yt_auth |> Collect(videoIDs = video_url,
                              maxComments = 3000,
                              writeToFile = TRUE,
                              verbose = TRUE) # use 'verbose' to show download progress


# Save & export the collected data

saveRDS(yt_data, file = "yt_data.rds") # save as an R object
write.csv(yt_data, file = "yt_data.csv") # export as a CSV


# View the collected YouTube data

View(yt_data)


# Inspect the comment authors

authors <- yt_data$AuthorDisplayName
authors[1:10]

authors[duplicated(authors)]
table(authors[duplicated(authors)])


# Create actor network and graph from the data #vosonSML

yt_actor_network <- yt_data |> Create("actor")
yt_actor_graph <- yt_actor_network |> Graph()

plot(yt_actor_graph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)


# Write graph to file
# Make sure to set your working directory to where you want to save the file
# before you execute the next line

write_graph(yt_actor_graph, file = "YouTubeActor.graphml", format = "graphml")


# Run Page Rank algorithm to find important users

rank_yt_actor <- sort(page_rank(yt_actor_graph)$vector, decreasing=TRUE)
rank_yt_actor[1:5]


# Overwrite the 'name' attribute in your graph with the 'screen name' attribute
# to replace YouTube IDs with more meaningful names,
# then run the Page Rank algorithm again

V(yt_actor_graph)$name <- V(yt_actor_graph)$screen_name

rank_yt_actor <- sort(page_rank(yt_actor_graph)$vector, decreasing = TRUE)
rank_yt_actor[1:6] # <NA> because this is the original video


# Are the top page rank users also the ones who commented the most?
# Not really

table(authors[duplicated(authors)])



# Sort the table by the number of comments in decreasing order
dup_authors_table <- table(authors[duplicated(authors)])
sorted_dup_authors <- sort(dup_authors_table, decreasing = TRUE)
sorted_dup_authors[1:10]  # Display top 10 most frequent commenters

# Calculate the number of unique actors
unique_actors <- unique(yt_data$AuthorDisplayName)
num_unique_actors <- length(unique_actors)
print(paste("Number of unique actors:", num_unique_actors))


#Q5 Part 2: Spotify artist analysis ----

# Load packages required for this session into library

library(spotifyr)
library(ggplot2)
library(ggridges)


# Set up authentication variables


SPOTIFY_CLIENT_I <- Sys.getenv("SPOTIFY_CLIENT_ID")
app_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
token <- "1"


# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get Spotify data on 'Meghan Trainor'

find_my_artist <- search_spotify("Meghan Trainor", type = "artist")
View(find_my_artist)


# Retrieve album data of artist

albums <- get_artist_albums("6JL8zeS1NmiOftqZTRgdTz",include_groups = c("album", "single", "appears_on", "compilation"))
View(albums)
sort(albums$release_date)
# Calculate the total number of tracks
total_tracks_sum <- sum(albums$total_tracks, na.rm = TRUE)
total_tracks_sum


songs <- get_album_tracks("5W98Ab4VvQEuFEE4TIe5fE")
View(songs)


# Retrieve song data

song <- get_track_audio_features("5jE48hhRu8E6zBDPRSkEq7")
View(song)


# Get audio features for 'Meghan Trainor'

audio_features <- get_artist_audio_features("6JL8zeS1NmiOftqZTRgdTz") # artist ID for Maghan
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name), ]
View(audio_features)

# Plot happiness (valence) scores for each album

ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Happiness in Meghan Trainor Albums",
          subtitle = "Based on valence from Spotify's Web API")


# Retrieve information about 'Maghan Trainer' related artists

related_bm <- get_related_artists("6JL8zeS1NmiOftqZTRgdTz")
View(related_bm)
related_bm$name


#Q6
# Load packages required for this session into library
update.packages("vosonSML") 
packageVersion("vosonSML")
library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)

# Part 1: Reddit Text Analysis ----

# Specify the threads from which to collect data

thread_urls <- c(
  "https://www.reddit.com/r/popculturechat/comments/12womcl/meghan_trainor_says_fck_teachers_and_slams_public/","https://www.reddit.com/r/Teachers/comments/12wft54/meghan_trainor_has_lost_all_of_my_respect/")


# Collect threads with their comments sorted by best comments first


rd_data <- Authenticate("reddit") |>  
  Collect(threadUrls = thread_urls,
          sort = "best", 
          waitTime = c(6, 8),
          writeToFile = TRUE, 
          verbose = TRUE) # use 'verbose' to show download progress
rd_data
View(rd_data)
# Save & export the collected data
# saveRDS(rd_data_1, file = "rd_data_1.rds") # save as an R object
# write.csv(rd_data_1, file = "rd_data_1.csv") # export as a CSV

saveRDS(rd_data, file = "rd_data.rds") # save as an R object
write.csv(rd_data, file = "rd_data.csv") # export as a CSV


# View the collected Reddit data

View(rd_data)

# Remove rows that have 'NA'

rd_data <- rd_data[complete.cases(rd_data), ]
View(rd_data)


# Clean the text with help from the textclean package

clean_text <- rd_data$comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> # ` vs '
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()
  #removePunctuation() |> 
  #replace_emoji() |> # optional
  #replace_emoticon() # optional
  
  
# order matters!

clean_text[1:10]

# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))
text_corpus
text_corpus[1]
text_corpus[[1]]
text_corpus[[1]]$content
text_corpus[[5]]$content


# Perform further pre-processing 

text_corpus <- text_corpus |>
  tm_map(content_transformer(tolower)) |>  #lower case
  tm_map(removeWords, stopwords(kind = "SMART")) |> #Remove common words
  
  tm_map(stripWhitespace) # Remove continuous spaces

text_corpus[[1]]$content
text_corpus[[5]]$content


# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)


# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)


# Plot word frequency

word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

ggplot(subset(word_frequ_df, freq > 2), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

ggplot(subset(word_frequ_df, freq > freq[12]), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")


#Q7 semantic (bigram) networks #lab2.2
# Create a network of artists related to BP
clean_df <- data.frame(clean_text)

#  Tokenisation: split into bigrams (two neighbouring words)

rd_bigrams <- clean_df |> unnest_tokens(output = bigram,
                                        input = clean_text,
                                        token = "ngrams",
                                        n = 2)
View(rd_bigrams)

rd_bigrams_table <- rd_bigrams |> 
  count(bigram, sort = TRUE) |> 
  separate(bigram, c("left", "right"))

View(rd_bigrams_table)

rd_bigrams_nostops <- rd_bigrams_table |> 
  anti_join(stop_words, join_by(left == word)) |> 
  anti_join(stop_words, join_by(right == word)) # different to above because now table

View(rd_bigrams_nostops)


# Remove rows that have 'NA'

rd_bigrams_nostops <- rd_bigrams_nostops[complete.cases(rd_bigrams_nostops), ]
View(rd_bigrams_nostops)


# If you have lots of bigrams, keep only those that occur at least X times (here X is 2)

rd_bigrams_nostops <- rd_bigrams_nostops |> filter(n >= 2)
View(rd_bigrams_nostops)


# Create a semantic network graph

rd_bigram_graph <- graph_from_data_frame(rd_bigrams_nostops, directed = FALSE)
rd_bigram_graph

vcount(rd_bigram_graph)
ecount(rd_bigram_graph)
rd_bigram_graph <- simplify(rd_bigram_graph) # remove loops and multiple edges
vcount(rd_bigram_graph)
ecount(rd_bigram_graph)

plot(rd_bigram_graph, vertex.size = 4, edge.arrow.size = 0.8)

write_graph(rd_bigram_graph, file = "RedditBigram.graphml", format = "graphml")

rank_rd_bigram <- sort(page_rank(rd_bigram_graph)$vector, decreasing=TRUE)
rank_rd_bigram[1:11]

# Remember to save your data
save.image(file = "Q7 2-2_Lab_Data.RData")

#Q8 Social Network Analysis lab3-1
#Reddit Actor Graph

library(vosonSML)
library(igraph)

#network_graph <- readRDS("RedditActor.rds")
#YouTubeActor
#network_graph <- readRDS("YouTubeActor.rds")
SpotifyActor
network_graph <- readRDS("SpotifyActor.rds")

# Inspect the graph object
length(V(network_graph))
V(network_graph)$name[1:20]


# Find all maximum components that are weakly connected

comps <- components(network_graph, mode = c("weak"))

comps$no #numbers How many island on the graph
comps$csize
head(comps$membership, n = 30) #belong to which island

#dev.off()  # This will close the current graphics device


# Get sub-graph with most members

largest_comp <- which.max(comps$csize)

comp_subgraph <- network_graph |> 
  induced_subgraph(vids = which(comps$membership == largest_comp))


# Degree Centrality- Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(comp_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(comp_subgraph, mode = "out"), decreasing = TRUE)[1:30]
sort(degree(comp_subgraph, mode = "total"), decreasing = TRUE)[1:30]


# Closeness Centrality- Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(comp_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(comp_subgraph, mode = "out"), decreasing = TRUE)[1:30]
sort(closeness(comp_subgraph, mode = "total"), decreasing = TRUE)[1:30]


# Betweenness Centrality- Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(comp_subgraph, directed = FALSE), decreasing = TRUE)[1:30]

# Remember to save your data
save.image(file = "Q8 SpotifyData.RData")



# Q9 Lab3.2 Girvan-Newman and (edge betweenness) Louvain methods. 
# !!Remember to set your working directory!!

library(vosonSML)
library(igraph)

# Load your chosen network graph
BritneySpotifyActor
#network_graph <- readRDS("RedditActor.rds")
network_graph <- readRDS("BritneySpotifyActor.rds")
network_graph <- readRDS("SpotifyActor.rds")
#View(RedditActor)
#RedditActor


# Transform into an undirected graph

undir_network_graph <- as.undirected(network_graph, mode = "collapse")


# Run Louvain algorithm

louvain_comm <- cluster_louvain(undir_network_graph)


# See sizes of communities

sizes(louvain_comm)


# Visualise the Louvain communities
#dev.off()  # This will close the current graphics device
plot(louvain_comm, 
     undir_network_graph, 
     vertex.label = V(undir_network_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Run Girvan-Newman (edge-betweenness) algorithm

eb_comm <- cluster_edge_betweenness(undir_network_graph)


# See sizes of communities

sizes(eb_comm)


# Visualise the edge-betweenness communities

plot(eb_comm,
     undir_network_graph, 
     vertex.label = V(undir_network_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

is_hierarchical(eb_comm)
as.dendrogram(eb_comm)
plot_dendrogram(eb_comm)

plot_dendrogram(eb_comm, mode = "dendrogram", xlim = c(1,20))

# Remember to save your data
save.image(file = "Q9  Britney Spears.RData")

#Q10 sentiment analysis Lab 2.2  Sentiment & Emotion Analysis

library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(syuzhet)
library(ggplot2)
# Load a dataset you want to work with (e.g., "rd_data" or "yt_data")
rd_data <- readRDS("rd_data.rds")
# Clean the text

clean_text <- rd_data$comment |> # change 'comment' to 'Comment' for YouTube
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |>
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()

# Assign sentiment scores to comments
sentiment_scores <- get_sentiment(clean_text, method = "afinn") |> sign()
sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)

# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)

# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Comments")


# Assign emotion scores to comments

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)

# Calculate proportion of emotions across all comments

emo_sums <- emo_scores_df[,2:9] |> 
  sign() |> 
  colSums() |> 
  sort(decreasing = TRUE) |> 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)

# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Comments")


#11  decision tree Lab 5.2
library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)


# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = )
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

# Get songs from meghan and their audio features

meghan_features <- get_artist_audio_features("Meghan Trainor")
View(meghan_features)
##degub
#rlang::last_trace()
# Install the latest spotifyr version from GitHub
#remotes::install_github("charlie86/spotifyr")

data.frame(colnames(meghan_features))

meghan_features_subset <- meghan_features[ , 9:20]
View(meghan_features_subset)

# Get top 50 songs and their audio features

top50_features <- get_playlist_audio_features("spotify", "37i9dQZF1DXcBWIGoYBM5M")
View(top50_features)

data.frame(colnames(top50_features))

top50_features_subset <- top50_features[ , 6:17]
View(top50_features_subset)

top50_features_subset <- top50_features_subset |> rename(track_id = track.id)

# Add the 'ismeghan' column (class variable) to each data frame
# to indicate which songs are by meghan and which are not

top50_features_subset["ismeghan"] <- 0
meghan_features_subset["ismeghan"] <- 1


# Remove any songs by meghan that appear in the top 50
# and combine the two data frames into one dataset

top50_features_nomeghan <- anti_join(top50_features_subset,
                                    meghan_features_subset,
                                    by = "track_id")
comb_data <- rbind(top50_features_nomeghan, meghan_features_subset)

# Format the dataset so that we can give it as input to a model:
# change the 'ismeghan' column into a factor
# and remove the 'track_id' column

comb_data$ismeghan <- factor(comb_data$ismeghan)
comb_data <- select(comb_data, -track_id)

# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]

# Train the C5.0 decision tree model

dt_model <- train(ismeghan~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to testing set size

predicted_label <- predict(dt_model, testing_set[prediction_row, ]) # predict the label for this row
predicted_label <- as.numeric(levels(predicted_label))[predicted_label] # transform factor into numeric value

if (predicted_label == testing_set[prediction_row, 12]){
  print(paste0("Prediction is: ", predicted_label, ". Correct!"))
} else {
  paste0("Prediction is: ", predicted_label, ". Wrong.")
}
# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$ismeghan)

# Remember to save your data
save.image(file = "Q11 5-2_Lab_Data.RData")


#Q12 LDA topic modelling
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(topicmodels)
library(reshape2)
library(dplyr)
library(ggplot2)
#Q12 LDA topic modelling
# Load a dataset you want to work with (e.g., "rd_data" or "yt_data")
yt_data <- readRDS("yt_data.rds")
yt_data <- yt_data[complete.cases(yt_data), ] # Remove rows that have 'NA'
# Clean the text
clean_text <- yt_data$Comment|> # change 'comment' to 'Comment' for YouTube
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()
# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content
# Perform further pre-processing 

text_corpus <- text_corpus |>
  tm_map(content_transformer(tolower)) |> 
  tm_map(removeWords, stopwords(kind = "SMART")) |> 
  # tm_map(stemDocument) |> # optional
  tm_map(stripWhitespace)

text_corpus[[1]]$content
text_corpus[[5]]$content
# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]
# Create LDA model with k topics

lda_model <- LDA(dtm, k = 3)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

found_topics <- tidy(lda_model, matrix = "beta")
View(found_topics)

# Visualise the top 10 terms per topic

top_terms <- found_topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta)

top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Remember to save your data
save.image(file = "Q12 LDA 5-1_Lab_Data.RData")


#Q14 
# Load required libraries
library(igraph)

# Load the network graph (replace with your actual file)
network_graph <- readRDS("SpotifyActor.rds")

# Inspect the graph
summary(network_graph)

# Find the largest connected component
comps <- components(network_graph, mode = "weak")
largest_comp <- which.max(comps$csize)
comp_subgraph <- induced_subgraph(network_graph, vids = which(comps$membership == largest_comp))

# Calculate Eigenvector Centrality
eigen_centrality <- evcent(comp_subgraph)$vector
cat("Top 5 nodes by Eigenvector Centrality:\n")
print(sort(eigen_centrality, decreasing = TRUE)[1:5])

# Plot the network with eigenvector centrality size scaling
plot(comp_subgraph, vertex.size = eigen_centrality*20, vertex.label = NA, main = "Network Graph (Node size = Eigenvector Centrality)")