# Objective of the analysis:
# - Load and process text data from the "Avatar" series to analyze character speech by season and episode.
# - Conduct text pre-processing, including tokenization and removal of stopwords.
# - Perform sentiment analysis to assess emotional tone using VADER sentiment scores.
# - Calculate additional sentiment metrics (arousal, valence, and description) and compile results into a final DataFrame.

# Import necessary libraries
import pandas as pd
import numpy as np
import re
import nltk
import spacy
from nltk.sentiment.vader import SentimentIntensityAnalyzer

# Load the Avatar dataset
avatar = pd.read_csv("https://raw.githubusercontent.com/austinminihan/IAA_TextAnalyticsProject/refs/heads/main/avatar.csv")
avatar.head(n=5) # Display first few rows

# Create a season-episode identifier column
avatar["season-episode"] = avatar['book_num'].astype(str) + "-" + avatar["chapter_num"].astype(str)

# Summarize character words by book, chapter, and IMDb rating
new_df = avatar.groupby(['book_num', 'chapter_num', 'imdb_rating'])['character_words'].sum().reset_index()
new_df['season-episode'] = new_df['book_num'].astype(str) + '-' + new_df['chapter_num'].astype(str)
new_df.to_csv('new.df.csv') # Save the summary to CSV
len(new_df) # Check the number of rows

# Verify season-episode identifiers as strings
new_df['season-episode'].astype(str)

# Loop through each episode to print episode text for validation
for i in range(len(new_df)): 
    ep_id = new_df['character_words'].iloc[i]
    print(i, ep_id)

# Initialize Spacy model for NLP tasks
filtered_text = []
nlp = spacy.load('en_core_web_sm')
for i in range(len(new_df)):
    # Convert text to lowercase and remove punctuation
    text = new_df.character_words[i].lower()
    text = re.sub(r'[^\w\s]', '', text)
    # Tokenize text, removing stopwords
    stop_words = nltk.corpus.stopwords.words('english')
    if text not in stop_words: 
        tokens = nlp(text)
        nlp_tokens = [token.text for token in tokens]
        filtered_text.append(" ".join(nlp_tokens))

# Create DataFrame with preprocessed (filtered) text
filter_text_df = pd.DataFrame(filtered_text).rename({0: "filtered_text"}, axis="columns")
filter_text_df.head(n=2) # Display first few rows of filtered text

# Download VADER lexicon for sentiment analysis
nltk.download('vader_lexicon')
sentiment = SentimentIntensityAnalyzer()

# Calculate VADER sentiment scores for each sentence in the first text sample
txt = filter_text_df.filtered_text[0]
sentences = txt.split('.')
for sentence in sentences:
    score = sentiment.polarity_scores(sentence)
    print(sentence)
    print('Sentiment:', score['compound'])

# Initialize lists to store sentiment data for each text entry
sentiment_data = []
for i in range(len(filter_text_df)): 
    txt = filter_text_df.filtered_text[i]
    score = sentiment.polarity_scores(txt)
    # Create dictionary with sentiment scores
    df_rows = {
        "id": i,
        "negative_score": score['neg'],
        "neutral_score": score['neu'],
        "positive_score": score['pos'],
        "compound_score": score['compound'], 
        "character_words": txt
    }
    sentiment_data.append(df_rows)

# Convert sentiment data to DataFrame
sentiment_data = pd.DataFrame(sentiment_data)

# Additional sentiment analysis with arousal, valence, and description metrics
other_data = []
for row in range(len(filter_text_df)):
    txt = filter_text_df.filtered_text[row]
    terms = txt.split()
    a_v_score = s.sentiment(terms) # Calculate arousal and valence scores
    description = s.describe(terms) # Generate descriptive metrics
    df_rows = {
        "valence": a_v_score['valence'],
        "arousal": a_v_score['arousal'],
        "description": description
    }
    other_data.append(df_rows)

# Convert additional sentiment data to DataFrame and merge with sentiment data
other_data = pd.DataFrame(other_data)
sentiment_data['arousal_score'] = other_data['arousal']
sentiment_data['valence_score'] = other_data['valence']
sentiment_data['description'] = other_data['description']

# Add IMDb rating and season columns to sentiment DataFrame
sentiment_data['imdb_rating'] = new_df['imdb_rating']
sentiment_data['season'] = new_df['book_num']
