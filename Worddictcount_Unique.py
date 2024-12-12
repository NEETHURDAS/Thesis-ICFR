# packages
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans
import string
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer, PorterStemmer
import re
import os
import pandas as pd
import nltk
from nltk.corpus import stopwords
import string
from bs4 import BeautifulSoup
from collections import Counter
from collections import defaultdict


def clean_docs(text):
    # Remove HTML tags
    soup = BeautifulSoup(text, "html.parser")
    clean_text = soup.get_text()

    # Tokenization
    tokens = nltk.word_tokenize(clean_text)

    # Convert to lowercase
    tokens = [token.lower() for token in tokens]

    # Remove punctuation
    tokens = [token for token in tokens if token not in string.punctuation]

    # Remove stopwords
    stop_words = set(stopwords.words('english'))
    tokens = [token for token in tokens if token not in stop_words]

    # Remove custom patterns
    text = re.sub(r"\b\d+[a-zA-Z]-\d+\(e\)", " ", text)
    # Remove numbers
    text = re.sub(r"\b\d+\b", "", text)

    pattern = r'[0-9]'
    # Match all digits in the string and replace them with an empty string
    new_string = re.sub(pattern, '', text)

    # Join tokens back into a single string
    preprocessed_text = ' '.join(tokens)

    return preprocessed_text


# Load data from Excel file

data = pd.read_excel("C:\\Users\\creat\\OneDrive - Högskolan Dalarna\\thesis\\Updated_annual_data_with_merge-2.xlsx ")
# Access the column 'Ic dc text1 (To be combined with IC_DC_TEXT2 and IC_DC_TEXT3)'
disclosure = data['text_disclosure']

# Read the Excel file containing the cybersecurity dictionary
dictionary = 'C:\\Users\\creat\Desktop\\Neethu-Resume\\Dalarna-University\\Thesis\\cybersecurity_dictionary.xlsx'
cybersecurity_dictionary = pd.read_excel(dictionary, names=['topic', 'word'])
cybersecurity_dictionary['word'] = cybersecurity_dictionary['word'].str.lower()

# Initialize a list to store preprocessed texts
cleaned_docs = []
row_info = []
# Initialize a list to store the information
phrase_info_list = []
row_phrase_info = []
total_phrase_freq = Counter()
worddict = []
# Initialize a list to store the total frequency of words under each category
totalcategory_freq_list = []
# Iterate through each text in the column and preprocess it, then append to the list
# for index, text in enumerate(disclosure):
for index, text in enumerate(disclosure):
    clean_documents = clean_docs(text)
    clean_documents = [clean_documents]

    # Initialize a defaultdict to store the total count for each category
    category_total_count = defaultdict(int)

    # Iterate over each phrase in the cybersecurity dictionary
    for phrase in cybersecurity_dictionary['word']:
        # Count occurrences of the entire phrase in the text
        count = text.lower().count(phrase.lower())
        # If the phrase occurs at least once
        if count > 0:
            # Get the corresponding category for the phrase
            category = cybersecurity_dictionary[cybersecurity_dictionary['word'].str.lower() == phrase.lower()][
                'topic'].iloc[0]
            # Increment the total count for the category
            category_total_count[category] += count



    totalcategory_freq_list.append(category_total_count)
# Convert the list of counters to a DataFrame
totalcategory_freq_df = pd.DataFrame(totalcategory_freq_list)
totalcategory_freq_df = totalcategory_freq_df.fillna(0)
#print(category_freq_df)
merged_data = pd.concat([data, totalcategory_freq_df], axis=1)


output_file = "C:\\Users\\creat\\OneDrive - Högskolan Dalarna\\thesis\\Updated_annual_data_with_merge-2.xlsx"
merged_data.to_excel(output_file, index=False)
