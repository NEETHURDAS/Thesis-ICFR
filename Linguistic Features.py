import spacy
from textstat.textstat import textstatistics
import pandas as pd
import re
import nltk
from nltk.corpus import stopwords
import string
from bs4 import BeautifulSoup
from textstat import textstat

def break_sentences(text):
    # Define a regular expression pattern to split text into sentences
    sentence_pattern = r'(?<=[.!?]) +'

    # Split the text into sentences using the regular expression pattern
    sentences = re.split(sentence_pattern, text)

    return sentences

# Returns Number of Words in the text
def word_count(text):
    sentences = break_sentences(text)
    words = 0
    for sentence in sentences:
        words += len([token for token in sentence])
    return words


# Returns the number of sentences in the text
def sentence_count(text):
    sentences = break_sentences(text)
    return len(sentences)


# Returns average sentence length
def avg_sentence_length(text):
    words = word_count(text)
    sentences = sentence_count(text)
    average_sentence_length = float(words / sentences)
    return average_sentence_length


# Textstat is a python package, to calculate statistics from
# text to determine readability,
# complexity and grade level of a particular corpus.
# Package can be found at https://pypi.python.org/pypi/textstat
def syllables_count(word):
    return textstatistics().syllable_count(word)


# Returns the average number of syllables per
# word in the text
def avg_syllables_per_word(text):
    syllable = syllables_count(text)
    words = word_count(text)
    ASPW = float(syllable) / float(words)
    return round(ASPW, 1)


# Return total Difficult Words in a text
def difficult_words(text):
    # Initialize a set to store difficult words
    diff_words_set = set()
    # Assuming you have a list of common words
    common_words = {
        "the", "be", "to", "of", "and", "a", "in", "that", "have", "I", "it", "for",
        "not", "on", "with", "he", "as", "you", "do", "at", "this", "but", "his",
        "by", "from", "they", "we", "say", "her", "she", "or", "an", "will", "my",
        "one", "all", "would", "there", "their", "what", "so", "up", "out", "if",
        "about", "who", "get", "which", "go", "me", "when", "make", "can", "like",
        "time", "no", "just", "him", "know", "take", "people", "into", "year",
        "your", "good", "some", "could", "them", "see", "other", "than", "then",
        "now", "look", "only", "come", "its", "over", "think", "also", "back",
        "after", "use", "two", "how", "our", "work", "first", "well", "way", "even",
        "new", "want", "because", "any", "these", "give", "day", "most", "us","under","as"
    }
    # Add more common words as needed

    # Split the text into words
    words = text.split()

    # Check each word against the list of common words
    for word in words:
        # If the word is not in the list of common words and has at least 2 syllables
        if word.lower() not in common_words and syllables_count(word) >= 2:
            diff_words_set.add(word)

    return len(diff_words_set)

def preprocess_text(text):
    # Remove HTML tags
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

    # Remove numbers
    text = re.sub(r"\b\d+\b", "", text)

    pattern = r'[0-9]'
    # Match all digits in the string and replace them with an empty string
    new_string = re.sub(pattern, '', text)

    # Join tokens back into a single string
    preprocessed_text = ' '.join(tokens)

    return preprocessed_text

# Replace 'your_excel_file.xlsx' with the path to your Excel file
excel_file = 'C:\\Users\\creat\\OneDrive - Högskolan Dalarna\\thesis\\Updated_annual_data_with_merge-2.xlsx'
# Read the Excel file
df = pd.read_excel(excel_file)
print(df)
# Access the column 'Ic dc text1 (To be combined with IC_DC_TEXT2 and IC_DC_TEXT3)'
ic_dc_text1_column = df['text_disclosure']
totalwords_count=[]
difficultwords=[]
sentencelength=[]
syllablecount=[]

for text in ic_dc_text1_column:
    preprocessed_text = preprocess_text(text)
    length_difficult=textstat.difficult_words(preprocessed_text)
    sentences_count=textstat.sentence_count(preprocessed_text)
    syllabel=textstat.syllable_count(preprocessed_text)
    totalword=textstat.lexicon_count(preprocessed_text,removepunct=True)

    totalwords_count.append(totalword)
    difficultwords.append(length_difficult)
    sentencelength.append(sentences_count)
    syllablecount.append(syllabel)

# Create a DataFrame to store the Gunning Fog Index values
# Add calculated values to the original DataFrame
df['Total word  Count']=totalwords_count
df['Difficult Words'] = difficultwords
df['Sentence Count'] = sentencelength
df['Syllable Count'] = syllablecount


# Save the updated DataFrame into an Excel file
output_excel_file = 'C:\\Users\\creat\\OneDrive - Högskolan Dalarna\\thesis\\Updated_annual_data_with_merge-2.xlsx'
df.to_excel(output_excel_file, index=False)
