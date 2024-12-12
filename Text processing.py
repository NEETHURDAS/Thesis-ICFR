import pandas as pd
import nltk
from nltk.corpus import stopwords
import string
from bs4 import BeautifulSoup
# Load NLTK stopwords
nltk.download('stopwords')


def preprocess_text(text):
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

    # Join tokens back into a single string
    preprocessed_text = ' '.join(tokens)

    return preprocessed_text

# Replace 'your_excel_file.xlsx' with the path to your Excel file
excel_file = 'C:\\Users\\creat\\Desktop\\Neethu-Resume\\Dalarna-University\\Thesis\\2019-2021 Data.xlsx'

# Read the Excel file
df = pd.read_excel(excel_file)

# Access the column 'Ic dc text1 (To be combined with IC_DC_TEXT2 and IC_DC_TEXT3)'
ic_dc_text1_column = df['Ic dc text1 (To be combined with IC_DC_TEXT2 and IC_DC_TEXT3)']

# Initialize a list to store preprocessed texts
preprocessed_texts = []

# Iterate through each text in the column and preprocess it, then append to the list
for text in ic_dc_text1_column:
    preprocessed_text = preprocess_text(text)
    preprocessed_texts.append(preprocessed_text)

# Add the preprocessed text as a new column named "Processed Text" in the DataFrame
df['Processed Text'] = preprocessed_texts

# Save the updated DataFrame as an Excel file
output_excel_file = 'processed_data.xlsx'
df.to_excel(output_excel_file, index=False)

# Load the Excel file
excel_file = 'C:\\Users\\creat\Desktop\\Neethu-Resume\\Dalarna-University\\Thesis\\pythonProject\\processed_data.xlsx'  # Replace 'your_excel_file.xlsx' with the path to your Excel file
df = pd.read_excel(excel_file)

# Convert the 'date' column to datetime format
df['date'] = pd.to_datetime(df['date'], format='%d-%m-%Y %H:%M:%S')

# Extract the year from the 'date' column
df['year'] = df['date'].dt.year

# Save the updated DataFrame with the year column to a new Excel file
output_excel_file = 'updated_data.xlsx'  # Choose a filename for the output Excel file
df.to_excel(output_excel_file, index=False)

print(f"Updated data saved to {output_excel_file}")
