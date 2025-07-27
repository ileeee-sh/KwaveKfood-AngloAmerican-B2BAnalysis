# 1. Install required packages
!pip install newspaper3k tqdm lxml
!pip install lxml_html_clean

# 2. Import libraries
import pandas as pd
from newspaper import Article
from tqdm.notebook import tqdm
from google.colab import files
import io

# 3. Upload metadata file(.csv)
print("Upload your GDELT metadata CSV file.")
uploaded = files.upload()
filename = next(iter(uploaded))
df = pd.read_csv(io.BytesIO(uploaded[filename]))

print(f"{len(df)} articles loaded.")
print(df.head())

# 4. Define article body scraping function
def get_article_text(url):
    """
    Attempts to download and extract main article text from a URL using newspaper3k.
    Returns an empty string if download or parsing fails.
    """
    try:
        article = Article(url)
        article.download()
        article.parse()
        return article.text
    except Exception:
        return ""

# 5. Scrape article bodies
body_texts = []
for url in tqdm(df['url'], desc="Scraping article bodies"):
    text = get_article_text(url)
    body_texts.append(text)

df['body_text'] = body_texts
print(df[['title', 'body_text']].head())

# 6. Save and download (.csv)
output_filename = "gdelt_anglosphere_articles_with_body.csv"
df.to_csv(output_filename, index=False, encoding='utf-8-sig')
print(f"File saved: {output_filename}")

files.download(output_filename)
