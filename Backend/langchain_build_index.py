import json
import os
from langchain_community.vectorstores import FAISS
from langchain_core.documents import Document
from langchain_openai import OpenAIEmbeddings
from typing import List

# Directory containing JSON files
JSON_DIR = 'utils/parsed_docs'

def load_documents(json_path: str) -> List[Document]:
    documents = []
    
    # Load JSON file
    with open(json_path, 'r', encoding='utf-8') as jsonfile:
        data = json.load(jsonfile)
    
    # Process sections
    for section in data['sections']:
        # Text sections (using title as summary)
        if section['content']:  # Only process if content exists
            documents.append(
                Document(
                    page_content=section['title'],  # Using title instead of summary
                    metadata={
                        'type': 'text',
                        'full_text': section['content'],
                        'source': os.path.basename(json_path)
                    }
                )
            )
    
    # Process tables
    if 'tables' in data:
        for table in data['tables']:
            documents.append(
                Document(
                    page_content=table['section'],  # Using section instead of summary
                    metadata={
                        'type': 'table',
                        'full_table': table['content'],
                        'source': os.path.basename(json_path)
                    }
                )
            )
    
    # Process R code
    if 'r_code' in data:
        for code in data['r_code']:
            documents.append(
                Document(
                    page_content=code['section'],  # Using section instead of summary
                    metadata={
                        'type': 'r_code',
                        'full_r_code': code['code'],
                        'source': os.path.basename(json_path)
                    }
                )
            )
    
    return documents

def build_index():
    all_documents = []
    
    # Process all JSON files in the directory
    for filename in os.listdir(JSON_DIR):
        if filename.endswith('.json'):
            json_path = os.path.join(JSON_DIR, filename)
            docs = load_documents(json_path)
            all_documents.extend(docs)
    
    # Initialize embeddings
    embeddings = OpenAIEmbeddings()
    
    # Create vector store
    vectorstore = FAISS.from_documents(all_documents, embeddings)
    
    # Save the index
    vectorstore.save_local("faiss_index")
    
    return len(all_documents)

if __name__ == "__main__":
    num_docs = build_index()
    print(f"Index built successfully with {num_docs} documents")
