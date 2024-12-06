import json
from langchain_community.vectorstores import FAISS
from langchain_core.documents import Document
from langchain_openai import OpenAIEmbeddings
from typing import List

# Path to the JSON file
JSON_FILE_PATH = 'utils/parsed_docs/01_Introduction_parsed.json'

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
                        'full_text': section['content']
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
                        'full_table': table['content']
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
                        'full_r_code': code['code']
                    }
                )
            )
    
    return documents

if __name__ == "__main__":
    # Load documents
    docs = load_documents(JSON_FILE_PATH)
    
    # Initialize embeddings
    embeddings = OpenAIEmbeddings()
    
    # Create vector store
    vectorstore = FAISS.from_documents(docs, embeddings)
    
    # Querying part
    user_query = "How to reshape a PD dataset into a wide format?"
    top_chunks = vectorstore.similarity_search_with_score(user_query, k=3)
    
    # Pretty print the retrieved chunks
    print(f"\nQuery: {user_query}\n")
    print("Top Retrieved Results:")
    print("-" * 80)
    
    for i, (doc, score) in enumerate(top_chunks, 1):
        print(f"\nResult {i} (Score: {score:.3f}):")
        print(f"Type: {doc.metadata['type']}")
        print("\nSection/Title:")
        print(doc.page_content)
        print("\nFull Content:")
        if doc.metadata['type'] == 'text':
            print(doc.metadata['full_text'])
        elif doc.metadata['type'] == 'table':
            print(doc.metadata['full_table'])
        elif doc.metadata['type'] == 'r_code':
            print(doc.metadata['full_r_code'])
        print("-" * 80) 
