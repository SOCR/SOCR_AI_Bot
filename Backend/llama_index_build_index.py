import csv
from llama_index.core import VectorStoreIndex, get_response_synthesizer, Document
from llama_index.core.retrievers import VectorIndexRetriever
from llama_index.core.query_engine import RetrieverQueryEngine

# Path to the CSV file
CSV_FILE_PATH = 'synthetic_dataset_langchain.csv'

def load_documents(csv_path):
    documents = []
    with open(csv_path, 'r', encoding='utf-8') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            # Text
            if row['Text'] and row['Text Summary']:
                documents.append(
                    Document(
                        text=row['Text Summary'],
                        metadata={
                            'type': 'text',
                            'full_text': row['Text']
                        }
                    )
                )
            # Table
            if row['Table'] and row['Table Summary']:
                documents.append(
                    Document(
                        text=row['Table Summary'],
                        metadata={
                            'type': 'table',
                            'full_table': row['Table']
                        }
                    )
                )
            # R Code
            if row['R Code'] and row['R Code Summary']:
                documents.append(
                    Document(
                        text=row['R Code Summary'],
                        metadata={
                            'type': 'r_code',
                            'full_r_code': row['R Code']
                        }
                    )
                )
    return documents

if __name__ == "__main__":
    docs = load_documents(CSV_FILE_PATH)
    index = VectorStoreIndex.from_documents(docs)

    # Querying part
    user_query = "What is linear regression?"
    retriever = index.as_retriever(similarity_top_k=3)
    top_chunks = retriever.retrieve(user_query)
    # Pretty print the retrieved chunks
    print(f"\nQuery: {user_query}\n")
    print("Top Retrieved Results:")
    print("-" * 80)
    
    for i, chunk in enumerate(top_chunks, 1):
        node = chunk.node
        print(f"\nResult {i} (Score: {chunk.score:.3f}):")
        print(f"Type: {node.metadata['type']}")
        print("\nSummary:")
        print(node.text)
        print("\nFull Content:")
        if node.metadata['type'] == 'text':
            print(node.metadata['full_text'])
        elif node.metadata['type'] == 'table':
            print(node.metadata['full_table'])
        elif node.metadata['type'] == 'r_code':
            print(node.metadata['full_r_code'])
        print("-" * 80)
