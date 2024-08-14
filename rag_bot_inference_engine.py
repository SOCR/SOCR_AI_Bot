from llama_index.core import Settings
from llama_index.llms.huggingface import HuggingFaceLLM
from llama_index.core import PromptTemplate
from llama_index.vector_stores.faiss import FaissVectorStore
from llama_index.core import StorageContext, load_index_from_storage
import torch
import pprint
from langchain.embeddings import HuggingFaceEmbeddings
from llama_index.embeddings.langchain import LangchainEmbedding
from flask import Flask, jsonify, request
from llama_index.llms.openai import OpenAI
from PIL import Image

from llama_index.core.memory import ChatMemoryBuffer

memory = ChatMemoryBuffer.from_defaults(token_limit=1500)


torch.manual_seed(1)
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
Settings.embed_model = LangchainEmbedding(
    HuggingFaceEmbeddings(model_name="./sentence-transformers")
    )


vector_store = FaissVectorStore.from_persist_dir("./storage")
storage_context = StorageContext.from_defaults(
    vector_store=vector_store, persist_dir="./storage"
)
index = load_index_from_storage(storage_context=storage_context)




def generate_response(api_key, model,input_query):
    
    llm = OpenAI(model=model)
    llm.api_key = api_key

    Settings.llm = llm

    chat_engine = index.as_chat_engine(
    chat_mode="condense_plus_context",
    memory=memory,
    context_prompt=(
        "You are a chatbot, able to have normal interactions, as well as answer questions from the given context"
        "Here are the relevant documents for the context:\n"
        "{context_str}"
        "\nInstruction: Use the previous chat history, or the context above, to interact and help the user."
    ),
    verbose=False,
)

    response = chat_engine.chat(input_query)

    return response.response






