from fastapi import FastAPI, WebSocket
from langchain_openai import ChatOpenAI
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from langchain_core.messages import HumanMessage, AIMessage, SystemMessage
from langgraph.graph import START, MessagesState
from langgraph.checkpoint.memory import MemorySaver
from langgraph.graph import StateGraph
from langchain_community.vectorstores import FAISS
from langchain_openai import OpenAIEmbeddings
import os
import logging
from websockets.exceptions import ConnectionClosed
import asyncio
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

logging.basicConfig(level=logging.ERROR)

# Create FastAPI app
app = FastAPI()

# Load the vector store with allow_dangerous_deserialization=True
embeddings = OpenAIEmbeddings()
vectorstore = FAISS.load_local("faiss_index", embeddings, allow_dangerous_deserialization=True)

# Define the model
llm = ChatOpenAI(model="gpt-4o", streaming=True)

# Define a graph
workflow = StateGraph(state_schema=MessagesState)

prompt = ChatPromptTemplate.from_messages([
    ("system", r"""You are an AI assistant with expertise in mathematics, science, and technical subjects. You are in particular an expert in data science and machine learning. Format all mathematical content using LaTeX:
                - Use $...$ for inline equations (e.g., The formula $E=mc^2$ shows...)
                - Use $$....$$ for display equations (e.g., The derivative is: $$E=mc^2$$)
                - Use proper LaTeX notation for all mathematical symbols
                - When explaining equations, break down complex mathematics step by step

                Use the following relevant information to help answer the user's question:
                {context}

                If the retrieved information is relevant, use it to enhance your response. If it's not relevant, you can ignore it.
                
                Respond thoughtfully while ensuring all mathematical expressions are properly formatted for rendering."""),
    MessagesPlaceholder(variable_name="messages"),
])

# Define the function that calls the models
def call_model(state: MessagesState):
    # Get the last message
    last_message = state["messages"][-1].content
    
    # Retrieve relevant chunks
    results = vectorstore.similarity_search_with_score(last_message, k=3)
    
    # Format context
    context = "\n\n".join([
        f"Source: {doc.metadata['source']}\n"
        f"Type: {doc.metadata['type']}\n"
        f"Content: {doc.metadata['full_text'] if doc.metadata['type'] == 'text' else doc.metadata['full_table'] if doc.metadata['type'] == 'table' else doc.metadata['full_r_code']}"
        for doc, score in results
    ])
    
    # Print the prompt and context
    print("\n" + "="*50)
    print("Query:", last_message)
    print("-"*50)
    print("Retrieved Context:")
    print(context)
    print("-"*50)
    
    # Call the chain with context
    chain = prompt | llm
    response = chain.invoke({"messages": state["messages"], "context": context})
    return {"messages": response}

# Define the node in the graph
workflow.add_edge(START, "model")
workflow.add_node("model", call_model)

# Add memory
memory = MemorySaver()

# Compile the workflow
graph = workflow.compile(checkpointer=memory)

# WebSocket endpoint
@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await websocket.accept()
    stop_event = asyncio.Event()
    current_task = None
    
    try:
        while True:
            data = await websocket.receive_json()
            
            # Handle stop request
            if data.get("type") == "stop":
                stop_event.set()
                await websocket.send_json({"type": "status", "content": "stopping"})
                continue

            # Handle normal message
            user_query = data.get("query")
            if not user_query:
                await websocket.send_json({"error": "Query cannot be empty"})
                continue

            # Reset stop event for new generation
            stop_event.clear()

            async def generate_response():
                input_messages = [HumanMessage(user_query)]
                config = {"configurable": {"thread_id": data.get("chat_id", "default")}}

                try:
                    async for chunk, metadata in graph.astream({"messages": input_messages},
                        config,
                        stream_mode="messages",
                    ):
                        if stop_event.is_set():
                            await websocket.send_json({"type": "status", "content": "stopped"})
                            break
                            
                        if isinstance(chunk, AIMessage):
                            await websocket.send_json({"role": "ai", "content": chunk.content})
                    else:  # This runs if the loop completes normally
                        await websocket.send_json({"type": "status", "content": "complete"})
                except Exception as e:
                    logging.error(f"Generation error: {e}")
                    await websocket.send_json({"type": "error", "content": str(e)})

            # Cancel any existing task before starting new one
            if current_task:
                current_task.cancel()
                
            current_task = asyncio.create_task(generate_response())
            
    except ConnectionClosed:
        logging.info("WebSocket connection closed.")
    except Exception as e:
        logging.error(f"WebSocket error: {e}")
    finally:
        if current_task:
            current_task.cancel()
