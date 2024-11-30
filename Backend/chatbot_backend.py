from fastapi import FastAPI, WebSocket
from langchain_openai import ChatOpenAI
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from langchain_core.messages import HumanMessage, AIMessage
from langgraph.graph import START, MessagesState
from langgraph.checkpoint.memory import MemorySaver
from langgraph.graph import StateGraph
import os
import logging
from websockets.exceptions import ConnectionClosed

logging.basicConfig(level=logging.ERROR)

# os.environ["OPENAI_API_KEY"] = ""

# Create FastAPI app
app = FastAPI()

# Define the model
llm = ChatOpenAI(model="gpt-4o", streaming=True)

# Define a graph
workflow = StateGraph(state_schema=MessagesState)

prompt = ChatPromptTemplate.from_messages(
    [
        ("system", "You are an AI assistant and would answer any question to the best of your knowledge by thinking things through step by step."),
        MessagesPlaceholder(variable_name="messages"),
    ]
)

# Define the function that calls the models
def call_model(state: MessagesState):
    chain = prompt | llm
    response = chain.invoke(state)
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
    try:
        while True:
            # Wait for client query
            data = await websocket.receive_json()
            print(data)
            unique_chat_id = data.get("chat_id", "default")
            user_query = data.get("query")

            if not user_query:
                await websocket.send_json({"error": "Query cannot be empty"})
                continue
            
            # print(user_query)
            # Prepare the state
            input_messages = [HumanMessage(user_query)]
            config = {"configurable": {"thread_id": unique_chat_id}}

            # Stream responses
            async for chunk, metadata in graph.astream(
                {"messages": input_messages},
                config,
                stream_mode="messages",
            ):
                if isinstance(chunk, AIMessage):
                    await websocket.send_json({"role": "ai", "content": chunk.content})
                else:
                    logging.warning(f"Unexpected chunk type: {type(chunk)}")

            # Send a special end-of-message marker after all tokens are streamed
            await websocket.send_json({"role": "ai", "content": "END_OF_MESSAGE"})



    except ConnectionClosed:
        logging.info("WebSocket connection closed.")
    except Exception as e:
        logging.error(f"WebSocket error: {e}")
    finally:
        await websocket.close()
