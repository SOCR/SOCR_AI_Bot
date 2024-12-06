import asyncio
import websockets
import json
import sys

async def test_chatbot():
    uri = "ws://localhost:8000/ws"
    
    # Prompt user for input
    query = input("Enter your question: ")
    
    async with websockets.connect(uri) as websocket:
        message = {
            "query": query,
            "chat_id": "test_chat"
        }
        
        await websocket.send(json.dumps(message))
        
        # For accumulating the response
        current_line = ""
        
        while True:
            try:
                response = await websocket.recv()
                data = json.loads(response)
                
                # Handle completion message
                if data.get("type") == "status" and data.get("content") == "complete":
                    print("\n\nResponse complete!")
                    break
                
                # Handle content streaming
                if data.get("role") == "ai":
                    content = data.get("content", "")
                    # Print without newline and flush immediately
                    print(content, end="", flush=True)
                    
            except websockets.ConnectionClosed:
                break

# Run the test
asyncio.run(test_chatbot()) 
