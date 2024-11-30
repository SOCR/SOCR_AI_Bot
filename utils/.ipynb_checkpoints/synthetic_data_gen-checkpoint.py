
# Set your OpenAI API Key
key_api = ""
import os
os.environ["OPENAI_API_KEY"] = key_api

from langchain_openai import ChatOpenAI

model = ChatOpenAI(model="gpt-3.5-turbo")

from langchain_core.messages import HumanMessage

# reply = model.invoke([HumanMessage(content="Hi! I'm Bob")])

# Prompts for generating content
prompts = {
    "text": "Write a random paragraph about machine learning topics.",
    "text_summary": "Summarize the following text about machine learning topics.",
    "table": "Create a markdown table with 3 columns and 5 rows about machine learning algorithms, their description, and use cases.",
    "table_summary": "Summarize the content of the following markdown table.",
    "r_code": "Generate an R code snippet for a machine learning use case.",
    "r_code_summary": "Summarize the purpose of the following R code snippet."
}

def generate_content(message):
    reply = model.invoke([HumanMessage(content=message)])
    return  reply.content

# Generate synthetic dataset
data = []
for _ in range(10):  # Create 10 rows
    text = generate_content(prompts["text"])
    text_summary = generate_content(f"{prompts['text_summary']} {text}")
    
    table = generate_content(prompts["table"])
    table_summary = generate_content(f"{prompts['table_summary']} {table}")
    
    r_code = generate_content(prompts["r_code"])
    r_code_summary = generate_content(f"{prompts['r_code_summary']} {r_code}")
    
    data.append({
        "Text": text,
        "Text Summary": text_summary,
        "Table": table,
        "Table Summary": table_summary,
        "R Code": r_code,
        "R Code Summary": r_code_summary
    })

# Convert to DataFrame
synthetic_dataset = pd.DataFrame(data)

# Save to a file or display
synthetic_dataset.to_csv("synthetic_dataset_langchain.csv", index=False)
print("Synthetic dataset saved to 'synthetic_dataset_langchain.csv'")
