import requests
import json

apiKey = "YOUR_API_KEY_HERE"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey}

query = '{ boards (limit:5) {name id} }'
data = {'query' : query}

r = requests.post(url=apiUrl, json=data, headers=headers) # make request
print(r.json())