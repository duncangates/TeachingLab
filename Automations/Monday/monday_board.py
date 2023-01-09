import requests
import os
import json

apiKey = "eyJhbGciOiJIUzI1NiJ9.eyJ0aWQiOjE2MDA3NzI5NCwidWlkIjoyMjAzOTU3NSwiaWFkIjoiMjAyMi0wNS0xMVQxODowMjo0MS4wMDBaIiwicGVyIjoibWU6d3JpdGUiLCJhY3RpZCI6ODg4NDgxOSwicmduIjoidXNlMSJ9.t1s_JYzM1xPn4qyOyOmDJ_aR03AL02rWnFmJQSPAQuw"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey}
print(boardId)

query3 = 'mutation{ create_item (board_id:YOUR_BOARD_ID, item_name:"WHAT IS UP MY FRIENDS!") { id } }'
data = {'query' : query3}

r = requests.post(url=apiUrl, json=data, headers=headers) # make request
print(r.json())
