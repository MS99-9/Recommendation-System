# Recommendation-System
This is a Recommending System using haskell language
Almost all the applications we use today rely extensively on recommender systems to build user profiles and accordingly recommend items to buy/watch/read that matches their users’ interests.
In this project, a recommender system uses the users’ purchases history to:
• Recommend an item to the user based on their previous purchases if the current cart is empty
• If there are items already added to the users cart, recommend an item to the user based on their previous purchases and the current items in the cart
• Another option is to recommend an item to the user based on the intersection between the items they previously purchased and other users’ purchases

This is the dataset where users is a list of the users, items is a list of the items that the users can buy and purchases is a list mapping each user to a list of all his/her previous shopping carts.
users = ["A", "B", "C", "D", "E"]
items = ["suit", "dress", "shoes", "T-shirt", "Jacket", "skirt", "shorts", "shirt", "trousers", " purchases = [("A", [["dress", "shoes"],["milk", "cheese", "eggs"]]),
             ("B", [["earphone", "mouse", "laptop"],["mp3 player"]]),
             ("C", [["bread", "milk"],["shoes"]]),
             ("D", [["milk", "meat", "chicken", "yogurt"],["beans", "cereal", "flour", "sugar"],
             ("E", [])
             ]
