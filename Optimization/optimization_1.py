# Complete Code for Q2 (1) without print commands
# Code tested, clean version, use this one
from pulp import *
import pandas as pd

# Test the pulp package
pulp.pulpTestAll()

# Read input data file
diet = pd.read_csv("diet.csv", sep=',')
diet.columns = ['Foods', 'Price_Per_Serving', 'Serving_Size', 'Calories', 'Cholesterol_mg',
               'Total_Fat_g', 'Sodium_mg','Carbohydrates_g', 'Dietary_Fiber_g','Protein_g',
               'Vit_A_IU', 'Vit_C_IU','Calcium_mg', 'Iron_mg']

# Remove dollar sign from second column and convert the column to numeric
diet['Price_Per_Serving'] = diet['Price_Per_Serving'].str.replace('$','')
diet['Price_Per_Serving'] = pd.to_numeric(diet['Price_Per_Serving'])
diet['Foods'] = diet['Foods'].str.replace(', ', '_')
diet['Foods'] = diet['Foods'].str.replace(',', "_")
diet['Foods'] = diet['Foods'].str.replace(' ', "_")

diet.head()

# Array/list foods to store all the food choices
foods = list(diet.iloc[:,0])

# List nutrients to store all the types of nutrients, e.g. Cholesterol...
nutrients = list(diet)[3:]

# Dictionary costs to store the food cost pairs
costs = dict(zip(diet.Foods, diet.Price_Per_Serving))

# Dictionaries to store the food minimum daily intake pairs
min_list = [1500,30,20,800,130,125,60,1000,400,700,10]
max_list = [2500,240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]
daily_min_intake = dict(zip(nutrients, min_list))
daily_max_intake = dict(zip(nutrients, max_list))

# Big dictionary to store nutrient dictionaries, each nutrient dictionary store food name
# and value of that nutrient
dic = {nutrient: dict(zip(diet.Foods, diet[nutrient])) for nutrient in nutrients}

# Define lp problem
prob = LpProblem ("Cheapest Food that Pass Regulation", LpMinimize)

# Define variabels
food_vars = LpVariable.dicts("DietSelection", foods,0)

# Define the objective function
prob += lpSum([costs[food] * food_vars[food] for food in foods]), "Total cost of a diet plan"

# Add constraints
k = 0
for nutrient in nutrients:
    prob += lpSum([dic[nutrient][food] * food_vars[food] for food in foods]) >= daily_min_intake[nutrient]
    prob += lpSum([dic[nutrient][food] * food_vars[food] for food in foods]) <= daily_max_intake[nutrient]
    print(daily_min_intake[nutrient], daily_max_intake[nutrient])
    k = k+1
print(k)

# Solve lp problem and print out results
prob.writeLP("MinDiet.lp")
prob.solve()
print("Status:", LpStatus[prob.status])
for v in prob.variables():
    print(v.name, "=", v.varValue)

print("Total Cost of diet per person = ", value(prob.objective))



# In[ ]:


# Result of Q2(1):
# DietSelection_Celery_Raw = 52.64371
# DietSelection_Frozen_Broccoli = 0.25960653
# DietSelection_Lettuce_Iceberg_Raw = 63.988506
# DietSelection_Oranges = 2.2929389
# DietSelection_Poached_Eggs = 0.14184397
# DietSelection_Popcorn_Air_Popped = 13.869322
# Total Cost of diet per person =  4.337116797399999
