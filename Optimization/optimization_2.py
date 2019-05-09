# Complete Code for Q2 (2) without print commands
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
print(foods)

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

# Define variabels, selection_vars is a dictionary on whether a food is selected or not
# food_vars is a dictionary on how many servings that food is used in the diet
selection_vars = LpVariable.dicts("Selection", foods,0,1, cat='Binary')
food_vars = LpVariable.dicts("Amount", foods,0)


# Define the objective function
prob += lpSum([costs[food] * food_vars[food] for food in foods]), "Total cost of a diet plan"

# Add constraints to bind selection_vars and food_vars
# minimum servings contained is 0.1
Up = 10000
Low = 0.1
for food in foods:
    prob += food_vars[food] <= Up * selection_vars[food]
    prob += food_vars[food] >= Low * selection_vars[food]

# Add contraint on celery and frozen broccoli
prob += (selection_vars['Frozen_Broccoli'] + selection_vars['Celery_Raw'] ==1)

# Add constraint on variety in daily protein, at least 3 kinds of meat/poultry/fish/eggs selected
prob += (selection_vars['Roasted_Chicken'] + selection_vars['Frankfurter_Beef']
         + selection_vars['Poached_Eggs'] + selection_vars['Scrambled_Eggs'] + selection_vars['Ham_Sliced_Extralean']
         + selection_vars['Kielbasa_Prk'] + selection_vars['Pork'] + selection_vars['Sardines_in_Oil']
         + selection_vars['White_Tuna_in_Water'] + selection_vars['Bologna_Turkey']) >= 3

# Add constraints for daily min/max nutrients intake
#k = 0
for nutrient in nutrients:
    prob += lpSum([dic[nutrient][food] * food_vars[food] for food in foods]) >= daily_min_intake[nutrient]
    prob += lpSum([dic[nutrient][food] * food_vars[food] for food in foods]) <= daily_max_intake[nutrient]
    #print(daily_min_intake[nutrient], daily_max_intake[nutrient])
#    k = k+1
#print(k)

# Solve lp problem and print out results
prob.writeLP("MinDiet.lp")
prob.solve()
print("Status:", LpStatus[prob.status])
for v in prob.variables():
    if (v.varValue > 0):
        print(v.name, "=", v.varValue)

print("Total Cost of diet per person = ", value(prob.objective))

# Result of Q2(1):
# Status: Optimal
# Amount_Celery_Raw = 42.399358
# Amount_Kielbasa_Prk = 0.1
# Amount_Lettuce_Iceberg_Raw = 82.802586
# Amount_Oranges = 3.0771841
# Amount_Peanut_Butter = 1.9429716
# Amount_Poached_Eggs = 0.1
# Amount_Popcorn_Air_Popped = 13.223294
# Amount_Scrambled_Eggs = 0.1
# Selection_Celery_Raw = 1.0
# Selection_Kielbasa_Prk = 1.0
# Selection_Lettuce_Iceberg_Raw = 1.0
# Selection_Oranges = 1.0
# Selection_Peanut_Butter = 1.0
# Selection_Poached_Eggs = 1.0
# Selection_Popcorn_Air_Popped = 1.0
# Selection_Scrambled_Eggs = 1.0
# Total Cost of diet per person =  4.512543427000001
