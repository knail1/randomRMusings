
# Import modules

from pulp import *

import pandas as pd

# Read data
data = pd.read_excel("diet.xls")

# Create a vector of food names
foods = data.iloc[:-3, 0]

# Create a dictionary of resources for each food
resources = data.iloc[:-3, 3:]
resources = resources.set_index(foods)
resources = resources.to_dict('index')

# Create a dictionary of costs
costs = dict(zip(foods, data.iloc[:-3, 1]))

# Create a dictionary of minimum and maximum requirements for each resource
requirements = dict(
    [
        [resource,
         dict(min=data[resource][65], max=data[resource][66])
         ]
    for resource in data.keys()[3:]]
)

# Decision variables used for all problems

# This decision variable represents how much food we eat/consume
foodVars = LpVariable.dicts("Foods", foods, 0)

# This decision variable represents whether we eat a certain food or not
chosenVars = LpVariable.dicts("Chosen", foods, 0, 1, 'Integer')

'''
Problem 1
Formulate an optimization model (a linear program) to find the cheapest diet that satisfies the
maximum and minimum daily nutrition constraints, and solve it using PuLP.
'''

# Create the minimization LP Problem
problem1 = LpProblem('problem1', LpMinimize)

# Set up cost objective function
problem1 += lpSum(costs[food] * foodVars[food] for food in foods)

# Add minimum and maximum resource constraints
for resource in requirements.keys():
    problem1 += lpSum([resources[food][resource] * foodVars[food] for food in foods]) >= requirements[resource]['min']
    problem1 += lpSum([resources[food][resource] * foodVars[food] for food in foods]) <= requirements[resource]['max']

# Solve the problem
problem1.solve()

# Display the results
for v in problem1.variables():
    if v.varValue != 0:
        print(v.name, "=", v.varValue)

print(value(problem1.objective))

'''
Problem 2
a) If a food is selected, then a minimum of 1/10 serving must be chosen. 
b) Many people dislike celery and frozen broccoli. So at most one, but not both, can be selected.
c) To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be selected.
'''

# Create the minimization LP Problem
problem2 = LpProblem('problem2', LpMinimize)

# Set up cost objective function
problem2 += lpSum(costs[food] * foodVars[food] for food in foods)

# Add minimum and maximum resource constraints
for resource in requirements.keys():
    problem2 += lpSum([resources[food][resource] * foodVars[food] for food in foods]) >= requirements[resource]['min']
    problem2 += lpSum([resources[food][resource] * foodVars[food] for food in foods]) <= requirements[resource]['max']

# a) Add constraint that 1/10 of a serving must be chosen
for food in foods:
    problem2 += foodVars[food] <= 100000 * chosenVars[food]
    problem2 += foodVars[food] >= 0.1 * chosenVars[food]

# b) Add constraint that at most one of frozen broccoli and spinach can be selected
problem2 += chosenVars['Frozen Broccoli'] + chosenVars['Celery, Raw'] <= 1

# c) Add constraint that at least 3 kinds of meat/poultry/fish/eggs must be selected
high_protein = ['Tofu','Roasted Chicken','Poached Eggs','Scrambled Eggs','Bologna,Turkey','Frankfurter, Beef',
                'Ham,Sliced,Extralean','Kielbasa,Prk','Taco','Hamburger W/Toppings','Hotdog, Plain','Pork',
                'Sardines in Oil','White Tuna in Water','Chicknoodl Soup','Splt Pea&Hamsoup','Vegetbeef Soup',
                'Neweng Clamchwd','New E Clamchwd,W/Mlk','Crm Mshrm Soup,W/Mlk','Beanbacn Soup,W/Watr']

problem2 += lpSum([chosenVars[food] for food in high_protein]) >= 3

# Solve the problem
problem2.solve()

# Display the results
for v in problem2.variables():
    if v.varValue != 0:
        print(v.name, "=", v.varValue)

print(value(problem2.objective))

'''
Optional
Run the optimization model on diet_large.xls, minimizing cholesterol
'''

# Read data
data_large = pd.read_excel("diet_large.xls", header=1)

# Create a vector of food names (note that there are duplicate rows)
foods_large = data_large.iloc[:-4, 0]

# Create a dictionary of resources for each food
# The to_dict() method implicitly removes duplicates by picking the later value
resources_large = data_large.iloc[:-4, 1:28].fillna(0)
resources_large = resources_large.set_index(foods_large)
resources_large = resources_large.to_dict('index')

# Remove duplicates from foods_large
foods_large = list(set(list(foods_large)))

# Create a dictionary of costs
costs_large = dict(zip(foods_large, data_large.iloc[:-4, 28].fillna(0)))

# Create a dictionary of minimum and maximum requirements for each resource
requirements_large = dict(
    [
        [resource,
         dict(min=data_large[resource][7147], max=data_large[resource][7149])
         ]
    for resource in data_large.keys()[1:28]]
)

# This decision variable represents how much food we eat/consume
foodVars_large = LpVariable.dicts("Foods_Large", foods_large, 0)

# Create the minimization LP Problem
cholesterol_problem = LpProblem('low_cholesterol', LpMinimize)

# Set up cost objective function
cholesterol_problem += lpSum(costs_large[food] * foodVars_large[food] for food in foods_large)

# Add minimum and maximum resource constraints
for resource in requirements_large.keys():
    cholesterol_problem += lpSum([resources_large[food][resource] * foodVars_large[food] for food in foods_large]) >= requirements_large[resource]['min']
    cholesterol_problem += lpSum([resources_large[food][resource] * foodVars_large[food] for food in foods_large]) <= requirements_large[resource]['max']

# Solve the problem
cholesterol_problem.solve()

# Display the results
for v in cholesterol_problem.variables():
    if v.varValue != 0:
        print(v.name, "=", v.varValue)

print(value(cholesterol_problem.objective))
