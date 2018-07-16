# -*- coding: utf-8 -*-

import pulp # for building/solving optimization models
import pandas as pd #for file reading

#Read data
data=pd.read_excel("dietSummer2018.xls")
#print(data)
data=data[0:64]

#Convert dataframe to list
data=data.values.tolist()

#----- Extract individual vectors of data for each nutrient

foods = [x[0] for x in data] # list of food names

calories = dict([(x[0], float(x[3])) for x in data]) # calories for each food
cholesterol = dict([(x[0], float(x[4])) for x in data])
totalFat = dict([(x[0], float(x[5])) for x in data]) # total  fat for each food
sodium = dict([(x[0], float(x[6])) for x in data])
carbohydrate = dict([(x[0], float(x[7])) for x in data])
fiber = dict([(x[0], float(x[8])) for x in data])
protein = dict([(x[0], float(x[9])) for x in data])
vitA = dict([(x[0], float(x[10])) for x in data])
vitC = dict([(x[0], float(x[11])) for x in data])
calcium = dict([(x[0], float(x[12])) for x in data])
iron = dict([(x[0], float(x[13])) for x in data])

cost=dict([(x[0], float(x[1])) for x in data]) # cost for each food


#-----Create the minimization Lp problem
problem1=pulp.LpProblem('Diet1', pulp.LpMinimize)

#-----Define the variables
#This decision variable represents how much food we eat
foodVars=pulp.LpVariable.dicts("Foods", foods, 0)

#-----Create objective function
problem1 += pulp.lpSum([cost[f] * foodVars[f]] for f in foods), 'Total costs'

#----Add constraints upper and lower bounds for each nutrient

problem1 += pulp.lpSum([calories[f] * foodVars [f] for f in foods]) >=1500, 'min Calories'
problem1 += pulp.lpSum([calories[f] * foodVars [f] for f in foods]) <=2500, 'max Calories'

problem1 += pulp.lpSum([cholesterol[f] * foodVars [f] for f in foods]) >=30, 'min Cholesterol' 
problem1 += pulp.lpSum([cholesterol[f] * foodVars [f] for f in foods]) <=240, 'max Cholesterol'

problem1 += pulp.lpSum([totalFat[f] * foodVars [f] for f in foods]) >=20, 'min Fat'
problem1 += pulp.lpSum([totalFat[f] * foodVars [f] for f in foods]) <=70, 'max Fat'

problem1 += pulp.lpSum([sodium[f] * foodVars [f] for f in foods]) >=800, 'min Sodium'
problem1 += pulp.lpSum([sodium[f] * foodVars [f] for f in foods]) <=2000, 'max Sodium'

problem1 += pulp.lpSum([carbohydrate[f] * foodVars [f] for f in foods]) >=130, 'min Carbohydrate'
problem1 += pulp.lpSum([carbohydrate[f] * foodVars [f] for f in foods]) <=450, 'max Carbohydrate'

problem1 += pulp.lpSum([fiber[f] * foodVars [f] for f in foods]) >=125, 'min Fiber'
problem1 += pulp.lpSum([fiber[f] * foodVars [f] for f in foods]) <=250, 'max Fiber'

problem1 += pulp.lpSum([protein[f] * foodVars [f] for f in foods]) >=60, 'min Protein'
problem1 += pulp.lpSum([protein[f] * foodVars [f] for f in foods]) <=100, 'max Protein'

problem1 += pulp.lpSum([vitA[f] * foodVars [f] for f in foods]) >=1000, 'min vit A'
problem1 += pulp.lpSum([vitA[f] * foodVars [f] for f in foods]) <=10000, 'max vit A'

problem1 += pulp.lpSum([vitC[f] * foodVars [f] for f in foods]) >=400, 'min vit C'
problem1 += pulp.lpSum([vitC[f] * foodVars [f] for f in foods]) <=5000, 'max vit C'

problem1 += pulp.lpSum([calcium[f] * foodVars [f] for f in foods]) >=700, 'min Calcium'
problem1 += pulp.lpSum([calcium[f] * foodVars [f] for f in foods]) <=1500, 'max Calcium'

problem1 += pulp.lpSum([iron[f] * foodVars [f] for f in foods]) >=10, 'min Iron'
problem1 += pulp.lpSum([iron[f] * foodVars [f] for f in foods]) <=40, 'max Iron'


#-----Solve the optimization problem
problem1.solve()
print ("Status:", pulp.LpStatus[problem1.status])

#-----Print the output in a readable format
print("----The solution to this diet problem is-----")
for var in problem1.variables():
    if var.varValue> 0 :
        if str(var).find('Chosen'):
            print(str(var.varValue)+ " units of " +str(var))
print("Total cost of food is $%.2f" % pulp.value(problem1.objective))