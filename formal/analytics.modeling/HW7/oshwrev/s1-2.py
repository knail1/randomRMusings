# -*- coding: utf-8 -*-

import pulp
import pandas as pd

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
problem2=pulp.LpProblem('Diet2', pulp.LpMinimize)

#-----Define the variables
#This decision variable represents how much food we eat
foodVars=pulp.LpVariable.dicts("Foods", foods, 0)

#This decision variable represents whether we eat a certain food or not
chosenVars=pulp.LpVariable.dicts("Chosen", foods, 0, 1, 'Binary')


#-----Create objective function
problem2 += pulp.lpSum([cost[f] * foodVars[f]] for f in foods), 'Total costs'

#-----Add constraints upper and lower bounds for each nutrient
problem2 += pulp.lpSum([calories[f] * foodVars [f] for f in foods]) >=1500, 'min Calories'
problem2 += pulp.lpSum([calories[f] * foodVars [f] for f in foods]) <=2500, 'max Calories'

problem2 += pulp.lpSum([cholesterol[f] * foodVars [f] for f in foods]) >=30, 'min Cholesterol' 
problem2 += pulp.lpSum([cholesterol[f] * foodVars [f] for f in foods]) <=240, 'max Cholesterol'

problem2 += pulp.lpSum([totalFat[f] * foodVars [f] for f in foods]) >=20, 'min Fat'
problem2 += pulp.lpSum([totalFat[f] * foodVars [f] for f in foods]) <=70, 'max Fat'

problem2 += pulp.lpSum([sodium[f] * foodVars [f] for f in foods]) >=800, 'min Sodium'
problem2 += pulp.lpSum([sodium[f] * foodVars [f] for f in foods]) <=2000, 'max Sodium'

problem2 += pulp.lpSum([carbohydrate[f] * foodVars [f] for f in foods]) >=130, 'min Carbohydrate'
problem2 += pulp.lpSum([carbohydrate[f] * foodVars [f] for f in foods]) <=450, 'max Carbohydrate'

problem2 += pulp.lpSum([fiber[f] * foodVars [f] for f in foods]) >=125, 'min Fiber'
problem2 += pulp.lpSum([fiber[f] * foodVars [f] for f in foods]) <=250, 'max Fiber'

problem2 += pulp.lpSum([protein[f] * foodVars [f] for f in foods]) >=60, 'min Protein'
problem2 += pulp.lpSum([protein[f] * foodVars [f] for f in foods]) <=100, 'max Protein'

problem2 += pulp.lpSum([vitA[f] * foodVars [f] for f in foods]) >=1000, 'min vit A'
problem2 += pulp.lpSum([vitA[f] * foodVars [f] for f in foods]) <=10000, 'max vit A'

problem2 += pulp.lpSum([vitC[f] * foodVars [f] for f in foods]) >=400, 'min vit C'
problem2 += pulp.lpSum([vitC[f] * foodVars [f] for f in foods]) <=5000, 'max vit C'

problem2 += pulp.lpSum([calcium[f] * foodVars [f] for f in foods]) >=700, 'min Calcium'
problem2 += pulp.lpSum([calcium[f] * foodVars [f] for f in foods]) <=1500, 'max Calcium'

problem2 += pulp.lpSum([iron[f] * foodVars [f] for f in foods]) >=10, 'min Iron'
problem2 += pulp.lpSum([iron[f] * foodVars [f] for f in foods]) <=40, 'max Iron'

   
#----Part 2a sort of
#This part is for making sure a food has an upper bound that matches whether it has been chosen or not

for f in foods:
    problem2 += foodVars[f] <= 10000*chosenVars[f]
    problem2 += foodVars[f] >= 0.1*chosenVars[f]

problem2.solve()
print ("Status:", pulp.LpStatus[problem2.status])

#-----Print the output in a readable format
print("----The solution to this diet problem is-----")
for var in problem2.variables():
    if var.varValue> 0 :
        if str(var).find('Chosen'):
            print(str(var.varValue)+ " units of " +str(var))
print("Total cost of food is $%.2f" % pulp.value(problem2.objective))


#----Part 2b sort of
#Basic equation that says we require to eat at most one of the foods 5 to 8
problem2 += chosenVars['Celery, Raw']+chosenVars['Frozen Broccoli']<=1

problem2.solve()
print ("Status:", pulp.LpStatus[problem2.status])

#-----Print the output in a readable format
print("----The solution to this diet problem is-----")
for var in problem2.variables():
    if var.varValue> 0 :
        if str(var).find('Chosen'):
            print(str(var.varValue)+ " units of " +str(var))
print("Total cost of food is $%.2f" % pulp.value(problem2.objective))

#
##----Part 2c sort of
##Basic equation that says we require to eat at least one of the foods 1 to 4
problem2 += chosenVars['Roasted Chicken']+chosenVars['Poached Eggs']+chosenVars['Scrambled Eggs']+chosenVars['Bologna,Turkey']+\
chosenVars['Frankfurter, Beef']+chosenVars['Ham,Sliced,Extralean']+chosenVars['Kielbasa,Prk']+chosenVars['Pizza W/Pepperoni']+\
chosenVars['Taco']+chosenVars['Hamburger W/Toppings']+chosenVars['Hotdog, Plain']+chosenVars['Pork']+\
chosenVars['Sardines in Oil']+chosenVars['White Tuna in Water']+chosenVars['Chicknoodl Soup']+\
chosenVars['Splt Pea&Hamsoup']+chosenVars['Vegetbeef Soup']+chosenVars['Neweng Clamchwd']>=3

problem2.solve()
print ("Status:", pulp.LpStatus[problem2.status])

#-----Print the output in a readable format
print("----The solution to this diet problem is-----")
for var in problem2.variables():
    if var.varValue> 0 :
        if str(var).find('Chosen'):
            print(str(var.varValue)+ " units of " +str(var))
print("Total cost of food is $%.2f" % pulp.value(problem2.objective))
