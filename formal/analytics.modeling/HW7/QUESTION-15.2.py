#!/usr/bin/env python
"""
Q15.2 HW7 ISYE 6501
June 2018
OA
"""

from pulp import *
import pandas as pd

dietData = pd.read_excel("dietSummer2018.xls")
limits = dietData[65:67]
dietData = dietData[0:64]

# change the data frame into a big list , with each row being a list within a big list (thus creating a matrix)
dietDataList = dietData.values.tolist()
limitsList = limits.values.tolist()

# pulling the foods out of this matrix'd list:
foods = [x[0] for x in dietDataList]
costOfMenuOptions = dict([(x[0], float(x[1])) for x in dietDataList])
calorieLevels = dict([(x[0], float(x[3])) for x in dietDataList])
cholesterolLevels =  dict([(x[0], float(x[4])) for x in dietDataList])
fatLevels = dict([(x[0], float(x[5])) for x in dietDataList])
carbLevels = dict([(x[0], float(x[7])) for x in dietDataList])
proteinLevels = dict([(x[0], float(x[9])) for x in dietDataList])

# TODO: finish all the 16 columns in diet and put them intheir respective arrays.
# finish pulling all the nutrients' daily recommended values for each food group

# Create the 'prob' variable to contain the problem data
# we will first just run the model against a simple set of 2 variables and simple constraints
# we do this to test the model and observe if the results are realistic and acceptable before trying this on all 64 menu options
# this also helps us visualize how to optimize the SW (e.g. where to add loops etc)
# essentially, this basic model acts like a prototype.
probBasic = LpProblem("The Army Diet Problem - basic - 2 menu options only", LpMinimize)
#LpSolverDefault.msg = 1
# food nutrients we shall optimize for for the final model
# note: we will only use calories, protein, and fats to optimize for basic model

#	- calories kCal <- basic model
#	- cholesterol mg
#	- total_fat g <- basic model
#	- sodium mg
#	- carbohydrates g
#	- dietary_fiber g
#	- protein g <- basic model

# Then, we define the constants for min and max levels of the nutrients
# note that some of the nutrients are just 1 value; that means there wont be a range for those values

CALORIES_MIN = 2000  # kCal not 1500 since this is the army and they need the energy, theyre not trying to lose weight!
CALORIES_MAX = 2500  # kCal
CHOLESTEROL_MIN = 0  # or 300mg
CHOLESTEROL_MAX = .3  # or 300mg
FAT = 78  # grams
SODIUM = 2.3  # or 2300mg
CARBS = 275  # grams
FIBER = 28  # g
PROTEIN_MIN = 50  # g
PROTEIN_MAX = 50 * (2500 / 2000)  # g could be higher but army trying to save money!

# min and max values of daily nutrients, listed in this order
# [calories, cholesterol ,fat, sodium, carbs, fiber, protein]
amin = [CALORIES_MIN, CHOLESTEROL_MIN, FAT, SODIUM, CARBS, FIBER, PROTEIN_MIN]
amax = [CALORIES_MAX, CHOLESTEROL_MAX, FAT, SODIUM, CARBS, FIBER, PROTEIN_MAX]

# The first run is just against two menu items to test the process:

# roasted chicken:
x1 = LpVariable(foods[8], 0, None, LpInteger)
# beef frankfurter:
x2 = LpVariable(foods[30], 0, None, LpInteger)

# as we can see, the lower bound for both the menu options is 0, and the upper bound is None (so it is undefined
# we want this to be there so we can allow for as much quantity as possible for these two to satisfy the requirements

# Now, let's define the objective function, which is to minimize to cost of these two items
# The objective function is added to 'prob' first
probBasic += costOfMenuOptions[foods[8]] * x1 + costOfMenuOptions[foods[30]] * x2, "Total Cost of Menu Options - Basic Run"

# TODO: each one of the 63 nutrients need to have an LpVariable created, maybe through a loop
# each one of the 63 variable has a different flip of ran.. I wrote this when nodding off, dont know what this means

# After the first line in prob, the five constraints are entered
# (this way of appending to a "variable" is very reminiscint to the way we operate ggplot "variable" (which is more like an object) in R)

# note: I'm not fully clear if the first constraint of the servings adding to 100 is correct as done in the whiskas example, so i'm omitting it
# the reason of omission is that the servings are not a percentage which need to add to 100%, but are just arbitrary data like 10 Oz Pakage, or 1/2 cup shredded etc

# we set up constraints for the basic model based on just calories, fat and proteins:
probBasic += calorieLevels[foods[8]] * x1 + calorieLevels[foods[30]] * x2 >= CALORIES_MIN, "Minimum Calorie Requirement"
probBasic += calorieLevels[foods[8]] * x1 + calorieLevels[foods[30]] * x2 <= CALORIES_MAX, "Maximum Calorie Requirement"
probBasic += fatLevels[foods[8]] * x1 + fatLevels[foods[30]] * x2 >= FAT, "Fat Requirement"
probBasic += proteinLevels[foods[8]] * x1 + proteinLevels[foods[30]] * x2 >= PROTEIN_MIN, "Protein Requirements"


# The problem data is written to an .lp file
#LP_PATH = '/tmp'
#probBasic.writeLP(LP_PATH + "/BasicArmyDiet.lp")
probBasic.writeLP("BasicArmyDiet.lp")

# The problem is solved using PuLP's choice of Solver
probBasic.solve()


# The status of the solution is printed to the screen
print("----First run with just 2 menu options and limited constraints, ie. calories, fat and protein----")
print "Status:", LpStatus[probBasic.status]

# Each of the variables is printed with it's resolved optimum value
for v in probBasic.variables():
    print v.name, "=", v.varValue

# The optimised objective function value is printed to the screen
print "Total Cost of Servings per meal = ", value(probBasic.objective)

# the output of the above is:
# Status: Optimal
# Frankfurter,_Beef = 15.0
# Roasted_Chicken = 0.0
# Total Cost of Servings per meal =  4.05
# All in all, a very unhealthy meal, but given the cheap cost of the frankfurter, it makes sense.

# now that we've confirmed the model is working, we shall attempt to run a full model

# TODO use this for the  full https://piazza.com/class/jgz9hhpeyjn4h3?cid=555

# First, let's define _all_ the dictionaries (aka key:value pairs) for the nutrients
# instead of going and re-using some and not re-using others, i'll define a fresh set of dicts:

#limits = xxx
foods = [x[0] for x in dietDataList]
costOfMenuOptions = dict([(x[0], float(x[1])) for x in dietDataList])
calorieLevels = dict([(x[0], float(x[3])) for x in dietDataList])
cholesterolLevels =  dict([(x[0], float(x[4])) for x in dietDataList])
fatLevels = dict([(x[0], float(x[5])) for x in dietDataList])
sodiumLevels = dict([(x[0], float(x[6])) for x in dietDataList])
carbLevels = dict([(x[0], float(x[7])) for x in dietDataList])
dietaryFiberLevels = dict([(x[0], float(x[8])) for x in dietDataList])
proteinLevels = dict([(x[0], float(x[9])) for x in dietDataList])
vitAIULevels = dict([(x[0], float(x[10])) for x in dietDataList])
vitCUILevels = dict([(x[0], float(x[11])) for x in dietDataList])
calciumLevels = dict([(x[0], float(x[12])) for x in dietDataList])
ironLevels = dict([(x[0], float(x[13])) for x in dietDataList])

# Then, we define the Linear problem variables in one line!
ingredientVariables = dict([(x[0], LpVariable([x[0]], 0, None, LpContinuous)) for x in dietDataList])

# let's make a copy for probFull2 , the 3rd run. we will use this later
ingredientVariables2 = ingredientVariables
# After this, we create the full linear problem:
probFull1 = LpProblem("The Army Diet Problem - Full all menu options with independently researched constraints", LpMinimize)

# Then we define the objective function, as the first item:
probFull1 += lpSum([costOfMenuOptions[i] * ingredientVariables[i] for i in foods]), "Total Cost"

# we follow this by appending the constraints to the problem "object"
# Note that here, we stick with the fda approved diet regulations where possible while taking liberties for what I think an army recruit must have:
# https://en.wikipedia.org/wiki/Dietary_Reference_Intake

# an army recruit must have a minimal amount of Calories of 2000 since she lives an active lifesyle
#probFull1 += lpSum([calorieLevels[i] * ingredientVariables[i] for i in foods]) >= 1500, "CaloriesMin"
probFull1 += lpSum([calorieLevels[i] * ingredientVariables[i] for i in foods]) >= 2000, "CaloriesMin"
probFull1 += lpSum([calorieLevels[i] * ingredientVariables[i] for i in foods]) <= 2500, "CaloriesMax"

# max cholesterol allowable is 300mg now
probFull1 += lpSum([cholesterolLevels[i] * ingredientVariables[i] for i in foods]) >= 30, "CholesterolMin"
#probFull1 += lpSum([cholesterolLevels[i] * ingredientVariables[i] for i in foods]) <= 240, "CholesterolMax"
probFull1 += lpSum([cholesterolLevels[i] * ingredientVariables[i] for i in foods]) <= 300, "CholesterolMax"

# fat ranges from 20-35% of calories, we will base it off the 2000 caloric intake per day
# however when I use this range, my results come about as infeasible. Therefore I'll fall back to
# the older range of fat: 20-70
# ended up using the 44-78 range from here: https://www.mayoclinic.org/healthy-lifestyle/nutrition-and-healthy-eating/expert-answers/fat-grams/faq-20058496

#probFull1 += lpSum([fatLevels[i] * ingredientVariables[i] for i in foods]) >= 0.2*2000, "TotalFatMin"
#probFull1 += lpSum([fatLevels[i] * ingredientVariables[i] for i in foods]) <= 0.35*2000, "TotalFatMax"
probFull1 += lpSum([fatLevels[i] * ingredientVariables[i] for i in foods]) >= 44, "TotalFatMin"
probFull1 += lpSum([fatLevels[i] * ingredientVariables[i] for i in foods]) <= 78, "TotalFatMax"

# sodium is 1500 to 2300
# confirmed here: https://sodiumbreakup.heart.org/how_much_sodium_should_i_eat
probFull1 += lpSum([sodiumLevels[i] * ingredientVariables[i] for i in foods]) >= 1500, "SodiumMin"
probFull1 += lpSum([sodiumLevels[i] * ingredientVariables[i] for i in foods]) <= 2300, "SodiumMax"
#probFull1 += lpSum([sodiumLevels[i] * ingredientVariables[i] for i in foods]) >= 800, "SodiumMin"
#probFull1 += lpSum([sodiumLevels[i] * ingredientVariables[i] for i in foods]) <= 2000, "SodiumMax"

# carbs are 130g/day for recommended value. there is no max value from FDA.
# therefore, we will use the upper range of the caloric intake as baseline (2500/2000)
# i used 225-325 : https://www.healthline.com/nutrition/how-many-carbs-per-day-to-lose-weight
#probFull1 += lpSum([carbLevels[i] * ingredientVariables[i] for i in foods]) >= 130, "CarbohydratesMin"
#probFull1 += lpSum([carbLevels[i] * ingredientVariables[i] for i in foods]) <= 130*2500/2000, "CarbohydratesMax"
probFull1 += lpSum([carbLevels[i] * ingredientVariables[i] for i in foods]) >= 225, "CarbohydratesMin"
probFull1 += lpSum([carbLevels[i] * ingredientVariables[i] for i in foods]) <= 325, "CarbohydratesMax"


# fiber is 38g/day, using the same logic as carbs..
probFull1 += lpSum([dietaryFiberLevels[i] * ingredientVariables[i] for i in foods]) >= 38, "DietaryFiberMin"
probFull1 += lpSum([dietaryFiberLevels[i] * ingredientVariables[i] for i in foods]) <= 38*2500/2000, "DietaryFiberMax"
#probFull1 += lpSum([dietaryFiberLevels[i] * ingredientVariables[i] for i in foods]) >= 125, "DietaryFiberMin"
#probFull1 += lpSum([dietaryFiberLevels[i] * ingredientVariables[i] for i in foods]) <= 250, "DietaryFiberMax"

# protein is 56, same logic for upper limit as carbs
# however, since this is the army, and we want our soliders to have muscles, lets increase this intake to 80g
# with no upper limit (i.e. as buff as you want to be)
probFull1 += lpSum([proteinLevels[i] * ingredientVariables[i] for i in foods]) >= 80, "ProteinMin"
#probFull1 += lpSum([proteinLevels[i] * ingredientVariables[i] for i in foods]) >= 56, "ProteinMin"
#probFull1 += lpSum([proteinLevels[i] * ingredientVariables[i] for i in foods]) <= 56*2500/2000, "ProteinMax"
#probFull1 += lpSum([proteinLevels[i] * ingredientVariables[i] for i in foods]) >= 60, "ProteinMin"
#probFull1 += lpSum([proteinLevels[i] * ingredientVariables[i] for i in foods]) <= 100, "ProteinMax"

# vitamin A RDA highest is 900 , and for micronutrients FDA does give upper liits. for VitA its 3000
probFull1 += lpSum([vitAIULevels[i] * ingredientVariables[i] for i in foods]) >= 900, "VitAMin"
probFull1 += lpSum([vitAIULevels[i] * ingredientVariables[i] for i in foods]) <= 3000, "VitAMax"
#probFull1 += lpSum([vitAIULevels[i] * ingredientVariables[i] for i in foods]) >= 1000, "VitAMin"
#probFull1 += lpSum([vitAIULevels[i] * ingredientVariables[i] for i in foods]) <= 10000, "VitAMax"

# vitamin C RDA is 90, and Upper limit is 2000
probFull1 += lpSum([vitCUILevels[i] * ingredientVariables[i] for i in foods]) >= 90, "VitCMin"
probFull1 += lpSum([vitCUILevels[i] * ingredientVariables[i] for i in foods]) <= 2000, "VitCMax"
#probFull1 += lpSum([vitCUILevels[i] * ingredientVariables[i] for i in foods]) >= 400, "VitCMin"
#probFull1 += lpSum([vitCUILevels[i] * ingredientVariables[i] for i in foods]) <= 5000, "VitCMax"


# calcium RDA is 1000 and UL is 2500
probFull1 += lpSum([calciumLevels[i] * ingredientVariables[i] for i in foods]) >= 1000, "CalciumMin"
probFull1 += lpSum([calciumLevels[i] * ingredientVariables[i] for i in foods]) <= 2500, "CalciumMax"
#probFull1 += lpSum([calciumLevels[i] * ingredientVariables[i] for i in foods]) >= 1000, "CalciumMin"
#probFull1 += lpSum([calciumLevels[i] * ingredientVariables[i] for i in foods]) <= 1500, "CalciumMax"

# iron RDA is 18 and UL is 45
probFull1 += lpSum([ironLevels[i] * ingredientVariables[i] for i in foods]) >= 18, "ironMin"
probFull1 += lpSum([ironLevels[i] * ingredientVariables[i] for i in foods]) <= 45, "ironMax"
#probFull1 += lpSum([ironLevels[i] * ingredientVariables[i] for i in foods]) >= 10, "ironMin"
#probFull1 += lpSum([ironLevels[i] * ingredientVariables[i] for i in foods]) <= 40, "ironMax"

probFull1.writeLP("Full1ArmyDiet.lp")

# The problem is solved using PuLP's choice of Solver
probFull1.solve()


# The status of the solution is printed to the screen
print("----Second run with all menu options and GOOGLE'd (independently researched) FDA nutritional constraints----")
print "Status:", LpStatus[probFull1.status]

# Each of the variables is printed with it's resolved optimum value
for v in probFull1.variables():
    if v.varValue != 0.0:
        print v.name, "=", v.varValue

# The optimised objective function value is printed to the screen
print "Total Cost of Servings per meal = ", value(probFull1.objective)

# we are looking for the following answer:
# Ingr_Celery,_Raw = 52.64371
# Ingr_Frozen_Broccoli = 0.25960653
# Ingr_Lettuce,Iceberg,Raw = 63.988506
# Ingr_Oranges = 2.2929389
# Ingr_Poached_Eggs = 0.14184397
# Ingr_Popcorn,Air_Popped = 13.869322
# Total price per day is $4.34

# per question details:  The optimal solution should be a diet of
#   - air-popped popcorn,
#   - poached eggs,
#   - oranges,
#   - raw iceberg lettuce,
#   - raw celery, and
#   - frozen broccoli.

# However, since I used my own judgment on the dietary intake limits doing research for each micro and macro-nutrient my results would be different

# my results are:
# ----Second run with all menu options and all FDA nutritional constraints----
# Status: Optimal
# _u'Chocolate_Chip_Cookies'_ = 1.0
# _u'Oranges'_ = 1.0
# _u'Peanut_Butter'_ = 2.0
# _u'Poached_Eggs'_ = 1.0
# _u'Popcorn,Air_Popped'_ = 6.0
# _u'Potatoes,_Baked'_ = 2.0
# _u'Pretzels'_ = 1.0
# _u'Skim_Milk'_ = 3.0
# _u'Wheat_Bread'_ = 1.0
# Total Cost of Servings per meal =  1.32

# ----------------------------------------------------------
# now we do a final run with the constraints provided within the excel sheet!

CALORIES_MIN = float(limitsList[0][3])
CALORIES_MAX = float(limitsList[1][3])
CHOLESTEROL_MIN = float(limitsList[0][4])
CHOLESTEROL_MAX = float(limitsList[1][4])
FAT_MIN = float(limitsList[0][5])
FAT_MAX = float(limitsList[1][5])
SODIUM_MIN = float(limitsList[0][6])
SODIM_MAX = float(limitsList[1][6])
CARB_MIN = float(limitsList[0][7])
CARB_MAX = float(limitsList[1][7])
FIBER_MIN = float(limitsList[0][8])
FIBER_MAX = float(limitsList[1][8])
PROTEIN_MIN = float(limitsList[0][9])
PROTEIN_MAX = float(limitsList[1][9])
VITAMIN_A_MIN = float(limitsList[0][10])
VITAMIN_A_MAX = float(limitsList[1][10])
VITAMIN_C_MIN = float(limitsList[0][11])
VITAMIN_C_MAX = float(limitsList[1][11])
CALCIUM_MIN = float(limitsList[0][12])
CALCIUM_MAX = float(limitsList[1][12])
IRON_MIN = float(limitsList[0][13])
IRON_MAX = float(limitsList[1][13])


probFull2 = LpProblem("The Army Diet Problem - Full all menu options with specified constraints", LpMinimize)


# Then we define the objective function, as the first item:
probFull2 += lpSum([costOfMenuOptions[i] * ingredientVariables2[i] for i in foods]), "Total Cost"

# we follow this by appending the constraints to the problem "object"

probFull2 += lpSum([calorieLevels[i] * ingredientVariables2[i] for i in foods]) >= CALORIES_MIN, "CaloriesMin"
probFull2 += lpSum([calorieLevels[i] * ingredientVariables2[i] for i in foods]) <= CALORIES_MAX, "CaloriesMax"

probFull2 += lpSum([cholesterolLevels[i] * ingredientVariables2[i] for i in foods]) >= CHOLESTEROL_MIN, "CholesterolMin"
probFull2 += lpSum([cholesterolLevels[i] * ingredientVariables2[i] for i in foods]) <= CHOLESTEROL_MAX, "CholesterolMax"

probFull2 += lpSum([fatLevels[i] * ingredientVariables2[i] for i in foods]) >= FAT_MIN, "TotalFatMin"
probFull2 += lpSum([fatLevels[i] * ingredientVariables2[i] for i in foods]) <= FAT_MAX, "TotalFatMax"

probFull2 += lpSum([sodiumLevels[i] * ingredientVariables2[i] for i in foods]) >= SODIUM_MIN, "SodiumMin"
probFull2 += lpSum([sodiumLevels[i] * ingredientVariables2[i] for i in foods]) <= SODIM_MAX, "SodiumMax"

probFull2 += lpSum([carbLevels[i] * ingredientVariables2[i] for i in foods]) >= CARB_MIN, "CarbohydratesMin"
probFull2 += lpSum([carbLevels[i] * ingredientVariables2[i] for i in foods]) <= CARB_MAX, "CarbohydratesMax"

probFull2 += lpSum([dietaryFiberLevels[i] * ingredientVariables2[i] for i in foods]) >= FIBER_MIN, "DietaryFiberMin"
probFull2 += lpSum([dietaryFiberLevels[i] * ingredientVariables2[i] for i in foods]) <= FIBER_MAX, "DietaryFiberMax"

probFull2 += lpSum([proteinLevels[i] * ingredientVariables2[i] for i in foods]) >= PROTEIN_MIN, "ProteinMin"
probFull2 += lpSum([proteinLevels[i] * ingredientVariables2[i] for i in foods]) <= PROTEIN_MAX, "ProteinMax"

probFull2 += lpSum([vitAIULevels[i] * ingredientVariables2[i] for i in foods]) >= VITAMIN_A_MIN, "VitAMin"
probFull2 += lpSum([vitAIULevels[i] * ingredientVariables2[i] for i in foods]) <= VITAMIN_A_MAX, "VitAMax"

probFull2 += lpSum([vitCUILevels[i] * ingredientVariables2[i] for i in foods]) >= VITAMIN_C_MIN, "VitCMin"
probFull2 += lpSum([vitCUILevels[i] * ingredientVariables2[i] for i in foods]) <= VITAMIN_C_MAX, "VitCMax"

probFull2 += lpSum([calciumLevels[i] * ingredientVariables2[i] for i in foods]) >= CALCIUM_MIN, "CalciumMin"
probFull2 += lpSum([calciumLevels[i] * ingredientVariables2[i] for i in foods]) <= CALCIUM_MAX, "CalciumMax"

probFull2 += lpSum([ironLevels[i] * ingredientVariables2[i] for i in foods]) >= IRON_MIN, "ironMin"
probFull2 += lpSum([ironLevels[i] * ingredientVariables2[i] for i in foods]) <= IRON_MAX, "ironMax"

# in order to prep for part 2, let's make a copy of the problem variable and we'll append to it later
probFull3 = probFull2

probFull2.writeLP("Full1ArmyDiet2.lp")

# The problem is solved using PuLP's choice of Solver
probFull2.solve()


# The status of the solution is printed to the screen
print("----Third run with all menu options and specified FDA nutritional constraints----")
print "Status:", LpStatus[probFull2.status]

# Each of the variables is printed with it's resolved optimum value
for v in probFull2.variables():
    if v.varValue != 0.0:
        print v.name, "=", v.varValue

# we are looking for the following answer:
# Ingr_Celery,_Raw = 52.64371
# Ingr_Frozen_Broccoli = 0.25960653
# Ingr_Lettuce,Iceberg,Raw = 63.988506
# Ingr_Oranges = 2.2929389
# Ingr_Poached_Eggs = 0.14184397
# Ingr_Popcorn,Air_Popped = 13.869322
# Total price per day is $4.34

# and we get:

# The optimised objective function value is printed to the screen
print "Total Cost of Servings per meal = ", value(probFull2.objective)

# Status: Optimal
# _u'Celery,_Raw'_ = 52.64371
# _u'Frozen_Broccoli'_ = 0.25960653
# _u'Lettuce,Iceberg,Raw'_ = 63.988506
# _u'Oranges'_ = 2.2929389
# _u'Poached_Eggs'_ = 0.14184397
# _u'Popcorn,Air_Popped'_ = 13.869322
# Total Cost of Servings per meal =  4.3371167974


# ------------------------------------------------------------------

# 13.2 Part 2: adding more constraints
# If a food is selected, then a minimum of 1/10 serving must be chosen.
# (Hint: now you will need two variables for each food i: whether it is chosen, and how much is part of the diet.
# Youll also need to write a constraint to link them.
# Many people dislike celery and frozen broccoli. So at most one, but not both, can be selected.
# To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be selected.
# [If something is ambiguous (e.g., should bean-and-bacon soup be considered meat?),


# First, let's define a new binary variable for each of the menu option.
# This variable will be selected by the model to be 0 or 1 to optimize the objective function
# The objective function still stays the same, ie. most economical food.

chosenVariableBinaryValue = dict([("Binary" + x[0], LpVariable(["Select_" + x[0]], 0, None, LpBinary)) for x in dietDataList])

# Recall that we had made a copy of probFull2 into probFull3. We
# now we loop through all the menu options, and apply the constraints that

for f in foods:
    # - if the menu option is chosen, then its serving size is AT LEAST 0.1
    # - with a very high upper limit
    probFull3 += ingredientVariables2[f] >= 0.1*chosenVariableBinaryValue["Binary" + f]
    probFull3 += ingredientVariables2[f] <= 10000000*chosenVariableBinaryValue["Binary" + f]
    # we bound the higher amount so that if the chosen variable is ZERO, than the ingredient chosen
    # is NOT allowed to be >= 0.1*0, because you also have <= 0.1 , so it would FORCE the
    # variable to be zero.

    # we also need to select only one of the two yucky foods (Celery or Broccoli) at most:
    probFull3 += chosenVariableBinaryValue["Binary" + 'Celery, Raw'] \
                 + chosenVariableBinaryValue["Binary" + 'Frozen Broccoli'] <= 1

    # next, we have to select atleast 3 types of protein to get variety:
    probFull3 += lpSum(chosenVariableBinaryValue["Binary" + foods[k]] for k in [8,27,28,29,30,31,32,49,50,51,56,57,58,59,61,63]) >= 3


probFull3.writeLP("Full1ArmyDiet3.lp")

# The problem is solved using PuLP's choice of Solver
probFull3.solve()

# The status of the solution is printed to the screen
print("---15.2.2 further constraints : 0.1x, choice between yucky food etc -----")
print "Status:", LpStatus[probFull3.status]

# Each of the variables is printed with it's resolved optimum value
for v in probFull3.variables():
    if (v.varValue != 0.0) & ("Select" not in v.name) :
        print v.name, "=", v.varValue

# The optimised objective function value is printed to the screen
print "Total Cost of Servings per meal = ", value(probFull3.objective)

# the results published are:
# Status: Optimal
# _u'Celery,_Raw'_ = 42.399358
# _u'Kielbasa,Prk'_ = 0.1
# _u'Lettuce,Iceberg,Raw'_ = 82.802586
# _u'Oranges'_ = 3.0771841
# _u'Peanut_Butter'_ = 1.9429716
# _u'Poached_Eggs'_ = 0.1
# _u'Popcorn,Air_Popped'_ = 13.223294
# _u'Scrambled_Eggs'_ = 0.1
# Total Cost of Servings per meal =  4.512543427