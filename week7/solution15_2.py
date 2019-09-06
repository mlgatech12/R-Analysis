from pulp import *
import pandas as pd

data = pd.read_excel("dietSummer2018.xls")

data_food = data[0:64]

print(data_food.columns)
##Index(['Foods', 'Price/ Serving', 'Serving Size', 'Calories', 'Cholesterol mg',
##       'Total_Fat g', 'Sodium mg', 'Carbohydrates g', 'Dietary_Fiber g',
##      'Protein g', 'Vit_A IU', 'Vit_C IU', 'Calcium mg', 'Iron mg'],
##      dtype='object')

#Create dictionary of foods, with key as food and value is list of 13 values of food
foods = data_food.set_index('Foods').transpose().to_dict(orient='list')
print(foods["Frozen Broccoli"])
##[0.16, '10 Oz Pkg', 73.8, 0.0, 0.8, 68.2, 13.6, 8.5, 8.0, 5867.4, 160.2, 159.0, 2.3]

#get the minimum nutrition intake

data_constraints = data[65:68]
min_calories = data_constraints.iloc[0]["Calories"]
min_cholesterol = data_constraints.iloc[0]["Cholesterol mg"]
min_fat = data_constraints.iloc[0]["Total_Fat g"]
min_sodium = data_constraints.iloc[0]["Sodium mg"]
min_carbs = data_constraints.iloc[0]["Carbohydrates g"]
min_fiber = data_constraints.iloc[0]["Dietary_Fiber g"]
min_protein = data_constraints.iloc[0]["Protein g"]
min_vit_a = data_constraints.iloc[0]["Vit_A IU"]
min_vit_c = data_constraints.iloc[0]["Vit_C IU"]
min_calcium = data_constraints.iloc[0]["Calcium mg"]
min_iron = data_constraints.iloc[0]["Iron mg"]

#get the maximum nutrition intake
max_calories = data_constraints.iloc[1]["Calories"]
max_cholesterol = data_constraints.iloc[1]["Cholesterol mg"]
max_fat = data_constraints.iloc[1]["Total_Fat g"]
max_sodium = data_constraints.iloc[1]["Sodium mg"]
max_carbs = data_constraints.iloc[1]["Carbohydrates g"]
max_fiber = data_constraints.iloc[1]["Dietary_Fiber g"]
max_protein = data_constraints.iloc[1]["Protein g"]
max_vit_a = data_constraints.iloc[1]["Vit_A IU"]
max_vit_c = data_constraints.iloc[1]["Vit_C IU"]
max_calcium = data_constraints.iloc[1]["Calcium mg"]
max_iron = data_constraints.iloc[1]["Iron mg"]

#-----------------------------------------------------------------------------------------------
#Part 1 - Find the cheapest diet that satisfies maximum and minimum daily nutrition constraints
#-----------------------------------------------------------------------------------------------

problem1 = LpProblem("Cheapest Diet", LpMinimize)

#Define variables

#This decision variable represents how much food we eat consume. 
#Lower limit is 0 since we can't eat negative of anything.
foodVars = LpVariable.dicts("Foods",foods,0)

#Create objective function
problem1 += lpSum([(foods[f])[0] * foodVars[f] for f in foodVars]), "Total Cost"


#Add constraints for all nutrients
problem1 += lpSum([(foods[f])[2]* foodVars[f] for f in foodVars]) >= min_calories, "Minimum Calories"
problem1 += lpSum([(foods[f])[2]* foodVars[f] for f in foodVars]) <= max_calories, "Maximum Calories"


problem1 += lpSum([(foods[f])[3]* foodVars[f] for f in foodVars]) >= min_cholesterol, "Minimum Cholesterol"
problem1 += lpSum([(foods[f])[3]* foodVars[f] for f in foodVars]) <= max_cholesterol, "Maximum Cholesterol"

problem1 += lpSum([(foods[f])[4]* foodVars[f] for f in foodVars]) >= min_fat, "Minimum Fat"
problem1 += lpSum([(foods[f])[4]* foodVars[f] for f in foodVars]) <= max_fat, "Maximum Fat"

problem1 += lpSum([(foods[f])[5]* foodVars[f] for f in foodVars]) >= min_sodium, "Minimum Sodium"
problem1 += lpSum([(foods[f])[5]* foodVars[f] for f in foodVars]) <= max_sodium, "Maximum Sodium"

problem1 += lpSum([(foods[f])[6]* foodVars[f] for f in foodVars]) >= min_carbs, "Minimum Carbs"
problem1 += lpSum([(foods[f])[6]* foodVars[f] for f in foodVars]) <= max_carbs, "Maximum Carbs"

problem1 += lpSum([(foods[f])[7]* foodVars[f] for f in foodVars]) >= min_fiber, "Minimum Fiber"
problem1 += lpSum([(foods[f])[7]* foodVars[f] for f in foodVars]) <= max_fiber, "Maximum Fiber"

problem1 += lpSum([(foods[f])[8]* foodVars[f] for f in foodVars]) >= min_protein, "Minimum Protein"
problem1 += lpSum([(foods[f])[8]* foodVars[f] for f in foodVars]) <= max_protein, "Maximum Protein"

problem1 += lpSum([(foods[f])[9]* foodVars[f] for f in foodVars]) >= min_vit_a, "Minimum Vitamin A"
problem1 += lpSum([(foods[f])[9]* foodVars[f] for f in foodVars]) <= max_vit_a, "Maximum Vitamin A"

problem1 += lpSum([(foods[f])[10]* foodVars[f] for f in foodVars]) >= min_vit_c, "Minimum Vitamin C"
problem1 += lpSum([(foods[f])[10]* foodVars[f] for f in foodVars]) <= max_vit_c, "Maximum Vitamin C"

problem1 += lpSum([(foods[f])[11]* foodVars[f] for f in foodVars]) >= min_calcium, "Minimum Calcium"
problem1 += lpSum([(foods[f])[11]* foodVars[f] for f in foodVars]) <= max_calcium, "Maximum Calcium"

problem1 += lpSum([(foods[f])[12]* foodVars[f] for f in foodVars]) >= min_iron, "Minimum Iron"
problem1 += lpSum([(foods[f])[12]* foodVars[f] for f in foodVars]) <= max_iron, "Maximum Iron"

problem1.solve()
#print(problem1.objective)

#Print solution in readable format
print("==== Solution - Part 1 ====")
for var in problem1.variables():
    if var.varValue > 0:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','').replace('_',' ' ))

print("Total cost of food = $%.2f" % value(problem1.objective))
##52.64371 units of Celery, Raw
##0.25960653 units of Frozen Broccoli
#63.988506 units of Lettuce,Iceberg,Raw
##2.2929389 units of Oranges
##0.14184397 units of Poached Eggs
##13.869322 units of Popcorn,Air Popped
##Total cost of food = $4.34

#-----------------------------------------------------------------------------------------------
#Part 2 - Find the cheapest diet that satisfies maximum and minimum daily nutrition constraints
#-----------------------------------------------------------------------------------------------
problem2 = LpProblem("Optimal Diet", LpMinimize)

#This decision variable represents how much food we eat consume. 
#Lower limit is 0 since we can't eat negative of anything.
foodVars = LpVariable.dicts("Foods",foods,0)
#Create binary varible to represent if each food is selected
foodVars_selected = LpVariable.dicts("Foods Selected",foods, 0, 1, LpBinary)

#Create objective function
problem2 += lpSum([(foods[f])[0] * foodVars[f] for f in foodVars]), "Total Cost"

#Add constraints for all nutrients
problem2 += lpSum([(foods[f])[2]* foodVars[f] for f in foodVars]) >= min_calories, "Minimum Calories"
problem2 += lpSum([(foods[f])[2]* foodVars[f] for f in foodVars]) <= max_calories, "Maximum Calories"


problem2 += lpSum([(foods[f])[3]* foodVars[f] for f in foodVars]) >= min_cholesterol, "Minimum Cholesterol"
problem2 += lpSum([(foods[f])[3]* foodVars[f] for f in foodVars]) <= max_cholesterol, "Maximum Cholesterol"

problem2 += lpSum([(foods[f])[4]* foodVars[f] for f in foodVars]) >= min_fat, "Minimum Fat"
problem2 += lpSum([(foods[f])[4]* foodVars[f] for f in foodVars]) <= max_fat, "Maximum Fat"

problem2 += lpSum([(foods[f])[5]* foodVars[f] for f in foodVars]) >= min_sodium, "Minimum Sodium"
problem2 += lpSum([(foods[f])[5]* foodVars[f] for f in foodVars]) <= max_sodium, "Maximum Sodium"

problem2 += lpSum([(foods[f])[6]* foodVars[f] for f in foodVars]) >= min_carbs, "Minimum Carbs"
problem2 += lpSum([(foods[f])[6]* foodVars[f] for f in foodVars]) <= max_carbs, "Maximum Carbs"

problem2 += lpSum([(foods[f])[7]* foodVars[f] for f in foodVars]) >= min_fiber, "Minimum Fiber"
problem2 += lpSum([(foods[f])[7]* foodVars[f] for f in foodVars]) <= max_fiber, "Maximum Fiber"

problem2 += lpSum([(foods[f])[8]* foodVars[f] for f in foodVars]) >= min_protein, "Minimum Protein"
problem2 += lpSum([(foods[f])[8]* foodVars[f] for f in foodVars]) <= max_protein, "Maximum Protein"

problem2 += lpSum([(foods[f])[9]* foodVars[f] for f in foodVars]) >= min_vit_a, "Minimum Vitamin A"
problem2 += lpSum([(foods[f])[9]* foodVars[f] for f in foodVars]) <= max_vit_a, "Maximum Vitamin A"

problem2 += lpSum([(foods[f])[10]* foodVars[f] for f in foodVars]) >= min_vit_c, "Minimum Vitamin C"
problem2 += lpSum([(foods[f])[10]* foodVars[f] for f in foodVars]) <= max_vit_c, "Maximum Vitamin C"

problem2 += lpSum([(foods[f])[11]* foodVars[f] for f in foodVars]) >= min_calcium, "Minimum Calcium"
problem2 += lpSum([(foods[f])[11]* foodVars[f] for f in foodVars]) <= max_calcium, "Maximum Calcium"

problem2 += lpSum([(foods[f])[12]* foodVars[f] for f in foodVars]) >= min_iron, "Minimum Iron"
problem2 += lpSum([(foods[f])[12]* foodVars[f] for f in foodVars]) <= max_iron, "Maximum Iron"

for food in foods:
    #Add constraint - If a food is selected, then a minimum of 1/10 serving must be chosen
    problem2 += foodVars[food] >= 0.1 * foodVars_selected[food]
    #If any food is eaten, its binary variable must be 1
    problem2 += foodVars_selected[food] >= 0.0000001 * foodVars[food]


#Add Constraint - Many people dislike celery and frozen broccoli. 
#So at most one, but not both, can be selected.
problem2 += foodVars_selected['Frozen Broccoli'] + foodVars_selected['Celery, Raw'] <= 1

#Add Constraint - 
#To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must be selected
problem2 += foodVars_selected['Roasted Chicken'] + foodVars_selected['Poached Eggs'] + foodVars_selected['Scrambled Eggs'] + \
foodVars_selected['Bologna,Turkey'] + foodVars_selected['Frankfurter, Beef'] + foodVars_selected['Ham,Sliced,Extralean'] + \
foodVars_selected['White Tuna in Water'] + foodVars_selected['Beanbacn Soup,W/Watr']  + foodVars_selected['Pizza W/Pepperoni'] + \
foodVars_selected['Hamburger W/Toppings'] + foodVars_selected['Chicknoodl Soup'] + foodVars_selected['Splt Pea&Hamsoup'] + \
foodVars_selected['Pork'] + foodVars_selected['Vegetbeef Soup'] + foodVars_selected['Neweng Clamchwd'] + foodVars_selected['New E Clamchwd,W/Mlk'] >= 3


problem2.solve()
#print(problem1.objective)

#Print solution in readable format
print("==== Solution - Part 2 ====")
for var in problem2.variables():
    if var.varValue > 0 and "Selected" not in var.name:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','').replace('_',' ' ))

print("Total cost of food = $%.2f" % value(problem2.objective))
##0.1 units of Bologna,Turkey
##42.423026 units of Celery, Raw
##82.673927 units of Lettuce,Iceberg,Raw
##3.0856009 units of Oranges
##1.9590978 units of Peanut Butter
##0.1 units of Poached Eggs
##13.214473 units of Popcorn,Air Popped
##0.1 units of Scrambled Eggs
##Total cost of food = $4.51