from pulp import *
import pandas as pd
import numpy as np

#-----------------------------------------------------------------------------------------------
#Part 3 - Find the low-cholesterol diet model with maximum and minimum daily nutrition constraints
#-----------------------------------------------------------------------------------------------


#skip first blank row
data = pd.read_excel("diet_largeSummer2018.xls", skiprows = 1, header = 0)

nrows = data.shape[0]
ncols = data.shape[1]

names = list(data.columns.values)
num_nutrients = ncols - 1

food_table = data[0: nrows - 4]
food_table = food_table.fillna(value = 0) #7146,31

min_values = data.iloc[nrows-3].values.tolist()
max_values = data.iloc[nrows-1].values.tolist()

# This will remove duplicate foods, and creates a dictionary with key as food names 
# and values as list of nutrients
food_data = food_table.set_index('Long_Desc').transpose().to_dict(orient='list') #6286
print((food_data["Butter, salted"])[0])
##0.85

#list of foodnames
foods = list(food_data.keys())

#create a dictionary with cholesterol for each food
cholesterol = dict()
for f in foods:
    cholesterol[f] = (food_data[f])[27]
    
#create nutrients list
food_nutrients = []
for i in range(0,num_nutrients): # for loop running through each nutrient
    food_nutrients.append(dict([(f, float((food_data[f])[i])) for f in foods])) # amount of nutrient i in food j




#Create 3rd optimization problem
problem3 = LpProblem('Minimize Cholesterol', LpMinimize)

foodVars = LpVariable.dicts("Foods", foods, 0)

#Create objective function
problem3 += lpSum([cholesterol[f] * foodVars[f] for f in foodVars]), 'Total Cholesterol'


#Add constraint for each nutrient
for i in range(0,num_nutrients):
    if (not np.isnan(min_values[i+1])) and (not np.isnan(max_values[i+1])): 
        problem3 += lpSum([food_nutrients [i][j] * foodVars[j] for j in foodVars]) >= min_values[i+1], 'Minimum '  + names[i+1]
        problem3 += lpSum([food_nutrients [i][j] * foodVars[j] for j in foodVars]) <= max_values[i+1], 'Maximum ' + names[i+1]
            


problem3.solve()


#Print solution in readable format
print("==== Solution - Part 3 ====")
for var in problem3.variables():
    if var.varValue > 0:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','').replace('_',' ' ))

print("Total Cholesterol in Diet = %.2f" % value(problem3.objective))
##==== Solution - Part 3 ====
##0.084602457 units of Beans, adzuki, mature seeds, raw
##0.12997145 units of Beans, pinto, mature seeds, raw
##0.16453428 units of GREEN GIANT, HARVEST BURGER, Original Flavor, All Vegetable Pro
##0.0021589366 units of Gelatin desserts, dry mix, reduced calorie, with aspartame, add
##0.88530624 units of Infant formula, ROSS, ISOMIL, with iron, powder, not reconstitu
##0.065999962 units of Leavening agents, baking powder, low sodium
##0.02545092 units of Miso
##0.05482638 units of Nuts, mixed nuts, oil roasted, with peanuts, with salt added
##1.411285 units of Oil, whale, bowhead (Alaska Native)
##0.017227144 units of Peanuts, all types, cooked, boiled, with salt
##0.069209616 units of Seeds, cottonseed kernels, roasted (glandless)
##0.015343761 units of Soup, clam chowder, manhattan style, dehydrated, dry
##0.57132848 units of Soybeans, mature seeds, dry roasted
##0.33481168 units of Spices, pepper, red or cayenne
##0.45760296 units of Tomatoes, sun dried
##9999.7193 units of Water, bottled, non carbonated, CALISTOGA
##Total Cholesterol in Diet = 0.00