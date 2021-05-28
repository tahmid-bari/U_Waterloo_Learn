## MSCI 719 Final Exam
## Tahmid Bari - 20864394

import numpy as np
import pandas as pd
import random
import docplex.mp.model as cpx
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings("ignore")

# Read excel file and transform it to python dataframe
ForwardPlayers = pd.read_excel('MSCI719-FinalExam.xlsx', sheet_name = 0)

# Normalize shooting, heading and pressing scores
ForwardPlayers["Shooting_Norm"] = (ForwardPlayers["Shooting"]-ForwardPlayers["Shooting"].min())/(ForwardPlayers["Shooting"].max()-ForwardPlayers["Shooting"].min())
ForwardPlayers["Heading_Norm"] = (ForwardPlayers["Heading"]-ForwardPlayers["Heading"].min())/(ForwardPlayers["Heading"].max()-ForwardPlayers["Heading"].min())
ForwardPlayers["Pressing_Norm"] = (ForwardPlayers["Pressing"]-ForwardPlayers["Pressing"].min())/(ForwardPlayers["Pressing"].max()-ForwardPlayers["Pressing"].min())

# New Market Value is a linear combination of normalized indexes for each 
ForwardPlayers["New_Market_Value"]  = 10000000*((ForwardPlayers["Shooting_Norm"]+ForwardPlayers["Heading_Norm"]+ForwardPlayers["Pressing_Norm"])/3)

# Calculate the Overrated percentage index for each player. The lower the better
ForwardPlayers["Perc_Overated"] = (ForwardPlayers["Market Value"]-ForwardPlayers["New_Market_Value"])/ForwardPlayers["Market Value"]

# Split the players into market category based on their market value
conditions = [(ForwardPlayers['Market Value'] < 2000000),
    (ForwardPlayers['Market Value'] >= 2000000) & (ForwardPlayers['Market Value'] < 5000000),
    (ForwardPlayers['Market Value'] >= 5000000)]
values = [3, 2, 1]
ForwardPlayers['Market_Category'] = np.select(conditions, values)

# Sort players by overrated percentage
ForwardPlayers.sort_values(by=['Perc_Overated'], inplace=True)

# Get most overrated players for each category (2 for cat1, 8 for cat2 and 20 for cat3)
ShortList_MC1 = ForwardPlayers[ForwardPlayers['Market_Category']==1].head(2)
ShortList_MC2 = ForwardPlayers[ForwardPlayers['Market_Category']==2].head(8)
ShortList_MC3 = ForwardPlayers[ForwardPlayers['Market_Category']==3].head(20)

# List of players
Players = ShortList_MC1['PlayerID'].values.tolist() + ShortList_MC2['PlayerID'].values.tolist() + ShortList_MC3['PlayerID'].values.tolist()

ForwardPlayers.set_index('PlayerID', inplace=True)
ForwardPlayers_dict = ForwardPlayers.to_dict('index')

# List of skills
Skills = ["Shooting","Heading","Pressing"]


##### This portion of code for answering QD1, defining the model, adding up the constraints and results #####
##### Used similar pattern of coding from MSCI 603, as did a project on optimizing a model #####

# Define Mathematical model
opt_model = cpx.Model(name="MIP Model")

# Binary variable. 1 if player i is selected for skill j. 0 otherwise
x = {(i,j): opt_model.binary_var(name="x_{0}_{1}".format(i,j)) 
for i in Players for j in Skills}

# A player cannot be selected for two different categories
c1 = {i : 
opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for j in Skills) <= 1,
        ctname="c1_{0}".format(i))
    for i in Players}

# One player has to be selected for each category
c2 = {j : 
opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players) == 1,
        ctname="c2_{0}".format(j))
    for j in Skills}

# Average age of chosen players for shooting and pressing must be at most 22 years old
c3 = {i : 
opt_model.add_constraint(
        ct=ForwardPlayers_dict[i]["Age"]*x[i,"Shooting"] + ForwardPlayers_dict[i]["Age"]*x[i,"Pressing"] <= 22*2,
        ctname="c3_{0}".format(i))
    for i in Players}

# Budget of the club for these transfers is at most $8,500,000
c4 = opt_model.add_constraint(
        ct=opt_model.sum(ForwardPlayers_dict[i]["Market Value"]*x[i,j] for i in Players for j in Skills) <= 8500000,
        ctname="c4")

# Do not allow the club to sign more than one player from the second division
c5 = opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players for j in Skills if ForwardPlayers_dict[i]["Division"] == "Second Devision") <= 1,
        ctname="c5")

# Each team can get at most one player from “Market Category” 1
c6 = opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players for j in Skills if ForwardPlayers_dict[i]["Market_Category"] == 1) <= 1,
        ctname="c6")

# Objective function: Maximize skill score for each player and category
objective = opt_model.sum(ForwardPlayers_dict[i][j]*x[i,j] for i in Players for j in Skills)/3
opt_model.maximize(objective)
sol = opt_model.solve()

print("QD1_Results")
# Print results
for i in Players:
    for j in Skills:
        if(x[i,j].solution_value == 1):
            print("Player", i, " Selected for skill", j) 
print(" ")

# Get shortlist of players
ShortList_MC1 = ShortList_MC1.append(ShortList_MC2)
ShortList_MC1 = ShortList_MC1.append(ShortList_MC3)
ShortList_MC1.reset_index(inplace=True)
ShortList_MC1.drop(columns=['index','Shooting_Norm','Heading_Norm','Pressing_Norm','New_Market_Value'], inplace=True)
ShortList_MC1.drop(columns=['Market Value','Shooting','Heading','Pressing','Substitute player?','Division','Age','Height'], inplace=True)

# Prepare data for Output table
selected_shooting = []
selected_heading = []
selected_pressing = []
QD1_Results = ShortList_MC1.copy()
for index, row in ShortList_MC1.iterrows():
    selected_shooting.append(x[row['PlayerID'],"Shooting"].solution_value)
    selected_heading.append(x[row['PlayerID'],"Heading"].solution_value)
    selected_pressing.append(x[row['PlayerID'],"Pressing"].solution_value)

QD1_Results["Selected for Shooting?"] = selected_shooting
QD1_Results["Selected for Heading?"] = selected_heading
QD1_Results["Selected for Pressing?"] = selected_pressing
QD1_Results.to_csv("QD1_Results.csv")


##### This portion of code for answering QD1, defining the model, adding up the constraints and results #####
##### Used similar pattern of coding from MSCI 603, as did a project on optimizing a model #####

QD2_Results = ShortList_MC1.copy()
QD2_Results.reset_index(inplace=True)
QD2_Results.set_index("PlayerID", inplace=True)
qd2_results_dict = QD2_Results.to_dict('index')

# Define the possible teammate combinations and increase per skill
# Given in the data table from question QD2

comb_indexes = [0, 1, 2, 3, 4]

combinations = [[1,7],
[2,14],
[3,8],
[1,9],
[4,5,17]]

# 1 & 7, The Pressing skill of both players increases by 50% and The Shooting skill of both players increases by 20%
# 2 & 14, The Pressing skill of both players increases by 10%
# 3 & 8, All skills of both players increase by 30%
# 1 & 9, The Shooting skill of both players increases by 15%
# 4, 5 & 17, All skills of three players increase by 10%

increase = [{"Shooting":0.2, "Heading":0,"Pressing":0.5},
{"Shooting":0, "Heading":0,"Pressing":0.1},
{"Shooting":0.3, "Heading":0.3,"Pressing":0.3},
{"Shooting":0.15, "Heading":0,"Pressing":0},
{"Shooting":0.1, "Heading":0.1,"Pressing":0.1}]

# Define Mathematical model
opt_model = cpx.Model(name="MIP Model")


# Binary variable. 1 if player i is selected for skill j. 0 otherwise
x = {(i,j): opt_model.binary_var(name="x_{0}_{1}".format(i,j)) 
for i in Players for j in Skills}

# Binary variable. 1 if player combination is selected. 0 otherwise
y = {(c): opt_model.binary_var(name="y_{0}".format(c)) 
for c in comb_indexes}

# A player cannot be selected for two different categories
c1 = {i : 
opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for j in Skills) <= 1,
        ctname="c1_{0}".format(i))
    for i in Players}

# One player has to be selected for each category
c2 = {j : 
opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players) == 1,
        ctname="c2_{0}".format(j))
    for j in Skills}

# Average age of chosen players for shooting and pressing must be at most 22 years old
c3 = {i : 
opt_model.add_constraint(
        ct=ForwardPlayers_dict[i]["Age"]*x[i,"Shooting"] + ForwardPlayers_dict[i]["Age"]*x[i,"Pressing"] <= 22*2,
        ctname="c3_{0}".format(i))
    for i in Players}

# Budget of the club for these transfers is at most $8,500,000
c4 = opt_model.add_constraint(
        ct=opt_model.sum(ForwardPlayers_dict[i]["Market Value"]*x[i,j] for i in Players for j in Skills) <= 8500000,
        ctname="c4")

# Do not allow the club to sign more than one player from the second division
c5 = opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players for j in Skills if ForwardPlayers_dict[i]["Division"] == "Second Devision") <= 1,
        ctname="c5")

# Each team can get at most one player from “Market Category” 1
c6 = opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players for j in Skills if ForwardPlayers_dict[i]["Market_Category"] == 1) <= 1,
        ctname="c6")

# Constraint to define when a combination is selected
c7 = {c : 
opt_model.add_constraint(
        ct=opt_model.sum(x[i,j] for i in Players for j in Skills if qd2_results_dict[i]["index"] in combinations[c]) >= y[c]*len(combinations[c]),
        ctname="c7_{0}".format(c))
    for c in comb_indexes}

# Objective function: Maximize skill score for each player and category + increase by teammate combination selection
objective = opt_model.sum(increase[c][j]*ForwardPlayers_dict[i][j]*y[c] for c in comb_indexes for i in Players for j in Skills if qd2_results_dict[i]["index"] in combinations[c])/3 + opt_model.sum(ForwardPlayers_dict[i][j]*x[i,j] for i in Players for j in Skills)/3
opt_model.maximize(objective)
sol = opt_model.solve()

print("QD2_Results")

# Print results
for i in Players:
    for j in Skills:
        if(x[i,j].solution_value == 1):
            print("Player", i, " Selected for skill", j) 
for c in comb_indexes:
    if(y[c].solution_value == 1):
        print("Combination", c, " is selected")

# Prepare data for Output table
selected_shooting = []
selected_heading = []
selected_pressing = []
QD2_Results = ShortList_MC1.copy()
for index, row in ShortList_MC1.iterrows():
    selected_shooting.append(x[row['PlayerID'],"Shooting"].solution_value)
    selected_heading.append(x[row['PlayerID'],"Heading"].solution_value)
    selected_pressing.append(x[row['PlayerID'],"Pressing"].solution_value)

QD2_Results["Selected for Shooting?"] = selected_shooting
QD2_Results["Selected for Heading?"] = selected_heading
QD2_Results["Selected for Pressing?"] = selected_pressing
QD2_Results.to_csv("QD2_Results.csv")