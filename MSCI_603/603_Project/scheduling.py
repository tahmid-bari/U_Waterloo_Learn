import gurobipy as grb

AwayGames = 1
HomeGames = 1
Divisions = [1,2]
MaxReplays = {1:1000, 2:1000}
#Teams_Division = {1:["Islamabad United","Karachi Kings","Lahore Qalandars",
                    #2:["Multan Sultans","Peshawar Zalmi","Quetta Gladiators"]
Teams_Division = {1:["Islamabad United","Karachi Kings","Lahore Qalandars","Jhelum Cats","Bahawalpur Panthers"], #"Jhelum Cats","Bahawalpur Panthers"
                    2:["Multan Sultans","Peshawar Zalmi","Quetta Gladiators","Gujranwla Lions","Balochistan Birds"]} #"Gujranwla Lions","Balochistan Birds"
                        #2:["Multan Sultans","Peshawar Zalmi","Quetta Gladiators"]} #"Gujranwla Lions","Balochistan Birds"

Weeks = []
Times = []
Days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
DaysNext = {
"Sunday":"Monday",
"Monday":"Tuesday",
"Tuesday":"Wednesday",
"Wednesday":"Thursday",
"Thursday":"Friday",
"Friday":"Saturday",
"Saturday":"Sunday"}
num_weeks = 5
time_min = 15
time_max = 23
for i in range(1, num_weeks+1):
    Weeks.append(i)
for i in range(time_min, time_max):
    Times.append(i)

#Fields_with_Lights = ["National Stadium (Karachi)", "Gaddafi Stadium (Lahore)", "Multan Cricket Stadium (Multan)", "Rawalpindi Cricket Stadium (Islamabad)"]
#Fields_without_Lights = ["Peshawar Stadium (Peshawar) without Lights", "Quetta Stadium (Quetta) without Lights"]
#Fields = Fields_with_Lights + Fields_without_Lights
#Fields = ["National Stadium (Karachi)"] #"Gaddafi Stadium (Lahore)", "Multan Cricket Stadium (Multan)"
Fields = ["National Cricket Stadium (Karachi)", "Gaddafi Stadium (Lahore)", "Multan Cricket Stadium (Multan)",
            "Rawalpindi Cricket Stadium (Islamabad)", "Peshawar Cricket Stadium (Peshawar)", "Quetta Cricket Stadium (Quetta)",
                "Jhelum Cricket Stadium  (Jhelum)", "Bahawalpur Cricket Stadium (Bahawalpur)", "Gujranwla Cricket Stadium (Gujranwla)","Balochistan Cricket Stadium (Balochistan)"]

#Fields = ["National Stadium (Karachi)", "Gaddafi Stadium (Lahore)", "Multan Cricket Stadium (Multan)"]
#Fields = ["National Stadium (Karachi)"]

Home_fields = {
"Islamabad United": "Rawalpindi Cricket Stadium (Islamabad)",
"Karachi Kings": "National Stadium (Karachi)",
"Lahore Qalandars": "Gaddafi Stadium (Lahore)",
"Multan Sultans": "Multan Cricket Stadium (Multan)",
"Peshawar Zalmi": "Peshawar Cricket Stadium (Peshawar)",
"Quetta Gladiators": "Quetta Cricket Stadium (Quetta)",
"Jhelum Cats": "Jhelum Cricket Stadium  (Jhelum)",
"Bahawalpur Panthers": "Bahawalpur Cricket Stadium (Bahawalpur)",
"Gujranwla Lions": "Gujranwla Cricket Stadium (Gujranwla)",
"Balochistan Birds": "Balochistan Cricket Stadium (Balochistan)"}

# Calling the optimization Model
opt_model = grb.Model(name="MIP Model")

# Variables: Binary variable which is 1
x  = {(i,j,k,w,d,t,f): opt_model.addVar(vtype=grb.GRB.BINARY, 
                        name="x Division {0}:" "{1} vs {2}" " (Week): {3}" " on {4}" " (Time): {5} hrs." "(Field): {6}".format(i,j,k,w,d,t,f))
for i in Divisions 
for j in Teams_Division[i] 
for k in Teams_Division[i]
for w in Weeks
for d in Days
for t in Times
for f in Fields}

#Constraint (0): This is the objective function not a constraint. In the paper, objective function were divided into 2 parts
c1 = {(i,j,w,d,t,f) :
opt_model.addConstr(
        lhs=x[i,j,j,w,d,t,f],
        sense=grb.GRB.EQUAL,
        rhs=0, 
        name="c1_{0}_{1}_{2}_{3}_{4}_{5}".format(i,j,w,d,t,f))
    for i in Divisions for j in Teams_Division[i] for w in Weeks for d in Days for t in Times for f in Fields}

#Constraint (1a): Each team should play a specified number of away games.
c2 = {(i,k) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] for j in Teams_Division[i] for w in Weeks for d in Days for t in Times for f in Fields),
        sense=grb.GRB.GREATER_EQUAL,
        rhs=AwayGames, 
        name="c2_{0}_{1}".format(i,k))
    for i in Divisions for k in Teams_Division[i]}

#Constraint (1b): Each team should play a specified number of home games.
c3 = {(i,j) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] for k in Teams_Division[i] for w in Weeks for d in Days for t in Times for f in Fields),
        sense=grb.GRB.GREATER_EQUAL,
        rhs=HomeGames, 
        name="c3_{0}_{1}".format(i,j))
    for i in Divisions for j in Teams_Division[i]}

#Constraint (2a): Each team should play at least one game per week.
c4 = {(i,j,w) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for k in Teams_Division[i] for d in Days for t in Times for f in Fields),
        sense=grb.GRB.GREATER_EQUAL,
        rhs=1, 
        name="c4_{0}_{1}_{2}".format(i,j,w))
    for i in Divisions for j in Teams_Division[i] for w in Weeks}

#Constraint (2b): Each team should play no more than two games, per week.
c5 = {(i,j,w) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for k in Teams_Division[i] for d in Days for t in Times for f in Fields),
        sense=grb.GRB.LESS_EQUAL,
        rhs=2, 
        name="c5_{0}_{1}_{2}".format(i,j,w))
    for i in Divisions for j in Teams_Division[i] for w in Weeks}

#Constraint (3): Each team can play only one game per day.
c6 = {(i,j,w,d) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for k in Teams_Division[i] for t in Times for f in Fields),
        sense=grb.GRB.LESS_EQUAL,
        rhs=1, 
        name="c6_{0}_{1}_{2}_{3}".format(i,j,w,d))
    for i in Divisions for j in Teams_Division[i] for w in Weeks for d in Days}

#Constraint (4): Only one game can be played on a field at a time.
c7 = {(w,d,t,f) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] for i in Divisions for j in Teams_Division[i] for k in Teams_Division[i]),
        sense=grb.GRB.LESS_EQUAL,
        rhs=1, 
        name="c7_{0}_{1}_{2}_{3}".format(w,d,t,f))
    for w in Weeks for d in Days for t in Times for f in Fields}

#Constraint (7): Each team should play the same team as few times as possible.
c8 = {(i,j,k) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for w in Weeks for d in Days for t in Times for f in Fields),
        sense=grb.GRB.LESS_EQUAL,
        rhs=MaxReplays[i], 
        name="c8_{0}_{1}_{2}".format(i,j,k))
    for i in Divisions for j in Teams_Division[i] for k in Teams_Division[i]}

#Constraint (8): Each team should play one game “under the lights.”
#c9 = {(i,j,k) :
#opt_model.addConstr(
        #lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for w in Weeks for d in Days if d != "Saturday" for t in Times for f in Fields if f in Fields_with_Lights and t>=19),
        #sense=grb.GRB.GREATER_EQUAL,
        #rhs=1,
        #name="c9_{0}_{1}_{2}".format(i,j,k))
    #for i in Divisions for j in Teams_Division[i] for k in Teams_Division[i]}

# #Constraint (10): Teams should not play on consecutive days.
c10 = {(i,j,w,d) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] + x[i,j,k,w,DaysNext[d],t,f] + x[i,k,j,w,DaysNext[d],t,f] for k in Teams_Division[i] for t in Times for f in Fields),
        sense=grb.GRB.LESS_EQUAL,
        rhs=1,
        name="c10_{0}_{1}_{2}_{3}".format(i,j,w,d))
    for i in Divisions for j in Teams_Division[i] for w in Weeks for d in Days if d != "Saturday"}

#Constraint (19): Schedule that each team should play every other team in its division at least once.
#c11 = {(i,j,k) :
#opt_model.addConstr(
        #lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for w in Weeks for d in Days for t in Times for f in Fields),
        #sense=grb.GRB.GREATER_EQUAL,
        #rhs=1,
        #name="c11_{0}_{1}_{2}".format(i,j,k))
    #for i in Divisions for j in Teams_Division[i] for k in Teams_Division[i]}

#Constraint (20): Do not schedule a team to play the same team twice in the same week.
c12 = {(i,j,k,w) :
opt_model.addConstr(
        lhs=grb.quicksum(x[i,j,k,w,d,t,f] + x[i,k,j,w,d,t,f] for d in Days for t in Times for f in Fields),
        sense=grb.GRB.LESS_EQUAL,
        rhs=1, 
        name="c12_{0}_{1}_{2}_{3}".format(i,j,k,w))
    for i in Divisions for j in Teams_Division[i] for k in Teams_Division[i] for w in Weeks}

# Objective Function: Minimize total time
objective = grb.quicksum(x[i,j,k,w,d,t,f] for i in Divisions 
for j in Teams_Division[i] 
for k in Teams_Division[i]
for w in Weeks
for d in Days
for t in Times
for f in Fields)
opt_model.ModelSense = grb.GRB.MINIMIZE
opt_model.setObjective(objective)

# Run model
opt_model.optimize()

# Printing results - only the row and column index of selected cells
for v in opt_model.getVars():
  if(v.x > 0):
   #print('%s %g' % (v.varName, v.x))
   print((v.varName))