;; A simulation of two different tree species based on a real-world model
;; used to explore forest succession and competition.
;; Developed and implemented by Matthew Ottomano and Ammar Almahdy.
;; following instructions provided by professor Mathew Dickerson for the class CS390.


; ============ INTERFACE GLOBALS ============ ;
; numberoftrees (slider input): an even-number slider that determines the initial number of trees in each species.
; ratio (output monitor): a monitor that keeps track of the ratio between the current numbers of the two species.
; fireProb (input slider): the probality of having a fire occur on a given year.
; overcrowing? (binary switch): a two-way switch that enables or disables the effects of the overcrowing procedure.
; harvestA (input slider): the percentage of harvestable trees that should be harvested in a given year.
; reproductionProb (input slider): controls the probability of all mature trees to produce a seed that hatches a tree.
; ============ END OF INTERFACE GLOBALS ============ ;



; ============ GLOBALS ============ ;

globals [
  numOfhardwoods ;number of living trees belonging to the species of hardwood trees.
  numOfsoftwoods ;number of living trees belonging to the species of softwood trees.
  max-diameter_hard ;the maximum diameter reached by a member of the hardwood species.
  max-diameter_soft ;the maximum diameter reached by a member of the softwood species.
  growthOfhardwoods ;the annual growth rate for hardwoods.
  growthOfsoftwoods ;the annual growth rate for softwoods.
  lifeOfhardwoods ;life expectancy or maximum age reached by hardwoods before dying.
  lifeOfsoftwoods ;life expectancy or maximum age reached by softwoods before dying.
  numOfharvests ;the number of hardwoods who have reached the age of maturity and whose diameter is greater than min-harvest-diam
  size-factor ;a constant that holds a factor by which all trees are scaled up.
  impact-radius ;determines the maximum distance seeds can travel and the impact of density-dependent death.
  max-density   ;determines the maximum density a block of area can have.
  harvested     ;number of trees that have been harvested
]

; ============ END OF GLOBALS ============ ;

; ************ BREEDS ************ ;

breed[ hardwoods hardwood]

breed[ hardseeds hardseed]

breed[ softwoods softwood]

breed[ softseeds softseed]

turtles-own [ age diameter ]

patches-own [ on-fire? fire-age fading? ]

; ************ END OF BREEDS ************ ;


; ------------------ PROCEDURES ------------------ ;

; == SETTING UP AND INITIATIONS

; OBSERVER CONTEXT
; sets up the world and initilizes all constants and plants the trees for the simulation
to setup
  ca
  reset-ticks
  ask patches [
    set pcolor white ;paint the system white
  ]
  init-constants ;gives all constants their values
  init-trees ;plants the trees and determines their properties
end

to init-constants
  ;Gives an initial value to the globals declared in the previous portions of the code.
  set numOfharvests 0;
  set numOfhardwoods numberoftrees ; sets the initial number of trees in both
  set numOfsoftwoods numberoftrees ;species to be equal the number required by the user.
  set max-diameter_hard 1.2 ;the maximum size reached by a hardwood before dying.
  set max-diameter_soft 1 ;the maximum size reached by a softwood before dying.
  set lifeOfhardwoods 150 ;the maximum life attained by hardwood trees.
  set lifeOfsoftwoods 100 ;the maximum life attained by softwood trees.
                          ;sets the annual growth rate for each species.
  set growthOfhardwoods max-diameter_hard / lifeOfhardwoods
  set growthOfsoftwoods max-diameter_soft / lifeOfsoftwoods
  set size-factor 3 ;controlls the scale factor that scales all the trees up.
  set impact-radius 3 ;maximum distance a seed can travel and impact of density-dependent death.
  set max-density 2.5 ;maximum density a block of area can have.
  set harvested 0     ; number of trees that have been harvested
end

to init-trees
  ;Sets up the trees and plants them at their initial spots for the simulation to start.
  create-hardwoods numberoftrees [
    ;creates the number of trees specified by the user.
    set shape "tree" ;sets shape to tree.
    set color yellow ;different color for each species.
                     ;uses random-cor instead of random-pcor to allow more than one tree to occupy the same patch.
    set xcor random-xcor
    set ycor random-ycor
    set age random 150 ;sets random age for each tree below the life-expectancy.
    set diameter growthOfhardwoods * age ;sets the diameter to match the current age of the tree at each given point in time.
    set size diameter * size-factor ;scales all trees up by the size-factor to enhance the visuals of the simulation and make it easier to observe.
  ]
  create-softwoods numberoftrees [
    ;creates the number of trees specified by the user.
    set shape "tree" ;sets shape to tree.
    set color green ;different color for each species.
                    ;uses random-cor instead of random-pcor to allow more than one tree to occupy the same patch.
    set xcor random-xcor
    set ycor random-ycor
    set age random 100 ;sets random age for each tree below the life-expectancy.
    set diameter growthOfhardwoods * age ;sets the diameter to match the current age of the tree at each given point in time.
    set size diameter * size-factor ;scales all trees up by the size-factor to enhance the visuals of the simulation and make it easier to observe.
  ]
  ask hardwoods with [age >= 25 and diameter > min-harvest-diam] [
    set numOfharvests numOfharvests + 1 ;counts the number of mature trees and ones over minimum diameter at the beginning of the simulation and keeps track of it.
  ]
end

;reports the ration between hardwoods and softwoods at all times.
to-report ratio
  let rat numOfhardwoods / numOfsoftwoods
  report rat
end

to nature
  tick
  ;the infinitely looping function that simulates the effect of time in the system
  ask patches with [ fading? = true ] [
    ;all patches who have been affected by a fire any time in the past 25 years.
    if on-fire? = true [ ;put the fire off and save the day
      set on-fire? false
    ]
    ;for those who are not on fire anymore but have not yet fully recovered from it.
    set fire-age fire-age - 1 ;augment the number of years spent in recovery and reduce the number of recovery years left.
    set pcolor pcolor + (1 / 5) ;fade the fire from the patch until it returns to normal.
    if fire-age = 0 [
      ;if patch has fully recovered from the fire.
      set pcolor white
      set fading? false
    ]
  ]
  ask hardwoods [
    ;update the trees properties based on the new year (size, diameter and age)
    set diameter growthOfhardwoods * age
    set size diameter * size-factor
    set age age + 1
    if age = 25 and diameter > min-harvest-diam  [ ;if the tree has reached maturity and can finally hatch seeds.
      set numOfharvests numOfharvests + 1 ;keep track of their amount in the system.
    ]
  ]

  ask softwoods [
    ;update the properties based on time
    set diameter growthOfsoftwoods * age
    set size diameter * size-factor
    set age age + 1]

  ;if age of tree equals the life-expectancy or max-life allowed for that species then tree should die.
  ask hardwoods with [ age = lifeOfhardwoods ] [

    set numOfhardwoods numOfhardwoods - 1
    if diameter > min-harvest-diam [       ;if the tree is harvestable
      set numOfharvests numOfharvests - 1

    ]
    die
  ]

  ask softwoods with [ age = lifeOfsoftwoods ] [
    set numOfsoftwoods numOfsoftwoods - 1
    die]

  reproduce ;procedure that controls each tree's reproduction abilities and probability

  mortality ;procedure that ensures no tree is cheating death and living more than its maxmimum allowed age

  harvest ;procedure that controls the means and probability of harvesting harvestable trees.

  if overcrowding? [
    ;procedure that excutes the instructions regarding having more than one tree growing on the same patch of land.
    overcrowding
  ]

  density-check ;density-dependent death

  fire ;procedue that controls the possiblity, effects and whereabouts of fires in the forest.
end

;; --Tree Procedures:

;OBSERVER CONTEXT
to reproduce
  ask hardwoods with [age >= 25][
    ;only mature trees can reproduce.
    let rand random 100
    if rand < reproductionProb [ ;if local probability is one at the given tick
      hatch-hardseeds 1 [
        ;send seed to travel with the wind around the tree
        set heading random 360
        fd random-float impact-radius
        hatch-hardwoods 1 [
          ;hatch tree at the same location of the seed scattered by the wind.
          set shape "tree"
          set color yellow
          set age 1
        ]
        die
      ]
      set numOfhardwoods numOfhardwoods + 1 ;add the newly born tree to the count
    ]
  ]

  ask softwoods with [age >= 25][ ;checking for mature trees
    let rand random 100
    if rand < reproductionProb [
      hatch-softseeds 1 [
        set heading random 360
        fd random-float impact-radius
        hatch-softwoods 1 [
          set shape "softtree"
          set color green
          set age 1
        ]
        die
      ]
      set numOfsoftwoods numOfsoftwoods + 1
    ]
  ]
end

;OBSERVER context
;controls the behavior of trees after they reach their life-expectancy or max-life allowed
;each species has a different mortality rate, i.e. probablities of death at each given year.
;species A, hardwoods, have a
to mortality
  ask turtles with [age >= 25] [
    let probDeath 0.6 ^ (lifeOfhardwoods - age)
    set probDeath probDeath * 100
    let rand random-float 100
    if rand < probDeath [
      ifelse breed = hardwoods [

        set numOfhardwoods numOfhardwoods - 1
        if diameter > min-harvest-diam [
          set numOfharvests numOfharvests - 1
        ]
      ]
      [
        set numOfsoftwoods numOfsoftwoods - 1
      ]
      die
    ]
  ]

  ask turtles with [age < 25] [
    let probDeath 0.3 ^ (age)
    set probDeath probDeath * 100
    let rand random-float 100
    if rand < probDeath [
      ifelse breed = hardwoods [
        set numOfhardwoods numOfhardwoods - 1
      ]
      [
        set numOfsoftwoods numOfsoftwoods - 1
      ]
      die
    ]
  ]
end

;OBSERVER context
;controls the behavior of harvestable trees
to harvest
  if p != 1.2 [ ;if the user specifies a harvest rate to work with
    let takendown numOfharvests * p
    while[takendown >= 1 and numOfharvests > 0] [
      ask one-of hardwoods with [age >= 25 and diameter > min-harvest-diam ] [ ;limiting harvestable trees to mature hardwoods with diameter greater than the minimum.
        set numOfharvests numOfharvests - 1
        set harvested harvested + 1
        set numOfhardwoods numOfhardwoods - 1
        set takendown takendown - 1
        die
      ]
    ]
  ]
end

;OBSERVER context
;overcrowding is when there is more than one mature tree growing on the same patch of land
to overcrowding
  ask turtles with [ age >= 25 ] [ ;only mature trees can perform overcrowding
    if any? other turtles-on patch-here [ ;only if there are other trees growing on the same patch
      ask min-one-of turtles-here with [age >= 25] [ diameter ] [ ;ask the one with the smallest diameter on that patch to die
        ifelse breed = hardwoods [

          if diameter > min-harvest-diam [
            set numOfharvests numOfharvests - 1
          ]
          set numOfhardwoods numOfhardwoods - 1
        ]
        [
          set numOfsoftwoods numOfsoftwoods - 1
        ]

        die
      ]
    ]
  ]
end

;OBSERVER context
;controls the possibility and effect of fires at each year. fire is one of the many possible natural disasters that should be taken into consideration in a simulation.
to fire
  let fireRand random 100
  if fireRand < fireProb [ ;if probability happens to decide this year a year of fire
    ask one-of patches [ ;pick a random patch
      ask patches in-radius 5 [ ;set all of its neighbors within distance 5 of its center to be set on fire
        set pcolor red ;visualization of the fire
        set on-fire? true
        set fire-age 25 ;patches take 25 years to recover completely from a fire
        set fading? true ;indicates that the patch is still recovering from fire and is not yet completely healed.
        ask turtles-here [ ;ask all turtles within the fire
          ifelse breed = hardwoods [ ;if the tree at hand is a hardwood
                                     ;as usual, different species have different survival probalities.
                                     ;hardwoods are less likely to survive during a fire than softwoods
            let probBurnHard .6 ^ (age)
            set probBurnHard probBurnHard * 100
            let randBurnHard random 100
            ifelse randBurnHard < probBurnHard [ ;if probability decides tree is to survive then don't do anything
            ]
            [ ;otherwise, the tree dies.
              set numOfhardwoods numOfhardwoods - 1
              if age >= 25 and diameter > min-harvest-diam [

                set numOfharvests numOfharvests - 1
              ]
              die
            ]
          ]
          [ ;if the tree is a softwood
            let probBurnSoft .95 ^ (age)
            set probBurnSoft probBurnSoft * 100
            let randBurnSoft random 100
            ifelse randBurnSoft < probBurnSoft [
            ]
            [
              set numOfsoftwoods numOfsoftwoods - 1
              die
            ]
          ]
        ]
      ]
    ]
  ]
end

to density-check  ;method for killing off trees in dense areas
  ask turtles with [age >= 25 ] [  ;targets mature trees
    let density [diameter] of self
    ask other turtles in-radius impact-radius [  ;targets trees in impact radius
      set density density + [diameter] of self   ;sums up the diameters of all trees
    ]
    set density (density / (9 * pi))             ;divides the total diameter by the area we are looking at to get the density
    if density > max-density [ ;if density is greater than the maximum allowed
      ask min-one-of turtles [diameter] [die]  ;ask the tree with the minimum distance to die
      show 4                                   ;proof that this indeed works
    ]
  ]
end











@#$#@#$#@
GRAPHICS-WINDOW
210
10
767
568
-1
-1
9.0
1
10
1
1
1
0
0
0
1
-30
30
-30
30
0
0
1
ticks
30.0

BUTTON
43
30
109
63
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
796
70
968
103
numberoftrees
numberoftrees
2
100
100.0
2
1
NIL
HORIZONTAL

BUTTON
40
86
112
119
NIL
nature
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
54
148
111
193
ratio
ratio
17
1
11

SLIDER
796
179
968
212
p
p
0
1
1.0
.01
1
NIL
HORIZONTAL

SWITCH
13
222
161
255
overcrowding?
overcrowding?
1
1
-1000

SLIDER
803
121
975
154
fireProb
fireProb
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
793
25
966
58
reproductionProb
reproductionProb
1
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
776
226
1058
259
min-harvest-diam
min-harvest-diam
0
1.2
0.2
.1
1
NIL
HORIZONTAL

@#$#@#$#@
## Names:
Ammar Almahdy and Matthew Ottomano, HW-07 for CS390.

## Project description by Professor Dickerson: 

This homework is inspired by (though significantly simplified from) a real-world model
used to explore forest succession and competition. Numbers, assumptions, and simplifications in this model are meant to help us learn NetLogo programming and principles of computer modeling, but are not representative of real tree species.

## What is this?

This is a simulation showing the competition between two different species of trees in a forest, hardwoods and softwoods. Each species has a different survival rate, a different resistance to natural disasters, a different life-expectancy, a different max-size, a different color and a different harvestability. The simulation focuses on showing the interaction and succession between the two species as a simplified example of what could be happening in the real world.


## How does it work? 

Using the interface, the user can choose the number of initial trees in the forst, the reproduction rate, the probability for natural disasters such as fire, the percentage of trees that should be harvested when possible, the minimum diameter of the trees to be harvested, and whether tree practice eliminating each other when a space is overcrowded. Once all values have been indicated, the user should setup the system by pressing the setup button. Once the forest is drawn and the trees are planted, the user should then press the button nature. Nature is a button that exempts the behavior of an infinitely running procedure that is supposed to imitate the concept and effects of time in the real world. Thus, every iteration of the procedure marks a new year in the simulation's calender, another way to refer to a year in this virtual world is by using the word tick.

As an improvement we've also implemented a density check which checks to see if a given area is too dense. If so, it will kill of the tree with the smallest diameter. This doesn't happen often when overcrowding is on since overcrowding controls how dense areas can be, however we see density playing a key role when overcrowding is off and areas become significantly more dense. 

## Improvements?

We understand that the program starts to run slowly once the number of trees increases substantially which implies that some optimazion is needed. The simulation makes several assumptions and simplifications and the less of these the more real and the more authentic our observation of the simulation could be useful and meaningful.

## Experimental Observations

In order to conduct our experiments, we created a variable to hold the overall number of trees harvested per run. Our experiments will then output the current ratio of SoftWoods against HardWoods, and the number of trees harvested at each step. We are using the behavior space in Netlogo to conduct our experiments.
Assumptions:
For simplicity, we have assumed a natural fire probability of 5% and a reproduction rate of 50%.
Experiment Variables:
We have decided to run multiple experiments with the following variables:
Overcrowding: True and False.
P: from 5 to 100 with increments of 5.
Min-harvest-diam: from 0 to 1.1 with increments of 0.1.
The experiments will take place with one controlled variable for each run. This will result in a total of 480 runs. Netlogo will output this data into an Excel file which we will use to evaluate the experiments.

Through thorough analysis of the data from our experiments, we found some interesting results. With every run, we took the 40th and 60th ratios of hardwoods to softwoods and used the formula abs(40th - 60th / 40th) in order to measure the consistency of the ratios; on a scale from 0 to 1, the closer to 0 the more consistent. We focused on only the runs with a consistency of .1 or less meaning that out of the 20 runs, it only experienced a 10 percent difference. While looking at these runs, we noticed that the consistency seemed to be .1 or less when a run reached a 0.2 min-harvest-diam. Every min-harvest-diam greater than this is negligible since a 0.2 gives the maximum harvest (revenue) and keeps the forest sustainable. Even as the p (proportion of trees to be harvest) increased, we continued to see consistency at 0.2 min-harvest-diam. The only difference was that the ratios decreased as p increased which makes sense since more trees are being harvested, however the ratio doesn’t really matter since we’re not looking at a model which gives the maximum amount of species A trees, rather we are looking for a sustainable model which is consistent and results in species A not going extinct. 

We also noticed that when overcrowding was on, the consistency started becoming .1 or less at 0.4 min-harvest-diam which makes sense since there are less harvestable trees when overcrowding is turned on. 

In conclusion, in order to reap the maximum benefits, we would suggest a proportion of 1 with a min-harvest of 0.2 assuming overcrowding is false. This would result in max harvest while keeping the forest sustainable. 


## Honor Code:

We have neither given nor received any unauthorized aid on this assignment. 
-AMMAR ALMAHDY
-MATTHEW OTTOMANO	
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

hardtree
false
0
Circle -7500403 true true 118 3 94
Rectangle -7500403 true true 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

softtree
false
0
Circle -7500403 true true 118 3 94
Rectangle -7500403 true true 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -7500403 true true 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>nature</go>
    <timeLimit steps="60"/>
    <metric>harvested</metric>
    <metric>ratio</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="numberoftrees">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproductionProb">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fireProb">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p" first="5" step="5" last="100"/>
    <enumeratedValueSet variable="overcrowding?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="min-harvest-diam" first="0" step="0.1" last="1.1"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
