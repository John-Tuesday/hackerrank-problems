# Mirko at the Construction Site

**difficulty** Advanced

[View on HackerRank](https://www.hackerrank.com/challenges/mirko-at-construction-site/problem)

## Prompt

Mirko is monitoring a construction site. He monitors buildings enumerated from to , starting from the left. For each building, he knows the current number of floors and the number of floors built on each day. He needs to know the answer to queries. The answer to each query is the index of the tallest building after days, as defined by the query. Your task is to help Mirko find the answers to these queries.

## General Strategy and Observations

b = Initial building height
m = Floors added per day
y = Total floors of a building

for each building there exists the line:
    mx+b=y

Precalculate when each building will be the tallest.
After enough days, the building with the highest slope (i.e. m value) will be the tallest.
Find the day that building becomes the tallest or tied for the tallest

1. Start at x=0
2. Find the winning index at `x` & add a discrete point noting such
3. Find the winning line just after `x` 
    - Record the open interval from starting this `x` and end at the next intersection 
    - This will guide us along the top until the next intersection where the winner may change
4. Set `x` as the next intersection of the winning line
5. Repeat steps 1-4 until the winning line meets its last intersection. That is to say,
    - It has the highest slope of all the lines and is currently winning
    - The intersections of the winning line and all other lines is less than `x`

