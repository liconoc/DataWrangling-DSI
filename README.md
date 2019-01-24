# Automated Data Transformation with  Domain-Adaptive Inductive Programming

Data manipulation is crucial in the early stages of many applications where data quality is essential, from database integration to data science. Data can come in different formats and from different sources, with format variation making a tedious integration necessary. Given one or two examples, humans are good at understanding how to solve a data transformation problem independently of its domain, because they are able to detect what the problem is and to choose the appropriate background knowledge according to the context. For instance, presented with the string "8/17/2017" to be transformed to "17th of August of 2017", humans will process this in two steps: (1) they recognise that it is a date and (2) they map the date to the 17th of August of 2017. Inductive Programming (IP) aims at learning declarative (functional or logic) programs from examples. Two key advantages of IP are the use of background knowledge and the ability to synthesise programs from a few input/output examples (as humans do). 

In this project we propose to use IP as a means for automating repetitive data formatting tasks, frequently presented during the process of data wrangling in many data manipulation problems. Here we show that with the use of general-purpose declarative (programming) languages jointly with generic IP systems and the definition of domain-adaptive knowledge, many specific data wrangling problems from different application domains can be automatically solved from very few examples. We also propose an integrated benchmark for data wrangling, which we share publicly for the community.


## Benchmark

To encourage future research all the datasets are published as the first data wrangling dataset repository, openly available at http://dmip.webs.upv.es/datawrangling/index.html.

## Demo

An example of the system running in a website can be seen on: https://www.youtube.com/watch?v=wxFhXYyonOw