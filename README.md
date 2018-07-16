# Code for DRC project

## Section 1: how we've proceeded

###### Step 1 Handling SpatialPolygone object
- drop kinshasa
- merge three Bandundu's regions

###### Step 2 Dealing with the census

- merge census (residential/non-residential)
- add cluster's attributes to points 
  - first spatial merge (80557 points attributed)
  - second classic df merge (+2136 points attributed)

## Section 2: what is the output

After the two merges, 365 data points remain without any clusters information because of naming issue in the variable `cluster_id` in the census data
It corresponds to 20 clusters unidentified:

```
 cluster_id     
 [1] "0"               "1"               "102"             "11"              "124"             "129"            
 [7] "167"             "19"              "2"               "243"             "3"               "4"              
[13] "5"               "528"             "56"              "58"              "6"               "7"              
[19] "8"               "drc_kwango_0000"
`````