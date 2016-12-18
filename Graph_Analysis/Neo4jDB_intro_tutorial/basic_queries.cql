
// This examples and codes are the combination of :
// Coursera classes
// Graph Database book
// Various websites. StackOverFlow etc.


// ***************************
// CREATE A GRAPH
// ***************************


Five Nodes
N1 = Tom
N2 = Harry
N3 = Julian
N4 = Michele N5 = Josephine

Five Edges
e1 = Harry ‘is known by’ Tom
e2 = Julian ‘is co-worker of’ Harry e3 = Michele ‘is wife of’ Harry
e4 = Josephine ‘is wife of’ Tom
e5 = Josephine ‘is friend of’ Michele
==============
A simple text description of a graph
N1 - e1 -> N2 N2 - e2 -> N3 2 - e3 -> N4 N1 - e4 -> N5 N4 - e5 -> N5


A more technical text description of a graph
N1:ToyNode - e1 -> N2:ToyNode
N2 - e2 -> N3:ToyNode //N2 defined above no need to repat it.
N2 - e3 -> N4:ToyNode
N1 - e4 -> N5:ToyNode
N4 - e5 -> N5


Even more technical pseudo-code
N1:ToyNode - ToyRelation -> N2:ToyNode N2 - ToyRelation -> N3:ToyNode
N2 - ToyRelation -> N4:ToyNode
N1 - ToyRelation -> N5:ToyNode
N4 - ToyRelation -> N5

N1:ToyNode {name: 'Tom'} - ToyRelation {relationship: 'knows'} -> N2:ToyNode {name: 'Harry'}
N2 - ToyRelation {relationship: 'co-worker'} -> N3:ToyNode {name: 'Julian', job: 'plumber'}
N2 - ToyRelation {relationship: 'wife'}-> N4:ToyNode {name: 'Michele', job: 'accountant'}
N1 - ToyRelation {relationship: 'wife'} -> N5:ToyNode {name: 'Josephine', job: 'manager'}
N4 - ToyRelation {relationship: 'friend'} -> N5

create
(N1:ToyNode {name: 'Tom'}) - [:ToyRelation {relationship: 'knows'}] -> (N2:ToyNode {name: 'Harry'}),
(N2) - [:ToyRelation {relationship: 'co-worker'}] -> (N3:ToyNode {name: 'Julian', job: 'plumber'}),
(N2) - [:ToyRelation {relationship: 'wife'}] -> (N4:ToyNode {name: 'Michele', job: 'accountant'}),
(N1) - [:ToyRelation {relationship: 'wife'}] -> (N5:ToyNode {name: 'Josephine', job: 'manager'}),
(N4) - [:ToyRelation {relationship: 'friend'}] -> (N5)


// ***************************
// VIEW or DELETE OPERATIONS ON A GRAPH
// ***************************

View the resulting graph
match (n:ToyNode)-[r]-(m) return n, r, m
==============
Delete all nodes and edges
match (n)-[r]-() delete n, r
==============
Delete all nodes which have no edges
match (n) delete n
==============
Delete only ToyNode nodes which have no edges
match (n:ToyNode) delete n
==============
Delete all edges
match (n)-[r]-() delete r

Delete only ToyRelation edges
match (n)-[r:ToyRelation]-() delete r

Delete only ToyRelation edges and nodes
match (n)-[r:ToyRelation]-() delete r,n;

//Selecting an existing single ToyNode node
match (n:ToyNode {name:'Julian'}) return n


// ***************************
// ADDITION AND MODIFICATION ON A GRAPH
// ***************************


//Adding a Node Correctly
// first find the Juilian node, then merge with the datas, relationship edge and, new Node Joyce having type again ToyNode.
match (n:ToyNode {name:'Julian'})
merge (n)-[:ToyRelation {relationship: 'fiancee'}]->(m:ToyNode {name:'Joyce', job:'store clerk'})

//Adding a Node Incorrectly:
//The following command will create Julian-fiance-Joyce, seperatly. There will be a duplicate Julian. To delete this worng attemp, see the next codes
create (n:ToyNode {name:'Julian'})-[:ToyRelation {relationship: 'fiancee'}]->(m:ToyNode {name:'Joyce', job:'store clerk'})

//Correct your mistake by deleting the bad nodes and edge: In other words delete all the relations and node connected with Joyce.
match (n:ToyNode {name:'Joyce'})-[r]-(m) delete n, r, m

//Modify a Node’s Information. lets get a Node Harry and and a job attribute it.
match (n:ToyNode) where n.name = 'Harry' set n.job = 'drummer'
<id>:182 name:Harryjob:drummer,lead drummer
//the following return s three times, and set operations done three times ?
match (n)-[r]-(m) where n.name='Harry' set n.job=n.job+['lead guitarist']
<id>:182 name:Harryjob:drummer,lead guitarist,lead guitarist,lead guitarist



//Actual directory in mac: ~/Neo4j/default.graphDB/import/neo4j_module_datasets/test.csv
LOAD CSV WITH HEADERS FROM "file:///neo4j_module_datasets/test.csv" AS line
MERGE (n:MyNode {Name:line.Source})
MERGE (m:MyNode {Name:line.Target})
MERGE (n) -[:TO {dist:line.distance}]-> (m)


//to show more nodes
:config maxNeighbours: 10000


// ***************************
// GENERATION of DESCRIPTIVE STATISTICS OF A GRAPH,
// ANALYSE THE PROPERTIES OF GRAPH
// ***************************


//Counting the number of nodes
match (n:MyNode)
return count(n)

//Counting the number of edges
match (n:MyNode)-[r]->()
return count(r)

//Finding leaf nodes:
match (n:MyNode)-[r:TO]->(m)
where not ((m)-->())
return m

//Finding root nodes:
match (m)-[r:TO]->(n:MyNode)
where not (()-->(m))
return m

//Finding triangles:
match (a)-[:TO]->(b)-[:TO]->(c)-[:TO]->(a)
return distinct a, b, c

//Finding 2nd neighbors of D:
match (a)-[:TO*..2]-(b)
where a.Name='D'
return distinct a, b


//Finding all properties of a node:
match (n:Actor)
return * limit 20

//Finding loops:
match (n)-[r]->(n)
return n, r limit 10

//Finding the loops after 3 or less edges
match (n)-[r:TO*..4]->(n)
return n, r limit 10

//Finding multigraphs:
match (n)-[r1]->(m), (n)-[r2]-(m)
where r1 <> r2
return n, r1, r2, m limit 10

//Finding the induced subgraph given a set of nodes:
match (n)-[r:TO]-(m)
where n.Name in ['A', 'B', 'C', 'D', 'E'] and m.Name in ['A', 'B', 'C', 'D', 'E']
return n, r, m



// ***************************
// PATH ANALYSIS ON A GRAPH
// ***************************

//Viewing the graph
match (n:MyNode)-[r]->(m)
return n, r, m

//Finding paths between specific nodes*:
match p=(a)-[:TO*]-(c)
where a.Name='H' and c.Name='P'
return p limit 1
//or for shortest path
match p=(a)-[:TO*]-(c) where a.Name='H' and c.Name='P' return p order by length(p) asc limit 1

//Finding the length between specific nodes:
match p=(a)-[:TO*]-(c)
where a.Name='H' and c.Name='P'
return length(p) limit 1

//Finding a shortest path between specific nodes:

match p=shortestPath((a)-[:TO*]-(c))
where a.Name='A' and c.Name='P'
return p, length(p) limit 1

//All Paths:
MATCH p = ((source)-[r:TO*]-(destination))
WHERE source.Name='A' AND destination.Name = 'P'
RETURN EXTRACT(n IN NODES(p)| n.Name) AS Paths

//All Shortest Paths:
MATCH p = allShortestPaths((source)-[r:TO*]-(destination))
WHERE source.Name='A' AND destination.Name = 'P'
RETURN EXTRACT(n IN NODES(p)| n.Name) AS Paths

//All Shortest Paths with Path Conditions:

MATCH p = allShortestPaths((source)-[r:TO*]->(destination))
WHERE source.Name='A' AND destination.Name = 'P' AND LENGTH(NODES(p)) > 5
RETURN EXTRACT(n IN NODES(p)| n.Name) AS Paths,length(p)


//shortestPath of the between all distinct nodes.
match (n:MyNode), (m:MyNode)
where n <> m
with n, m
match p=shortestPath((n)-[*]->(m))
return n.Name, m.Name, length(p)

//Diameter of the graph:
match (n:MyNode), (m:MyNode)
where n <> m
with n, m
match p=shortestPath((n)-[*]->(m))
return n.Name, m.Name, length(p)
order by length(p) desc limit 1

//Extracting and computing with node and properties:

match p=(a)-[:TO*]-(c)
where a.Name='H' and c.Name='P'
return extract(n in nodes(p)|n.Name) as Nodes, length(p) as pathLength,
reduce(s=0, e in relationships(p)| s + toInt(e.dist)) as pathDist limit 1

//Dijkstra's algorithm for a specific target node:

MATCH (from: MyNode {Name:'A'}), (to: MyNode {Name:'P'}),
path = shortestPath((from)-[:TO*]->(to))
WITH REDUCE(dist = 0, rel in rels(path) | dist + toInt(rel.dist)) AS distance, path
RETURN path, distance

//Dijkstra's algorithm SSSP:

MATCH (from: MyNode {Name:'A'}), (to: MyNode),
path = shortestPath((from)-[:TO*]->(to))
WITH REDUCE(dist = 0, rel in rels(path) | dist + toInt(rel.dist)) AS distance, path, from, to
RETURN from, to, path, distance order by distance desc

//Graph not containing a selected node:

match (n)-[r:TO]->(m)
where n.Name <> 'D' and m.Name <> 'D'
return n, r, m

//Shortest path over a Graph not containing a selected node:

match p=shortestPath((a {Name: 'A'})-[:TO*]-(b {Name: 'P'}))
where not('D' in (extract(n in nodes(p)|n.Name)))
return p, length(p)

//Graph not containing the immediate neighborhood of a specified node:
match (d {Name:'D'})-[:TO]-(b)
with collect(distinct b.Name) as neighbors
match (n)-[r:TO]->(m)
where
not (n.Name in (neighbors+'D'))
and
not (m.Name in (neighbors+'D'))
return n, r, m;

match (d {Name:'D'})-[:TO]-(b)-[:TO]->(leaf)
where not((leaf)-->())
return (leaf);

match (d {Name:'D'})-[:TO]-(b)<-[:TO]-(root)
where not((root)<--())
return (root)

//Graph not containing a selected neighborhood:
match (a {Name: 'F'})-[:TO*..2]-(b)
with collect(distinct b.Name) as MyList
match (n)-[r:TO]->(m)
where not(n.Name in MyList) and not (m.Name in MyList)
return distinct n, r, m



// ***************************
// CONNECTIVITY ANALYSIS OF A GRAPH 
// ***************************
//Viewing the graph
match (n:MyNode)-[r]->(m)
return n, r, m

// Find the outdegree of all nodes, last union is to print leaf node too.
match (n:MyNode)-[r]->()
return n.Name as Node, count(r) as Outdegree
order by Outdegree
union
match (a:MyNode)-[r]->(leaf)
where not((leaf)-->())
return leaf.Name as Node, 0 as Outdegree

// Find the indegree of all nodes
match (n:MyNode)<-[r]-()
return n.Name as Node, count(r) as Indegree
order by Indegree
union
match (a:MyNode)<-[r]-(root)
where not((root)<--())
return root.Name as Node, 0 as Indegree

// Find the degree of all nodes
match (n:MyNode)-[r]-()
return n.Name, count(distinct r) as degree
order by degree

// Find degree histogram of the graph
match (n:MyNode)-[r]-()
with n as nodes, count(distinct r) as degree
return degree, count(nodes) order by degree asc

//Save the degree of the node as a new node property
match (n:MyNode)-[r]-()
with n, count(distinct r) as degree
set n.deg = degree
return n.Name, n.deg

// Construct the Adjacency Matrix of the graph
match (n:MyNode), (m:MyNode)
return n.Name, m.Name,
case
when (n)-->(m) then 1
else 0
end as value

// Construct the Normalized Laplacian Matrix of the graph
match (n:MyNode), (m:MyNode)
return n.Name, m.Name,
case
when n.Name = m.Name then 1
when (n)-->(m) then -1/(sqrt(toInt(n.deg))*sqrt(toInt(m.deg)))
else 0
end as value
