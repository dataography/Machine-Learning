// Load the data.

LOAD CSV WITH HEADERS FROM "file:///neo4j_module_datasets_coursera/gene_gene_associations_50k.csv" AS line
MERGE (n:TrialGene {Name:line.OFFICIAL_SYMBOL_A})
MERGE (m:TrialGene {Name:line.OFFICIAL_SYMBOL_B})
MERGE (n) -[:TO {AssociationType:line.EXPERIMENTAL_SYSTEM}]-> (m)

// Lets count the all Nodes
MATCH (n)
RETURN count(n)

// Lets count all relationships
MATCH ()-->() RETURN count(*);

// Lets list the relationships types

// Lets display contraints and indexes
:schema

//Lets create an index on Name which has a node type TrialGene
create index on :TrialGene(Name)

// List all associations Types
MATCH (n) WHERE EXISTS(n.AssociationType)
RETURN DISTINCT "node" as element, n.AssociationType AS AssociationType LIMIT 25
UNION ALL
MATCH ()-[r]-() WHERE EXISTS(r.AssociationType)
RETURN DISTINCT "relationship" AS element, r.AssociationType AS AssociationType LIMIT 25

// Lets count the number of distinct paths from USP7 to UBE2D1
match p=(n)-[r*]->(m) where n.Name='USP7' and m.Name='UBE2D1' return count(p)

// lets count plot tha graphs of at most 3rd degree neighbors of Node USP7
match p=(n)-[r*..3]-(m) where n.Name='USP7' and m.Name='UBE2D1' return p

// Get the list of degrees of top 10 Nodes
match (n:TrialGene)-[r]-()
with n as nodes, count(distinct r) as degree
return degree, count(nodes) order by degree asc limit 10;
