# Custom Network Input Format

The local live interface accepts a JSON object with three required fields:

```json
{
  "directed": false,
  "nodes": [
    {"id": "a", "attrs": {"group": "x", "score": 3}},
    {"id": "b", "attrs": {"group": "x", "score": 5}},
    {"id": "c", "attrs": {"group": "y", "score": 2}},
    {"id": "d", "attrs": {"group": "y", "score": 4}}
  ],
  "edges": [["a", "b"], ["b", "c"], ["c", "d"]]
}
```

Rules:

- `directed` is a Boolean.
- `nodes` contains at least four unique string IDs.
- `attrs` may contain numeric or categorical node attributes.
- Every edge endpoint must match a node ID.
- Self-loops are ignored by the local runner.
- The live demo caps inputs at 60 nodes and 400 edges.
- The current release handles binary directed or undirected networks only.

The domain description is supplied separately through the Actors, Tie Meaning,
and Constraint fields in the live interface.
