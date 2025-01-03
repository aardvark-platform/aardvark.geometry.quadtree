### 0.6.1
- update dependencies
- fix Sample.Position

### 0.6.0
- updated dependencies

### 0.5.9
- fixed special case in query merge code

### 0.5.8
- [builder] add build logic for yet more cases

### 0.5.7
- fix Sample.PositionsWithBounds (dict trygetvalue fails when key does not exist, probably due to uninitialized tuple)

### 0.5.6
- [builder] quadtree build fix (experimental)

### 0.5.5
- [builder] flatten layer fix

### 0.5.4
- [builder] builder now flattens multiple layer sets into one (at split limit tile size)
  - this fixes a previous sampling problem with layers having masks
  - this also massively improves performance for querying/sampling

### 0.5.3
- [builder] save/load, export/import

### 0.5.2
- [builder] fixed root cell for case when data is smaller than split limit tile size
- [materialize] fixed indexing for mask data

### 0.5.1
- updated dependencies (System.Collections.Immutable lowest_matching: true)

### 0.5.0
- same as 0.4.4
- see also new maintenance branch 4.2

### 0.4.4
- Builder (creates quadtree from many small patches)

### 0.4.3
- fix queries

### 0.4.2
- fix System.Collections.Immutable dependency (from 7 to 6)

### 0.4.1
- update packages (Aardvark.Data.Durable 0.3.9)
- update dotnet tools
- remove Newtonsoft.Json dependency
- update .github/workflows

### 0.4.0
- updated to Aardvark.Base 5.2