#!/bin/bash

dotnet tool restore
dotnet paket restore
dotnet build src/Aardvark.Geometry.Quadtree.sln
dotnet test src/Aardvark.Geometry.Quadtree.sln