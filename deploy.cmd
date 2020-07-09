@echo off
SETLOCAL
PUSHD %~dp0

dotnet build src/Aardvark.Geometry.Quadtree.sln --configuration Release

git tag %1
git push --tags

dotnet paket pack bin --version %1

set /p Key=<%HOMEPATH%\.ssh\nuget.key
dotnet nuget push "bin/**.%1.nupkg" --skip-duplicate --source https://api.nuget.org/v3/index.json -k %Key%
