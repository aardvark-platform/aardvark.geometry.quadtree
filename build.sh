#!/bin/bash

dotnet tool restore
dotnet paket restore
dotnet build src/Aardworx.Identity.sln --configuration Release
