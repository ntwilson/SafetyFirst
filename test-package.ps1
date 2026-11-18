# Packs SafetyFirst into artifacts/ and runs PackageTests against that package.
param([string]$Version = '0.0.0-local')

$ErrorActionPreference = 'Stop'
$PSNativeCommandUseErrorActionPreference = $true

Push-Location $PSScriptRoot
try {
  # A REPACK REUSES THE VERSION NUMBER, SO EVICT THE CACHED COPY OR RESTORE KEEPS THE STALE ONE.
  Remove-Item artifacts/*.nupkg, artifacts/packages/safetyfirst -Recurse -Force -ErrorAction SilentlyContinue

  # CI MODE EVEN LOCALLY: OTHERWISE THE PDB RECORDS THIS MACHINE'S PATHS AND EDITORS OPEN THOSE FILES INSTEAD OF USING SOURCE LINK.
  dotnet pack SafetyFirst/SafetyFirst.fsproj -c Release -o artifacts -p:Version=$Version -p:ContinuousIntegrationBuild=true
  dotnet restore PackageTests/PackageTests.fsproj --force -p:SafetyFirstVersion=$Version

  dotnet test PackageTests/PackageTests.fsproj --no-restore -p:SafetyFirstVersion=$Version
}
finally {
  Pop-Location
}
