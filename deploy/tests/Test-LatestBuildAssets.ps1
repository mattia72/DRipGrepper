# Run directly with PowerShell; no GitHub access, Delphi installation, or Pester required.
$ErrorActionPreference = 'Stop'

$deployScript = Join-Path (Split-Path $PSScriptRoot -Parent) 'Deploy-DRipGrepper.ps1'
$tokens = $null
$parseErrors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile($deployScript, [ref]$tokens, [ref]$parseErrors)
if ($parseErrors.Count) {
    throw ($parseErrors.Message -join '; ')
}

# Load only the functions under test, avoiding deployment and environment initialization.
foreach ($name in @('Test-DelphiDeploymentEnabled', 'Get-DelphiName', 'Get-InstalledDelphiVersions',
    'Get-LastInstalledDelphiVersion', 'Get-LatestBuildAssets', 'Get-BinaryAssetVersion', 'New-StandaloneZips', 'New-ExpertDllZip')) {
    $definition = $ast.Find({
        param($node)
        $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq $name
    }, $true)
    if (-not $definition) { throw "Missing function: $name" }
    # Dynamically created script blocks need the original script root explicitly.
    $quotedScriptRoot = "'" + (Split-Path $deployScript -Parent).Replace("'", "''") + "'"
    . ([scriptblock]::Create($definition.Extent.Text.Replace('$PSScriptRoot', $quotedScriptRoot)))
}

function Assert-Equal($Actual, $Expected, [string]$Message) {
    if (($Actual -join '|') -cne ($Expected -join '|')) {
        throw "${Message}: expected [$($Expected -join ', ')], got [$($Actual -join ', ')]"
    }
}

$tempRoot = [System.IO.Path]::GetFullPath([System.IO.Path]::GetTempPath())
$testRoot = Join-Path $tempRoot ('RipGrepper-AssetTests-' + [guid]::NewGuid().ToString('N'))
[System.IO.Directory]::CreateDirectory($testRoot) | Out-Null

try {
    $names = @(
        'DRipGrepper.x64.v4.16.0.999-beta.zip',
        'DRipGrepper.x64.v5.0.0-beta.zip',
        'DRipGrepper.x64.v5.0.0.9-beta.zip',
        'DRipGrepper.x64.v5.0.0.10-beta.zip',
        'DRipGrepper.x64.v5.0.0.100-beta.zip',
        'DRipGrepper.x86.v5.0.0.10-beta.zip',
        'DRipExtension.Dll.x86.Delphi11.v5.0.0.9-beta.zip',
        'DRipExtension.Dll.x86.Delphi11.v5.0.0.10-beta.zip',
        'DRipExtension.Dll.x86.Delphi12.v5.0.0.9-beta.zip',
        'DRipExtension.Dll.x86.Delphi12.v5.0.0.10-beta.zip',
        'DRipExtension.Dll.x86.Delphi13.v5.0.0.8-beta.zip',
        'DRipExtension.Dll.x64.Delphi13.v5.0.0.7-beta.zip',
        'DRipExtension.Bpl.x86.Delphi12.v5.0.0.6-beta.zip',
        'DRipExtension.Bpl.x86.Delphi11.v5.0.0.6-beta.zip',
        'unrecognized.zip',
        'notes.txt'
    )
    foreach ($name in $names) {
        [System.IO.File]::WriteAllText((Join-Path $testRoot $name), 'fixture')
    }
    $nested = Join-Path $testRoot 'nested'
    [System.IO.Directory]::CreateDirectory($nested) | Out-Null
    $nestedFile = Join-Path $nested 'DRipGrepper.x64.v5.0.0.9999-beta.zip'
    [System.IO.File]::WriteAllText($nestedFile, 'nested fixture')

    $expected = @(
        'DRipGrepper.x64.v5.0.0.100-beta.zip',
        'DRipGrepper.x86.v5.0.0.10-beta.zip',
        'DRipExtension.Dll.x86.Delphi11.v5.0.0.10-beta.zip',
        'DRipExtension.Dll.x86.Delphi13.v5.0.0.8-beta.zip',
        'DRipExtension.Dll.x64.Delphi13.v5.0.0.7-beta.zip',
        'DRipExtension.Bpl.x86.Delphi11.v5.0.0.6-beta.zip'
    ) | Sort-Object

    $selected = @(Get-LatestBuildAssets -Path $testRoot -WarningAction SilentlyContinue)
    Assert-Equal ($selected.Name | Sort-Object) $expected 'Numeric selection and independent binary groups'
    Assert-Equal @(Get-ChildItem -LiteralPath $testRoot -File).Count $names.Count 'Selection keeps files'
    Write-Host 'PASS: Numeric version/build selection, legacy names, and independent binary groups'

    $selected = @(Get-LatestBuildAssets -Path $testRoot -RemoveOlderBuilds -DryRun -WarningAction SilentlyContinue)
    Assert-Equal ($selected.Name | Sort-Object) $expected 'Dry-run selection'
    Assert-Equal @(Get-ChildItem -LiteralPath $testRoot -File).Count $names.Count 'Dry run keeps files'
    Write-Host 'PASS: Dry run selects newest builds without deleting files'

    $selected = @(Get-LatestBuildAssets -Path $testRoot -RemoveOlderBuilds -WarningAction SilentlyContinue)
    Assert-Equal ($selected.Name | Sort-Object) $expected 'Cleanup selection'
    $remaining = @(Get-ChildItem -LiteralPath $testRoot -File | Select-Object -ExpandProperty Name | Sort-Object)
    $pausedAssets = @($names | Where-Object { $_ -like '*Delphi12*' })
    Assert-Equal $remaining (@($expected) + $pausedAssets + @('unrecognized.zip', 'notes.txt') | Sort-Object) 'Cleanup preserves winners, paused assets, and unrelated files'
    Assert-Equal (Test-Path -LiteralPath $nestedFile) $true 'Cleanup preserves nested files'
    Write-Host 'PASS: Cleanup deletes only older recognized ZIPs and preserves unrelated/nested files'
    Write-Host 'PASS: Paused Delphi 12 DLL/BPL archives are excluded from deployment and preserved locally'

    $empty = Join-Path $testRoot 'empty'
    [System.IO.Directory]::CreateDirectory($empty) | Out-Null
    Assert-Equal @(Get-LatestBuildAssets -Path $empty -RemoveOlderBuilds).Count 0 'Empty directory'
    Write-Host 'PASS: Empty assets directory'

    foreach ($name in @('Delphi12', 'Delphi12.Dll', 'Delphi12.Bpl')) {
        Assert-Equal (Test-DelphiDeploymentEnabled -Name $name) $false "Paused compiler/asset directory $name"
    }
    foreach ($name in @('Delphi11', 'Delphi13.Dll', 'DRipGrepper.x64.v5.0.0.100-beta.zip')) {
        Assert-Equal (Test-DelphiDeploymentEnabled -Name $name) $true "Active deployment item $name"
    }

    # Mock only registry access in a child scope, retaining real discovery and caching.
    & {
        $script:registryVersions = @('23.0', '37.0')
        function Test-Path { param($Path) return $Path -eq 'HKLM:\SOFTWARE\Embarcadero\BDS' }
        function Get-ChildItem {
            param($Path)
            $script:registryVersions | ForEach-Object { [pscustomobject]@{ PSChildName = $_ } }
        }
        $global:InstalledDelphiVersions = @()
        Assert-Equal @((Get-InstalledDelphiVersions).Data.Dir) @('Delphi13') 'Discovery skips paused compiler'
        Assert-Equal (Get-LastInstalledDelphiVersion).Data.Dir 'Delphi13' 'Build compiler selection'
        Assert-Equal @((Get-InstalledDelphiVersions).Data.Dir) @('Delphi13') 'Cached discovery'
        $global:InstalledDelphiVersions = @()
        Assert-Equal @((Get-LastInstalledDelphiVersion).Data.Dir) @('Delphi13') 'Uncached build compiler selection returns one compiler'

        $script:registryVersions = @('23.0')
        $global:InstalledDelphiVersions = @()
        $threw = $false
        try { Get-InstalledDelphiVersions | Out-Null } catch {
            $threw = $_.Exception.Message -like '*No Delphi versions with active support*'
        }
        Assert-Equal $threw $true 'Only a paused compiler installed fails clearly'
    }
    Write-Host 'PASS: Compiler discovery and build selection exclude Delphi 12, including when it is the only installation'

    $global:Version = 'v5.0.0-beta'
    Assert-Equal (Get-BinaryAssetVersion -FileVersion '5.0.0.481') 'v5.0.0.481-beta' 'EXE version'
    Assert-Equal (Get-BinaryAssetVersion -FileVersion '5.0.0.470') 'v5.0.0.470-beta' 'DLL version'
    $global:Version = 'v5.0.0'
    Assert-Equal (Get-BinaryAssetVersion -FileVersion '5.0.0.482') 'v5.0.0.482' 'Stable version'
    $threw = $false
    try { Get-BinaryAssetVersion -FileVersion '5.0.0' | Out-Null } catch { $threw = $true }
    Assert-Equal $threw $true 'Missing binary build is rejected'
    Write-Host 'PASS: Archive versions preserve release suffix and use each binary build'

    # Verify actual archive creation uses each EXE/DLL version, not a shared build.
    $global:Version = 'v5.0.0-beta'
    $global:AssetsDirectory = $testRoot
    $global:AssetZipName = 'DRipGrepper.{0}.{1}.zip'
    $global:AssetExpertZipName = 'DRipExtension.Dll.{0}.{1}.{2}.zip'
    $global:StandaloneAppName = 'DRipGrepper.exe'
    $BuildConfig = 'Release'
    $script:archiveNames = @()
    function Get-LastInstalledDelphiVersion { @{ Data = @{ Dir = 'Delphi13' } } }
    function Get-InstalledDelphiVersions { @{ Data = @{ Dir = 'Delphi13' } } }
    function Get-ExpertDllBaseName { 'DripExtensionD13' }
    function Add-ToAssetsDir {
        param($AssetDir, $AssetItemPath, [switch]$Win64, [switch]$ReplaceContent)
        if ($AssetItemPath -like '*.map') { return }
        $fileVersion = if ($AssetItemPath -like '*.dll') { '5.0.0.471' } elseif ($Win64) { '5.0.0.481' } else { '5.0.0.480' }
        [pscustomobject]@{ Version = $fileVersion }
    }
    function Compress-Archive {
        param($Path, $CompressionLevel, $DestinationPath, $Force)
        $script:archiveNames += Split-Path $DestinationPath -Leaf
    }
    New-StandaloneZips | Out-Null
    New-ExpertDllZip | Out-Null
    Assert-Equal ($script:archiveNames | Sort-Object) (@(
        'DRipGrepper.x86.v5.0.0.480-beta.zip',
        'DRipGrepper.x64.v5.0.0.481-beta.zip',
        'DRipExtension.Dll.x86.Delphi13.v5.0.0.471-beta.zip'
    ) | Sort-Object) 'Per-binary archive names'
    Write-Host 'PASS: EXE and DLL packaging uses each binary build number'
}
finally {
    # Validate the exact temporary target before recursive cleanup.
    $resolved = [System.IO.Path]::GetFullPath($testRoot)
    if ((Split-Path $resolved -Parent).TrimEnd('\') -ne $tempRoot.TrimEnd('\') -or
        (Split-Path $resolved -Leaf) -notlike 'RipGrepper-AssetTests-*') {
        throw "Refusing cleanup outside the test temporary directory: $resolved"
    }
    Remove-Item -LiteralPath $resolved -Recurse -Force
}
