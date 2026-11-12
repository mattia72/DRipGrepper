# Run directly with PowerShell; no network access, Delphi build, or real publishing.
$ErrorActionPreference = 'Stop'
$deployRoot = Split-Path $PSScriptRoot -Parent
foreach ($file in 'Deploy-DRipGrepper.ps1', 'GitHubReleaseUtils.ps1') {
    $tokens = $null
    $parseErrors = $null
    $ast = [System.Management.Automation.Language.Parser]::ParseFile((Join-Path $deployRoot $file), [ref]$tokens, [ref]$parseErrors)
    if ($parseErrors.Count) { throw ($parseErrors.Message -join '; ') }
    foreach ($definition in $ast.FindAll({
        param($node)
        $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and
        $node.Name -in @('Get-VersionsFromChangelog', 'Get-CurrentVersionChangesFromChangelog',
            'Update-ChangelogReleaseDate', 'New-ReleaseWithAsset', 'Update-ScoopManifest', 'New-Deploy', 'Add-AssetToRelease')
    }, $true)) {
        . ([scriptblock]::Create($definition.Extent.Text))
    }
}
$scoopImplementation = ${function:Update-ScoopManifest}
$uploadImplementation = ${function:Add-AssetToRelease}

function Assert-Equal($Actual, $Expected, [string]$Message) {
    if (($Actual -join '|') -cne ($Expected -join '|')) {
        throw "${Message}: expected [$($Expected -join ', ')], got [$($Actual -join ', ')]"
    }
}
function Assert-Throws([scriptblock]$Action, [string]$Pattern) {
    $caught = $false
    try { & $Action | Out-Null } catch {
        if ($_.Exception.Message -notlike $Pattern) { throw }
        $caught = $true
    }
    Assert-Equal $caught $true "Expected failure: $Pattern"
}

$global:Changelog = Join-Path ([IO.Path]::GetTempPath()) ('RipGrepper-DeployTest-' + [guid]::NewGuid().ToString('N') + '.md')
$global:Version = 'v5.1.0-beta'
$global:Token = 'test-placeholder'
$global:AssetsDirectory = 'mock-assets'
$global:AssetsDirectoryName = 'assets'
$global:Owner = 'test-owner'
$global:Repo = 'test-repo'
$global:Url = 'https://example.invalid/releases'
$global:headers = @{}
$today = (Get-Date).ToString('yyyy-MM-dd', [Globalization.CultureInfo]::InvariantCulture)
$original = "# Changelog`r`n`r`n## [v5.1.0-beta] - 2026-01-01`r`n`r`nUnicode: café 😀`r`n`r`n## [v5.0.0-beta] - 2025-12-01`r`n`r`nOld notes.`r`n"
$expected = $original.Replace('## [v5.1.0-beta] - 2026-01-01', "## [v5.1.0-beta] - $today")
try {
    foreach ($bom in $false, $true) {
        [IO.File]::WriteAllText($global:Changelog, $original, [Text.UTF8Encoding]::new($bom))
        Assert-Equal (Update-ChangelogReleaseDate -DryRun) $false 'Dry run reports no file change'
        Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) $original 'Dry run preserves changelog'
        Assert-Equal (Update-ChangelogReleaseDate) $true 'Date update reports a file change'
        Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) $expected 'Only newest date changes; CRLF and Unicode preserved'
        $bytes = [IO.File]::ReadAllBytes($global:Changelog)
        Assert-Equal ($bytes[0] -eq 0xEF) $bom 'Preserve UTF-8 BOM choice'
        Assert-Equal (Update-ChangelogReleaseDate) $false 'Current date reports no file change'
        Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) $expected 'Repeat date update is stable'
    }
    Assert-Throws { Update-ChangelogReleaseDate -Version 'v5.0.0-beta' } '*newest changelog entry does not match*'
    [IO.File]::WriteAllText($global:Changelog, $original.Replace(' - 2026-01-01', '').Replace("`r`n", "`n"))
    Assert-Equal (Update-ChangelogReleaseDate) $true 'Missing date reports a file change'
    Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) ($expected.Replace("`r`n", "`n")) 'Missing date and LF line endings'
    Write-Host 'PASS: Changelog date, dry run, version validation, line endings, encoding, and older entries'

    # Keep real release orchestration, stubbing only builds, remote operations, and Scoop.
    function New-StandaloneZips { @{ Dir = 'assets\Win32' }; @{ Dir = 'assets\Win64' } }
    function New-ExpertDllZip { @{ Dir = 'assets\Delphi13.Dll' } }
    function Get-ChildItem { }
    function List-Assets { }
    function Get-LatestBuildAssets {
        if (-not $script:noAssets) {
            [pscustomobject]@{ Name = 'app.zip'; FullName = 'app.zip' }
            [pscustomobject]@{ Name = 'extension.zip'; FullName = 'extension.zip' }
        }
    }
    function Get-Releases {
        if ($script:lookupFailure) { throw 'GitHub lookup failed' }
        if ($script:existingRelease) {
            @{ id = 123; assets = @(@{ name = 'app.zip' }, @{ name = 'extension.zip' }) }
        }
    }
    function New-Release {
        param($description)
        $script:events += 'release'
        if (-not $DryRun) {
            Assert-Equal $description.Contains("## [v5.1.0-beta] - $today") $true 'Release body uses deployment date'
        }
        if ($script:createFailure) { throw 'GitHub creation failed' }
        @{ id = 123 }
    }
    function New-ReleaseNotes { $script:events += 'notes' }
    function Add-AssetToRelease {
        param($zipFilePath)
        $script:events += "upload:$zipFilePath"
        if ($script:uploadFailure -and $zipFilePath -eq 'extension.zip') { return $false }
        return $true
    }
    function Update-ScoopManifest { $script:events += 'scoop' }

    $DeployToGitHub = $true
    $DryRun = $false
    $TestRepo = ''
    $UpdateScoopManifest = $false
    $LocalDeploy = $BuildStandalone = $BuildExtension = $BuildUIComponents = $BuildUnittest = $RunUnittest = $DeployToTransferDrive = $AddMissingAsset = $false

    [IO.File]::WriteAllText($global:Changelog, $original)
    $script:events = @()
    $Force = $true
    $UpdateScoopManifest = $true
    Assert-Throws { New-Deploy } '*CHANGELOG.md was updated. Commit and push*then run deployment again.*'
    Assert-Equal $script:events @() 'Changed changelog stops before release creation, notes, uploads, and Scoop, even with Force'
    Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) $expected 'Changed changelog remains ready to commit and push'
    $Force = $false
    $UpdateScoopManifest = $false
    New-Deploy | Out-Null
    Assert-Equal $script:events @('release', 'notes', 'upload:app.zip', 'upload:extension.zip', 'scoop') 'Scoop follows successful uploads'
    Write-Host 'PASS: A changed changelog stops deployment; rerunning with the current date proceeds'

    $UpdateScoopManifest = $true
    $script:existingRelease = $true
    [IO.File]::WriteAllText($global:Changelog, $original)
    $script:events = @()
    New-Deploy | Out-Null
    Assert-Equal $script:events @('scoop') 'Existing assets skip uploads; both flags run Scoop once'
    Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) $original 'Existing release keeps its date'
    $script:existingRelease = $false

    [IO.File]::WriteAllText($global:Changelog, $expected)
    foreach ($failure in 'lookupFailure', 'createFailure', 'uploadFailure', 'noAssets') {
        Set-Variable -Scope Script -Name $failure -Value $true
        $script:events = @()
        Assert-Throws { New-Deploy } '*'
        Assert-Equal ($script:events -contains 'scoop') $false "Scoop skipped on $failure, even with explicit switch"
        Set-Variable -Scope Script -Name $failure -Value $false
    }

    & {
        function Update-ScoopManifest { & $scoopImplementation }
        $DryRun = $true
        [IO.File]::WriteAllText($global:Changelog, $original)
        $script:events = @()
        New-Deploy | Out-Null
        Assert-Equal $script:events @('release', 'upload:app.zip', 'upload:extension.zip') 'Combined dry run skips real notes and Scoop operations'
        Assert-Equal ([IO.File]::ReadAllText($global:Changelog)) $original 'Combined dry run preserves changelog'
    }

    $TestRepo = 'sandbox-repo'
    $UpdateScoopManifest = $false
    $script:existingRelease = $true
    $script:events = @()
    New-Deploy | Out-Null
    Assert-Equal $script:events @() 'Test repository skips automatic Scoop publishing'
    $TestRepo = ''
    $DeployToGitHub = $false
    $UpdateScoopManifest = $true
    $script:events = @()
    New-Deploy | Out-Null
    Assert-Equal $script:events @('scoop') 'Standalone Scoop deployment remains available'
    Write-Host 'PASS: Release and upload ordering, existing release retries, failure gating, and standalone Scoop deployment'

    # Exercise the real Scoop function with native commands stubbed out.
    & {
        function Push-Location { $script:events += 'push-location' }
        function Pop-Location { $script:events += 'pop-location' }
        function Clear-ScoopTokenEnvironment { }
        function Set-ScoopAssetTemplates { $script:events += 'manifest' }
        function .\bin\checkver.ps1 { $script:events += 'checkver' }
        function Test-YesAnswer { $true }
        function git {
            $command = $args[0]
            $script:events += "git:$command"
            $global:LASTEXITCODE = if ($command -eq $script:gitFailure) { 2 } elseif ($command -eq 'diff' -and $script:hasChanges) { 1 } else { 0 }
        }
        function scoop { $script:events += 'scoop:update'; $global:LASTEXITCODE = 0 }
        $DryRun = $true
        $script:events = @()
        & $scoopImplementation
        Assert-Equal $script:events @() 'Scoop dry run performs no operations'
        $DryRun = $false
        $script:hasChanges = $true
        $script:events = @()
        & $scoopImplementation
        Assert-Equal $script:events @('push-location', 'manifest', 'checkver', 'git:add', 'git:diff', 'git:commit', 'git:push', 'pop-location', 'scoop:update') 'Scoop publishing order'
        $script:hasChanges = $false
        $script:events = @()
        & $scoopImplementation
        Assert-Equal ($script:events -contains 'git:commit') $false 'Unchanged manifest skips commit'
        Assert-Equal ($script:events -contains 'git:push') $true 'Retry still pushes an existing commit'
        $script:hasChanges = $true
        foreach ($command in 'add', 'diff', 'commit', 'push') {
            $script:gitFailure = $command
            $script:events = @()
            Assert-Throws { & $scoopImplementation } '*Failed to*'
            Assert-Equal $script:events[-1] 'pop-location' 'Scoop failure restores location and stops'
        }
    }
    Write-Host 'PASS: Scoop dry run, publishing order, unchanged manifests, and Git failures'

    & {
        function curl.exe {
            Assert-Equal ($args -contains '--fail') $true 'HTTP upload failures set a failing exit code'
            $global:LASTEXITCODE = 22
            'HTTP error fixture'
        }
        Assert-Throws { & $uploadImplementation -zipFilePath 'app.zip' -releaseID 123 } '*Failed to upload asset*'
    }
    Write-Host 'PASS: Upload HTTP failures stop deployment'
}
finally {
    Remove-Item -LiteralPath $global:Changelog -Force
}
