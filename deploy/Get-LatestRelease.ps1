$repoName = "mattia72/DripGrepper"
$assetPattern = "DripGrepper.x64*.zip"
$extractDirectory = "C:\Users\Public\Downloads"


$releasesUri = "https://api.github.com/repos/$repoName/releases/latest"
$asset = (Invoke-WebRequest $releasesUri | ConvertFrom-Json).assets | Where-Object name -like $assetPattern
$downloadUri = $asset.browser_download_url

$extractPath = [System.IO.Path]::Combine($extractDirectory, $asset.name)
Invoke-WebRequest -Uri $downloadUri -Out $extractPath