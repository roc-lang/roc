$ErrorActionPreference = "Continue"

$variants = @(
    "MinimalGlue",
    "ConcatGlue",
    "InterpolationGlue",
    "LoopGlue",
    "TypesAccessGlue",
    "CGlue"
)

$iterations = 20

foreach ($variant in $variants) {
    $success = 0
    $fail = 0

    for ($i = 1; $i -le $iterations; $i++) {
        $tmpDir = New-Item -ItemType Directory -Path "$env:TEMP\glue_test_$([guid]::NewGuid())" -Force

        $proc = Start-Process -FilePath ".\zig-out\bin\roc.exe" `
            -ArgumentList "experimental-glue", "src\glue\src\$variant.roc", $tmpDir.FullName, "test\fx\platform\main.roc" `
            -NoNewWindow -Wait -PassThru -RedirectStandardOutput "$tmpDir\stdout.txt" -RedirectStandardError "$tmpDir\stderr.txt"

        if ($proc.ExitCode -eq 0) {
            $success++
        } else {
            $fail++
        }

        Remove-Item -Recurse -Force $tmpDir -ErrorAction SilentlyContinue
    }

    $rate = [math]::Round(($fail / $iterations) * 100, 1)
    Write-Host "$variant : Success=$success, Fail=$fail, FailRate=${rate}%"
}
