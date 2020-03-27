function Test-Administrator  
{  
    $user = [Security.Principal.WindowsIdentity]::GetCurrent();
    (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)  
}

Write-Output "==== Building Verishot ===="
dotnet publish -c release -r win-x64
Write-Output "==== Building Verishot complete! ====`n"

Write-Output "This script can add verishot to your PATH variables. (Requires Administrator session)"

$PROMPT_MSG = "Would you like to add verishot to PATH variables? (required for VSCode extension) [Y/n]"
$CONFIRMATION = Read-Host $PROMPT_MSG
while (($CONFIRMATION -ne "y") -and ($CONFIRMATION -ne "Y")) 
{
    if ($CONFIRMATION -eq "n" -or $CONFIRMATION -eq "N") 
    {
        Write-Output "WARNING: You will not be able to use the VSCode Extension!"
        Write-Output "All Done!"
        exit
    }
    $CONFIRMATION = Read-Host $PROMPT_MSG
}

$ADMIN = Test-Administrator

if (-not $ADMIN)
{
    Write-Output "ERROR: Please re-run this script with Administrator privileges to add verishot to PATH variables."
    exit
}

$REQD_DIR = $PSScriptRoot + "\bin\release\netcoreapp3.1\win-x64"

& .\scripts\Set-PathVariable.ps1 -NewLocation $REQD_DIR

Write-Output "`nAll Done!"