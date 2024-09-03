## Changes and notes
* Regular maintenance update: 
    * Removed dependency on orphaned package 'taxize'.
    * Fixed minor bug that resulted in occasional errors in Windows.
    * Minor updates to documentation
    * Fixed malfunction in retrieve_calanguize_genomes() that let to it wiping 
    out existing files in the target directory.

## R CMD check results  
* Duration: 28.7s
* 0 errors | 0 warnings | 0 notes ; R CMD check succeeded

## Test environments (via Github actions)
* macOS 14.6.1 (R 4.4.1)
* Linux Ubuntu 20.04.4 (R 4.1.3, 4.2.3, 4.3.3, 4.4.1 and devel)
* Microsoft Windows Server 2022 - 10.0.20348 (R 4.2.3 and 4.4.1)
