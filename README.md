# kanjistat

This repository contains the R package 

**kanjistat: A Statistical Framework for the Analysis of Japanese Kanji Characters**

As of April 8, 2023, this first public version is still a work in progress as more testing is required and more introductory material has to be added.  

Comments, suggestions and contributions are very welcome...



## Installation

kanjistat is most conveniently installed directly from R by saying
```
remotes::install_github("dschuhmacher/kanjistat", build_vignettes=TRUE)
```
The function `kanjidist` requires currently via its use of the suggested package `ROI.plugin.glpk` that you have the [GNU Linear Programming Kit](https://www.gnu.org/software/glpk/) installed.



## Getting started

Suggested readings are the vignette `getting_started` and the help for `kanjivec` and `kanjidist`:
```
library(kanjistat)
vignette("getting_started", package="kanjistat")
?kanjivec
?kanjidist
```

Note that a file with `kanjivec` objects for all Jōyō kanji can be obtained from the [kanjistat.data repository](https://github.com/dschuhmacher/kanjistat.data).



## License 

`kanjistat` is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

A copy of the GNU General Public License is in the file LICENSE.md
and at <https://www.gnu.org/licenses/>.



## Attribution of prior work


The code in the file `R/svgparser_lite` is slightly altered from R package  
`svgparser` v0.1.2 --- Copyright &copy; 2021 <mikefc@coolbutuseless.com>  
made available under an MIT License at  
<https://github.com/coolbutuseless/svgparser>

The data sets `kbase`, `kmorph` and `kreadmean` are mostly derived from  
KANJIDIC2 --- Copyright &copy; James William Breen and The Electronic Dictionary
              Research and Development Group (EDRDG)  
made available under a [CC BY-SA 4.0 License](https://creativecommons.org/licenses/by-sa/4.0/) at  
<https://www.edrdg.org/wiki/index.php/KANJIDIC_Project>

The variable `components` of data set `kmorph` is derived from  
RADKFILE/KRADFILE --- Copyright &copy; James William Breen and The Electronic
                      Dictionary Research and Development Group (EDRDG)  
made available under a [CC BY-SA 4.0 License](https://creativecommons.org/licenses/by-sa/4.0/) at  
<http://www.edrdg.org/wiki/>

The variables `jlpt`, `frank` and `idc` of data sets `kbase` and `kmorph` are taken from  
Kanjium --- Copyright &copy; Uros O. on any additions or modifications
            from previous sources  
made available under a [CC BY-SA 4.0 License](https://creativecommons.org/licenses/by-sa/4.0/) at  
<https://github.com/mifunetoshiro/kanjium>

The data set `fivebetas` is derived from five of the svg files in  
KanjiVG --- Copyright (C) 2009-2023 Ulrich Apel  
made available under a [CC BY-SA 3.0 License](https://creativecommons.org/licenses/by-sa/3.0/) at  
<https://kanjivg.tagaini.net/>  
`085e4.svg` and `090f5.svg` (included for unit tests) are exact copies
of files from the same source
