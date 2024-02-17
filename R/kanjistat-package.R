#' @importFrom graphics image lines par strheight strwidth text
#' @importFrom grDevices dev.new dev.off gray hcl.colors pdf png svg tiff
#' @importFrom methods hasArg is
#' @importFrom stats approx setNames
#' @importFrom utils combn data head packageVersion str relist
# non-standard packages (tidyverse), frequently used functions only (otherwise call with ::)  
#' @importFrom purrr attr_getter chuck list_flatten pluck pluck<-
#' @importFrom stringr str_sub
#' @importFrom RANN nn2
#' @importClassesFrom Matrix dsCMatrix
NULL

## usethis namespace: start
#' @importFrom lifecycle badge
## usethis namespace: end
NULL

#' Data on kanji
#'
#' The tibbles kbase and kmorph provide basic and morphologic information, respectively, for
#' all kanji contained in the KANJIDIC2 file (see below)
#'
#' @format kbase is a tibble with 13,108 rows and 13 variables:
#' \describe{
#'   \item{kanji}{the kanji}
#'   \item{unicode}{the Unicode codepoint}
#'   \item{strokes}{the number of strokes}
#'   \item{class}{one of four classes: "kyouiku", "jouyou", "jinmeiyou" or "hyougai"}
#'   \item{grade}{a number from 1-11, basically a finer version of class, same as in KANJIDIC2,
#'   except that we assgined an 11 for all hyougaiji (rather than an NA value)}
#'   \item{kanken}{at what level the kanji appears in the Nihon Kanji Nouryoku Kentei (Kanken)}
#'   \item{jlpt}{at what level the kanji appears in the Japanese Language Proficiency Test
#'   (Nihongou Nouryoku Shiken)}
#'   \item{wanikani}{at what level the kanji is learned on the kanji learning website Wanikani}
#'   \item{frank}{the frequency rank (1 = most frequent) "based on several averages (Wikipedia,
#'   novels, newspapers, ...)"}
#'   \item{frank_news}{the frequency rank (1 = most frequent) based on news paper data (2501
#'   most frequent kanji over four years in the Mainichi Shimbun)}
#   \item{freq_innoc}{the relative frequency of the kanji (per 1000 kanji) in the Innocent Corpus
#   containing over 5000 novels}
#   derived from https://foosoft.net/projects/yomichan/
#   currently not included due to unclear license
#'   \item{read_on, read_kun}{a single ON reading in katakana}
#'   \item{read_kun}{a single kun reading in hiragana}
#'   \item{mean}{a single English meaning of the kanji}
#' }
#' 
#' @format kmorph is a tibble with 13,108 rows and 15 variables:
#' \describe{
#'   \item{kanji}{the kanji}
#'   \item{strokes}{the number of strokes}
#'   \item{radical}{the traditional (Kangxi) radical used for indexing kanji (one of 214)}
#'   \item{radvar}{the variant of the radical if it is different, otherwise \code{NA}}
#'   \item{nelson_c}{the Nelson radical if it differs from the traditional one, otherwise \code{NA}}
#'   \item{idc}{ideographic description character (plus sometimes a number or a letter) describing
#'     the shape of the kanji}
#'   \item{components}{visible components of the kanji; originally from KRADFILE}
#   \item{wk_components}{components of the kanji (according to Wanikani)}
#   compiled from \url{https://www.wanikani.com/}
#   currently not included due to unclear permission
#'   \item{skip}{the kanji's SKIP code}
#'   \item{mean}{a single English meaning of the kanji (same as in kbase)}
#' }
#' 
#' @details The single ON and kun readings and the single meaning are for easy identification
#'   of the more difficult kanji. They are the first entry in the KANJIDIC2 file which may not
#'   always be the most important one. For full readings/meanings use the function \code{\link{lookup}}
#'   or consult a dictionary.
#' 
#' @source Most of the data is directly from the KANJIDIC2 file.
#'   \url{https://www.edrdg.org/wiki/index.php/KANJIDIC_Project}\cr
#'   Variables \code{jlpt}, \code{frank}, \code{idc}, \code{components} were taken from the Kanjium data base 
#'   \url{https://github.com/mifunetoshiro/kanjium}\cr
#'   Variable \code{components} is originally from RADKFILE/KRADFILE.
#'   \url{https://www.edrdg.org/})
#'   
#'   The use of this data is covered in each case by a Creative Commons BY-SA 4.0 License.
#'   See the package's LICENSE file for details and copyright holders.
#'   
#'   Variable "class" is derived from "grade".\cr
#'   Variable "kanken" was compiled based on the Wikipedia description of the test levels (as of September 2022). 
#'   
#' @name kanjidata
NULL

#' @format NULL
#' @rdname kanjidata
"kbase"

#' @format NULL
#' @rdname kanjidata
"kmorph"


#' Kanji readings and meanings
#'
#' Data set of all kanji readings and meanings from the KANJIDIC2 dataset in an R list
#' format. For convenient access to this data use function \code{\link{lookup}}.
#' 
#' @source KANJIDIC2 file by Jim Breen and The Electronic Dictionary Research
#' and Development Group (EDRDG)\cr
#' \url{https://www.edrdg.org/wiki/index.php/KANJIDIC_Project}\cr 
#' The use of this data is covered by the Creative Commons BY-SA 4.0 License.
"kreadmean"


#' Sample lists of kanjimat objects
#'
#' @encoding UTF-8
#'
#' @format \code{fivetrees1}, \code{fivetrees2} and \code{fivetrees3} are lists of five \code{\link{kanjimat}} 
#' objects each, representing the same five basic kanji 校,木,休,林,相, containing each
#' a tree component. Their matrices are antialiased 64 x 64 pixel representations of the kanji. The size
#' is chosen as a compromise between aesthetics and memory/computational cost,
#' such as for \code{\link{kmatdist}}. 
#'
#' All of them are in handwriting style fonts.
#' \code{fivetrees1} is in a Kyoukasho font (schoolbook style),
#' \code{fivetrees2} is in a Kaisho font (regular script calligraphy font),
#' \code{fivetrees3} is in a Gyousho font (semi-cursive calligraphy font).
#' 
#' @source The list has been generated with the function \code{\link{kanjimat}} using the Mac OS
#' pre-installed YuKyokasho font (fivetrees1), as well as the freely available fonts nagayama_kai
#' by Norio Nagayama and KouzanBrushFontGyousyo by Aoyagi Kouzan.
#'  
#' @examples
#' oldpar <- par(mfrow = c(3,5))
#' invisible( lapply(fivetrees1, plot) )
#' invisible( lapply(fivetrees2, plot) )
#' invisible( lapply(fivetrees3, plot) )
#' par(oldpar)
#'   
#' @name fivetrees
NULL

#' @rdname fivetrees
"fivetrees1"

#' @rdname fivetrees
"fivetrees2"

#' @rdname fivetrees
"fivetrees3"



#' A sample list of kanjivec objects
#'
#' @encoding UTF-8
#'
#' @format \code{fivebetas} is a list of five \code{\link{kanjivec}} objects
#' representing the basic kanji 部,障,陪,郵,陣 containing  "beta" components,
#' which come in fact from two different classical radicals:
#' \itemize{
#'   \item 阜-->⻖ on the left: mound, small village
#'   \item 邑-->⻏ on the right: large village
#' }
#' 
#' @source The list has been generated with the function \code{\link{kanjivec}} 
#' with parameter \code{flatten="intelligent"} from the corresponding files
#' in the KanjiVG database by Ulrich Apel (\url{https://kanjivg.tagaini.net/}).
#'  
#' @examples 
#' oldpar <- par(mfrow = c(1,5), mai = rep(0,4))
#' invisible( lapply(fivebetas, plot, seg_depth = 2) )
#' par(oldpar)
#'   
# warnings are no problems for R CMD check
"fivebetas"

ddd <- function() { kanjistat::dstrokedit[243,] }

#' Precomputed kanji distances
#'
#' @encoding UTF-8
#'
#' @format
#' Symmetric sparse matrices containing distances between a key kanji, its ten nearest neighbors and
#' possibly some other close kanji.<br>
#' For `dstrokedit`, these are the stroke edit distances according to Yencken and Baldwin (2008).<br>
#' For `dyehli`, these are the bag-of-radicals distances according to Yeh and Li (2002).
#' Both are an instance of the S4 class `dsCMatrix` (symmetric sparse matrices in _column_-compressed format)
#' with 2133 rows and 2133 columns.
#' 
#' All pre-2010 jouyou kanji that are also post-2010
#' jouyou kanji are included. The indices are those from [`kbase`].
#' 
#' @source 
#' Datasets from <https://lars.yencken.org/datasets>, made available under the
#' Creative Commons Attribution 3.0 Unported licence.
#' 
#' Computed as part of *Yencken, Lars (2010) 
#' [Orthographic support for passing the reading hurdle in Japanese](https://lars.yencken.org/papers/phd-thesis.pdf). 
#' PhD Thesis, University of Melbourne, Melbourne, Australia*.
#' 
#' @references Yeh, Su-Ling 
#' and Li, Jing-Ling (2002). Role of structure and component in judgements of 
#' visual similarity of Chinese characters. *Journal of Experimental Psychology: 
#' Human Perception and Performance*, **28**(4), 933–947.
#' 
#' Yencken, Lars, & Baldwin, Timothy (2008). Measuring and predicting orthographic associations:
#' Modelling the similarity of Japanese kanji. In: *Proceedings of the 22nd International Conference on Computational
#' Linguistics (Coling 2008)*, pp. 1041-1048.
#' 
#' @examples
#' # Find index for kanji 部
#' bu_index <- match("部", kbase$kanji)
#' 
#' # Look up available stroke edit distances for 部.
#' non_zero <- which(dstrokedit[bu_index,] != 0)
#' sed <- dstrokedit[non_zero, bu_index]
#' names(sed) <- kbase[non_zero,]$kanji
#' sort(sed)
#'
#' # Look up available bag-of-radicals distance for 部.
#' non_zero <- which(dyehli[bu_index,] != 0)
#' bord <- dyehli[non_zero, bu_index]
#' names(bord) <- kbase[non_zero,]$kanji
#' sort(bord)
#'
#' @name distdata
NULL

#' @format NULL
#' @rdname distdata
"dstrokedit"

#' @format NULL
#' @rdname distdata
"dyehli"
