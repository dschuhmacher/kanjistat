% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_kanjidic2.R
\name{read_kanjidic2}
\alias{read_kanjidic2}
\title{Read a KANJIDIC2 file}
\usage{
read_kanjidic2(fpath = NULL, output = c("list", "xml"))
}
\arguments{
\item{fpath}{the path to a local KANJIDIC2 file. If \code{NULL} (the default) the most recent
KANJIDIC2 file is downloaded from \url{https://www.edrdg.org/kanjidic/kanjidic2.xml.gz} after asking for confirmation.}

\item{output}{one of \code{"list"} or \code{"xml"}. The desired type of output.}
}
\value{
If \code{output = "xml"}, the exact XML document obtained from \link[xml2:read_xml]{xml2::read_xml}. If \code{output = "list"}, a list of
lists (the individual kanji), each with the following seven components.
\itemize{
\item \code{literal}: a single UTF-8 character representing the kanji.
\item \code{codepoint}: a named character vector giving the available codepoints in the unicode and jis standards.
\item \code{radical}: a named numeric vector giving the radical number(s), in the range 1 to 214.
The number named \code{classical} is as recorded in the \emph{KangXi Zidian} (1716); if there is a number
named \code{nelson_c}, the kanji was reclassified in Nelson's \emph{Modern Reader's Japanese-English Character
Dictionary} (1962/74).
\item \code{misc}: a list with six components
\itemize{
\item \code{grade}: the kanji grade level. 1 through 6 indicates a kyouiku kanji and the grade in which the
kanji is taught in Japanese primary school. 8 indicates one of the remaining jouyou kanji learned
in junior high school, and 9 or 10 are jinmeiyou kanji. The remaining (hyougai) kanji have \code{NA}
as their entry.
\item \code{stroke_count}: The stroke count of the kanji, including the radical. If more than
one, the first is considered the accepted count, while subsequent ones are common miscounts.
\item \code{variant}: a named character vector giving either a cross-reference code to another kanji,
usually regarded as a variant, or an alternative indexing code for the current kanji.
The type of variant is given in the name.
\item \code{freq}: the frequency rank (1 = most frequent) based on newspaper data.
\code{NA} if not among the 2500 most frequent.
\item \code{rad_name}: a character vector. For a kanji that is a radical itself, the name(s) of the radical
(if there are any), otherwise of length 0.
\item \code{jlpt}: The Japanese Language Proficiency Test level according to the old four-level system
that was in place before 2010. A value from 4 (most elementary) to 1 (most advanced).
}
\item \code{dic_number}: a named character vector (possibly of length 0) giving the index numbers (for some
kanji with letters attached) of the kanji in various dictionaries,
textbooks and flashcard collections (specified by the name). For Morohashi's \emph{Dai Kan-Wa Jiten},
the volume and page number is also provided in the format moro.VOL.PAGE.
\item \code{query_code}: a named character vector giving the codes of the kanji in various query systems
(specified by the name). For Halpern's SKIP code, possible misclassifications (if any) of the kanji
are also noted in the format mis.skip.TYPE, where TYPE indicates the type of misclassification.
\item \code{reading_meaning}: a (possibly empty) list containing zero or more \code{rmgroup} components creating
groups of readings and meanings (in practice there is never more than one \code{rmgroup} currently)
as well as a component \code{nanori} giving a character vector (possibly of length 0) of readings only
associated with names. Each \code{rmgroup} is a list with entries:
\itemize{
\item \code{reading}: a (possibly empty) list of entries named from among \code{pinyin}, \code{korean_r}, \code{korean_h},
\code{vietnam}, \code{ja_on} and \code{ja_kun}, each containing a character vector of the corresponding readings
\item \code{meaning}: a (possibly empty) list of entries named with two-letter (ISO 639-1) language codes,
each containing a character vector of the corresponding meanings.
}
}
}
\description{
Perform basic validity checks and transform data to a standardized list or keep as an object of
class \code{\link[xml2:read_xml]{xml_document}} (package \code{xml2}).
}
\details{
KANJIDIC2 contains detailed information on all of the 13108 kanji in three main Japanese standards (JIS X 0208, 0212 and 0213).
The KANJIDIC files have been compiled and maintained by Jim Breen since 1991, with the help of various
other people. The copyright is now held by the Electronic Dictionary Research and Development Group (EDRDG).
The files are made available under the Creative Commons BY-SA 4.0 license.
See \url{https://www.edrdg.org/wiki/index.php/KANJIDIC_Project} for details on the contents of the files
and their license.

If \code{output = "xml"}, some minimal checks are performed (high level structure and
total number of kanji).

If \code{output = "list"}, additional validity checks of the lower level structure are performed.
Most are in accordance with the file's Document Type Definition (DTD).
Some additional check concern some common patterns that are true about the current
KANJIDIC2 file (as of December 2023) and seem unlikely to change in the near future.
This includes that there is always at most one \code{rmgroup} entry in \code{reading_meaning}.
Informative warnings are provided if any of these additional checks fail.
}
\examples{
if (interactive()) {
  read_kanjidic2("kanjidic2.xml")
}

}
\seealso{
\link{kanjidata}, \link{kreadmean}
}
